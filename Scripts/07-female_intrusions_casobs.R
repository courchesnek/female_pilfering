#load packages
source("Scripts/00-packages.R")

#connection to KRSP database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password"))

#pull in feeding obs + census exact locs
feeding <- read.csv("Input/all_feeding_census_exact_locs.csv")

#pull in census data
census <- read.csv("Input/squirrel_census_exact_locs.csv")

#numeric locx to alphabet ----------------------------------
num_to_letter <- setNames(LETTERS, as.character(1:26))

numeric_to_locx <- function(x) {
  x_chr <- as.character(x)
  
  # 1) If it already starts with a letter, do nothing
  if (grepl("^[A-Za-z]", x_chr)) return(x_chr)
  
  # 2) If it's zero or negative, leave it untouched
  #    (we assume anything starting with "-" or "0" should stay)
  if (grepl("^0", x_chr) || grepl("^-", x_chr)) return(x_chr)
  
  # 3) Split into integer-part before dot and the rest
  base  <- sub("\\.(.*)$", "", x_chr)      # e.g. "3" from "3.57"
  after <- sub("^[^.]*", "", x_chr)        # e.g. ".57" from "3.57"
  
  # 4) Look up letter (only if base in 1:26)
  letter <- num_to_letter[base]
  if (is.na(letter)) {
    warning("Integer part out of A–Z range (or not 1–26): ", base)
    return(x_chr)}
  
  # 5) Reassemble
  paste0(letter, after)}

numeric_to_locx_vec <- Vectorize(numeric_to_locx)

feeding <- feeding %>%
  mutate(locx_feed = numeric_to_locx_vec(locx_feed))

#filter for mating repro stage only --------------
# feeding_mating <- feeding_mating %>%
#   filter(repro_stage == "mating")

#filter for cached (cones and mushrooms) resoures only --------------
feeding <- feeding %>%
  filter(food_type == "capital")

#identify female intrusions ----------------------------------------------
#first, add sex to census table and filter for May census 
squirrel_sex <- tbl(con,"flastall") %>%
  collect() %>%
  dplyr::select(squirrel_id, sex)

census <- left_join(census, squirrel_sex, by = "squirrel_id") %>%
  filter(is.na(sex) == FALSE)

census <- census %>%
  mutate(census_date = as.Date(census_date)) %>%
  filter(format(census_date, "%m-%d") == "05-15")

#create a year column
census <- census %>%
  mutate(year = as.numeric(format(census_date, "%Y")))

#locx back to numeric in both tables
feeding$locx_feed <- loc_to_numeric(feeding$locx_feed)

census$locx <- loc_to_numeric(census$locx)

#okay now, identify intrusions
intrusions <- feeding %>%
  #rename columns in feeding table for clarity
  rename(
    feeder_id   = squirrel_id,
    feeder_sex  = sex) %>%
  #join census middens by grid and year
  inner_join(
    census %>%
      #rename columns in census table for clarity
      rename(
        midden_owner = squirrel_id,
        locx_mid     = locx,
        locy_mid     = locy,
        midden_owner_sex  = sex) %>%
      # make sure these are numeric so we can subtract the distance
      mutate(
        locx_mid = as.numeric(locx_mid),
        locy_mid = as.numeric(locy_mid)),
    by = c("grid","year"), #by doing this, like years and grids stay grouped; any non-matches (i.e. feeding near a midden that is not in a census) get dropped
    relationship = "many-to-many") %>%
  # also make sure these are numeric so we can subtract the distance
  mutate(
    locx_feed = as.numeric(locx_feed),
    locy_feed = as.numeric(locy_feed),
    
    dx = abs(locx_feed - locx_mid), #distance between feeding loc and midden loc on x-axis
    dy = abs(locy_feed - locy_mid), #distance between feeding loc and midden loc on y-axis
    
    on_own   = (feeder_id == midden_owner) & (dx <= 0.3 & dy <= 0.3), #on own = TRUE if within 0.3 loc units (~9m) of a midden she owns
    intruder = (feeder_id != midden_owner) & (dx <= 0.3 & dy <= 0.3), #intruder = TRUE if within 0.3 loc units (~9m) of a different squirrel's midden = intrusion
    off_any  = (!on_own & !intruder)) %>% #feeding not on any known midden (neither her own nor another’s)
    dplyr::select(
    grid, year, repro_stage,
    feeder_id, feeder_sex,
    locx_feed, locy_feed,
    midden_owner, midden_owner_sex,
    locx_mid,   locy_mid,
    dx, dy,
    on_own, intruder, off_any)

#filter for female feeding events on midden only (her own or intruding)
female_feeding <- intrusions %>%
  filter(feeder_sex == "F",
         !off_any) %>%
  dplyr::select(-off_any)


# sex ratios --------------------------------------------------------------
#F:M per year (spring) per grid
sex_ratio <- census %>%
  group_by(year, grid, sex) %>%
  summarise(count = n_distinct(squirrel_id), .groups = "drop") %>% #only count each individual squirrel once, so don't double count if a squirrel owns multiple middens in a given year
  pivot_wider(names_from = sex, values_from = count, values_fill = list(count = 0)) %>%
  mutate(F_M = round(F / M, 3))

#join this with the female feeding table - keep only matches
female_feeding <- female_feeding %>%
  inner_join(sex_ratio, by = c("year", "grid"))

# do females pilfer from males during mating? -----------------------------
# fit a generalized linear mixed effects model with binary response
##0) make non-breeding the reference category
mod_data <- mod_data %>%
  mutate(repro_stage = relevel(factor(repro_stage), ref = "non-breeding"))

##1) intruder needs to be 1 or 0, and make sure year is a factor
mod_data <- female_feeding %>%
  mutate(
    intruder = as.integer(intruder),   # TRUE→1, FALSE→0
    year     = factor(year)) %>%
  dplyr::select(grid, year, repro_stage, feeder_sex, feeder_id, midden_owner_sex, midden_owner, intruder, F_M)

##2) fit the GLMM with random intercepts for squirrel and year
# model <- glmer(
#   intruder ~ F_M + (1 | feeder_id) + (1 | midden_owner) + (1 | year),
#   data   = mod_data,
#   family = binomial(link = "logit"))
# 
# summary(model)

#include midden owner as a random effect to control for relatedness/tolerance and also resources levels cached

#year is contributing nothing to the model (variance = 0), so we'll drop it
# model_2 <- glmer(
#   intruder ~ F_M + (1 | feeder_id) + (1 | midden_owner),
#   data   = mod_data,
#   family = binomial(link = "logit"))
# 
# summary(model_2)

#even after accounting for sex‐ratio, the “baseline” chance that a female feeds 
#on a midden that isn’t her own is extremely small.

#changes in the female∶male ratio have no clear impact on a female’s odds of intruding

#sex ratio is poorly centered relative ot the intercept, which can inflate SEs and mask effects
mod_data <- mod_data %>%
  mutate(F_Mc = scale(F_M, scale = FALSE))

model_3 <- glmer(
  intruder ~ F_Mc + repro_stage + (1 | feeder_id) + (1 | midden_owner),
  data   = mod_data,
  family = binomial(link = "logit"))

summary(model_3)

#convert log-odds into probabilities
coefs <- fixef(model_3)
beta0       <- coefs["(Intercept)"]
beta_mating <- coefs["repro_stagemating"]
beta_lac    <- coefs["repro_stagelactation"]

# baseline (non-breeding) intrusion probability
p_nonbreeding <- plogis(beta0)

# intrusion probability during mating
p_mating      <- plogis(beta0 + beta_mating)

# intrusion probability during lactation
p_lactation   <- plogis(beta0 + beta_lac)

probabilities <- data.frame(
  stage          = c("non-breeding","mating","lactation"),
  probability    = c(p_nonbreeding, p_mating, p_lactation))
 
#a female feeding event has a probability on the order of one‐in‐a‐hundred‐million 
#of being an intrusion.... they don't pilfer. 
  
  
