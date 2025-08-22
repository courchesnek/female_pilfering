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

#filter for cached (cones and mushrooms) resources only --------------
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

#fix bad loc entries
feeding <- feeding %>%
  mutate(
    # 1) Turn "(null)" into NA
    locy_feed = na_if(locy_feed, "(null)"),
    # 2) Convert commas to dots
    locy_feed = str_replace(locy_feed, ",", ".")) %>%
  na.omit()

# prepare census table to join
census <- census %>%
  rename(
    midden_owner = squirrel_id,
    locx_mid     = locx,
    locy_mid     = locy,
    midden_owner_sex = sex) %>%
  mutate(
    locx_mid = as.numeric(locx_mid),
    locy_mid = as.numeric(locy_mid))

#prepare feeding table to join
feeding <- feeding %>%
  rename(
    feeder_id   = squirrel_id,
    feeder_sex  = sex) %>%
  mutate(
    locx_feed = as.numeric(locx_feed))

# join feeding to census table
feeding_census <- feeding %>%
  inner_join(census, by = c("grid", "year"), relationship = "many-to-many") %>%
  mutate(
    # euclidean distance from feeding event to each midden
    dx   = as.numeric(locx_feed) - as.numeric(locx_mid),
    dy   = as.numeric(locy_feed) - as.numeric(locy_mid),
    dist = sqrt(dx^2 + dy^2))

# for each feeding event, keep only the *closest* midden
feeding_nearest <- feeding_census %>%
  group_by(grid, year, feeder_id, locx_feed, locy_feed) %>%
  slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%
  ungroup()

# classify whether it was on own midden, an intrusion, or or off any
intrusions <- feeding_nearest %>%
  mutate(
    on_own   = (feeder_id == midden_owner) & (dist <= 10/30),
    intruder = (feeder_id != midden_owner) & (dist <= 10/30),
    off_any  = !on_own & !intruder)

# female feeding 
female_feeding <- intrusions %>%
  filter(feeder_sex == "F")

#filter for female feeding events on midden only
female_intrusions <- intrusions %>%
  filter(feeder_sex == "F", intruder == TRUE) %>%
  mutate(event_type = if_else(midden_owner_sex == "M", "male", "female"))

# sex ratios --------------------------------------------------------------
#F:M per year (spring) per grid
sex_ratio <- census %>%
  group_by(year, grid, midden_owner_sex) %>%
  summarise(count = n_distinct(midden_owner), .groups = "drop") %>% #only count each individual squirrel once, so don't double count if a squirrel owns multiple middens in a given year
  pivot_wider(names_from = midden_owner_sex, values_from = count, values_fill = list(count = 0)) %>%
  mutate(F_M = round(F / M, 3))

#join this with the female feeding table - keep only matches
female_intrusions <- female_intrusions %>%
  inner_join(sex_ratio, by = c("year", "grid"))

# do females pilfer from males during mating? -----------------------------
# fit a generalized linear mixed effects model with binary response
##1) intruder needs to be 1 or 0, and make sure year is a factor
mod_data <- female_intrusions %>%
  mutate(
    intrusion = as.integer(event_type == "male"),   # 1 = male intrusion, 0 = female midden
    year     = factor(year)) %>%
  dplyr::select(grid, year, repro_stage, feeder_sex, feeder_id, midden_owner_sex, midden_owner, intrusion, F_M)

##2) make non-breeding the reference category
mod_data <- mod_data %>%
  mutate(repro_stage = relevel(factor(repro_stage), ref = "non-breeding"))

##3) fit the GLMM with random intercepts for squirrel and year
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

#sex ratio is poorly centered relative ot the intercept, which can inflate SEs and mask effects
mod_data <- mod_data %>%
  mutate(F_Mc = as.numeric(scale(F_M, scale = FALSE)))

# feeder_id has zero extra variation and midden_owner perfectly separates intrusions (all 1's on male middens
#, 0's otherwise. they either add no signal or overfit the data, so let's drop them)
# model_3 <- glmer(
#   intrusion ~ F_Mc + repro_stage + (1 | feeder_id) + (1 | midden_owner),
#   data   = mod_data,
#   family = binomial(link = "logit"))
# 
# summary(model_3)

##3.0) fit the GLM with fixed effects only
model_4 <- glm(
  intrusion ~ F_Mc + repro_stage,
  data   = mod_data,
  family = binomial(link = "logit"))

summary(model_4)

#convert log-odds into probabilities
coefs <- coef(model_4)
beta0       <- coefs["(Intercept)"]
beta_mating <- coefs["repro_stagemating"]
beta_lac    <- coefs["repro_stagelactation"]

# baseline (non-breeding) intrusion probability
p_nonbreeding <- plogis(beta0)

# intrusion probability during mating
p_mating      <- plogis(beta0 + beta_mating)

# intrusion probability during lactation
p_lactation   <- plogis(beta0 + beta_lac)

data.frame(
  stage          = c("non-breeding","mating","lactation"),
  probability    = c(p_nonbreeding, p_mating, p_lactation))

#repro stage doesn't matter: females are just as unlikely 
#to feed on male middens in mating as in non-breeding or lactation 

#therefore there’s no support for the idea that males cache extra cones to 
#compensate for increased theft during mating season.

# generate predictions from model and plot --------------------------------
#1) build a new data frame at mean sex-ratio (i.e. F_M = 0)
newdata <- expand.grid(
  repro_stage = factor(
    c("non-breeding","mating","lactation"),
    levels = c("non-breeding","mating","lactation")),
  F_Mc = 0)

newdata$F_Mc <- as.numeric(newdata$F_Mc)

#2) predict intrusion probability from the GLM
newdata$pred_intrusion <- predict(
  model_4, 
  newdata = newdata, 
  type = "response")

#3) create a data frame to plot
plot_df <- data.frame(
  stage  = factor(
    c("non-breeding","mating","lactation"),
    levels = c("non-breeding","mating","lactation")),
  male_midden     = c(p_nonbreeding, p_mating, p_lactation),
  female_midden   = 1 - c(p_nonbreeding, p_mating, p_lactation))

plot_df <- plot_df %>%
  pivot_longer(
    cols = c(male_midden, female_midden),
    names_to = "type",
    values_to = "prob") %>%
  mutate(
    type = recode(type,
                  male_midden   = "Male midden",
                  female_midden = "Female midden"),
    type = factor(type, levels = c("Male midden", "Female midden")))

#4) plot
ggplot(plot_df, aes(x = stage, y = prob, fill = type)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_x_discrete(
    limits = c("mating", "lactation", "non-breeding"),
    labels = c("mating" = "Mating", "lactation" = "Lactation", "non-breeding" = "Non-breeding")) +
  scale_y_continuous(labels = scales::percent_format(1)) +
  scale_fill_manual(values = c("Male midden" = "#3DB7E9",
                               "Female midden" = "#F748A5")) + 
  labs(
    x     = "Reproductive stage",
    y     = "Predicted proportion of feeding events",
    fill  = "Feeding location",
    title = "Proportion of female feeding\non male- vs female-owned middens") +
  theme_minimal(base_size = 20) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
        panel.grid = element_blank(),
        axis.text.x = element_text(hjust = 0.5, color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(margin = margin(t=10)),
        plot.title = element_text(size = 25, face = "bold", hjust = 0.5, margin = margin(b=20)),
        plot.margin = margin(t = 20, r = 20, b = 10, l = 20),
        legend.position = "bottom",
        legend.box.margin = margin(t = -20, r = 0, b = 0, l = 0))
