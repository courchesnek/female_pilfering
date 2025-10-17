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

#filter for cached cones resources --------------
feeding <- feeding %>%
  filter(food_type == "capital", detail == 2)

#identify male intrusions ----------------------------------------------
##*first, add sex to census table and filter for May census ----
squirrel_sex <- tbl(con,"flastall") %>%
  collect() %>%
  dplyr::select(squirrel_id, sex)

census <- left_join(census, squirrel_sex, by = "squirrel_id") %>%
  filter(is.na(sex) == FALSE)

census <- census %>%
  mutate(census_date = as.Date(census_date)) %>%
  filter(format(census_date, "%m-%d") == "05-15")

##*create a year column ----
census <- census %>%
  mutate(year = as.numeric(format(census_date, "%Y")))

##*locx back to numeric in both tables ----
feeding$locx_feed <- loc_to_numeric(feeding$locx_feed)

census$locx <- loc_to_numeric(census$locx)

##*fix bad loc entries ----
feeding <- feeding %>%
  mutate(
    # 1) Turn "(null)" into NA
    locy_feed = na_if(locy_feed, "(null)"),
    # 2) Convert commas to dots
    locy_feed = str_replace(locy_feed, ",", ".")) %>%
  na.omit()

##*prepare census table to join ----
census <- census %>%
  rename(
    midden_owner = squirrel_id,
    locx_mid     = locx,
    locy_mid     = locy,
    midden_owner_sex = sex) %>%
  mutate(
    locx_mid = as.numeric(locx_mid),
    locy_mid = as.numeric(locy_mid))

##*prepare feeding table to join ----
feeding <- feeding %>%
  rename(
    feeder_id   = squirrel_id,
    feeder_sex  = sex) %>%
  mutate(
    locx_feed = as.numeric(locx_feed))

##*join feeding to census table ----
feeding_census <- feeding %>%
  inner_join(census, by = c("grid", "year"), relationship = "many-to-many") %>%
  mutate(
    # euclidean distance from feeding event to each midden
    dx   = as.numeric(locx_feed) - as.numeric(locx_mid),
    dy   = as.numeric(locy_feed) - as.numeric(locy_mid),
    dist = sqrt(dx^2 + dy^2))

##*for each feeding event, keep only the *closest* midden ----
feeding_nearest <- feeding_census %>%
  group_by(grid, year, feeder_id, locx_feed, locy_feed) %>%
  slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%
  ungroup()

##*classify whether it was on own midden, an intrusion, or or off any ----
intrusions <- feeding_nearest %>%
  mutate(
    on_own   = (feeder_id == midden_owner) & (dist <= 13/30), #10m midden "island" radius +/- 3m error
    intruder = (feeder_id != midden_owner) & (dist <= 13/30), #10m midden "island" radius +/- 3m error
    off_any  = !on_own & !intruder)

##*male feeding with feeding type classification ----
male_feeding <- intrusions %>%
  filter(feeder_sex == "M") %>%
  filter(on_own | intruder) %>%  # only keep midden-based events
  mutate(
    feeding_type = case_when(
      on_own ~ "own_midden",
      intruder & midden_owner_sex == "M" ~ "male",
      intruder & midden_owner_sex == "F" ~ "female",
      TRUE ~ NA_character_)) %>%
  filter(!is.na(feeding_type))  # drop any weird NAs just in case

# sex ratios --------------------------------------------------------------
##*F:M per year (spring) per grid ----
sex_ratio <- census %>%
  group_by(year, grid, midden_owner_sex) %>%
  summarise(count = n_distinct(midden_owner), .groups = "drop") %>% #only count each individual squirrel once, so don't double count if a squirrel owns multiple middens in a given year
  pivot_wider(names_from = midden_owner_sex, values_from = count, values_fill = list(count = 0)) %>%
  mutate(F_M = round(F / M, 3))

##*join this with the female feeding table - keep only matches----
male_feeding <- male_feeding %>%
  inner_join(sex_ratio, by = c("year", "grid"))

# do males pilfer from other squirrels during mating? -----------------------------
##*fit a generalized linear mixed effects model with binary response ----
###1) intruder needs to be 1 or 0, and make sure year is a factor ----
mod_data <- male_feeding %>%
  mutate(
    feeding_type = factor(feeding_type, levels = c("own_midden", "male", "female")),
    year = factor(year),
    repro_stage = relevel(factor(repro_stage), ref = "non-breeding"),
    F_Mc = as.numeric(scale(F_M, scale = FALSE))) %>%
  dplyr::select(grid, year, repro_stage, feeder_sex, feeder_id,
                midden_owner_sex, midden_owner, feeding_type, F_M, F_Mc)

###2) make non-breeding and own_midden are the reference categories ----
mod_data <- mod_data %>%
  mutate(repro_stage = relevel(factor(repro_stage), ref = "non-breeding"))

mod_data <- mod_data %>%
  mutate(feeding_type = relevel(feeding_type, ref = "own_midden"))

###3) fit a multinomial model with own_midden as reference (cannot include random effects in this model type) ----
#okay since previous GLMM showed zero variance for feeder_id, midden_owner, and year
#focused on population-level effects, not individual
multi_model <- multinom(
  feeding_type ~ F_Mc + repro_stage,
  data = mod_data)

summary(multi_model)

###4) save model summary table ----
model_summary <- tidy(multi_model) %>%
  mutate(
    term = if_else(term == "(Intercept)", "Non-breeding", term)) %>%
  rename(
    Midden_type    = y.level,    # feeding_type level
    Term      = term,       # predictor
    Estimate  = estimate,   # β
    SE        = std.error,  # standard error
    Z         = statistic,  # z‐value
    P_value   = p.value) %>%     # p‐value
  mutate(
    Estimate = round(Estimate, 4),
    SE       = round(SE,       4),
    Z        = round(Z,        4),
    P_value  = round(P_value,  4)) %>%
  # 3. Reorder columns
  dplyr::select(Midden_type, Term, Estimate, SE, Z, P_value)

#save as csv
write.csv(model_summary, "Output/MULTINOM_model_summary_MALES.csv", row.names = FALSE)

# generate predictions from model and plot --------------------------------
###1) build prediction dataset at mean sex-ratio ----
newdata <- expand.grid(
  repro_stage = factor(c("non-breeding", "mating", "lactation"),
                       levels = c("non-breeding", "mating", "lactation")),
  F_Mc = 0)  # mean-centered sex ratio

###2) predict class probabilities from multinomial model ----
predicted_probs <- predict(multi_model, newdata = newdata, type = "probs")

###3) combine with newdata ----
plot_df <- cbind(newdata, as.data.frame(predicted_probs))

###4) pivot longer for plotting ----
plot_long <- plot_df %>%
  pivot_longer(cols = c("male", "female", "own_midden"),
               names_to = "feeding_type",
               values_to = "prob") %>%
  mutate(
    feeding_type = recode(feeding_type,
                          male   = "Male midden (intrusion)",
                          female = "Female midden (intrusion)",
                          own_midden    = "Own midden"),
    feeding_type = factor(feeding_type, levels = c("Male midden (intrusion)", "Female midden (intrusion)", "Own midden")))

#total number of feeding events per season
sample_sizes <- male_feeding %>%
  group_by(repro_stage) %>%
  summarise(total = n(), .groups = "drop")

###5) plot ----
male_intrusions_casobs <- ggplot(plot_long, aes(x = repro_stage, y = prob, fill = feeding_type)) +
  geom_col(position = position_stack(reverse = TRUE), width = 0.96) +
  geom_text(data = sample_sizes,
            aes(x = repro_stage, y = 1.05, label = paste0("n = ", total)),
            inherit.aes = FALSE,
            vjust = 0.4, size = 9) +
  scale_x_discrete(
    limits = c("mating", "lactation", "non-breeding"),
    labels = c("mating" = "Mating", "lactation" = "Lactation", "non-breeding" = "Non-breeding"),
    expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format(1), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  scale_fill_manual(values = c("Male midden (intrusion)" = "#88CCEE",
                               "Female midden (intrusion)" = "#CC6677",
                               "Own midden" = "#44AA99")) +
  labs(
    x     = "Reproductive Stage",
    y     = "Proportion of Total Feeding Events",
    fill  = "Feeding Location") +
  theme_minimal(base_size = 22) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
        panel.grid = element_blank(),
        axis.text.x = element_text(hjust = 0.5, color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.margin = margin(t = 60, r = 30, b = 10, l = 20),
        legend.position = "bottom",
        legend.box.margin = margin(t = -20, r = 0, b = 0, l = 0))

male_intrusions_casobs

#save plot
ggsave(filename = "Output/male_intrusions_casobs.jpeg", plot = male_intrusions_casobs, width = 12, height = 7)


# breakdown of intrusions -------------------------------------------------
##1) Extract coefficients ----
coefs <- coef(multi_model)  # this gives log-odds for male/female vs own

##2) Build a function to compute probabilities from coefficients ----
get_probs <- function(stage, sex_ratio = 0) {
  # predictor values for this scenario
  newdata <- data.frame(
    repro_stage = stage,
    F_Mc = sex_ratio)
  
  # predict probabilities directly from the model
  probs <- predict(multi_model, newdata = newdata, type = "probs")
  return(round(probs, 3))}

##3) Get probabilities for each reproductive stage ----
get_probs("non-breeding")
get_probs("mating")
get_probs("lactation")

# sample sizes -------------------------------------------------------------
##*own midden feeding total ----
mod_data %>%
  filter(feeding_type == "own_midden") %>%
  summarise(count = n())

##*male-owned midden feeding total ----
mod_data %>%
  filter(feeding_type == "male") %>%
  summarise(count = n())

##*female-owned midden feeding total ----
mod_data %>%
  filter(feeding_type == "female") %>%
  summarise(count = n())

length(unique(mod_data$feeder_id))
length(unique(mod_data$year))



