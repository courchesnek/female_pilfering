#load packages
source("Scripts/00-packages.R")

#load in data
intruders <- read.csv("Input/intruders.csv")

#filter data for female intruders
female_intrusions <- intruders %>%
  filter(sex_trap == "F",
         grid %in% c("KL", "SU", "CH")) %>%  #focus on female intruders and control grids
  group_by(year, grid, sex_owner) %>%
  mutate(
    trapped_on_male = ifelse(sex_owner == "M", 1, 0)  #1 if male midden, 0 otherwise
  )

#data summary
season_snow_summary <- female_intrusions %>%
  group_by(season, snow) %>%
  summarise(
    total_intrusions = n(),                 
    male_midden_intrusions = sum(trapped_on_male),  
    female_midden_intrusions = total_intrusions - male_midden_intrusions,
    .groups = "drop")

#save
write.csv(season_snow_summary, file = "Output/season_snow_summary.csv", row.names = FALSE)

#how many squirrels?
length(unique(female_intrusions$squirrel_id_trap))

female_intrusions %>%
  group_by(sex_trap) %>%
  summarise(unique_squirrels = n_distinct(squirrel_id_trap))

#how many years of data?
length(unique(female_intrusions$year))

#how many grids?
length(unique(female_intrusions$grid))

# model -------------------------------------------------------------------
#relevel 'season' to make the 'other' category the reference level
female_intrusions$season <- factor(female_intrusions$season)
female_intrusions$season <- relevel(female_intrusions$season, ref = "non-breeding")

logistic_model <- glm(trapped_on_male ~ sex_ratio + snow + season,
                      family = binomial(link = "logit"),
                      data = female_intrusions)

summary(logistic_model)

par(mfrow = c(2, 2))
plot(logistic_model)

#summary stats ------------------------------------------------------------
#extract the coefficients
coef_summary <- summary(logistic_model)$coefficients

#calculate odds ratios
odds_ratios <- exp(coef_summary[, "Estimate"])

#include 95% confidence intervals for odds ratios
conf_int <- exp(confint(logistic_model))

#combine into a summary table
odds_ratio_table <- data.frame(
  Predictor = rownames(coef_summary),
  Estimate = coef_summary[, "Estimate"],
  Odds_Ratio = odds_ratios,
  Lower_CI = conf_int[, 1],
  Upper_CI = conf_int[, 2],
  p_value = coef_summary[, "Pr(>|z|)"])

#print the table
print(odds_ratio_table, row.names = FALSE)

#generate predictions and plot --------------------------------------------------------------------
mean_sex_ratio <- mean(female_intrusions$sex_ratio, na.rm = TRUE)

newdata <- expand.grid(
  season = unique(female_intrusions$season),
  snow = unique(female_intrusions$snow),
  sex_ratio = mean_sex_ratio)

preds <- predict(logistic_model, newdata = newdata, type = "response", se.fit = TRUE)

# Store predictions in newdata
newdata$fit <- preds$fit
newdata$lower <- preds$fit - 1.96 * preds$se.fit
newdata$upper <- preds$fit + 1.96 * preds$se.fit

# Ensure CI bounds stay within [0, 1] if desired (optional step)
newdata$lower <- pmax(0, newdata$lower)
newdata$upper <- pmin(1, newdata$upper)

#plot predictions
female_intrusions_snow_season <- ggplot(newdata, aes(x = season, y = fit, fill = snow)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.8), 
           color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.2,
                position = position_dodge(width = 0.8)) +
  scale_fill_brewer(palette = "Set3",
                    labels = c("No snow", "Snow cover")) +
  scale_x_discrete(limits = c("mating", "lactation", "non-breeding"),
                   labels = c("mating" = "Mating", "lactation" = "Lactation", "non-breeding" = "Non-breeding")) +
  labs(
    title = "Predicted Probability of a Female Being Trapped on a Male Midden",
    x = "Season",
    y = "Predicted Probability",
    fill = "Snow Cover") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

female_intrusions_snow_season

#save
ggsave("Output/female_intrusions_snow_season.jpeg", plot = female_intrusions_snow_season, width = 8, height = 6)

# plot - raw data --------------------------------------------------------------------
#proportions
bar_data <- intruders %>%
  group_by(snow, season) %>%
  summarise(
    total_count = n(), 
    count = sum(sex_owner == "M"),
    .groups = "drop") %>%
  mutate(
    prop = count / total_count)

#bar graph
bar_graph <- ggplot(bar_data, aes(x = season, y = prop, fill = snow)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c("no snow" = "#00FF99", "snow" = "#0099FF"),
    name = "Snow Period") +
  labs(
    title = "Proportion of Female Intrusions on Male Middens by Snow and Season",
    x = "Season",
    y = "Proportion of Intrusions on Male Middens") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top")

bar_graph

#save
ggsave("Output/bar_graph.jpeg", plot = bar_graph, width = 8, height = 6)

odds_ratio_table$Predictor <- factor(odds_ratio_table$Predictor, levels = c(
  "(Intercept)",
  "seasonbreeding",
  "seasonlactation",
  "snowsnow",
  "sex_ratio"))

box_whisker <- ggplot(odds_ratio_table, aes(x = Predictor, y = Estimate)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = log(Lower_CI), ymax = log(Upper_CI)), width = 0.2, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +
  labs(
    title = "Effect of Snow, Season, and Sex Ratio on Female Intrusions on Male Middens",
    x = "Predictor",
    y = "Log-Odds Estimate (with 95% CI)") +
  scale_x_discrete(labels = c(
    "(Intercept)" = "Baseline\n(non-breeding, no snow, balanced sex ratio)",
    "sex_ratio" = "Sex ratio\n(females:males)",
    "snowsnow" = "Snow cover",
    "seasonbreeding" = "Breeding season",
    "seasonlactation" = "Lactation season")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 10))

box_whisker

#save
ggsave("Output/box_whisker.jpeg", plot = box_whisker, width = 12, height = 6)




