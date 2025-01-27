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

# model -------------------------------------------------------------------
#relevel 'season' to make the 'other' category the reference level
female_intrusions$season <- factor(female_intrusions$season)
female_intrusions$season <- relevel(female_intrusions$season, ref = "other")

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

# plot --------------------------------------------------------------------
#proportions
bar_data <- intruders %>%
  group_by(snow, season) %>%
  summarise(
    total_count = n(), # Total female intrusions (on all middens)
    count = sum(sex_owner == "M"), # Female intrusions on male middens
    .groups = "drop"
  ) %>%
  mutate(
    prop = count / total_count # Proportion of intrusions on male middens
  )

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

box_whisker <- ggplot(odds_ratio_table, aes(x = reorder(Predictor, Estimate), y = Estimate)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = log(Lower_CI), ymax = log(Upper_CI)), width = 0.2, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +  # Flip the coordinates for horizontal bars
  labs(
    title = "Effect of Snow, Season, and Sex Ratio on Female Intrusions on Male Middens",
    x = "Predictor",
    y = "Log-Odds Estimate (with 95% CI)"
  ) +
  theme_minimal()

box_whisker

#save
ggsave("Output/box_whisker.jpeg", plot = box_whisker, width = 8, height = 6)




