#load packages
source("Scripts/00-packages.R")

#load in data
intruders <- read.csv("Input/intruders.csv")

#male intrusions
male_intrusions <- intruders %>%
  filter(sex_trap == "M") %>%  #focus on male intruders
  group_by(year, grid, season) %>%
  summarise(
    count = n(),  #count of intrusions
    .groups = 'drop')

#overview of male intrusions by season
table(male_intrusions$season, male_intrusions$count)

#plot
ggplot(male_intrusions, aes(x = season, y = count, fill = season)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Male Intrusions Across Seasons", x = "Season", y = "Count of Intrusions") +
  theme_minimal()

#data summary
male_summary <- male_intrusions %>%
  group_by(season) %>%
  summarise(
    total_intrusions = n(),                 
    .groups = "drop")

#save
write.csv(male_summary, file = "Output/male_summary.csv", row.names = FALSE)

# #pull in yearly cones and add to male_intrusions ------------
# yearly_cones <- read.csv("Input/yearly_cones.csv")
# 
# yearly_cones <- yearly_cones %>%
#   rename(year = Year, 
#          grid = Grid)
# 
# male_intrusions <- male_intrusions %>%
#   left_join(yearly_cones, by = c("year", "grid")) %>%
#   na.omit() %>%
#   dplyr::select(-num_trees, -cone_counts, -cone_index, -total_cones)

# model --------------------------------------------------------------------
#relevel 'season' to make the 'other' category the reference level
male_intrusions$season <- factor(male_intrusions$season)
male_intrusions$season <- relevel(male_intrusions$season, ref = "other")

#negative binomial model - data is overdispersed (i.e. variance significantly exceeds mean)
nb_model <- glm.nb(count ~ season, data = male_intrusions)
summary(nb_model)

#plot residuals
plot(nb_model$residuals, type = 'b', main = "Residuals from Negative Binomial Model",
     xlab = "Index", ylab = "Residuals")
abline(h = 0, col = "red")

#summary stats ------------------------------------------------------------
#extract the coefficients
coef_summary <- summary(nb_model)$coefficients

#calculate odds ratios
odds_ratios <- exp(coef_summary[, "Estimate"])

#include 95% confidence intervals for odds ratios
conf_int <- exp(confint(nb_model))

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
box_plot <- ggplot(male_intrusions, aes(x = season, y = count, fill = season)) +
  geom_boxplot() +
  labs(title = "Total Count of Male Intrusions by Season",
       x = "Season",
       y = "Total Count of Male Intrusions") +
  scale_fill_brewer(palette = "Set3") +
  scale_x_discrete(labels = c("breeding" = "Breeding", 
                              "lactation" = "Lactation", 
                              "other" = "Non-Reproductive")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

box_plot

#save
ggsave("Output/box_plot.jpeg", plot = box_plot, width = 8, height = 6)












