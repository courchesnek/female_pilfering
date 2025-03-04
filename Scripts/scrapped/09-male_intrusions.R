#load packages
source("Scripts/00-packages.R")

#load in data
intruders <- read.csv("Input/intruders.csv")

#male intrusions
male_intrusions <- intruders %>%
  filter(sex_trap == "M",
         grid %in% c("KL", "SU", "CH"),
         sex_owner == "F",
         intruder ==1) %>%
  group_by(year, season) %>%
  summarise(total_intrusions = n(),
            .groups = "drop")

total_observations <- male_intrusions %>%
  summarize(total = sum(total_intrusions, na.rm = TRUE))

#plot - raw data
ggplot(male_intrusions, aes(x = season, y = count, fill = season)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_x_discrete(limits = c("mating", "lactation", "non-breeding")) +
  labs(title = "Male Intrusions Across Seasons", x = "Season", y = "Count of Intrusions") +
  theme_minimal()

#data summary
male_summary <- male_intrusions %>%
  group_by(season) %>%
  summarise(
    total_intrusions = sum(total_intrusions, na.rm = TRUE),                 
    .groups = "drop")

#save
write.csv(male_summary, file = "Output/male_summary.csv", row.names = FALSE)

#how many years of data?
length(unique(male_intrusions$year))

# model --------------------------------------------------------------------
#relevel 'season' to make the 'other' category the reference level
male_intrusions$season <- factor(male_intrusions$season)
male_intrusions$season <- relevel(male_intrusions$season, ref = "non-breeding")

#negative binomial model - data is overdispersed (i.e. variance significantly exceeds mean)
nb_model <- glm.nb(total_intrusions ~ season, data = male_intrusions)
summary(nb_model)

#model reference categories?
contrasts(male_intrusions$season) #non-breeding is reference category

#plot residuals
plot(nb_model$residuals, type = 'b', main = "Residuals from Negative Binomial Model",
     xlab = "Index", ylab = "Residuals")
abline(h = 0, col = "red")

model_output <- tidy(nb_model)

model_output <- model_output %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  rename(zvalue = statistic,
         pvalue = p.value)

#save
write.csv(model_output, "Output/maleint_model_output.csv", row.names = FALSE)

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

#generate predictions and plot --------------------------------------------------------------------
newdata <- data.frame(season = unique(male_intrusions$season))
preds <- predict(nb_model, newdata = newdata, type = "response", se.fit = TRUE)

newdata$fit <- preds$fit
newdata$lower <- preds$fit - 1.96 * preds$se.fit
newdata$upper <- preds$fit + 1.96 * preds$se.fit

#plot predictions
male_intrusions <- ggplot(newdata, aes(x = season, y = fit, fill = season)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(
    title = "Predicted Mean Male Intrusions on Female Middens by Season",
    x = "Season",
    y = "Predicted Count of Intrusions") +
  scale_fill_brewer(palette = "Set3") +
  scale_x_discrete(limits = c("mating", "lactation", "non-breeding"),
                   labels = c("mating" = "Mating", "lactation" = "Lactation", "non-breeding" = "Non-breeding")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

male_intrusions

#save
ggsave("Output/male_intrusions.jpeg", plot = male_intrusions, width = 8, height = 6)

#plot - raw data --------------------------------------------------------------------
season_summary <- male_intrusions %>%
  group_by(season) %>%
  summarise(
    mean_intrusions = mean(total_intrusions),
    se_intrusions = sd(total_intrusions) / sqrt(n()),
    .groups = "drop")

ggplot(season_summary, aes(x = season, y = mean_intrusions, fill = season)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = mean_intrusions - se_intrusions,
                    ymax = mean_intrusions + se_intrusions),
                width = 0.2) +
  labs(
    title = "Mean Male Intrusions on Female Middens by Season",
    x = "Season",
    y = "Mean Count of Intrusions") +
  scale_fill_brewer(palette = "Set3") +
  scale_x_discrete(limits = c("mating", "lactation", "non-breeding"),
                   labels = c("mating" = "Mating", "lactation" = "Lactation", "non-breeding" = "Non-breeding")) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")









