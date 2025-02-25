#load packages
source("Scripts/00-packages.R")

#load in data
intruders <- read.csv("Input/intruders.csv")

#filter data for female intruders and only control grids (non-experimental)
female_intrusions <- intruders %>%
  filter(sex_trap == "F",
         grid %in% c("KL", "SU", "CH"))

#add a binary outcome
female_intrusions <- female_intrusions %>%
  mutate(trapped_on_male = ifelse(sex_owner == "M", 1, 0))  #1 if trapped on a male midden, 0 if trapped on a female midden

#data summary
summary <- female_intrusions %>%
  summarise(
    total_intrusions = n(),                  
    male_midden_intrusions = sum(trapped_on_male),  
    female_midden_intrusions = total_intrusions - male_midden_intrusions, 
    .groups = "drop")

write.csv(summary, file = "Output/females_summary.csv", row.names = FALSE)

#logistic regression, and report magnitude ----------------------------------------------------
model <- glm(trapped_on_male ~ sex_ratio,
             family = binomial(link = "logit"),
             data = female_intrusions)

summary(model)

par(mfrow = c(2, 2))
plot(model)

#odds ratio for magnitude 
coefficient_sex_ratio <- coef(model)["sex_ratio"]

#calculate the odds ratio for being trapped on a male midden
odds_ratio_male = exp(coefficient_sex_ratio)

#calculate the reciprocal to find the odds of being trapped on a female midden
odds_ratio_female = 1 / odds_ratio_male

#proportions
prop_male <- mean(female_intrusions$trapped_on_male == 1, na.rm = TRUE)
prop_female <- 1 - prop_male

#how many squirrels?
length(unique(female_intrusions$squirrel_id_trap))

female_intrusions %>%
  group_by(sex_trap) %>%
  summarise(unique_squirrels = n_distinct(squirrel_id_trap))

#how many years of data?
length(unique(female_intrusions$year))

#how many grids?
length(unique(female_intrusions$grid))

#calculate proportions by sex ratio ---------------------------------------
intrusion_summary <- female_intrusions %>%
  group_by(year, grid, sex_ratio) %>%  
  summarise(
    total_intrusions = n(),  
    trapped_on_female = sum(trapped_on_male == 0, na.rm = TRUE),  
    trapped_on_male = sum(trapped_on_male == 1, na.rm = TRUE),    
    prop_female = trapped_on_female / total_intrusions, 
    prop_male = trapped_on_male / total_intrusions) %>%
  ungroup()

# generate predictions and plot -------------------------------------------
sex_ratio_seq <- seq(min(female_intrusions$sex_ratio, na.rm = TRUE),
                     max(female_intrusions$sex_ratio, na.rm = TRUE),
                     length.out = 100)

newdata <- data.frame(sex_ratio = sex_ratio_seq)

#generate predictions on the response scale (predicted probability) with standard errors
preds <- predict(model, newdata = newdata, type = "response", se.fit = TRUE)

#female predictions
newdata$fit_female <- 1 - preds$fit
newdata$lower_female <- 1 - (preds$fit + 1.96 * preds$se.fit)
newdata$upper_female <- 1 - (preds$fit - 1.96 * preds$se.fit)

#male predictions
newdata$fit_male <- preds$fit
newdata$lower_male <- pmax(0, preds$fit - 1.96 * preds$se.fit)
newdata$upper_male <- pmin(1, preds$fit + 1.96 * preds$se.fit)

#ensure predicted probabilities remain within [0, 1]
newdata$lower_female <- pmax(0, newdata$lower_female)
newdata$upper_female <- pmin(1, newdata$upper_female)
newdata$lower_male <- pmax(0, newdata$lower_male)
newdata$upper_male <- pmin(1, newdata$upper_male)

plot_data <- newdata %>%
  dplyr::select(sex_ratio, starts_with("fit_"), starts_with("lower_"), starts_with("upper_")) %>%
  pivot_longer(cols = -sex_ratio,
               names_to = c("metric", "sex"),
               names_sep = "_",
               values_to = "value") %>%
  pivot_wider(names_from = "metric", values_from = "value") %>%
  mutate(sex = recode(sex, "female" = "Female", "male" = "Male"))

plot_summary <- plot_data %>%
  group_by(sex) %>%
  summarise(
    fit = mean(fit),
    lower = mean(lower),
    upper = mean(upper),
    .groups = "drop")


#plot predictions
female_intrusions <- ggplot(plot_summary, aes(x = sex, y = fit, fill = sex)) +
  geom_col(position = position_dodge(), width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.6)) +
  labs(title = "Predicted Probability of Female Intrusions on Female- vs Male-owned Middens",
       x = "Sex",
       y = "Predicted Probability of Intrusion") +
  scale_fill_manual(values = c("Female" = "#FF99CC", "Male" = "#99CCFF")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

female_intrusions

#save
ggsave("Output/female_intrusions.jpeg", plot = female_intrusions, width = 8, height = 6)

# female_intrusions <- ggplot() +
#   #raw data points: proportion on female middens
#   geom_point(data = intrusion_summary, 
#              aes(x = sex_ratio, y = prop_female, color = grid), alpha = 0.6) +
#   #predicted line: probability of being on female middens
#   geom_line(data = newdata, aes(x = sex_ratio, y = fit_female), color = "black", size = 1) +
#   geom_ribbon(data = newdata, aes(x = sex_ratio, ymin = lower_female, ymax = upper_female),
#               alpha = 0.2, fill = "grey") +
#   scale_color_brewer(palette = "Set2") +
#   labs(
#     title = "Predicted Probability of Female Intrusions on Female-owned Middens by Sex Ratio",
#     x = "Sex Ratio (F:M)",
#     y = "Probability Trapped on Female Midden",
#     color = "Study grid") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# female_intrusions
# 
# #save
# ggsave("Output/female_intrusions.jpeg", plot = female_intrusions, width = 8, height = 6)
# 
# # plot - raw data ----------------------------------------------------------
# sex_ratios <- ggplot(intrusion_summary, aes(x = sex_ratio, y = prop_female, color = grid)) +
#   geom_point(alpha = 0.6) +
#   geom_smooth(method = "loess", se = TRUE, aes(group = 1), color = "black", size = 1) +
#   labs(
#     title = "Proportion of Female Intrusions on Female Middens Remains High Across Sex Ratios and Study Grids",
#     x = "Sex Ratio (Females:Males)",
#     y = "Proportion of Intrusions on Female Middens") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     axis.text.x = element_text(angle = 45, hjust = 1))
# 
# sex_ratios
# 
# #save
# ggsave("Output/sex_ratios.jpeg", plot = sex_ratios, width = 10, height = 6)

