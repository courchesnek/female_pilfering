#load packages
source("Scripts/00-packages.R")

#load in data
intruders <- read.csv("Input/intruders.csv")

#filter data for female intruders and only control grids (non-experimental)
female_intrusions <- intruders %>%
  filter(intruder == 1,
         sex_trap == "F",
         grid %in% c("KL", "SU", "CH"),
         squirrel_id_trap != squirrel_id_owner) #exclude trapped on own midden

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

#how many squirrels?
length(unique(female_intrusions$squirrel_id_trap))

#logistic regression, and report magnitude ----------------------------------------------------
model <- glm(trapped_on_male ~ season + F_M,
             family = binomial(link = "logit"),
             data = female_intrusions)

summary(model)

par(mfrow = c(2, 2))
plot(model)

#odds ratio for magnitude 
coefficient_sex_ratio <- coef(model)["F_M"]

#calculate the odds ratio for being trapped on a male midden
odds_ratio_male = exp(coefficient_sex_ratio)

#calculate the reciprocal to find the odds of being trapped on a female midden
odds_ratio_female = 1 / odds_ratio_male

#proportions
prop_male <- mean(female_intrusions$trapped_on_male == 1, na.rm = TRUE)
prop_female <- 1 - prop_male

#how many years of data?
length(unique(female_intrusions$year))

#how many grids?
length(unique(female_intrusions$grid))

# generate predictions and plot -------------------------------------------
emm_male <- emmeans(model, ~ season, 
                    at = list(F_M = mean(female_intrusions$F_M, na.rm = TRUE)),
                    type = "response")
emm_male_df <- as.data.frame(emm_male)

#total number of intrusions for plot
sample_sizes <- female_intrusions %>%
  group_by(season) %>%
  summarise(total = n(), .groups = "drop")

#male and female predictions
emm_male_df <- merge(emm_male_df, sample_sizes, by = "season")
predictions_stack <- emm_male_df %>%
  mutate(prob_female = 1 - prob) %>%
  pivot_longer(cols = c(prob, prob_female), names_to = "trap_location", values_to = "predicted_proportion") %>%
  mutate(trap_location = recode(trap_location, "prob" = "Male Midden", "prob_female" = "Female Midden"))

female_intrusions_plot <- ggplot(predictions_stack, aes(x = season, y = predicted_proportion, fill = trap_location)) +
  geom_col(position = "stack") +
  geom_text(data = sample_sizes,
            aes(x = season, y = 1.05, label = total),
            inherit.aes = FALSE,
            vjust = 0.4, size = 5) +
  scale_x_discrete(
    limits = c("mating", "lactation", "non-breeding"),
    labels = c("mating" = "Mating", "lactation" = "Lactation", "non-breeding" = "Non-breeding")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  scale_fill_manual(values = c("Female Midden" = "#FF3399", "Male Midden" = "#00CCFF")) +
  labs(x = "Season",
       y = "Proportion of Intrusions",
       title = "Female Intrusions Across Seasons",
       fill = "Trap Location") +
  theme_minimal(base_size = 18) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b=30)),
        plot.margin = margin(t = 20, r = 5, b = 10, l = 15))

female_intrusions_plot

#save
ggsave(filename = "Output/female_intrusions.jpeg", plot = female_intrusions_plot, width = 10, height = 6)

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

