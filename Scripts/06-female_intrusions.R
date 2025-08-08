#load packages
source("Scripts/00-packages.R")

#load in data
intruders <- read.csv("Input/intruders.csv")

#filter data for female intruders
female_intrusions <- intruders %>%
  filter(intruder == 1,
         sex_trap == "F",
         (grid %in% c("KL", "SU", "CH", "JO", "BT")) & !(grid == "JO" & year < 2013),
         squirrel_id_trap != squirrel_id_owner) #exclude trapped on own midden

#add a binary outcome
female_intrusions <- female_intrusions %>%
  mutate(trapped_on_male = ifelse(sex_owner == "M", 1, 0))  #1 if trapped on a male midden, 0 if trapped on a female midden

#data summary
summary <- female_intrusions %>%
  summarise(
    total_intrusions = n(),                  
    male_midden_intrusions = sum(trapped_on_male), #sum all 1's
    female_midden_intrusions = total_intrusions - male_midden_intrusions, 
    .groups = "drop")

write.csv(summary, file = "Output/females_summary.csv", row.names = FALSE)

#how many squirrels?
length(unique(female_intrusions$squirrel_id_trap))

#how many years of data?
length(unique(female_intrusions$year))

# model ----------------------------------------------------
#make non-breeding the reference category
female_intrusions$season <- factor(female_intrusions$season, ordered = FALSE)
female_intrusions$season <- relevel(female_intrusions$season, ref = "non-breeding")

#make trapped on male = 1 (male midden) the reference category
female_intrusions$trapped_on_male <- as.factor(female_intrusions$trapped_on_male)
female_intrusions$trapped_on_male <- relevel(female_intrusions$trapped_on_male, ref = "1")

contrasts(female_intrusions$season) #non-breeding is reference category
contrasts(female_intrusions$trapped_on_male)

# fit generalized linear mixed effects model with binary response
model <- glmer(trapped_on_male ~ season + F_M + (1 | squirrel_id_trap) + (1 | year),
             family = binomial(link = "logit"),
             data = female_intrusions)

summary(model)

#check residuals
sim_res <- simulateResiduals(model) #remember: with large sample sizes, even very small deviations can become significant
plot(sim_res) 

testOutliers(sim_res) #no significant outliers, again large sample size can call small deviations significant outliers

#how many years of data?
length(unique(female_intrusions$year))

#how many grids?
length(unique(female_intrusions$grid))

#how many squirrels?
length(unique(female_intrusions$squirrel_id_trap))

#clean up model output to save as csv
model_output <- tidy(model)

model_output <- model_output %>%
  dplyr::select(-effect, -group)

model_output <- model_output[-5, ]
model_output <- model_output[-5, ]

model_output <- model_output %>%
  rename(zvalue = statistic,
         pvalue = p.value)

model_output <- model_output %>%
  mutate(across(c(estimate, std.error, zvalue, pvalue), ~round(., 4)))

write.csv(model_output, "Output/model_output.csv", row.names = FALSE)

# generate predictions and plot -------------------------------------------
emm_male <- emmeans(model, ~ season, 
                    at = list(F_M = mean(female_intrusions$F_M, na.rm = TRUE)),
                    type = "response")
emm_male_df <- as.data.frame(emm_male)

#total number of intrusions per season
sample_sizes <- female_intrusions %>%
  group_by(season) %>%
  summarise(total = n(), .groups = "drop")

#male and female predictions
emm_male_df <- merge(emm_male_df, sample_sizes, by = "season")

predictions_stack <- emm_male_df %>%
  mutate(prob_male = 1 - prob) %>%
  pivot_longer(cols = c(prob, prob_male), 
               names_to = "trap_location", 
               values_to = "predicted_proportion") %>%
  mutate(trap_location = recode(trap_location, 
                                "prob" = "Female Midden", 
                                "prob_male" = "Male Midden"))

female_intrusions_plot <- ggplot(predictions_stack, aes(x = season, y = predicted_proportion, fill = trap_location)) +
  geom_col(width = 0.96) +
  geom_text(data = sample_sizes,
            aes(x = season, y = 1.05, label = paste0("n = ", total)),
            inherit.aes = FALSE,
            vjust = 0.4, size = 10) +
  scale_x_discrete(
    limits = c("mating", "lactation", "non-breeding"),
    labels = c("mating" = "Mating", "lactation" = "Lactation", "non-breeding" = "Non-breeding"),
    expand = c(0, 0)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  scale_fill_manual(values = c("Female Midden" = "#CC6677", "Male Midden" = "#88CCEE"),
                    breaks = c("Male Midden", "Female Midden")) +
  labs(x = "Reproductive Stage",
       y = "Proportion of Total Intrusion Events",
       title = "Female Intrusion Events Across Reproductive Stages\n(Trapping Events)",
       fill = "Intrusion Location") +
  theme_minimal(base_size = 22) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
        panel.grid = element_blank(),
        axis.text.x = element_text(hjust = 0.5, color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(margin = margin(t=10)),
        plot.title = element_text(size = 24, face = "bold", hjust = 0.5, margin = margin(b=50)),
        plot.margin = margin(t = 20, r = 20, b = 10, l = 20),
        legend.position = "bottom",
        legend.box.margin = margin(t = -30, r = 0, b = 0, l = 0))

female_intrusions_plot

#save
ggsave(filename = "Output/female_intrusions.jpeg", plot = female_intrusions_plot, width = 12, height = 7)

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

