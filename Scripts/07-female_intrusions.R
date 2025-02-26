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
  filter(intruder == 1) %>%
  summarise(
    total_intrusions = n(),                  
    male_midden_intrusions = sum(trapped_on_male),  
    female_midden_intrusions = total_intrusions - male_midden_intrusions, 
    .groups = "drop")

write.csv(summary, file = "Output/females_summary.csv", row.names = FALSE)

#how many squirrels?
num_squirrels <- female_intrusions %>%
  filter(intruder == 1)

length(unique(num_squirrels$squirrel_id_trap))

#logistic regression, and report magnitude ----------------------------------------------------
model <- glm(trapped_on_male ~ season + sex_ratio,
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
emm_male <- emmeans(model, ~ season, 
                    at = list(sex_ratio = mean(female_intrusions$sex_ratio, na.rm = TRUE)),
                    type = "response")
emm_male_df <- as.data.frame(emm_male)

female_intrusions <- ggplot(emm_male_df, aes(x = season, y = prob, fill = season)) +
  geom_col() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  scale_fill_brewer(palette = "Set3") +
  scale_x_discrete(
    limits = c("mating",
               "lactation",
               "non-breeding"),
    labels = c(
      "mating" = "Mating",
      "lactation" = "Lactation",
      "non-breeding" = "Non-breeding")) +
  labs(x = "Season",
       y = "Probability",
       title = "Probability of a Female Being Trapped on a Male Midden Across Seasons") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))

female_intrusions

#save
ggsave(filename = "Output/female_intrusions.jpeg", plot = female_intrusions, width = 10, height = 6)

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

