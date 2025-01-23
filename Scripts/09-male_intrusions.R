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

# #pull in yearly cones and add to male_intrusions
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

# plot --------------------------------------------------------------------
box_plot <- ggplot(male_intrusions, aes(x = season, y = count, fill = season)) +
  geom_boxplot() +
  labs(title = "Total Count of Male Intrusions by Season",
       x = "Season",
       y = "Total Count of Male Intrusions") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

box_plot

#save
ggsave("Output/box_plot.jpeg", plot = box_plot, width = 8, height = 6)












