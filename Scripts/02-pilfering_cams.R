library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)

pilfering <- read.csv("Input/pilfering.csv")

pilfering <- pilfering %>%
  mutate(midden = ifelse(midden == "5", "05.", midden))

pilfering_sums <- pilfering %>%
  summarize(
    total.number.of.pics = sum(total.number.of.pics, na.rm = TRUE),
    hits.of.owner.on.midden = sum(hits.of.owner.on.midden, na.rm = TRUE),
    hits.of.owner.feeding = sum(hits.of.owner.feeding, na.rm = TRUE),
    hits.of.intruder = sum(hits.of.intruder, na.rm = TRUE),
    hits.of.intruder.with.cone = sum(hits.of.intruder.with.cone, na.rm = TRUE))

pilfering_long <- pilfering_sums %>%
  pivot_longer(
    cols = everything(),
    names_to = "category",
    values_to = "count")

pilfering <- ggplot(pilfering_long, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  scale_x_discrete(
    limits = c("total.number.of.pics",
               "hits.of.owner.on.midden",
               "hits.of.owner.feeding",
               "hits.of.intruder",
               "hits.of.intruder.with.cone"),
    labels = c(
      "total.number.of.pics" = "Total Photos",
      "hits.of.owner.on.midden" = "Owner on Midden",
      "hits.of.owner.feeding" = "Owner Feeding",
      "hits.of.intruder" = "Intruder",
      "hits.of.intruder.with.cone" = "Intruder with Cone")) +
  labs(
    x = NULL,
    y = "Number of Photos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

pilfering

#save
ggsave(filename = "Output/pilfering.jpeg", plot = pilfering, width = 10, height = 6)