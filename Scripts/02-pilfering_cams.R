library(ggplot2)
install.packages("reshape2")
library(reshape2)
library(dplyr)

pilfering <- read.csv("Input/pilfering.csv")

pilfering <- pilfering %>%
  mutate(midden = ifelse(midden == "5", "05.", midden))

long_data <- melt(pilfering, id.vars = "midden", 
                  measure.vars = c("total_pics", "owner", "owner_cone", 
                                   "intruder", "intruder_cone"))

pilfering <- ggplot(long_data, aes(x = midden, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Midden", 
       y = "Number of Pictures") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Slant x-axis labels
  scale_fill_discrete(name = "Picture Type",
                      labels = c("Total Pictures", "Owner", 
                                 "Owner With Cone", "Intruder", 
                                 "Intruder With Cone"))

ggsave(filename = "Output/pilfering.jpeg", 
       plot = pilfering, 
       width = 10, height = 6, dpi = 300)

