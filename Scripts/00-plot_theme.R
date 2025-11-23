# plot theme for consistency across figures throughout thesis

theme_thesis <- function() {
  theme_minimal() +
    theme(
      text = element_text(size = 21),
      axis.title.x = element_text(margin = margin(t = 20)),
      axis.title.y = element_text(margin = margin(r = 15)),
      axis.text.x = element_text(hjust = 0.5, color = "black", margin = margin(t = 10)),
      axis.text.y = element_text(color = "black"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1.3),
      axis.ticks.y = element_line(colour = "black", linewidth = 0.75),
      axis.ticks.length.y = unit(0.25, "cm"),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.6),
      panel.grid.minor.y = element_line(color = "grey92", linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
      legend.position = "none")}
