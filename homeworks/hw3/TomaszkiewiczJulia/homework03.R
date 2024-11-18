library(ggplot2)
library(patchwork)

categories_work_location <- c("Hybrid", "Fully Remote", "Fully in person")
scores_work_location <- c(73, 63, 71)

categories_team_size <- c("15<", "5-15", "5>", "0")
scores_team_size <- c(80, 70, 63, 60)

categories_work_type <- c("Human care", "Business management", "Knowledge work",
                          "Technical vocational", "Visual and performing arts",
                          "Customer service", "Physical labour")
scores_work_type <- c(77, 76, 75, 70, 64, 60, 60)

base_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#333333"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#555555"),
    axis.title = element_text(size = 12, face = "bold", color = "#333333"),
    axis.text = element_text(size = 10, color = "#444444"),
    panel.grid.major.x = element_line(color = "gray80", size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  )

plot1 <- ggplot(data.frame(Category = categories_work_location, Score = scores_work_location),
                aes(x = reorder(Category, Score), y = Score, fill = Score)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_text(aes(label = Score), hjust = -0.2, size = 4, color = "black") +
  scale_fill_gradient(low = "#a6cee3", high = "#1f78b4") +
  labs(
    title = "Average MHQ score - work location",
    subtitle = "Comparing average MHQ scores for different work locations",
    x = "Work location",
    y = "MHQ"
  ) +
  ylim(0, 80) +
  coord_flip() +
  base_theme

plot2 <- ggplot(data.frame(Category = categories_team_size, Score = scores_team_size),
                aes(x = reorder(Category, Score), y = Score, fill = Score)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_text(aes(label = Score), hjust = -0.2, size = 4, color = "black") +
  scale_fill_gradient(low = "#b2df8a", high = "#33a02c") +
  labs(
    title = "Average MHQ score - team size",
    subtitle = "Comparing average MHQ scores for different team sizes",
    x = "Team size",
    y = "MHQ"
  ) +
  ylim(0, 80) +
  coord_flip() +
  base_theme

plot3 <- ggplot(data.frame(Category = categories_work_type, Score = scores_work_type),
                aes(x = reorder(Category, Score), y = Score, fill = Score)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_text(aes(label = Score), hjust = -0.2, size = 4, color = "black") +
  scale_fill_gradient(low = "#fdbf6f", high = "#ff7f00") +
  labs(
    title = "Average MHQ score - work type",
    subtitle = "Comparing average MHQ scores for different work types",
    x = "Work type",
    y = "MHQ"
  ) +
  ylim(0, 80) +
  coord_flip() +
  base_theme

combined_plot <- plot1 / plot2 / plot3
combined_plot
