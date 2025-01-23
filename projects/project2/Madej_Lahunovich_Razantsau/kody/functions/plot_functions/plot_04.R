library(ggplot2)
library(dplyr)
library(stringr)

plot_04 <- function(data) {
  data <- data %>% filter(Weight != 0)
  
  summary_data <- data %>%
    group_by(Weight, Owner) %>%
    summarize(total_sets = sum(Set.Order), .groups = "drop")
  
  bar_plot <- ggplot(summary_data, aes(x = Weight, y = total_sets, fill = Owner)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Total Sets by Weight and Owner",
      x = "Weight (kg)",
      y = "Total Sets",
      fill = "Owner"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right"
    )
  
  hist_plot <- ggplot(data, aes(x = Weight, color = Owner, fill = Owner)) +
    geom_histogram(stat = "count", position = "identity", bins = 10, alpha = 0.6) +
    labs(
      title = "Distribution of Total Repetitions by Weight and Owner",
      x = "Weight (kg)",
      y = "Total Repetitions",
      fill = "Owner"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right"
    )
  
  return(list(bar_plot = bar_plot, hist_plot = hist_plot))
}
