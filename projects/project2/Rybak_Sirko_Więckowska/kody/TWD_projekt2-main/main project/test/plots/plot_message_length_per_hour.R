# 
# zwraca wykres slupkowy porownojacy 3 osoby
# 
# argumenty:
#   table1 - tabela, who1 - kto jest w tabeli1
#   ...

library(ggplot2)
library(dplyr)
source(file.path("functions", "message_length_per_hour.R"))

library(sysfonts)
library(showtext)

plot_message_length_per_hour <- function(data, who){
  data_to_plot <- message_length_per_hour(data, who)
  
  order <- c(unique(data_to_plot$hour_range)[6:length(data_to_plot$hour_range)], unique(data_to_plot$hour_range)[1:5])
  
  plot <- data_to_plot %>%
    mutate(hour_range = factor(hour_range, levels = order)) %>%
    ggplot(aes(x = hour_range, y = message_length)) +
    scale_y_continuous(
      limits = c(0, 100),   # Set y-axis range from 0 to 150
      expand = c(0, 0)      # Remove extra space around the axis
    ) +
    geom_violin(fill = "#ff547e", color = "#ff547e") +
    theme_minimal() +
    labs(
      title = "Message Length for Each Hour of the Day",
      x = "Hour of the Day",
      y = "Length"
    ) +
    theme(
      plot.title = element_text(
        color = "white",
        size = 20,
        face = "bold",
        hjust = 0.5
      ),
      axis.title = element_text(color = "white", size = 18),
      axis.text = element_text(color = "white", size = 16),
      axis.text.x = element_text(angle = 30, hjust = 1),  # Rotate x-axis labels
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10),  # Adjust margins
      text = element_text(family = "space mono"),  # Apply consistent font
      legend.position = "none"  # Remove the legend
    )
  
  return(plot)
}