library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(scales)

plot_03 <- function(data = read.csv("data/training_data.csv")) {
  bodypart <- data %>%
    group_by(Owner, Body.Part) %>%
    summarise(count = n(), .groups = "drop")
  
  colors <- c("#3de60e", "#0edfe6", "#dbf20a")
  
  ggplot(bodypart, aes(x = Body.Part, y = count, fill = Owner)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = "Number of Sets per Muscle Group", x = "Body Part", y = "Count") +
    theme_minimal() +
    theme(
      # axes and plot title
      axis.title = element_text(color = "white", size = 15, face = "bold"),
      plot.title = element_text(colour = "white", size=20, face="bold", hjust = 0.5),
      axis.text = element_text(color = "white", face = "bold", size=11),
      axis.text.x = element_text(color = "white", face = "bold", size=10),
      axis.text.y = element_text(color = "white", face = "bold", size=10),
      
      # background
      plot.background = element_rect("#3d3a36"),
      
      #legend
      legend.text = element_text(color="white", size=10),
      legend.title = element_blank(),
      
      # grid
      panel.grid.major = element_line(color = "#5b5c55", linetype = "dashed"),
      panel.grid.minor = element_line(color = "#4f4f4c", linetype = "dotted")
    ) +
    scale_fill_manual(values = c(
      "Ludwik"="#3de60e",
      "Yahor"="#0edfe6",
      "Maxim"="#dbf20a")
    ) +
    scale_color_manual(values = c(
      "Ludwik"="#3de60e",
      "Yahor"="#0edfe6",
      "Maxim"="#dbf20a"))+
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
}