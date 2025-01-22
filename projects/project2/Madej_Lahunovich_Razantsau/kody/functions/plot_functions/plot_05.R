library(dplyr)
library(tidyr)
library(ggplot2)

plot_05 <- function(data = read.csv("data/training_data.csv")) {
  
  weights <- data %>% 
    group_by(Owner, Weight) %>% 
    summarise(count = n()) 
  
  weights[is.na(weights$Weight),]$Weight = 0
  weights[weights$Weight == 0,]$Weight = -1
  
  weights <- weights %>% 
    mutate(weight_group = (Weight)%/%5) 
  
  weights$weight_group <- as.factor(weights$weight_group)
  levels(weights$weight_group)[levels(weights$weight_group) == "-1"] <- "Bodyweight"
  
  levels(weights$weight_group) <- sapply(levels(weights$weight_group), function(x) {
    if (x == "Bodyweight") return("Bodyweight")
    else {
      group <- as.integer(x) * 5
      paste0(group, "-", group + 4)
    }
  })
  
  plot <- ggplot(weights, aes(x=weight_group, y = count, fill = Owner)) +
    geom_bar(stat = "identity") +
    labs(title = "Distribution of Weights Used in Workouts",
         x = "Weight (kg)",
         y = "Count") +
    theme_minimal() +
    theme(
      # axes and plot title
      axis.title = element_text(color = "white", size = 15, face = "bold"),
      axis.text = element_text(color = "white", face = "bold", size=11),
      axis.text.x = element_text(color = "white", face = "bold", size=8, angle = 90, vjust = 0.5),
      axis.text.y = element_text(color = "white", face = "bold", size=10),
      plot.title = element_text(colour = "white", size=20, face="bold", hjust = 0.5),
      
      # background
      plot.background = element_rect("#3d3a36"),
      strip.text = element_text(colour = "white", size=17, face="bold"),
      
      #legend
      legend.text = element_text(color="white", size=10),
      legend.title = element_blank(),
      legend.position = "none",
      
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
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
    facet_wrap(~Owner, scales = "free")
  
  return(plot)
}


#ggsave("www/images/plot_05.png", width = 16, height = 10)
plot_05()