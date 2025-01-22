source("spozycieSummarizing.R")

plot_06_prepare_data <- function(){
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  spozycieSummarizing()
  data = read.csv("data/spozycie.csv")
  
  
  
  data$DATA <- as.Date(data$DATA)
  
  rename_dict <- c("WODA" = "Water", "HERBATA" = "Tea", "KAWA" = "Coffee", "INNE" = "Other")
  
  data$Drink <- rename_dict[data$Drink]
  
  data$Drink <- factor(data$Drink, levels = c("Water", "Tea", "Coffee", "Other"))
  
  colnames(data) <- c("Date", "Owner", "Drink", "Amount (in litres)", "Trening")
  
  return(data)
  
}

plot_06 <- function(
    data = plot_06_prepare_data(),
    people = c("Ludwik", "Maxim", "Yahor")
){
  
  if (length(people) != 3){
    data <- data %>% 
      filter(Owner %in% people)
  }
  
  drink_colors <- c(
    "Water" = "#ADD8E6",   # Light blue for water (better contrast on dark background)
    "Tea" = "#D2B48C",     # Light tan for tea
    "Coffee" = "#A0522D",  # Lighter coffee brown for better visibility
    "Other" = "#FFD700"    # Bright gold remains suitable
  )
  ## KOD DLA DRINKOW KOLOREM JAK PERSON
  # person_colors <- c(
  #   "Ludwik" = "#3de60e",
  #   "Yahor"  = "#0edfe6",
  #   "Maxim"  = "#dbf20a"
  # )
  # 
  # if (length(people) == 1) {
  #   chosen_color <- person_colors[people]
  #   drink_colors <- rep(chosen_color, 4)
  #   names(drink_colors) <- c("Water","Tea","Coffee","Other")
  # } else {
  #   drink_colors <- c(
  #     "Water"  = "#888888",
  #     "Tea"    = "#888888",
  #     "Coffee" = "#888888",
  #     "Other"  = "#888888"
  #   )
  # }
  # 
  ## dodac legende dla kropek?
  ggplot(data, aes(x = Date, y = `Amount (in litres)`, color = Drink)) +
    geom_line(size = 0.5) +  # Linie pokazujące spożycie napojów
    geom_point(data = filter(data, Trening == TRUE),  aes(color = Drink), size = 1.5) +
    scale_x_date(
      breaks = date_breaks("1 week"),
      labels = date_format("%b %d")
    ) +
    labs(
      title = "Drink Consumption Vs. Training Days",
      subtitle = "The dots represent training days",
      x = "Date",
      y = "Quantity (l)"
    ) +
    theme_minimal() +
    theme(
      # axes and plot title
      axis.title = element_text(color = "white", size = 15, face = "bold"),
      axis.text = element_text(color = "white", size=8),
      plot.title = element_text(colour = "white", size=20, face="bold", hjust = 0.5),
      
      # background
      plot.background = element_rect("#3d3a36"),
      
      #legend
      legend.text = element_text(color="white", size=10),
      legend.title = element_blank(),
      
      # grid
      panel.grid.major = element_line(color = "#5b5c55", linetype = "dashed"),
      panel.grid.minor = element_line(color = "#4f4f4c", linetype = "dotted"),
      
      strip.text.x = element_text(
        size = 8, color = "white"
      ),
    ) +
    scale_fill_manual(values = c(
      "Ludwik"="#3de60e",
      "Yahor"="#0edfe6",
      "Maxim"="#dbf20a")
    ) +
    scale_color_manual(
      values = drink_colors,
      labels = c("Water" = "Water", 
                 "Tea"   = "Tea",
                 "Coffee"= "Coffee",
                 "Other" = "Other")
    )
  
}

