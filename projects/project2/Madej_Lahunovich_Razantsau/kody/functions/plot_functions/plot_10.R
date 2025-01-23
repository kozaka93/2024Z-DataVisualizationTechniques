plot_10_prepare_data <- function(
    data = read.csv("data/training_data.csv")
    ){
  
  library(scales)
  data$Date <- as.POSIXct(data$Date, format = "%Y-%m-%d %H:%M:%S")
  
  data %>%
    group_by(Date, Owner) %>%
    summarize(
      total_reps = sum(Reps),
      duration = DurationInMinutes,
      Body.Part = Body.Part,
      .groups = "drop"
    ) %>%
    distinct(Date, Owner, .keep_all = TRUE)
  
  
}


plot_10 <- function(
    data = plot_10_prepare_data(),
    people = c("Ludwik", "Maxim", "Yahor")
    ){
  
  if (length(people) != 3){
    data <- data %>% 
      filter(Owner %in% people)
  }
  
  ggplot(data, aes(x = duration, y = total_reps, color = Owner)) +
    geom_point(size = 1.5) +  
    labs(
      title = "Total Reps Vs. Workout Length",
      x = "Workout Length (minutes)",
      y = "Total Reps",
      color = ""
    ) +
    theme_minimal() +
    theme(
      # axes and plot title
      axis.title = element_text(color = "white", size = 15, face = "bold"),
      axis.text = element_text(color = "white", face = "bold", size=11),
      axis.text.x = element_text(color = "white", face = "bold", size=10),
      axis.text.y = element_text(color = "white", face = "bold", size=10),
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
    scale_color_manual(values = c(
      "Ludwik"="#3de60e",
      "Yahor"="#0edfe6",
      "Maxim"="#dbf20a"))
}

plot_10()
