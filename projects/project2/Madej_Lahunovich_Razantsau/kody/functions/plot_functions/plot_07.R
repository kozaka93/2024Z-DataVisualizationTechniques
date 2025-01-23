plot_07_prepare_data <- function(
    data = read.csv("data/training_data.csv")
    ){
  
  library(scales)
  data$Date <- as.POSIXct(data$Date, format = "%Y-%m-%d %H:%M:%S")
  data$Date <- as.Date(data$Date)
  
  data %>%
    group_by(Date, Owner) %>%
    summarize(
      total_sets = n(),
      duration = DurationInMinutes,
      Body.Part = Body.Part,
      .groups = "drop"
    ) %>%
    mutate(intensivity = total_sets / duration) |>
    distinct(Date, Owner, .keep_all = TRUE)
  

  
}


plot_07 <- function(
    data = plot_07_prepare_data(),
    people = c("Ludwik", "Maxim", "Yahor")
){
  #Sys.setlocale("LC_TIME", "en_GB.UTF-8")
  
  if (length(people) != 3){
    data <- data %>% 
      filter(Owner %in% people)
  }
  
  ggplot(data, aes(x = Date, y = intensivity, color = Owner)) +
    geom_line(size = 0.6) +
    geom_point(size = 1.5) +
    scale_x_date(
      breaks = date_breaks("1 week"), 
      labels = date_format("%b %d")  
    ) +
    labs(
      title = "Training intensity",
      x = "Date",
      y = "Number of Sets in 1 Minute",
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
plot_07()
