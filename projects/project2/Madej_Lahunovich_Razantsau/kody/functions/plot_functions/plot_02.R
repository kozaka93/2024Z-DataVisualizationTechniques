plot_02_prepare_data <- function(data = read.csv("data/training_data.csv")){
  # requires aggregate data
  # return proper data for plot_2
  Date <- seq(
    from = as.Date("2024-12-05"),
    to = as.Date("2025-01-24"),
    by = "day"
  )
  
  Owner = rep(c("Ludwik", "Maxim", "Yahor"), times = length(Date))
  Date <- rep(as.character(Date), each=3)
  datesDataFrame <- as.data.frame(cbind(Date, Owner))
  
  data %>% 
    distinct(Date, Owner, DurationInMinutes) %>% 
    mutate(Date = str_sub(Date, 1, 10)) %>% 
    right_join(
      datesDataFrame, 
      by = c("Date", "Owner")
    ) %>% 
    mutate(
      DurationInMinutes = if_else(
        is.na(DurationInMinutes), 
        0, 
        DurationInMinutes),
      Date = as.Date(Date)
      )
}

plot_02 <- function(
    data = plot_02_prepare_data(),
    people = c("Ludwik", "Maxim", "Yahor")
    ){
  # requires processed data for the chart
  library(dplyr)
  library(stringr)
  library(ggplot2)
  source("functions/adjust_plot.R")
  
  if (length(people) != 3){
    data <- data %>% 
      filter(Owner %in% people)
  }
  
  plot <- adjust_plot(
    ggplot(
      data = data,
      aes(
        x = Date,
        y = DurationInMinutes/60,
        fill = Owner
      )
    ) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5)
      ) +
      labs(
        title = "Duration of trainig by data",
        x = "Date",
        y = "Duraton of training in hours"
      ) +
      scale_x_date(
        date_breaks = "2 days",
        date_labels = "%d-%m-%Y",
        limits = c(as.Date("2024-12-05"), as.Date("2025-01-22"))
      )
    )
  plot +
    theme(
      axis.text = element_text(color = "white", face = "bold", size=11),
      axis.text.x = element_text(color = "white", face = "bold", size=10, angle=-90),
      axis.text.y = element_text(color = "white", face = "bold", size=10)
      ) + 
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
}

plot_02()