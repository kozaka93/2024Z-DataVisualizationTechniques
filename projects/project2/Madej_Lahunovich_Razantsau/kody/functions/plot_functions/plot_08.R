plot_08_prepareData <- function(data = read.csv("data/training_data.csv")){
  data %>% 
    mutate(
      seriesWeight = Reps * Weight,
      Body.Part = factor(Body.Part, levels = c("Chest", "Back", "Arms", "Legs", "Shoulders", "Core")),
      Owner = factor(Owner, levels = c("Ludwik", "Yahor", "Maxim"))
    )
}

plot_08 <- function(
    data = plot_08_prepareData(),
    people = c("Ludwik", "Maxim", "Yahor")
    ){
  library(dplyr)
  library(ggplot2)
  source("functions/adjust_plot.R")
  
  if (length(people) != 3){
    data <- data %>% 
      filter(Owner %in% people)
  }
  adjust_plot(
    ggplot(
        data = data,
        aes(
          x = Body.Part,
          y = seriesWeight,
          fill = Owner,
          color = Owner
        )
      ) +
      geom_boxplot() +
      labs(
        title = "Plot of total weight lifted during\n a single set by body part",
        x = "Body Part",
        y = "Weight"
      ) +
      scale_y_continuous(
        breaks = seq(0, 1600, by=100)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
      )
  ) +
    theme(
      # teksty na osiach
      axis.text = element_text(color = "white", face = "bold", size=11),
      axis.text.x = element_text(color = "white", face = "bold", size=10),
      axis.text.y = element_text(color = "white", face = "bold", size=10)
    ) +
    scale_color_manual(values = c(
      "Ludwik"="#067d12",
      "Yahor"="#0670a1",
      "Maxim"="#a18f06")
    )
}
