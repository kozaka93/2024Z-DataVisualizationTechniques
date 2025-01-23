spozycieSummarizing <- function() {
  library(dplyr)
  library(stringr)
  library(tools)
  source("functions/mergeDatasetsFunction.R")
  
  exercise_days <- read.csv("data/training_data.csv")
  exercise_days$Date <- as.POSIXct(exercise_days$Date, format = "%Y-%m-%d %H:%M:%S")
  exercise_days$Date <- as.Date(exercise_days$Date)
  
  exercise_days <- exercise_days |>
    select(c("Date", "Owner", "Workout.Name")) |>
    distinct()
  
  ludwik <- read.csv("data/spozycieLudwik.csv", sep=",")
  maxim <- read.csv("data/spozycieMaxim.csv", sep=",")
  yahor <- read.csv("data/spozycieYahor.csv", sep=",")
  
  data <- mergeDatasets(ludwik, maxim, yahor)
  data$DATA <- as.POSIXct(data$DATA, format = "%d.%m.%Y")
  
  data <- data %>%
    pivot_longer(cols = c(WODA, HERBATA, KAWA, INNE), 
                 names_to = "Drink", 
                 values_to = "Amount")
  
  data <- data %>%
    left_join(exercise_days, by = c("Owner", "DATA" = "Date"))  %>%
    mutate(Trening = if_else(!is.na(Workout.Name), TRUE, FALSE))
  
  data <- data %>%
    select(-Workout.Name)
  
  write.csv(data, file = "data/spozycie.csv", row.names = FALSE)
}

