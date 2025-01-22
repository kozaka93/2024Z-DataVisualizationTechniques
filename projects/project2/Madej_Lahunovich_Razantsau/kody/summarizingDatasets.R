library(dplyr)
library(stringr)
source("functions/mergeDatasetsFunction.R")

ludwik <- read.csv("data/Ludwik.csv", sep=";")
maxim <- read.csv("data/Maxim.csv", sep=",")
yahor <- read.csv("data/Yahor_21.01.csv", sep=";")

yahor <- yahor %>% filter(Date >= "2024-12-05")
ludwik <- ludwik %>% mutate(Weight = as.double(sub(",", ".", Weight)))
yahor <- yahor %>% mutate(Weight = as.double(sub(",", ".", Weight)))
maxim <- maxim %>% rename(Workout.Duration = Duration)

yahor <- yahor %>% select(Date, Workout.Name, Exercise.Name, Set.Order, Weight, Workout.Notes, Workout.Duration, Reps, Notes)
ludwik <- ludwik %>% select(Date, Workout.Name, Exercise.Name, Set.Order, Weight, Workout.Notes, Workout.Duration, Reps,Notes)
maxim <- maxim %>% select(Date, Workout.Name, Exercise.Name, Set.Order, Weight, Workout.Notes, Workout.Duration, Reps, Notes)

data <- mergeDatasets(ludwik, maxim, yahor)
data <- data %>% mutate(
  Workout.Duration = paste(Workout.Duration, "2137", sep = " ")
) %>% 
  mutate(
    duration_hour = sapply(str_split(Workout.Duration, " "), `[`, 1),
    duration_minute = sapply(str_split(Workout.Duration, " "), `[`, 2)
  ) %>% 
  mutate(
    duration_minute = if_else(
      str_sub(duration_minute, -1) == "7", 
      as.numeric(str_sub(duration_hour, end = -2)), 
      as.numeric(str_sub(duration_minute, end = -2))),
    
    duration_hour = if_else(
      str_sub(duration_hour, -1) == "m",
      0, 
      as.numeric(str_sub(duration_hour, end = -2)))
  ) %>% 
  mutate(
    DurationInMinutes = 60*duration_hour+duration_minute
  ) %>% 
  select(Date, Workout.Name, Exercise.Name, Set.Order,Reps, Weight, Workout.Notes, DurationInMinutes, Owner, Notes)

body_parts <- read.csv("data/exercise_body_parts.csv") %>% 
  rename(Exercise.Name = Exercise)

data <- data %>% 
  left_join(
    body_parts,
    by = "Exercise.Name"
  ) %>% 
  mutate(
    Body.Part = if_else(
      is.na(Body.Part),
      "Unknown",
      Body.Part
    )
  )

data <- data %>% 
  mutate(Notes = if_else(
    Owner == "Ludwik",
    paste("(",Notes,")", sep=""),
    "")
    ) %>% 
  mutate(
    Exercise.Name = if_else(
      Owner == "Ludwik" & Notes != "()",
      paste(Exercise.Name, Notes),
      Exercise.Name
    )
  ) %>% 
  select(-Notes) 

write.csv(data, file = "data/training_data.csv", row.names = FALSE)
