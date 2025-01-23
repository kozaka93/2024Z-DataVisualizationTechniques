library(dplyr)
library(stringr)
library(tidyr)

data <- read.csv("data/training_data.csv")
# 01 - favourite exercise
personal_data_01 <- function(data=read.csv("data/training_data.csv"), name){ 
  data %>% 
    filter(Owner %in% name) %>% 
    group_by(Exercise.Name, Owner) %>% 
    summarise(Set_count = n()) %>% 
    group_by(Owner) %>% 
    filter(Set_count == max(Set_count)) %>% 
    ungroup()
}
#View(personal_data_01(data=data,name="Ludwik"))


# 02 - weight lifted during the trainig - mean/max
personal_data_02 <- function(data=read.csv("data/training_data.csv"), name){
  data %>% 
    filter(Owner %in% name) %>% 
    mutate(weight_lifted = as.numeric(Reps) * as.numeric(Weight)) %>% 
    group_by(Date, Owner) %>% 
    summarise(
      weight_lifted_during_training = sum(weight_lifted, na.rm = TRUE)
    ) %>% 
    group_by(Owner) %>% 
    summarise(
      mean_weight_lifted_during_training = round(mean(weight_lifted_during_training), 2),
      max_weight_lifted_during_training = max(weight_lifted_during_training)
    ) -> statistics
  statistics
  # data %>% 
  #   filter(Owner == name) %>% 
  #   group_by(Owner) %>% 
  #   summarise(
  #     mean_weight = mean(Weight, na.rm = TRUE),
  #     max_weight = max(Weight, na.rm = TRUE)
  #   ) -> statistics
}

#View(personal_data_02(data=data,name="Ludwik"))

# 03/04 - najdłuższa przerwa/najdłuższy streak
personal_data_03_04 <- function(data=read.csv("data/training_data.csv"), name){
 dates_on_gym <- (data %>% 
    filter(Owner %in% name) %>%
    mutate(Date = str_sub(Date, 1, 10)))$Date
 
 dates_on_gym <- c(as.Date("2024-11-30"), as.Date(dates_on_gym), as.Date("2025-01-23"))
 dates_on_gym <- unique(as.Date(dates_on_gym))
 dates_on_gym <- sort(dates_on_gym)
 
 longest_break <- as.numeric(
   max(diff(dates_on_gym)) - 1, 
   units = "days"
   )
 
 daily_diff <- c(0, diff(sort(dates_on_gym)))
 
 longest_streak <- 0
 current_streak <- 0
 
 for (i in seq_along(daily_diff)){
   if (daily_diff[i] == 1){
     current_streak <- current_streak + 1
     if (current_streak > longest_streak) {
       longest_streak <- current_streak
     }
   } else {
     current_streak <- 0
   }
 }
 
 
 return(data.frame(longest_streak, longest_break))
}
#View(personal_data_03_04(name = "Ludwik"))

name = "Ludwik"

# 5 ilosc powtorzeni na czesc ciala
personal_data_05 <- function(
    data = read.csv("data/training_data.csv"),
    name,        
    body_part    # vector or single value
    ) {
  
  data %>% 
    filter(Owner %in% name, Body.Part %in% body_part) %>% 
    group_by(Body.Part) %>% 
    summarise(
      min_reps = min(Reps),
      mean_reps = round(mean(Reps), 2),
      max_reps = max(Reps)
    )
}
#View(personal_data_05(name = "Ludwik", body_part = "Chest"))

consumption <- read.csv("data/spozycie.csv")

# 6_8 consumption statistics
personal_data_6_7_8 <- function(
    consumption = read.csv("data/spozycie.csv"),
    name = c("Ludwik", "Yahor", "Maxim"),
    categories = c("WODA", "KAWA", "HERBATA", "INNE")
    ) {
  consumption %>% 
    filter(
      Owner %in% name,
      Drink %in% categories,
    ) %>% 
    group_by(Owner, Drink) %>% 
    summarise(
      min_consumption = min(Amount),
      mean_consumption = round(mean(Amount), 2),
      max_consumption = max(Amount)
    ) %>% 
    pivot_wider(
      names_from = Owner,
      values_from = ends_with("consumption")
    )
  
}

#View(personal_data_6_7_8(name = "Ludwik", categories = c("WODA", "KAWA")))
