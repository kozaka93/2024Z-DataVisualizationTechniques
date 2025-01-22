#zwraca tablelkę o kolumnach: is_holidays (wakacje letnie), day_of_week, count

library(dplyr)
library(tidyr)

message_count_by_dow <- function(data, who){
  df <- data
  
  res <- df %>% filter(who_sent == who) %>% 
    mutate(words = strsplit(as.character(content), "\\s+")) %>% # Split text into words
    unnest(words) %>% 
    mutate(day_of_week = weekdays(date_of_message)) %>% 
    mutate(is_holidays = case_when(month(date_of_message) >= 7 & month(date_of_message) <= 9 ~ TRUE,
                                   TRUE ~ FALSE)) %>% 
    group_by(is_holidays, day_of_week) %>% 
    summarise(count = n()) %>%
    mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
}