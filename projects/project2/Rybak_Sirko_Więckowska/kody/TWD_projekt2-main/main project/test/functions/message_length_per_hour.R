# 
# zwraca srednia dlugosc wiadomosci (w znakach + spacje + wszytko, dlugosc stringa)
# dla danej osoby
# 
# argumenty:
#   tabela
#   dla kogo
# 
# zwraca kolumny:
#   hour(np. 12), mean_length, hour_range(np. 12-13 to jest zeby na wykresie ladniej wygladalo)

library(dplyr)
library(lubridate)

message_length_per_hour <- function(data, who){
  
  full_hours <- tibble(hour = 0:23)
  
  pre_dane <- data %>% 
    mutate(hour = hour(ymd_hms(date_of_message))) %>% 
    filter(who_sent == who) %>% 
    select(content, hour) %>% 
    mutate(message_length = nchar(content)) 
  
  
  dane <- full_hours %>%
    left_join(pre_dane, by = "hour") %>%
    mutate(length = replace_na(message_length, 0))%>%
    mutate(hour_range = paste0(hour,"-", hour+1))
  return(dane)
}