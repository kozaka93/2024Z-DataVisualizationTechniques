# 
# zwraca liczbe wiadomosci od polnocy do 5:59
# 
# argumenty:
#   data
#   dla kogo
#   
# zwraca:
#   who_sent  message_number
#  Kuba Rybak      243

library(dplyr)
library(lubridate)

message_number_between_0_and_6 <- function(data, who){
  dane <- data %>% 
    mutate(hour = hour(ymd_hms(date_of_message))) %>% 
    filter(who_sent == who) %>% 
    select(who_sent, hour) %>% 
    filter(hour >=0 & hour <= 5) %>% 
    group_by(who_sent) %>% 
    summarise(message_number = n()) 
  return(dane)
}