# 
# zwraca liczbe emotek ktore ktos DOSTAL
# 
# argumenty:
#   tabela,
#   dla kogo
# 
# zwraca:
#   who_sent    number_of_emotes
#  Kuba Rybak         5

library(dplyr)

emote_number <- function(data, who){
  dane <- data %>% 
    filter(!reaction == "NULL") %>% 
    filter(who_sent == who) %>% 
    group_by(who_sent) %>%
    summarise(number_of_emotes = n()) %>%
    mutate(who_sent = who)
  return(dane)
}