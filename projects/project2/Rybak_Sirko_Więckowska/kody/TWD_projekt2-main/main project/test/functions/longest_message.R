# 
# zwraca najdluzsza wiadomosc kiedykolwiek
# argumenty:
#   tabela
#   dla kogo
#   
# zwraca:
# who_sent       max_length
# Kuba Rybak        431

library(dplyr)

longest_message <- function(data, who){
  dane <- data %>% 
    filter(who_sent == who) %>% 
    select(who_sent, content) %>% 
    mutate(max_length = nchar(content)) %>% 
    arrange(-max_length) %>% 
    slice(1) %>% 
    select(who_sent, max_length)
  return(dane)
}