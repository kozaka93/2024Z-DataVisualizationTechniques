#
# zwraca najdluzsze xd, 
# argumenty :
# tabela, kto(kogo xd chcemy najdluzsze)
# 
# zwraca:
#   who_sent         word           length_xd
#   Kuba Rybak     XDDDDDDDDDDD        12

library(dplyr)
library(tidytext)

longest_xd <- function(data, who){
  
  check_xd <- function(x) {
    if (grepl("^X[D]+$", x)) {
      return(nchar(x))
    } else {
      return(0)
    }
  }
  
  dane <- data %>% 
    select(who_sent, content) %>% 
    filter(who_sent == who) %>% 
    unnest_tokens(word, content) %>% 
    mutate(word = toupper(word)) %>% 
    mutate(length_xd = sapply(word, check_xd)) %>% 
    filter(length_xd > 0) %>% 
    arrange(-length_xd) %>% 
    slice(1)
  return(dane)
}