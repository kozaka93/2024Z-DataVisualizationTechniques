# 
# zwraca najczestsze slowa (conajmniej 10 razy) dla danego kwartalu
# dla kazdego miesiaca
# 
# trzeba jakos wybrac top 3 normalne albo cos
# 
# zwraca kolumny:
# month, word, number

library(dplyr)

most_frequent_words_for_months_in_quarter <- function(data, which_quarter){
  
  data <- data %>% 
    mutate(month = month(ymd_hms(date_of_message))) %>% 
    mutate(quarter = case_when(
      month <=3 ~ 1,
      month <= 6 ~ 2,
      month <= 9 ~ 3,
      month <= 12 ~ 4)) %>% 
    filter(quarter == which_quarter) %>% 
    select(content, month) %>% 
    unnest_tokens(word, content) %>% 
    select(word, month) %>% 
    group_by(month, word) %>% 
    summarise(number = n(), .groups = "drop") %>% 
    filter(number >= 10) %>% 
    arrange(month, -number)
  return(data)
}