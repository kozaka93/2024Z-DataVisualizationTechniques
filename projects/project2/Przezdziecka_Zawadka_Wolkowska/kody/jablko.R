library(readxl)
library(dplyr)
library(wordcloud2)
path <- "Dane_posiłki_twd2.xlsx"

food_A <- read_excel(path, range = "A1:W130", col_names = TRUE, sheet = 1)
food_P <- read_excel(path, range = "A1:W118", col_names = TRUE, sheet = 2)
food_O <- read_excel(path, range = "A1:W120", col_names = TRUE, sheet = 3)

food_A <- food_A %>% mutate(
  godzina_posilku = substr(godzina_posilku, start = 12, stop = 19),
  data_posilku = as.Date(substr(data_posilku, start = 1, stop = 10), format = "%Y-%m-%d")
)
food_P <- food_P %>% mutate(
  godzina_posilku = substr(godzina_posilku, start = 12, stop = 19),
  data_posilku = as.Date(substr(data_posilku, start = 1, stop = 10), format = "%Y-%m-%d")
)
food_O <- food_O %>% mutate(
  godzina_posilku = substr(godzina_posilku, start = 12, stop = 19),
  data_posilku = as.Date(substr(data_posilku, start = 1, stop = 10), format = "%Y-%m-%d")
)


combined_food <- bind_rows(food_A %>% mutate(osoba = "A"),
                           food_P %>% mutate(osoba = "P"),
                           food_O %>% mutate(osoba = "O"))

combined_words <- combined_food %>%
  mutate(skladniki = strsplit(as.character(skladniki), ",")) %>%
  unnest(skladniki) %>%
  mutate(skladniki = trimws(skladniki)) %>%
  count(skladniki, sort = TRUE)

top_combined_words <- combined_words[1:50, ]

wordcloud2(top_combined_words,size = 0.5, color='random-light', backgroundColor="transparent",shape= 'star', hover = NULL)
## napotkałyśmy problemy z dodawaniem zdjecia jako tło i stworzyłyśmy plik 
## ze składnikami na podstawie ramki "top_combined_words" i wygenerowałyśmy jabłko na stronie:
## https://www.wordclouds.com/ 
