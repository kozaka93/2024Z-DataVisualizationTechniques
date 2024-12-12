###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 6            ###
########################################### 

library(ggplot2)
library(dplyr)
library(SmarterPoland)


## Zadanie 1
# Zadania są zamieszczone w pliku .pdf w folderze lab5.
# Dane potrzebne do odtworzenia wizualizacji wczytujemy następująco:

df <- read.csv(file = "https://raw.githubusercontent.com/MI2-Education/2023Z-DataVisualizationTechniques/main/homeworks/hw1/house_data.csv", 
               encoding = "UTF-8")

f1 <- df %>% 
  select(id, price, sqft_living, waterfront) %>% 
  mutate(size_of_the_house = case_when(sqft_living > 0 & sqft_living <= 2000 ~ "(0, 2000]",
                                       sqft_living > 2000 & sqft_living <= 4000 ~ "(2000, 4000]",
                                       sqft_living > 4000 & sqft_living <= Inf ~ "(4000, +Inf)")) %>% 
  mutate(price_in_k = price / 1000)

plot1 <- ggplot(f1, aes(x = price_in_k, y = size_of_the_house, color = factor(waterfront))) +
  geom_boxplot() + theme_bw() +
  theme(legend.position = "top") +
  labs(x = "Price [1k $]",
       y = "Living area [sqft]",
       color = "Waterfront") +
  scale_color_manual(values = c("darkred","navy"))

plot1



## Zadanie 2
# Posługując się danymi z *Zadania 1* należy odwzorować poniższy wykres.


head(df)

f2 <- df %>% 
  group_by(zipcode) %>% 
  summarise(n = n()) %>% 
  filter(n > quantile(n, 0.95)) %>% 
  left_join(df) %>% 
  select(id, price, grade, zipcode, yr_built) %>% 
  filter(grade == 8 | grade == 9)


p2 <- ggplot(f2, aes(x = factor(zipcode), y = price/1000)) +
geom_boxplot() +
facet_grid(grade ~ yr_built) +
theme_bw() +
labs(x = "Zipcode",
     y = "Price [1k $]",
     title = "Distribution of property prices for zip codes in 2013−2014
              for properties with grade of 8 or 9.",
     subtitle = "We consider the 5% most numerous zip codes.")

p2



## patchwork

# install.packages("patchwork")
# install.packages("grid")
# install.packages("gridExtra")


library(patchwork)


## Zadanie 3
# Przygotuj tabelę z podsumowaniem ile nieruchomości znajduje się dla poszczególnych kodów pocztowych i lat z wykresu.








## Zadanie 4
# Utwórz nową zmienną `is_renovated`, która będzie przyjmować wartość TRUE jeżeli była renowacja i FALSE gdy jej nie było. 
# Przygotuj wykres ukazujący rozkład piwerzchni mieszkanel dla domów w podziale na liczbę pięter i status renowacji.

head(df)

f4 <- df %>% 
  mutate(was_renovated = case_when(yr_renovated == 0 ~ FALSE,
                                   yr_renovated != 0 ~ TRUE)) %>% 
  select(id, sqft_living, floors, was_renovated)

f4_renovated <- f4 %>% 
  filter(was_renovated == TRUE)

f4_not_renovated <- f4 %>% 
  filter(was_renovated == FALSE)


p4a <- ggplot(f4_renovated, aes(x = floors, y = sqft_living, color = as.factor(floors))) +
  geom_violin()

p4b <- ggplot(f4_not_renovated, aes(x = floors, y = sqft_living, color = as.factor(floors))) +
  geom_violin()


p4a + p4b

p4 <- ggplot(f4, aes(x = floors, y = sqft_living, color = as.factor(floors))) +
  geom_violin() +
  facet_wrap(~was_renovated)

p4

## Zadanie 5 - stworzyć wykres gęstości brzegowych:
# a) wykres punktowy dwóch wskaźników + kolor
# b) dodać po lewej rozkład zmiennej death.rate
# c) dodać na dole rozkład zmiennej birth.rate

head(countries)





## ggrepel
# https://ggrepel.slowkow.com/articles/examples.html

install.packages("ggrepel")

library(ggrepel)




## Zadanie 6
# Narysuj wykres punktowy zależności między wskaźnikiem urodzeń a wskaźnikiem śmierci 
# oraz podpisz punkty o najniższym i najwyższym wskaźniku śmiertelności (nazwą kraju).




