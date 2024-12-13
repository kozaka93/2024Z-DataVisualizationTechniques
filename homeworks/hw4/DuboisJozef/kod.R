urodzenia <- read.csv("Urodzenia-zywe-w-Polsce-2007-2023.csv")
noworodki_pozostawione <- read.csv("Noworodki-pozostawione-w-szpitalu-2007-2023.csv")
wych_w_pieczy_zastepczej <- read.csv("Wychowankowie-_0-24-lata_-w-pieczy-zastepczej-2014-2023.csv")

library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(sf)
library(patchwork)
library(RColorBrewer)
library(tidyr)


temp = c("mazowieckie")


pivot_longer(noworodki_pozostawione, cols = X2007:X2023, names_to = "rok", values_to = "l_noworodkow") %>% 
  mutate(Rok = as.numeric(substr(rok, 2, 5))) %>% 
  filter(Wojewodztwa %in% temp) %>% 
  ggplot(aes(x = Rok, y = l_noworodkow, fill = Wojewodztwa)) +
  geom_line()
  

install.packages("shiny")
