library(ggplot2)
library(dplyr)
library(tidyr)
library(showtext)

font_add_google("Raleway", "Raleway")
showtext_auto() 


# WYKRES 1 - NOWORODKI POZOSTAWIONE W SZPITALU, TOTAL DLA CAŁEGO KRAJU

pozostawione_noworodki <- read.csv("clean_noworodki_pozostawione_w_szpitalu.csv") %>% 
  pivot_longer(!Województwa, names_to='Rok', values_to = 'Liczba_pozostawionych_noworodków') %>% 
  mutate(Rok = as.numeric(sub('.', '', Rok))) %>% 
  rename(Województwo = Województwa) %>% 
  mutate(Województwo = ifelse(Województwo == "Polska", 
                              Województwo, 
                              paste0("woj. ", Województwo)))

pozostawione_noworodki_Polska <- pozostawione_noworodki %>% 
  filter(Województwo == "Polska") %>% 
  ggplot(aes(x = Rok, y = Liczba_pozostawionych_noworodków)) +
  geom_line(color = "#e62248", size = 2) +
  geom_vline(xintercept=2015.33, linetype="dashed", color="#884292", size=2) +
  annotate("label", x = 2015.33, y = 900, 
           label = "wprowadzenie programu 500+", 
           fill = "#884292", 
           color = "white", 
           family = "Raleway", 
           label.size = 0, 
           size = 5) +
  labs(title = "Noworodki pozostawione w szpitalu nie ze względów zdrowotnych \nw Polsce w latach 2007-2023", 
       subtitle = "Ministerstwo Zdrowia, dane udostępniane na prośbę Fundacji Gajusz", 
       y = "Liczba pozostawionych noworodków") +
  theme_bw() +
  theme(
    axis.title = element_text(family = "Raleway", size = 17),
    axis.text = element_text(family = "Raleway", size = 15),
    plot.title = element_text(family = "Raleway", size = 21), 
    plot.subtitle = element_text(family = "Raleway", size = 17)
  )
