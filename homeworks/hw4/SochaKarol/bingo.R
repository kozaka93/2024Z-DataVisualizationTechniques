library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(sf)
library(maps)
library(mapdata)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(devtools)

pallette = c('#303174', '#315ca8', '#b5e0f3', '#884292', '#8c2a64', '#e62248', '#e4007e', '#ea4f7f')

# WCZYTANIE DANYCH

# Liczba dzieci 0-18 lat w Polsce 2014-2013
d18 <- readxl::read_xlsx("C:/Users/kerel/OneDrive/Dokumenty/R_scripts/BINGO/Liczba dzieci 0-18 lat w Polsce 2014-2023.xlsx")

# Liczba osób w wieku 0-24 lata w Polsce, 2014-2023
d24 <- readxl::read_xlsx("C:/Users/kerel/OneDrive/Dokumenty/R_scripts/BINGO/Liczba osób w wieku 0-24 lata w Polsce, 2014-2023.xlsx")

# Noworodki pozostawione w szpitalu 2007-2023
np <- readxl::read_xlsx("C:/Users/kerel/OneDrive/Dokumenty/R_scripts/BINGO/Noworodki pozostawione w szpitalu 2007-2023.xlsx",
                        range = "A4:R25",         
                        col_names = TRUE) %>% 
  slice(-c(1:4))
colnames(np) <- c("Województwo", 2007:2023)

# Urodzenia żywe w Polsce 2007-2023
uz <- readxl::read_xlsx("C:/Users/kerel/OneDrive/Dokumenty/R_scripts/BINGO/Urodzenia żywe w Polsce 2007-2023.xlsx")

# Wychowankowie (0-24 lata) w pieczy zastępczej 2014-2023
w <- as.data.frame(readxl::read_xlsx("C:/Users/kerel/OneDrive/Dokumenty/R_scripts/BINGO/Wychowankowie (0-24 lata) w pieczy zastępczej 2014-2023.xlsx"))
colnames(w) <- c("Województwo", 2014:2023)

# TWORZENIE WYKRESÓW

# WYKRES 1

nw <- cbind(np[1],round(np[-1]/(uz[-1]/1000),1))

t_nw <- as.data.frame(t(nw)) %>% slice(-1) 
colnames(t_nw) <- nw$Województwo
t_nw <- t_nw %>% mutate(Rok = rownames(t_nw)) %>%
  relocate(Rok) %>% mutate(across(everything(), as.numeric))
rownames(t_nw) <- NULL

plot1 <- ggplot(t_nw, aes(x = Rok, y = Polska)) +
  geom_col(fill = "#b5e0f3") +
  geom_text(aes(label = Polska), vjust = -0.5, color = "white") +
  scale_x_continuous(breaks = seq(2007, 2023, by = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Wskaźnik liczby pozostawianych noworodków",
    subtitle = "w Polsce w latach 2007-2023",
    x = "Rok",
    y = "Liczba pozostawionych dzieci na 1000 urodzeń"
  ) +
  theme(
    plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(color = "white", hjust = 0.5, face = "bold"),
    panel.background = element_rect(fill = "#e4007e", color = "#e4007e"), 
    plot.background = element_rect(fill = "#e4007e", color = "#e4007e"), 
    legend.background = element_rect(fill = "transparent"), 
    legend.box.background = element_rect(fill = "transparent"), 
    legend.text = element_text(color = "white"),       
    legend.title = element_text(color = "white"),     
    axis.text.x = element_text(color = "white", angle = 30), 
    axis.text.y = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()
  )

plot1

# WYKRES 2

dw <- cbind(w[1],round((w[-1]/(d24[-1]/1000)),2))

t_dw <- as.data.frame(t(dw)) %>% slice(-1) 
colnames(t_dw) <- dw$Województwo
t_dw <- t_dw %>% mutate(Rok = rownames(t_dw)) %>%
  relocate(Rok) %>% mutate(across(everything(), as.numeric))
rownames(t_dw) <- NULL

plot2 <- ggplot(t_dw, aes(x = Rok, y = POLSKA)) +
  geom_line(color = "#b5e0f3", linewidth = 1.5) +
  geom_point(size = 2, color = "#b5e0f3") +
  scale_x_continuous(breaks = seq(2007, 2023, by = 1)) +
  labs(
    title = "Wskaźnik liczby wychowanków w pieczy zastępczej",
    subtitle = "w Polsce w latach 2014-2023",
    x = "Rok",
    y = "Liczba wcyhowanków na 1000 osób"
  ) +
  theme(
    plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(color = "white", hjust = 0.5, face = "bold"),
    panel.background = element_rect(fill = "#e4007e", color = "#e4007e"), 
    plot.background = element_rect(fill = "#e4007e", color = "#e4007e"),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"), 
    legend.text = element_text(color = "white"),      
    legend.title = element_text(color = "white"),     
    axis.text = element_text(color = "white"),       
    axis.title = element_text(color = "white"),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()
  )

plot2

# WYKRES 3

p_dw <- round(t_dw/10, 2) %>% 
  mutate(Rok = t_dw$Rok) %>% 
  filter(Rok == 2023) %>% 
  select(-c(Rok, POLSKA))

p_dw_long <- p_dw %>%
  pivot_longer(
    cols = everything(), 
    names_to = "Wojewodztwo", 
    values_to = "Procent"
  ) %>%
  mutate(Wojewodztwo = gsub("_", "-", Wojewodztwo)) %>% 
  mutate(Wojewodztwo = case_when(
    Wojewodztwo == "dolnośląskie" ~ "województwo dolnośląskie",
    Wojewodztwo == "kujawsko-pomorskie" ~ "województwo kujawsko-pomorskie",
    Wojewodztwo == "lubelskie" ~ "województwo lubelskie",
    Wojewodztwo == "lubuskie" ~ "województwo lubuskie",
    Wojewodztwo == "łódzkie" ~ "województwo łódzkie",
    Wojewodztwo == "małopolskie" ~ "województwo małopolskie",
    Wojewodztwo == "mazowieckie" ~ "województwo mazowieckie",
    Wojewodztwo == "opolskie" ~ "województwo opolskie",
    Wojewodztwo == "podkarpackie" ~ "województwo podkarpackie",
    Wojewodztwo == "podlaskie" ~ "województwo podlaskie",
    Wojewodztwo == "pomorskie" ~ "województwo pomorskie",
    Wojewodztwo == "śląskie" ~ "województwo śląskie",
    Wojewodztwo == "świętokrzyskie" ~ "województwo świętokrzyskie",
    Wojewodztwo == "warmińsko-mazurskie" ~ "województwo warmińsko-mazurskie",
    Wojewodztwo == "wielkopolskie" ~ "województwo wielkopolskie",
    Wojewodztwo == "zachodniopomorskie" ~ "województwo zachodniopomorskie",
    TRUE ~ stringr::str_to_title(Wojewodztwo))
  )

# Wczytanie mapy Polski z podziałem na województwa
poland_map <- ne_states(country = "Poland", returnclass = "sf")

# Połączenie danych procentowych z mapą na podstawie nazw województw
map_data <- poland_map %>%
  left_join(p_dw_long, by = c("name_pl" = "Wojewodztwo"))

# Tworzenie wykresu mapy
plot3 <- ggplot(data = map_data) +
  geom_sf(aes(fill = Procent), color = "black") +
  scale_fill_gradient(low = "#b5e0f3", high = "#303174", name = "Procent") + 
  labs(
    title = "Procent osób w wieku 0-24 lat pod pieczą zastępczą",
    subtitle = "w roku 2023"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(color = "white", hjust = 0.5, face = "bold"),
    panel.background = element_rect(fill = "#e4007e", color = "#e4007e"), 
    plot.background = element_rect(fill = "#e4007e", color = "#e4007e"), 
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"), 
    legend.text = element_text(color = "white"),      
    legend.title = element_text(color = "white"),      
    axis.text = element_blank(),         
    axis.title = element_text(color = "white"),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()
  )

plot3

# Zapisanie wykresów do oddzielnych plików

ggsave("wykres_1.png",
       plot = plot1, 
       width = 1950, height = 1300, units = "px")

ggsave("wykres_2.png",
       plot = plot2, 
       width = 1950, height = 1300, units = "px")

ggsave("wykres_3.png",
       plot = plot3, 
       width = 1950, height = 1300, units = "px")
