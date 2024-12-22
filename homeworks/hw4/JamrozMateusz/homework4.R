# do wykonania wykresów użyłem zedytowanych danych, które również załączam 

library(tidyverse)
library(plotly)
library(ggplot2)
library(readxl)
library(showtext)
library(geodata) 
library(ggplot2)
library(sf)
library(maps)      

font_add("Raleway_Variable", "Raleway-VariableFont_wght.ttf")
font_add("Raleway_Italic", "Raleway-Italic-VariableFont_wght.ttf")
font_add("TenorSans", "TenorSans-Regular.ttf")

showtext_auto(enable = TRUE)

urodzenia <- read_excel("Urodzenia żywe w Polsce 2007-2023.xlsx")
pozostawione <- read_excel("Noworodki pozostawione w szpitalu 2007-2023.xlsx")
data<- read_excel("2023_PieczaZastepcza.xlsx",sheet=1)

# wykres 1
pozostawione_long1 <- pozostawione %>%
  filter(Województwa == "Polska") %>% 
  pivot_longer(
    cols = -Województwa,
    names_to = "Rok",
    values_to = "Noworodki_Pozostawione")

pozostawione_long1$Rok <- as.numeric(gsub(".*\\.", "", pozostawione_long1$Rok))

ggplot(data = pozostawione_long1, aes(x = Rok, y = Noworodki_Pozostawione)) +
  geom_line(color = "blue", size = 1.5) +
  geom_point(color = "black", size = 3) +
  scale_y_continuous(limits = c(0,1000))+
  labs(
    title = "Noworodki pozostawione w szpitalach w Polsce (2007-2023)",
    x = "Rok",
    y = "Liczba noworodków"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "TenorSans",face="bold", size = 18),
    axis.title = element_text(family = "TenorSans",face="bold", size = 14))

# wykres 2
urodzenia_pol <- as.numeric(urodzenia[nrow(urodzenia), -1]) 
pozostawione_pol <- as.numeric(pozostawione[nrow(pozostawione), -1])

lata <- as.numeric(colnames(urodzenia)[-1]) 

podzielnosc <- pozostawione_pol / urodzenia_pol * 100 

dane <- data.frame(
  Rok = lata,
  Podzielnosc = podzielnosc
)

ggplot(dane, aes(x = Rok, y = Podzielnosc)) +
  geom_line(color = "navy", size = 1.5) +
  geom_point(color = "navy", size = 3) +
  scale_x_continuous(breaks = seq(min(lata), max(lata), by = 1)) +  
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0,0.25)) +  
  labs(
    title = "Odsetek noworodków pozostawionych w szpitalu w stosunku do urodzeń żywych",
    subtitle = "Dane dla Polski w latach 2007-2023",
    x = "Rok",
    y = "Odsetek (%) pozostawionych noworodków"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(family = "TenorSans",face="bold", size = 18),
    plot.subtitle = element_text(family = "TenorSans",face="bold", size = 12),
    axis.title = element_text(family = "TenorSans",face="bold", size = 14)
    )

# wykres 3
data<-rename(data, wojewodztwo=Wyszczególnienie)

data <- data %>%
  mutate(
    oblozenie = Wychowankowie / Miejsca,wojewodztwo = str_to_lower(wojewodztwo),
    kategoria = case_when(
      oblozenie <= 1 ~ "limit miejsc nieprzekroczony",
      oblozenie > 1 ~ "limit miejsc przekroczony",
      
    )
  )

poland_map <- map_data("state") %>%
  filter(region == "poland") %>%
  mutate(subregion = str_to_lower(subregion))

map <- geodata::gadm("Poland", level = 1, path = tempdir())

map <- st_as_sf(map)  

map <- map %>%
  mutate(NAME_1 = str_to_lower(NAME_1))  

map_data <- map %>%
  left_join(data, by = c("NAME_1" = "wojewodztwo"))

ggplot(data = map_data) +
  geom_sf(aes(fill = kategoria), color = "black", size = 0.2) +  
  scale_fill_manual(
    values = c("limit miejsc nieprzekroczony" = "darkgreen", 
               "limit miejsc przekroczony" = "red"),
    name = ""
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    plot.title = element_text(family = "TenorSans", face = "bold", size = 22,  hjust = 0.5),
    plot.subtitle = element_text(family = "TenorSans", size = 16, color = "gray40", hjust = 0.5),
    plot.caption = element_text(family = "TenorSans", face = "bold", size = 12),
    legend.text = element_text(family = "TenorSans", size = 14),
    axis.text = element_blank(),  
    axis.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),  
    panel.background = element_rect(fill = "lightgray", color = NA),  
    legend.background = element_rect(fill = "white", color = NA),  
    legend.key.size = unit(1, "cm"),  
    legend.key.width = unit(1, "cm"),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
      ) +
  labs(
    title = "Obciążenie placówek opiekuńczo-wychowawczych w 2023 roku w Polsce",
    subtitle = "Status limitów miejsc",
    x = "",
    y = "",
    caption = "Stan w dniu 31.12.2023"
  )


# wykres 4
data1 <- data[data$wojewodztwo != "ogółem", ]
data1$Wychowankowie_na_Placowke <- data1$Wychowankowie / data1$Placówki
ggplot(data1, aes(x = reorder(wojewodztwo, -Wychowankowie_na_Placowke), y = Wychowankowie_na_Placowke)) +
  geom_col(fill = "skyblue", color = "black") +
  labs(
    title = "Średnia liczba wychowanków na placówkę w poszczególnych województwach",
    x = "Województwo",
    y = "Średnia liczba wychowanków na placówkę"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "TenorSans", face = "bold", size = 22),
    axis.title = element_text(family = "TenorSans",face="bold", size = 14),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1, family = "TenorSans")
  )+
  geom_text(
    aes(label = round(Wychowankowie_na_Placowke, 1)),
    position = position_stack(vjust = 1.03), size = 3, family = "TenorSans", color = "black"
  )

