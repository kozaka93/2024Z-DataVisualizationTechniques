library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(stringr)
library(geodata)
install.packages("geodata")
install.packages("showtext")
install.packages("sysfonts")
library(showtext)
library(sysfonts)

font_add("czcionka1", "Raleway-Medium.ttf")
showtext_auto()

urodzenia <- read_excel("Urodzenia żywe w Polsce 2007-2023.xlsx")
#noworodki <- read_excel("Noworodki pozostawione w szpitalu 2007-2023.xlsx")

colnames(urodzenia) <- gsub("\\.0$", "", colnames(urodzenia))
colnames(noworodki) <- gsub("\\.0$", "", colnames(noworodki))

polska <- geodata::gadm("Poland", level = 1, path = tempdir())

polska <- st_as_sf(polska)

# URODZENIA MAPA 

urodzenia2000 <- urodzenia %>%
  filter(Województwo != "POLSKA") %>%
  mutate(Województwo = str_to_title(Województwo)) %>%
  inner_join(polska, by = c("Województwo" = "NAME_1")) %>%
  select(1,2,10,18,29) %>%
  pivot_longer(cols = starts_with("20"),  
               names_to = "rok",          
               values_to = "ilosc")
  
urodzenia2000 <- st_as_sf(urodzenia2000)

u <- ggplot(data = urodzenia2000) +
  geom_sf(aes(fill = ilosc)) +
  facet_wrap(~rok) +
  scale_fill_gradient(low = "#B5E0F3", high = "#E62248") +
  labs(title = "Urodzenia żywe w polsce w danych latach",
       fill = "Ilość urodzeń") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 40, face = "bold",family = "czcionka1"), 
    legend.position = "right",    
    legend.direction = "vertical", 
    legend.title = element_text(size = 30,family = "czcionka1"), 
    axis.text = element_blank(),     
    axis.ticks = element_blank(),     
    panel.grid = element_blank(), legend.text = element_text(size = 25,family = "czcionka1"),  
    strip.text = element_text(size = 40,family = "czcionka1")     
  )

# WYKRES LINIOWY
  
urodzenia2 <- urodzenia %>%
  pivot_longer(cols = starts_with("20"),  
               names_to = "rok",          
               values_to = "ilosc") %>%  
  group_by(Województwo, rok) %>%
  summarise(ilosc = sum(ilosc), .groups = "drop") %>%
  mutate(Województwo = str_to_title(Województwo))
  

urodzenia2$rok <- as.numeric(urodzenia_line$rok)


paleta <- c(
  "Dolnośląskie" = "#303174",
  "Kujawsko-Pomorskie" = "#315CAB",
  "Lubelskie" = "#B5E0F3",
  "Lubuskie" = "#884292",
  "Łódzkie" = "#8C2A64",
  "Małopolskie" = "#E62248",
  "Mazowieckie" = "#E4007E",
  "Opolskie" = "#EA4F7F",
  "Podkarpackie" = "#6a6bb0",
  "Podlaskie" = "#829ecf",
  "Pomorskie" = "#aec7e8",
  "Śląskie" = "#a676ad",
  "Świętokrzyskie" = "#bf5895",
  "Warmińsko-Mazurskie" = "#c4687a",
  "Wielkopolskie" = "#c5b0d5",
  "Zachodniopomorskie" = "#e388ba",
  "Polska" = "#AEC7D1"
)

urodzenia0723 <- plot_ly(data = urodzenia2, 
                     x = ~rok, 
                     y = ~ilosc, 
                     color = ~Województwo, 
                     colors = paleta,
                     type = 'scatter', 
                     mode = 'lines+markers',
                     text = ~paste(Województwo, "<br>Rok:", rok, "<br>Urodzenia:", ilosc),
                     hoverinfo = 'text') %>%
  layout(
    title = "Liczba urodzeń w województwach w latach 2007 - 2023",
    xaxis = list(title = "Rok",
                 title_standoff = 5),
    yaxis = list(title = "Liczba urodzeń",
                 tickformat = "0"),
    legend = list(title = list(text = "Województwo")),
    hovermode = "closest",
    font = list(family = "czcionka1", size = 14),  
    margin = list(l = 50, r = 50, t = 50, b = 50)  
  )

urodzenia0723


# nie używam

# colnames(noworodki)[2:ncol(noworodki)] <- seq(2007, 2023)
# 
# noworodki1 <- noworodki %>% 
#   slice(c(8:24)) %>%
#   filter(`Noworodki pozostawione w szpitalu nie ze względów zdrowotnych w latach 2007-2023` != "Polska") %>%
#   pivot_longer(cols = starts_with("20"),  
#                names_to = "rok",          
#                values_to = "ilosc") %>%  
#   mutate(ilosc = as.numeric(ilosc)) %>%
#   group_by(`Noworodki pozostawione w szpitalu nie ze względów zdrowotnych w latach 2007-2023`, rok) %>%
#   summarise(ilosc = sum(ilosc), .groups = "drop") %>%
#   mutate(`Noworodki pozostawione w szpitalu nie ze względów zdrowotnych w latach 2007-2023` = str_to_title
#          (`Noworodki pozostawione w szpitalu nie ze względów zdrowotnych w latach 2007-2023`))
# 
# noworodki0723 <- plot_ly(data = noworodki1, 
#                          x = ~rok, 
#                          y = ~ilosc, 
#                          color = ~`Noworodki pozostawione w szpitalu nie ze względów zdrowotnych w latach 2007-2023`, 
#                          colors = paleta,
#                          type = 'scatter', 
#                          mode = 'lines+markers',
#                          text = ~paste(`Noworodki pozostawione w szpitalu nie ze względów zdrowotnych w latach 2007-2023`, "<br>Rok:", rok, "<br>Urodzenia:", ilosc),
#                          hoverinfo = 'text') %>%
#   layout(
#     title = "Noworodki pozostawione w szpitalu nie ze względów zdrowotnych w latach 2007-2023",
#     xaxis = list(title = "Rok",
#                  title_standoff = 5),
#     yaxis = list(title = "Ilość porzuconych noworodków",
#                  tickformat = "0"),
#     legend = list(title = list(text = "Województwo")),
#     hovermode = "closest",
#     font = list(family = "czcionka1", size = 14),  
#     margin = list(l = 50, r = 50, t = 50, b = 50)  
#   )
# 
# noworodki0723
