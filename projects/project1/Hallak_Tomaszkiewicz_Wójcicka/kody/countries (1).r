install.packages("ggplot2")
install.packages("dplyr")
install.packages("maps")
install.packages("maptools")
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")

library(ggplot2)
library(dplyr)
library(maps)
library(maptools)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

countries <- read.csv('/Users/oliwi/Desktop/R2/countries.csv')

countries <- countries %>%
  separate(Country.Overall.MHQ.Score, into = c("Country", "MHQ"), sep = ";", remove = TRUE)

world_map <- map_data("world")

missing_countries <- countries %>%
  anti_join(world_map, by = c("Country" = "region"))

countries_in_worldmap <- countries %>%
  mutate(Country = case_when(
    Country == "Congo Democratic Republic" ~ "Democratic Republic of the Congo",
    Country == "Cote d'Ivoire" ~ "Ivory Coast",
    Country == "Trinidad and Tobago" ~ "Trinidad",
    Country == "United Kingdom" ~ "UK",
    Country == "United States" ~ "USA",
    TRUE ~ Country 
  ))  

new_row <- countries %>%
  filter(Country == "Trinidad and Tobago") %>%  
  mutate(Country = "Tobago")

countries_in_worldmap2 <- countries_in_worldmap %>%
  bind_rows(new_row) %>% 
  arrange(Country)

countries_in_worldmap3 <- countries_in_worldmap2 %>%
  mutate(MHQ = as.numeric(MHQ),  # Upewnij si??, ??e MHQ jest liczb??
         MHQ_category = cut(
           MHQ,
           breaks = c(0, 50, 60, 70, 80, 100),  
           labels = c("0-50", "51-60", "61-70", "71-80", "81-100"), 
           right = TRUE
         ))

merged_data <- world_map %>%
  left_join(countries_in_worldmap3, by = c("region" = "Country")) %>% 
  mutate(MHQ_category = ifelse(is.na(MHQ_category), "No Data", as.character(MHQ_category)))


ggplot(data = merged_data) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = MHQ_category)) +  
  scale_fill_manual(values = c("No Data" = "grey",  # Kolor dla krajow bez danych
                               "0-50" = "#cce5ff",  # Jasny niebieski
                               "51-60" = "#66b3ff",  # Niebieski
                               "61-70" = "#3399ff",  # Ciemniejszy niebieski
                               "71-80" = "#0073e6",  # Granatowy
                               "81-100" = "#0056b3"),  # Ciemnoniebieski
                    name = "MHQ") +
  labs(title = "Overall MHQ Score")+
  theme_minimal() +
  theme(legend.position = "right",
        axis.title.x = element_blank(),  # Ukrycie etykiet osi X
        axis.title.y = element_blank(),  # Ukrycie etykiet osi Y
        axis.text.x = element_blank(),  # Ukrycie etykiet osi X
        axis.text.y = element_blank(),  # Ukrycie etykiet osi Y
        axis.ticks = element_blank(),     # Ukrycie znacznikow osi
        panel.grid.major = element_blank(),  # Ukrycie siatki g??ownej
        panel.grid.minor = element_blank(),  # Ukrycie siatki pomocniczej
        plot.title = element_text(size = 16, hjust = 0.5))+  # Dostosowanie tytu??u
  coord_fixed(ratio = 1.3)
