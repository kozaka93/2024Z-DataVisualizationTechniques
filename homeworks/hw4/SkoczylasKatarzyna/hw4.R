library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)

zywe_urodzenia <- read.csv("Dane/Żywe urodzenia.csv", check.names = FALSE)

pozostawione_noworodki <- readxl::read_excel("Dane/Pozostawione noworodki.xlsx")

pozostawione_long <- pozostawione_noworodki %>%
  pivot_longer(cols = starts_with("20"), names_to = "Rok", values_to = "Pozostawione")

urodzenia_long <- zywe_urodzenia %>%
  pivot_longer(cols = starts_with("20"), names_to = "Rok", values_to = "Urodzenia")

merged_data <- pozostawione_long %>% 
  inner_join(urodzenia_long, by = c("Rok", "Województwo")) %>% 
  mutate(wskaznik = Pozostawione / Urodzenia * 1000)

#### wykres slupkowy
wykres1 <- merged_data %>% 
  filter(Województwo == "Polska") %>% 
  ggplot(aes(x = Rok, y = wskaznik)) + 
  geom_col(fill = "#8C2a64") +
  theme_minimal() +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Liczba noworodków pozostawionch w szpitalu na 1000 urodzeń", 
       x = "Rok", y = "Wskaźnik na 1000 urodzeń") +
  theme(
         plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 20)))
ggsave("slupki.png", wykres1, width = 9, height = 5)
wykres1

### mapa
mapa_wojewodztw <- st_read("Dane/wojewodztwa.shp")
map_data <- mapa_wojewodztw %>%
  left_join(pozostawione_long, by = c("JPT_NAZWA_" = "Województwo")) %>% 
  filter(Rok == "2023")

mapa_noworodki <- ggplot(data = map_data) +
  geom_sf(aes(fill = Pozostawione), color = "black") +
  geom_sf_text(aes(label = Pozostawione), color = "black", size = 10) + 
  scale_fill_gradientn(
    colours = c("#ea4f7f", "#884292", "#302174"),
    values = scales::rescale(c(min(map_data$Pozostawione), median(map_data$Pozostawione), max(map_data$Pozostawione))),
  ) +
  theme_minimal() +
  labs(
    title = "Liczba noworodków pozostawionych w szpitalu (2023)",
    fill = "Pozostawione"
  ) +
  labs(fill = "Liczba noworodków") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),
    legend.position = "right",
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  )
mapa_noworodki
ggsave("mapa_noworodki.png", mapa_noworodki, width = 20, height = 10)
