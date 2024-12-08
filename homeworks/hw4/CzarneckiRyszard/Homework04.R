library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(sf)
library(patchwork)
library(RColorBrewer)
library(tidyr)
Żywe_Urodzenia <- read.csv("Urodzenia żywe w Polsce 2007-2023.csv")
Pozostawienia <- read.csv("Noworodki pozostawione w szpitalu 2007-2023.csv")
Pozostawienia_2013 <- Pozostawienia %>% 
  select(województwo, X2013)
Pozostawienia_2019 <- Pozostawienia %>% 
  select(województwo, X2019)
województwa <- st_read("wojewodztwa.shp")
województwa_2013_pozost <- merge(województwa, Pozostawienia_2013, by.x = "JPT_NAZWA_", by.y = "województwo")
województwa_2019_pozost <- merge(województwa, Pozostawienia_2019, by.x = "JPT_NAZWA_", by.y = "województwo")

Wykres_Pozostawień_2013 <- ggplot(data = województwa_2013_pozost) + 
  geom_sf(aes(fill = X2013), color = "black") + 
  scale_fill_gradientn(colors = c("#b5e0f3", "#303174", "#884292"), name = "Liczba zostawionych dzieci") +
  labs(title = "Liczba pozostawionych dzieci w 2013 roku") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20)) +
  guides(
    fill = guide_colorbar(
      barwidth = 2,
      barheight = 15,
      title.position = "top",
      title.hjust = 0.5,
      label = TRUE,
      frame.colour = "black"
    )
  )
Wykres_Pozostawień_2013
Wykres_Pozostawień_2019 <- ggplot(data = województwa_2019_pozost) + 
  geom_sf(aes(fill = X2019), color = "black") + 
  scale_fill_gradientn(colors = c("#b5e0f3", "#303174", "#884292"), name = "Liczba zostawionych dzieci", limits = c(0, 150)) +
  labs(title = "Liczba pozostawionych dzieci w 2019 roku") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20)) +
  guides(
    fill = guide_colorbar(
      barwidth = 2,
      barheight = 15,
      title.position = "top",
      title.hjust = 0.5,
      label = TRUE,
      frame.colour = "black"
    )
  )
Wykres_Pozostawień_2019
Wykres_Pozostawień <- (Wykres_Pozostawień_2013 + theme(plot.title = element_text(size = 12))) + 
  (Wykres_Pozostawień_2019 + theme(plot.title = element_text(size = 12), legend.position = "none")) + 
  plot_layout(guides = "collect") + 
  plot_annotation(title = "Porównanie liczby pozostawionych dzieci 3 lata przed 500+ i 3 lata po 500+", 
                  theme = theme(plot.title = element_text(size = 16, hjust = 0.5)))
Wykres_Pozostawień
df_pozostawienia_long <- Pozostawienia %>%
  gather(key = "rok", value = "porzucone", -województwo)
df_urodzenia_long <- Żywe_Urodzenia %>%
  gather(key = "rok", value = "urodzenia", -Województwo)
df <- merge(df_pozostawienia_long, df_urodzenia_long, by.x = c("województwo", "rok"), by.y = c("Województwo", "rok"))
df$rok <- as.character(as.integer(gsub("X", "", df$rok)))
final_df <- df %>%
  group_by(rok) %>%
  summarise(suma_porzuconych = sum(porzucone),
            suma_urodzonych = sum(urodzenia)) %>% 
  mutate(procent = suma_porzuconych/ suma_urodzonych * 100)
Wykres <- ggplot(final_df, aes(x = rok, y = procent)) +
  geom_bar(stat = "identity", fill = "#303174") +
  labs(title = "Procent porzuconych dzieci w każdym roku",
       x = "Rok",
       y = "Procent porzuconych dzieci") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )
Wykres

ggsave("Wykres_Pozostawień.png", plot = Wykres_Pozostawień, width = 10, height = 6, dpi = 300)
ggsave("Wykres.png", plot = Wykres, width = 10, height = 6, dpi = 300)



