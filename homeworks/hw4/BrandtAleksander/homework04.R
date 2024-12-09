library(dplyr)
library(ggplot2)
library(readxl)
library(geodata)
library(tidyr)
library(sf)
library(stringr)
library(patchwork)
library(broom)
library(viridis)

options(scipen = 999)


uZ <- read_excel("UrodzeniaZ.xlsx")
nP <- read_excel("NoworodkiP.xlsx")


# poprawianie ramek danych
nP <- nP %>% 
  rename_with(~c("voivodeship", seq(2007, 2023, 1))) %>% 
  mutate(voivodeship = tolower(voivodeship))

nP <- nP[seq(8, 24, 1),]
nP <- nP %>% 
  mutate(across(-voivodeship, as.numeric))

uZ <- uZ %>% 
  rename_with(~c("voivodeship", seq(2007, 2023, 1))) %>% 
  mutate(voivodeship = tolower(voivodeship)) %>% 
  mutate(across(-voivodeship, as.numeric))

nPlong <- nP %>%
  pivot_longer(cols = -voivodeship, names_to = "year", values_to = "babies_left") %>%
  mutate(year = as.numeric(year))

uZlong <- uZ %>%
  pivot_longer(cols = -voivodeship, names_to = "year", values_to = "births") %>%
  mutate(year = as.numeric(year))

data <- uZlong %>%
  inner_join(nPlong, by = c("voivodeship", "year"))%>%
  mutate(ratio=babies_left/births)



# Wykres o skali logarytmicznej przedstawiajacy ilosc narodzin oraz ilosc dzieci pozostawionych po narodzinach w szpitalach,
#wykres dodatkowo okraszony jest w procentowa wartosc przedstawiajaca jaka czesc wsysztkich narodzonych dzieci zostala pozostawiona w szpitalach
#wszystko z podzialem na rok
ggplot(data, aes(x = year)) +
  geom_bar(aes(y = births, fill = "Urodzenia żywe"), stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_bar(aes(y = babies_left, fill = "Noworodki pozostawione"), stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(
      x = year,
      y = babies_left,  
      label = paste0(sprintf("%.4f", ratio), "%")
    ),
    vjust = -0.2,          # Pozycja tekstu w pionie
    angle = 90,            # Obrót tekstu o 90 stopni
    size = 3,              # Rozmiar tekstu
    color = "black"        # Kolor tekstu
  ) +
  scale_fill_manual(values = c("Urodzenia żywe" = "blue", "Noworodki pozostawione" = "red")) +
  scale_y_log10(
    breaks = c(500, 1000, scales::log_breaks(base = 10)(range(data$births))),
    labels = scales::comma
  ) +
  labs(
    title = "Liczba urodzeń i pozostawionych dzieci w szpitalach (Polska)",
    x = "Rok",
    y = "Liczba (skala logarytmiczna)",
    fill = "Kategoria"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(500, NA))



# Mapa Polski dla lat 2007–2023 przedstawiajaca wpolczynnik dzieci pozostawionych po urodzinach a wszystkimi urodzonymi dziecmi w podziale na wojewodztwa
ggplot(data, aes(x = year)) +
  geom_bar(aes(y = births, fill = "Urodzenia żywe"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = babies_left, fill = "Noworodki pozostawione"), stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Urodzenia żywe" = "blue", "Noworodki pozostawione" = "red")) +
  scale_y_log10(
    breaks = c(500, 1000, scales::log_breaks(base = 10)(range(data$births))),
    labels = scales::comma  
  ) +
  labs(
    title = "Liczba urodzeń i pozostawionych dzieci w szpitalach (Polska)",
    x = "Rok",
    y = "Liczba (skala logarytmiczna)",
    fill = "Kategoria"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(500, NA))


#tworzenie porzadanej przeze mnie mapy
poland_map <- geodata::gadm("Poland", level = 1, path = tempdir())
poland_map_sf <- st_as_sf(poland_map)
poland_map_sf <- poland_map_sf %>%
  mutate(NAME_1 = tolower(NAME_1))

# Filtracja i dalsza obrobka danych dla lat 2007–2010
data_avg_2007_2010 <- data %>%
  filter(year >= 2007 & year <= 2010) %>% 
  group_by(voivodeship) %>%  
  summarize(
    mean_births = mean(births, na.rm = TRUE),
    mean_babies_left = mean(babies_left, na.rm = TRUE),
    mean_ratio = mean(ratio, na.rm = TRUE)
  )


# Połączenie danych mapowych z wartościami współczynnika średniego ratio
poland_map_data_avg <- poland_map_sf %>%
  left_join(data_avg_2007_2010, by = c("NAME_1" = "voivodeship"))

# Mapa Polski dla lat 2007–2010 przedstawiajaca wpolczynnik dzieci pozostawionych po urodzinach a wszystkimi urodzonymi dziecmi w podziale na wojewodztwa
plot_2007_2010<-ggplot(poland_map_data_avg) +
  geom_sf(aes(fill = mean_ratio), color = "black", size = 0.2) +
  scale_fill_gradient(
    low = "lightyellow",   
    high = "darkred",      
    name = "Współczynnik",
    na.value = "grey80",  
    labels = scales::percent
  ) +
  labs(
    title = "(2007–2010)",  
        
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5), 
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9), 
    legend.position = "right"  
  ) +
  coord_sf(expand = FALSE)  



#Filtracja i dalsza obrobka danych dla lat 2020–2023

data_avg_2020_2023 <- data %>%
  filter(year >= 2020 & year <= 2023) %>%
  group_by(voivodeship) %>%
  summarize(
    mean_births = mean(births, na.rm = TRUE),
    mean_babies_left = mean(babies_left, na.rm = TRUE),
    mean_ratio = mean(ratio, na.rm = TRUE)
  )

# Połączenie danych mapowych z wartościami współczynnika średniego ratio
poland_map_data_avg_2020_2023 <- poland_map_sf %>%
  left_join(data_avg_2020_2023, by = c("NAME_1" = "voivodeship"))

# Mapa Polski dla lat 2020–2023 przedstawiajaca wpolczynnik dzieci pozostawionych po urodzinach a wszystkimi urodzonymi dziecmi w podziale na wojewodztwa
plot_2020_2023 <- ggplot(poland_map_data_avg_2020_2023) +
  geom_sf(aes(fill = mean_ratio), color = "black", size = 0.2) +
  scale_fill_gradient(
    low = "lightyellow",
    high = "darkred",
    name = "Współczynnik",
    na.value = "grey80",
    labels = scales::percent
  ) +
  labs(
    title = "(2020–2023)",

  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    legend.position = "right"
  ) +
  coord_sf(expand = FALSE)


# Filtracja i dalsza obrobka danych dla lat 2007–2023
data_avg_2007_2023 <- data %>%
  filter(year >= 2007 & year <= 2023) %>%
  group_by(voivodeship) %>%
  summarize(
    mean_births = mean(births, na.rm = TRUE),
    mean_babies_left = mean(babies_left, na.rm = TRUE),
    mean_ratio = mean(ratio, na.rm = TRUE)
  )

# Połączenie danych mapowych z wartościami współczynnika średniego ratio
poland_map_data_avg_2007_2023 <- poland_map_sf %>%
  left_join(data_avg_2007_2023, by = c("NAME_1" = "voivodeship"))

# Mapa Polski dla lat 2007–2023 przedstawiajaca wpolczynnik dzieci pozostawionych po urodzinach a wszystkimi urodzonymi dziecmi w podziale na wojewodztwa
plot_2007_2023 <- ggplot(poland_map_data_avg_2007_2023) +
  geom_sf(aes(fill = mean_ratio), color = "black", size = 0.2) +
  scale_fill_gradient(
    low = "lightyellow",
    high = "darkred",
    name = "Współczynnik",
    na.value = "grey80",
    labels = scales::percent
  ) +
  labs(
    title = "(2007–2023)",
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    legend.position = "right"
  ) +
  coord_sf(expand = FALSE)


combined_plot_vertical <- (plot_2007_2010 | plot_2020_2023 | plot_2007_2023) +
  plot_annotation(
    title = "Porównanie współczynnika pozostawionych noworodków w różnych okresach",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    )
  ) &
  theme(
    legend.title = element_text(size = 9),  
    legend.text = element_text(size = 8),   
    plot.margin = margin(10, 10, 10, 10)    
  )


combined_plot_vertical

