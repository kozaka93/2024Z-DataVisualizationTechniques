library(readxl)
library(maps)
library(ggplot2)
library(tidyr)
library(patchwork)
library(sf)
library(RColorBrewer)



urdz_zywe <- read_excel("/Users/daria1942/Downloads/Urodzenia\ żywe\ w\ Polsce\ 2007-2023.xlsx")
poz_nowo <- read_excel("/Users/daria1942/Downloads/Noworodki\ pozostawione\ w\ szpitalu\ 2007-2023.xlsx")

urdz_zywe_long <- urdz_zywe %>%
  pivot_longer(
    cols = starts_with("20"), # Wybierz kolumny z latami
    names_to = "Rok",         # Nazwa kolumny dla lat
    values_to = "Liczba"      # Nazwa kolumny dla wartości
  )
urdz_polska <- urdz_zywe_long[-c(1:272), ]


plot1 <- ggplot(urdz_polska, aes(x = as.numeric(Rok), y = Liczba)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "navy", size = 2) +
  labs(
    title = "Liczba urodzeń w Polsce na przestrzeni lat",
    x = "Rok",
    y = "Liczba urodzeń"
  ) +
  scale_x_continuous(
    breaks = seq(min(as.numeric(urdz_zywe_long$Rok)), max(as.numeric(urdz_zywe_long$Rok)), by = 1)
  ) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Wyśrodkowanie tytułu
  )
  
poz_nowo_long <- poz_nowo %>%
  pivot_longer(
    cols = starts_with("20"), # Wybierz kolumny z latami
    names_to = "Rok",         # Nazwa kolumny dla lat
    values_to = "Liczba"      # Nazwa kolumny dla wartości
  )

poz_polska <- poz_nowo_long[-c(1:272), ]

plot2 <- ggplot(poz_polska, aes(x = as.numeric(Rok), y = Liczba)) +
  geom_line(color = "aquamarine2", size = 1) +
  geom_point(color = "aquamarine4", size = 2) +
  labs(
    title = "Noworodki pozostawione w szpitalu nie ze względów zdrowotnych na przestrzeni lat",
    x = "Rok",
    y = "Liczba urodzeń"
  ) +
  scale_x_continuous(
    breaks = seq(min(as.numeric(urdz_zywe_long$Rok)), max(as.numeric(urdz_zywe_long$Rok)), by = 1)
  ) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Wyśrodkowanie tytułu
  ) +
  geom_vline(xintercept = 2016.5, color = "cyan", size = 1) +
  
  annotate("text", x = 2017, y = max(poz_polska$Liczba, na.rm = TRUE) * 1, 
           label = "Start 500+", color = "cyan", angle = 0, hjust = 1.5, size = 4)


combined_plot <- plot1/plot2
combined_plot

urdz_23 <- urdz_zywe_long[urdz_zywe_long$Rok == 2023, ]


granice_woj <- st_read("/Users/daria1942/Downloads/00_jednostki_administracyjne/A01_Granice_wojewodztw.shp")
granice_woj1 <- merge(granice_woj, urdz_23, by.x = "JPT_NAZWA_", by.y = "Wojewodztwo")

plot3 <- ggplot(data = granice_woj1) + 
  geom_sf(aes(fill = Liczba), color = "black", size = 0.2) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Blues"), name = "Liczba urodzeń") +  
  labs(
    title = "Liczba urodzeń", 
    subtitle = "w 2023 roku"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

poz_23 <- poz_nowo_long[poz_nowo_long$Rok == 2023, ]

granice_woj2 <- merge(granice_woj, poz_23, by.x = "JPT_NAZWA_", by.y = "Wojewodztwo")
plot4 <- ggplot(data = granice_woj2) + 
  geom_sf(aes(fill = Liczba), color = "black", size = 0.2) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Blues"), name = "Ilość porzuconych noworodków w Polsce") +  
  labs(
    title = "Ilość porzuconych noworodków", 
    subtitle = "w 2023 roku"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

combined_plot2 <- plot3 / plot4
combined_plot2

