library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(readxl)

urodzenia_zywe <- read_excel("urodzenia_zywe.xlsx")
noworodki_pozostawione <- read_excel("noworodki_pozostawione.xlsx")
noworodki_pozostawione <- noworodki_pozostawione %>%
  filter(!row_number() %in% c(18))

noworodki_pozostawione[, -1] <- lapply(noworodki_pozostawione[, -1], as.numeric)
urodzenia_zywe[, -1] <- lapply(urodzenia_zywe[, -1], as.numeric)


urodzenia_zywe <- urodzenia_zywe %>%
  rename(województwo = Województwo)

urodzenia_zywe <- urodzenia_zywe %>%
  filter(!row_number() %in% c(17))

noworodki_pozostawione <- noworodki_pozostawione %>%
  filter(!row_number() %in% c(17))


# Konwersja ramek danych
urodzenia_zywe_long <- urodzenia_zywe %>%
  pivot_longer(cols = -województwo, names_to = "Rok", values_to = "value") %>%
  mutate(Rok = as.integer(Rok))

noworodki_pozostawione_long <- noworodki_pozostawione %>%
  pivot_longer(cols = -województwo, names_to = "Rok", values_to = "value") %>%
  mutate(Rok = as.integer(Rok))




# Obliczenie odsetka
heatmap_dane <- noworodki_pozostawione_long %>%
  inner_join(urodzenia_zywe_long, by = c("województwo", "Rok")) %>%
  mutate(Odsetek = (value.x / value.y) * 100)

# Heatmapa
ggplot(heatmap_dane, aes(x = Rok, y = województwo, fill = Odsetek)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Odsetek (%)", option = "magma") +
  labs(
    title = "Odsetek pozostawionych noworodków w szpitalach (2007-2023)",
    x = "Rok",
    y = "Województwo"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(face = "bold", size = 15),
        plot.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 12),  # Pogrubienie tytułu osi X
        axis.title.y = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size = 15),  # Pogrubienie tytułu legendy
        legend.text = element_text(face = "bold", size = 15))



noworodki_pozostawione_long %>%
  ggplot(aes(x = województwo, y = value, fill = województwo)) +
  geom_boxplot() +
  labs(title = "Rozkład liczby porzuconych noworodków w województwach (2007-2023)",
       x = "Województwo", y = "Liczba porzuconych noworodków") +
  theme(axis.text.x = element_blank()) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),   # Usunięcie etykiet na osi X
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 12),  # Pogrubienie tytułu osi X
        axis.title.y = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size = 15),  # Pogrubienie tytułu legendy
        legend.text = element_text(face = "bold", size = 15)) + # Usunięcie ticków na osi X
  scale_x_discrete(labels = NULL) # Dodatkowe usunięcie etykiet


# Wykres
ggplot(compare_dane, aes(x = województwo, y = Odsetek, fill = as.factor(Rok))) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Porównanie odsetka pozostawionych noworodków: 2007 vs 2023",
    x = "Województwo",
    y = "Odsetek (%)",
    fill = "Rok"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(face = "bold", size = 15),
        plot.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 12),  # Pogrubienie tytułu osi X
        axis.title.y = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size = 15),  # Pogrubienie tytułu legendy
        legend.text = element_text(face = "bold", size = 15))

        