library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(showtext)
library(stringr)

font_add_google("Tenor sans", "tenor")
showtext_auto()

c("#303174", "#315ca8", "#b5e0f3", "#884292", "#8c2a64", "#e62248")


urodzenia_zywe_df <- read_excel("Urodzenia żywe w Polsce 2007-2023.xlsx", sheet = 1)
noworodki_pozostawione_df <- read_excel("Noworodki pozostawione w szpitalu 2007-2023.xlsx", sheet = 1)

noworodki_pozostawione_df <- noworodki_pozostawione_df %>% 
  slice(-c(1:7,25))

colnames(noworodki_pozostawione_df) <- c("wojewodztwo", 2007:2023)
colnames(urodzenia_zywe_df) <- c("wojewodztwo", 2007:2023)

# Do 1 i 2 grafiki
noworodki_pozostawione_df <- noworodki_pozostawione_df %>%
  pivot_longer(cols = -wojewodztwo, names_to = "rok", values_to = "liczba_noworodki") %>% 
  mutate(rok = as.numeric(rok),
         liczba_noworodki = as.numeric(liczba_noworodki))


#Do 3 grafiki
urodzenia_zywe_df <- urodzenia_zywe_df %>%
  mutate(wojewodztwo = str_replace(wojewodztwo, "POLSKA", "Polska")) %>% 
  mutate(across(-wojewodztwo, as.numeric)) %>%
  pivot_longer(cols = -wojewodztwo, names_to = "rok", values_to = "liczba_urodzenia")

urodzenia_df <- urodzenia_zywe_df %>%
  mutate(rok = as.numeric(rok),
         liczba_urodzenia = as.numeric(liczba_urodzenia))

noworodki_df <- noworodki_pozostawione_df %>%
  mutate(rok = as.numeric(rok),
         liczba = as.numeric(liczba_noworodki)) %>% 
  left_join(urodzenia_df, by = c("wojewodztwo", "rok"))%>%
  mutate(liczba = (liczba_noworodki / liczba_urodzenia) * 10000) %>% 
  select(wojewodztwo,rok,liczba)

# Grafiki

mapa_polski <- st_read("wojewodztwa.shp")

#Mapa noworodków pozostawoinych na 10000 urodzonych dzieci
mapa_polski_1 <- mapa_polski %>% 
  left_join(noworodki_df %>% filter(rok == 2023), by = c("JPT_NAZWA_" = "wojewodztwo"))

ggplot(mapa_polski_1) +
  geom_sf(aes(fill = liczba), color = "black", size = 0.2) + 
  geom_sf_text(aes(label = round(liczba, 1)), size = 30, family = "tenor") + 
  scale_fill_steps(
    low = "#315ca8",
    high = "#ea4f7f",
    n.breaks = 5
  ) +
  labs(
    title = "Liczba pozostawionych noworodków w szpitalach",
    subtitle = "(na 10 tys. urodzeń w 2023r.)"
  ) +
  theme_minimal(base_family = "tenor") +
  theme(
    panel.grid = element_blank(), 
    plot.title = element_text(size = 100, face = "bold", hjust = 0), 
    plot.subtitle = element_text(size = 90, hjust = 0),
    legend.position = "left",
    axis.title = element_blank(), 
    axis.text = element_blank(),
    panel.background = element_blank(),  
    plot.background = element_blank(),
    legend.text = element_text(size = 60), 
    legend.title = element_blank()
   
  )

ggsave("mapa_noworodki_2023.png", width = 8, height = 6, dpi = 600)

#Mapa noworodków pozostawoinych w 2023 roku.
mapa_polski_2 <- mapa_polski %>% 
  left_join(noworodki_pozostawione_df %>% filter(rok == 2023), by = c("JPT_NAZWA_" = "wojewodztwo"))

ggplot(mapa_polski_2) +
  geom_sf(aes(fill = liczba_noworodki), color = "black", size = 0.2) + 
  geom_sf_text(aes(label = round(liczba_noworodki, 1)), size = 30, family = "tenor") + 
  scale_fill_steps(
    low = "#315ca8",
    high = "#ea4f7f",
    n.breaks = 5
  ) +
  labs(
    title = "Liczba pozostawionych noworodków w szpitalach w 2023r.",
  ) +
  theme_minimal(base_family = "tenor") +
  theme(
    panel.grid = element_blank(), 
    plot.title = element_text(size = 100, face = "bold", hjust = 0), 
    plot.subtitle = element_text(size = 90, hjust = 0),
    legend.position = "left",
    axis.title = element_blank(), 
    axis.text = element_blank(),
    panel.background = element_blank(),  
    plot.background = element_blank(),
    legend.text = element_text(size = 60), 
    legend.title = element_blank()
  )

ggsave("mapa_noworodki_2_2023.png", width = 8, height = 6, dpi = 600)

# Wykres ile dzieci pozostawiono w danym roku w Polsce z 500+

ggplot(noworodki_pozostawione_df %>% 
         filter(wojewodztwo == "Polska"), aes(x = rok, y = liczba_noworodki, group = 1)) +
  geom_line(color = "#315ca8", size = 1) +
  geom_point(color = "#884292", size = 2) +
  scale_x_continuous(breaks = seq(min(noworodki_pozostawione_df$rok), max(noworodki_pozostawione_df$rok), by = 1)) +   
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +  
  labs(
    title = "Liczba pozostawionych noworodków w szpitalach",
    subtitle = "dane dla całej Polski",
    x = "",
    y = "Liczba noworodków"
  ) +
  annotate(
    "segment", 
    x = 2015.5, xend = 2015.5, 
    y = 450, yend = 680,  
    arrow = arrow(length = unit(0.2, "cm")), 
    color = "#e62248", size = 1
  ) +
  annotate(
    "text", 
    x = 2015.5, y = 360,  
    label = " 1.04.2016r.", 
    color = "#e62248", size = 30, family = "tenor", hjust = 0.5
  ) +
  annotate(
    "text", 
    x = 2015.5, y = 320,  
    label = "Wprowadzenie 500+", 
    color = "#e62248", size = 30, family = "tenor", hjust = 0.5
  ) +
  theme_minimal(base_family = "tenor") +
  theme(
    plot.title = element_text(size = 100, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 90, hjust = 0),
    axis.title = element_text(size = 80),
    axis.text = element_text(size = 60),
    panel.grid.minor = element_blank()
  )

ggsave("wykres_noworodki_500.png", width = 8, height = 6, dpi = 600)

# Wykres ile pozostawione dzieci w szpitalach
ggplot(noworodki_pozostawione_df %>% 
         filter(wojewodztwo == "Polska"), aes(x = rok, y = liczba_noworodki, group = 1)) +
  geom_line(color = "#315ca8", size = 1) +
  geom_point(color = "#884292", size = 2) +
  scale_x_continuous(breaks = seq(min(noworodki_pozostawione_df$rok), max(noworodki_pozostawione_df$rok), by = 1)) +   
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +  
  labs(
    title = "Liczba pozostawionych noworodków szpitalach",
    subtitle = "dane dla całej Polski",
    x = "",
    y = "Liczba noworodków"
  ) +
  theme_minimal(base_family = "tenor") +
  theme(
    plot.title = element_text(size = 100, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 90, hjust = 0),
    axis.title = element_text(size = 80),
    axis.text = element_text(size = 60),
    panel.grid.minor = element_blank()
  )

ggsave("wykres_noworodki.png", width = 8, height = 6, dpi = 600)

# Wykres ile dzieci pozostawiono w danym roku w Polsce z 500+, na 10 tys.
ggplot(noworodki_df %>% 
         filter(wojewodztwo == "Polska"), aes(x = rok, y = liczba, group = 1)) +
  geom_line(color = "#315ca8", size = 1) +
  geom_point(color = "#884292", size = 2) +
  scale_x_continuous(breaks = seq(min(noworodki_df$rok), max(noworodki_df$rok), by = 1)) +   
    
  labs(
    title = "Liczba pozostawionych noworodków w szpitalach",
    subtitle = "dane dla całej Polski na 10 tys. urodzeń",
    x = "",
    y = "Liczba noworodków na 10 tys. urodzeń"
  ) +
  annotate(
    "segment", 
    x = 2015.5, xend = 2015.5, 
    y = 15.5, yend = 19.25,  
    arrow = arrow(length = unit(0.2, "cm")), 
    color = "#e62248", size = 1
  ) +
  annotate(
    "text", 
    x = 2015, y = 15,  
    label = " 1.04.2016r.", 
    color = "#e62248", size = 30, family = "tenor", hjust = 0.5
  ) +
  annotate(
    "text", 
    x = 2015, y = 14.5,  
    label = " wprowadzenie 500+", 
    color = "#e62248", size = 30, family = "tenor", hjust = 0.5
  ) +
  theme_minimal(base_family = "tenor") +
  theme(
    plot.title = element_text(size = 100, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 90, hjust = 0),
    axis.title = element_text(size = 80),
    axis.text = element_text(size = 60),
    panel.grid.minor = element_blank()
  )

ggsave("wykres_noworodki_2_500.png", width = 8, height = 6, dpi = 600)


# Wykres ile dzieci pozostawiono w danym roku w Polsce na 10 tys.
ggplot(noworodki_df %>% 
         filter(wojewodztwo == "Polska"), aes(x = rok, y = liczba, group = 1)) +
  geom_line(color = "#315ca8", size = 1) +
  geom_point(color = "#884292", size = 2) +
  scale_x_continuous(breaks = seq(min(noworodki_df$rok), max(noworodki_df$rok), by = 1)) +   
  labs(
    title = "Liczba pozostawionych noworodków w szpitalach",
    subtitle = "dane dla całej Polski na 10 tys. urodzeń",
    x = "",
    y = "Liczba noworodków na 10 tys. urodzeń"
  ) +
  theme_minimal(base_family = "tenor") +
  theme(
    plot.title = element_text(size = 100, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 90, hjust = 0),
    axis.title = element_text(size = 80),
    axis.text = element_text(size = 60),
    panel.grid.minor = element_blank()
  )

ggsave("wykres_noworodki_2.png", width = 8, height = 6, dpi = 600)
