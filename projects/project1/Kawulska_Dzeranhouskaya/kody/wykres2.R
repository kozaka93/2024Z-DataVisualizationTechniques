library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

data_comp1 <- read_tsv("hsw_ph3_06_tabular.tsv", show_col_types = FALSE)

data_comp1 <- data_comp1 %>%
  separate(`freq,deviatn,severity,nace_r2,sex,age,unit,geo\\TIME_PERIOD`, into = c("freq", "deviatn", "severity",  "nace_r2","sex","age","unit", "geoTIME_PERIOD"), sep = ",")

for (rok in 2021) {
  kolumna <- as.character(rok)
  data_comp1[[kolumna]] <- as.numeric(gsub("[^0-9]", "", data_comp1[[kolumna]]))
}

dane_bez_na1 <- data_comp1 %>%
  filter(severity=="FAT") %>% 
  select(nace_r2,deviatn, "2021") 

dane_bez_na1 <- na.omit(dane_bez_na1)

colnames(dane_bez_na1) <- c("sektor","rodzaj_urazu","ilosc")

dane_bez_na1<-dane_bez_na1 %>% 
  filter(rodzaj_urazu!="TOTAL"&sektor!="TOTAL"&sektor!="UNK"&sektor!="A_C-N" &sektor !="T" & sektor!="U") %>% 
  group_by(sektor,rodzaj_urazu) %>% 
  summarise(suma=sum(ilosc))


kategorie <- list(
  "DEV_1" = "Electrical
problems",
  "DEV_2" = "Overflow or
leak",
  "DEV_3" = "Collapse of
material agent",
  "DEV_4" = "Loss of control
of the machine",
  "DEV_5" = "Slipping
",
  "DEV_6" = "Body movement
(without stress)",
  "DEV_7" = "Body movement
(stress)",
  "DEV_8" = "Shock,fright 
or violence",
  "DEV_9" = "Other
deviations")

kategorie2 <- list(
  "A" = "Agriculture&hunting",
  "B" = "Mining",
  "C" = "Manufacturing",
  "D" = "Electricity",
  "E" = "Water supply",
  "F" = "Construction",
  "G" = "Wholesaleand retail trade;",
  "H" = "Transportation",
  "I" = "Accommodation service",
  "J" = "Information",
  "K" = "Financial activity",
  "L" = "Real estate",
  "M" = "Scientific activities",
  "N" = "Administrative, support service",
  "O" = "Public administration",
  "P" = "Education",
  "Q" = "Human health and social work",
  "R" = "Entertainment",
  "S" = "Other service activities"
)

# Funkcja przypisująca kategorię główną na podstawie kodu NACE
przypisz_kategorie <- function(nace_code) {
  # Wyodrębnia pierwszą literę kodu
  litera <- substr(nace_code, 1, 5)
  
  # Przypisuje kategorię na podstawie słownika, jeśli istnieje
  if (litera %in% names(kategorie)) {
    return(kategorie[[litera]])
  } else {
    return(NA)  # Na wypadek, gdyby kod nie istniał w słowniku
  }
}

dane_bez_na1$rodzaj_urazu <- sapply(dane_bez_na1$rodzaj_urazu, przypisz_kategorie)

przypisz_kategorie2 <- function(nace_code) {
  # Wyodrębnia pierwszą literę kodu
  litera <- substr(nace_code, 1, 1)
  
  # Przypisuje kategorię na podstawie słownika, jeśli istnieje
  if (litera %in% names(kategorie2)) {
    return(kategorie2[[litera]])
  } else {
    return(NA)  # Na wypadek, gdyby kod nie istniał w słowniku
  }
}

dane_bez_na1$kategoria_glowna <- sapply(dane_bez_na1$sektor, przypisz_kategorie2)


data <- dane_bez_na1 %>% filter(sektor %in% c("A", "B", "C", "F", "H", "J", "K", "M", "P", "R"))

moja_paleta <- c( "#005F73", "#0A9396","#94D2BD", "#E9D8A6", "#EE9B00",
                 "#CA6702", "#BB3E03", "#AE2012", "#9B2226")

plot <- data %>% ggplot( aes(x = kategoria_glowna, y = suma, fill =rodzaj_urazu)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = moja_paleta) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs( title = "Fatal accidents at Work by Cause, 2021",
       x = "",
       y = "",
       fill = "Cause")  +
  theme_minimal()  +
  theme(
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold", colour = "black"),
    axis.title.x = element_blank(),  
    axis.title.y = element_blank(),  
    axis.ticks = element_blank(),    
    plot.title = element_text(hjust = 0.5, size = 15.8,  face = "bold"),
    legend.position = "right",
    legend.title = element_blank(),  
    legend.text = element_text(size = 14),
    panel.background = element_rect(fill = "transparent", color = NA),  
    plot.background = element_rect(fill = "transparent", color = NA)   
  ) 
plot

#output_path <- file.path("d:/RStudiobaza/pr/mati1", "bolt33_plot.png")

# Zapisanie wykresu z przezroczystym tłem
#ggsave(output_path, plot = plot, bg = "transparent", width = 8, height = 6)





