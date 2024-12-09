library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

rodzinna_piecza_df <- read_excel("Wychowankowie (0-24 lata) w pieczy zastępczej 2014-2023.xlsx", 
                              sheet = "Wychowankowie rodzinnej pieczy ")
colnames(rodzinna_piecza_df) <- c("Wojewodztwo", 2014:2023)

instytucjonalna_piecza_df <- read_excel("Wychowankowie (0-24 lata) w pieczy zastępczej 2014-2023.xlsx", 
                                        sheet = "Wychowankowie instytucjonalnej ")
colnames(instytucjonalna_piecza_df) <- c("Wojewodztwo", 2014:2023)

rodzinna_PL <- rodzinna_piecza_df %>% filter(Wojewodztwo == "POLSKA") %>%
  pivot_longer(!Wojewodztwo, names_to = "Rok", values_to = "Piecza_rodzinna") %>%
  select(-Wojewodztwo)
instytucjonalna_PL <- instytucjonalna_piecza_df %>% filter(Wojewodztwo == "POLSKA") %>%
  pivot_longer(!Wojewodztwo, names_to = "Rok", values_to = "Piecza_instytucjonalna") %>%
  select(-Wojewodztwo)

wychowankowie_df <- rodzinna_PL %>%
  left_join(instytucjonalna_PL, by = "Rok") %>%
  mutate("Wychowankowie" = Piecza_rodzinna + Piecza_instytucjonalna)

wychowankowie_long <- wychowankowie_df %>% select(-Wychowankowie) %>%
  pivot_longer(-Rok, names_to = "Piecza", values_to = "Wychowankowie")


#Liczba wychowanków na przestrzeni lat z podziałem na pieczę rodzinną i instytucjonalną
# wykres_wychowankow <- wychowankowie_long %>% ggplot(aes(x = Rok, y = Wychowankowie, fill = Piecza)) + 
#   geom_bar(stat = "identity") +
#   scale_fill_manual(values = c("Piecza_rodzinna" = "#955b9f", "Piecza_instytucjonalna" = "#ed6d8f")) +
#   labs(fill = "") + scale_y_continuous(expand = c(0, 0))
# wykres_wychowankow


#Liczba osób w wieku 0-24 na przestrzeni lat
# populacja <- read_excel("Liczba osób w wieku 0-24 lata w Polsce, 2014-2023.xlsx", 
#                         sheet = "Liczba osób 0-24 lata w Polsce")
# colnames(populacja) <- c("Wojewodztwo", 2014:2023)
# populacja <- populacja %>% filter(Wojewodztwo == "RAZEM") %>%
#   pivot_longer(!Wojewodztwo, names_to = "Rok", values_to = "Populacja") %>%
#   select(-Wojewodztwo)
# wykres_populacji <- populacja %>% 
#   ggplot(aes(x = Rok, y = Populacja, group = 1)) + geom_line() + geom_point()
# wykres_populacji

#Liczba wychowanków na 1000 osób w wieku 0-24
#na przestrzeni lat z podziałem na pieczę rodzinną i instytucjonalną
wychowankowie_populacja <- wychowankowie_df %>%
  left_join(populacja , by = "Rok") %>%
  mutate(Wychowankowie = Wychowankowie / Populacja * 1000, 
         Piecza_rodzinna = Piecza_rodzinna / Populacja * 1000,
         Piecza_instytucjonalna = Piecza_instytucjonalna / Populacja * 1000)
wykres_porownanie <- wychowankowie_populacja %>%
  select(-Populacja, -Wychowankowie) %>%
  pivot_longer(-Rok, names_to = "Piecza", values_to = "Wychowankowie") %>%
  ggplot(aes(x = Rok, y = Wychowankowie, fill = Piecza)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Piecza_rodzinna" = "#955b9f", "Piecza_instytucjonalna" = "#ed6d8f")) +
  labs(fill = "", x = "", y = "") + scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() + theme(legend.position = "none")
wykres_porownanie

wzrost <- as.numeric((wychowankowie_populacja[10, 4] - wychowankowie_populacja[9, 4]) / wychowankowie_populacja[9, 4]) * 100
wzrost <- round(wzrost, 2)

#Wychowankowie w placowkach

placowki_df <- read_excel("2023_PieczaZastepcza.xlsx", sheet = "TABL.I.2")

liczba_wychowankow <- placowki_df[12:27, 1:5] %>% select(-c(`...2`, `...3`, `...4`))
colnames(liczba_wychowankow) <- c("Wojewodztwo", "Wychowankowie")

rodzinne_df <- read_excel("2023_PieczaZastepcza.xlsx", sheet = "TABL.I.11")
wychowankowie_rodzinnych <- rodzinne_df[10:25, 1:3] %>% select(-`...2`)
colnames(wychowankowie_rodzinnych) <- c("Wojewodztwo", "Placowki_rodzinne")

wszyscy_wychowankowie <- wychowankowie_rodzinnych %>%
  left_join(liczba_wychowankow, by = "Wojewodztwo") %>%
  mutate(Suma = as.numeric(Wychowankowie) + as.numeric(Placowki_rodzinne))

#Mapa wszystkich wychowankow w wojewodztwach
skala_do_mapy <- colorRampPalette(c("#B5E0F3", "#315CA8", "#303174"))
mapa_PL_wszyscy_wychowankowie <- structure(as.numeric(wszyscy_wychowankowie$Suma), names = rownames(voivData))
mapa_wszystkich_wychowankow <- suppressMessages(suppressWarnings(
  voivPlot(mapa_PL_wszyscy_wychowankowie, col.regions = skala_do_mapy(200),
           sp.layout = list(list("sp.text", koordynaty, 
                                 txt = wszyscy_wychowankowie$Suma,
                                 cex = 1, col = "white")),
           par.settings = list(axis.line = list(col = "transparent")),
           colorkey = list(axis.line = list(col = "black")))))
mapa_wszystkich_wychowankow

liczba_wszystkich_wychowankow <- sum(wszyscy_wychowankowie$Suma)