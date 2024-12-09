library(readxl)
library(dplyr)
library(ggplot2)
#install.packages("mapoland", repos="http://R-Forge.R-project.org")
library(sf)
library(sp)
library(terra)
library(mapoland)

wolontariusze_df <- read_excel("2023_PieczaZastepcza.xlsx", sheet = "TABL.I.4")

liczba_wolontariuszy_Polska <- as.numeric(wolontariusze_df[12, 3])

liczba_wolontariuszy <- wolontariusze_df[13:28, 1:3] %>% select(-`...2`)
colnames(liczba_wolontariuszy) <- c("Wojewodztwo", "Wolontariusze")


placowki_df <- read_excel("2023_PieczaZastepcza.xlsx", sheet = "TABL.I.2")

liczba_placowek_Polska <- as.numeric(placowki_df[11, 3])
liczba_wychowankow_Polska <- as.numeric(placowki_df[11, 5])

liczba_wychowankow <- placowki_df[12:27, 1:5] %>% select(-c(`...2`, `...3`, `...4`))
liczba_placowek <- placowki_df[12:27, 1:5] %>% select(-c(`...2`, `...4`, `...5`))
colnames(liczba_wychowankow) <- c("Wojewodztwo", "Wychowankowie")
colnames(liczba_placowek) <- c("Wojewodztwo", "Placowki")


wolontariusze_na_placowke <- round(liczba_wolontariuszy_Polska / liczba_placowek_Polska, 2)
wychowankowie_dla_wolontariusza <- round(liczba_wychowankow_Polska / liczba_wolontariuszy_Polska, 2)


wolontariusze_placowki_wychowankowie <- liczba_placowek %>% left_join(liczba_wychowankow, by = "Wojewodztwo") %>%
  left_join(liczba_wolontariuszy, by = "Wojewodztwo") %>%
  mutate("Wolontariusze_na_placowke" = round(as.numeric(Wolontariusze) / as.numeric(Placowki), 2),
         "Wychowankowie_dla_wolontariusza" = round(as.numeric(Wychowankowie) / as.numeric(Wolontariusze), 0))

#Mapa placówek instytucjonalnych w województwach
# mapa_PL_placowki <- structure(as.numeric(wolontariusze_placowki_wychowankowie$Placowki), names = rownames(voivData))
# skala_do_mapy <- colorRampPalette(c("#B5E0F3", "#315CA8", "#303174"))
# mapa_placowek <- suppressMessages(suppressWarnings(
#   voivPlot(mapa_PL_placowki, col.regions = skala_do_mapy(200),
#            sp.layout = list(list("sp.text", koordynaty, 
#                                  txt = wolontariusze_placowki_wychowankowie$Placowki,
#                                  cex = 1, col = "white")),
#            par.settings = list(axis.line = list(col = "transparent")),
#            colorkey = list(axis.line = list(col = "black")))))
# mapa_placowek

#Mapa wychowanków w województwach w placowkach instytucjonalnych
# mapa_PL_wychowankowie <- structure(as.numeric(wolontariusze_placowki_wychowankowie$Wychowankowie), names = rownames(voivData))
# mapa_wychowankow <- suppressMessages(suppressWarnings(
#   voivPlot(mapa_PL_wychowankowie, col.regions = skala_do_mapy(200),
#            sp.layout = list(list("sp.text", koordynaty, 
#                                  txt = wolontariusze_placowki_wychowankowie$Wychowankowie,
#                                  cex = 1, col = "white")),
#            par.settings = list(axis.line = list(col = "transparent")),
#            colorkey = list(axis.line = list(col = "black")))))
# mapa_wychowankow

#Mapa wolontariuszy w województwach
wojewodztwa <- suppressWarnings(getShape("voiv"))
koordynaty <- coordinates(wojewodztwa)[c(1:2, 4:5, 3, 6:16),]
mapa_PL_wolontariusze <- structure(as.numeric(wolontariusze_placowki_wychowankowie$Wolontariusze), names = rownames(voivData))
skala_do_mapy_2 <- colorRampPalette(c("#f7bfd7", "#f398c0", "#ef72ab", "#ec4e9c"))
mapa_wolontariuszy <- suppressMessages(suppressWarnings(
  voivPlot(mapa_PL_wolontariusze, col.regions = skala_do_mapy_2(200), 
           sp.layout = list(list("sp.text", koordynaty, 
                                 txt = wolontariusze_placowki_wychowankowie$Wolontariusze,
                                 cex = 1, col = "black")),
           par.settings = list(axis.line = list(col = "transparent")),
           colorkey = list(axis.line = list(col = "black")))))
mapa_wolontariuszy

#Mapa wychowanków na jednego wolontariusza w województwach
mapa_PL_wolontariusze2 <- structure(as.numeric(wolontariusze_placowki_wychowankowie$Wychowankowie_dla_wolontariusza), names = rownames(voivData))
mapa_wolontariuszy2 <- suppressMessages(suppressWarnings(
  voivPlot(mapa_PL_wolontariusze2, col.regions = skala_do_mapy_2(200), 
           sp.layout = list(list("sp.text", koordynaty, 
                                 txt = wolontariusze_placowki_wychowankowie$Wychowankowie_dla_wolontariusza,
                                 cex = 1, col = "black")),
           par.settings = list(axis.line = list(col = "transparent")),
           colorkey = list(axis.line = list(col = "black")))))
mapa_wolontariuszy2
