library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

placowki_df <- read_excel("2023_PieczaZastepcza.xlsx", sheet = "TABL.I.2")

liczba_miejsc_Polska <- as.numeric(placowki_df[11, 4])
liczba_wychowankow_Polska <- as.numeric(placowki_df[11, 5])

miejsca_wychowankowie <- placowki_df[12:27, 1:5] %>% select(-c(`...2`, `...3`))
colnames(miejsca_wychowankowie) <- c("Wojewodztwo","Miejsca", "Wychowankowie")
miejsca_wychowankowie$Miejsca <- as.numeric(miejsca_wychowankowie$Miejsca)
miejsca_wychowankowie$Wychowankowie <- as.numeric(miejsca_wychowankowie$Wychowankowie)

miejsca_wychowankowie <- miejsca_wychowankowie %>%
  mutate("Zapelnienie" = round(Wychowankowie / Miejsca * 100, 1), 
         "Przepelniony" = if_else(Zapelnienie > 100, TRUE, FALSE))

#Procent zapełnienia placówek w danym województwie
wojewodztwa <- suppressWarnings(getShape("voiv"))
koordynaty <- coordinates(wojewodztwa)[c(1:2, 4:5, 3, 6:16),]
mapa_PL_przepelnienie <- structure(miejsca_wychowankowie$Przepelniony, names = rownames(voivData))
mapa_zapelnienia <- suppressMessages(suppressWarnings(
  voivPlot(mapa_PL_przepelnienie, col.regions = c("#bde6f6", "#e9535c"), colorkey = FALSE,
           sp.layout = list(list("sp.text", koordynaty, 
                                 txt = percent(miejsca_wychowankowie$Zapelnienie / 100),
                                 cex = 1, col = "black")),
           par.settings = list(axis.line = list(col = "transparent")))))
mapa_zapelnienia

srednie_zapelnienie = round(liczba_wychowankow_Polska / liczba_miejsc_Polska * 100, 1)
dzieci_ponad_miejsca = liczba_wychowankow_Polska - liczba_miejsc_Polska
