library(dplyr)
library(readr)
library(lubridate)
library(calendR)
library(gridExtra)
library(plotly)
library(rlang)
library(htmltools)
# data preparation
plik <- "C:/Users/Admin/Downloads/sleepdata.xls.csv"
sleep <- read_csv2(plik)
sleep <- sleep %>% select(!c(`Alertness score`,`Alertness reaction time (seconds)`,`Alertness accuracy`))
sleep$`Went to bed` <- ymd_hms(sleep$`Went to bed`)
sleep$date <- as.Date(sleep$`Went to bed`)
sleep$bed <- format(sleep$`Went to bed`, "%H:%M:%S")
sleep$`Woke up` <- ymd_hms(sleep$`Woke up`)
sleep <- sleep %>% select(!c(`Wake up window stop`,`Wake up window start`,`Went to bed`,`Woke up`))
sleep$day <- day(sleep$date)
sleep$month <- month(sleep$date)
sleep$date <- NULL
sleep$generalday <- ifelse(hour(hms(sleep$bed)) < 8, sleep$day-1,sleep$day)
vector <- as.numeric(c(rep(0,11), gsub("%", "", sleep$`Sleep Quality`), rep(0,14)))
# chart
color_palette <- colorRampPalette(c("#fc8d59", "#ffffbf", "#91cf60"))(100)

december <- calendR(year = 2024, month = 12,
                    start = "M", title = "Jakość snu w grudniu",
                    weeknames = c("Pn", "Wt", "Śr", "Czw", "Pt", "Sb", "Nd"),
                    special.days = vector, gradient = TRUE,
                    special.col = color_palette,
                    low.col = "white")

hover_info <- sapply(1:31, function(day) {
  if(day %in% sleep$generalda) {
    sleep_info <- sleep[sleep$generalday == day, ]
    tmp <- paste( "<br>Czas snu: ",
          round(as.numeric(sleep_info$`Time asleep (seconds)`)/3600, 2), " godz.",
          "<br>Chrapanie: ", ifelse(sleep_info$`Did snore`, "Tak", "Nie"),
          "<br>Położenie się do łóżka: ", sleep_info$bed,
          "<br>Czas do zaśnięcia: ",
          round(as.numeric(sleep_info$`Asleep after (seconds)`)/60,2), " min") 
    tmp
  } else {
    "Brak danych"
  }
})


january <- calendR(year = 2025, month = 1,
                   start = "M", title = "Jakość snu w styczniu",
                   weeknames = c("Pn", "Wt", "Śr", "Czw", "Pt", "Sb", "Nd"))