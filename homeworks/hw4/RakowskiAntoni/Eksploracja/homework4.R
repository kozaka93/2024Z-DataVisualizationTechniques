# set to directory containing urodzenia-zywe.csv
# and noworodki-pozostawione-w-szpitalu.csv
# setwd("path to directory")

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

births <- read.csv("urodzenia-zywe.csv")
abandonment <- read.csv("noworodki-pozostawione-w-szpitalu.csv")

names(births)[-1] <- 2007:2023
names(births)[1] <- "voivodeship"
names(abandonment)[-1] <- 2007:2023
names(abandonment)[1] <- "voivodeship"
abandonment <- abandonment[c(-1:-7, -25), ]
rownames(abandonment) <- NULL

abandonment <- abandonment %>%
  pivot_longer(cols = -voivodeship, names_to = "year", values_to = "abandonned")

births <- births %>%
  pivot_longer(cols = -voivodeship, names_to = "year", values_to = "births")

abandonment <- abandonment %>%
  mutate(year = as.numeric(year), voivodeship = str_to_title(voivodeship),
         abandonned = as.numeric(abandonned))

births <- births %>%
  mutate(year = as.numeric(year), voivodeship = str_to_title(voivodeship))

data <- full_join(abandonment, births, by = c("voivodeship", "year")) %>%
  arrange(voivodeship, year) %>%
  mutate(abandonned_per_100k = round(abandonned * 100000 / births))

write.csv(data, "data.csv", row.names = FALSE)