# load_data.R

library(readxl)
library(tidyr)

wczytywanie_pieczazastepcza <- function(file_path) {

  df <- read_excel(file_path)
  
  colnames(df) <- c("Region", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023")
  
  # Filtracja danych dla POLSKA
  #polska_data <- df[df$Region == "POLSKA", ]
  
  data_long <- pivot_longer(df, cols = -Region, names_to = "Rok", values_to = "Wartosc")
  
  data_long <- as.data.frame(data_long)
  
  return(data_long)
}