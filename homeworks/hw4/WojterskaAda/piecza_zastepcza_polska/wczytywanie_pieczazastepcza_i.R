# load_data.R

library(readxl)
library(tidyr)

wczytywanie_pieczazastepcza_i <- function(file_path) {
  
  df <- read_excel(file_path, sheet = 2)
  
  colnames(df) <- c("Region", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023")
  
  data_long <- pivot_longer(df, cols = -Region, names_to = "Rok", values_to = "WartoscI")
  
  
  data_long <- as.data.frame(data_long)
  
  return(data_long)
}