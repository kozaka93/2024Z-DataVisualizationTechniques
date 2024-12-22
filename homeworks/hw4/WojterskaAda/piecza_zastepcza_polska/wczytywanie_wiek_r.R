library(readxl)
library(tidyr)
library(dplyr)

wczytywanie_wiek_r <- function(file_path) {
  # Wczytywanie danych z pliku Excel
  df <- read_excel(file_path, sheet = 2)
  
  # Zmieniamy nazwę kolumn
  colnames(df) <- c("Region", "ogółem", "0", "1—3", "4—6", "7—13", "14—17", "18—24")
  
  # Konwertujemy kolumny (oprócz Region) na liczby (numericzne)
  df[,-1] <- df[,-1] %>%
    mutate(across(everything(), as.numeric))  # Konwertujemy wszystkie kolumny na liczby
  
  # Przekształcamy dane do formatu długiego
  data_long <- pivot_longer(df, cols = -Region, names_to = "Wiek", values_to = "WartoscR")
  
  # Konwertujemy na ramkę danych
  data_long <- as.data.frame(data_long)
  
  return(data_long)
}