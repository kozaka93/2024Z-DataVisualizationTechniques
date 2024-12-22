library(readxl)
library(tidyr)
library(dplyr)

wczytywanie_wiek_i <- function(file_path) {
  # Wczytywanie danych z pliku Excel
  df <- read_excel(file_path, sheet = 1)
  
  # Zmieniamy nazwę kolumn
  colnames(df) <- c("Region", "ogółem", "0", "1—3", "4—6", "7—9", "10—13", "14—17", "18—24")
  
  # Konwertujemy kolumny (oprócz Region) na liczby (numericzne)
  df[,-1] <- df[,-1] %>%
    mutate(across(everything(), as.numeric))  # Konwertujemy wszystkie kolumny na liczby
  
  
  # Łączymy kolumny 7-9 oraz 10-13 w jedną
  df <- df %>%
    mutate(
      `7—13` = `7—9` + `10—13`  # Sumujemy kolumny 7-9 oraz 10-13
    ) %>%
    select(-c("7—9", "10—13"))  # Usuwamy stare kolumny 7-9 oraz 10-13
  
  # Przekształcamy dane do formatu długiego
  data_long <- pivot_longer(df, cols = -Region, names_to = "Wiek", values_to = "WartoscIns")
  
  # Konwertujemy na ramkę danych
  data_long <- as.data.frame(data_long)
  
  return(data_long)
}