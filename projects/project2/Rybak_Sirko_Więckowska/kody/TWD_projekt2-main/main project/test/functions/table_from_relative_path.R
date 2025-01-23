# 
#to bierze nazwe sciezki i zwraca ladna tabelke
#
# jak nie dziala to możliwe ze jest ustawiony zly katalog
# trzeba byc w katalogu projektu getwd -> setwd
#
# to robi tylko z 1 pliku
# mozna poprawic zeby bralo nazwe foldera i stamtad brala wszystkie pliki (raczej trudne)
# mozna zrobic zeby z listy bralo (łatwe raczej)
#
# zwraca kolumny:
# who_sent, date_of_message, content, reaction, link


library(jsonlite)
library(dplyr)
library(stringi)

#ta funkcja zrobi ladna tabelke 

table_from_relative_path <- function(file_path){
  
  data <- fromJSON(file_path)
  
  df <- as.data.frame(data$messages)
  
  #changed column names, if column text appears (only in encrypted data)
  if ("text" %in% names(df)){
    df <- df %>% 
      mutate(
        content = text,
        timestamp_ms = timestamp,
        sender_name = senderName
      ) %>% 
      select(-text) %>% 
      select(-timestamp)
  }
  
  if ("share" %in% names(df)) {
    df <- df %>%
      mutate(
        share_link = share$link,
        share_text = share$text
      ) %>% select(-share)
  } else {
    
    df$share_link <- NA
    df$share_text <- NA
  }
  
  required_columns <- c("sender_name", "timestamp_ms", "content", "share_link", "share_text", "reactions")
  
  missing_columns <- setdiff(required_columns, names(df))
  df[missing_columns] <- NA
  
  df <- df %>% select(all_of(required_columns))
  
  
  df2 <- df %>% 
    select(-c("share_text", "share_link"))
  
  df3 <- as.data.frame(df$share_link)
  colnames(df3) <- "link"
  
  dane <- cbind(df2,df3)
  
  # # Definicja mapy zamiany
  poprawki <- naprawa_znakow <- c(
    "Ä\u0087"  = "ć",
    "Å\u009b"  = "ś",
    "Å\u0082"  = "ł",
    "Å\u0081"  = "Ł",
    "Å¼"      = "ż",
    "Å»"      = "Ż",
    "Å"      = "Ś",
    "Åº"      = "ź",
    "Ã³"      = "ó",
    "Ä\u0085"  = "ą",
    "Ä\u0084"  = "Ą",
    "Ä\u0099"  = "ę",
    "Ä\u0098"  = "Ę",
    "Å\u0084"  = "ń",
    "Å\u0083"  = "Ń"
  )

  # Funkcja do zamiany wielu błędnych znaków
  replace_chars <- function(text, fixes) {
    for (old in names(fixes)) {
      text <- gsub(old, fixes[[old]], text)
    }
    return(text)
  }


  #uses stringi package to replace special characters
  #działa tylko dla plików encrypted. jsony z tych z którymi pracowaliśmy poprzednio mają zepsute kodowanie...
  
  fix_encoding <- function(text) {
    stri_enc_toutf8(text)
  }
  
  dane <- dane %>%
    filter(!is.na(content)) %>%
    mutate(link = gsub(".*https?://(www\\.|open\\.)?([a-z]+)\\..*", "\\2", link)) %>%
    mutate(content = fix_encoding(content)) %>%
    mutate(content = replace_chars(content, poprawki)) %>%
    mutate(sender_name = replace_chars(sender_name, poprawki)) %>%
    mutate(timestamp_ms = as.POSIXct(timestamp_ms / 1000, origin = "1970-01-01", tz = "UTC")) %>%
    rename(who_sent = sender_name, date_of_message = timestamp_ms, reaction = reactions)
  
  return(dane)
}
