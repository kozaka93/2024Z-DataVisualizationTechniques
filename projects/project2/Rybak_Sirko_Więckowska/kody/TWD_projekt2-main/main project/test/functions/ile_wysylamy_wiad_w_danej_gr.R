library(dplyr)


# Funkcja mówiąca ile wiadomości wysyła osoba podana w argumencie do: 
  # 1/ grupy rodzinnej (lub jakiejs osoby z rodziny)
  # 2/ najlepszego przyjaciela
  # 3/ kolegi ze studiów
  # 4/ kolegi spoza studiów

# Zwraca tabelke w takiej postaci:

#                 grupa  liczba_wiadomosci

# 1              Rodzina              1623
# 2           Przyjaciel              4018
# 3    Kolega ze studiów              1098
# 4 Kolega spoza studiów              1199


liczba_wiadomosci_w_danej_gr <- function(wiadom_rodzina, wiadom_przyj, wiadomosci_kolega_studia, wiadom_kol_poza_stud, osoba){

  filtruj <- function(tabela) {
    
    # if (is.null(tabela) || nrow(tabela) == 0) {
    #   return(0)
    # }
    tabela <- tabela %>%
      filter(who_sent == osoba) 
    return(nrow(tabela)) 
  }
  
  

  liczba_wiadomosci_rodzina <- filtruj(wiadom_rodzina)
  liczba_wiadomosci_przyjaciel <- filtruj(wiadom_przyj)
  liczba_wiadomosci_kolega_studia <- filtruj(wiadomosci_kolega_studia)
  liczba_wiad_kol_poza_stud <- filtruj(wiadom_kol_poza_stud)
  

  wynik <- data.frame(
    grupa = c("Family", "Best Friend", "Friend (from Uni)", "Friend"),
    liczba_wiadomosci = c(
      liczba_wiadomosci_rodzina, 
      liczba_wiadomosci_przyjaciel, 
      liczba_wiadomosci_kolega_studia, 
      liczba_wiad_kol_poza_stud
    )
  )
  
  return(wynik)
}

