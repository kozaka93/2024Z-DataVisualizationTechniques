library(dplyr)


# Funckja dająca informacje o tym, ile ktoś wysyła wiadomości (procentowo) w danej grupie
# Zwraca  udzial procentowy



procentowy_udzial_wiadomosci <- function(tabela, osoba) {
  
  liczba_wszystkich_wiadomosci <- nrow(tabela)
  
  liczba_wiadomosci_osoby <- tabela %>%
    filter(who_sent == osoba) %>%
    nrow()
  
  udzial_procentowy <- (liczba_wiadomosci_osoby / liczba_wszystkich_wiadomosci) * 100

  
  return(udzial_procentowy)
}

