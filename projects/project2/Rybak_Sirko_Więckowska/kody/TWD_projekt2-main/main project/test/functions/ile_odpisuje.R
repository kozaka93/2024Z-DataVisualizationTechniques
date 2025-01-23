
library(dplyr)


# Funkcja mówiąca ile średnio minut zajmuje nam odpisanie drugiej osobie
# do średniej nie wliczam wiadomości których odstęp cxasowy wynosi więcej niż godzina 
# funkcja zwraca 1 liczbę (w minutach)



ile_srednio_minut_odpisuje <- function(tabela, osoba){
  
  tabela <- tabela %>% 
    select(who_sent, date_of_message) %>% 
    arrange(date_of_message) %>% 
    mutate(czy_wyslane_przez_nas = (who_sent == osoba))
  
  czasy_odp <- c()
  

  
  for (i in 2:nrow(tabela)) {
    if (tabela$czy_wyslane_przez_nas[i] & !tabela$czy_wyslane_przez_nas[i-1]) {
      roznice_czasow <- as.numeric(difftime(tabela$date_of_message[i], tabela$date_of_message[i - 1], units = "secs"))
      
      if (!is.na(roznice_czasow) && roznice_czasow <= 3600 && roznice_czasow >= 0) {
        czasy_odp <- c(czasy_odp, roznice_czasow)
      }
    }
  }
  
  sredni_czas_odp <- mean(czasy_odp, na.rm = TRUE)/60
  
  wynik <- tibble(
    who_sent = osoba,
    sredni_czas_odp = sredni_czas_odp
  )
  
  return(wynik)
}

