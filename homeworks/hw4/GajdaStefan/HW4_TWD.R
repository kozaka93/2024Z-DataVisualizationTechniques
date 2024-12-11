library(dplyr)
library(ggplot2)
library(tidyr)

noworodki<-read.csv("NoworodkiPozostawioneWSzpitalu.csv")
urodzenia<-read.csv("UrodzeniaZyweWPolsce.csv")
#Zmieniam nazwy kolumn
names(urodzenia)<-c("Województwa",2007:2023)
names(noworodki)<-c("Województwa",2007:2023)

#Zmieniam format df na long
urodzenia_long <- urodzenia %>%
  pivot_longer(
    cols = -Województwa,
    names_to = "rok",  
    values_to = "liczba_urodzen" 
  )


noworodki_long <- noworodki %>%
  pivot_longer(
    cols = -Województwa, 
    names_to = "rok", 
    values_to = "liczba_opuszczonych" 
  )
#Łączę oba dfy
dane <- urodzenia_long %>%
  left_join(noworodki_long, by = c("Województwa", "rok"))


write.csv(dane, "dane.csv", row.names = FALSE)
