# changing names of countries
# Czechia, United Kingdom, Lithuania, Luxembourg, Netherlands
library(readr)

library(dplyr)
Table_2<- read_csv("CORRECTED_Table 2 Country-level descriptive statistics. - Arkusz1.csv")
colnames(Table_2)<-c('num','Country', 'Count','Divorce_rate','Care_supply')

Table_2[6,1]<-'Czechia'
Table_2[29,1]<-'United Kingdom'
Table_2[17,1]<-'Lithuania'
Table_2[18,1]<-'Luxembourg'
Table_2[27,1]<-'Netherlands'

#dodac kolumne Custom_factor

Table_2<-Table_2 %>% 
  mutate(Custom_factor = Care_supply/Divorce_rate) %>% 
  select(-num)

#dodac wiersze z panstwami:islandia(), norwegia(), bialorus(),ukraina(),mołdawia(),szwajcaria(),
#bosnia i hercegowina(), serbia(),kosowo(),czarnogóra(),albania(),macedonia polnocna(). 
#i wartosciami w nowej kolumnie

countries <- c("Iceland", "Norway", "Belarus", "Ukraine", "Moldova", 
               "Switzerland", "Bosnia and Herzegovina", "Serbia", 
               "Kosovo", "Montenegro", "Albania", "North Macedonia")

# Tworzenie data frame z trzema kolumnami NA i kolumną z losowymi wartościami
set.seed(123)  # Ustawienie ziarna losowości dla powtarzalności wyników
df <- data.frame(
  Country = countries,                 # Pierwsza kolumna z nazwami państw
  Kolumna1 = NA,                       # Druga kolumna - pusta
  Kolumna2 = NA,                       # Trzecia kolumna - pusta
  Kolumna3 = NA,                       # Czwarta kolumna - pusta
  Value = runif(length(countries), 0, 100)  # Piąta kolumna z losowymi wartościami od 0 do 100
)

df$Value<-c(570,220,120,130,175,450,330,200,200,170,350,400)
colnames(df)<-c('Country', 'Count','Divorce_rate','Care_supply','Custom_factor')

merged<-rbind(Table_2, df)


write.csv(merged, "CORRECTED_Table 2 Country-level descriptive statistics. - Arkusz1.csv")
 


