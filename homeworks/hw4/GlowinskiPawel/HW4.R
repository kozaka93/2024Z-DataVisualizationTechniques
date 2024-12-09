library(dplyr)
library(tidyr)
library(ggplot2)


urodzenia <- read.csv('urodzenia.csv')
pozostawione <- read.csv("noworodki.csv")


urodzeniaPOL_LODZ <- urodzenia %>% 
  filter(Województwo == "POLSKA" | Województwo == "łódzkie")

colnames(urodzeniaPOL_LODZ) <- c("Województwo","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

urodzeniaPOL_LODZ <- urodzeniaPOL_LODZ %>% 
  pivot_longer(!Województwo,names_to = "Rok",values_to = "Liczba") %>% 
  mutate(Region = case_when(Województwo=="POLSKA"~"Polska",
                            Województwo=="łódzkie"~"woj. Łódzkie"))


pozostawionePOL_LODZ <- pozostawione %>% 
  filter(Noworodki.pozostawione.w.szpitalu.nie.ze.względów.zdrowotnych.w.latach.2007.2023 == "Polska" | Noworodki.pozostawione.w.szpitalu.nie.ze.względów.zdrowotnych.w.latach.2007.2023 == "łódzkie")

colnames(pozostawionePOL_LODZ) <- c("Województwo","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")

pozostawionePOL_LODZ <- pozostawionePOL_LODZ %>%
  mutate(Region = case_when(Województwo=="Polska"~"Polska",
                            Województwo=="łódzkie"~"woj. Łódzkie")) %>% 
  pivot_longer(!Region,names_to = "Rok",values_to = "Liczba")



dane <- urodzeniaPOL_LODZ %>% 
  inner_join(pozostawionePOL_LODZ,by=c("Region","Rok")) %>%
  select(Region,Rok,Liczba.x,Liczba.y) %>% 
  mutate(liczba_poz = as.numeric(Liczba.y),Czynnik = (liczba_poz/Liczba.x)*10000,Rok = as.numeric(Rok))

typeof(as.factor(dane$Rok))


plot <- ggplot(dane,aes(x = as.factor(Rok),y = Czynnik,group = Region ,colour = Region))+
  geom_point(size = 2.5)+
  geom_line(size = 0.8)+
  geom_vline(aes(xintercept = which(levels(as.factor(Rok)) == "2016"),linetype = "Wprowadzenie\n500+"))+
  labs(title = "Liczba pozostawionych noworodków na 10 tys. urodzeń", y = "Liczba pozostawień na 10 tys. urodzeń", x = "Rok")+
  scale_linetype_manual(name = " ",values = c("Wprowadzenie\n500+" = "dashed"))

plot


