library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(fmsb)

df1 <- read.csv("dane1.csv")
df2 <- read.csv("dane2.csv")
df3 <- read_excel("dane3.xlsx", sheet = 1)

colnames(df2)
dim(df2)

#POZIOM PRZYGNEBIENIA OD CZASU I PLCI
wykres_1 <- df2 %>% 
  filter(X2..Gender %in% c("Female", "Male")) %>% 
  group_by(X8..What.is.the.average.time.you.spend.on.social.media.every.day., X2..Gender) %>% 
  summarise(sredni_poziom_przygnebienia = mean(X18..How.often.do.you.feel.depressed.or.down.)) %>% 
  ggplot(aes(x = factor(X8..What.is.the.average.time.you.spend.on.social.media.every.day., levels = c("Less than an Hour", "Between 1 and 2 hours", "Between 2 and 3 hours", "Between 3 and 4 hours", "Between 4 and 5 hours", "More than 5 hours")), y = sredni_poziom_przygnebienia,, fill = X2..Gender)) + geom_bar(stat = "identity", position = "dodge")
#POiOM SNU OD CZASU I PLCI
wykres_2 <- df2 %>% 
  filter(X2..Gender %in% c("Female", "Male")) %>% 
  group_by(X8..What.is.the.average.time.you.spend.on.social.media.every.day., X2..Gender) %>% 
  summarise(sredni_poziom_snu = mean(X20..On.a.scale.of.1.to.5..how.often.do.you.face.issues.regarding.sleep.)) %>% 
  ggplot(aes(x = factor(X8..What.is.the.average.time.you.spend.on.social.media.every.day., levels = c("Less than an Hour", "Between 1 and 2 hours", "Between 2 and 3 hours", "Between 3 and 4 hours", "Between 4 and 5 hours", "More than 5 hours")), y = sredni_poziom_snu, fill = X2..Gender)) + geom_bar(stat = "identity", position = "dodge")
#POZIOM POROWNYWANIA SIE OD CZASU I PLCI
wykres_3 <- df2 %>% 
  filter(X2..Gender %in% c("Female", "Male")) %>% 
  group_by(X8..What.is.the.average.time.you.spend.on.social.media.every.day., X2..Gender) %>% 
  summarise(sredni_poziom_porownywania_sie = mean(X15..On.a.scale.of.1.5..how.often.do.you.compare.yourself.to.other.successful.people.through.the.use.of.social.media.)) %>% 
  ggplot(aes(x = factor(X8..What.is.the.average.time.you.spend.on.social.media.every.day., levels = c("Less than an Hour", "Between 1 and 2 hours", "Between 2 and 3 hours", "Between 3 and 4 hours", "Between 4 and 5 hours", "More than 5 hours")), y = sredni_poziom_porownywania_sie, fill = X2..Gender)) + geom_bar(stat = "identity", position = "dodge")
#POZIOM ROZPROSZENIA OD CZASU I PLCI
wykres_4 <- df2 %>% 
  filter(X2..Gender %in% c("Female", "Male")) %>% 
  group_by(X8..What.is.the.average.time.you.spend.on.social.media.every.day., X2..Gender) %>% 
  summarise(sredni_poziom_rozproszenia = mean(X12..On.a.scale.of.1.to.5..how.easily.distracted.are.you.)) %>% 
  ggplot(aes(x = factor(X8..What.is.the.average.time.you.spend.on.social.media.every.day., levels = c("Less than an Hour", "Between 1 and 2 hours", "Between 2 and 3 hours", "Between 3 and 4 hours", "Between 4 and 5 hours", "More than 5 hours")), y = sredni_poziom_rozproszenia, fill = X2..Gender)) + geom_bar(stat = "identity", position = "dodge")
#POZIOM NIEPEWNOSCI OD CZASU I PLCI
wykres_5 <- df2 %>% 
  filter(X2..Gender %in% c("Female", "Male")) %>% 
  group_by(X8..What.is.the.average.time.you.spend.on.social.media.every.day., X2..Gender) %>% 
  summarise(sredni_poziom_niepewnosci = mean(X13..On.a.scale.of.1.to.5..how.much.are.you.bothered.by.worries.)) %>% 
  ggplot(aes(x = factor(X8..What.is.the.average.time.you.spend.on.social.media.every.day., levels = c("Less than an Hour", "Between 1 and 2 hours", "Between 2 and 3 hours", "Between 3 and 4 hours", "Between 4 and 5 hours", "More than 5 hours")), y = sredni_poziom_niepewnosci, fill = X2..Gender)) + geom_bar(stat = "identity", position = "dodge")
#POZIOM PROBLEMOW Z KONCENTRACJA OD CZASU I PLCI
wykres_6 <- df2 %>% 
  filter(X2..Gender %in% c("Female", "Male")) %>% 
  group_by(X8..What.is.the.average.time.you.spend.on.social.media.every.day., X2..Gender) %>% 
  summarise(sredni_poziom_problemow_z_koncentracja = mean(X14..Do.you.find.it.difficult.to.concentrate.on.things.)) %>% 
  ggplot(aes(x = factor(X8..What.is.the.average.time.you.spend.on.social.media.every.day., levels = c("Less than an Hour", "Between 1 and 2 hours", "Between 2 and 3 hours", "Between 3 and 4 hours", "Between 4 and 5 hours", "More than 5 hours")), y = sredni_poziom_problemow_z_koncentracja, fill = X2..Gender)) + geom_bar(stat = "identity", position = "dodge")
#POZIOM DOWARTOSCIOWYWANIA SIE OD RELACJI
wykres_7 <- df2 %>% 
 group_by(X3..Relationship.Status) %>% 
  summarise(sredni_poziom_szukania_dowartosciowania_sie = mean(X17..How.often.do.you.look.to.seek.validation.from.features.of.social.media.)) %>% 
  ggplot(aes(x = factor(X3..Relationship.Status), y = sredni_poziom_szukania_dowartosciowania_sie)) + geom_bar(stat = "identity", position = "dodge")
#JAK CZESTO SIEGAMY BEZ POWODU
wykres_8 <- df2 %>% 
  ggplot(aes(x = X9..How.often.do.you.find.yourself.using.Social.media.without.a.specific.purpose.)) + geom_bar()


dane_srednia_aplikacje <- df2 %>% 
  mutate(ile_czasu = case_when(
    X8..What.is.the.average.time.you.spend.on.social.media.every.day. == "Less than an Hour" ~ 0.5,
    X8..What.is.the.average.time.you.spend.on.social.media.every.day. == "Between 1 and 2 hours" ~ 1,
    X8..What.is.the.average.time.you.spend.on.social.media.every.day. == "Between 2 and 3 hours" ~ 2,
    X8..What.is.the.average.time.you.spend.on.social.media.every.day. == "Between 3 and 4 hours" ~ 3,
    X8..What.is.the.average.time.you.spend.on.social.media.every.day. == "Between 4 and 5 hours" ~ 4,
    X8..What.is.the.average.time.you.spend.on.social.media.every.day. == "More than 5 hours" ~ 5
  )) %>% 
  separate_rows(X7..What.social.media.platforms.do.you.commonly.use., sep = ', ') %>% 
  group_by(X7..What.social.media.platforms.do.you.commonly.use.) %>% 
  summarise(czas = mean(ile_czasu),bez_powodu = mean(X9..How.often.do.you.find.yourself.using.Social.media.without.a.specific.purpose.), rozproszenie = mean(X10..How.often.do.you.get.distracted.by.Social.media.when.you.are.busy.doing.something.), niespokojnosc = mean(X11..Do.you.feel.restless.if.you.haven.t.used.Social.media.in.a.while.), rozproszenie = mean(X12..On.a.scale.of.1.to.5..how.easily.distracted.are.you.), przejmowanie_sie = mean(X13..On.a.scale.of.1.to.5..how.much.are.you.bothered.by.worries.), koncentracja = mean(X14..Do.you.find.it.difficult.to.concentrate.on.things.), porownowanie_sie = mean(X15..On.a.scale.of.1.5..how.often.do.you.compare.yourself.to.other.successful.people.through.the.use.of.social.media.), dowartosciowanie_sie = mean(X17..How.often.do.you.look.to.seek.validation.from.features.of.social.media.), depresja = mean(X18..How.often.do.you.feel.depressed.or.down.), zmiennosc = mean(X19..On.a.scale.of.1.to.5..how.frequently.does.your.interest.in.daily.activities.fluctuate.), sen = mean(X20..On.a.scale.of.1.to.5..how.often.do.you.face.issues.regarding.sleep.))

dane_srednia_aplikacje <- dane_srednia_aplikacje %>% 
  mutate(across(c(czas, bez_powodu, rozproszenie, niespokojnosc, przejmowanie_sie, koncentracja, porownowanie_sie,dowartosciowanie_sie,depresja,zmiennosc,sen), as.numeric)) %>% 
  mutate(czas = czas^7/2, bez_powodu = bez_powodu^6, rozproszenie = rozproszenie^6, niespokojnosc = niespokojnosc^6, przejmowanie_sie = przejmowanie_sie^6, koncentracja = koncentracja^6,porownowanie_sie = porownowanie_sie^20/1500000,dowartosciowanie_sie = dowartosciowanie_sie^18/9400, depresja = 1.5*depresja^6, zmiennosc = zmiennosc^6, sen = sen^6)

data_for_radar <- rbind(c("Max",(rep(max(as.matrix(dane_srednia_aplikacje[c(2,6,9),-1])), ncol(dane_srednia_aplikacje) - 1))), c("Min",rep(1, ncol(dane_srednia_aplikacje) - 1)), dane_srednia_aplikacje)
data_for_radar <- data_for_radar %>% 
  mutate(across(c(czas, bez_powodu, rozproszenie, niespokojnosc, przejmowanie_sie, koncentracja, porownowanie_sie,dowartosciowanie_sie,depresja,zmiennosc,sen), as.numeric))

Discord <- data_for_radar[1:3,-1]
Facebook <- data_for_radar[c(1:2,4),-1]
Instagram <- data_for_radar[c(1:2,5),-1]
Pinterest <- data_for_radar[c(1:2,6),-1]
Reddit <- data_for_radar[c(1:2,7),-1]
Snapchat <- data_for_radar[c(1:2,8),-1]
TikTok <- data_for_radar[c(1:2,9),-1]
Twitter <- data_for_radar[c(1:2,10),-1]
Youtube <- data_for_radar[c(1:2,11),-1]

Discord <- data_for_radar[1:3,c(-1,-5,-12)]
Instagram <- data_for_radar[c(1:2,5),c(-1,-5,-12)]
Pinterest <- data_for_radar[c(1:2,6),c(-1,-5,-12)]
Reddit <- data_for_radar[c(1:2,7),c(-1,-5,-12)]
TikTok <- data_for_radar[c(1:2,9),c(-1,-5,-12)]
Twitter <- data_for_radar[c(1:2,10),c(-1,-5,-12)]


Facebook <- data_for_radar[c(1:2,4),c(-1,-3,-5,-6,-11,-12)]
Youtube <- data_for_radar[c(1:2,11),c(-1,-3,-5,-6,-11,-12)]
Snapchat <- data_for_radar[c(1:2,8),c(-1,-3,-5,-6,-11,-12)]
#colnames(Facebook) <- c(" ", " ", " "," " ," ", " ")
#colnames(Youtube) <- c(" ", " ", " "," " ," ", " ")
#colnames(Snapchat) <- c(" ", " ", " "," " ," ", " ")
colnames(Facebook) <- c("Screen-time","Distraction", "Focus Issues", "Social Comparison", "Self-Doubt", "Depression")
colnames(Youtube) <- c("Screen-time","Distraction", "Focus Issues", "Social Comparison", "Self-Doubt", "Depression")
colnames(Snapchat) <- c("Screen-time","Distraction", "Focus Issues", "Social Comparison", "Self-Doubt", "Depression")


#par(mfrow = c(2, 4), mar = c(1, 1, 2, 1))
#radarchart(Discord,
#           cglty = 1, cglcol = "gray",
#           pcol = 4, plwd = 2,
#           pfcol = rgb(0, 0.4, 1, 0.25),
#           title = "Discord")

#radarchart(Instagram,
#           cglty = 1, cglcol = "gray",
#           pcol = 4, plwd = 2,
#           pfcol = rgb(0, 0.4, 1, 0.25),
#           title = "Instagram")
#radarchart(Pinterest,
#           cglty = 1, cglcol = "gray",
#           pcol = 4, plwd = 2,
#           pfcol = rgb(0, 0.4, 1, 0.25),
#           title = "Pinterest")

#radarchart(TikTok,
#           cglty = 1, cglcol = "gray",
#           pcol = 4, plwd = 2,
#           pfcol = rgb(0, 0.4, 1, 0.25),
#           title = "TikTok")
#radarchart(Twitter,
#           cglty = 1, cglcol = "gray",
#           pcol = 4, plwd = 2,
#           pfcol = rgb(0, 0.4, 1, 0.25),
#           title = "Twitter")

# Zapisz wykres Facebooka
png("facebook.png", width = 1600, height = 1600, bg = "transparent")  # Tworzenie pliku PNG z przezroczystym tłem
radarchart(Facebook,
           cglty = 1, cglcol = "white",  # Kolor siatki
           pcol = "#1877F2",  # Kolor linii wykresu (Facebook - niebieski)
           plwd = 2,  # Grubość linii wykresu
           pfcol = rgb(0.094, 0.510, 0.949, 0.8),  # Bardziej nasycony niebieski
           vlcex = 1)  # Zmniejszenie czcionki etykiet osi (np. parametrów)
dev.off()  # Zakończenie zapisu wykresu do pliku

# Zapisz wykres Snapchata
png("snapchat.png", width = 1600, height = 1600, bg = "transparent")  # Tworzenie pliku PNG z przezroczystym tłem
radarchart(Snapchat,
           cglty = 1, cglcol = "white",  # Kolor siatki
           pcol = "#FFFC00",  # Kolor linii wykresu (Snapchat - żółty)
           plwd = 2,  # Grubość linii wykresu
           pfcol = rgb(1, 1, 0, 0.8),  # Bardziej nasycony żółty
           vlcex = 1)  # Zmniejszenie czcionki etykiet osi (np. parametrów)
dev.off()  # Zakończenie zapisu wykresu do pliku

# Zapisz wykres YouTube
png("youtube.png", width = 1600, height = 1600, bg = "transparent")  # Tworzenie pliku PNG z przezroczystym tłem
radarchart(Youtube,
           cglty = 1, cglcol = "white",  # Kolor siatki
           pcol = "#FF0000",  # Kolor linii wykresu (YouTube - czerwony)
           plwd = 2,  # Grubość linii wykresu
           pfcol = rgb(1, 0, 0, 0.8),  # Bardziej nasycony czerwony
           vlcex = 1)  # Zmniejszenie czcionki etykiet osi (np. parametrów)
dev.off()  # Zakończenie zapisu wykresu do pliku

# Zapisz wykres Facebooka
png("facebook2.png", width = 800, height = 800)  # Tworzenie pliku PNG z przezroczystym tłem
radarchart(Facebook,
           cglty = 1, cglcol = "gray",  # Kolor siatki
           pcol = "#1877F2",  # Kolor linii wykresu (Facebook - niebieski)
           plwd = 2,  # Grubość linii wykresu
           pfcol = rgb(0.094, 0.510, 0.949, 0.8),  # Bardziej nasycony niebieski
           vlcex = 1)  # Zmniejszenie czcionki etykiet osi (np. parametrów)
title("Facebook", col.main = "#1877F2", cex.main = 2)  # Tytuł w kolorze Facebooka
dev.off()  # Zakończenie zapisu wykresu do pliku

# Zapisz wykres Snapchata
png("snapchat2.png", width = 800, height = 800)  # Tworzenie pliku PNG z przezroczystym tłem
radarchart(Snapchat,
           cglty = 1, cglcol = "gray",  # Kolor siatki
           pcol = "#FFFC00",  # Kolor linii wykresu (Snapchat - żółty)
           plwd = 2,  # Grubość linii wykresu
           pfcol = rgb(1, 1, 0, 0.8),  # Bardziej nasycony żółty
           vlcex = 1)  # Zmniejszenie czcionki etykiet osi (np. parametrów)
title("Snapchat", col.main = "#FFFC00", cex.main = 2)  # Tytuł w kolorze Snapchata
dev.off()  # Zakończenie zapisu wykresu do pliku

# Zapisz wykres YouTube
png("youtube2.png", width = 800, height = 800)  # Tworzenie pliku PNG z przezroczystym tłem
radarchart(Youtube,
           cglty = 1, cglcol = "gray",  # Kolor siatki
           pcol = "#FF0000",  # Kolor linii wykresu (YouTube - czerwony)
           plwd = 2,  # Grubość linii wykresu
           pfcol = rgb(1, 0, 0, 0.8),  # Bardziej nasycony czerwony
           vlcex = 1)  # Zmniejszenie czcionki etykiet osi (np. parametrów)
title("YouTube", col.main = "#FF0000", cex.main = 2)  # Tytuł w kolorze YouTube
dev.off()  # Zakończenie zapisu wykresu do pliku

