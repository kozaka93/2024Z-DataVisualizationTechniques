library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(mapdata)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(geodata)
library(stringr)
library(showtext)

showtext_auto()
font_add("Raleway_Italic", "./fonts/Raleway-Italic-VariableFont_wght.ttf")
font_add("Raleway_Variable", "./fonts/Raleway-VariableFont_wght.ttf")
font_add("TenorSans", "./fonts/TenorSans-Regular.ttf")


PieczaZastepcza_df <- read_excel("./data/2022_PieczaZastepcza.xlsx")
#View(PieczaZastepcza_df)
Dzieci018_df <- read_excel("./data/Liczba dzieci 0-18 lat w Polsce 2014-2023.xlsx")
#View(Dzieci018_df)
NoworodkiPozostawione_df <- read_excel("./data/Noworodki pozostawione w szpitalu 2007-2023.xlsx")
#View(NoworodkiPozostawione_df)
Noworodki_df <- read_excel("./data/Urodzenia żywe w Polsce 2007-2023.xlsx")
#View(Noworodki_df)
Osoby24_df <- read_excel("./data/Liczba osób w wieku 0-24 lata w Polsce, 2014-2023.xlsx")
#View(Osoby24_df)
Wychowankowie24_df <- read_excel("./data/Wychowankowie (0-24 lata) w pieczy zastępczej 2014-2023.xlsx")
#View(Wychowankowie24_df)

#------------------wykresy noworodkow------------------
colnames(NoworodkiPozostawione_df)[1] <- "wojewodztwa"
colnames(NoworodkiPozostawione_df)[-1] <- 2007:2023

#View(df)
df <- NoworodkiPozostawione_df[24, ] %>% 
  pivot_longer(cols = !wojewodztwa, names_to = "year", values_to = "amount") %>% 
  arrange(year)

#dla zapoznania sie z danymi
ggplot(data = df, aes(x = year, y = amount)) + 
  geom_line(aes(group = wojewodztwa), color = "navy") +
  geom_point(size = 3, color = "blue")

df2 <- Noworodki_df[17, ] %>% pivot_longer(cols = !Województwo, 
                                           names_to = "year", 
                                           values_to = "amount") %>% 
  arrange(year)
#View(df2)

#dla zapoznania sie z danymi
ggplot(data = df2, aes(x = year, y = amount)) + 
  geom_line(aes(group = Województwo), color = "navy") +
  geom_point(size = 3, color = "red")

df["all_amount"] <- df2$amount
df["normalized_amount"] <- as.numeric(df$amount)/(as.numeric(df$all_amount)/10000)
df$year <- as.numeric(df$year)

#dla spójności danych
df <- df %>% filter(year >= 2014)

#ZNORMALIZOWANY
nw <- ggplot(data = df, aes(x = year, y = normalized_amount)) + 
  geom_line(aes(group = wojewodztwa), color = "black") +
  geom_point(size = 3, color = "#315ca8") +
  labs(title = "Liczba noworodków w Polsce pozostawinoych w spitalu",
       subtitle = "(na 10tys. urodzonych dzieci)") +
  xlab(label = "rok") + ylab(label = "liczba noworodków") +
  theme(title = element_text(family = "TenorSans", size = 55),
        axis.text = element_text(size = 55), 
        axis.title.x = element_text(size = 65),
        axis.title.y = element_text(size = 65))
ggsave("./plots/nw.png", plot = nw, width = 9, height = 6, units = "in", dpi = 300)


#------------------wykresy osób 0-24------------------
df3 <- Osoby24_df[17, ] %>% pivot_longer(cols = !Województwo,
                                         names_to = "year",
                                         values_to = "amount") %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)
#View(df3)

#dla zapoznania sie z danymi
ggplot(data = df3, aes(x = year, y = amount/1000000)) + 
  geom_line(aes(group = Województwo), color = "black") + 
  geom_point(color = "red", size = 3)

colnames(Wychowankowie24_df)[1] <- "wz"

df4 <- Wychowankowie24_df[17, ] %>% pivot_longer(cols = !wz,
                                                 names_to = "year",
                                                 values_to = "amount") %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)
#View(df4)

#dla zapoznania sie z danymi
ggplot(data = df4, aes(x = year, y = amount)) + 
  geom_line(aes(group = wz), color = "black") + 
  geom_point(size = 3, color = "orange")

df4["all_amount"] <- df3$amount
df4 <- df4 %>% mutate(normalized_amount = amount/(all_amount/100000))

#ZNORMALIZOWANY
wch <- ggplot(data = df4, aes(x = year, y = normalized_amount)) + 
  geom_line(aes(group = wz), color = "black") + 
  geom_point(size = 3, color = "#e4007e") + 
  labs(title = "Liczba wychowanków w Polsce",
       subtitle = "(na 100tys. osób wieku 0-24)") + 
  xlab(label = "rok") + ylab(label = "liczba wychowanków") +
  theme(title = element_text(family = "TenorSans", size = 55),
        axis.text = element_text(size = 55), 
        axis.title.x = element_text(size = 65),
        axis.title.y = element_text(size = 65))

ggsave("./plots/wch.png", plot = wch, width = 9, height = 6, units = "in", dpi = 300)


#------------------mapy------------------
map <- geodata::gadm("Poland", level = 1, path = tempdir())
map_df <- as.data.frame(map)
#class(map)
map1 <- st_as_sf(map)
#class(map1)
#View(map1)


#View(NoworodkiPozostawione_df)
mapdf1 <- NoworodkiPozostawione_df %>% select(wojewodztwa, as.character(2023)) %>% 
  filter(!is.na(wojewodztwa))
mapdf1 <- mapdf1[c(-1, -2), ]
mapdf1$wojewodztwa <- str_to_title(mapdf1$wojewodztwa)
mapdf1 <- mapdf1[-17, ]
colnames(mapdf1)[2] <- "count"
#View(mapdf1)
populacja <- Osoby24_df %>% select(Województwo, "2023.0")
populacja <- populacja[-17, ]
colnames(populacja)[2] <- "num"
populacja$Województwo <- str_to_title(populacja$Województwo)

#View(mapdf1)

ndf <- Noworodki_df %>% select(Województwo, "2023.0") %>% 
  mutate(Województwo = str_to_title(Województwo))
ndf <- ndf[-17, ]
colnames(ndf)[2] = "total_count"

mapdf1 <- mapdf1 %>% inner_join(ndf, by = c("wojewodztwa" = "Województwo"))
mapdf1$count <- as.numeric(mapdf1$count)
mapdf1 <- mapdf1 %>% mutate(normalized_count_nw = count/(total_count/10000))

#dla raportu
mapdf1 %>% filter((normalized_count_nw == max(normalized_count_nw)) | (normalized_count_nw == min(normalized_count_nw)))

map1 <- map1 %>% left_join(mapdf1, by = c("NAME_1" = "wojewodztwa"))

map_nw <- ggplot(data = map1) +
  geom_sf(aes(fill = normalized_count_nw), color = "black") +
  scale_fill_gradient(low = "#b5e0f3", high = "#303174", name = "") +
  labs(title = "Liczba pozostawionych w szpitalu noworodków",
       subtitle = "(na 10tys. dzieci urodzonych w 2023 roku)") +
  theme_void() +
  theme(title = element_text(family = "TenorSans", size = 40),
        legend.text = element_text(size = 45))

ggsave("./plots/map_nw.png", plot = map_nw, width = 6, height = 6, units = "in", dpi = 300)

#View(Wychowankowie24_df)
mapdf2 <- Wychowankowie24_df %>% select(wz, "2023.0")
mapdf2 <- mapdf2[-17, ]
mapdf2$wz <- str_to_title(mapdf2$wz)
colnames(mapdf2)[2] <- "count2"

mapdf2 <- mapdf2 %>% inner_join(populacja, c("wz" = "Województwo"))
mapdf2 <- mapdf2 %>% mutate(normalized_count = count2/(num/100000))

#dla raportu
mapdf2 %>% filter((normalized_count == max(normalized_count)) | (normalized_count == min(normalized_count)))

map1 <- map1 %>% left_join(mapdf2, by = c("NAME_1" = "wz"))

map_wch <- ggplot(data = map1) + 
  geom_sf(aes(fill = normalized_count), 
          color = "black") +
  scale_fill_gradient(low = "#f7b9cc", high = "#e62248", 
                      name = "") +
  labs(title = "Liczba wychowanków w pieczy zastępczej",
       subtitle = "(na 100tys. osób w wieku 0-24 w 2023 roku)") + 
  theme_void() +
  theme(title = element_text(family = "TenorSans", size = 40),
        legend.text = element_text(size = 45))

ggsave("./plots/map_wch.png", plot = map_wch, width = 6, height = 6, units = "in", dpi = 300)








