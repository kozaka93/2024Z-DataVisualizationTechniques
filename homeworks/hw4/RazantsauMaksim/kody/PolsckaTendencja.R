# Jak zmienia sie problem (pozostawienia noworodkow w Szitalu) w 
# Polsce na przestrzeni lat 

# Opis wykorzystanego aspektu danych

# Wykorzystano dane o urodzeniach żywych w Polsce na przestrzeni lat oraz dane
# o noworodkach pozostawionych w szpitalu nie z powodów zdrowotnych
# (dane z podzialem na lata), 
# Uwzględnieniono aspekt zbierania
# danych na przestrzeni lat, co umożliwiło pokazanie rozwoju problemu oraz jego 
# tendencji z latami. Rowniez uwzgledniona dodatkowa informacja
# o wprowadzeniu programu 500+ 1 kwietnia 2016

## praca z danymi

setwd("/Users/admin/TWD")
# install.packages("ggplot2")
# install.packages("sf")           
# install.packages("rnaturalearthdata")
library(tools)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(shiny)
library(tidyr)

df_pozostawione <- read.csv("noworodkipozostawione.csv")
df_zywe<- read.csv("urodzeniazywe.csv")

df_pozostawione$Region <- toTitleCase(tolower(df_pozostawione$Region))
df_zywe$Województwo <- toTitleCase(tolower(df_zywe$Województwo))

long_df1 <- df_zywe %>%
  pivot_longer(
    cols = !Województwo,         
    names_to = "year",          
    values_to = "amount"         
  )
long_df2 <- df_pozostawione %>%
  pivot_longer(
    cols = !Region,         
    names_to = "year",          
    values_to = "amount"         
  )


long_df1$year <- substring(long_df1$year, 2)
long_df2$year <- substring(long_df2$year, 2)

df_merged <- long_df1 |>
  inner_join(long_df2, by = c("Województwo" = "Region", "year")) |>
  arrange(year) |>
  mutate(leftBy1K = (amount.y / amount.x) * 1000)



View(df_merged)

pozostawione_Polska <- df_merged %>%
  filter(Województwo == "Polska")
View(pozostawione_Polska)


# wykres 
# w Polsce na przestrzeni lat 

ggplot(pozostawione_Polska, aes(x = as.numeric(year), y = amount.y)) +
  geom_line(color = "#b5e0f3", size = 1) +
  geom_point(color = "#303174", size = 2) +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "#315ca8", size = 0.8) +
  annotate(
    "text",
    x = 2016.5, y = max(pozostawione_Polska$amount.y, na.rm = TRUE) * 0.9,
    label = "Wprowadzenie 500+",
    color = "#315ca8",
    size = 3,
    hjust = 0
  ) +
  labs(
    title = "Liczba pozostawionych noworodków w Polsce na przestrzeni lat",
    x = "Rok",
    y = "Liczba pozostawionych noworodków"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 6)
  )

# Wizualizacja przedstawia skale oraz ogolny rozwoj problemu w Polsce
# Wyrozniony zostal miesiac wprowadzenia "500+", widac ze program
# przyczynil sie do polepszenia problemu na okres okolo 3 lat,
# jednak po 2018-2019 problem znowu zaczal sie nasilac
# mozna zauwazyc tendecje rosnace (2007 - 2012) oraz 2019 - 2023
# oraz spadkowa 2012 - 2019


## pomysl do dalszego realizowania 
## dodac do aplikacji shiny mozliwosc wyboru (osobnie wojewodztwa oraz opcjii
## "przestrzen lat") i wyswietlac wykresy podobne do poprzedniego interaktywnie dla
## posczegolnego wojewodztwa








