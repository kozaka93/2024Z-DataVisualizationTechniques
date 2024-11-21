#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#                                              wykres Elissa
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------


library(ggplot2)
library(dplyr)
library(tidyr)

mhq_education <- data.frame(
  Region = c("South Asia", "South-East Asia", "Europe", "Latin America & Caribbean", 
             "Middle East & North Africa", "North America", "Oceania", "Sub-Saharan Africa"),
  Primary_Education = c(23.6, 69.4, 62.2, 54.3, 42.4, 53.2, 49.0, 62.4),
  Some_High_School = c(41.5, 73.7, 56.1, 67.6, 46.8, 50.5, 41.0, 45.9),
  High_School = c(31.3, 79.6, 58.3, 67.5, 53.1, 65.8, 54.1, 54.0),
  Associate_Degree = c(69.6, 90.5, 76.3, 73.9, 67.5, 86.0, 85.7, 72.5),
  Vocational_Certification = c(80.9, 88.0, 70.7, 89.2, 66.1, 81.5, 64.1, 70.9),
  Bachelors_Degree = c(63.0, 95.8, 73.4, 86.9, 66.4, 91.2, 75.6, 76.1),
  Masters_Degree = c(77.4, 112.0, 87.8, 110.2, 78.8, 103.7, 84.2, 90.5),
  PhD_Doctorate_MD_JD = c(95.8, 121.0, 86.7, 91.7, 89.9, 108.9, 84.7, 85.6)
)

mhq_education <- mhq_education %>% 
  select(Region, Primary_Education, High_School, Bachelors_Degree, Masters_Degree, PhD_Doctorate_MD_JD)
colnames(mhq_education) <- c("Region", "Primary Education", "High School", "Bachelors Degree", "Masters Degree", "PhD Doctorate")
mhq_education <- pivot_longer(mhq_education, cols = -Region, names_to = "Education_Level", values_to = "MHQ_Score")
mhq_education$Education_Level <- factor(mhq_education$Education_Level, 
                                        levels = c("Primary Education", 
                                                   "High School", 
                                                   "Bachelors Degree", 
                                                   "Masters Degree", 
                                                   "PhD Doctorate"))

c("#165a2b", "#3c6d4d", "#9fb7a7", "#cfdbd3", "#ec5527", "#efdbd4", "#f7abd2", "#fff4f0")

ggplot(mhq_education, aes(x = Education_Level, y = MHQ_Score, color = Region)) +
  geom_point(size = 2.5) +  
  geom_line(aes(group = Region), size = 1) +  
  labs(title = "MHQ and the level of education for different regions", 
       x = "Level of education", 
       y = "MHQ") +
  scale_colour_manual(values = c("#133d28","#3c6d4d","#9fb7a7","#cfdbd3","#fff4f0", "#ec9ac7","lightcoral","#d04520"),
                      labels = c("South-East Asia", "North America", "Latin America & Caribbean",
                        "Oceania", "Sub-Saharan Africa" , "Europe", "Middle East & North Africa","South Asia"),
                      breaks = c("South-East Asia", "North America", "Latin America & Caribbean",
                                 "Oceania", "Sub-Saharan Africa" , "Europe", "Middle East & North Africa","South Asia"
                                 ))+
  theme_minimal() +  
  theme(
    plot.title = element_blank(),
    plot.background = element_rect(fill = "#efdbd4"),     
    axis.text.x = element_text(angle = 45, hjust = 1))

colors <- c(
  "#165a2b", "#3c6d4d", "#9fb7a7", "#cfdbd3", "#ec5527", "#efdbd4", "#f7abd2", "#fff4f0",
  "#124d26", "#2e6045", "#8ca698", "#b8c9c1", "#d04520", "#dcbcb0", "#e291b5", "#ffe7e1",
  "#133d28", "#2b5d44", "#819c8f", "#aac6b8", "#b3391c", "#e4c9c3", "#ec9ac7", "#ffefea",
  "#0f4a23", "#295541", "#789f8d", "#b2c7bf", "#cd4b1a", "#e8d3c9", "#f3a4c6", "#fff8f4",
  "#194b2c", "#366e4e", "#91b6a8", "#d1e2d7", "#f05129", "#f0ddd6", "#f8b9d4", "#fffff7"
)









#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#                                              wykres Oliwia
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(maps)
library(maptools)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

countries <- read.csv('~/twd projekt 1/countries.csv')

countries <- countries %>%
  separate(Country.Overall.MHQ.Score, into = c("Country", "MHQ"), sep = ";", remove = TRUE)

world_map <- map_data("world")

missing_countries <- countries %>%
  anti_join(world_map, by = c("Country" = "region"))

countries_in_worldmap <- countries %>%
  mutate(Country = case_when(
    Country == "Congo Democratic Republic" ~ "Democratic Republic of the Congo",
    Country == "Cote d'Ivoire" ~ "Ivory Coast",
    Country == "Trinidad and Tobago" ~ "Trinidad",
    Country == "United Kingdom" ~ "UK",
    Country == "United States" ~ "USA",
    TRUE ~ Country 
  ))  

new_row <- countries %>%
  filter(Country == "Trinidad and Tobago") %>%  
  mutate(Country = "Tobago")

countries_in_worldmap2 <- countries_in_worldmap %>%
  bind_rows(new_row) %>% 
  arrange(Country)

countries_in_worldmap3 <- countries_in_worldmap2 %>%
  mutate(MHQ = as.numeric(MHQ),  # Upewnij si??, ??e MHQ jest liczb??
         MHQ_category = cut(
           MHQ,
           breaks = c(0, 50, 60, 70, 80, 100),  
           labels = c("0-50", "51-60", "61-70", "71-80", "81-100"), 
           right = TRUE
         ))

merged_data <- world_map %>%
  left_join(countries_in_worldmap3, by = c("region" = "Country")) %>% 
  mutate(MHQ_category = ifelse(is.na(MHQ_category), "No Data", as.character(MHQ_category)))



c("#165a2b", "#3c6d4d", "#9fb7a7", "#cfdbd3", "#ec5527", "#efdbd4", "#f7abd2", "#fff4f0")



ggplot(data = merged_data) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = MHQ_category)) +  
  scale_fill_manual(values = c("No Data" = "#fff4f0",  # Kolor dla krajow bez danych
                               "0-50" = "#ec5527",  # Jasny niebieski
                               "51-60" = "#f7abd2",  # Niebieski
                               "61-70" = "#cfdbd3",  # Ciemniejszy niebieski
                               "71-80" = "#9fb7a7",  # Granatowy
                               "81-100" = "#165a2b"),  # Ciemnoniebieski
                    name = "MHQ") +
  labs(title = "Overall MHQ Score")+
  theme_minimal() +
  theme(legend.position = "right",
        axis.title.x = element_blank(),  # Ukrycie etykiet osi X
        axis.title.y = element_blank(),  # Ukrycie etykiet osi Y
        axis.text.x = element_blank(),  # Ukrycie etykiet osi X
        axis.text.y = element_blank(),  # Ukrycie etykiet osi Y
        axis.ticks = element_blank(),     # Ukrycie znacznikow osi
        panel.grid.major = element_blank(),  # Ukrycie siatki g??ownej
        panel.grid.minor = element_blank(),  # Ukrycie siatki pomocniczej
        plot.title = element_text(size = 16, hjust = 0.5),
        #panel.background = element_rect(fill = "#efdbd4"),  # Kolor tła panelu wykresu
        plot.background = element_rect(fill = "#efdbd4"))+  # Dostosowanie tytu??u
  coord_fixed(ratio = 1.3)





#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#                                              wykres Julia
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------


library(readr)
library(ggplot2)

rodzina <- read.csv("~/twd projekt 1/dane-rodzina-csv.csv", sep = ';')
znajomi <- read.csv("~/twd projekt 1/dane-znajomi-csv.csv", sep = ';')

str(rodzina)
rodzina$X1 <- as.numeric(gsub(",", ".", rodzina$X1))
rodzina$X2 <- as.numeric(gsub(",", ".", rodzina$X2))
rodzina$X3 <- as.numeric(gsub(",", ".", rodzina$X3))
rodzina$X4 <- as.numeric(gsub(",", ".", rodzina$X4))
rodzina$X5 <- as.numeric(gsub(",", ".", rodzina$X5))

znajomi$X0 <- as.numeric(gsub(",", ".", znajomi$X0))
znajomi$od.1.do.3 <- as.numeric(gsub(",", ".", znajomi$od.1.do.3))
znajomi$od.4.do.6 <- as.numeric(gsub(",", ".", znajomi$od.4.do.6))
znajomi$od.7.do.9 <- as.numeric(gsub(",", ".", znajomi$od.7.do.9))
znajomi$X10. <- as.numeric(gsub(",", ".", znajomi$X10.))


ggplot(rodzina, aes(x = wiek)) +
  geom_line(aes(y = X1, color = "1", group = 1), size = 0.6) +
  geom_line(aes(y = X2, color = "2", group = 2), size = 0.6) +
  geom_line(aes(y = X3, color = "3", group = 3), size = 0.6) +
  geom_line(aes(y = X4, color = "4", group = 4), size = 0.6) +
  geom_line(aes(y = X5, color = "5", group = 5), size = 0.6) +
  geom_point(aes(y = X1, color = "1", group = 1)) +
  geom_point(aes(y = X2, color = "2", group = 2)) +
  geom_point(aes(y = X3, color = "3", group = 3)) +
  geom_point(aes(y = X4, color = "4", group = 4)) +
  geom_point(aes(y = X5, color = "5", group = 5)) +
  labs(x = "Age group",
       y = "MHQ score",
       color = "Household atmosphere 
(5 meaning the best)") +
  scale_color_manual(values = c("1" = "#165a2b",  
                                "2" = "#9fb7a7",  
                                "3" = "#fff4f0", 
                                "4" = "#e291b5", 
                                "5" = "#b3391c"),
                     breaks = c("1", "2", "3", "4", "5")) + 
  
  theme_minimal()+
  theme(
    plot.title = element_blank(),
    plot.background = element_rect(fill = "#efdbd4"))


legend= c("Znajomi 0", "Znajomi 1-3", "Znajomi 4-6", "Znajomi 7-9", "Znajomi 10+")

colors <- c(
  "#165a2b", "#3c6d4d", "#9fb7a7", "#cfdbd3", "#ec5527", "#efdbd4", "#f7abd2", "#fff4f0",
  "#124d26", "#2e6045", "#8ca698", "#b8c9c1", "#d04520", "#dcbcb0", "#e291b5", "#ffe7e1",
  "#133d28", "#2b5d44", "#819c8f", "#aac6b8", "#b3391c", "#e4c9c3", "#ec9ac7", "#ffefea",
  "#0f4a23", "#295541", "#789f8d", "#b2c7bf", "#cd4b1a", "#e8d3c9", "#f3a4c6", "#fff8f4",
  "#194b2c", "#366e4e", "#91b6a8", "#d1e2d7", "#f05129", "#f0ddd6", "#f8b9d4", "#fffff7"
)

       
       
ggplot(znajomi, aes(x = wiek)) +
  geom_line(aes(y = X0, color = "0 friends", group = 1), size = 0.6) +
  geom_line(aes(y = od.1.do.3, color = "1-3 friends", group = 2), size = 0.6) +
  geom_line(aes(y = od.4.do.6, color = "4-6 friends", group = 3), size = 0.6) +
  geom_line(aes(y = od.7.do.9, color = "7-9 friends", group = 4), size = 0.6) +
  geom_line(aes(y = X10., color = "10+ friends", group = 5), size = 0.6) +
  geom_point(aes(y = X0, color = "0 friends", group = 1)) +
  geom_point(aes(y = od.1.do.3, color = "1-3 friends", group = 2)) +
  geom_point(aes(y = od.4.do.6, color = "4-6 friends", group = 3)) +
  geom_point(aes(y = od.7.do.9, color = "7-9 friends", group = 4)) +
  geom_point(aes(y = X10., color = "10+ friends", group = 5)) +
  labs(x = "Age group",
        y = "MHQ score",
        color = "Amount of friends") +  
  scale_color_manual(values = c("0 friends" = "#165a2b",
                               "1-3 friends" = "#9fb7a7",
                               "4-6 friends" = "#fff4f0",
                               "7-9 friends" = "#e291b5",
                               "10+ friends" = "#b3391c"),
                     breaks = c("0 friends" ,
                                "1-3 friends",
                                "4-6 friends" ,
                                "7-9 friends" ,
                                "10+ friends")) +  
  theme_minimal()+
  theme(
    plot.title = element_blank(),
    plot.background = element_rect(fill = "#efdbd4"))









# ggplot(mhq_education, aes(x = Education_Level , y = MHQ_Score, fill = Region)) +
#   geom_bar(stat = "identity", position = "dodge") +  
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
#   labs(title = "MHQ vs. Poziom eukacji dla różnych regionów", 
#        x = "Poziom edukacji", 
#        y = "Średni wynik MHQ") +
#   theme_minimal() +
#   scale_fill_grey()
# 
# ggplot(mhq_education, aes(x = Education_Level , y = MHQ_Score, fill = Region)) +
#   geom_boxplot() +  
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
#   labs(title = "MHQ vs. Poziom eukacji dla różnych regionów", 
#        x = "Poziom edukacji", 
#        y = "Średni wynik MHQ") +
#   theme_minimal() +
#   scale_fill_grey()
# 
# mhq_age <- data.frame(
#   Region = c("South Asia", "South-East Asia", "Europe", 
#              "Latin America & Caribbean", "Middle East & North Africa", 
#              "North America", "Oceania", "Sub-Saharan Africa"),
#   "18-24" = c(28.4, 50.0, 35.8, 34.0, 40.7, 43.6, 31.6, 50.4),
#   "25-34" = c(39.7, 52.5, 45.1, 51.1, 56.5, 50.7, 31.9, 74.2),
#   "35-44" = c(62.3, 80.8, 51.5, 72.4, 72.5, 54.5, 43.8, 85.3),
#   "45-54" = c(86.0, 103.0, 60.6, 91.4, 87.2, 64.6, 49.4, 94.4),
#   "55-64" = c(98.7, 115.7, 77.2, 108.1, 96.8, 78.3, 60.7, 104.5),
#   "65-74" = c(103.4, 118.2, 99.0, 114.0, 100.1, 103.4, 88.5, 109.1),
#   "75+" = c(103.7, 104.5, 104.8, 109.9, 97.1, 122.3, 112.9, 111.5)
# )
# 
# colnames(mhq_age) <- c("Region", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
# mhq_age <- pivot_longer(mhq_age, cols = -Region, names_to = "Age_Group", values_to = "Value")
# 
# mhq_age$Region <- as.factor(mhq_age$Region)
# 
# mhq_age$Age_Group <- factor(mhq_age$Age_Group, 
#                                  levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"))
# 
# 
# ggplot(mhq_age, aes(x = Age_Group  , y =Value)) +
#   geom_violin() +
#   labs(title = "Violin Plot of Values by Region and Age Group",
#        x = "Age Group",
#        y = "Value") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# country_data <- data.frame(
#   Country = c("Australia", "Canada", "India", "New Zealand", 
#               "Singapore", "South Africa", "United Kingdom", "United States"),
#   "2020" = c(60.3, 66.0, 61.9, 60.3, 94.4, 55.9, 54.0, 72.4),
#   "2021" = c(53.5, 62.9, 55.8, 59.5, 74.1, 45.6, 46.4, 63.1),
#   "2022" = c(54.4, 64.4, 58.8, 63.7, 74.2, 47.5, 46.2, 67.9)
# )
# 
# colnames(country_data) <- c("Country", "2020", "2021", "2022")
# country_data <- pivot_longer(country_data, cols = -Country, names_to = "Year", values_to = "Value")
# 
# ggplot(country_data, aes(x = Year, y = Value, color = Country, group = Country)) +
#   geom_line(size = 1) +  
#   geom_point(size = 2) + 
#   labs(title = "MHQ values in chosen countries, 2020-2022",
#        x = "Year",
#        y = "MHQ") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

