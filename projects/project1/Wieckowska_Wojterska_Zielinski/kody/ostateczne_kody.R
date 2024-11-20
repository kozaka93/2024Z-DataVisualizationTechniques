library(ggplot2)
library(dplyr)
library(maps)
library(countrycode)
library(stringr)
library(tidyr)
library(readr)


world_map <- map_data("world")
df_toothpaste <- read.csv('data.csv')
df_dentists <-read.csv('dentists.csv')
df_sugar <- read.csv('sugar-consumption-by-country-2024.csv')
df_edentulism <- read.csv('edentulism.csv')
df_population <- read.csv('population_years.csv')


### Zabawa z danymi ###

population <- df_population %>% 
  select(Country.Code,35:68) %>% 
  pivot_longer(cols = starts_with("X"),names_to = "year",names_prefix = "X",values_to = "population") %>% 
  rename(code = Country.Code) %>% 
  mutate(year = as.numeric(year))

dentists <- df_dentists %>%
  select(SpatialDimValueCode,Period,FactValueNumeric) %>% 
  rename(code = SpatialDimValueCode,year=Period) %>% 
  mutate(continent = countrycode(code, "iso3c", "continent")) %>% 
  bind_rows(japonia)  %>% 
  left_join(population, by = c('code','year'))  %>% 
  mutate(total_doctors = FactValueNumeric * population / 10000) %>% 
  group_by(continent, year) %>% 
  summarise(total_doctors = sum(total_doctors, na.rm = TRUE), 
          total_population = sum(population, na.rm = TRUE)) %>% 
  mutate(value = total_doctors / total_population * 10000)

edentulism <- df_edentulism %>% 
  select(ParentLocation, SpatialDimValueCode,Period,FactValueNumeric) %>% 
  rename(code = SpatialDimValueCode) 

sugar <- df_sugar %>% 
  select(country,SugarConsumption_SugarAndSweeteners_AnnualConsPerCapita_Kg_2022) %>% 
  rename(value = SugarConsumption_SugarAndSweeteners_AnnualConsPerCapita_Kg_2022) %>% 
  mutate(value_in_grams = (value * 1000)/365) %>% 
  mutate(code = countrycode(country, "country.name", "iso3c")) %>% 
  mutate(continent = countrycode(code, "iso3c", "continent")) %>% 
  filter(!is.na(continent))


### Mapa bezzębia ###


map_data <- world_map %>%
  filter(long <= 180) %>% 
  mutate(code = countrycode(region, "country.name", "iso3c")) %>% 
  filter(!is.na(code)) %>% 
  left_join(edentulism, by = 'code')
  
export_map <- ggplot(map_data, aes(long, lat, group = group, fill = FactValueNumeric )) +
  geom_polygon(color = "grey40", linewidth =0.25) +
  scale_fill_steps(
    low = "grey",  
    high = "#DB5D25",
    na.value = "ivory1",
    guide = guide_colorbar(title = NULL,label.theme = element_text(color = "white"))
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),       
        panel.grid = element_blank(),
        panel.background = element_blank())
  

export_map

ggsave(export_map, filename = "mercator_grey_orange.png", bg = "transparent",width = 10, height = 6, dpi = 600)



### Wykres spożycia cukru ###

export_sugar <- ggplot(sugar %>% filter(!is.na(value_in_grams)), aes(x = continent, y = value_in_grams, fill = continent)) +
  geom_violin(alpha = 0.6, color = NA, trim = TRUE) +  
  geom_boxplot(width = 0.14, alpha = 0.9, size = 0.3, color = "black", outlier.shape = NA) +  
  scale_fill_brewer(palette = "Pastel1") +
  scale_y_continuous(limits = c(0, 302)) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        panel.grid.major.x  = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.text.y = element_text(colour = "white", size = 20))

export_sugar

ggsave(export_sugar, filename = "sugar_colourful_3.png", bg = "transparent",width = 6, height = 6, dpi = 600)


### Wykres liczby lekarzy na 10000 mieszkańców ###
export_dentists <- ggplot(dentists, aes(x = year, y = value, color = continent)) +
  geom_line(linewidth = 1.5) +  
  geom_point(size = 3) +  
  scale_color_brewer(palette = "Pastel1")+
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),       
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "white", size = 20),  
        axis.text.y = element_text(colour = "white", size = 20))

export_dentists


ggsave(export_dentists, filename = "dentists_2.png", bg = "transparent",width = 10, height = 6, dpi = 600)

