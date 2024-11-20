library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(RColorBrewer)

perm <- file.choose()
permanent <- read.csv(perm) %>%
  select(continent = ParentLocation, country = Location, permanent = FactValueNumeric) %>%
  mutate(country = case_when(
    country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    country == "United States of America" ~ "USA",
    country == "Russian Federation" ~ "Russia",
    country == "Iran (Islamic Republic of)" ~ "Iran",
    country == "Bolivia (Plurinational State of)" ~ "Bolivia",
    country == "Syrian Arab Republic" ~ "Syria",
    country == "United Republic of Tanzania" ~ "Tanzania",
    country == "Republic of Moldova" ~ "Moldova",
    country == "Swaziland" ~ "Eswatini",
    country == "Cote d'Ivoire" ~ "Ivory Coast",
    country == "Türkiye" ~ "Turkey",
    country == "Republic of Korea" ~ "South Korea",
    country == "Democratic People's Republic of Korea" ~ "North Korea",
    country == "Czechia" ~ "Czech Republic",
    country == "Viet Nam" ~ "Vietnam",
    country == "Lao People's Democratic Republic" ~ "Laos",
    country == "Congo" ~ "Republic of Congo",
    country == "United Kingdom of Great Britain and Northern Ireland" ~ "UK",
    TRUE ~ country  # zostawia inne kraje bez zmian
  )) %>% 
  mutate(perm_groups = cut(permanent,
                 breaks = seq(20,50,10)))


world_map <- map_data("world") %>%
  filter(abs(long)<180) %>% filter(!(region %in% c("Antarctica", "Greenland")))

data_map <- world_map %>%
  left_join(permanent, by = c("region" = "country")) %>% 
  mutate(perm_groups = ifelse(is.na(perm_groups), "Brak danych", perm_groups))

colors <-c(brewer.pal(9, "GnBu")[c(5,7,9)], "grey")

ggplot() +
  geom_polygon(data = data_map, mapping = aes(x = long, y = lat, group = group, fill = perm_groups),
               color = "white") +
  scale_fill_manual(values = colors,
                    labels = c("20-30%","30-40%", "40-50%", "Brak danych")) +
    labs(x = element_blank(), y = element_blank(),
       fill = "Procent próchnicy\nw społeczeństwie") +
  coord_fixed(1.3) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(color = "white", size = 15, face = "bold"),
    legend.text = element_text(color = "white", size = 15, face = "bold"),
    legend.background = element_blank(),
    axis.line = element_blank(),           # Usuń linie osi
    axis.ticks = element_blank(),          # Usuń znaczniki osi
    axis.text = element_blank(),
    plot.background = element_blank(),       # usuwa tło całego wykresu
    panel.grid.major = element_blank(),      # usuwa główne linie siatki
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),    # Usuń tło panelu
    ) 

ggsave("nieleczona_próchnica_trwałe.png", width = 15, height = 8)

