install.packages(c("ggplot2", "dplyr", "maps", "mapdata"))
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)

df2 <- read.csv("C:/Users/macie/OneDrive/Pulpit/TWD/projekt 1/Table2_5.csv")
df2 <- df2 %>% 
  rename(region = Country) %>% 
  mutate(region = recode(region, "Czechia" = "Czech Republic")) %>% 
  mutate(region = recode(region, "United Kingdom" = "UK")) %>% 
  mutate(divorces = round(Divorce_rate*Count),
         Care_supply = round(Care_supply),
         SpecialistsPerDivorce = Care_supply/divorces)


world_map <- map_data("world")

mapa <- world_map %>%
  filter(region %in% df2$region) %>%
  left_join(df2, by = "region") %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = SpecialistsPerDivorce), color = "white") +
  scale_fill_gradient(low = "navyblue", high = "cyan", na.value = "grey90") +
  theme_minimal() +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    panel.grid = element_blank(), 
    axis.title = element_blank(), # Usuwa tytuły osi
    legend.title = element_blank(), # Usuwa tytuł legendy
    legend.text = element_blank() # Usuwa napisy w legendzie
  ) +
  coord_cartesian(ylim = c(35, 70), xlim = c(-10, 40))
mapa

ggsave("mapa_wysoka_rozdzielczosc.png", plot = mapa, dpi = 300, width = 10, height = 8)
