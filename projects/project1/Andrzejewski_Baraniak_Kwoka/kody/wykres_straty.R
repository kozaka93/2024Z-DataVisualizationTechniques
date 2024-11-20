library(dplyr)
library(ggplot2)
library(forcats)

options(scipen = 12)

straty <- read.csv("straty.csv")
populacja <- read.csv("world_population.csv")
cbc <- read.csv("cbc.csv")


straty <- straty %>% 
  select(Location, FactValueNumeric)

straty_pop <- straty %>% 
  inner_join(populacja, by = "Location") %>% 
  mutate(per_person = (FactValueNumeric*1000000/Population)) %>% 
  select(Location, per_person) %>%
  inner_join(cbc, by="Location") %>% 
  arrange(-per_person)


custom_colors <- c(
  "Africa" = "#5de1e6",               
  "North America" = "#3d9ca4",   
  "South America" = "darkred",          
  "Mediterranean" = "#f9dca0", 
  "Europe" = "#f27c7d",           
  "Asia" = "#ffb8b1",   
  "Oceania" = "#ffea7f"   
)


wyk1 <- straty_pop %>% 
  head(n=10)


got1 <- wyk1 %>% 
  ggplot(aes(x = reorder(Location, per_person), y = per_person, fill = Continent)) +
  geom_col() +
  scale_fill_manual(values = custom_colors) +
  labs(
    x = "Kraj", 
    y = "Średnie straty w pracy na mieszkańca \nz powodu problemów z zębami "
  ) +
  theme_minimal() +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "white"),                
    panel.grid.minor = element_line(color = "white"),
    axis.title.x = element_text(color = "white", size = 24, face = "bold"),
    axis.title.y = element_text(color = "white", size = 24, face = "bold"),
    axis.text = element_text(color = "white", size = 24, face = "bold"),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::dollar)+
  coord_flip()

got1

ggsave("straty.png", plot = got1, bg = "transparent", width = 9, height = 6, dpi = 300)