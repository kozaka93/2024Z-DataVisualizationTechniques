library(sf)
library(tigris)
library(cowplot)
library(ggplot2)
czynniki <- read.csv("C:/Users/Admin/Desktop/heart_2022_no_nans.csv")
usa_states <- states(cb = TRUE)
czynniki$State <- tolower(czynniki$State)
usa_states$State <- tolower(usa_states$NAME)
merged_data <- usa_states %>%
  left_join(czynniki, by = "State")
czynniki <- czynniki %>% filter(SmokerStatus!= "Former smoker") %>% 
  mutate(Smokepower = case_when(SmokerStatus == "Never smoked" ~0, SmokerStatus == "Current smoker - now smokes every day" ~1, SmokerStatus == "Current smoker - now smokes some days" ~ 0.5)) %>% 
  group_by(State) %>% summarise(Smokepower = mean(Smokepower))#0.5 - palacz lekki; 0 - brak palenia; 1- palacz
continental_us <- merged_data %>% filter(!State %in% c("alaska", "hawaii"))
alaska <- merged_data %>% filter(State == "alaska")
hawaii <- merged_data %>% filter(State == "hawaii")
continental_plot <- ggplot(data = continental_us) +
  geom_sf(aes(fill = Smokepower), color = "white") +
  scale_fill_viridis_c(option = "B") +
  theme_void() +
  theme(legend.position = c(0.85,0.5)) +
  labs(title = "Mapa chloropletyczna Smokepower dla USA", plot.title = element_text(hjust = 1)) +
  coord_sf(xlim = c(-180, 0), ylim = c(0, 70))
alaska_plot <- ggplot(data = alaska) +
  geom_sf(aes(fill = Smokepower), color = "white") +
  scale_fill_viridis_c(option = "B")+
  theme_void() +
  coord_sf(xlim = c(-180, -130), ylim = c(50, 72)) +  # Powiększenie mapy Alaski
  theme(legend.position = "none")
hawaii_plot <- ggplot(data = hawaii) +
  geom_sf(aes(fill = Smokepower), color = "white") +
  scale_fill_viridis_c(option = "B") +
  theme_void() +
  coord_sf(xlim = c(-161, -154), ylim = c(18, 23)) +  # Powiększenie mapy Hawajów
  theme(legend.position = "none")
final_map <- ggdraw() +
  draw_plot(continental_plot, 0, 0, 1, 1) +  
  draw_plot(alaska_plot, 0.6, 0.35, 0.15, 0.15) +  
  draw_plot(hawaii_plot, 0.6, 0.5, 0.12, 0.12)
print(final_map)
ggsave("mapa.png", plot = final_map, width = 16, height = 10, dpi = 300)