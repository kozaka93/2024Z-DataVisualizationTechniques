library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tidyr)
library(dplyr)
library(ggplot2)

type <- read.table("hsw_ph3_06_tabular.tsv", sep = "\t", header = TRUE)

type <- type %>%
  separate(freq.deviatn.severity.nace_r2.sex.age.unit.geo.TIME_PERIOD, into = c("Frequency", "Deviation", "Severity","Job","Sex","Age", "Unit","Country"), sep = ",") %>%
  mutate(across(starts_with("X"), as.numeric)) %>%
  pivot_longer(
    cols = starts_with("X"),      
    names_to = "year",              
    values_to = "Victims"             
  ) %>%
  mutate(year = as.numeric(sub("X", "", year)))

mapa_europa <- ne_countries(scale = "medium", returnclass = "sf", continent = "Europe")
mapa_europa <- mapa_europa %>%
  select(iso_a2_eh)

type1 <- type %>%
  filter(!is.na(Victims),Sex != "UNK", Sex != "T",Unit == "NR") %>%
  group_by(Country) %>%
  mutate(total = sum(Victims)) %>%
  group_by(Country,Sex) %>%
  mutate(FemaleAccidentPercentage = sum(Victims)/total*100) %>%
  distinct(FemaleAccidentPercentage) %>%
  filter(Sex == "F", Country != "EU28",Country != "EU27_2007",Country != "EU27_2020") %>%
  mutate(Country = case_when(
    Country == "EL" ~ "GR",
    Country == "UK" ~ "GB",
    TRUE ~ Country
  ))

mapa_dane <- mapa_europa %>%
  left_join(type1, by = c("iso_a2_eh" = "Country")) %>%
  filter(iso_a2_eh != "RU")

plot <- ggplot(mapa_dane) +
  geom_sf(aes(fill = FemaleAccidentPercentage), color = "black", size = 0.1) +
  scale_fill_gradient(low = "#E9D8A6", high = "#9B2226", na.value = "#94D2BD",
                      name = "Percentage of women who\nhad an accident at work(%)",
                      guide = guide_colorbar(direction = "horizontal")) +
  theme_minimal() +
  labs(
    title = "Percentage share of women\nin accidents at work in 2013-2022"
  ) +
  coord_sf(xlim = c(-25, 35), ylim = c(32
                                       , 72), expand = FALSE) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",                  
    panel.grid = element_line(color = "black"),
    legend.box = "horizontal",
    axis.text = element_text(size = 12) 
  ) 
plot
#ggsave("womanatwork.png", plot = plot, bg = "transparent",
       width = 16, height = 10, units = "in", dpi = 300)