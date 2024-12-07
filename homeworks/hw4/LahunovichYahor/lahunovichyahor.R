library(dplyr)
library(ggplot2)
library(readxl)
library(geodata)
library(tidyr)
library(sf)
library(stringr)
library(patchwork)
library(broom)

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)


# Live births and babies left in hospital

# Load data
birth <- read_excel("~/Documents/uni/sem_3/twd/2024Z-DataVisualizationTechniques/homeworks/hw4/LahunovichYahor/DANE/Noworodki opuszczone przez rodziców/Urodzenia żywe w Polsce 2007-2023.xlsx")
hospitals <- read_excel("~/Documents/uni/sem_3/twd/2024Z-DataVisualizationTechniques/homeworks/hw4/LahunovichYahor/DANE/Noworodki opuszczone przez rodziców/Noworodki pozostawione w szpitalu 2007-2023.xlsx")

# clear data and rename columns
hospitals <- hospitals %>% 
  rename_with(~c("wojewodstwo", seq(2007, 2023, 1))) %>% 
  mutate(wojewodstwo = tolower(wojewodstwo))

hospitals <- hospitals[seq(8, 24, 1),]
hospitals <- hospitals %>% 
  mutate(across(-wojewodstwo, as.numeric))

birth <- birth %>% 
  rename_with(~c("wojewodstwo", seq(2007, 2023, 1))) %>% 
  mutate(wojewodstwo = tolower(wojewodstwo)) %>% 
  mutate(across(-wojewodstwo, as.numeric))

hospitals_long <- hospitals %>%
  pivot_longer(cols = -wojewodstwo, names_to = "year", values_to = "hospital_left") %>%
  mutate(year = as.numeric(year))

birth_long <- birth %>%
  pivot_longer(cols = -wojewodstwo, names_to = "year", values_to = "births") %>%
  mutate(year = as.numeric(year))

birth_hospitals_long <- hospitals_long %>%
  inner_join(birth_long, by = c("wojewodstwo", "year"))

# percentage hospitals/birth
percentage_birth_hospitals <- birth_hospitals_long %>%
  mutate(percentage_left = (hospital_left / births) * 100)

# Whole Poland
percentage_birth_hospitals_poland <- percentage_birth_hospitals %>% 
  filter(wojewodstwo == "polska")
poland_plot <- ggplot(percentage_birth_hospitals_poland, aes(x = year, y = percentage_left)) +
  geom_col(fill = "#315ca8", alpha = 1) +
  scale_x_continuous(breaks = seq(2007, 2023, 1))+
  labs(x = "Year",
       y = "Percentage left babies of all newborns(%)",
       title = "Newborns Left in Hospital for Reasons OTHER than Their Health Condition in Poland",
       caption = "Fundacja Gajusz \n#BI_NGO") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "Raleway"),
    axis.title.x = element_text(size = 11, face = "bold", family = "Raleway"),
    axis.title.y = element_text(size = 11, face = "bold", family = "Raleway"),

    plot.caption = element_text(size = 10, face = "italic", family = "Raleway", color = "#505050"),
    axis.text.x = element_text(size = 10, family = "Raleway"),
    axis.text.y = element_text(size = 10, family = "Raleway"),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave(
  "~/Documents/uni/sem_3/twd/2024Z-DataVisualizationTechniques/homeworks/hw4/LahunovichYahor/images/poland_plot.png", 
  plot = poland_plot, 
  width = 15, 
  height = 8, 
  units = "in",
  dpi = 150
)

# map
map <- geodata::gadm("Poland", level = 1, path = tempdir())
map_df <- as.data.frame(map)
map1 <- st_as_sf(map)

# 2007
mapdf_2007 <- percentage_birth_hospitals %>%
  filter(year == "2007")

mapdf_2007$wojewodstwo <- str_to_title(mapdf_2007$wojewodstwo)

map_with_data1 <- map1 %>%
  left_join(mapdf_2007, by = c("NAME_1" = "wojewodstwo"))

p1 <- ggplot(map_with_data1) +
  geom_sf(aes(fill = percentage_left)) +
  scale_fill_gradientn(colors = c("#b5e0f3", "#315ca8", "#303174"), name = "Percentage left") +
  geom_sf_text(aes(label = NAME_1), size = 3, color = "white", fontface = "bold") + 
  theme_minimal(base_family = "Raleway") +
  theme(
    text = element_text(family = "Raleway"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(-0.2, 0.5),
    legend.direction = "vertical",
    legend.justification = c("left", "center"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic", family = "Raleway", color = "#505050")
  ) +
  labs(
    title = "Newborns Left in Hospital for Reasons OTHER than Their Health Condition in Poland by Voivodeships in 2007",
    caption = "Fundacja Gajusz \n#BI_NGO"
  )
ggsave(
  "~/Documents/uni/sem_3/twd/2024Z-DataVisualizationTechniques/homeworks/hw4/LahunovichYahor/images/poland_map1.png", 
  plot = p1, 
  width = 15, 
  height = 8, 
  units = "in",
  dpi = 150
)

# 2015
mapdf_2015 <- percentage_birth_hospitals %>%
  filter(year == "2015")

mapdf_2015$wojewodstwo <- str_to_title(mapdf_2015$wojewodstwo)

map_with_data2 <- map1 %>%
  left_join(mapdf_2015, by = c("NAME_1" = "wojewodstwo"))

p2 <- ggplot(map_with_data2) +
  geom_sf(aes(fill = percentage_left)) +
  scale_fill_gradientn(colors = c("#b5e0f3", "#315ca8", "#303174"), name = "Percentage left") +
  geom_sf_text(aes(label = NAME_1), size = 3, color = "white", fontface = "bold") + 
  theme_minimal(base_family = "Raleway") +
  theme(
    text = element_text(family = "Raleway"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(-0.2, 0.5),
    legend.direction = "vertical",
    legend.justification = c("left", "center"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(face = "italic", family = "Raleway", color = "#505050")
  ) +
  labs(
    title = "Newborns Left in Hospital for Reasons OTHER than Their Health Condition in Poland by Voivodeships in 2015",
    caption = "Fundacja Gajusz \n#BI_NGO"
  )

ggsave(
  "~/Documents/uni/sem_3/twd/2024Z-DataVisualizationTechniques/homeworks/hw4/LahunovichYahor/images/poland_map2.png", 
  plot = p2, 
  width = 15, 
  height = 8, 
  units = "in",
  dpi = 150
)

  # 2023
mapdf_2023 <- percentage_birth_hospitals %>%
  filter(year == "2023")

mapdf_2023$wojewodstwo <- str_to_title(mapdf_2023$wojewodstwo)

map_with_data <- map1 %>%
  left_join(mapdf_2023, by = c("NAME_1" = "wojewodstwo"))

p3 <- ggplot(map_with_data) +
  geom_sf(aes(fill = percentage_left)) +
  scale_fill_gradientn(colors = c("#b5e0f3", "#315ca8", "#303174"), name = "Percentage left") +
  geom_sf_text(aes(label = NAME_1), size = 3, color = "white", fontface = "bold") + 
  theme_minimal(base_family = "Raleway") +
  theme(
    text = element_text(family = "Raleway"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(-0.2, 0.5),
    legend.direction = "vertical",
    legend.justification = c("left", "center"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(face = "italic", family = "Raleway", color = "#505050")
  ) +
  labs(
    title = "Newborns Left in Hospital for Reasons OTHER than Their Health Condition in Poland by Voivodeships in 2023",
    caption = "Fundacja Gajusz \n#BI_NGO"
  )

ggsave(
  "~/Documents/uni/sem_3/twd/2024Z-DataVisualizationTechniques/homeworks/hw4/LahunovichYahor/images/poland_map3.png", 
  plot = p3, 
  width = 15, 
  height = 8, 
  units = "in",
  dpi = 150
)

p1_new <- p1 + labs(title = "2007", caption = NULL) +
  scale_fill_gradientn(
    colors = c("#b5e0f3", "#315ca8", "#303174"),
    name = "Percentage left",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom"
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(face = "bold"),
    legend.justification = "center"
  )

p2_new <- p2 + labs(title = "2015", caption = NULL) +
  scale_fill_gradientn(
    colors = c("#b5e0f3", "#315ca8", "#303174"),
    name = "Percentage left",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom"
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(face = "bold"),
    legend.justification = "center"
  )

p3_new <- p3 + labs(title = "2023", caption = NULL) +
  scale_fill_gradientn(
    colors = c("#b5e0f3", "#315ca8", "#303174"),
    name = "Percentage left",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom"
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(face = "bold"),
    legend.justification = "center"
  )

combined_plot <- (p1_new + p2_new + p3_new) +
  plot_layout(ncol = 3, width = c(1, 1, 1)) +
  plot_annotation(
    title = "Newborns Left in Hospital for Reasons OTHER than Their Health Condition in Poland by Voivodeships",
    caption = "Fundacja Gajusz \n#BI_NGO",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.caption = element_text(hjust = 1, face = "italic", size = 12,  color = "#505050")  
    )
  ) +
  theme(
    legend.position = "bottom",
    plot.margin = margin(20, 20, 20, 20),
    plot.spacing = unit(1, "cm")
  )

combined_plot

ggsave(
  "~/Documents/uni/sem_3/twd/2024Z-DataVisualizationTechniques/homeworks/hw4/LahunovichYahor/images/poland_map4.png", 
  plot = combined_plot, 
  width = 15, 
  height = 8, 
  units = "in",
  dpi = 150
)


#Linear regression
percentage_birth_hospitals <- percentage_birth_hospitals %>%
  mutate(wojewodstwo = str_to_title(wojewodstwo))

percentage_birth_hospitals <- percentage_birth_hospitals %>%
  mutate(
    wojewodstwo = factor(
      wojewodstwo,
      levels = c(
        sort(unique(wojewodstwo[wojewodstwo != "Polska"])),
        "Polska"
      )
    )
  )

trend_analysis <- percentage_birth_hospitals %>%
  group_by(wojewodstwo) %>%
  summarize(
    trend = lm(percentage_left ~ year, data = cur_data()) %>%
      broom::tidy() %>%
      filter(term == "year") %>%
      pull(estimate),
    .groups = "drop"
  )%>%
  mutate(
    trend_category = case_when(
      trend > 0 ~ "Worsening",
      trend < 0 ~ "Improving",
      TRUE ~ "No change"
    )
  )

# Trend 1
z1 <- ggplot(percentage_birth_hospitals, aes(x = year, y = percentage_left)) +
  geom_line(aes(), size = 1, color = "#315ca8") +
  facet_wrap(~ wojewodstwo, scales = "free_y") +  
  labs(
    title = "Trends of left newborns in hospital \nfor reasons unrelated to their health \n condition in Poland by voivodeship (2007-2023)",
    x = "Year",
    y = "Percentage of newborns left",
    caption = "Fundacja Gajusz \n#BI_NGO"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", family = "Raleway"),
    axis.title.x = element_text(size = 12, face = "bold", family = "Raleway"),
    axis.title.y = element_text(size = 12, face = "bold", family = "Raleway"),
    axis.text.x = element_text(size = 11, family = "Raleway"),
    axis.text.y = element_text(size = 11, family = "Raleway"),
    plot.margin = margin(20, 20, 20, 20),
    legend.position  = "None",
    plot.caption = element_text(face = "italic", family = "Raleway", color = "#505050")
  )
ggsave(
  "~/Documents/uni/sem_3/twd/2024Z-DataVisualizationTechniques/homeworks/hw4/LahunovichYahor/images/trend1.png", 
  plot = z1, 
  width = 15, 
  height = 8, 
  units = "in",
  dpi = 150
)


# Trend 2
z2 <- ggplot(trend_analysis, aes(x = wojewodstwo, y = trend, fill = trend_category)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("Worsening" = "#E62248", "Improving" = "#32CD32", "No change" = "#808080")) +
  labs(
    title = "Trend of Newborns Left in Hospitals by Region in Poland (2007-2023)",
    x = "Region",
    y = "Trend in percentage of newborns left",
    caption = "Fundacja Gajusz \n#BI_NGO"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", family = "Raleway"),
    axis.title.x = element_text(size = 12, face = "bold", family = "Raleway"),
    axis.title.y = element_text(size = 12, face = "bold", family = "Raleway"),
    axis.text.y = element_text(size = 11, family = "Raleway"),
    plot.margin = margin(20, 20, 20, 20),
    axis.text.x = element_text(angle = 30, size = 11, family = "Raleway", vjust = 0.5, hjust=1),
    plot.caption = element_text(face = "italic", family = "Raleway", color = "#505050")
  )
ggsave(
  "~/Documents/uni/sem_3/twd/2024Z-DataVisualizationTechniques/homeworks/hw4/LahunovichYahor/images/trend2.png", 
  plot = z2, 
  width = 15, 
  height = 8, 
  units = "in",
  dpi = 150
)

