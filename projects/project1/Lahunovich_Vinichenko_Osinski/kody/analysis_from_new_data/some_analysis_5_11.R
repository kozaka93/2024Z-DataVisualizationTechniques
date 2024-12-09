library(dplyr)
library(ggplot2)
library(ggtext)
library(lubridate)
library(tools)
library(png)
library(grid)
library(showtext)
library(cowplot)

df <- read.csv("~/Documents/uni/sem_3/twd/football-injuries/analysis_from_new_data/Final-player.csv")
injuries <- read.csv("~/Documents/uni/sem_3/twd/football-injuries/analysis_from_new_data/Final-player-injuies.csv")

#delete injuries==rest data because of lack the information about this
injuries[injuries$type == "Rest ",]$type = NA

injuries[injuries$type == "Ill",]$type = "Illness"
# let's see which injuries are the worst(based on recovery days)
injuries <- injuries %>%
  filter(!is.na(type), tolower(type) != "unknown injury")
injuries$type <- tolower(injuries$type) 
injuries$type <- dplyr::case_when(
  injuries$type == "knee injury" ~ "Knee Issue",
  injuries$type == "knee problems" ~ "Knee Issue",
  injuries$type == "ill-influenza" ~ "Illness",
  injuries$type == "hamstring-thigh" ~ "Hamstring/Thigh Issue",
  injuries$type == "calf injury" ~ "Calf Issue",
  injuries$type == "calf problems" ~ "Calf Issue",
  TRUE ~ injuries$type 
)

injuries$type <- toTitleCase(tolower(injuries$type))
inj_rec <- injuries %>% 
  filter(!is.na(days)) %>%
  mutate(days = as.numeric(days)) %>% 
  group_by(type) %>% 
  summarise(
    count = n(),
    mean_day = mean(days, na.rm = TRUE)
  ) %>% 
  arrange(desc(count)) %>% 
  head(15) 


font_add_google("Roboto", "roboto")
showtext_auto()

background_image <- png::readPNG("~/Documents/uni/sem_3/twd/football-injuries/images/texture.png")
background_grob <- rasterGrob(
  background_image, 
  width = unit(1, "npc"), 
  height = unit(1, "npc")
)

plot <- ggplot(inj_rec, aes(x = reorder(type, mean_day), y = mean_day, fill = count)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_gradient(low = "#3F72AF", high = "#112D4E") +
  geom_text(aes(label = paste0(round(mean_day, 1))), 
            hjust = -0.2, color = "#112D4E", size = 10, fontface = "bold") + 
  labs(
    title = "Average Recovery Time by Injury Type",
    #subtitle = "Mean days required for players to recover from top 15 types of injury",
    x = "Injury Type",
    y = "Average Days to Recover",
    fill = "Frequency"
  ) +
  coord_flip() + 
  expand_limits(y = max(inj_rec$mean_day) * 1.1) +
  theme_minimal(base_size = 20, base_family = "roboto") +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = NA, color = NA), 
    plot.background = element_rect(fill = NA, color = NA),   
    plot.title = element_blank(),
    plot.subtitle = element_text(size = 25, color = "#3F72AF"),
    axis.title.x = element_text(size = 25, face = "bold", color = "#3F72AF"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 25, color = "#3F72AF"), 
    axis.text.y = element_text(size = 25, face = "bold", color = "#3F72AF"),
    legend.title = element_text(size = 25, face = "bold", color = "#3F72AF"),
    legend.text = element_text(size = 25, color = "#3F72AF")
  )

plot

ggsave(
  "updated_plot_high_res.png", 
  bg = "transparent",
  plot = plot, 
  width = 15, 
  height = 8, 
  units = "in",
  dpi = 150
)

#bmi
injuries <- injuries %>%
  mutate(
    height = as.numeric(paste0(substr(height, 1, 1), "", substr(height, 3, 4)))/100, 
    weight = as.numeric(weight),                    
    bmi = weight / (height^2)                                      
  ) %>% 
  mutate(
    bmi = round(bmi)
  )

grouped_injuries <- injuries %>% 
  group_by(bmi) %>% 
  summarise(
    count = n()
  )





#Football players with the most injuries

players <- injuries %>% 
  group_by(id, name) %>% 
  summarise(count = n()) %>% 
  select(id, name, count)

# clubs
# Top 5 premier league clubs
clubs <- injuries %>% 
  group_by(club) %>% 
  summarise(
    count = n()
  )
chelsea <- injuries[injuries$club == "Chelsea FC", ] %>% 
  group_by(id, name) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(5)
liverpool <- injuries[injuries$club == "Liverpool FC", ] %>% 
  group_by(id, name) %>% 
  summarise(count = n())%>% 
  arrange(desc(count)) %>% 
  head(5)
arsenal <- injuries[injuries$club == "Arsenal FC", ] %>% 
  group_by(id, name) %>% 
  summarise(count = n())%>% 
  arrange(desc(count)) %>% 
  head(5)
mu <- injuries[injuries$club == "Manchester United", ] %>% 
  group_by(id, name) %>% 
  summarise(count = n())%>% 
  arrange(desc(count)) %>% 
  head(5)
mc <- injuries[injuries$club == "Manchester City", ] %>% 
  group_by(id, name) %>% 
  summarise(count = n())%>% 
  arrange(desc(count)) %>% 
  head(5)

top_premier <- bind_rows(chelsea, liverpool, mu, mc, arsenal)



# injuries by age

injury_types <- c("Illness", "Cruciate Ligament Rupture", "Knee Injury", "Ankle Injury", "Shoulder Injury")

total_players <- injuries %>% 
  mutate(
    birth_date = ymd(birth)
  ) %>% 
  distinct(id, birth_date, season) %>% 
  mutate(
    season_end_year = paste0("20", substr(season, 4, 5)),
    season_end_date = ymd(paste0(season_end_year, "-06-30")),
    age = as.integer(interval(birth_date, season_end_date) / years(1))
  ) %>% 
  group_by(age) %>% 
  summarise(
    total_players = n(),
    .groups = 'drop'
  )

injury_rates <- lapply(injury_types, function(injury_type) {
  injuries_filtered <- injuries %>% 
    filter(type == injury_type) %>% 
    mutate(
      birth_date = ymd(birth),
      season_end_year = paste0("20", substr(season, 4, 5)),
      injury_date = tryCatch(
        ymd(paste0(season_end_year, "-06-30")),
        error = function(e) NA  
      ),
      age = ifelse(!is.na(injury_date), as.integer(interval(birth_date, injury_date) / years(1)), NA)
    )
  
  injury_age <- injuries_filtered %>% 
    group_by(age) %>% 
    summarise(
      count = n(),
      .groups = 'drop'
    )
  
  injury_rate <- injury_age %>% 
    left_join(total_players, by = "age") %>% 
    mutate(
      rate = count / total_players
    ) %>% 
    filter(!is.na(rate) & total_players > 0) %>%
    mutate(injury_type = injury_type)
  
  return(injury_rate)
}) %>% 
  bind_rows()

injury_rates_filtered <- injury_rates %>%
  filter(age >= 18 & age <= 38)

plot2 <- ggplot(injury_rates_filtered, aes(x = age, y = rate, color = injury_type)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    x = "Age (Years)",
    y = "Proportion of Injuries",
    color = "Injury Type"
  ) +
  theme_minimal(base_size = 20, base_family = "roboto") +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = NA, color = NA),
    plot.title = element_text(
      size = 20, face = "bold", color = "#3F72AF", 
      hjust = 0.5  
    ),
    plot.subtitle = element_text(
      size = 20, color = "#3F72AF", face = "bold",
      hjust = 0.5 
    ),
    axis.title.x = element_text(size = 25, face = "bold", color = "#3F72AF"),
    axis.title.y = element_text(size = 25, face = "bold", color = "#3F72AF"),
    axis.text.x = element_text(size = 25, color = "#3F72AF"),
    axis.text.y = element_text(size = 25, color = "#3F72AF"),
    legend.title = element_text(size = 25, face = "bold", color = "#3F72AF"),
    legend.text = element_text(size = 25, color = "#3F72AF"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("#3F72AF", "#112D4E", "#006400", "#8B0000", "#0000FF"))

ggsave(
  "updated_plot_high_res2.png", 
  bg = "transparent",
  plot = plot2, 
  width = 12, 
  height = 8, 
  units = "in",
  dpi = 150
)

# Popular players
# Hazard, Neymar, Ronaldo, Messi, Lewandowski

hazard <- injuries[injuries$name == "Eden Hazard",]
ronaldo <- injuries[injuries$name == "Cristiano Ronaldo",]
neymar <- injuries[injuries$name == "Neymar",]
messi <- injuries[injuries$name == "Lionel Messi",]
lewandowski <- injuries[injuries$name == "Robert Lewandowski",]

combined_injuries <- bind_rows(hazard, ronaldo, neymar, messi, lewandowski)

injuries_by_season <- combined_injuries %>%
  group_by(season, name) %>%
  summarise(injury_count = n(), .groups = 'drop') %>%
  mutate(season_start_year = as.integer(substr(season, 1, 2)) + 2000) %>%
  arrange(season_start_year) %>%
  mutate(season = factor(season, levels = unique(season)))

ggplot(injuries_by_season, aes(x = season, y = injury_count, color = name, group = name)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Number of Injuries per Season",
    x = "Season",
    y = "Number of Injuries",
    color = "Player"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#b6c2b6", color = NA),
    panel.background = element_rect(fill = "#b6c2b6", color = NA),
    plot.title = element_text(size = 24, face = "bold", color = "#004d00"),
    plot.subtitle = element_text(size = 16, color = "#005700"),
    axis.title.x = element_text(size = 14, face = "bold", color = "#004d00"),
    axis.title.y = element_text(size = 14, face = "bold", color = "#004d00"),
    axis.text.x = element_text(size = 12, color = "#004d00"),
    axis.text.y = element_text(
      size = 12,
      face = "bold",
      color = "#004d00",
      margin = margin(r = 5)
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 14, face = "bold", color = "#004d00"),
    legend.text = element_text(size = 12, color = "#004d00")
  ) +
  scale_color_brewer(palette = "Set1")
