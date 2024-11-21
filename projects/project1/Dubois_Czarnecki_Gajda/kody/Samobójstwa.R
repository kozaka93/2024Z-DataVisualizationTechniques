library(ggplot2)
library(dplyr)
library(maps)
library(mapdata)
library(readr)
library(countrycode)
library(RColorBrewer)

df_samobójstwa <- read.csv('16BBF41_ALL_LATEST.csv')
df_depresja <-read.csv('wgm_full_wave2_public_file_final (1)_csv.csv')
world_map <- map_data("world")

df_samobójstwa1 <- df_samobójstwa %>% 
  filter(DIM_GEO_CODE_TYPE == 'COUNTRY') %>% 
  select(DIM_TIME, GEO_NAME_SHORT, DIM_SEX, DIM_AGE, RATE_PER_100000_N, RATE_PER_100000_NL, RATE_PER_100000_NU) %>% 
  arrange(DIM_TIME,GEO_NAME_SHORT, DIM_SEX)

df_samobójstwa_w_sumie <- df_samobójstwa1 %>% 
  filter(DIM_TIME %in% c(2019)) %>% 
  group_by(GEO_NAME_SHORT, DIM_TIME) %>% 
  summarise(suicides_N = sum(RATE_PER_100000_N, na.rm = TRUE),
            suicides_NL = sum(RATE_PER_100000_NL, na.rm = TRUE),
            suicides_NU = sum(RATE_PER_100000_NU, na.rm = TRUE)) %>% 
  mutate(Total = suicides_N + suicides_NL + suicides_NU) %>% 
  arrange(GEO_NAME_SHORT) %>% 
  ungroup()
  
df_samobójstwa_w_sumie$GEO_NAME_SHORT <- countrycode(df_samobójstwa_w_sumie$GEO_NAME_SHORT, 
                                    origin = "country.name",
                                    destination = "country.name")

world_map$region <- countrycode(world_map$region, 
                                origin = "country.name",
                                destination = "country.name")
world_map_suicides <- left_join(world_map, df_samobójstwa_w_sumie, by = c("region" = "GEO_NAME_SHORT"))
world_map_suicides <- world_map_suicides %>% 
  filter(long <= 180)


ggplot(world_map_suicides, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = suicides_N), color = "black") +
  scale_fill_stepsn(
    colors = c('#c6afae', '#a68395', '#8c557c', '#692a5f', '#52154E'),
    breaks = seq(0, 3000, by = 1000),
    labels = scales::comma_format(accuracy = 1),
    limits = c(0, 3000),
    na.value = "grey30",
    name = "Samobójstwa \nna 100k ludzi"
  ) +
  labs(title = "Samobójstwa na świecie") +
  theme_void() +
  coord_map("mollweide") +
  theme(
    plot.background = element_rect(fill = NA, color = NA), 
    panel.background = element_rect(fill = NA, color = NA),     
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(
      hjust = 0.5,
      size = 20,
      color = "black",
      face = "bold"
    ),
    legend.title = element_text(
      size = 10,
      color = "black",
      face = "bold"
    )
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 2,
      barheight = 20,
      title.position = "top",
      title.hjust = 0.5,
      label = TRUE, # Wyświetl wszystkie etykiety
      frame.colour = "black" # Obramowanie legendy
    )
  )



df_depresja1 <- df_depresja %>% 
  filter(YEAR_WAVE == 2020) %>% 
  select(COUNTRYNEW, YEAR_WAVE, MH7A) %>% 
  filter(MH7A == 1) %>% 
  group_by(COUNTRYNEW) %>% 
  summarise(Total1 = n())
df_depresja2 <- df_depresja %>% 
  filter(YEAR_WAVE == 2020) %>% 
  select(COUNTRYNEW, YEAR_WAVE, MH7A) %>% 
  filter(MH7A == 2) %>% 
  group_by(COUNTRYNEW) %>% 
  summarise(Total2 = n())
df_depresja_final <- df_depresja1 %>% 
  inner_join(df_depresja2, by = 'COUNTRYNEW') %>% 
  mutate(TOTAL = Total1 + Total2,
         DEPRESSED_PER_100000 = round(Total1/TOTAL*100000)) %>% 
  select(COUNTRYNEW, DEPRESSED_PER_100000)

df_depresja_final$COUNTRYNEW <- countrycode(df_depresja_final$COUNTRYNEW, 
                                                     origin = "country.name",
                                                     destination = "country.name")

world_map_depressed <- left_join(world_map, df_depresja_final, by = c("region" = "COUNTRYNEW"))
world_map_depressed <- world_map_depressed %>% 
  filter(long <= 180)

ggplot(world_map_depressed, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = DEPRESSED_PER_100000), color = "black") +
  scale_fill_stepsn(
    colors = c('#c6afae', '#a68395', '#8c557c', '#692a5f', '#52154E'),
    breaks = round(seq(
      min(world_map_depressed$DEPRESSED_PER_100000, na.rm = TRUE),
      max(world_map_depressed$DEPRESSED_PER_100000, na.rm = TRUE),
      length.out = 5
    ), -3),  # Zaokrąglenie do tysięcy
    labels = scales::comma_format(accuracy = 1), # Usuń miejsca po przecinku
    na.value = "grey30", # Kolor dla brakujących danych
    name = "Przypadki depresji \nna 100k ludzi",
    guide = guide_colorbar(
      barwidth = 2,
      barheight = 20
    )
  ) +
  labs(title = "Depresja na świecie") +
  theme_void() +
  coord_map("mollweide") +
  theme(
    plot.background = element_rect(fill = NA, color = NA), 
    panel.background = element_rect(fill = NA, color = NA),     
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(
      hjust = 0.5,
      size = 20,
      color = "black",
      face = "bold"
    ),
    legend.title = element_text(
      size = 10,
      color = "black",
      face = "bold"
    )
  )



