library(dplyr)
library(ggplot2)
library(fmsb)
library(tidyr)
library(maps)
library(mapdata)
library(patchwork)
library(shadowtext)
library(stringr)
library(cowplot)

dane_polska_1990_2020 <- read.csv("IHME-GBD_2021_DATA-94b3f34c-1.csv")
nadwaga_w_polsce <- read.csv("NCD_RisC_Lancet_2024_BMI_age_standardised_Poland.csv")
nadciśnienie_w_polsce <- read.csv("Poland.csv")

dane_polska_1990_2020_uporzadkowane <- dane_polska_1990_2020 %>% 
  select(year, val, cause) %>% 
  mutate(val = format(val, scientific = FALSE))
  
nadwaga_w_polsce$ilosc <- nadwaga_w_polsce$Prevalence.of.BMI..30.kg.m...obesity.
nadciśnienie_w_polsce$ilosc <- nadciśnienie_w_polsce$Age.standardised.prevalence.of.raised.blood.pressure

nadwaga_w_polsce_uporzadkowane <- nadwaga_w_polsce %>% 
  select(-Sex) %>% 
  group_by(Year) %>% 
  summarise(val = mean(ilosc)) %>% 
  mutate(cause = "Obesity", val = val * 100000) %>% 
  rename(year = Year)

nadciśnienie_w_polsce_uporzadkowane <- nadciśnienie_w_polsce %>% 
  select(-Sex) %>% 
  group_by(Year) %>% 
  summarise(val = mean(ilosc)) %>% 
  mutate(cause = "Hypertension", val = val * 100000) %>% 
  rename(year = Year)

# df2 = data.frame(year = 2021, val=0.352, cause = "Hypertension")
# nadciśnienie_w_polsce_uporzadkowane <- nadciśnienie_w_polsce_uporzadkowane %>% 
#   rows_insert(df2)

dane <- rbind(dane_polska_1990_2020_uporzadkowane,
              nadwaga_w_polsce_uporzadkowane,
              nadciśnienie_w_polsce_uporzadkowane)


dane$val <- as.numeric(dane$val)

dane <- dane %>% 
  filter(year <= 2015 & year >= 1990) %>% 
  mutate(top3 = cause %in% c("Diabetes mellitus type 2", "Obesity", "Hypertension")) %>% 
  mutate(cause_pl = case_when(cause == "Diabetes mellitus type 2" ~ "Cukrzyca typu 2",
                              cause == "Anxiety disorders" ~ "Zaburzenia lękowe",
                              cause == "Tracheal, bronchus, and lung cancer" ~ "Rak tchawicy, oskrzeli i płuc",
                              cause == "Depressive disorders" ~ "Depresja",
                              cause == "Obesity" ~ "Otyłość",
                              cause == "Hypertension" ~ "Nadciśnienie",
                              ))
# dane <- dane %>% 
#   filter(year == 1990 | year == 2021) %>% 
#   filter(metric_name == "Percent") %>% 
#   select(-metric_name) %>% 
#   mutate(val =as.numeric(val) *100)

p <- dane %>% 
  ggplot(aes(x = year, y = val, color = cause_pl)) + 
  geom_line(size = 2) + 
  labs(y = "ilość chorych na 100 000 osób", 
       x = "rok", 
       title = "Ilość chorych osób w Polsce na poszczególne choroby", 
       subtitle = "w latach 1990 - 2015") + 
  theme(
    legend.position = "none",
    strip.text = element_blank(),
    strip.background = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "white"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "white"),
    axis.title.x = element_blank(),
    panel.background = element_rect(fill='transparent' , color='transparent'),
    plot.background = element_rect(fill='transparent' , color='transparent'),
    legend.background = element_rect(fill='transparent' , color='transparent'),
    legend.box.background = element_rect(fill='transparent' , color='transparent'),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white")
  
  ) + 
  facet_wrap(~top3, scales = "free_y") +
  scale_color_brewer(palette = "PuBu")
p


dane_zmiana <- dane %>% 
  select(year, val, cause) %>% 
  filter(year == 1990 | year == 2015) %>% 
  pivot_wider(names_from = cause, values_from = val) %>% 
  as.data.frame() %>% 
  select(-year)
dane_zmiana

dane_zmiana <- (dane_zmiana[2,] - dane_zmiana[1,])*100/ dane_zmiana[1,]
dane_zmiana <- dane_zmiana %>% 
  pivot_longer(everything(), names_to = "cause", values_to = "val")
dane_zmiana

wrap_text <- function(text, width) {
  str_wrap(text, width = width)
}
dane_zmiana
dane_zmiana$cause <- c("Zaburzenia lękowe", "Cukrzyca typu 2", "Rak tchawicy, oskrzeli i płuc", "Depresja",  "Otyłość", "Nadciśnienie")

# Apply the wrap_text function to the category labels
dane_zmiana$wrapped_cause <- sapply(dane_zmiana$cause, wrap_text, width = 15)
dane_zmiana$wrapped_cause <- factor(dane_zmiana$wrapped_cause, 
                                    levels = dane_zmiana$wrapped_cause[order(dane_zmiana$val)])



pp <- dane_zmiana %>% 
  ggplot(aes(x = wrapped_cause, y = val, fill = cause)) + 
  geom_col() + 
  labs(y = "Zmiana w %", 
       x = NULL, 
       title = "Zmiana w ilości osób chorych",
       subtitle = "od 1990 do 2015", 
       fill = "Choroba") +
  scale_y_continuous(breaks = seq(-40,100,20),
                     minor_breaks = seq(-40,100,5),
                     limits = (c(-40,100)),
                     expand = expansion(mult = c(0, 0))) +
  
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", color = "white"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "white"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "white"),
    panel.background = element_rect(fill='transparent' , color='transparent'),
    plot.background = element_rect(fill='transparent' , color='transparent'),
    legend.background = element_rect(fill='transparent' , color='transparent'),
    legend.box.background = element_rect(fill='transparent' , color='transparent'),
    legend.text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    axis.title.y = element_text(color = "white")
  ) +
  geom_shadowtext(aes(y = ifelse(val > 0, val + 5, 5), label = wrapped_cause), 
                  color = "white",
                  bg.color = "black",
                  size = 4)  +
  scale_fill_brewer(palette = "PuBu")
pp

p1 <- plot_grid(p, pp, ncol = 1)
p1
p2 <- p1 + theme(
  panel.background = element_rect(fill='transparent' , color='transparent'),
  plot.background = element_rect(fill='transparent' , color='transparent'),
  legend.background = element_rect(fill='transparent' , color='transparent'),
  legend.box.background = element_rect(fill='transparent' , color='transparent'),
  legend.text = element_text(color = "white"),
  legend.title = element_text(color = "white"),
  axis.text = element_text(color = "white"),
  axis.title = element_text(color = "white"),
  plot.subtitle = element_text(color = "white")
)
p2

anxiety <- read.csv("anxiety.csv")
w1 <- map_data("world")
head(w1)
w1_prim <- map_data("world") %>% filter(long <= 180, region != "Antarctica")
ggplot() + 
  geom_polygon(data = w1_prim, aes(x = long, y = lat, group = group)) +
  coord_map("mollweide")

w1_prim_anxiety <- w1_prim %>% 
  left_join(anxiety, by = c("region" = "location"))

w1_prevelance <- w1_prim_anxiety %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = val)) +
  geom_polygon(color = "white") +
  theme_void() +
  geom_polygon(color = "black", fill = NA) +
  coord_map("mollweide") + 
  scale_fill_gradient2(high = "#091e4a", mid = "#1c5a95", low="#b5cddf", midpoint = 6000) +
  labs(title = "Mapa ilości osób cierpiących na zburzenia lękowe (na 100 000 osób)", 
       fill = "ilość (na 100 000 osób)") + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "white"),
    legend.position = "bottom",
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill='transparent' , color='transparent'),
    plot.background = element_rect(fill='transparent', color='transparent'),
    legend.background = element_rect(fill='transparent' , color='transparent'),
    legend.box.background = element_rect(fill='transparent' , color='transparent'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) 

w1_prevelance
p2

ggsave("iloscPrzypadkow.png",
       plot = p2, bg = "transparent",
       width = 3000, height = 2000, units = "px")

ggsave("mapaBLU.png",
       plot = w1_prevelance, bg = "transparent",
       width = 3000, height = 2000, units = "px")


# w1_prim %>% 
#   filter(!region %in% (anxiety$location)) %>% 
#   distinct(region) %>% 
#   arrange(region)

# anxiety %>% 
#   distinct(location)





# dane <- dane %>%
#   pivot_wider(names_from = cause, values_from = val) %>%
#   filter(year == 1990 | year == 2015) %>% 
#   select(-year)
# 
# dane <- as.data.frame(dane)
# dane
# row.names(dane) <- c(1990,2015)
# dane <- dane / 1000
# 
# df <- data.frame(c(50,0),c(50,0),c(50,0),c(50,0),c(50,0),c(50,0))
# 
# colnames(df) <- colnames(dane)
# row.names(df) <- c('max','min')
# df
# 
# dane <- rbind(df, dane)
# dane <- log10(dane)
# dane
# radarchart(dane)
# 
# create_beautiful_radarchart <- function(data, color = "#00AFBB",
#                                         vlabels = colnames(data), vlcex = 0.7,
#                                         caxislabels = NULL, title = NULL, ...){
#   radarchart(
#     data, axistype = 1,
#     # Customize the polygon
#     pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
#     # Customize the grid
#     cglcol = "grey", cglty = 1, cglwd = 0.8,
#     # Customize the axis
#     axislabcol = "grey",
#     # Variable labels
#     vlcex = vlcex, vlabels = vlabels,
#     caxislabels = caxislabels, title = title, ...
#   )
# }
# 
# create_beautiful_radarchart(dane,caxislabels = c(0, 10, 20, 30, 40, 50),
#                             color = c("green", "orange"))

