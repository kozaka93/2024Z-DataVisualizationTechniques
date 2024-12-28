library(ggplot2)
library(ggimage)
library(gganimate)
library(dplyr)
library(tidyr)
library(grid)
library(magick)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Dane punkty choinki
punkty <- matrix(data = c(0, 0, 
                          1, 0,
                          1, 1,
                          5.5, 1,
                          2, 2.25,
                          4.5, 2.25,
                          1.5, 3.5,
                          3.5, 3.5,
                          1, 4.75, 
                          2.5, 4.75,
                          0, 6,
                          -2.5, 4.75,
                          -1, 4.75,
                          -3.5, 3.5,
                          -1.5, 3.5,
                          -4.5, 2.25,
                          -2, 2.25,
                          -5.5, 1,
                          -1, 1,
                          -1, 0,
                          0, 0), ncol=2, byrow = TRUE)
punkty <- as.data.frame(punkty)
names(punkty) <- c("x", "y")
punkty <- punkty %>% 
  mutate(lp = c(1:nrow(punkty))) %>% 
  select(c(3, 1, 2))

# Wyliczanie kątów dla żółwia
punkty <- punkty %>%
  mutate(dx = lead(x) - x,
         dy = lead(y) - y,
         angle = ifelse(is.na(atan2(dy, dx)), 0, - atan2(dy, dx) * 180 / pi)) # Kąt w stopniach

# Ścieżka do obrazu żółwia (dodaj plik "turtle.png" do katalogu roboczego)
turtle_image <- "turtle.png"


p_line <- ggplot() + 
  geom_path(data = punkty %>% filter(lp <= 3), 
            mapping = aes(x = x, y = y), size = 2, color = 'brown') +
  geom_path(data = punkty %>% filter(lp >= 3 & lp <= 19), 
            mapping = aes(x = x, y = y), size = 2, color = 'green') +
  geom_path(data = punkty %>% filter(lp >= 19), 
            mapping = aes(x = x, y = y), size = 2, color = 'brown') +
  geom_image(data = punkty,
             mapping = aes(x = x, y = y, image = turtle_image, angle = angle),
             size = 0.1) +
  xlim(-10, 10) +
  theme_minimal() +
  theme(axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  transition_reveal(lp)


animate(p_line, nframes = 200, fps = 15, width = 600, height = 600, renderer = gifski_renderer())

