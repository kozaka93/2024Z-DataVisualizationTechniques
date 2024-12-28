library(ggplot2)
library(dplyr)
library(gganimate)
library(gifski)

pien <- data.frame(
  x = c(-0.5, 0.5, 0.5, -0.5),
  y = c(1, 1, 2, 2) 
)

zielone_pietra <- data.frame(
  x = c(-3, 3, 0, -2.5, 2.5, 0, -2, 2, 0),
  y = c(2, 2, 5, 4, 4, 7, 6, 6, 8),
  level = rep(1:3, each = 3),
  odcien = c("darkgreen", "forestgreen", "springgreen3")
)

star_x <- c(0, 0.2, 0.8, 0.3, 0.5, 0, -0.5, -0.3, -0.8, -0.2)
star_y <- c(9, 8.6, 8.6, 8.3, 7.7, 8, 7.7, 8.3, 8.6, 8.6)
gwiazda <- data.frame(x = star_x, y = star_y)

lancuch <- data.frame(
  x = seq(-2.8, 2.8, length.out = 50),
  y = 2 + 0.2 * sin(seq(-pi, pi, length.out = 50))
)
lancuch <- rbind(lancuch, data.frame(
  x = seq(-2.3, 2.3, length.out = 50),
  y = 4 + 0.2 * sin(seq(-pi, pi, length.out = 50))
))
lancuch <- rbind(lancuch, data.frame(
  x = seq(-1.8, 1.8, length.out = 50),
  y = 6 + 0.2 * sin(seq(-pi, pi, length.out = 50))
))

bombki_na_wierzcholkach <- zielone_pietra %>%
  filter(row_number() %% 3 != 0) %>% 
  mutate(kolor1 = sample(c("red", "blue", "purple"), n(), replace = TRUE),
         kolor2 = sample(c("green", "pink", "cyan"), n(), replace = TRUE))

bombki_na_wierzcholkach_ <- bombki_na_wierzcholkach %>%
  pivot_longer(cols = starts_with("kolor"), names_to = "kolor_type", values_to = "kolor") %>%
  mutate(kolor_type = factor(kolor_type, levels = c("kolor1", "kolor2")))


animacja <- ggplot(bombki_na_wierzcholkach_) +
  
  # Pień
  geom_polygon(data = pien, aes(x = x, y = y), fill = "brown") +
  
  # Piętra choinki
  geom_polygon(data = zielone_pietra, aes(x = x, y = y, fill = factor(level), group = level)) +
  scale_fill_manual(values = c("darkgreen", "forestgreen", "springgreen3")) +
  
  # Gwiazda
  geom_polygon(data = gwiazda, aes(x = x, y = y), fill = "yellow", color = "gold") +
  
  # Łańcuchy
  geom_path(data = lancuch, aes(x = x, y = y), color = "gold", size = 2) +
  
  # Bombki 
  geom_point(aes(x = x, y = y, color = kolor), size = 10, show.legend = FALSE) +
  scale_color_manual(values = c(
    "red" = "red", "blue" = "blue", "purple" = "purple",
    "green" = "green", "pink" = "pink", "cyan" = "cyan"
  )) +
  
  # Animacja
  transition_states(kolor_type, state_length = 0.1, wrap = TRUE) +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "lightblue",color = "lightblue"),
    legend.position = "none"
  )

animacja
#anim_save("choinka.gif", animation = animacja, nframes = 200, fps = 30)
