library(ggplot2)
library(gganimate)
library(dplyr)
library(tidyr)

generate_tree_data <- function() {
  layers <- data.frame(
    x = c(-0.6, 0.6, 0, NA, -0.5, 0.5, 0, NA, -0.4, 0.4, 0),
    y = c(0, 0, 0.4, NA, 0.2, 0.2, 0.5, NA, 0.4, 0.4, 0.6),
    type = "tree",
    color = "darkgreen",
    size = NA
  )

  trunk <- data.frame(
    x = c(-0.1, 0.1, 0.1, -0.1),
    y = c(-0.2, -0.2, 0, 0),
    type = "trunk",
    color = "#4B2E07", 
    size = NA
  )

  ornaments <- expand.grid(
    x = seq(-0.5, 0.5, length.out = 10),
    y = seq(0.1, 0.6, length.out = 5)
  ) %>%
    mutate(
      type = "ornament",
      color = sample(c("red", "blue", "gold", "green"), nrow(.), replace = TRUE),
      size = runif(nrow(.), 3, 6)
    ) %>%
    filter(y <= 0.6 - abs(x) & y > abs(x) * 0.5)
  
  new_ornaments <- data.frame(
    x = c(-0.4, -0.28, 0.28, 0.4),  
    y = c(0.1, 0.1, 0.1, 0.1),   
    type = "ornament",
    color = sample(c("red", "blue", "gold", "green"), 2, replace = TRUE),
    size = runif(2, 3, 6)  
  )

  ornaments <- rbind(ornaments, new_ornaments)
  
  presents <- data.frame(
    x = c(-0.6, -0.35, 0.15, 0.4),  
    xend = c(-0.4, -0.15, 0.35, 0.6),  
    y = c(-0.2, -0.2, -0.2, -0.2), 
    yend = c(-0.05, -0.05, -0.05, -0.05),  
    type = "present",
    color = c("red", "blue", "green", "gold"),
    label = c("p. Kozak Anna", "p. Katarzyna Wo??nica", "p. Hubert Ruczy??ski", "p. Maciej Chrab??szcz")
  )
  
  return(list(tree = rbind(layers, trunk, ornaments), presents = presents))
}

generate_snow <- function(frames = 100, flakes = 200) {
  snow <- expand.grid(frame = 1:frames, id = 1:flakes) %>%
    mutate(
      x = runif(n(), -0.6, 0.6),
      y = runif(n(), -0.3, 0.7),
      size = runif(n(), 0.5, 1.5),
      alpha = sample(c(0.2, 0.5, 0.8, 1), n(), replace = TRUE)
    )
  return(snow)
}

data <- generate_tree_data()
tree_data <- data$tree
presents_data <- data$presents
snow_data <- generate_snow()

plot <- ggplot() +
  geom_polygon(data = subset(tree_data, type == "tree"), aes(x = x, y = y), fill = "darkgreen", color = "darkgreen") +
  geom_polygon(data = subset(tree_data, type == "trunk"), aes(x = x, y = y), fill = "#4B2E07", color = "#4B2E07") +
  geom_point(data = subset(tree_data, type == "ornament"), aes(x = x, y = y, color = color, size = size)) +
  geom_rect(data = presents_data, aes(xmin = x, xmax = xend, ymin = y, ymax = yend, fill = color), color = "black") +
  geom_segment(data = presents_data, aes(x = x, xend = xend, y = (y + yend) / 2, yend = (y + yend) / 2), color = "white", size = 5) +
  geom_segment(data = presents_data, aes(x = (x + xend) / 2, xend = (x + xend) / 2, y = y, yend = yend), color = "white", size = 5) +
  geom_text(data = presents_data, aes(x = (x + xend) / 2, y = (y + yend) / 2, label = label), 
            color = "black", size = 3.5, fontface = "bold") +
  geom_point(data = snow_data, aes(x = x, y = y, size = size, alpha = alpha), color = "white") +  
  scale_color_manual(values = c("red" = "red", "blue" = "blue", "gold" = "gold", "green" = "green")) +
  scale_fill_manual(values = c("red" = "red", "blue" = "blue", "gold" = "gold", "green" = "green")) +
  scale_size(range = c(0.5, 6)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "black", color = NA)
  ) +
  transition_time(frame) +  
  ease_aes('linear')

gganimate::animate(plot, nframes = 100, fps = 10, width = 800, height = 800, renderer = gifski_renderer("choinka.gif"))

