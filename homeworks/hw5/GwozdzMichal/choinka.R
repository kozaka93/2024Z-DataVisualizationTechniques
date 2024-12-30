library(ggplot2)
library(ggridges)
library(gganimate)
library(dplyr)
library(tidyr)

# Generowanie danych dla choinki
set.seed(123)
n_layers <- 7

generate_layer_data <- function(layer, max_layers) {
  width <- 1.5 * (max_layers - layer + 1) / max_layers
  n_points <- 100
  data.frame(
    layer = layer,
    x = rnorm(n_points, 0, width)
  )
}

# Tworzenie danych dla wszystkich warstw
tree_data <- do.call(rbind, lapply(1:n_layers, generate_layer_data, max_layers = n_layers))

# Paleta kolorow do bombek
christmas_colors <- c(
  "#FF0000", 
  "#FFFF00", 
  "#FFFFFF",
  "#8B0000", 
  "#FFD700", 
  "#FF4500", 
  "#ADFF2F", 
  "#0000FF", 
  "#FF69B4", 
  "#B22222"
)


# Tworzenie danych dla bombek
baubles <- data.frame(
  x = c(0.5, 1, 1.3, -0.7, -0.9, 0.2, -0.2, 0.4, -0.1, 0.3, -0.3, -0.4, 1, 0.25, 0.4, -1, 0.3),
  y = c(1.5, 2.3, 4.3, 2.5, 5.2, 9, 8, 7, 6.5, 5.7, 3.5, 5, 2.3, 3, 4, 1.3, 4.8),  
  color = sample(christmas_colors, 17, replace = TRUE)
)

# Generowanie danych dla płatków śniegu
n_snowflakes <- 150
snowflakes <- data.frame(
  x = runif(n_snowflakes, -3, 3),
  y = runif(n_snowflakes, 1, 17),
  frame = 1
)

# Tworzenie klatek animacji dla śniegu
n_frames <- 20
snow_animation <- do.call(rbind, lapply(1:n_frames, function(i) {
  snowflakes$frame <- i
  snowflakes$y <- snowflakes$y - 0.2 * i
  snowflakes$y[snowflakes$y < 0] <- 13
  snowflakes
}))

p <- ggplot() +
  # Warstwy choinki
  geom_density_ridges(
    data = tree_data,
    aes(x = x, y = layer, group = layer),
    fill = "forestgreen",
    color = "darkgreen",
    scale = 4.5,           
    bandwidth = 0.3,
    rel_min_height = 0.005,
  ) +
  # Bombki na choince
  geom_point(
    data = baubles,
    aes(x = x, y = y, color = color),
    size = 20,
    shape = 21,
    fill = baubles$color
  ) +
  # Gwiazdka na czubku
  annotate(
    "text",
    x = -0.03,
    y = 10.7,
    label = "*",
    size = 100,
    color = "#FFD700"
  ) +
  # Płatki śniegu
  geom_text(
    data = snow_animation,
    aes(x = x, y = y, frame = frame),
    label = "*",
    color = "white",
    size = 4
  ) +
  # Pień choinki
  annotate(
    "rect",
    xmin = -0.3,
    xmax = 0.3,
    ymin = 0,
    ymax = 1,
    fill = "brown"
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "navy"),
    plot.background = element_rect(fill = "navy"),
    legend.position = "none"
  ) +
  coord_cartesian(xlim = c(-3, 3), ylim = c(0, 12)) +
  transition_manual(frame)

anim_save(
  "choinka.gif",
  animation = animate(
    p,
    nframes = n_frames,
    fps = 10,
    width = 800,
    height = 1200,
    renderer = gifski_renderer()
  )
)

