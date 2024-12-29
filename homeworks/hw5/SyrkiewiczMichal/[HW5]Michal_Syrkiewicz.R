library(ggplot2)

set.seed(45)

layers <- 12

data_tree <- lapply(1:layers, function(layer) {
  x_base <- seq(-0.5 * layer, 0.5 * layer, length.out = 100)
  y_base <- layers - layer
  data.frame(
    x = c(x_base, rev(x_base)),
    y = c(rep(y_base, length(x_base)), rep(y_base - 1, length(x_base))),
    layer = layer
  )
})
data_tree <- do.call(rbind, data_tree)

bombs <- lapply(1:layers, function(layer) {
  x_positions <- seq(-0.45 * layer, 0.45 * layer, length.out = layer+1) 
  y_position <- layers - layer - 0.5
  data.frame(
    x = x_positions,
    y = rep(y_position, length(x_positions)),
    size = sample(10, length(x_positions), replace = TRUE),  
    color = sample(c("red", "gold", "blue", "green"), length(x_positions), replace = TRUE)
  )
})
bombs <- do.call(rbind, bombs)

chain <- lapply(1:layers, function(layer) {
  x_positions <- seq(-0.5 * layer, 0.5 * layer, length.out = layer*4)
  y_positions <- rep(layers - layer - 0.3, length(x_positions))
  data.frame(
    x = x_positions,
    y = y_positions
  )
})
chain <- do.call(rbind, chain)

trunk <- data.frame(
  x = c(-1, 1, 1, -1),
  y = c(-4, -4, -1, -1)
)

# Gwiazda z większymi rozmiarami i konturem
star <- data.frame(
  x = 0,
  y = layers-0.6,
  size = 15,  
  color = "gold"
)

snow <- data.frame(
  x = runif(500, -layers, layers),
  y = runif(500, -4, layers),
  size = runif(500, 0.5, 2)
)

# Wykres
p <- ggplot() +
  geom_point(data = snow, aes(x = x, y = y, size = size), color = "white", alpha = 0.7) +
  geom_polygon(data = data_tree, aes(x = x, y = y, group = layer), fill = "darkgreen", alpha = 0.8) +
  geom_point(data = bombs, aes(x = x, y = y, size = size, color = color, fill = color), shape = 21) +
  geom_point(data = chain, aes(x = x, y = y), color = "gold", size = 0.5) +
  geom_point(data = star, aes(x = x, y = y, size = size, color = color, fill = color), shape = 8, stroke = 1.5) +
  geom_polygon(data = trunk, aes(x = x, y = y), fill = "saddlebrown") +
  coord_fixed() +
  theme_void() +
  scale_color_manual(values = c("red" = "#FF0000", "gold" = "#FFC300", "blue" = "#0000FF", "green" = "#008000")) +
  theme(legend.position = "none", plot.background = element_rect(fill = "lightblue", color = NA)) 

# Zapisanie wykresu
ggsave("Drzewko.png", plot = p, width = 8, height = 7, dpi = 300)

print(p)
