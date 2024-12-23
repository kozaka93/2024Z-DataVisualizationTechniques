library(ggplot2)

#choinka i bombki
generate_tree_with_baubles <- function(layers = 5, width = 0.5, height = 0.2, n_baubles = 50) {
  #tworzymy drzewko
  tree <- data.frame(x = numeric(), y = numeric(), layer = integer())
  #tworzymy wspolrzedne pieter
  for (i in seq_len(layers)) {
    base_y <- (i - 1) * height
    tree <- rbind(tree, data.frame(
      x = c(-width / 2 * (layers - i + 1), 0, width / 2 * (layers - i + 1)),
      y = c(base_y, base_y + height, base_y),
      layer = i
    ))
  }
  
  #bombki (dla niektórych bańki)
  baubles <- data.frame(x = numeric(), y = numeric(), color = character())
  
  for (layer in unique(tree$layer)) {

    layer_data <- tree[tree$layer == layer, ]
    
    x1 <- layer_data$x[1]
    y1 <- layer_data$y[1]
    x2 <- layer_data$x[2]
    y2 <- layer_data$y[2]
    x3 <- layer_data$x[3]
    y3 <- layer_data$y[3]
    
    # bombki dla danego pietra
    for (i in 1:(n_baubles / layers)) {
      repeat {
        
        u <- runif(1)
        v <- runif(1)
        if (u + v <= 1) {

          px <- (1 - u - v) * x1 + u * x2 + v * x3
          py <- (1 - u - v) * y1 + u * y2 + v * y3
          
          baubles <- rbind(baubles, data.frame(
            x = px,
            y = py,
            color = sample(c("red", "gold", "blue"), 1)
          ))
          break
        }
      }
    }
  }
  
  list(tree = tree, baubles = baubles)
}

# gwiazdka
generate_star <- function(center_x = 0, center_y = 1.05, radius1 = 0.05, radius2 = 0.025, arms = 9, scale = 1.5) {
  angles <- seq(0, 2 * pi, length.out = arms * 2 + 1)
  x <- numeric()
  y <- numeric()
  for (i in seq_along(angles[-length(angles)])) {
    r <- ifelse(i %% 2 == 1, radius1, radius2) * scale
    x <- c(x, center_x + cos(angles[i]) * r)
    y <- c(y, center_y + sin(angles[i]) * r)
  }
  data.frame(x = x, y = y)
}


tree_data <- generate_tree_with_baubles()
star <- generate_star()

# pieniek
trunk <- data.frame(
  x = c(-0.05, 0.05, 0.05, -0.05),
  y = c(-0.1, -0.1, 0, 0)
)

ggplot() +

  geom_polygon(data = tree_data$tree, aes(x = x, y = y, group = layer), fill = "darkgreen", color = "green") +

  geom_polygon(data = trunk, aes(x = x, y = y), fill = "saddlebrown") +

  geom_polygon(data = star, aes(x = x, y = y), fill = "gold", color = "goldenrod") +

  geom_point(data = tree_data$baubles, aes(x = x, y = y, color = color), size = 3) +
  scale_color_manual(values = c("red" = "red", "gold" = "gold", "blue" = "blue")) +
  #tło
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    legend.position = "none"  
  ) +
  coord_equal()

ggsave("christmas_tree.png", width = 8, height = 10, dpi = 300)


