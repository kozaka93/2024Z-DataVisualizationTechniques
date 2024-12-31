library(ggplot2)
library(dplyr)

n <- 70

# decorations
data <- data.frame(
  y = runif(n, 0, 0.9),
  size = sample(100:300, n, replace = TRUE),
  color = sample(c("purple", "green", "blue", "red", "yellow", "orange"), n, replace = TRUE)
)

data$x <- sapply(data$y, function(y_val) {
  if (y_val < 0.4) {
    runif(1, -1 + y_val + 0.1 , 1 - y_val - 0.1)
  } else if (y_val < 0.7) {
    runif(1, -0.7 + (y_val - 0.4), 0.7 - (y_val - 0.4))
  } else {
    runif(1, -0.5 + (y_val - 0.7), 0.5 - (y_val - 0.7))
  }
})

# tree elements
base_triangle <- data.frame(x = c(-1, 0, 1), y = c(0, 0.6, 0))
middle_triangle <- data.frame(x = c(-0.7, 0, 0.7), y = c(0.4, 0.8, 0.4))
top_triangle <- data.frame(x = c(-0.5, 0, 0.5), y = c(0.7, 1, 0.7))
trunk <- data.frame(x = c(-0.1, 0.1, 0.1, -0.1), y = c(0, 0, -0.2, -0.2))
star <- data.frame(x = 0, y = 1.05)

# tree
ggplot() +
  geom_polygon(data = base_triangle, aes(x = x, y = y), fill = "darkgreen") +
  geom_polygon(data = middle_triangle, aes(x = x, y = y), fill = "darkgreen") +
  geom_polygon(data = top_triangle, aes(x = x, y = y), fill = "darkgreen") +
  geom_point(data = data, aes(x = x, y = y, size = size, color = color), alpha = 1) +
  geom_point(data = star, aes(x = x, y = y), size = 10, color = "gold", shape = 8, stroke = 3) +
  scale_color_identity() +
  geom_polygon(data = trunk, aes(x = x, y = y), fill = "brown", alpha = 1) +
  geom_curve(aes(x = -1, y = 0, xend = 0.6, yend = 0.3), color = "white", linewidth = 2, linetype = "dotted", curvature = 0.3) +
  geom_curve(aes(x = -0.7, y = 0.4, xend = 0.4, yend = 0.6), color = "white", linewidth = 2, linetype = "dotted", curvature = 0.3) +
  geom_curve(aes(x = -0.5, y = 0.7, xend = 0.25, yend = 0.9), color = "white", linewidth = 2, linetype = "dotted", curvature = 0.3) +
  theme(
    panel.background = element_rect(fill = "midnightblue"),  
    plot.background = element_rect(fill = "midnightblue")   
  ) +
  coord_cartesian(xlim = c(-1.2, 1.2), ylim = c(-0.2, 1.2)) +
  theme(
    axis.text = element_blank(),  
    axis.ticks = element_blank(), 
    axis.title = element_blank(), 
    panel.grid = element_blank(), 
    plot.margin = margin(0, 0, 0, 0)  
  ) +
  theme(legend.position = "none")
