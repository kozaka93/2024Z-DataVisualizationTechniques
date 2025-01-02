library(ggplot2)

tree <- data.frame(
  x = c(0, -4, 4, 0, -6, 6, 0, -8, 8),
  y = c(10, 6, 6, 7, 3, 3, 4, 0, 0),
  group = c(1, 1, 1, 2, 2, 2, 3, 3, 3)
)

wood <- data.frame(
  x = c(-1, 1, 1, -1),
  y = c(0, 0, -2, -2)
)

ornaments <- data.frame(
  x = c(-5, -2, 0, 2, 5, -2, 2, -4.5, -1, 1, 4.5, -0.7, 0.7, -1, 1),
  y = c(0.4, 0.4, 0.4, 0.4, 0.4, 2, 2, 3.7, 3.7 ,3.7, 3.7, 5, 5, 8, 8),
  color = sample(c("red", "gold", "blue", "white"), 30, replace = TRUE)
)

ggplot() +
  geom_polygon(data = tree, aes(x, y, group = group), fill = "#006400") +
  geom_polygon(data = wood, aes(x, y), fill = "#8b4513") +
  geom_point(data = ornaments, aes(x, y, color = color), size = 3) +
  scale_color_manual(values = c("red" = "red", "gold" = "gold", "blue" = "blue", "white" = "white")) +
  geom_point(aes(x = 0, y = 10), shape = 8, size = 6, color = "gold") +
  theme_void() +
  theme(legend.position = "none")
