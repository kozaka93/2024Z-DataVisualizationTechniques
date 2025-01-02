library(ggplot2)

tree <- data.frame(
  x = c(-3, 0, 3, -2.5, 0, 2.5, -2, 0, 2, -1.5, 0, 1.5),
  y = c(0, 3, 0, 3, 6, 3, 6, 9, 6, 9, 12, 9),
  group = c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3))
)

baubles <- data.frame(
  x = c(-2, 0, 2, -1.5, 1.5, -1, 0, 1, -0.5, 0, 0.5, 0, 0),
  y = c(0.5, 1.5, 0.5, 3.7, 3.7, 7, 8.5, 7, 10, 11, 10, 3.9, 5.5),
  color = c("red", "blue", "gold","red", "blue", "green", "purple", "orange", "pink", "yellow", "green", "purple", "orange")
)

star <- data.frame(
  x = c(0, -0.1, -0.3, -0.1, -0.2, 0, 0.2, 0.1, 0.3, 0.1),
  y = c(13.7, 13, 13, 12.5, 11.7, 12.3, 11.7, 12.5, 13, 13)
)

chains <- data.frame(
  x = c(-2.5, -1.5, -0.5, 0.5, 1.5,  -2, -1, 0, 1.37, -1.5, 0, 1),
  y = c(3, 3.3, 3.6, 3.9, 4.2, 6, 6.3, 6.6, 6.9, 9, 9.65, 10),
  group = c(rep(1, 5), rep(2, 4), rep(3, 3))
)

plot <- ggplot() +
  geom_polygon(data = tree, aes(x = x, y = y, group = group), fill = "darkgreen", color = "black") +
  geom_line(data = chains, aes(x = x, y = y, group = group), color = "gold", size = 2) +
  geom_point(data = baubles, aes(x = x, y = y, color = color), size = 4) +
  geom_polygon(data = star, aes(x = x, y = y), fill = "yellow", color = "gold") +
  theme_void() +
  theme(legend.position = "none")
plot
