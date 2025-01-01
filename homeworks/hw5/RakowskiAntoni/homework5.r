library(ggplot2)

tree_data <- data.frame(
  x = c(0, -3, 3,   0, -2.5, 2.5,   0, -2, 2),
  y = c(3, 0, 0,   5, 2, 2,   7, 4, 4),
  group = c(rep(1, 3), rep(2, 3), rep(3, 3))
)

trunk_data <- data.frame(
  x = c(-0.5, -0.5, 0.5, 0.5),
  y = c(0, -1, -1, 0)
)

set.seed(12345)
ornament_data <- data.frame(
  x = c(rep(c(seq(-2, 2), seq(-1.5, 1.5)), 2),
        c(-1.5, -0.75, 0, 0.75, 1.5), seq(-1, 1)),
  y = c(rep(0.3, 5), rep(0.9, 4), rep(2.3, 5),
        rep(2.9, 4), rep(4.3, 5), rep(4.9, 3)),
  color = sample(c("red", "white"), 26, replace = TRUE)
)

chain_data <- data.frame(
  x = c(seq(-1.5, 1.5, length.out = 6),
        seq(-1.4, 1.4, length.out = 6),
        seq(-1.3, 1.3, length.out = 5),
        seq(-1.2, 1.2, length.out = 5),
        seq(-1, 1, length.out = 4),
        seq(-0.9, 0.9, length.out = 4)),
  y = c(rep(1.5, 6),
        rep(1.6, 6),
        rep(3.5, 5),
        rep(3.6, 5),
        rep(5.5, 4),
        rep(5.6, 4)),
  group = c(rep(1, 6), rep(2, 6),
            rep(3, 5), rep(4, 5),
            rep(5, 4), rep(6, 4))
)

star_data <- data.frame(
  x = c(0, 0.15, 0.5, 0.2, 0.3, 0, -0.3, -0.2, -0.5, -0.15),
  y = c(7.3, 7, 7, 6.8, 6.5, 6.7, 6.5, 6.8, 7, 7)
)

gift_box_data <- data.frame(
  x = c(-1.5, 1.5, 1.5, -1.5) + 2.2,
  y = c(-2.1, -2.1, -3, -3) + 2
)

ribbon_horizontal_data <- data.frame(
  x = c(-1.5, 1.5, 1.5, -1.5) + 2.2,
  y = c(-2.6, -2.6, -2.4, -2.4) + 2
)

ribbon_vertical_data <- data.frame(
  x = c(-0.1, 0.1, 0.1, -0.1) + 2.2,
  y = c(-2.1, -2.1, -3, -3) + 2
)

snowflakes_data <- data.frame(
  x = runif(100, -4, 4),
  y = runif(100, -1, 8),
  size = runif(100, 2, 5)
)

ground_data <- data.frame(
  x = c(-4, 4, 4, -4),
  y = c(-1, -1, -4, -4)
)

ggplot() +
  geom_polygon(data = ground_data, aes(x = x, y = y), fill = "beige") + 
  geom_polygon(data = tree_data, aes(x = x, y = y, group = group), fill = "#128112") +
  geom_polygon(data = trunk_data, aes(x = x, y = y), fill = "#3e1c00") +
  geom_point(data = ornament_data, aes(x = x, y = y, color = color), size = 7) +
  geom_path(data = chain_data, aes(x = x, y = y, group = group), color = "gold", size = 1) +
  geom_polygon(data = star_data, aes(x = x, y = y), fill = "gold") +
  geom_polygon(data = gift_box_data, aes(x = x, y = y), fill = "red") +
  geom_polygon(data = ribbon_horizontal_data, aes(x = x, y = y), fill = "gold") +
  geom_polygon(data = ribbon_vertical_data, aes(x = x, y = y), fill = "gold") +
  geom_point(data = snowflakes_data, aes(x = x, y = y), color = "white", size = snowflakes_data$size, alpha = 0.5) +
  geom_text(aes(x = 0, y = 7.5), label = "Merry Christmas", size = 8, color = "white", fontface = "bold") +
  coord_fixed() +
  theme_void() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme(legend.position = "none") +
  theme(plot.background = element_rect(fill = "#87ceeb", color = "#87ceeb")) +
  scale_color_manual(values = c("red" = "red", "white" = "white"))
