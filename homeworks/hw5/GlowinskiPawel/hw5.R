library(ggplot2)
library(dplyr)

tree_layers <- data.frame(
  x = c(-4.0, 0, 4.0, -3.5, 0, 3.5, -3.0, 0, 3.0, -2.5, 0, 2.5, -2.0, 0, 2.0, -1.5, 0, 1.5, -1.1, 0, 1.1, -0.7, 0,0.7),
  y = c(0, 2, 0, 1, 3, 1, 2, 4, 2, 3, 5, 3, 4, 6, 4, 4.85, 6.85, 4.85, 5.7, 7.5, 5.7, 6.5, 8, 6.5),
  group = rep(1:8, each = 3)
)

trunk <- data.frame(
  x = c(-0.4, 0.3, 0.3, -0.4),
  y = c(-1, -1,0, 0)
)

set.seed(1885)

baubles <- data.frame(
  x = runif(25, -2, 2),
  y = runif(25, 0, 3),
  color = sample(c("gold"), size = 25, replace = TRUE)
)

baubles2 <- data.frame(
  x = runif(20, -0.8, 0.8),
  y = runif(20, 3, 7),
  color = sample(c("gold"), size = 20, replace = TRUE)
)


chain <- data.frame(
  x = seq(-1.5, 1.5, length.out = 100),
  y = 4 - 1.5 * sin(seq(-pi, pi, length.out = 100))
)

chain2 <- data.frame(
  x = seq(-2.5, 2.5, length.out = 100),
  y = 2 - 1.5 * sin(seq(-pi, pi, length.out = 100))
)

snow <- data.frame(
    x = runif(200, min = -4.5, max = 4.5),
    y = runif(200, min = -1.3, max = 8.5)
  )



ggplot() +
  geom_polygon(data = tree_layers, aes(x = x, y = y, group = group), fill = "darkgreen", color = "darkgreen") +
  geom_polygon(data = trunk, aes(x = x, y = y), fill = "saddlebrown") +
  geom_point(data = baubles, aes(x = x, y = y, color = color), size = 4) +
  geom_point(data = baubles2, aes(x = x, y = y, color = color), size = 4) +
  geom_line(data = chain, aes(x = x, y = y), color = "white", size = 1, linetype = "dashed") +
  geom_line(data = chain2, aes(x = x, y = y), color = "white", size = 1, linetype = "dashed") +
  geom_point(data = snow, aes(x = x, y = y), color = "white", size = 1)+
  theme_void() +
  coord_equal() +
  scale_color_manual(values = c("gold" = "gold"))+
  theme(panel.background = element_rect(fill = "#003366"),
        plot.background = element_rect(fill = "#003366"),
        panel.grid =  element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        legend.position = "none")


