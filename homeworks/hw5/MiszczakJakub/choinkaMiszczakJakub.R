library(ggplot2)
library(gganimate)
library(magick)
library(dplyr)

layers <- 100 
points_per_layer <- 100 

tree_data <- data.frame(
  x = unlist(lapply(1:layers, function(i) runif(points_per_layer, -i, i))),
  y = unlist(lapply(1:layers, function(i) rep(-i, points_per_layer)))
)

chain_data1 <- data.frame(
  x = seq(-70, 70, length.out = 200),
  y = -layers + 30 + 5*sin(seq(0, 2 * pi, length.out = 200))
)

chain_data2 <- data.frame(
  x = seq(-30, 30, length.out = 100),
  y = -layers + 70 + 3*sin(seq(0, 2 * pi, length.out = 100))
)


lamp_data1 <- data.frame(
  frame = seq(1, 200),
  x = seq(-70, 70, length.out = 200),
  y = -layers + 30 + 5*sin(seq(0, 2 * pi, length.out = 200))
)

lamp_data1_2 <- data.frame(
  frame = seq(51, 250),
  x = seq(-70, 70, length.out = 200),
  y = -layers + 30 + 5*sin(seq(0, 2 * pi, length.out = 200))
)

lamp_data1_3 <- data.frame(
  frame = seq(101, 300),
  x = seq(-70, 70, length.out = 200),
  y = -layers + 30 + 5*sin(seq(0, 2 * pi, length.out = 200))
)

lamp_data1_4 <- data.frame(
  frame = seq(151, 350),
  x = seq(-70, 70, length.out = 200),
  y = -layers + 30 + 5*sin(seq(0, 2 * pi, length.out = 200))
)



lamp_data2 <- data.frame(
  frame = seq(1, 150),
  x = seq(-30, 30, length.out = 150),
  y = -layers + 70 + 3*sin(seq(0, 2 * pi, length.out = 150))
)

lamp_data2_2 <- data.frame(
  frame = seq(101, 250),
  x = seq(-30, 30, length.out = 150),
  y = -layers + 70 + 3*sin(seq(0, 2 * pi, length.out = 150))
)

lamp_data2_3 <- data.frame(
  frame = seq(201, 350),
  x = seq(-30, 30, length.out = 150),
  y = -layers + 70 + 3*sin(seq(0, 2 * pi, length.out = 150))
)


p <- ggplot() +
  geom_point(data = tree_data, aes(x = x, y = y), color = "darkgreen", size = 1) +
  geom_line(data = chain_data1, aes(x = x, y = y), color = "gold", size = 1) +
  geom_line(data = chain_data2, aes(x = x, y = y), color = "gold", size = 1) +
  geom_point(data = lamp_data1, aes(x = x, y = y, frame = frame), color = "#cc0000", size = 4) +
  geom_point(data = lamp_data1_2, aes(x = x, y = y, frame = frame), color = "#33ccff", size = 4) +
  geom_point(data = lamp_data1_3, aes(x = x, y = y, frame = frame), color = "#cc0000", size = 4) +
  geom_point(data = lamp_data1_4, aes(x = x, y = y, frame = frame), color = "#33ccff", size = 4) +
  geom_point(data = lamp_data2, aes(x = x, y = y, frame = frame), color = "#cc0000", size = 4) +
  geom_point(data = lamp_data2_2, aes(x = x, y = y, frame = frame), color = "#33ccff", size = 4) +
  geom_point(data = lamp_data2_3, aes(x = x, y = y, frame = frame), color = "#cc0000", size = 4) +
  geom_point(data = tail(chain_data1, 1), aes(x = x, y = y), color = "black", size = 4) +
  geom_point(data = tail(chain_data2, 1), aes(x = x, y = y), color = "black", size = 4) +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))


anim <- p +
  transition_reveal(along = frame) +
  ease_aes('linear')


anim_save("choinka.gif", animation = anim, duration = 5, width = 500, height = 600, fps = 50)
