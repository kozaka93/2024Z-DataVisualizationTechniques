#library(ggplot2)

trawa <- data.frame(
  x = c(-100, 100, 100, -100),
  y = c(-50, -50, -10, -10)
)


pien <- data.frame(
  x = c(-10, 10, 10, -10),
  y = c(-10, -10, 10, 10)
)

snieg_grunt <- data.frame(
  x = runif(1000, -100, 100),
  y = runif(100, -20, -10)
)

snieg_grunt2 <- data.frame(
  x = runif(50, -100, 100),
  y = runif(50, -30, -20)
)

choinka1 <- data.frame(
  x = c(-70, 0, 70),
  y = c(10, 60, 10)
)

choinka2 <- data.frame(
  x = c(-55, 0, 55),
  y = c(35, 75, 35)
)

choinka3 <- data.frame(
  x = c(-45, 0, 45),
  y = c(55, 100, 55)
)

choinka4 <- data.frame(
  x = c(-35, 0, 35),
  y = c(75, 120, 75)
)

choinka5 <- data.frame(
  x = c(-25, 0, 25),
  y = c(95, 150, 95)
)

bombki <- data.frame(
  x = c(-50, -43, -32, -25, -11, -2, 8, 12, 30, 39, 48),
  y = c(15, 42, 30, 60, 100, 25, 125, 52, 80, 42, 16), 
  kolor = c('red', 'purple', 'red', 'purple', 'red', 'purple', 'red', 'purple', 'red', 'purple', 'red')
)

gwiazda <- data.frame(
  x = c(-15, -5, 0, 5, 15, 8, 12, 0, -12, -8),
  y = c(150, 150, 160, 150, 150, 143, 132, 138, 132, 143)
)

snieg <- data.frame(
  x = runif(200, -100, 100),
  y = runif(200, 0, 160)
)



ggplot() +
  geom_polygon(data = trawa, aes(x = x, y = y), fill = "darkgreen") +
  geom_polygon(data = pien, aes(x = x, y = y), fill = "brown") +
  geom_point(data = snieg_grunt, aes(x = x, y = y), color = "white") +
  geom_point(data = snieg_grunt2, aes(x = x, y = y), color = "white") +
  geom_polygon(data = choinka1, aes(x = x, y = y), fill = "darkgreen") +
  geom_polygon(data = choinka2, aes(x = x, y = y), fill = "lightgreen") +
  geom_polygon(data = choinka3, aes(x = x, y = y), fill = "darkgreen") +
  geom_polygon(data = choinka4, aes(x = x, y = y), fill = "lightgreen") +
  geom_polygon(data = choinka5, aes(x = x, y = y), fill = "darkgreen") +
  geom_point(data = bombki, aes(x = x, y = y, color = kolor), size = 8) +
  geom_polygon(data = gwiazda, aes(x = x, y = y), fill = "yellow") +
  geom_point(data = snieg, aes(x = x, y = y), color = "white") +
  coord_fixed() +
  scale_color_manual(values = c("red" = "red", "purple" = "purple"))+
  theme(legend.position = "none")+
  theme(
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  );
