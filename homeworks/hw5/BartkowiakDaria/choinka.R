library(ggplot2)
library(ggstar)

choinka <- data.frame(
  x = c(-4, 0, 4, -3.5, 0, 3.5, -3, 0, 3, -2.75, 0, 2.75),
  y = c(1, 8, 1, 3, 9, 3, 5, 10, 5, 7, 11, 7),
  group = rep(1:4, each = 3) 
)
  
pien <- data.frame(
  x = c(-0.5, 0.5, 0.5, -0.5),
  y = c(0, 0, 1, 1)
)

bombki <- data.frame(
  x = c(-3.5, -2, 0, 2, 3.5, -2.5, 0, 2.5, -3, -1.5, 0, 1.5, 3, -2, 0, 2, -1.75, 0, 1.75, -1.75, 0, 1.75, -1, 1),
  y = c(1.5, 1.5, 1.5, 1.5, 1.5, 2.75, 2.75, 2.75, 3.75, 3.75, 3.75, 3.75, 3.75, 5, 5, 5, 6.5, 6.5 ,6.5, 7.75, 7.75, 7.75, 9, 9)
)
set.seed(123) 
kolory <- sample(colors(), size = nrow(ornaments_data), replace = TRUE)

bombki$kolor <- kolory

lancuch <- data.frame(
  x = c(-0.7, 1.3, -2, 1.9, -2.4, 2.8, -2.8, 3.2, -3.3, 3.7),
  y = c(10, 9, 8, 7, 6, 5.3, 4.2, 3.5, 2.2, 1.5),
  group = rep(1, 2)
)

stworz_gwiazdke <- function(center_x, center_y, size) {
  angles <- seq(0, 2 * pi, length.out = 11)
  radius <- rep(c(size, size / 2), 5) 
  data.frame(
    x = center_x + radius * cos(angles),
    y = center_y + radius * sin(angles)
  )
}
gwiazdka <- stworz_gwiazdke(0, 11, 1)

ggplot() +
  geom_polygon(data = choinka, aes(x = x, y = y, group = group),
               fill = "darkgreen", color = "darkgreen") +
  geom_polygon(data = pien, aes(x = x, y = y), fill = "chocolate4") +
  geom_point(data = bombki, aes(x = x, y = y, color = kolor),
             size = 4) +
  geom_path(data = lancuch, aes(x = x, y = y, group = group),
            color = "yellow", linewidth = 1) +
  geom_polygon(data = gwiazdka, aes(x = x, y = y),
               fill = "gold", color = "gold") +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none") 
