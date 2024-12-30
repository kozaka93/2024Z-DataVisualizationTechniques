library(ggplot2)
library(dplyr)

warstwy_choiny <- data.frame(
  x = c(-0.8, 0, 0.8, -1.6, 0, 1.6, -2.4, 0, 2.4),
  y = c(3.5, 5, 3.5, 2.5, 4, 2.5, 1.5, 3, 1.5),
  grupa = rep(1:3, each = 3)
)

pień <- data.frame(
  x = c(-0.3, 0.3, 0.3, -0.3),
  y = c(0, 0, 1.5, 1.5),
  grupa = 4
)

set.seed(123)
bombki <- data.frame(
  x = runif(50, -2.4, 2.4),
  y = runif(50, 1.5, 5),
  kolor = sample(c("red", "blue", "yellow", "purple"), 50, replace = TRUE)
) %>%
  filter(
    (y > 1.5 & y <= 3 & abs(x) <= 2.4 - 1.6 * (y - 1.5)) |
      (y > 2.5 & y <= 4 & abs(x) <= 1.6 - 1.2 * (y - 2.5)) |
      (y > 3.5 & y <= 5 & abs(x) <= 0.8 - 0.8 * (y - 3.5))
  )

dane_choiny <- rbind(
  data.frame(warstwy_choiny, kolor = "green"),
  data.frame(pień, kolor = "brown")
)

choinka <- ggplot() +
  geom_polygon(data = dane_choiny, aes(x, y, group = grupa, fill = kolor), color = "black") +
  geom_point(data = bombki, aes(x, y, color = kolor), size = 3) +
  scale_fill_manual(values = c("green" = "forestgreen", "brown" = "saddlebrown")) +
  scale_color_manual(values = c("red" = "red", "blue" = "blue", "yellow" = "yellow", "purple" = "purple")) +
  theme_void() +
  theme(legend.position = "none") +
  coord_fixed();
print(choinka)