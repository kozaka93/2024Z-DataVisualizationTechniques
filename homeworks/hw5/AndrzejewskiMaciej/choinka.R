library(ggplot2)
library(dplyr)
set.seed(13)

tree <- data.frame(ground = seq(0, 10, length.out = 70)) %>%
  mutate(green = (10 - 2*abs(ground - 5)),
         trunk = ifelse(abs(ground - 5) < 1.5, -2, NA))

baubles <- data.frame(b_x = c(1.5, 3, 3.2, 3.8, 4, 4, 5, 5, 5.5, 5.9, 6,
                              6.5, 7.4, 7.9, 8.8),
                      b_y = c(1, 3, 2, 5, 0.8, 7, 3, 6.2, 7.8, 5.5, 3.5,
                              1.8, 3.9, 1.2, 0.7),
                      b_col = sample(1:5, 15, replace = TRUE))


snow <- data.frame(snow_x = c(-4.2, -3.5, -4.1, -4, -3, -1.6, -1.5, -0.2,
                              0.2, 1, 1, 3, 6.7, 7.2, 9.5, 12.1, 8.9, 11.5,
                              10.7, 10, 13, 12, 12, 14.5),
                   snow_y = c(-0.8, 2, 4, 9.5, 7, 2, 6.5, 10, 2.5, 5, 7.8,
                              10, 11, 9, 10, 9.8, 6.5, 7.2, 5.2, 2.9, 5.5,
                              3.9, 0.7, 1.2))

ggplot(tree) + 
  geom_point(aes(ground, green), color = "lightgreen") +
  geom_segment(aes(ground, y = 0, yend = green), color = "green") + 
  geom_point(aes(ground, trunk), color = "brown", na.rm = TRUE) + 
  geom_segment(aes(ground, y = 0, yend = trunk), color = "brown",
               na.rm = TRUE) + 
  geom_point(data = baubles, aes(b_x, b_y, fill = factor(b_col)),
             shape = 21, size = 4.7, color = "navy") +
  scale_fill_manual(values = c("#ff0000", "#f0db4d", "#DF70FF",
                               "turquoise", "#ff1414")) +
  ylim(-2, 12) +
  xlim(-5, 15) +
  annotate("text", x = 5, y = 10.5,
           label = "\u2605", color = "gold", size = 20) +
  coord_fixed(1.3) +
  annotate("text", x = 2, y = -1, label = "\U1F381", size = 10, color = "red") +
  annotate("text", x = 8, y = -1, label = "\U1F381", size = 10, color = "navy") +
  annotate("text", x = -0.5, y = -1, label = "\U1F381", size = 10, color = "green") +
  annotate("text", x = 10.5, y = -1, label = "\U1F381", size = 10, color = "purple") +
  theme_void() +
  theme(
  legend.position = "None",
  panel.background = element_rect(fill = "#17104d", color = "white")
  ) +
  annotate("text", x = snow$snow_x, y = snow$snow_y,
           label = "\U2744", color = "white", size = 5)
