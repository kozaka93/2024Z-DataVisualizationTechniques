
library(ggplot2)

tree_layers <- data.frame(
  x = c(-4, 0, 4, -3.5, 0, 3.5, -3, 0, 3, -2.5, 0, 2.5,
        -2, 0, 2, -1.6, 0, 1.6, -1.3, 0, 1.3), 
  y = c(1, 5, 1, 2, 6, 2, 3, 7, 3, 4, 8, 4,
        5, 9, 5, 6, 10, 6, 7, 10, 7), 
  group = rep(1:7, each = 3)  
)


set.seed(123)
snow <- data.frame(
  x = runif(100, -5, 5),
  y = runif(100, 0, 11)
)

lights <- data.frame(
  x = c(-2.5, 0, 2.5, -1.5, -0.4, 2.2, -0.5, 0.6, 2, -1.2, 0, 1),
  y = c(2, 3, 2, 3.5, 7, 3.5, 5, 8, 4.5, 6, 9, 6),
  color = rep(c("red", "yellow", "blue", "green"), length.out = 12)
)



trunk <- data.frame(
  x = c(-0.5, -0.5, 0.5, 0.5),
  y = c(0, 1, 1, 0)
)


star <- data.frame(x = 0, y = 10)


plot <- ggplot() +
  geom_polygon(data = tree_layers, aes(x = x, y = y, group = group),
               fill = "darkgreen", color = "black") +
  geom_polygon(data = trunk, aes(x = x, y = y), fill = "saddlebrown") +
  geom_point(data = star, aes(x = x, y = y), color = "gold", size = 9, shape = 8, stroke = 1.5) +
  geom_point(data = snow, aes(x = x, y = y), color = "white", size = 2, alpha = 0.8) +
  geom_point(data = lights, aes(x = x, y = y, color = color), size = 4) +
  scale_color_manual(values = c("red" = "red", "yellow" = "yellow",
                "blue" = "blue", "green" = "green"))+
  theme_void()+
  coord_fixed() +
  labs(title = "Wesołych Świąt!") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        legend.position = 'none',
        panel.background = element_rect(fill = "midnightblue"))


ggsave("choinka.png", plot = plot)
