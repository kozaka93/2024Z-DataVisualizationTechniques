library(ggplot2)

layers1 <- data.frame()
layers2 <- data.frame()
layers3 <- data.frame()
layers4 <- data.frame()
layers5 <- data.frame()
layers6 <- data.frame()

num_layers <- 16

triangle1 <- function(x, peak, width) {
  y <- pmax(0, 1 - abs(x - peak) / width)
  return(y)
}

for (i in 1:num_layers) {

  width <- 2 * (num_layers - i + 1) 

  x <- seq(-width, width, length.out = 100)
  
  y1 <- triangle1(x, peak = 0, width = width) + i
  
  width2 <- width * 0.7 
  y2 <- triangle1(x, peak = 0, width = width2) + i 
  
  width3 <- width * 0.5 
  y3 <- triangle1(x, peak = 0, width = width3) + i 
  
  width4 <- width * 0.3 
  y4 <- triangle1(x, peak = 0, width = width4) + i
  
  width5 <- width * 0.1 
  y5 <- triangle1(x, peak = 0, width = width5) + i 
  
  width6 <- width * 0.01 
  y6 <- triangle1(x, peak = 0, width = width6) + i

  layers1 <- rbind(layers1, data.frame(x = x, y = y1, group = i))
  layers2 <- rbind(layers2, data.frame(x = x, y = y2, group = i))
  layers3 <- rbind(layers3, data.frame(x = x, y = y3, group = i))
  layers4 <- rbind(layers4, data.frame(x = x, y = y4, group = i))
  layers5 <- rbind(layers5, data.frame(x = x, y = y5, group = i))
  layers6 <- rbind(layers6, data.frame(x = x, y = y6, group = i))
}

set.seed(2004)
snieg <- data.frame(
  x = runif(120, -35, 35),
  y = runif(120, 0, 19)
)

gwiazdka <- data.frame(x = 0, y = 17)

swiatelka <- data.frame(
  x = c(-25, -20, -15, -10,-7, -5, -2, 0, 2, 5,7, 10, 15, 20, 25),
  y = c(2, 4, 7, 1, 5, 10, 15, 13, 4, 8, 2, 11, 4, 6, 3),
  color = rep(c("#C45159", "#D7CE4B", "#889DF1", "#CA72CC"), length.out = 15)
)

# Dane dla pnia
pien <- data.frame(
  x = c(-4, -4, 4, 4),
  y = c(0, 12, 12, 0)
)

# Tworzenie wykresu
plot <- ggplot() +
  geom_polygon(data = pien, aes(x = x, y = y), fill = "#6A5952") +
  # Rysowanie warstw jako krzywych Gaussa
  geom_line(data = layers1, aes(x = x, y = y, group = group),
            color = "#006400", size = 5) +
  geom_line(data = layers2, aes(x = x, y = y, group = group),
            color = "#228B22", size = 5) +
  geom_line(data = layers3, aes(x = x, y = y, group = group),
            color = "#2E8B57", size = 5) +
  geom_line(data = layers4, aes(x = x, y = y, group = group),
            color = "#3CB371", size = 5) +
  geom_line(data = layers5, aes(x = x, y = y, group = group),
            color = "#82BD7F", size = 4) +
  geom_line(data = layers6, aes(x = x, y = y, group = group),
            color = "#8DA38C", size = 1) +
  geom_point(data = gwiazdka, aes(x = x, y = y), color = "#D7CB7C", size = 10, shape = 24, stroke = 3, fill = "#D7CB7C") +
  geom_point(data = gwiazdka, aes(x = x, y = y), color = "#D7CB7C", size = 10, shape = 25, stroke = 3, fill = "#D7CB7C") +
  geom_point(data = snieg, aes(x = x, y = y), color = "#E8F1F4", size = 3, shape = 8) +
  geom_point(data = swiatelka, aes(x = x, y = y, color = color), size = 6) +
  scale_color_manual(values = c("#C45159" = "#C45159", "#D7CE4B" = "#D7CE4B", "#889DF1" = "#889DF1", "#CA72CC" = "#CA72CC")) +
  theme_void() +
  theme(legend.position = 'none',
        panel.background = element_rect(fill = "#5E7387"))

ggsave("choinka_twd.png", plot = plot, width = 8, height = 10)