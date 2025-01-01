library(ggplot2)

segments <- list(
  data.frame(x = c(-10, 0, 10), y = c(0, 20, 0)),   
  data.frame(x = c(-8, 0, 8), y = c(10, 30, 10)),   
  data.frame(x = c(-6, 0, 6), y = c(20, 35, 20))
)

trunk <- data.frame(
  x = c(-2, -2, 2, 2),
  y = c(-5, 0, 0, -5)
)

baubles <- data.frame(
  x = c(-8, -6, -4, -3, -2, 0, 2, 3, 4, 6, 8,   
        -6, -4.5, -3, -1.5, 0, 1.5, 3, 4.5, 6,              
        -4, -3, -2, -1, 0, 1, 2, 3, 4),                    
  y = c(2, 6, 3, 7, 10, 7 , 5, 2, 8, 3, 1,     
        12, 14, 18, 15, 13, 19, 15, 16, 12,           
        21.5, 25, 23, 28, 30, 21, 27, 25, 22),
  color = c("red", "yellow", "blue", "pink", "red", "yellow", "blue", "pink",   
            "blue", "pink", "yellow", "red", "blue", 
            "red", "yellow", "blue", "red", "yellow", "blue", "pink", "red", "yellow", "pink","blue", "red", "yellow","blue", "pink", "blue")
)

star <- data.frame(
  x = 0,    
  y = 35,   
  color = "gold"
)

snowflakes <- data.frame(
  x = c(runif(25, min = -15, max = -10), runif(25, min = 10, max = 15)),  
  y = runif(50, min = 0, max = 40)
)

ggplot() +
  theme_void() +
  theme(plot.background = element_rect(fill = "navy", color = "navy")) +
  geom_polygon(data = segments[[1]], aes(x = x, y = y), fill = "darkgreen") +
  geom_polygon(data = segments[[2]], aes(x = x, y = y), fill = "darkgreen") +
  geom_polygon(data = segments[[3]], aes(x = x, y = y), fill = "darkgreen") +
  geom_polygon(data = trunk, aes(x = x, y = y), fill = "saddlebrown") +
  geom_point(data = baubles, aes(x = x, y = y, color = color), size = 3) +
  geom_point(data = star, aes(x = x, y = y, color = color), size = 6, shape = 8) +
  geom_point(data = snowflakes, aes(x = x, y = y), color = "white", size = 2, alpha = 0.7) +
  scale_color_identity() +
  labs(title = "Wesołych świąt!") +
  theme(
    plot.title = element_text(color = "white", size = 20, face = "bold", hjust = 0.5)
  )
