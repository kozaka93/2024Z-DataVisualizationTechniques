library(ggplot2)
library(dplyr)

trunk <- data.frame(
  x = c(4.5, 5.5, 5.5, 4.5),
  y = c(0, 0, 1, 1)
)

layers <- list(
  data.frame(x = c(2, 5, 8), y = c(1, 3, 1)),
  data.frame(x = c(2.5, 5, 7.5), y = c(2.5, 4.5, 2.5)),
  data.frame(x = c(3, 5, 7), y = c(4, 6, 4)),
  data.frame(x = c(3.5, 5, 6.5), y = c(5.5, 7.5, 5.5)),
  data.frame(x = c(4.2, 5, 5.8), y = c(7, 8.5, 7))
)

baubles <- data.frame(
  x = c(
    2, 4.5, 8,   
    2.5, 5.5, 7.5,   
    3, 4.5, 7,   
    3.5, 5.3, 6.5,   
    4.2, 5, 5.8    
  ),
  y = c(
    1, 1.5, 1,  
    2.5, 3, 2.5,      
    4, 4.7, 4,  
    5.5, 6.2, 5.5,    
    7, 7.5, 7     
  ),
  color = c(
    "blue", "red", "gold",    
    "gold", "red", "blue",    
    "red", "blue", "gold",    
    "red", "gold", "blue",    
    "gold", "blue", "red"     
  )
)

star <- data.frame(
  x = 5,
  y = 8.5
)

garlands <- data.frame(
  x_start = c(2, 2.5, 3, 3.5),  
  x_end = c(7.5, 7, 6.5, 5.8),    
  y_start = c(1, 2.5, 4, 5.5), 
  y_end = c(2.5, 4, 5.5, 7)   
)


p <- ggplot() +
  geom_polygon(data = trunk, aes(x = x, y = y), fill = "saddlebrown") +
  lapply(layers, function(layer) {
    geom_polygon(data = layer, aes(x = x, y = y), fill = "lightgreen", color = "darkgreen")
  }) +
  geom_curve(data = garlands, 
             aes(x = x_start, y = y_start, xend = x_end, yend = y_end), 
             color = "gold", curvature = 0.3, size = 1) +
  geom_point(data = baubles, aes(x = x, y = y, color = color), size = 3) +
  scale_color_manual(values = c("red" = "red", "gold" = "gold", "blue" = "blue")) +
  geom_point(data = star, aes(x = x, y = y), color = "gold", size = 7, shape = 8) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none")


print(p)