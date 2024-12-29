library(ggplot2)


tree_outline <- data.frame(
  x = c(
    0, -1.5, -2, -3, -2.5, -4, 0, 4, 2.5, 3, 2, 1.5, 0,   
    0, -2, -2.5, -4, -3.5, -5, 0, 5, 3.5, 4, 2.5, 2, 0,   
    0, -3, -3.5, -5, -4.5, -6, 0, 6, 4.5, 5, 3.5, 3, 0,   
    0, -4, -4.5, -6, -5.5, -7, 0, 7, 5.5, 6, 4.5, 4, 0,   
    0, -5, -5.5, -7, -6.5, -8, 0, 8, 6.5, 7, 5.5, 5, 0    
  ),
  y = c(
    0, -1, -1.5, -2, -2.5, -3, -4, -3, -2.5, -2, -1.5, -1, 0,   
    -4, -5, -5.5, -6, -6.5, -7, -8, -7, -6.5, -6, -5.5, -5, -4,  
    -8, -9, -9.5, -10, -10.5, -11, -12, -11, -10.5, -10, -9.5, -9, -8, 
    -12, -13, -13.5, -14, -14.5, -15, -16, -15, -14.5, -14, -13.5, -13, -12, 
    -16, -17, -17.5, -18, -18.5, -19, -20, -19, -18.5, -18, -17.5, -17, -16  
  ),
  group = rep(1:5, each = 13)
)


star <- data.frame(
  x = c(0),
  y = c(-0.5),  
  label = c("*")
)
tree_trunk <- data.frame(
  x = c(-1, -1, 1, 1),
  y = c(-20, -19, -19, -20)
)


ornaments <- data.frame(
  x = c(0.5, -1.2, 1.5, -2.5, 2.5, 0, -3.5, 2, -4, 3, -0.5, 4, 1, -1,-1,-1, -4, 2),  # Współrzędne x dla bombek
  y = c(-3, -5.4, -5.8, -7, -6.8, -9.5, -11, -9.8, -14, -14.5, -18,-17.5, -13.5, -14.8,-11, -2,-18.5, -19),  # Współrzędne y dla bombek
  color = c("red", "blue", "gold", "grey", "red", "blue", "gold", "yellow", "red", "blue", "gold","magenta","orange","yellow","brown","black","navy","red")  # Kolory bombek
)


ggplot() +
  geom_polygon(data = tree_outline, aes(x = x, y = y, group = group), fill = "green", color = "black", size = 1) +
  geom_text(data = star, aes(x = x, y = y, label = label), size = 30, color = "gold") +
  geom_polygon(data = tree_trunk, aes(x = x, y = y), fill = "saddlebrown", color = "black") +
  geom_point(data = ornaments, aes(x = x, y = y, color = color), size = 5, shape = 16) +
  scale_color_identity() +
  theme_void() +
  coord_equal()

