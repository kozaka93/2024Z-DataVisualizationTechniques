library(ggplot2)
library(gganimate)
library(sf)
library(gifski)

generate_star_coords <- function(r_outer, r_inner) {
  coords <- data.frame(x = numeric(0), y = numeric(0))
  
  for (k in 0:4) {
    x_outer <- r_outer * cos(2 * pi * k / 5 + pi / 2)
    y_outer <- r_outer * sin(2 * pi * k / 5 + pi / 2)
    coords <- rbind(coords, c(x_outer, y_outer))
    
    x_inner <- r_inner * cos(2 * pi * k / 5 + pi / 2 + pi / 5)
    y_inner <- r_inner * sin(2 * pi * k / 5 + pi / 2 + pi / 5)
    coords <- rbind(coords, c(x_inner, y_inner))
  }
  
  coords <- rbind(coords, coords[1, ])
  colnames(coords) <- c("x", "y")
  
  return(coords)
}

r_outer <- 1   
r_inner <- 0.4 

coords <- generate_star_coords(r_outer, r_inner)

tree_coords = list(
  matrix(
    c(-4, 0,
      -2.22, 2,
      -3.5, 2,
      -1.5, 4,
      -2.5, 4,
      -0.8, 6,
      -1.5, 6,
      0, 8,
      1.5, 6,
      0.8, 6,
      2.5, 4,
      1.5, 4,
      3.5, 2,
      2.22, 2,
      4, 0,
      -4, 0),
    ncol = 2, byrow = T
  )
)

tree = st_polygon(tree_coords)

points = st_sample(tree, 75)
colours = sample(c("red", "purple", "blue", "yellow", "white"), 75, replace = TRUE)

scale_factor = 1.2  
coords_scaled <- data.frame(
  x = coords$x * scale_factor,
  y = coords$y * scale_factor + 8
)

lancuch1 <- data.frame(
  x = seq(-3.5, 1.7, length.out = 10),
  y = seq(2, 4, length.out = 10)
)

lancuch2 <- data.frame(
  x = seq(-2.5, 1, length.out = 6),
  y = seq(4, 6, length.out = 6)
)

lancuch3 <- data.frame(
  x = seq(-4, 2.5, length.out = 12),
  y = seq(0, 2, length.out = 12)
)


set.seed(123)  
snow_points = data.frame(
  x = runif(100, -7, 7),  
  y = runif(100, -6, 10)  
)

gg_tree = ggplot() +
  geom_sf(aes(), data = tree, fill = "forestgreen", color = "darkgreen") +
  geom_sf(aes(size = 3, color = colours), data = points) +
  geom_rect(aes(xmin = -0.75, xmax = 0.75, ymin = -2, ymax = 0), fill = "saddlebrown", color = "sienna4") +
  geom_rect(aes(xmin = -7, xmax = 7, ymin = -6, ymax = -2), fill = "darkgreen", color = "darkgreen") +
  geom_polygon(data = coords_scaled, aes(x = x, y = y), fill = "gold", color = "gold", size = 2) +
  geom_path(data = lancuch1, aes(x = x, y = y), color = "lightblue", size = 1, linetype = "solid") +
  geom_point(data = lancuch1, aes(x = x, y = y), color = "yellow", size = 4) + 
  geom_path(data = lancuch2, aes(x = x, y = y), color = "lightblue", size = 1, linetype = "solid") +
  geom_point(data = lancuch2, aes(x = x, y = y), color = "yellow", size = 4) + 
  geom_path(data = lancuch3, aes(x = x, y = y), color = "lightblue", size = 1, linetype = "solid") +
  geom_point(data = lancuch3, aes(x = x, y = y), color = "yellow", size = 4) + 
  geom_point(data = snow_points, aes(x = x, y = y), color = "white", size = 2) + 
  theme_void() +
  theme(plot.background = element_rect(fill = "navy", color = "navy"),
        legend.position = "none") +
  coord_sf(xlim = c(-6, 6), ylim = c(-4, 10)) +
  annotate("text", x = 0, y = 10, label = "Wesołych Świąt!", size = 6, color = "white")

animated_tree = gg_tree + 
  transition_time(1:75) + 
  ease_aes('linear')

anim_save("C:/Studia/3sem/techniki wizualizacji/homeworks/hw5/choinka_gif.gif", animated_tree, renderer = gifski_renderer())

browseURL("choinka_gif.gif")
