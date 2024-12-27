library(dplyr)
library(plotly)

generate_cone_or_cylinder <- function(z_range, r_function, color) {
  z <- seq(z_range[1], z_range[2], length.out = 200)
  theta <- seq(0, 2 * pi, length.out = 200)
  r <- r_function(z)
  
  z_grid <- rep(z, each = length(theta))
  theta_grid <- rep(theta, times = length(z))
  r_grid <- rep(r, each = length(theta))
  
  x <- r_grid * cos(theta_grid)
  y <- r_grid * sin(theta_grid)
  
  data.frame(x = x, y = y, z = z_grid, color = color)
}

generate_disk <- function(z, outer_radius, inner_radius = 0, color) {
  
  
  theta <- seq(0, 2 * pi, length.out = 200)
  
  r <- seq(inner_radius, outer_radius, length.out = 50)
  
  r_grid <- rep(r, each = length(theta))
  theta_grid <- rep(theta, times = length(r))
  
  x <- r_grid * cos(theta_grid)
  y <- r_grid * sin(theta_grid)
  z_grid <- rep(z, length(x))
  
  data.frame(x = x, y = y, z = z_grid, color = color)
}



cone1 <- generate_cone_or_cylinder(c(-3, 0), function(z) abs(z), "darkgreen")
disk1 <- generate_disk(z = -3, outer_radius = 3, inner_radius = 1,color = "darkgreen")
cone2 <- generate_cone_or_cylinder(c(-6, -3), function(z) abs(z + 2), "darkgreen")
disk2 <- generate_disk(z = -6, outer_radius = 4, inner_radius = 1,color = "darkgreen")
cone3 <- generate_cone_or_cylinder(c(-9, -6), function(z) sqrt(1.5) * abs(z + 4), "darkgreen")
cylinder <- generate_cone_or_cylinder(c(-10.5, -3), function(z) rep(1, length(z)), "brown")

df_combined <- bind_rows(cone1, disk1, cone2,disk2, cone3, cylinder)

fig<-plot_ly(df_combined, x = ~x, y = ~y, z = ~z, color = ~color, 
        colors = c("#4d2005", "darkgreen"), type = 'scatter3d', mode = 'markers',
        marker = list(size = 1)) %>%
  layout(scene = list(xaxis = list(title = "X"),
                      yaxis = list(title = "Y"),
                      zaxis = list(title = "Z")))

fig

htmlwidgets::saveWidget(fig, "choinka.html")
