library(plotly)


generate_3d_tree_data <- function(layers, points_per_layer) {
  data <- data.frame()
  for (layer in 1:layers) {
    angle <- seq(0, 2 * pi, length.out = points_per_layer + 1)[-1]
    radius <- seq(layers, 1, length.out = layers)[layer]
    x <- radius * cos(angle)
    y <- radius * sin(angle)
    z <- rep(layer * 1.5, points_per_layer)
    data <- rbind(data, data.frame(x = x, y = y, z = z, Layer = paste("Layer", layer)))
  }
  
  return(data)
}

generate_ornaments_on_tree <- function(tree_data, num_ornaments) {
  sampled_points <- tree_data[sample(nrow(tree_data), num_ornaments, replace = TRUE), ]
  sampled_points$color <- sample(c("red", "orange", "blue"), num_ornaments, replace = TRUE)
  return(sampled_points)
}

layers <- 7
points_per_layer <- 50
num_ornaments <- 100
tree_data <- generate_3d_tree_data(layers, points_per_layer)
ornaments <- generate_ornaments_on_tree(tree_data, num_ornaments)

star <- data.frame(
  x = 0,
  y = 0,
  z = max(tree_data$z) + 1.5,
  color = "gold"
)

plot_ly() %>%
  add_trace(
    data = tree_data,
    x = ~x, y = ~y, z = ~z,
    type = 'scatter3d',
    mode = 'lines',
    line = list(width = 5, color = "forestgreen"),
    showlegend = FALSE
  ) %>%
  add_trace(
    data = ornaments,
    x = ~x, y = ~y, z = ~z,
    type = 'scatter3d',
    mode = 'markers',
    marker = list(size = 5, color = ~color, opacity = 0.8),
    showlegend = FALSE
  ) %>%
  add_trace(
    data = star,
    x = ~x, y = ~y, z = ~z,
    type = 'scatter3d',
    mode = 'markers',
    marker = list(size = 10, color = ~color, symbol = 'star'),
    showlegend = FALSE
  ) %>%
  layout(
    title = list(text = "Wesołych Świąt!", y = 0.95),
    scene = list(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      zaxis = list(visible = FALSE),
      camera = list(eye = list(x = 1.5, y = 1.5, z = 2.5))
    )
  )