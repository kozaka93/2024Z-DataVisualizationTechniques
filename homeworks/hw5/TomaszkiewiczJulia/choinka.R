set.seed(677)

n_layers <- 8
max_width <- 6
tree_height <- n_layers * 1.2

plot(NA, xlim = c(-max_width, max_width), ylim = c(-2, tree_height + 2), xlab = "", ylab = "", axes = FALSE, main = "Ho ho ho")

rect(-max_width, -2, max_width, tree_height + 2, col = "skyblue", border = NA)

n_snowflakes <- 200

for (i in 1:n_snowflakes) {
  x_snow <- runif(1, -max_width, max_width)
  y_snow <- runif(1, -2, tree_height + 3)
  points(x_snow, y_snow, col = "white", pch = 16, cex = 0.8)
}

for (i in 1:n_layers) {
  width <- max_width * (1.1 - (i - 1) / (n_layers - 1))
  
  x_triangle <- c(-width / 2, 0, width / 2)
  y_triangle <- c(i - 0.5, i + 1, i - 0.5)
  
  polygon(x_triangle, y_triangle, col = "forestgreen", border = "forestgreen")
}

rect(-0.5, -0.5, 0.5, 0.5, col = "brown", border = "brown")

points(0, n_layers + 1, col = "yellow", pch = 8, cex = 3, lwd = 3)

for (i in 1:n_layers) {
  width_at_layer <- max_width * (1 - (i - 1) / (n_layers - 1))
  
  n_balls_on_layer <- round(width_at_layer * 2)
  
  for (j in 1:n_balls_on_layer) {
    x_pos <- -width_at_layer / 2 + (j - 1) * (width_at_layer / (n_balls_on_layer - 1))
    y_pos <- i + runif(1, -0.5, 0.5)
    
    points(x_pos, y_pos, col = sample(c("red", "gold", "pink"), 1), pch = 16, cex = 1.5)
  }
}

rect(-max_width, -4, max_width, -0.5, col = "white", border = "white")
