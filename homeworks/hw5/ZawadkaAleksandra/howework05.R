library(igraph)
library(magick)

layers <- 25
available_colors <- c("red", "gold", "blue", "white")

nodes <- data.frame(
  id = 1:sum(1:layers),
  layer = rep(1:layers, times = 1:layers),
  x = unlist(lapply(1:layers, function(l) seq(-l + 1, l - 1, length.out = l))),
  y = rep(1:layers, times = 1:layers) * -2
)

edges <- data.frame(from = integer(0), to = integer(0))
for (l in 1:(layers - 1)) {
  start_index <- sum(1:(l-1)) + 1
  end_index <- sum(1:l)
  next_start_index <- end_index + 1
  next_end_index <- sum(1:(l + 1))
  new_edges <- expand.grid(from = start_index:end_index, to = next_start_index:next_end_index)
  new_edges <- new_edges[new_edges$from != new_edges$to, ]
  edges <- rbind(edges, new_edges)
}

x_coords <- unlist(lapply(1:layers, function(l) seq(-l + 1, l - 1, length.out = l)))
y_coords <- rep(1:layers, times = 1:layers) * -2
library(igraph)
library(magick)

layers <- 25
available_colors <- c("red", "gold", "blue", "white")

nodes <- data.frame(
  id = 1:sum(1:layers),
  layer = rep(1:layers, times = 1:layers),
  x = unlist(lapply(1:layers, function(l) seq(-l + 1, l - 1, length.out = l))),
  y = rep(1:layers, times = 1:layers) * -2
)

edges <- data.frame(from = integer(0), to = integer(0))
for (l in 1:(layers - 1)) {
  start_index <- sum(1:(l-1)) + 1
  end_index <- sum(1:l)
  next_start_index <- end_index + 1
  next_end_index <- sum(1:(l + 1))
  new_edges <- expand.grid(from = start_index:end_index, to = next_start_index:next_end_index)
  new_edges <- new_edges[new_edges$from != new_edges$to, ]
  edges <- rbind(edges, new_edges)
}

g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

x_coords <- unlist(lapply(1:layers, function(l) seq(-l + 1, l - 1, length.out = l)))
y_coords <- rep(1:layers, times = 1:layers) * -2
custom_layout <- cbind(x_coords, y_coords)

frames <- list()

for (i in 1:6) {
  set.seed(i)
  nodes$color <- sample(available_colors, size = nrow(nodes), replace = TRUE) 
  plot_file <- paste0("frame_", i, ".png")
  png(plot_file, width = 800, height = 800)
  plot(
    g,
    vertex.size = 3.5,
    vertex.label = NA,
    edge.color = "darkgreen",
    vertex.color = nodes$color,
    layout = custom_layout
  )
  dev.off()
  frames[[i]] <- image_read(plot_file)
}

gif_file <- "choinka.gif"
animation <- image_animate(image_join(frames), fps = 2)

image_write(animation, gif_file)

file.remove(paste0("frame_", 1:6, ".png"))