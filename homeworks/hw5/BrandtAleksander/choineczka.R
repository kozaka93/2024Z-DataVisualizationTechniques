library(ggplot2)

generate_section <- function(y_start, height, width) {
  branches <- data.frame(
    x = unlist(lapply(seq(width, 0.1, length.out = height), function(w) runif(50, -w, w))),
    y = rep(seq(y_start, y_start + height * 0.1, length.out = height), each = 50),
    size = runif(50 * height, 0.02, 0.08),
    color = sample(c("darkgreen", "forestgreen", "green4", "seagreen"), 50 * height, replace = TRUE)
  )
  
  ornaments <- data.frame(
    x = unlist(lapply(seq(width, 0.1, length.out = height), function(w) runif(7, -w, w))),
    y = rep(seq(y_start, y_start + height * 0.1, length.out = height), each = 7),
    size = runif(7 * height, 0.03, 0.08),
    color = sample(c("red", "gold", "blue", "orange", "purple"), 7 * height, replace = TRUE)
  )
  
  list(branches = branches, ornaments = ornaments)
}

generate_tree_data <- function() {
  section1 <- generate_section(y_start = 0.2, height = 10, width = 1.5) 
  section2 <- generate_section(y_start = 1.2, height = 8, width = 1.0) 
  section3 <- generate_section(y_start = 2.0, height = 6, width = 0.5) 
  branches <- rbind(section1$branches, section2$branches, section3$branches)
  ornaments <- rbind(section1$ornaments, section2$ornaments, section3$ornaments)
  
  star <- data.frame(
    x = 0,
    y = 2.7,
    size = 1.0,
    color = "yellow"
  )
  
  trunk <- data.frame(
    xmin = -0.1,
    xmax = 0.1,
    ymin = 0,
    ymax = 0.2
  )
  
  list(branches = branches, ornaments = ornaments, star = star, trunk = trunk)
}

draw_tree <- function(tree_data) {
  ggplot() +
    theme(panel.background = element_rect(fill = "gray20", color = "gray20")) +
    geom_point(data = tree_data$branches, aes(x = x, y = y, size = size, color = color), shape = 17) +
    geom_point(data = tree_data$ornaments, aes(x = x, y = y, size = size, color = color), shape = 16) +
    geom_point(data = tree_data$star, aes(x = x, y = y, size = size), shape = 8, color = "yellow") +
    geom_rect(data = tree_data$trunk, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "saddlebrown") +
    theme_void() +
    theme(legend.position = "none") +
    scale_size_continuous(range = c(1, 6)) +
    scale_color_identity() +
    coord_fixed()
}

data <- generate_tree_data()
choinka <- draw_tree(data)
print(choinka)

ggsave("choineczka.png", plot = choinka, width = 6, height = 8)


