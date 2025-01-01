library(ggplot2)

rysuj_choinke <- function() {
  trunk_x <- c(-0.3, 0.3, 0.3, -0.3)
  trunk_y <- c(-5, -5, -6, -6)
  branches <- list(
    data.frame(x = c(0, -1, 1, 0), y = c(-3.5, -5, -5, -3.5)),
    data.frame(x = c(0, -0.6, 0.6, 0), y = c(-2.45, -3.95, -3.95, -2.45)),
    data.frame(x = c(0, -0.3, 0.3, 0), y = c(-1.5, -3, -3, -1.5))
  )
  set.seed(67)
  ornaments_x <- runif(30, min = -1.5, max = 1.5)
  ornaments_y <- runif(30, min = -5, max = -2)
  plot <- ggplot() +
    geom_polygon(aes(x = trunk_x, y = trunk_y), fill = "brown") +
    lapply(branches, function(branch) {
      geom_polygon(data = branch, aes(x = x, y = y), fill = "darkgreen")
    }) +
    geom_point(aes(x = ornaments_x, y = ornaments_y), color = "white", size = 2) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#2c3e50"),
      panel.background = element_rect(fill = "#2c3e50")
    ) +
    ggtitle("Moja Choinka")
  ggsave("choinka.png", plot = plot, width = 8, height = 10, dpi = 300)
}
rysuj_choinke()
                                                                                                                        