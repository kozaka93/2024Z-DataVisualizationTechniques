library(ggplot2)
library(dplyr)

draw_christmas_tree <- function(output_file = "choinka.png") {
    trunk <- data.frame(
        x = c(4, 6, 6, 4),
        y = c(0, 0, 2, 2)
    )
    
    layers <- list(
        data.frame(x = c(1, 5, 9), y = c(2, 6, 2)),
        data.frame(x = c(2, 5, 8), y = c(6, 8.5, 6)),
        data.frame(x = c(3, 5, 7), y = c(8.5, 10, 8.5))
    )
    
    baubles <- data.frame(
        x = c(5, 3.5, 6.5, 4.5, 6, 5.2, 3.2, 5.8, 4.5, 6.5, 5, 4.6, 5.4, 3.7, 6.3, 4.4, 5.6, 5.1),
        y = c(2.5, 3.5, 3.5, 4.2, 4.7, 5.3, 6.6, 6.1, 6.4, 6.9, 7.2, 8.6, 8.9, 8.8, 8.9, 9.5, 9.4, 9.7),
        color = c("red", "gold", "blue", "red", "gold", "blue", "red", "gold", "blue", "red", "gold", "blue", "gold", "blue", "red", "gold", "blue", "red")
    )
    
    star <- data.frame(
        x = 5,
        y = 10.5
    )
    
    p <- ggplot() +
        geom_polygon(data = trunk, aes(x = x, y = y), fill = "saddlebrown") +
        lapply(layers, function(layer) {
            geom_polygon(data = layer, aes(x = x, y = y), fill = "forestgreen")
        }) +
        geom_point(data = baubles, aes(x = x, y = y, color = color), size = 3) +
        scale_color_manual(values = c("red" = "red", "gold" = "gold", "blue" = "blue")) +
        geom_point(data = star, aes(x = x, y = y), color = "gold", size = 5, shape = 8) +
        coord_fixed() +
        theme_void() +
        theme(legend.position = "none")
    
    print(p)
    ggsave(output_file, plot = p, width = 6, height = 10, dpi = 300)
}

draw_christmas_tree()
