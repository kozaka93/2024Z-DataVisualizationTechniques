library(ggplot2)
library(patchwork)


df <- data.frame(
  layer = 1:12,
  value = c(20, seq(from = 100, to = 15, length.out = 10), 8),
  color = c("#8B4513", rep("#228B22", 10), "gold")
)

p1 <- ggplot(data = df, aes(x = layer, y = value, fill = color)) +
  geom_col() +
  scale_fill_identity()+
  coord_flip() +
  theme_void()

p2 <- p1 + scale_y_reverse()

tree = p2 + p1 + plot_annotation(title = "wesołych świąt!",
                                 theme=theme(plot.title=element_text(hjust=0.5)))
tree
