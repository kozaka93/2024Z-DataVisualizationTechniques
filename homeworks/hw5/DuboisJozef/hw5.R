library(dplyr)
library(ggplot2)
library(ggridges)
library(tidyr)


drzewo <- tibble(rnorm(1000, 0, 1), rnorm(1000, 0, 2), rnorm(1000, 0, 3),
                 rnorm(1000, 0, 4), rnorm(1000, 0, 5), rnorm(1000, 0, 6), 
                 rnorm(1000, 0, 7), rnorm(1000, 0, 8), rnorm(1000, 0, 9), 
                 rnorm(1000, 0, 10), rnorm(1000, 0, 11),runif(1000, -1, 1))

colnames(drzewo) <- c("zgalez","ygalez", "xgalez", "wgalez" ,"vgalez", "ugalez", "qgalez", "pgalez", "ogalez", "ngalez", "mgalez","lpien")


drzewo_long <- pivot_longer(drzewo, cols = c("zgalez","ygalez", "xgalez", "wgalez" ,"vgalez", "ugalez", "qgalez", "pgalez", "ogalez", "ngalez", "mgalez","lpien"))

drzewo_long <- drzewo_long %>%
  mutate(name = factor(name, levels = rev(c("zgalez","ygalez", "xgalez", "wgalez" ,"vgalez", "ugalez", "qgalez", "pgalez", "ogalez", "ngalez", "mgalez","lpien"))))
                       
custom_colors <- c(
  "lpien" = "#8B4513",  
  "mgalez" = "#228B22", 
  "ngalez" = "#32CD32",
  "ogalez" = "#228B22", 
  "pgalez" = "#32CD32",
  "qgalez" = "#228B22", 
  "ugalez" = "#32CD32",
  "vgalez" = "#228B22", 
  "wgalez" = "#32CD32",
  "xgalez" = "#228B22", 
  "ygalez" = "#32CD32",
  "zgalez" = "#228B22"
)


obraz_choinki <- ggplot(drzewo_long, aes(x = value, y = name, fill = name)) +
  geom_density_ridges(rel_min_height = 0.01, panel_scaling = TRUE, scale = 3) +
  theme_ridges() + 
  labs(
    title = " ",
    x = " ",
    y = " "
  ) +
  
  theme_void() +
  scale_fill_manual(values = custom_colors) +
  theme(legend.position = "none")


ggsave(
  filename = "choinka.png",
  plot = obraz_choinki,                       
  width = 8,
  height = 6,
  dpi = 300
)
