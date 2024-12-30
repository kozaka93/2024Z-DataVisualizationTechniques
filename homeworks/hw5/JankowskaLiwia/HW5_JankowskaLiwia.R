library(dplyr)
library(ggplot2)
library(plotly)

v1 = c("a", "b", "c", "d", "e")
v1_500 <- rep(v1, length.out = 500)
v2 <- c(100, 75, 75, 50, 50, 50, 25, 25, 25, 25, 25, 0, 0, 0, 0, 0, 0, 0)
v2_500 <- sort(rep(v2, length.out = 500))

v1_500
v2_500
View(airquality)

df <- data.frame(
  id = v1_500,
  val = v2_500
) 

View(df)

df %>% 
  ggplot() +
  geom_violin(aes(x = val, y = id)) +
  coord_flip()
