library(ggplot2)

dataset <- c(10, 9,8,7,6,5,4,3,2,1,0.5,0.2)

df <- data.frame(group = rep(c("l","d"), 
                             each=length(dataset)),
                 x = 1:length(dataset),
                 y = c(dataset, dataset*-1))

ggplot() +
  geom_bar(data = df, aes(x=x, y=y),stat = "identity", fill = '#00A650',width=.8) +
  geom_point(aes(x=6, y=-0.5), colour="#CF140D", size=6) +
  geom_point(aes(x=5, y=4), colour="#393762", size=8) +
  geom_point(aes(x=7, y=3), colour="#CF140D", size=8) +
  geom_point(aes(x=2, y=3.5), colour="#393762", size=6) +
  geom_point(aes(x=10, y=-0.1), colour="#CF140D", size=6) +
  geom_point(aes(x=4, y=3), colour="#393762", size=8) +
  geom_point(aes(x=8, y=2), colour="#CF140D", size=8) +
  geom_point(aes(x=3, y=1), colour="#393762", size=6) +
  geom_point(aes(x=2, y=-5), colour="#393762", size=6) +
  geom_point(aes(x=8, y=-0.5), colour="#393762", size=6) +
  geom_point(aes(x=12, y=0), shape=8, fill="yellow", color="gold", size=13) +
  geom_point(aes(x=4, y=-2.8), colour="#CF140D", size=12) + ggtitle("Merry Christmas!!!") + theme_void() +
  coord_flip()
