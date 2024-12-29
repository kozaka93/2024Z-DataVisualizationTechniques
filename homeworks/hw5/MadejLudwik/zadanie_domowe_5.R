library("ggplot2")
a <- c(1100, 1150)


x <- seq(800, 1000, length.out=11)
x <- rep(x, times=seq(31,1, length.out=11))

y <- seq(400, 700, length.out=11)
y <- rep(y, times=seq(51, 20, length.out=11))

z <- seq(-100, 300, length.out=11)
z <- rep(z, times=seq(80, 40, length.out=11))

wektor <- cbind(c(a, x, y, z))
wektor <- as.data.frame(wektor)
colnames(wektor) <- "kolumna"

ggplot(
  wektor,
  aes(x = "", y = kolumna)
  ) +
  geom_violin(fill = "darkgreen") +
  theme(
    axis.title = element_blank(),     
    axis.text = element_blank(),  
    axis.ticks = element_blank(), 
    panel.grid = element_blank(), 
    panel.border = element_blank(),
    panel.background = element_blank()
    )

  
  