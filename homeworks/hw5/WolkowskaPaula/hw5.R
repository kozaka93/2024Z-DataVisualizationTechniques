library(ggplot2)

choinka <- data.frame(
  x = c(-2.5, 2.5, 1.5, -1.5, -2.5,
        -2, 2, 1, -1, -2,
        -1.5, 1.5, 0.5, -0.5, -1.5,
        -1, 1, 0, -1),
  y = c(0.5, 0.5, 1.5, 1.5, 0.5,
        1.5, 1.5, 2.5, 2.5, 1.5,
        2.5, 2.5, 3.5, 3.5, 2.5,
        3.5, 3.5, 4.5, 3.5),
  level = c(1, 1, 1, 1, 1,
            2, 2, 2, 2, 2,
            3, 3, 3, 3, 3,
            4, 4, 4, 4)
  )

pien <- data.frame(
  x = c(-0.25, 0.25),
  y = c(0, 0.5)
)

bombki <- data.frame(
  x = c(-1.5 ,-1, 0.2, -0.1, -0.5, 0.1, 0.5, 1.1),
  y = c(1, 1.8, 1.3, 2.2, 3, 3.8, 3.1, 1.7),
  color = c("#B91F1C", "#D86558", "#EDBBA4","#D86558","#B91F1C", "#C30F16", "#EDBBA4", "#C30F16")
)

gwiazda <- data.frame(
  x = c(0, 0.3, 0.15, 0.35, 0.1, 0, -0.1, -0.35, -0.15, -0.3, 0),
  y = c(4.2, 4, 4.3, 4.4, 4.4, 4.7, 4.4, 4.4, 4.3, 4, 4.2)+0.28
)

set.seed(123)
girlanda <- data.frame(
  x = c(seq(-2, 2, length.out = 70),
        seq(-1.5, 1.5, length.out = 50),
        seq(-1, 1, length.out = 30)),
  y = rep(c(1.5, 2.5, 3.5), c(70,50,30))
)


ggplot() +
  geom_rect(aes(xmin = -3, xmax = 3, ymin = -0.5, ymax = 5), fill = "#ECDEB9") +
  geom_polygon(data=c,aes(x=x,y=y,group=level),fill="#355829")+
  geom_point(data=girlanda, aes(x=x, y=y), color="#caa906", size=4)+
  geom_rect(data = pien, aes(xmin = -0.25, xmax = 0.25, ymin = 0, ymax = 0.5), fill = "#63250e") +
  geom_point(data=bombki, aes(x=x,y=y,color=color),size=10)+
  geom_polygon(data=gwiazda, aes(x=x, y=y), fill="#caa906") +
  annotate("text", x = 0, y = 5.3, label = "Wesołych Świąt!", 
           size = 8, color = "#caa906", fontface = "bold") +
  coord_fixed() +
  theme_void() +
  scale_color_identity()
  

