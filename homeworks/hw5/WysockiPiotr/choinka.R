library(ggplot2)

stripped <- function(x1,x2, a,b, n = 10000, point = 200){
  side_x <- seq(x1,x2, length.out = n)
  side_y <- a*side_x + b
  points_x <- sort(sample(side_x, point, replace = FALSE))
  p <- 1
  result <- data.frame(x = numeric(), y = numeric(), color = numeric())
  for (i in seq_along(side_x)){
    result <- rbind(result, c(x = side_x[i], y = side_y[i], color = 1))
    if (p <= length(points_x) && side_x[i] == points_x[p]){
      p <- p + 1
      if (a<0)
      result <- rbind(result, c(x = runif(1, side_x[i], side_x[i] + 0.02), y = side_y[i], color = 0.5))
      else
      result <- rbind(result, c(x = runif(1, side_x[i] - 0.02, side_x[i]), y = side_y[i], color = 0.5))
    }
  }
  result
}
left <- stripped(0, 0.2, 5, 0)
right <- stripped(0.2, 0.4, -5, 2)
colnames(left) <- c("x","y", "color")
colnames(right) <- c("x","y", "color")
bottom <- data.frame(x = seq(0,0.4,length.out = 100), y = 0, color = 0.5)
dane <- rbind(left, right, bottom %>% arrange(desc(x)))

edge_points <- dane %>% filter(color == 0.5)

pieniek <- data.frame(x = c(0.15,0.25,0.25,0.15), y = c(0,0,-0.1,-0.1))

generate_star <- function(radius1, radius2, arms = 10) {
  angles <- seq(0, 2 * pi, length.out = arms + 1)
  x <- c()
  y <- c()
  
  for (i in seq(arms)) {
    x <- c(x, cos(angles[i]) * radius1, cos(angles[i + 1] - pi / arms) * radius2)
    y <- c(y, sin(angles[i]) * radius1, sin(angles[i + 1] - pi / arms) * radius2)
  }
  
  data.frame(x = x, y = y)
}

star_data <- generate_star(0.1, 0.075)

star_data$x <- star_data$x + 0.2
star_data$y <- star_data$y + 1

promyki_data <- data.frame(
  x = star_data$x,
  y = star_data$y,
  xend = 0.2 + (star_data$x - 0.2) * 2,
  yend = 1 + (star_data$y - 1) * 2
)

generate_lancuch <- function(x, y, xend, yend, amp = 0.01, freq = 30, points = 100){
  t <- seq(0, 1, length.out = points)
  x_pos <- x + t * (xend - x)
  y_pos <- y + t* (yend - y)
  angle <- atan2(yend - y, xend - x) 
  x_offsets <- -sin(angle) * sin(seq(0, freq * pi, length.out = points)) * amp
  y_offsets <- cos(angle) * sin(seq(0, freq * pi, length.out = points)) * amp
  data.frame(
    x = x_pos + x_offsets,
    y = y_pos + y_offsets
  )
}

generate_lancuchy <- function(n = 8){
  x <- 0.4
  y <- 0
  xend <- 0.02
  yend <- 0.1
  freq <- 30
  d <- data.frame(x = numeric(), y = numeric())
  for (i in 1:n){
    d <- rbind(d, c(x = generate_lancuch(x,y,xend,yend,freq = freq)))
    x <- xend
    y <- yend
    yend <- yend + 0.1
    if (i %% 2 == 0)
      xend <- 0.42 - xend 
    else
      xend <- 0.38 - xend
    if (i >=4)
      freq <- 20
    if (i >=7)
      freq = 10
  }
  d
}

lancuch <- generate_lancuchy()
colnames(lancuch) <- c("x","y")

generate_bombki <- function(x1,x2,y1,y2, n = 50){
  u <- runif(n)
  v <- runif(n)
  df <- data.frame(x = numeric(), y = numeric(), color = character())
  for (i in seq_along(u)){
    x <- u[i] * (x2 - x1)
    y <- v[i] * (y2 - y1)
    y_p1 <- -5 * x + 2
    y_p2 <- 5 * x
    if (y < y_p1 && y < y_p2){
      tmp <- runif(1)
      if (tmp < 0.33)
        df <- rbind(df, c(x = x, y = y, color = "#ffd700"))
      else if(tmp < 0.66)
        df <- rbind(df, c(x = x, y = y, color = "#800020"))
      else
        df <- rbind(df, c(x = x, y= y, color = "#F1F1F1"))
    }
      
  }
  df
}

bombki <- generate_bombki(0,0.4,0,1)
colnames(bombki) <- c("x","y","color")
bombki$x <- as.numeric(bombki$x)
bombki$y <- as.numeric(bombki$y)
bombki$color <- as.factor(bombki$color)

ggplot() +
  geom_polygon(data = edge_points, aes(x = x, y = y), fill = "darkgreen", color = "darkgreen") +
  coord_equal(xlim = c(-1.12, 1.52), ylim = c(-0.2,1.2)) +
  geom_polygon(data = pieniek, aes(x = x, y = y), fill = "saddlebrown") +
  theme_void() +
  theme(panel.background = element_rect(fill = "midnightblue"),
        plot.background = element_rect(fill = "midnightblue"))+
  geom_polygon(data = star_data, aes(x = x, y = y), fill = "goldenrod", color = "black") +
  geom_segment(data = promyki_data, aes(x = x, y = y, xend = xend, yend = yend), color = "lightgoldenrod", size = 1) +
  geom_path(data = lancuch, aes(x = x, y = y), color = "#DCDCDC", size = 1) +
  geom_point(data = bombki, aes(x = x, y = y, color = color)) +
  scale_color_identity()




