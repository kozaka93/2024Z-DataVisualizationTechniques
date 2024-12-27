#install.packages("rgl)
#install.packages("magick")
library(rgl)
library(magick)

# Parametry choinki - spirali
t <- seq(0, 15 * pi, by = 0.1)
x <- sin(t) * t / 10
y <- cos(t) * t / 10
z <- -t / 5

color_tree <- rep("darkgreen", length(t))

color_lights <- rep(c("yellow","red","orange"), length(t))

# Przesunięcie w górę dla światełek
offset <- 0.1 

open3d(windowRect = c(100, 100, 500, 500), zoom = 0.9)
bg3d("darkblue")  

# Rysowanie zielonej linii gałęzi choinki
for (i in 1:(length(t)-1)) {
  lines3d(c(x[i], x[i+1]), c(y[i], y[i+1]), c(z[i], z[i+1]), color = "darkgreen", lwd = 6)
}

# Światełka
spheres3d(x, y, z + offset, radius = 0.05, color = color_lights)

text3d(x = 0, y = 0, z = max(z) + 1, texts = "Merry Christmas!", cex = 1.5, col = "white")

# Do uruchomienia animacji bez zapisu
#play3d(spin3d(axis = c(0, 0, 1), rpm = 5))  

# Do zapisania animacji
movie3d(
  spin3d(axis = c(0, 0, 1), rpm = 3),
  duration = 10,  
  movie = "choina",            
  type = "gif"
)
gif <- image_read("choina.gif")
gif <- image_animate(gif, fps = 25, loop = 0)
image_write(gif, "choinka.gif")
