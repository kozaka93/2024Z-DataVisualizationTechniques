library(plotly)
# Kod tworzy z ograniczonych dwuwymiarowych rozkładów normalnych tworzy kolejne warstwy choinki i nakłada na nie punkty (bombki)

# Tworzenie siatki 
x <- seq(-3, 3, length.out = 800)
y <- seq(-3, 3, length.out = 800)
grid <- expand.grid(x = x, y = y)

# Parametry rozkładu i bombek
mu <- c(0, 0)
sigma_start <- 1.5  
radius_start <- 3.0  
levels <- 7
num_bombki <- 300  

# Funkcja dwuwymiarowego rozkładu normalnego
gaussssss <- function(x, y, mu, sigma, radius) {
  z <- exp(-((x - mu[1])^2 + (y - mu[2])^2) / (2 * sigma^2))
  distance <- sqrt((x - mu[1])^2 + (y - mu[2])^2)
  z[distance > radius] <- NA
  return(z)
}

# Generowanie wszystkich warstw z bombkami
z_layers <- list()
bombki <- list()
for (i in seq_len(levels)) {
  sigma <- sigma_start * (1 - i / (levels + 1))  
  radius <- radius_start * (1 - i / (levels + 1))  
  z_layers[[i]] <- matrix(gaussssss(grid$x, grid$y, mu, sigma, radius),nrow = length(x), ncol = length(y)) + i 
  
  # Tworzenie bombek (x i y)
  bombki[[i]] <- data.frame(
    x = runif(num_bombki, -radius, radius),
    y = runif(num_bombki, -radius, radius))
  num_bombki <- num_bombki - 46
  
  # Obliczanie wartości z dla każdej bombki
  bombki[[i]]$z <- mapply(function(xb, yb) {
    value <- gaussssss(xb, yb, mu, sigma, radius)
    return(value + i)
  }, bombki[[i]]$x, bombki[[i]]$y)
  
  # Przypisanie koloru
  bombki[[i]]$color <- sample(c("white", "red"), nrow(bombki[[i]]), replace = TRUE)
}

fig <- plot_ly(showscale = FALSE)

for (i in 1:length(z_layers)) {
  fig <- add_surface(fig, z = z_layers[[i]], x = x, y = y,
                     colorscale = list(c(0, 1), c("darkgreen", "green")))
  # Dodanie bombek
  fig <- add_markers(fig, x = bombki[[i]]$x,
                             y = bombki[[i]]$y,
                             z = bombki[[i]]$z,
                             marker = list(size = 3, color = bombki[[i]]$color))
}

fig <- layout( fig, title = "",
  scene = list(
    xaxis = list(title = "",showgrid = FALSE,zeroline = FALSE,showline = FALSE, showticklabels = FALSE),
    yaxis = list(title = "",showgrid = FALSE,zeroline = FALSE,showline = FALSE, showticklabels = FALSE),
    zaxis = list(title = "",showgrid = FALSE,zeroline = FALSE,showline = FALSE, showticklabels = FALSE, backgroundcolor = "brown",showbackground = TRUE),
    bgcolor = "#8AC0E0"
  )
)
fig


