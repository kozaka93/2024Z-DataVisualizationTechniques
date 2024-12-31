library(plotly)



#Tworzymy punkty rysowania choinki
z <- seq(0, 200, by = 0.1)
x <- (101 - z*0.5) * cos(z)
y <- (101 - z*0.5) * sin(z)

tree_data <- data.frame(x = x, y = y, z = z)

set.seed(50)
#Losujemy punkty na bombki
ornaments <- tree_data[sample(1:nrow(tree_data), size = nrow(tree_data) * 0.1), ]
ornaments$color <- sample(c("red", "yellow", "blue", "pink", "purple"), nrow(ornaments), replace = TRUE)

#Tworzymy wykres
fig <- plot_ly()

# Dodajemy punkty choinki
fig <- fig %>%
  add_markers(
    data = tree_data, 
    x = ~x, y = ~y, z = ~z, 
    marker = list(size = 2, color = "green"), 
    name = "Tree"
  )

# Dodajemy bombki
fig <- fig %>%
  add_markers(
    data = ornaments, 
    x = ~x, y = ~y, z = ~z, 
    marker = list(size = 5, color = ~color), 
    name = "Ornaments"
  )

# Dodajemy czubek choinki
fig <- fig %>%
  add_markers(
    x = 0, y = 0, z = max(tree_data$z) + 5,
    marker = list(size = 10, color = "gold", symbol = "diamond"),
    name = "Star"
  )

# Dodajemy opis wykresu
fig <- fig %>%
  layout(
    scene = list(
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      zaxis = list(title = "")
    ),
    title = "3D Christmas Tree",
    showlegend = FALSE
  )

#
fig
