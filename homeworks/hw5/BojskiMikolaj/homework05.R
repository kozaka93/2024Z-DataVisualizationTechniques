library(plotly)

a <- seq(0, 150, by = 0.7)^0.6

without_axis <- list(title = "", zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE)

p <- plot_ly(x = a * c(sin(a), sin(a + pi)), y = a * c(cos(a), cos(a + pi)), z = -2 * c(a/10, a/10),
        type = "scatter3d", marker = list(size = 3), mode="markers", color = rep(c(T, F), each = length(a)), 
        colors = c("darkgreen", "yellow3")) |>
  layout(paper_bgcolor = 'black', showlegend = F, 
         scene = list(xaxis = without_axis, yaxis = without_axis, zaxis = without_axis))

p

htmlwidgets::saveWidget(p, file = "hw5.html")
