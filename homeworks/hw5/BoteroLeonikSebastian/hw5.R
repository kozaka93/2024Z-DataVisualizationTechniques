library(plotly)
library(dplyr)

a = 10:2000
z <- -log(a)

df <- data_frame(a = a, z = z, x = 0, y = 0)|>
  mutate(x = ifelse(a%%2==0,
                    yes = 0.3*abs(z-max(z))*cos(1000*z),
                    no = 0),
         y = ifelse(a%%2==0,
                    yes = 0.3*abs(z-max(z))*sin(1000*z),
                    no = 0),
         z = ifelse(a%%2==0,
                    yes = z,
                    no = z + 0.6)) |>
  mutate(z = z - max(z))

f1 <- plot_ly(
  df,
  x = ~x,
  y = ~y,
  z = ~z,
  type = 'scatter3d',
  mode = 'lines',
  line = list(
    color = "#006c32"
  )
)


f2 <- plot_ly() |>
  add_markers(
  x = 0,
  y = 0,
  z = 0,
  # symbols = c('circle', 'cross', 'diamond', 'square', 'circle'),
  marker = list(size = 25,
                symbol = "cross",
                line = list(color = 'yellow', width = 5)),
  type = 'scatter3d',
  mode = "markers"
)

f3 <- df |>
  filter(a %% 10 == 0) |>
  mutate(color = factor(a %% 50)) |>
  plot_ly(
    x = ~x,
    y = ~y,
    z = ~z,
    type = 'scatter3d',
    mode = "markers",
  marker = list(size = 7),
    color = ~color,
    colors = "Set1"
  )


subplot(f1, f2, f3) |>
  layout(showlegend = FALSE)
