library(ggplot2)
library(gganimate)
library(gifski)

nSteps <- 40      
N      <- 2000    

create_data_for_step <- function(i) {
  set.seed(123 + i)
  alpha_i <- 2 + 0.7 * sin(i * pi / 10) 
  beta_i  <- 5 + 0.7 * cos(i * pi / 12)  
  
  y_vals <- rbeta(N, shape1 = alpha_i, shape2 = beta_i) * 10
  data.frame(
    step = i,
    y    = y_vals
  )
}

all_data <- do.call(rbind, lapply(seq_len(nSteps), create_data_for_step))
all_data$x <- "Choinka"

p <- ggplot(all_data, aes(x = x, y = y)) +

  annotate("rect",
           xmin = 0.95, xmax = 1.05,
           ymin = -1.5, ymax = 0,    
           fill = "saddlebrown") +

  geom_violin(
    fill  = "forestgreen",
    color = "black",
    alpha = 0.9,
    scale = "width"
  ) +
  transition_states(step, transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out") +
  coord_cartesian(ylim = c(-1.5, 10.5)) +
  labs(
    title    = "Falująca skrzypcowa choinka",
    x        = "",
    y        = "Wysokość (wartość)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x  = element_blank(), 
    axis.ticks.x = element_blank(),
    plot.title   = element_text(face = "bold", hjust = 0.5)
  )

anim <- animate(
  p,
  fps       = 10, 
  duration  = 10,   
  width     = 600,
  height    = 600,
  renderer  = gifski_renderer(loop = TRUE)
)

anim_save("choinka_skrzypcowa_falujaca.gif", animation = anim)
