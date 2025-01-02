library(dplyr)
library(ggplot2)

x_values <- c()
y_values <- c()

start <- 1

# Generowanie danych do wykresu
for (i in 1:26) {
  n_rows <- 100 - (i - 1) * 4
  
  x_segment <- seq(start, start + n_rows - 1)
  
  x_values <- c(x_values, x_segment)
  y_values <- c(y_values, rep(i, length(x_segment)))
  
  start <- start + 2
}

# Ramka danych
df <- data.frame(x = x_values, y = y_values)

n <- nrow(df)
df$star <- rep(FALSE, n)
df$star[(n-5):n] <- TRUE

# Kod wykresu
ggplot(data = df, aes(x = x, y = y, color = star)) +
  geom_point(size = 8, alpha = 0.3, shape = 18) +
  scale_color_manual(values = c("darkgreen", "gold")) +
  labs(title = "HO HO HO, Marry Christmas!") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "italic"),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# ggsave("choinka.png", plot = p, width = 5, height = 5, dpi = 300)