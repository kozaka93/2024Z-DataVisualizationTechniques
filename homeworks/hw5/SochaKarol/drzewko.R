library(ggplot2)
library(dplyr)

set.seed(19)

d10 <- data.frame(val = rnorm(10000))
d9 <- data.frame(val = rnorm(9000))
d8 <- data.frame(val = rnorm(8000))
d7 <- data.frame(val = rnorm(7000))
d6 <- data.frame(val = rnorm(6000))
d5 <- data.frame(val = rnorm(5000))
d4 <- data.frame(val = rnorm(4000))
d3 <- data.frame(val = rnorm(3000))
d2 <- data.frame(val = rnorm(2000))
d1 <- data.frame(val = rnorm(1000))

star <- data.frame(x = 0, y = 421)

tree <- ggplot(d10, aes(x = val)) + 
  geom_histogram(d10, mapping = aes(x = val), breaks = seq(-3, 3, by = 0.1), 
                 fill = "#09622A", color = "#B19429") +
  geom_histogram(d9, mapping = aes(x = val), breaks = seq(-3, 3, by = 0.1), 
                 fill = "#0D652C", color = "#A6ACAD") +
  geom_histogram(d8, mapping = aes(x = val), breaks = seq(-3, 3, by = 0.1), 
                 fill = "#11692D", color = "#B19429") +
  geom_histogram(d7, mapping = aes(x = val), breaks = seq(-3, 3, by = 0.1), 
                 fill = "#146C2F", color = "#A6ACAD") +
  geom_histogram(d6, mapping = aes(x = val), breaks = seq(-3, 3, by = 0.1), 
                 fill = "#176F31", color = "#B19429") +
  geom_histogram(d5, mapping = aes(x = val), breaks = seq(-3, 3, by = 0.1), 
                 fill = "#1A7332", color = "#A6ACAD") +
  geom_histogram(d4, mapping = aes(x = val), breaks = seq(-3, 3, by = 0.1), 
                 fill = "#1D7634", color = "#B19429") +
  geom_histogram(d3, mapping = aes(x = val), breaks = seq(-3, 3, by = 0.1), 
                 fill = "#207936", color = "#A6ACAD") +
  geom_histogram(d2, mapping = aes(x = val), breaks = seq(-3, 3, by = 0.1), 
                 fill = "#237C38", color = "#B19429") +
  geom_histogram(d1, mapping = aes(x = val), breaks = seq(-3, 3, by = 0.1), 
                 fill = "#25803A", color = "#A6ACAD") +
  geom_point(data = star, aes(x = x, y = y), shape = "*", size = 33, color = "gold3") +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    panel.background = element_rect(fill = "midnightblue", color = "midnightblue"), 
    plot.background = element_rect(fill = "midnightblue", color = "midnightblue"),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"), 
    legend.text = element_blank(),      
    legend.title = element_blank(),     
    axis.text = element_blank(),       
    axis.title = element_blank(),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()
  )
tree
