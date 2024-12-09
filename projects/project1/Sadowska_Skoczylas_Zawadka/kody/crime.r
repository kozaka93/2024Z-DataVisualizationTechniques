library(dplyr)
library(readxl)
library(ggplot2)
library(igraph)
library(scales)
library(extrafont)
loadfonts(device = "win")

crime_dt <- read_excel("Transnational_Crime2.xlsx", sheet = "Organs") %>% select(-Country)

# boxplot price of organs
price_plot <- ggplot(crime_dt, aes(x = Organ, y = Price)) +
  geom_boxplot(fill = "#aecfee", color = "white") +
  scale_y_continuous(labels = label_dollar(), limits = c(0, NA)) +
  labs(
    x = "Organ",
    y = "Price"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, color = "white"),
        axis.title = element_text(color = "white", size = 14, family = "Calibri"),
        axis.text = element_text(color = "white", size = 14, family = "Calibri"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
price_plot
ggsave("price_plot.png", price_plot, width = 8, height = 6)

#### grafy

crime_dt2 <- read_excel("Transnational_Crime2.xlsx", sheet = "Vendors vs Recipients")
crime_dt2 <- crime_dt2 %>% mutate(markup = Recipient_Paid-Vendor_Received)

edges <- data.frame(from = crime_dt2$Vendor_From, to = crime_dt2$Recipient_From)

g <- graph_from_data_frame(d = edges, directed = TRUE)

# Dodanie wartości markup jako atrybutu krawędzi
# E(g)$label <- crime_dt2$markup
# E(g)$label.cex <- 0.8
# E(g)$label.color <- "black"

x_coords <- c(4, 6, 2, 2, 4, 3, 6.0, 4.5, 0, 0, 6, 1)
y_coords <- c(6, 5, 6, 1, 1, 4, 3.5, 2.5, 4, 2, 2, 3)

custom_layout <- cbind(x_coords, y_coords)

plot(
  g,
  vertex.size = 33,
  vertex.label.cex = 0.7,
  vertex.label.color = "black",
  edge.arrow.size = 0.5,
  edge.color = "darkgrey",
  vertex.color = "lightblue",
  edge.loop.angle = pi/2,
  layout = custom_layout
  # edge.label = E(g)$label,
  # edge.label.cex = E(g)$label.cex,
  # edge.label.color = E(g)$label.color
)
title("Kidney Crime Map")



png("kidney_crime_map.png", bg = "transparent", width = 2600, height = 2600, res = 450)

plot(
  g,
  vertex.size = 33,
  vertex.label.cex = 0.7,
  vertex.label.color = "black",
  edge.arrow.size = 0.5,
  edge.color = "white",
  vertex.color = "#aecfee",
  vertex.frame.color = "#aecfee",
  edge.loop.angle = pi/2,
  layout = custom_layout,
  vertex.label.family = "Calibri"
)

dev.off()

