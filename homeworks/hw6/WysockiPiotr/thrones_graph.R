library(igraph)
library(ggrepel)
data <-
  read.csv("C:/Users/Admin/Downloads/got-edges.csv", stringsAsFactors = FALSE)
g <- graph_from_data_frame(data)
font_add_google(name = "Daenerys", family = "Daenerys")
communities <- cluster_walktrap(g)
V(g)$community <- membership(communities)
V(g)$degree <- case_when(degree(g) < 5 ~ 1.2, degree(g) < 10 ~ 1.5,
                         degree(g) < 18 ~ 2, .default = 2.5)
E(g)$width <- data$Weight
par(
  mfrow = c(1, 1),
  mar = c(0.1, 0, 0.9, 0.1),
  oma = c(0.1, 0, 0.1, 0.1),
  xaxs = "i",
  yaxs = "i",
  bg = "white"
)
custom_palette <- c("#4FC3F7", "#FFD34F", "#B39DDB", "#FF8A65")
V(g)$color <- custom_palette[V(g)$community]
E(g)$color <- V(g)[ends(g, E(g))[, 1]]$color
V(g)$label.dist <- -V(g)$degree / 3
layout <- layout_with_dh(g)
layout[which(V(g)$name == "Sansa"), ] <- c(-10,10)
layout[which(V(g)$name == "Cersei"), ] <- c(-1,30)
layout[which(V(g)$name == "Gregor"), ] <- c(-9.5,26)
layout[which(V(g)$name == "Robert Arryn"), ] <- c(9,13)
layout[which(V(g)$name == "Aerys"), ] <- c(4.5,12)
layout[which(V(g)$name == "Chataya"), ] <- c(4,23)
layout[which(V(g)$name == "Tommen"), ] <- c(-25,30)
layout[which(V(g)$name == "Rickard"), ] <- c(17,5)
layout[which(V(g)$name == "Theon"), ] <- c(9.5,-8)
layout[which(V(g)$name == "Missandei"), ] <- c(-10,-35)
layout[which(V(g)$name == "Daario"), ] <- c(-17,-36)
unique_widths <- sort(unique(E(g)$width))
every_five <- unique_widths[seq(1, length(unique_widths), by = 5)]
plot.igraph(
  g,
  edge.arrow.size = 0,
  layout = layout,
  vertex.label.cex = V(g)$degree / 2.7,
  vertex.label.color = "black",
  vertex.size = V(g)$degree * 2,
  edge.curved = 0.2,
  vertex.color = V(g)$color,
  vertex.label.family = "Daenerys",
  edge.width = E(g)$width / 10,
  edge.color = E(g)$color,
  vertex.label.dist = V(g)$label.dist
)
title(main = "Game of Thrones Social Network", col.main = "black",
      font.main = 2, cex.main = 1.9, adj = 0)
text(x = -2, y = 0.2, labels = "     Edge thickness is detemined by number \n
     of interactions between characters. \n
     Colors in this graph are used to \n
     point out groups of people and \n
     improve overall quality of data \n 
     visualization. They do not have any \n
     additional meaning. Size of a vertex \n
     is generally bigger for character with \n 
     more interactions but it is not related \n
     to any variable.
     ", cex = 0.8, col = "black", pos = 4, line = -1)
rect(xleft = -2, ybottom = -0.15, xright = -1.2, ytop = 0.75, border = "green", lty = 1, lwd = 2)

legend(
  "bottomright",
  legend = paste("Thickness =", every_five),
  col = "black", 
  lty = 1, 
  lwd = every_five/14,
  title = "Number of interactions"
)

text(x = -2, y = -0.7, labels = " Number of interactions is defined by how many \n times two characters' names appeared within 15 \n words of one another in the novel.", cex = 0.5, col = "black", pos = 4, line = -1.5)

