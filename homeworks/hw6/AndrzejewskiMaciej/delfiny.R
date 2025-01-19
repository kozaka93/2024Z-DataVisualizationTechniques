library(igraph)
library(networkD3)
library(leaflet)
library(dplyr)

file <- file.choose() # dolphins.gml
graph <- read_graph(file, format = "gml")

edge_list <- igraph::as_data_frame(graph, what = "edges")
node_list <- data.frame(
  id = V(graph)$id,
  degree = degree(graph),
  name = V(graph)$label
) %>%
  arrange(degree)

edge_list$from <- edge_list$from - 1
edge_list$to <- edge_list$to - 1

dolphins <- forceNetwork(
  Links = edge_list,
  Nodes = node_list,
  Source = "from",
  Target = "to",
  NodeID = "name",
  Group = "degree",
  Nodesize = "degree",
  fontSize = 15,
  legend = TRUE,
  fontFamily = "serif",
  charge = -80,
  linkDistance = 80,
  opacity = 0.9,
  opacityNoHover = 1
) %>%
  htmlwidgets::prependContent(
    htmltools::tags$h1(
      "Social network of frequent
associations between 62 dolphins in a community living off Doubtful Sound in New Zealand"
    ),
    htmltools::tags$p(
      "(The legend shows the colors of the nodes relative
      to the specified number of node degrees)"
    )
    
  )

htmlwidgets::saveWidget(widget = dolphins,
                        file = "dolphin_network.html",
                        selfcontained = TRUE)
