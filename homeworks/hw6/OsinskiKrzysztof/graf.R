# PD 6 - wizualizacja grafu

library(igraph)
library(tidygraph)
library(ggraph)

# Dane
#install.packages("remotes")
library(remotes)
#remotes::install_git("https://codeberg.org/pjphd/movienetdata.git")
library(movienetdata)

sw <- movienetdata::starwars_tfa

adj_matrix <- sw$adjacency
nodes <- sw$node_list

# Wizualizacja grafu
graph <- graph_from_adjacency_matrix(adj_matrix, 
                                     mode = "directed", 
                                     weighted = TRUE)

V(graph)$name <- nodes$char_name
V(graph)$nlines <- nodes$nlines
V(graph)$gender <- nodes$char_female


g <- ggraph(graph, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.7, color = "grey50") +
  geom_node_point(aes(size = nlines, color = ifelse(gender == 1, "female", "male")), alpha = 0.8) +
  geom_node_text(aes(label = name), repel = TRUE, vjust = -1, hjust = 0.5, size = 3.5, max.overlaps = Inf) +
  scale_edge_width(name = "Number of interactions", range = c(0.25, 3)) +
  scale_size(range = c(3, 10)) +
  theme_void() +
  labs(title = "Character interactions in Star Wars: The Force Awakens (2015)",
       size = "Number of quotes",
       color = "Gender") +
  theme(legend.position.inside = c(0.9, 0.1),
        legend.justification = c(1, 0),
        legend.box.margin = margin(0, 10, 10, 0))

g

#ggsave("graf.jpg", plot = g, width = 10, height = 6, dpi = 300, device = "jpeg")
