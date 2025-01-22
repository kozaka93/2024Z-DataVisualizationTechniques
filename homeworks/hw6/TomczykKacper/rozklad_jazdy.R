
library(readxl)
library(dplyr)
library(igraph)
library(tidygraph)
library(ggraph)
library(ggplot2)

# Ścieżka do pliku Excela
file_path <- "s1s2s3.xlsx" # Podaj ścieżkę do pliku

# Wczytanie pierwszego arkusza do data frame 'a'
a <- read_excel(file_path)
a <- a[, -ncol(a)]

stacje <- a %>%
  select(from, to) %>%
  unlist() %>%  # Zamienia data frame na wektor
  unique() 




# Dane o wierzchołkach
vertices <- data.frame(
  station_id = c(1:length(stacje)),
  station_name = stacje
)

a_with_ids <- a %>%
  left_join(vertices, by = c("from" = "station_name")) %>%
  left_join(vertices, by = c("to" = "station_name"), suffix = c("_from", "_to")) %>%
  select(from = station_id_from, to = station_id_to, travel_time)

edges <- data.frame(
  from = a_with_ids$from,
  to = a_with_ids$to,
  width = a_with_ids$travel_time
)


# Tworzenie obiektu grafu
graph <- tbl_graph(
  edges = edges, 
  nodes = vertices, 
  directed = FALSE
)

# Oznaczenie liści (wierzchołki o stopniu 1)
graph <- graph %>%
  mutate(is_leaf = degree(graph, mode = "all") == 1)

# Przetwarzanie nazw stacji: jeśli zaczynają się od "Warszawa", wyświetl tylko drugie słowo
graph <- graph %>%
  mutate(
    processed_name = ifelse(
      grepl("^Warszawa", station_name),
      sub("^Warszawa\\s+", "", station_name),
      station_name
    )
  )

# Zliczanie liczby połączeń między parami wierzchołków
edge_counts <- as.data.frame(as_edgelist(graph)) %>%
  dplyr::group_by(V1, V2) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")

# Dodanie informacji o wielokrotnych połączeniach do krawędzi
graph <- graph %>%
  activate(edges) %>%
  left_join(edge_counts, by = c("from" = "V1", "to" = "V2"))

# Wizualizacja grafu
ggraph(graph, layout = "fr") +  # Fruchterman-Reingold layout
  geom_edge_parallel(
    aes(color = width, group = interaction(from, to)),
    show.legend = TRUE,
    alpha = 0.8,
    width = 1.2
  ) +
  scale_edge_color_gradient(low = "blue", high = "red") +
  geom_node_point(size = 5, color = "black") +
  geom_node_text(
    aes(label = processed_name),
    repel = TRUE, size = 4
  ) +
  theme_void() +
  labs(
    title = "Graf stacji i połączeń",
    edge_color = "Czas Podróży"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


