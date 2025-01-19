import pandas as pd
import networkx as nx
import matplotlib.pyplot as plt

edges = pd.read_csv("edges.csv")
nodes = pd.read_csv("nodes.csv")

# nie chcemy extra spacji
edges.columns = edges.columns.str.strip()
nodes.columns = nodes.columns.str.strip()

# zmieniamy NaN-y które będą nam przeszkadzać na 0, bo psują
nodes['Curso'] = nodes['Curso'].fillna(0)
nodes['prosocial'] = nodes['prosocial'].fillna(0.0)

# tworzymy nasz graf
G = nx.Graph()

# wierzchołki
for idx, row in nodes.iterrows():
    node_id = row['# index']
    G.add_node(node_id, name=row['name'], crttotal=row['crttotal'],
               pos=row['_pos'], curso=row['Curso'], prosocial=row['prosocial'])

# krawędzie
for idx, row in edges.iterrows():
    G.add_edge(row['# source'], row['target'], weight=row['weight'])

# kolorujemy krawędzie - jest ich za dużo, żeby je pogrubiać dlatego zamiast tego zostaną pokolorowane, no powstawała jedna wielka czarna plama
edge_colors = []
for u, v, data in G.edges(data=True):
    if data['weight'] == 1:
        edge_colors.append('lightgray')
    elif data['weight'] == 2:
        edge_colors.append('gray')
    else:
        edge_colors.append('black')

# kolory dla nodes
curso_map = {
    1: 'orange',
    2: 'lightgreen',
    3: 'lightblue',
    4: 'yellow',
    0: 'pink'
}

# kolorujemy
node_colors = []
for node in G.nodes:
    curso_value = G.nodes[node]['curso']
    color = curso_map.get(curso_value, 'purple')  # Domyślnie fioletowy, jeśli wartość kursu nieznana
    node_colors.append(color)

# rozmiar wierzchołków na podstawie 'prosocial'
node_sizes = [150 + 300 * G.nodes[node]['prosocial'] for node in G.nodes]

# układ wierzchołków
pos = nx.spring_layout(G, k=0.6)

# rysujemy graf
plt.figure(figsize=(20, 20))

# wierzch
nx.draw_networkx_nodes(G, pos, node_size=node_sizes, node_color=node_colors, alpha=0.7)

# krawędzie
nx.draw_networkx_edges(G, pos, edge_color=edge_colors, width=1, alpha=0.5)

# indeksu wyświetlamy 'name', pogrubiamy zeby bylo lepiej widac bo sie zlewa
nx.draw_networkx_labels(G, pos, labels=nx.get_node_attributes(G, 'name'), font_size=8, font_color='black', font_weight='bold')

# tytuł
plt.title("Graph representing networks of friendships in Spanish highschool", loc='center')


# robimy legende

# dla kolorów
curso_labels = [plt.Line2D([0], [0], marker='o', color='w', markerfacecolor=color, markersize=10, label=f'Curso {key}')
                for key, color in curso_map.items()]

# dla edges
edge_labels = [plt.Line2D([0, 1], [0, 0], color='gray', linewidth=4, label='weight = 0'),
               plt.Line2D([0, 1], [0, 0], color='lightgray', linewidth=4, label='weight = 1'),
               plt.Line2D([0, 1], [0, 0], color='black', linewidth=4, label='weight = else than 0 and 1 ')]

# dla wielkości kropek
size_labels = [plt.Line2D([0], [0], marker='o', color='w', markerfacecolor='gray', markersize=13, label='Prosocial = 0'),
                plt.Line2D([0], [0], marker='o', color='w', markerfacecolor='gray', markersize=15, label='Prosocial = 0.33'),
                plt.Line2D([0], [0], marker='o', color='w', markerfacecolor='gray', markersize=18, label='Prosocial = 0.67'),
               plt.Line2D([0], [0], marker='o', color='w', markerfacecolor='gray', markersize=20, label='Prosocial = 1')]


# dodajemy legende w prawu górny róg
plt.legend(handles=curso_labels + edge_labels + size_labels, loc='upper right', bbox_to_anchor=(1.15, 1),
           labelspacing=1.2, handlelength=2)


plt.tight_layout()
plt.show()