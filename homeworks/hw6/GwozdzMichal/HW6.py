import matplotlib.pyplot as plt
import networkx as nx

# wcztytanie grafu 
G = nx.read_gml("homeworks/hw6/GwozdzMichal/lesmiserables.gml")

# stopnie wierzcholkow 
degrees = dict(G.degree())
max_degree = max(degrees.values())

# listy odpowiadajace wierzcholkom
node_sizes = [degrees[node] * 30 + 50 for node in G.nodes()]
node_colors = [degrees[node] for node in G.nodes()]

# layout
pos = nx.spring_layout(G, k=1.2, seed=42)
fig, ax = plt.subplots(figsize=(12, 12))

# rysowanie wierzcholkow i krawedzi
nodes = nx.draw_networkx_nodes(
    G,
    pos=pos,
    node_size=node_sizes,
    node_color=node_colors,
    cmap=plt.cm.viridis,
    alpha=0.9,
    ax=ax
)
nx.draw_networkx_edges(
    G,
    pos=pos,
    alpha=0.3,
    ax=ax
)

# podpisy dla wybranych wierzchołkow
labels_to_draw = {}
for node, deg in degrees.items():
    if deg > 12:  # próg, powyżej którego etykiety się pojawią
        labels_to_draw[node] = node

nx.draw_networkx_labels(
    G,
    pos=pos,
    labels=labels_to_draw,
    font_size=10,
    font_color="black",
    ax=ax
)

sm = plt.cm.ScalarMappable(
    cmap=plt.cm.viridis,
    norm=plt.Normalize(vmin=0, vmax=max_degree)
)
sm.set_array([])

# dodanie colorbar do figury
cbar = fig.colorbar(sm, ax=ax)
cbar.set_label("Stopień wierzchołka (Degree)")

# tytul i ukyrcie osi
ax.set_title("Sieć współwystępowania postaci w 'Nędznikach' (Les Miserables)")
ax.axis("off")

# zapis rysunku
plt.savefig("homeworks/hw6/GwozdzMichal/les_miserables_network.png", dpi=300, bbox_inches="tight")
plt.show()