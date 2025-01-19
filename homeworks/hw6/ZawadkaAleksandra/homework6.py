import networkx as nx
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.lines as mlines

file_path = "out.moreno_bison_bison"

G = nx.DiGraph()

with open(file_path, 'r') as file:
    for line in file:
        if not line.startswith('%'):
            parts = line.strip().split()
            if len(parts) >= 2:
                source, target = int(parts[0]), int(parts[1])
                weight = int(parts[2]) if len(parts) > 2 else 1
                G.add_edge(source, target, weight=weight)

weights = [G[u][v]['weight'] for u, v in G.edges()]
pos = nx.spring_layout(G, seed=42)

fig, ax = plt.subplots(1, 1, figsize=(12, 12))
nx.draw(
    G,
    pos=pos,
    with_labels=True,
    arrowstyle='->',
    arrowsize=12,
    font_size=10,
    node_color="lightblue",
    edge_color="gray",
    node_size=400,
    width=[w * 0.15 for w in weights],
    ax=ax,
)

unique_weights = np.unique(weights)
edge_legend_handles = [
    mlines.Line2D([], [], color="gray", linewidth=w * 0.15, label=f"{w}") for w in unique_weights
]
edge_legend = ax.legend(
    handles=edge_legend_handles,
    loc="upper right",
    title="Zaobserwowane dominacje",
    frameon=False,
)
ax.add_artist(edge_legend)

plt.title("Dominacja między bizonami", fontsize=16)
plt.figtext(0.5, 0.05,
            "Graf przedstawia dominację między bizonami amerykańskimi zaobserwowaną w 1972 roku "
            "na obszarze National Bison Range w Montanie. Każdy wierzchołek grafu reprezentuje "
            "danego bizona, krawędź wskazuje na dominację jednego osobnika nad drugim. Wartości przypisane "
            "krawędziom odzwierciedlają częstotliwość występowania tego zachowania w badanym okresie.",
            wrap=True, horizontalalignment='center', fontsize=9, alpha=0.8)
plt.axis('off')
plt.show()
