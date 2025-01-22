import networkx as nx
import matplotlib.pyplot as plt
import random
from matplotlib.lines import Line2D


G = nx.karate_club_graph()


for u, v in G.edges():
    G[u][v]['strength'] = random.uniform(0.5, 2.0)


pos = nx.kamada_kawai_layout(G)

node_colors = [
    'lightblue' if G.nodes[node]['club'] == 'Mr. Hi' else 'salmon'
    for node in G.nodes()
]
node_sizes = [
    G.degree(node) * 80 + 200
    for node in G.nodes()
]


edge_widths = [
    G[u][v]['strength']
    for u, v in G.edges()
]


labels = {node: str(node) for node in G.nodes()}


plt.figure(figsize=(12, 8))

nx.draw_networkx_edges(
    G,
    pos,
    width=edge_widths,
    edge_color='gray',
    alpha=0.8
)

nx.draw_networkx_nodes(
    G,
    pos,
    node_color=node_colors,
    node_size=node_sizes,
    alpha=0.9
)

nx.draw_networkx_labels(
    G,
    pos,
    labels=labels,
    font_size=9
)

plt.title("Zachary’s Karate Club", fontsize=14)
plt.axis('off')

blue_patch = Line2D([], [], marker='o', color='w',
                    label='Mr. Hi Club (kolor błękitny)',
                    markerfacecolor='lightblue', markersize=10)
red_patch  = Line2D([], [], marker='o', color='w',
                    label='Officer Club (kolor łososiowy)',
                    markerfacecolor='salmon', markersize=10)
big_node = Line2D([], [], marker='o', color='w',
                  label='Większy węzeł = wyższy stopień',
                  markerfacecolor='gray', markersize=15)
thick_edge = Line2D([], [], color='gray', linewidth=3,
                    label='Grubsza krawędź = silniejsza relacja')

plt.legend(
    handles=[blue_patch, red_patch, big_node, thick_edge],
    loc='upper left',
    bbox_to_anchor=(1.05, 1),
    borderaxespad=0.
)


plt.tight_layout()
plt.savefig("karate_network.png", dpi=400, bbox_inches='tight')
plt.show()
