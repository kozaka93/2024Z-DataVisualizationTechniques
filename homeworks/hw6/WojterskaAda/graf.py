import json
import networkx as nx
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.lines as mlines

file_name = "starwars1.json"
with open(file_name, "r", encoding="utf-8") as file:
    data = json.load(file)

G = nx.DiGraph()

for i, node in enumerate(data["nodes"]):
    G.add_node(i, name=node["name"], value=node["value"], colour=node["colour"])

for link in data["links"]:
    G.add_edge(link["source"], link["target"], weight=link["value"])

weights = np.array([d['weight'] for _, _, d in G.edges(data=True)])
max_weight = weights.max() if len(weights) > 0 else 1

pos = nx.spring_layout(G, seed=42, k=1, iterations=20)

node_colors = [
    "#EACFAE" if data["nodes"][i]["colour"] == "#808080" else data["nodes"][i]["colour"]
    for i in G.nodes()
]

fig, ax = plt.subplots(figsize=(8, 6))

nx.draw_networkx_nodes(
    G, pos, node_color=node_colors,
    node_size=[10 * data["nodes"][i]["value"] for i in G.nodes()],
    ax=ax
)

nx.draw_networkx_edges(
    G,
    pos=pos,
    width=np.sqrt(weights / max_weight) * 15,  # Skalowanie szerokości krawędzi
    edge_color="#E1C7A9",
    alpha=0.3,  # Półprzezroczyste krawędzie
    ax=ax
)

node_sizes = [10 * data["nodes"][i]["value"] for i in G.nodes()]

node_sizes = [10 * data["nodes"][i]["value"] for i in G.nodes()]
for v in np.quantile(node_sizes, np.linspace(0, 1, 5)):
    ax.plot([], [], "o", color="#f5deb3", markersize=np.sqrt(v), label=f"{int(v)}")

for spine in ax.spines.values():
    spine.set_visible(False)

node_labels = {i: data["nodes"][i]["name"] for i in G.nodes()}
nx.draw_networkx_labels(G, pos, labels=node_labels, font_size=8, font_weight="bold", ax=ax)

node_legend = ax.legend(
    labelspacing=1.5,
    title="Number of scenes \n the character appeared in",
    frameon=True,
    ncol=1,
    loc="upper right",
    bbox_to_anchor=(1.1, 0.5),
)
ax.add_artist(node_legend)

unique_weights = np.unique(weights)
edge_legend_handles = [
    mlines.Line2D([], [], color="#E1C7A9", linewidth=np.sqrt(w / max_weight) * 10, label=f"{int(w)}")
    for w in unique_weights
]

edge_legend = ax.legend(
    handles=edge_legend_handles,
    title="Number of scenes\nwhere characters\nappeared together",
    frameon=True,
    loc="upper left",
    bbox_to_anchor=(-0.1, 0.5),
)
ax.add_artist(edge_legend)

plt.title("Star Wars social networks - season 1")
plt.show()
