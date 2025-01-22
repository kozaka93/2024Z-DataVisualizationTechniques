import networkx as nx
import matplotlib.pyplot as plt


#DANE: https://public.websites.umich.edu/~mejn/netdata/

g = nx.read_gml("football.gml")
plt.figure(figsize=(30, 20))

pos = nx.kamada_kawai_layout(g)

node_colors = [data['value'] for _, data in g.nodes(data=True)]

nx.draw_networkx_nodes(
    g,
    pos,
    node_color=node_colors,
    cmap=plt.cm.tab20,
    node_size=500,
    alpha=0.9,
    edgecolors="black",
    linewidths=1
)

nx.draw_networkx_edges(
    g,
    pos,
    edge_color="gray",
    alpha=0.3
)


for node, (x, y) in pos.items():
    plt.text(
        x, y + 0.03,
        s=node,
        fontsize=9,
        fontweight="bold",
        color="black",
        ha="center",
        va="center"
    )

plt.title(
    "Network of American Football Games Between Division IA Colleges During Regular Season Fall 2000",
    fontsize=18,
    fontweight="bold",
    color="darkred",
)

plt.axis("off")



description_text = (
    "Description\n\n"
    "This graph visualizes games played between Division IA college football teams\n"
    "in the Fall 2000 regular season. Each node represents a team, and edges\n"
    "indicate if there was a match between them. Node colors correspond to team groups."
)

plt.gca().text(
    0.75, 0.21,
    description_text,
    fontsize=12,
    ha="left",
    va="top",
    bbox=dict(boxstyle="round,pad=0.3", edgecolor="black", facecolor="lightyellow"),
    transform=plt.gca().transAxes
)

#plt.show()
output_file = "TWD_GRAF.png"
plt.savefig(output_file, format="png", dpi=150, bbox_inches="tight")
plt.close()

