import networkx as nx
import matplotlib.pyplot as plt
from matplotlib.patches import Patch

# Read 'terrorist.pairs'
edges = []
with open('terrorist.pairs', 'r') as f:
    for line in f:
        source, target = map(int, line.strip().split('\t'))
        source -= 1
        target -= 1
        edges.append((source, target))

# Read 'terrorist.names'
node_labels = [0]*62
node_numbers = []
with open('terrorist.names', 'r') as f:
    for line in f:
        node_id, name = line.strip().split('\t')
        node_id = int(node_id)-1
        node_numbers.append(node_id)
        name = name.replace("_", "\n")
        node_labels[node_id] = name

# Read 'terrorist.groups'
groups = [0]*62
with open('terrorist.groups', 'r') as f:
    for line in f:
        node_id, group_id = line.strip().split('\t')
        node_id = int(node_id)-1
        groups[node_id] = group_id

G = nx.Graph()
for node_id in node_numbers:
    G.add_node(node_id, label=node_labels[node_id], group=groups[node_id])

G.add_edges_from(edges)

group_colors = ["#e62020", "#b26fd6", "#6fd66f", "#ffee52", "#97b4c9"]
node_colors = [group_colors[int(group)] for group in groups]

fig, ax = plt.subplots(1, 1, figsize=(8, 8))
nx.draw(
    G,
    pos=nx.spring_layout(G, seed=126, k=0.8, iterations=20),
    with_labels=True,
    labels=nx.get_node_attributes(G, 'label'),
    node_color=node_colors,
    edge_color="#adadad",
    node_size=300,
    font_size=6,
    ax=ax
)

# Create a legend for the group colors
group_labels = [f"Plane {i+1}" for i in range(len(group_colors)-1)]  # Adjust group labeling as needed
group_labels.append("None")
legend_elements = [
    Patch(facecolor=color, edgecolor='black', label=label)
    for color, label in zip(group_colors, group_labels)
]

# Add the legend to the plot
ax.legend(
    handles=legend_elements,
    title="Node color - which plane (if any)\n that person was on",
    loc="upper right",
    bbox_to_anchor=(0.25, 0.15),  # Adjust positioning if needed
    frameon=False
)

ax.set_title("9/11 Terrorist Network Graph", fontsize=14, fontweight='bold')

plt.show()