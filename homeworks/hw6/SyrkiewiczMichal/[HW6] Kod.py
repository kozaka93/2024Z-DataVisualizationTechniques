import matplotlib.pyplot as plt
import networkx as nx
import numpy as np
from sklearn.cluster import KMeans

dolphins_gml_path = './dolphins/dolphins.gml'
dolphins_graph = nx.read_gml(dolphins_gml_path)
pos = nx.spring_layout(dolphins_graph, seed=122, k=0.71)
node_sizes = [100 * dolphins_graph.degree(node) for node in dolphins_graph.nodes]
positions = np.array(list(pos.values()))
kmeans = KMeans(n_clusters=2, random_state=45, n_init='auto').fit(positions)
labels = kmeans.labels_
colors = []
for label, position in zip(labels, positions):
    gradient = position / np.linalg.norm(position) if np.linalg.norm(position) > 0 else np.zeros_like(position)
    if label == 0:
        color = (1 - gradient[0], gradient[1], gradient[0])
    else:
        color = (gradient[0], gradient[1], 1 - gradient[0])
    colors.append(np.clip(color, 0, 1))
plt.figure(figsize=(14, 14))
nx.draw_networkx_nodes(
    dolphins_graph, pos,
    node_size=node_sizes,
    node_color=colors,
    alpha=0.75,
    edgecolors="black"
)
nx.draw_networkx_edges(
    dolphins_graph, pos,
    width=1,
    alpha=0.6,
    edge_color="gray"
)
for node, (x, y) in pos.items():
    if node == "TR88":
        plt.text(x + 0.044, y - 0.02, node, fontsize=8, ha='center', va='bottom', fontweight='bold')
    elif dolphins_graph.degree(node) * 100 < 400 and len(node) > 3:
        plt.text(x, y + 0.035, node, fontsize=8, ha='center', va='bottom', fontweight='bold')
    else:
        plt.text(x, y, node, fontsize=8, ha='center', va='center', fontweight='bold')

plt.text(
    -1.2, -1.26,
    "Graf przedstawia siatkę połączeń w środowisku delfinów.\n"
    "Większy węzeł oznacza, że delfin ma większą liczbę połączeń.\n"
    "Kolor węzła pokazuje grupy delfinów – im więcej połączeń delfin ma z „granatowymi” delfinami, "
    "tym jego węzeł staje się bardziej granatowy.\n"
    "Widać również grupy węzłów-łączników m.in. „magenta”, czy „zielonych”, które łączą poszczególne grupy, "
    "nie należąc w pełni do żadnej z nich.",
    fontsize=10,
    ha="left",
    va="bottom",
    bbox=dict(facecolor="white", edgecolor="black", alpha=0.7)
)

plt.title("Graf Środowiska Delfinów", fontsize=20)
plt.axis("off")
plt.show()
