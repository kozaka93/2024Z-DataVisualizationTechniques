import networkx as nx
import matplotlib.pyplot as plt
import matplotlib.lines as mlines

G = nx.read_gml('lesmiserables.gml')

edge_weights = [G[u][v]['value'] for u, v in G.edges()]

min_weight = min(edge_weights)
max_weight = max(edge_weights)
normed_weights = [(weight - min_weight) / (max_weight - min_weight) for weight in edge_weights]
line_widths = [1 + 9 * weight for weight in normed_weights]

pos = nx.kamada_kawai_layout(G)

fig, ax = plt.subplots(figsize=(12, 12))

nx.draw(G, pos=pos, with_labels=True, node_size=200, node_color="red", font_size=9, font_weight="bold", font_color="black", edge_color="gray", width=line_widths, alpha=0.6, ax=ax)

handles = []
for thickness in [1, 5, 10]:
    line = mlines.Line2D([], [], color="gray", lw=thickness, label=f'{int((thickness - 1) * (max_weight - min_weight) / 9 + min_weight)}')
    handles.append(line)
ax.legend(handles=handles, title="Liczba wspólnych scen", fontsize=10)

ax.set_title("Sieć połączeń postaci z Les Miserables", fontsize=16)

plt.tight_layout()

plt.show()
