import networkx as nx
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.lines as mlines

klub = nx.karate_club_graph()
for (x, y) in klub.edges():
    klub[x][y]['waga'] = np.random.randint(1, 6)

wagi = [klub[x][y]['waga'] for x, y in klub.edges()]
pozycje = nx.spring_layout(klub, k=1, seed=42)
fig, ax = plt.subplots(1, 1, figsize=(12, 12))

nx.draw(
    klub,
    pos=pozycje,
    with_labels=True,
    font_size=10,
    font_weight='bold', 
    font_color='black',
    node_color="#FFC0CB",
    edge_color="#4B0082",
    node_size=380,
    width=[w * 0.4 for w in wagi],
    ax=ax
)

unikalne = np.unique(wagi)
linie = [
    mlines.Line2D([], [], color="#4B0082", linewidth=w * 0.5, label=f"{w}") 
    for w in unikalne
]
legenda = ax.legend(
    handles=linie,
    loc="upper right",
    title="Częstotliwość interakcji",
    frameon=True,
    edgecolor='black'
)
ax.add_artist(legenda)

plt.title("Sieć społeczna klubu karate", fontsize=18)
plt.figtext(0.5, 0.05,
            "Graf przedstawia sieć społeczną członków klubu karate. "
            "Każdy wierzchołek reprezentuje członka klubu, a krawędzie pokazują "
            "interakcje między nimi. Grubość krawędzi odzwierciedla częstotliwość "
            "kontaktów między danymi osobami.",
            wrap=True, horizontalalignment='center', fontsize=12, alpha=0.8)

plt.axis('off')
plt.show()