import networkx as nx
import matplotlib.pyplot as plt

G = nx.read_gml(r"C:\Users\eliss\Downloads\karate\karate.gml", label='id')

node_degrees = dict(G.degree())  #pobieramy stopnie wierzcholkow
node_weights = list(node_degrees.values())  #wagi wierzchołków = stopień wierzchołka

node_sizes = [100 + 20 * weight for weight in node_weights] #mapowanie wag wierzcholkow

node_colors = [plt.cm.viridis(weight / max(node_weights)) for weight in node_weights] #mapowanie wag - kolory

#ustawiamy wierzcholki - spring layout
pos = nx.spring_layout(G, seed=42, k=0.15)
#spring layout - umieszcza wierzcholki o wyzszej wadze w centrum grafu, o niższej na obrzeżach;
#dzieki temu zwiekszamy czytelnosc grafu

fig, ax = plt.subplots(figsize=(12, 12)) #rysujemy graf

#rysujemy wierzcholki
nx.draw_networkx_nodes(
    G, pos,
    node_size=node_sizes,
    node_color=node_colors,
    edgecolors='black',
    linewidths=1.5,
    alpha=0.9,
    ax=ax
)

#rysujemy krawędzie
nx.draw_networkx_edges(
    G, pos,
    edge_color='gray',
    width=1.5,
    alpha=0.5,
    ax=ax
)

#etykiety iwerzcholkow
nx.draw_networkx_labels(
    G, pos,
    labels={i: str(i) for i in G.nodes()},
    font_size=8,
    font_color='white',
    font_weight='bold',
    verticalalignment='center',
    ax=ax
)

sm = plt.cm.ScalarMappable(cmap=plt.cm.viridis, norm=plt.Normalize(vmin=min(node_weights), vmax=max(node_weights)))
sm.set_array(node_weights)
fig.colorbar(sm, ax=ax, label='Waga wierzchołków \n (liczba znajomych)')

ax.set_title("Graf opisujący sieć 34 znajomych na zajęciach karate w USA (1970 r.)", fontsize=16)
ax.axis('off')

plt.savefig("circular_graph_with_node_weights_based_on_degree_updated_white_labels.png", dpi=300)

plt.show()


import networkx as nx
import matplotlib.pyplot as plt

G = nx.read_gml(r"C:\Users\eliss\Downloads\karate\karate.gml", label='id')

node_degrees = dict(G.degree())  #pobieramy stopnie wierzcholkow
node_weights = list(node_degrees.values())  #wagi wierzchołków = stopień wierzchołka

node_sizes = [100 + 20 * weight for weight in node_weights] #mapowanie wag wierzcholkow

node_colors = [plt.cm.viridis(weight / max(node_weights)) for weight in node_weights] #mapowanie wag - kolory

#ustawiamy wierzcholki - spring layout
pos = nx.spring_layout(G, seed=42, k=0.15)
#spring layout - umieszcza wierzcholki o wyzszej wadze w centrum grafu, o niższej na obrzeżach;
#dzieki temu zwiekszamy czytelnosc grafu

fig, ax = plt.subplots(figsize=(12, 12)) #rysujemy graf

#rysujemy wierzcholki
nx.draw_networkx_nodes(
    G, pos,
    node_size=node_sizes,
    node_color=node_colors,
    edgecolors='black',
    linewidths=1.5,
    alpha=0.9,
    ax=ax
)

#rysujemy krawędzie
nx.draw_networkx_edges(
    G, pos,
    edge_color='gray',
    width=1.5,
    alpha=0.5,
    ax=ax
)

#etykiety wierzcholkow
nx.draw_networkx_labels(
    G, pos,
    labels={i: str(i) for i in G.nodes()},
    font_size=8,
    font_color='white',
    font_weight='bold',
    verticalalignment='center',
    ax=ax
)

sm = plt.cm.ScalarMappable(cmap=plt.cm.viridis, norm=plt.Normalize(vmin=min(node_weights), vmax=max(node_weights)))
sm.set_array(node_weights)
fig.colorbar(sm, ax=ax, label='Waga wierzchołków \n (liczba znajomych)')

ax.set_title("Graf opisujący sieć 34 znajomych na zajęciach karate w USA (1970 r.)", fontsize=16)
ax.axis('off')

plt.savefig("circular_graph_with_node_weights_based_on_degree_updated_white_labels.png", dpi=300)

plt.show()


# podsumowanie i wnioski:
# graf przedstawia sieć 34 uczestników zajęć karate, na podstawie danych o ich wzajemnych znajomościach.
# wierzchołki reprezentują osoby, a krawędzie między nimi wskazują czy te osoby się znają.
# do wizualizacji użyłam mapowania spring layout, które w centrum grafu umieszcza wierzchołki o większej
# liczbie krawędzi, a na obrzeżach te o mniejszej. dzięki temu odczytywanie grafu jest bardziej intuicyjne -
# - osoby w centrum grafu, otoczone największą ilością innych osób (wierzchołków) mają większą sieć znajomych
# niż osoby trzymające się z boku