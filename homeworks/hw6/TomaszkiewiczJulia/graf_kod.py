import networkx as nx
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D

# Stworzenie grafu
G = nx.Graph()

# Dodanie wierzchołków (postaci)
characters = [
    "Pan Młody", "Haneczka", "Radczyni", "Panna Młoda", "Ojciec", "Jasiek", "Kuba",
    "Gospodarz", "Poeta", "Gospodyni", "Isia", "Marysia", "Wojtek", "Dziennikarz",
    "Nos", "Ksiądz", "Żyd", "Rachela", "Zosia", "Maryna", "Klimina", "Dziad",
    "Czepiec", "Czepcowa", "Kasper", "Kasia", "Staszek", "Chochoł", "Widmo",
    "Stańczyk", "Rycerz", "Hetman", "Upiór", "Wernyhora", "Muzykant"
]
G.add_nodes_from(characters)

# Lista relacji (krawędzi)
## Przepisując z książki wszystkie rozmowy ręcznie uswiadomiłam sobie jak rzadko powtarzały się rozmowy dwóch postaci :')
relationships = [
    ("Czepiec", "Dziennikarz"),
    ("Dziennikarz", "Zosia"),
    ("Radczyni", "Haneczka"),
    ("Haneczka", "Zosia"),
    ("Zosia", "Radczyni"),
    ("Radczyni", "Klimina"),
    ("Zosia", "Kasper"),
    ("Haneczka", "Jasiek"),
    ("Ksiądz", "Panna Młoda"),
    ("Ksiądz", "Pan Młody"),
    ("Pan Młody", "Panna Młoda"),
    ("Poeta", "Maryna"),
    ("Radczyni", "Maryna"),
    ("Pan Młody", "Żyd"),
    ("Pan Młody", "Rachela"),
    ("Żyd", "Rachela"),
    ("Poeta", "Rachela"),
    ("Pan Młody", "Poeta"),
    ("Radczyni", "Pan Młody"),
    ("Poeta", "Gospodarz"),
    ("Poeta", "Czepiec"),
    ("Poeta", "Ojciec"),
    ("Ojciec", "Gospodarz"),
    ("Ojciec", "Czepiec"),
    ("Gospodarz", "Czepiec"),
    ("Ojciec", "Dziad"),
    ("Dziad", "Żyd"),
    ("Żyd", "Ksiądz"),
    ("Żyd", "Czepiec"),
    ("Ksiądz", "Czepiec"),
    ("Pan Młody", "Gospodarz"),
    ("Gospodarz", "Ksiądz"),
    ("Kasper", "Jasiek"),
    ("Panna Młoda", "Poeta"),
    ("Gospodyni", "Isia"),
    ("Gospodyni", "Klimina"),
    ("Klimina", "Isia"),
    ("Isia", "Chochoł"),
    ("Marysia", "Wojtek"),
    ("Marysia", "Widmo"),
    ("Stańczyk", "Dziennikarz"),
    ("Dziennikarz", "Poeta"),
    ("Poeta", "Rycerz"),
    ("Pan Młody", "Hetman"),
    ("Pan Młody", "Dziad"),
    ("Dziad", "Upiór"),
    ("Kasia", "Kasper"),
    ("Kasia", "Jasiek"),
    ("Nos", "Kasper"),
    ("Nos", "Kasia"),
    ("Gospodarz", "Kuba"),
    ("Gospodarz", "Gospodyni"),
    ("Gospodyni", "Kuba"),
    ("Gospodarz", "Wernyhora"),
    ("Jasiek", "Gospodarz"),
    ("Gospodarz", "Staszek"),
    ("Gospodyni", "Staszek"),
    ("Panna Młoda", "Gospodarz"),
    ("Nos", "Gospodarz"),
    ("Gospodyni", "Pan Młody"),
    ("Gospodyni", "Panna Młoda"),
    ("Gospodyni", "Poeta"),
    ("Gospodyni", "Nos"),
    ("Nos", "Pan Młody"),
    ("Nos", "Panna Młoda"),
    ("Poeta", "Nos"),
    ("Muzykant", "Czepiec"),
    ("Czepiec", "Czepcowa"),
    ("Czepcowa", "Gospodyni"),
    ("Haneczka", "Pan Młody"),
    ("Czepiec", "Kuba"),
    ("Czepiec", "Dziad"),
    ("Czepiec", "Gospodyni"),
    ("Dziennikarz", "Radczyni"),
    ("Radczyni", "Panna Młoda"),
    ("Panna Młoda", "Marysia"),
    ("Marysia", "Ojciec"),
    ("Czepiec", "Pan Młody"),
    ("Czepiec", "Panna Młoda"),
    ("Jasiek", "Chochoł")
]
G.add_edges_from(relationships)

# Lista wierzchołków do pokolorowania na inny kolor
fikcyjne = ["Chochoł", "Widmo", "Stańczyk", "Hetman", "Rycerz", "Upiór", "Wernyhora"]
rzeczywiste = list(set(characters) - set(fikcyjne))

# Tworzenie listy kolorów wierzchołków
node_colors = [
    "#75C3FA" if node in fikcyjne
    else "#E55381" for node in G.nodes
]

dashed_edges = []
solid_edges = []

for u, v in G.edges:
    if u in fikcyjne or v in fikcyjne:
        dashed_edges.append((u, v))  # Krawędź do fikcyjnej postaci - linia przerywana
    else:
        solid_edges.append((u, v))  # Krawędź między postaciami rzeczywistymi - linia ciągła

pos = nx.circular_layout(G)

# Rysowanie grafu
plt.figure(figsize=(16, 16))
plt.gca().set_facecolor('lightgray')  # Tło

# Wierzchołki
nx.draw_networkx_nodes(
    G, pos, node_size=1800, node_color=node_colors, alpha=0.6, edgecolors="black", linewidths=1
)

# Krawędzie: ciągłe
nx.draw_networkx_edges(
    G, pos, edgelist=solid_edges, edge_color="gray", width=1, alpha=0.6
)

# Krawędzie: przerywane
nx.draw_networkx_edges(
    G, pos, edgelist=dashed_edges, edge_color="gray", width=1, alpha=0.6, style="dashed"
)

# Etykiety
nx.draw_networkx_labels(
    G, pos, font_size=6, font_color="white", font_weight="bold"
)

# Dodanie legendy
legend_elements = [
    Line2D([0], [0], marker='o', color='w', markerfacecolor='#E55381', markersize=10, label='Postacie rzeczywiste'),
    Line2D([0], [0], marker='o', color='w', markerfacecolor='#75C3FA', markersize=10, label=' Postacie fikcyjne')
]
plt.legend(handles=legend_elements, loc="best")

plt.title("Graf pokazujący rozmowy przeprowadzone przez bohaterów 'Wesela' Stanisława Wyspiańskiego", fontsize=18)
plt.show()