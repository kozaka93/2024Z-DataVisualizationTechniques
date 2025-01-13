import networkx as nx
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

# Tworzymy graf
G = nx.DiGraph()

# Dodajemy wierzchołki i krawędzie
death_data = [
    ("Harry Potter", "Quirrell", 1),
    ("Voldemort", "Lily Potter", 1),
    ("Peter Pettigrew", "Cedric Diggory", 4),
    ("Alastor Moody", "Evan Rosier", 4),
    ("Molly Weasley", "Bellatrix Lestrange", 7),
    ("Barty Crouch Jnr", "Barty Crouch Snr", 4),
    ("Voldemort", "Frank Bryce", 4),
    ("Voldemort", "Bertha Jorkins", 4),
    ("Severus Snape", "Dumbledore", 6),
    ("Bellatrix Lestrange", "Sirius Black", 5),
    ("Voldemort", "Amelia Bones", 6),
    ("Voldemort", "Alastor Moody", 6),
    ("Voldemort", "Rufus Scrimgeour", 6),
    ("Nagini", "Bathilda Bagshot", 7),
    ("Voldemort", "James Potter", 1),
    ("Nagini", "Severus Snape", 7),
    ("Voldemort", "Gellert Grindelwald", 7),
    ("Voldemort", "Peter Pettigrew", 7),
    ("Bellatrix Lestrange", "Dobby", 7),
    ("Bellatrix Lestrange", "Nymphadora Tonks", 7),
    ("Neville Longbottom", "Nagini", 7),
    ("Harry Potter", "Voldemort", 7)
]

# Dodajemy krawędzie z kolorami zależnymi od tomu
for murderer, victim, book in death_data:
    if book == 1:
        color = "pink"
    elif book == 2:
        color = "brown"
    elif book == 3:
        color = "purple"
    elif book == 4:
        color = "green"
    elif book == 5:
        color = "orange"
    elif book == 6:
        color = "red"
    elif book == 7:
        color = "blue"
    G.add_edge(murderer, victim, color=color)

# Rysujemy graf
plt.figure(figsize=(14, 14))

# Używamy układu kołowego
pos = nx.spring_layout(G, seed=52, k=1.2, weight='weight')

# Przygotowujemy kolory krawędzi
edge_colors = [G[u][v]['color'] for u, v in G.edges()]

# Rysujemy graf
nx.draw_networkx_nodes(G, pos, node_size=500, node_color="lightblue")  # prostokątne wierzchołki
nx.draw_networkx_edges(G, pos, edge_color=edge_colors, width=2, alpha=0.7)
nx.draw_networkx_labels(G, pos, font_size=10, font_weight="bold")

# Tworzymy legendę z odpowiednimi kolorami
legend_labels = {
    "Tom 1": "pink",
    "Tom 2": "brown",
    "Tom 3": "purple",
    "Tom 4": "green",
    "Tom 5": "orange",
    "Tom 6": "red",
    "Tom 7": "blue"
}

# Dodajemy legendę do wykresu
patches = [mpatches.Patch(color=color, label=label) for label, color in legend_labels.items()]
plt.legend(handles=patches, loc="upper left", bbox_to_anchor=(-0.13, 1), title="W którym tomie doszło do zabójstwa")

# Tytuł i ukrycie osi
plt.title("Sieć zabójstw w Harrym Potterze")
plt.axis("off")
plt.show()
