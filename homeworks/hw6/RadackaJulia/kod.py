import networkx as nx
import matplotlib.pyplot as plt

# Tworzenie grafu
G = nx.DiGraph()

# Dodawanie wierzchołków i krawędzi (przemiana w wampira)
connections = [
    ("Esther Mikaelson", "Elijah Mikaelson"),
    ("Esther Mikaelson", "Klaus Mikaelson"),
    ("Esther Mikaelson", "Rebekah Mikaelson"),
    ("Esther Mikaelson", "Kol Mikaelson"),
    ("Esther Mikaelson", "Finn Mikaelson"),
    ("Rose-Marie", "Katherine Pierce"),
    ("Klaus Mikaelson", "Mary Porter"),
    ("Mary Porter", "Rose-Marie"),
    ("Katherine Pierce", "Stefan Salvatore"),
    ("Katherine Pierce", "Damon Salvatore"),
    ("Damon Salvatore", "Caroline Forbes"),
    ("Damon Salvatore", "Vicki Donovan"),
    ("Klaus Mikaelson", "Tyler Lockwood"),
    ("Damon Salvatore", "Elena Gilbert"),
    ("Klaus Mikaelson","Marcellus Gerard")
]
# Dodawanie połączeń do grafu
G.add_edges_from(connections)

# Lista oryginalnych wampirów i czarownic
original_vampires = {"Klaus Mikaelson", "Rebekah Mikaelson", "Kol Mikaelson", "Elijah Mikaelson", "Finn Mikaelson"}
witches = {"Esther Mikaelson"}

# Kolory wierzchołków
node_colors = [
    "red" if node in original_vampires else "purple" if node in witches else "blue"
    for node in G.nodes()
]

# Tworzenie figury i rysowanie grafu
plt.figure(figsize=(10, 12))

# Pozycjonowanie wierzchołków
pos = nx.circular_layout(G)

# Rysowanie grafu
nx.draw(
    G, pos, with_labels=True, node_size=1000, node_color=node_colors,
    font_size=10, font_weight="bold", edge_color="gray"
)

# Dodanie tytułu
plt.suptitle("Drzewo przemian wampirów w uniwersum Pamiętników Wampirów", fontsize=16, y=0.97)

# Dodanie legendy z kolorowymi punktami
legend_text = (
    "Postacie są połączone, jeśli jedna\nprzemieniła drugą w wampira.\n"
    "Kolory wierzchołków: \n"
)

# Wyświetlanie tekstu legendy
plt.text(0.02, 0.065, legend_text, fontsize=12, ha="left", va="bottom", transform=plt.gca().transAxes)

# Kolory legendy
legend_colors = {
    "Oryginalne wampiry": "red",
    "Zwykłe wampiry": "blue",
    "Czarownice": "purple"
}

y_position = 0.065
for label, color in legend_colors.items():
    plt.text(0.1, y_position, f"• {label}", fontsize=12, color=color, ha="left", va="bottom", transform=plt.gca().transAxes)
    y_position -= 0.03

# Dostosowanie rozmieszczenia wykresu
plt.tight_layout(rect=[0, 0.05, 1, 0.93])  # Dostosowanie przestrzeni dla legendy i tytułu

# Wyświetlenie grafu
plt.show()
