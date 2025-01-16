import networkx as nx
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

# Tworzymy graf skierowany
G = nx.DiGraph()

# Lista zabójstw w formacie (zabójca, ofiara, sezon)
death_data = [
    ("Lisa Arryn", "Jon Arryn", 1),
    ("Khal Drogo", "Viserys Targaryen", 1),
    ("Dzik", "Robert Baratheon", 1),
    ("Unknown", "Benjen Stark", 1),
    ("Meryn Trant", "Syrio Forel", 1),
    ("Ilyn Paine", "Ned Stark", 1),
    ("Daenerys Targaryen", "Khal Drogo", 1),
    ("Stannis Baratheon", "Renly Baratheon", 2),
    ("Theon Greyjoy", "Rodrik Cassel", 2),
    ("Osha", "Maester Luwin", 2),
    ("Daenerys Targaryen", "Pyat Pree", 2),
    ("Karl Tanner", "Jeor Mormont", 3),
    ("Karl Tanner", "Craster", 3),
    ("Jon Snow", "Karl Tanner", 3),
    ("Samuel Tarly", "Biały Wędrowiec 1", 3),
    ("Jon Snow", "Orell", 3),
    ("Walder Frey", "Robb Stark", 3),
    ("Walder Frey", "Talisa Stark", 3),
    ("Roose Bolton", "Catelyn Stark", 3),
    ("Olenna Tyrell", "Joffrey Baratheon", 4),
    ("Littlefinger", "Lysa Arryn", 4),
    ("Gregor Clegane", "Oberyn Martell", 4),
    ("Olly", "Ygritte", 4),
    ("Umarli", "Jojen Reed", 4),
    ("Tyrion Lannister", "Shae", 4),
    ("Tyrion Lannister", "Tywin Lannister", 4),
    ("Jon Snow", "Mance Rayder", 5),
    ("Jon Snow", "Biały Wędrowiec 2", 5),
    ("Stannis Baratheon", "Shireen Baratheon", 5),
    ("Brienne of Tarth", "Stannis Baratheon", 5),
    ("Synowie Harpii", "Hizdahr", 5),
    ("Arya Stark", "Meryn Trant", 5),
    ("Olly", "Jon Snow", 5),
    ("Synowie Harpii", "Barristan Selmy", 5),
    ("Ellaria Sand", "Myrcella Baratheon", 5),
    ("Obara Sand", "Trystane Martell", 6),
    ("Ramsey Bolton", "Roose Bolton", 6),
    ("Euron Greyjoy", "Balon Greyjoy", 6),
    ("Jon Snow", "Alliser Thorne", 6),
    ("Jon Snow", "Olly", 6),
    ("Ramsey Bolton", "Osha", 6),
    ("Nocny król", "Trójoka wrona", 6),
    ("Umarli", "Hodor", 6),
    ("Arya Stark", "Waif", 6),
    ("Ramsey Bolton", "Rickon Stark", 6),
    ("Jon Snow", "Ramsey Bolton", 6),
    ("Qyburn", "Maester Pycelle", 6),
    ("Cersei Lannister", "Lancel Lannister", 6),
    ("Cersei Lannister", "Wielki Wróbel", 6),
    ("Cersei Lannister", "Loras Tyrell", 6),
    ("Cersei Lannister", "Margaery Tyrell", 6),
    ("Cersei Lannister", "Mace Tyrell", 6),
    ("Samobójstwo", "Tommen Baratheon", 6),
    ("Arya Stark", "Walder Frey", 6),
    ("Euron Greyjoy", "Obara Sand", 7),
    ("Euron Greyjoy", "Nymeria Sand", 7),
    ("Cersei Lannister", "Tyene Sand", 7),
    ("Jamie Lannister", "Olenna Tyrell", 7),
    ("Daenerys Targaryen", "Randyll Tarly", 7),
    ("Daenerys Targaryen", "Dickon Tarly", 7),
    ("Umarli", "Thoros", 7),
    ("Umarli", "Benjen Stark", 7),
    ("Arya Stark", "Littlefinger", 7),
    ("Umarli", "Eddison Tollet", 8),
    ("Umarli", "Lyanna Mormont", 8),
    ("Nocny król", "Theon Greyjoy", 8),
    ("Arya Stark", "Nocny król", 8),
    ("Umarli", "Jorah Mormont", 8),
    ("Cersei Lannister", "Missandei", 8),
    ("Daenerys Targaryen", "Varys", 8),
    ("Jamie Lannister", "Euron Greyjoy", 8),
    ("Gregor Clegane", "Qyburn", 8),
    ("Sandor Clegane", "Gregor Clegane", 8),
    ("Daenerys Targaryen", "Cersei Lannister", 8),
    ("Daenerys Targaryen", "Jamie Lannister", 8),
    ("Jon Snow", "Daenerys Targaryen", 8)
]

# Słownik przypisujący kolory do sezonów
season_colors = {
    1: "black",
    2: "brown",
    3: "purple",
    4: "green",
    5: "orange",
    6: "red",
    7: "blue",
    8: "violet"
}

# Dodajemy krawędzie do grafu z odpowiednimi kolorami
for murderer, victim, season in death_data:
    color = season_colors.get(season, "black")
    G.add_edge(murderer, victim, color=color)

# Liczymy liczbę zabójstw dla każdej postaci
node_sizes = [len(list(G.successors(node))) * 100 for node in G.nodes()]  # Proporcjonalnie do liczby zabójstw

# Rysujemy graf
plt.figure(figsize=(20, 20))

# Używamy układu sprężynowego do rozmieszczenia wierzchołków z większym rozciągnięciem
pos = nx.spring_layout(G, seed=24, k=1, iterations=1000)
pos['Daenerys Targaryen'] = (-0.5, 0)
pos['Samuel Tarly']=(0,0.5)
pos['Khal Drogo']=(0.25,0.25)
pos['Sandor Clegane']=(-0.25,0.25)
pos['Jon Snow'] = (0.5, 0)
pos['Cersei Lannister']=(0,-0.5)
pos['Synowie Harpii']=(0.75,0)
# Przygotowujemy kolory krawędzi
edge_colors = [G[u][v]['color'] for u, v in G.edges()]

# Rysujemy wierzchołki
nx.draw_networkx_nodes(G, pos, node_size=node_sizes, node_color="lightblue")

# Rysujemy krawędzie
nx.draw_networkx_edges(G, pos, edge_color=edge_colors, width=1, alpha=0.7)

# Rysujemy etykiety wierzchołków
nx.draw_networkx_labels(G, pos, font_size=6, font_weight="bold")

# Tworzymy legendę z odpowiednimi kolorami dla sezonów
patches = [mpatches.Patch(color=color, label=f"Sezon {season}") for season, color in season_colors.items()]
plt.legend(handles=patches, loc="upper left", title="Sezon zabójstwa")

# Tytuł i ukrycie osi
plt.title("Sieć zabójstw w 'Grze o tron'")
plt.axis("off")
plt.show()
