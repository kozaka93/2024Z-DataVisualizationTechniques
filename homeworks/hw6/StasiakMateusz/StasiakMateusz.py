import pandas as pd
import networkx as nx
import matplotlib.pyplot as plt

df = pd.read_csv(r"srep00196-s2.csv", sep=';')

# Oczyszczanie danych

df = df.iloc[3:].reset_index(drop=True)

df = df[(df["#"].str.count(",") == 2)]

df[['Food_1', 'Food_2', 'Connection']] = df['#'].str.split(",", expand=True)
df['Connection'] = df['Connection'].astype(int)

df = df.drop(columns=["#"])

# Ograniczam graf do 50 krawędzi bo wiecej to robi sie juz nieczytelnie

df = df[
         (df["Food_1"].str.endswith(("fruit", "cheese"))) &
         (df["Food_2"].str.endswith(("fruit", "cheese")))
         ].iloc[0:50]

G = nx.from_pandas_edgelist(
    df,
    source='Food_1',
    target='Food_2',
    edge_attr='Connection'
)

# Kolorujemy wierzcholki
node_colors = []
for node in G.nodes():
    if node.endswith("cheese"):
        node_colors.append("yellow")
    else:
        node_colors.append("plum")

pos = nx.spring_layout(G, seed=9999, k=20, iterations=1000)

# Rysowanie grafu
plt.figure(figsize=(12, 12))
weights = [G[u][v]['Connection'] for u, v in G.edges()]
nx.draw(
    G, pos, with_labels=True, node_size=700, node_color=node_colors,
    font_size=10, font_weight="bold"
)
nx.draw_networkx_edges(G, pos, edge_color="grey", width=[weight / 30 for weight in weights])

plt.title("Jak stworzyć idealną deskę serów.")
plt.suptitle(
    "Graf ten przedstawia na podstawie grubości krawędzi,\n jak bardzo profile smakowe poszczególnych artykułów sporzywczych do siebie pasują. \nArtykuły dzielą się na sery zaznaczone na żółto oraz owowce zaznaczone na fioletowo.")
plt.show()