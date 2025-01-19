import pandas as pd
import networkx as nx
import plotly.graph_objects as go

file_path = 'budapest_connectome_3.0_209_0_median.csv'

df = pd.read_csv(file_path, delimiter=";")

G = nx.Graph()

for index, row in df.iterrows():
    node1 = row['name node1']
    node2 = row['name node2']
    weight = row['edge weight(med nof)']
    G.add_edge(node1, node2, weight=weight)

pos = nx.spring_layout(G, k=0.7, iterations=100, seed=42)

node_x = []
node_y = []
edge_x = []
edge_y = []
node_text = []
node_color = []

for node in G.nodes():
    x, y = pos[node]
    node_x.append(x)
    node_y.append(y)
    node_text.append(node)
    node_color.append(G.degree(node))

for edge in G.edges():
    x0, y0 = pos[edge[0]]
    x1, y1 = pos[edge[1]]
    edge_x.append(x0)
    edge_x.append(x1)
    edge_y.append(y0)
    edge_y.append(y1)

edge_trace = go.Scatter(
    x=edge_x, y=edge_y,
    line=dict(width=0.5, color='gray'),
    hoverinfo='none',
    mode='lines'
)

node_trace = go.Scatter(
    x=node_x, y=node_y,
    mode='markers+text',
    hoverinfo='text',
    text=node_text,
    marker=dict(
        showscale=True,
        colorscale='RdBu',
        size=10,
        color=node_color,
        colorbar=dict(thickness=15, title='Stopień wierzchołka', xanchor='left', titleside='right')
    ),
    textfont=dict(
        size=8,
        color='black',
    )
)

layout = go.Layout(
    title="Graf połączeń regionów mózgu",
    title_x=0.5,  # Centrowanie tytułu
    title_y=0.05,  # Przeniesienie tytułu niżej
    showlegend=False,
    hovermode='closest',
    margin=dict(b=0, l=0, r=0, t=0),
    xaxis=dict(showgrid=False, zeroline=False),
    yaxis=dict(showgrid=False, zeroline=False),
    width=1500,
    height=1500
)

fig = go.Figure(data=[edge_trace, node_trace], layout=layout)
fig.show()
