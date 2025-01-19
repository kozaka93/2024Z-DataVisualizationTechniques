import osmnx as ox
#import matplotlib.pyplot as plt
import random
#import networkx as nx
import folium
from collections import defaultdict
from a_star import astar

def main():
    # Download graph
    #G = ox.graph_from_place("New York, New York, USA", network_type="drive")
    #G = ox.graph_from_place("Warsaw, Poland", network_type="drive")
    
    # Load the pre-downloaded graph from file
    # G = ox.load_graphml("srodmiescie_warsaw_graph.graphml")
    # G = ox.load_graphml("ochota_warsaw_graph.graphml")
    
    # G = ox.load_graphml("warsaw_graph.graphml")
    # G = ox.load_graphml("singapore_graph.graphml")
    G = ox.load_graphml("moscow_graph.graphml")
    # G = ox.load_graphml("poland_graph.graphml")
    
    print("Finished reading region")

    nodes, edges = ox.graph_to_gdfs(G)
    pos = {node: (data['x'], data['y']) for node, data in nodes.iterrows()}

    # Przygotowanie pod algorytm A*
    edges_dict = defaultdict(dict)
    for index, row in edges.iterrows():
        u, v = index[:2]
        weight = row['length']
        edges_dict[u][v] = {'length': weight}

    # losowanie poczatku i konca
    random_nodes = random.sample(list(G.nodes), 2)
    start_node, end_node = random_nodes
    
    #testowanie dla warszawy
    # start_node = 1977909478
    # end_node = 320278634
    # print(start_node, end_node)
    # print()

    # algorytm A*
    shortest_path_edges, visited_edges = astar(pos, edges_dict, start_node, end_node)

    # covert do folium (lepsze mapy, rowniez ladniej sie oglada, w matplotlibie bylo zlagowane i brzydkie)
    m = ox.plot_graph_folium(G, popup_attribute="length", weight=1, color="black", opacity=0.7)

    # odwiedzone drogi na ciemny niebieski
    for u, v in visited_edges:
        visited_coords = [(pos[u][1], pos[u][0]), (pos[v][1], pos[v][0])]
        folium.PolyLine(visited_coords, color="darkblue", weight=3, opacity=0.8).add_to(m)

    # najkrotsza droga na zolto
    for u, v in shortest_path_edges:
        path_coords = [(pos[u][1], pos[u][0]), (pos[v][1], pos[v][0])]
        folium.PolyLine(path_coords, color="yellow", weight=5, opacity=1).add_to(m)

    # zaznaczenie poczatku i konca
    folium.CircleMarker(
        location=(pos[start_node][1], pos[start_node][0]),
        radius=5,
        color="green",
        fill=True,
        fill_opacity=1,
    ).add_to(m)

    folium.CircleMarker(
        location=(pos[end_node][1], pos[end_node][0]),
        radius=5,
        color="red",
        fill=True,
        fill_opacity=1,
    ).add_to(m)

    # wizualizacja w pliku html
    m.save("shortest_path_map.html")
    print("Map saved to 'shortest_path_map.html'.")

if __name__ == "__main__":
    main()
