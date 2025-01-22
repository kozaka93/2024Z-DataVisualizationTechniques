import heapq
from heapq import heappop, heappush
import math
from collections import defaultdict


def heuristic(node1, node2, nodes):
    """
    Euklidesowa odleglosc 
    """
    x1, y1 = nodes[node1]
    x2, y2 = nodes[node2]
    dist = (x1 - x2) ** 2 + (y1 - y2) ** 2

    return math.sqrt(dist)


def astar(nodes, edges, start_node, end_node):
    """
    Generated comment

    Perform A* search to find the shortest path between two nodes.

    Parameters:
        nodes (dict): A dictionary where keys are node IDs and values are (x, y) coordinates.
        edges (dict): A dictionary where keys are node IDs and values are dictionaries of neighbors with edge weights.
        start_node: The starting node ID.
        end_node: The ending node ID.

    Returns:
        tuple: (shortest_path_edges, visited_edges)
            - shortest_path_edges: List of edges [(u1, v1), (u2, v2), ...] in the shortest path.
            - visited_edges: List of all edges [(u1, v1), (u2, v2), ...] visited during the algorithm.
    """
    
    open_set = []
    heapq.heappush(open_set, (0, start_node))

    came_from = {}  # Tracks the parent of each node
    g_score = defaultdict(lambda: float('inf'))  # Cost from start to node
    g_score[start_node] = 0

    f_score = defaultdict(lambda: float('inf'))  # Estimated cost from start to end through node
    f_score[start_node] = heuristic(start_node, end_node, nodes)

    visited_edges = []

    while open_set:
        _, current = heapq.heappop(open_set)

        if current == end_node:
            shortest_path_edges = []
            while current in came_from:
                prev = came_from[current]
                shortest_path_edges.append((prev, current))
                current = prev
            return shortest_path_edges[::-1], visited_edges

        for neighbor, data in edges[current].items():
            weight = data['length']
            tentative_g_score = g_score[current] + weight

            if tentative_g_score < g_score[neighbor]:
                visited_edges.append((current, neighbor))  # Mark edge as visited
                came_from[neighbor] = current
                g_score[neighbor] = tentative_g_score
                f_score[neighbor] = tentative_g_score + heuristic(neighbor, end_node, nodes)

                heapq.heappush(open_set, (f_score[neighbor], neighbor))

    # If no path is found, return empty lists
    return [], visited_edges