import networkx as nx
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import matplotlib.cm as cm

def main():
    
    G = nx.read_gml(r"dolphins.gml")

    degrees = dict(G.degree())
    max_degree = max(degrees.values())
    min_degree = min(degrees.values())
    avg_degree = sum(degrees.values()) / len(degrees)

    cmap = cm.coolwarm
    norm = mcolors.Normalize(vmin=0, vmax=max_degree)
    node_colors = [cmap(norm(deg)) for deg in degrees.values()]

    node_sizes = [100 * (deg + 1) for deg in degrees.values()]

    pos = nx.spring_layout(G, seed=99, k=5, iterations=1000)

    fig, ax = plt.subplots(figsize=(12, 10))

    nx.draw_networkx_edges(G, pos, alpha=0.5, edge_color="gray", ax=ax)
    nx.draw_networkx_nodes(
        G,
        pos,
        node_color=node_colors,
        node_size=node_sizes,
        ax=ax
    )
    
    nx.draw_networkx_labels(G, pos, ax=ax, font_size=9)

    ax.set_title(
        "                              Dolphin Social Network\n",
        fontsize=14,
        fontweight="bold",
        loc = "center"
    )
    ax.set_axis_off()
    fig.suptitle(
    "The graph represents the frequency and intensity of social interactions\n" 
    "among 62 dolphins within a single community.\n"
    "Each node in the graph represents an individual dolphin,\n"
    "and each edge indicates that two dolphins were frequently seen together.\n"
    "The size and color of a node indicate how many interactions that particular dolphin has with others in the network",
    fontsize=9, 
    y = 0.88
)

    sm = cm.ScalarMappable(cmap=cmap, norm=norm)
    sm.set_array([])
    cbar = fig.colorbar(
        sm, 
        ax=ax,
        shrink=0.4,     
        fraction=0.05,
        pad=0.05,
        location='right'
    )
    cbar.set_label("Node Degree")

    cbar.outline.set_edgecolor("black")
    cbar.outline.set_linewidth(1.2)

    ax.scatter([], [], s=100*(min_degree+1), c=[cmap(norm(min_degree))],
               label=f"Min degree: {min_degree}")
    ax.scatter([], [], s=100*(avg_degree+1), c=[cmap(norm(avg_degree))],
               label=f"Avg degree: {avg_degree:.2f}")
    ax.scatter([], [], s=100*(max_degree+1), c=[cmap(norm(max_degree))],
               label=f"Max degree: {max_degree}")

    leg = ax.legend(
        title="Node Degrees",
        scatterpoints=1,
        frameon=True,
        borderpad=1.5, 
        labelspacing=2,
        loc="upper left",
        bbox_to_anchor=(1.02, 1),
        title_fontsize=12
    )
    
    leg.get_frame().set_edgecolor("black")
    leg.get_frame().set_linewidth(1.2)

    fig.tight_layout()
    #plt.savefig("dolphins_graph.png", dpi=300)
    plt.show()

if __name__ == "__main__":
    main()
