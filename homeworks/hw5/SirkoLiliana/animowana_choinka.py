import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation, PillowWriter

# Ornament positions
ornament_positions = []
xy_scale_factor = 1.2  # Scale factor for x and y
z_scale_factor = 16.7 / 10.6  # Scale factor for z (tree height adjustment)

# Ornament positions
for x, y, z in [
    [2.39957940141516, -0.9714820320193606, 2.5110631528063793],
    [2.27116885699266, 0.5955706097712004, 2.6583739629206075],
    [1.9461403311321093, 1.8847318080125064, 2.439471646574008],
    [0.6166699938819524, 2.077765836581497, 2.47286336137061],
    [-0.26014401406621884, 2.6966648417144095, 2.139471646574008],
    [-0.7485258934600582, 2.0339866616670315, 2.47286336137061],
    [2.2448979591836737, -5.498414200253424e-16, 3.3751046760297343],
    [1.683673469387755, 0, 3.791238888760419],
    [1.7173149040027644, 1.2816654224811768, 3.447357724207661],
    [0.6608707642814596, 1.4929185681890815, 3.831645077118461],
    [-0.19106622502038675, 1.980601296174425, 3.5583739629206077],
    [1.6285714285714286, 0, 4.7819848284962205],
    [1.680400357952251, 0.9463838502933087, 4.534490516339223],
    [1.095560651837489, 1.3737896049081095, 4.673856804412198],
    [0.5551314419964261, 1.2540515972788284, 5.0090863243642305],
    [-0.1563803564782242, 1.621045983945837, 4.7819848284962205],
    [1.3423683813602854, -0.35201044297646056, 5.798072216452177],
    [1.2043164453567987, 0.15527941061690012, 5.975305339119514],
    [0.39485461001857597, 1.3303962042141413, 5.798072216452177],
    [-0.2470436491270102, 1.0823689474018614, 6.08652757161],
    [0.9128516249679266, 0.11769918350961675, 6.753571115196479],
    [0.37239629397000995, 0.6143066379140446, 7.063968818855769],
    [0.3953583720108542, 0.16006286534650857, 7.583963261069062]
]:
    new_x = x * xy_scale_factor
    new_y = y * xy_scale_factor
    new_z = z * z_scale_factor
    ornament_positions.append([new_x, new_y, new_z])


z_scale_factor = 5

# Initialize figure
fig = plt.figure(figsize=(10, 10))
ax = fig.add_subplot(111, projection='3d')


# Draw the tree layers
def draw_tree():
    tree_center = (0, 0)
    trunk_radius = 0.3
    trunk_height = 3
    layer_heights = [3.3, 3.3, 3.2, 3.1, 3]
    layer_radii = [2.95, 2.5, 2.1, 1.7, 1.1]
    layer_offsets = [trunk_height]
    for i in range(1, len(layer_heights)):
        overlap_amount = 1.6
        layer_offsets.append(layer_offsets[-1] + layer_heights[i - 1] - overlap_amount)

    # Draw each tree layer
    for height, radius, offset in zip(layer_heights, layer_radii, layer_offsets):
        u = np.linspace(0, 2 * np.pi, 100)
        v = np.linspace(0, radius, 100)
        U, V = np.meshgrid(u, v)
        X = V * np.cos(U) + tree_center[0]
        Y = V * np.sin(U) + tree_center[1]
        Z = offset + height * (1 - (V / radius) ** 0.6)
        ax.plot_surface(X, Y, Z, color='green', alpha=1.0)  # Make tree opaque

    # Draw trunk
    u = np.linspace(0, 2 * np.pi, 100)
    z = np.linspace(0, trunk_height, 100)
    U, Z = np.meshgrid(u, z)
    X = trunk_radius * np.cos(U)
    Y = trunk_radius * np.sin(U)
    ax.plot_surface(X, Y, Z, color='brown', alpha=1.0)  # Make trunk opaque

ornaments = []

def init():
    ax.clear()
    draw_tree()
    ax.view_init(elev=7, azim=40)

    ax.set_xlim(-4, 4)
    ax.set_ylim(-4, 4)
    ax.set_zlim(0, 12)
    ax.set_title('Wesołych Świąt :)', fontsize=16, color='black', fontname='Courier New')
    ax.set_facecolor('lightblue')
    ax.set_axis_off()
    return ornaments


# Update frame
def update(frame):
    if frame < len(ornament_positions):
        x, y, z = ornament_positions[frame]
        ornament = ax.scatter(x, y, z, color='red', s=100)  # Adjust 's' for size
        ornaments.append(ornament)

    ornament = ax.scatter(0, 0, 12.5, color='#FFEA00', s=200)  # Yellow ornament at the top
    ornaments.append(ornament)
    return ornaments


# Animation
ani = FuncAnimation(fig, update, frames=len(ornament_positions), init_func=init, blit=False)

# Save as GIF
ani.save('choinka.gif', writer=PillowWriter(fps=7))

plt.show()
