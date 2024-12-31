import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from mpl_toolkits.mplot3d import Axes3D
import random

H = 10  # Wysokosc drzewa
num_spheres = 15  # Ilosc poziomow dla bombek
base_radius = 3

heights = np.linspace(0, H, num_spheres)
radii = base_radius * (1 - heights / H)

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

ax.set_xlim(-base_radius - 1, base_radius + 1)
ax.set_ylim(-base_radius - 1, base_radius + 1)
ax.set_zlim(0, H + 1)


# Stozek
theta = np.linspace(0, 2 * np.pi, 100)
z_cone = np.linspace(0, H, 100)
Z_cone, Theta_cone = np.meshgrid(z_cone, theta)  # Powierzchnia stozka

R_cone = base_radius * (1 - Z_cone / H)

X_cone = R_cone * np.cos(Theta_cone)
Y_cone = R_cone * np.sin(Theta_cone)

ax.plot_surface(X_cone, Y_cone, Z_cone, color="green", alpha=0.5)


total_spheres = sum(num_spheres - i for i in range(num_spheres))

colors = ["#{:06x}".format(random.randint(0, 0xFFFFFF)) for _ in range(total_spheres)]
colors[total_spheres-1] = "#ffff00" # zlota bombka na szczycie


scatter = ax.scatter([0 for _ in range(total_spheres)], [0 for _ in range(total_spheres)], [0 for _ in range(total_spheres)], c=colors, s=50)

# Poczatkowe polozenie i predkosc
starting_pos = [[random.random() / 10, random.random() * 2 * np.pi] for _ in range(total_spheres)]

# Animation
def update(frame):
    iter = 0
    x = []
    y = []
    z = []
    for i, h in enumerate(heights):
        radius = radii[i]
        for j in range(num_spheres - i):
            angle = frame * starting_pos[iter][0] + starting_pos[iter][1]
            x.append(radius * np.cos(angle))
            y.append(radius * np.sin(angle))
            z.append(h)
            iter += 1

    scatter._offsets3d = (x, y, z)  # Update
    return scatter,



ani = FuncAnimation(fig, update, frames=360, interval=50, blit=False)

ax.set_axis_off()

# Animate
# ani = FuncAnimation(fig, update, frames=360, interval=50, blit=False)

# Save the GIF
ani.save("tree_animation.gif", writer='pillow', fps=30)

# Display
plt.show()
