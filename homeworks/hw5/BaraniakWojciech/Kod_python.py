import numpy as np
import plotly.graph_objects as go
import plotly.io as pio
import imageio
import os

try:
    os.mkdir("zdjecia")
except FileExistsError:
    pass



# program do generowania choinki, może się długo renderować dla wysokiego parametru num_frames
num_frames = 5
czas_klatki = 0.05
def transpozycja(x, y, z, dx, dy, dz):
    x = x + dx
    y = y + dy
    z = z + dz
    return x, y, z

def gen_coord(radius, num_radial=100, num_angular=100):
    r_values = np.linspace(0, radius, num_radial)
    theta_values = np.linspace(0, 2 * np.pi, num_angular)
    r, theta = np.meshgrid(r_values, theta_values)
    x = r * np.cos(theta)
    y = r * np.sin(theta)
    return x, y

def generate_cylinder(radius, height, num_points=20):
    theta = np.linspace(0, 2 * np.pi, num_points)
    z = np.linspace(0, height, num_points)
    theta, z = np.meshgrid(theta, z)
    x = radius * np.cos(theta)
    y = radius * np.sin(theta)
    return x, y, z

# można dać mniej punktów, bąbki będą znacznie mniejsze niż cała choinka
def generate_sphere(radius, num_points=20):
    phi = np.linspace(0, np.pi, num_points)
    theta = np.linspace(0, 2 * np.pi, num_points)
    phi, theta = np.meshgrid(phi, theta)
    x = radius * np.sin(phi) * np.cos(theta)
    y = radius * np.sin(phi) * np.sin(theta)
    z = radius * np.cos(phi)
    return x, y, z

def r1_tree_3d(x, y):
    r = np.sqrt(x**2 + y**2)
    return ((np.cos(0.9 * r) + 1.5) * (0.75 * r - 2)**2 * 0.7 + 12.3)

def r2_tree_3d(x, y):
    r = np.sqrt(x**2 + y**2)
    return ((1/3 * np.cos(r / 1.2) + 1) * (1.25 * 0.2 * r - 3)**2 + 4.5)

def r3_tree_3d(x, y):
    r = np.sqrt(x**2 + y**2)
    return ((1/3 * np.cos(r / 1.8) + 1) * (0.1 * 1.5 * r - 3)**2 - 0.5)


# część pnia
x0, y0, z0 = generate_cylinder(2, 3)
# Choinka
x1, y1 = gen_coord(3.5)
z1 = r1_tree_3d(x1, y1)
x2, y2 = gen_coord(5.5)
z2 = r2_tree_3d(x2, y2)
x3, y3 = gen_coord(7.5)
z3 = r3_tree_3d(x3, y3)
# szczyt
x4, y4, z4 = generate_sphere(0.75, 20)
x4, y4, z4 = transpozycja(x4, y4, z4, 0, 0, 19)
# inne bąbki
x5, y5, z5 = generate_sphere(0.45, 10)
x6, y6, z6 = generate_sphere(0.45, 10)
x7, y7, z7 = generate_sphere(0.45, 10)
x8, y8, z8 = generate_sphere(0.45, 10)

x5, y5, z5 = transpozycja(x5, y5, z5, 3.5, -0.9, 4.6)
x6, y6, z6 = transpozycja(x6, y6, z6, 0.5, 2.7, 8.6)
x7, y7, z7 = transpozycja(x7, y7, z7, -0.01, -0.95, 14.8)
x8, y8, z8 = transpozycja(x8, y8, z8, -2.1, -1.7, 8.6)

fig = go.Figure()

fig.add_trace(go.Surface(x=x0, y=y0, z=z0, colorscale=[[0, 'brown'], [1, 'brown']], showscale=False, opacity=1))
fig.add_trace(go.Surface(x=x1, y=y1, z=z1, colorscale=[[0, 'green'], [1, 'green']], showscale=False, opacity=1))
fig.add_trace(go.Surface(x=x2, y=y2, z=z2, colorscale=[[0, 'green'], [1, 'green']], showscale=False, opacity=1))
fig.add_trace(go.Surface(x=x3, y=y3, z=z3, colorscale=[[0, 'green'], [1, 'green']], showscale=False, opacity=1))
fig.add_trace(go.Surface(x=x4, y=y4, z=z4, colorscale=[[0, 'yellow'], [1, 'yellow']], showscale=False, opacity=1))
fig.add_trace(go.Surface(x=x5, y=y5, z=z5, colorscale=[[0, 'white'], [1, 'white']], showscale=False, opacity=1))
fig.add_trace(go.Surface(x=x6, y=y6, z=z6, colorscale=[[0, 'red'], [1, 'red']], showscale=False, opacity=1))
fig.add_trace(go.Surface(x=x7, y=y7, z=z7, colorscale=[[0, 'white'], [1, 'white']], showscale=False, opacity=1))
fig.add_trace(go.Surface(x=x8, y=y8, z=z8, colorscale=[[0, 'red'], [1, 'red']], showscale=False, opacity=1))

camera_angles = np.linspace(0, 360, num_frames)
frames = []
frame_filenames = []

for i, angle in enumerate(camera_angles):
    fig.update_layout(
        scene=dict(
            camera=dict(
                eye=dict(x=2 * np.cos(np.radians(angle)), y=2 * np.sin(np.radians(angle)), z=0.125),
                up=dict(x=0, y=0, z=1),
                center=dict(x=0, y=0, z=0)
            ),
            xaxis=dict(showgrid=False, zeroline=False, showticklabels=False, visible=False),
            yaxis=dict(showgrid=False, zeroline=False, showticklabels=False, visible=False),
            zaxis=dict(showgrid=False, zeroline=False, showticklabels=False, visible=False)
        ),
        plot_bgcolor='white',
        margin=dict(l=0, r=0, t=0, b=0),
        paper_bgcolor='white'
    )

    filename = f'zdjecia/frame_{i:03d}.png'
    pio.write_image(fig, filename)
    frame_filenames.append(filename)


# generowanie Dżifa z folderu
with imageio.get_writer('choinka.gif', mode='I', duration=czas_klatki, loop=0) as writer:
    for frame in frame_filenames:
        img = imageio.imread(frame)
        writer.append_data(img)
