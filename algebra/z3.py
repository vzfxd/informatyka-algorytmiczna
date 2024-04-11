import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(-5, 5, 100)
y = np.linspace(-5, 5, 100)
x, y = np.meshgrid(x, y)

# V(z - x^2 - y^2)
z1 = x**2 + y**2

# V(z^2 - x^2 - y^2)
z2_p = np.sqrt(x**2 + y**2)
z2_n = -np.sqrt(x**2 + y**2)

# V(z - x^2 + y^2)
z3 = x**2 - y**2

# V(xz, yz)
z4 = np.zeros_like(x)
z4_prosta = np.linspace(-5, 5, 100)

fig = plt.figure(figsize=(15, 10))

ax1 = fig.add_subplot(221, projection='3d')
ax1.plot_surface(x, y, z1, cmap='viridis')
ax1.set_title('V(z - x^2 - y^2)')

ax2 = fig.add_subplot(222, projection='3d')
ax2.plot_surface(x, y, z2_p, cmap='viridis')
ax2.plot_surface(x, y, z2_n, cmap='viridis')
ax2.set_title('V(z^2 - x^2 - y^2)')

ax3 = fig.add_subplot(223, projection='3d')
ax3.plot_surface(x, y, z3, cmap='viridis')
ax3.set_title('V(z - x^2 + y^2)')

ax4 = fig.add_subplot(224, projection='3d')
ax4.plot_surface(x, y, z4, cmap='viridis')
ax4.plot(np.zeros(100), np.zeros(100), z4_prosta)
ax4.set_title('V(xz, yz)')

plt.show()
