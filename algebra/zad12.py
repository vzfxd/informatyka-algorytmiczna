import numpy as np
import matplotlib.pyplot as plt

def plot_conchoid(a_values):
    x = np.linspace(-5,5,100)
    y = np.linspace(-5,5,100)
    x, y = np.meshgrid(x, y)
    plt.figure(figsize=(15, 10))

    for i, a in enumerate(a_values, 1):
        z = -a*x**2 + x**3 - x**2 + x*y**2 - y**2
        plt.subplot(2, 3, i)
        plt.contour(x, y, z, levels=[0], label=f'a = {a}')
        plt.xlabel('x')
        plt.ylabel('y')
        plt.axhline(0, color='black',linewidth=0.5)
        plt.axvline(0, color='black',linewidth=0.5)
        plt.grid(color='gray', linestyle='--', linewidth=0.5)
        plt.legend()
        plt.title(f'Conchoid of Slus for a = {a}')
        plt.axis('equal')

    plt.tight_layout()
    plt.show()

a_values = [-4, -2, 0, 1, 2, 3]

plot_conchoid(a_values)
