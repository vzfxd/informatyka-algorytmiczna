import sympy as sp
import matplotlib.pyplot as plt
import numpy as np

x, y = sp.symbols('x y')

eq1 = (x**2 + y**2 + 4*y)**2 - 16 * (x**2 + y**2)
eq2 = 2 * (x**2 + 9) * (y**2 - 16) + (x**2 - 9)**2 + (y**2 - 16)**2
eq3 = 350 * x**2 * y**2 - 15**2 * (x**2 + y**2) + 12**2 * (x**4 + y**4) + 81

def find_singular_points(eq):
    dF_dx = sp.diff(eq, x)
    dF_dy = sp.diff(eq, y)
    singular_points = sp.solve([eq, dF_dx, dF_dy], (x, y), dict=True)
    return singular_points

singular_points_eq1 = find_singular_points(eq1)
singular_points_eq2 = find_singular_points(eq2)
singular_points_eq3 = find_singular_points(eq3)

print("Singular points of Equation 1:", singular_points_eq1)
print("Singular points of Equation 2:", singular_points_eq2)
print("Singular points of Equation 3:", singular_points_eq3)

def plot_curve(eq, singular_points, ax, title):
    f = sp.lambdify((x, y), eq, 'numpy')
    X = np.linspace(-10, 10, 400)
    Y = np.linspace(-10, 10, 400)
    X, Y = np.meshgrid(X, Y)
    Z = f(X, Y)
    
    ax.contour(X, Y, Z, levels=[0], colors='b')
    ax.set_title(title)
    ax.set_xlabel('x')
    ax.set_ylabel('y')
    
    for pt in singular_points:
        ax.plot(pt[x], pt[y], 'ro')

fig, axs = plt.subplots(1, 3, figsize=(18, 6))

plot_curve(eq1, singular_points_eq1, axs[0], 'Equation 1')
plot_curve(eq2, singular_points_eq2, axs[1], 'Equation 2')
plot_curve(eq3, singular_points_eq3, axs[2], 'Equation 3')

plt.show()