import sympy as sp
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

u, v, x, y, z = sp.symbols('u v x y z')

param_eqs = [x - u * v, y - v, z - u**2]

ideal = sp.groebner(param_eqs, u, v, x, y, z, order='lex')

print("Groebner basis:")
for poly in ideal:
    print(poly)

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

Y = np.linspace(-10, 10, 400)
Z = np.linspace(-10, 10, 400)
Y, Z = np.meshgrid(Y, Z)
X = np.sqrt(Y**2 * Z)

ax.plot_surface(X, Y, Z, alpha=0.5, rstride=100, cstride=100)
ax.plot_surface(-X, Y, Z, alpha=0.5, rstride=100, cstride=100)

ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('Z')
ax.set_title(r'Surface $x^2 - y^2z = 0$')

plt.show()

F = x**2 - y**2 * z
dF_dx = sp.diff(F, x)
dF_dy = sp.diff(F, y)
dF_dz = sp.diff(F, z)

singular_points = sp.solve([F, dF_dx, dF_dy, dF_dz], (x, y, z), dict=True)
print("\nSingular points of the variety V(x^2 - y^2z):")
for pt in singular_points:
    print(pt)