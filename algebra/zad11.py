import numpy as np
import matplotlib.pyplot as plt

t = np.linspace(-100, 100, 10000)

x = (1 - t**2) / (1 + t**2)
y = (2 * t) / (1 + t**2)

plt.figure(figsize=(8, 8))
plt.plot(x, y, label=r'$\frac{1 - t^2}{1 + t^2}$, $\frac{2t}{1 + t^2}$')

plt.scatter([-1], [0], color='red', label='Point (-1, 0) not included')

plt.xlabel('x')
plt.ylabel('y')
plt.title('Parametric plot of x(t) and y(t)')
plt.axhline(0, color='black',linewidth=0.5)
plt.axvline(0, color='black',linewidth=0.5)
plt.grid(color = 'gray', linestyle = '--', linewidth = 0.5)
plt.legend()
plt.axis('equal')
plt.show()