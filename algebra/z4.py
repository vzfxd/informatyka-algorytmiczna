import numpy as np
import matplotlib.pyplot as plt

def r(theta):
    return np.sin(2 * theta)

theta_vals = np.linspace(0, 2 * np.pi, 1000)

r_vals = r(theta_vals)

x_vals = r_vals * np.cos(theta_vals)
y_vals = r_vals * np.sin(theta_vals)

plt.figure(figsize=(8, 8))
plt.plot(x_vals, y_vals)
plt.show()
