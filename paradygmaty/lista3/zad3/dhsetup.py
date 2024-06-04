import math
import random

class dhsetup:
    def __init__(self, T):
        self.T = T
        self.generator = self.generate_generator()

    def is_generator(self, a, p, primes):
        for q in primes:
            if self.power(a, (p - 1) // q) == 1:
                return False
        return True

    def generate_generator(self):
        p = self.T.get_characteristic()

        primes = []
        for i in range(2, int(math.sqrt(p - 1)) + 1):
            if (p - 1) % i == 0:
                primes.append(i)
                if i != (p - 1) // i:
                    primes.append((p - 1) // i)

        while True:
            gen_value = random.randint(2, p - 1)
            a = self.T(gen_value)
            if self.is_generator(a, p, primes):
                return a

    def get_generator(self):
        return self.generator

    def power(self, a, b):
        result = self.T(1)
        while b > 0:
            if b % 2 == 1:
                result = result * a
            a = a * a
            b //= 2
        return result
