from gf import GF

# Addition
a = GF(3)
b = GF(7)
c = GF(10)

print()
print(f"a = GF(3)\nb = GF(7)\nc = GF(10)")
print(f"a + b = {a+b}")
a += b
print(f"a += b || a = {a}")
print(f"a == c {a == c}")

# Substraction
a = GF(915)
b = GF(916)
c = GF(1234576)

print()
print(f"a = GF(915)\nb = GF(916)\nc = GF(1234576)")
print(f"a - b = {a-b}")
print(f"a - b == c {a-b==c}")
a -= b
print(f"a -= b || a = {a}")
print(f"a == c {a == c}")

# Multiplication
a = GF(12)
b = GF(88)
c = GF(1056)

print()
print(f"a = GF(12)\nb = GF(88)\nc = GF(1056)")
print(f"a * b = {a*b}")
print(f"a * b == c {a*b==c}")
a *= b
print(f"a *= b || a = {a}")
print(f"a == c {a == c}")
print(f"a > b {a > b}")
print(f"a >= b {a >= b}")
print(f"a < b {a < b}")
print(f"a <= b {a <= b}")


# Division
a = GF(88)
b = GF(11)
c = GF(8)

print()
print(f"a = GF(88)\nb = GF(11)\nc = GF(8)")
print(f"a / b = {a / b}")
print(f"a / b == c {a / b == c}")
print(f"a /= b")
a /= b
print(f"a == c {a == c}")