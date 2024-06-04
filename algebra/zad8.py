from z2 import CustomPoly

a = CustomPoly([1,0,1])
b = CustomPoly([1,2,1])
d,s,t = CustomPoly.extended_gcd(a,b)
x = a*s + b*t

print(x == d)
if d == x:
    print("test passed")
    print(f"d = {d}")
    print(f"s = {s}")
    print(f"t = {t}")