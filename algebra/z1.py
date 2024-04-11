class GaussInt:
    def __init__(self, re: int, im: int):
        self.re = round(re)
        self.im = round(im)

    def __str__(self):
        if self.im < 0:
            t = " - "
        else:
            t = " + "
        return f"{self.re}" + t + f"{abs(self.im)}i"

    def __sub__(self, other):
        return GaussInt(self.re - other.re, self.im - other.im)

    def __mul__(self, other):
        return GaussInt(self.re * other.re - self.im*other.im, self.im*other.re + self.re*other.im)
    
    def __truediv__(self, other):
        a,b,c,d = self.re, self.im, other.re, other.im
        den = c**2 + d**2
        x = (a*c + b*d) / den
        y = (b*c - a*d) / den
        
        q = GaussInt(x,y)
        r = self - q*other

        return [q,r]

    def __floordiv__(self, other):
        return (self / other)[0]
    
    def __mod__(self, other):
        return (self / other)[1]
    
    def norm(self):
        return self.re**2 + self.im**2
    
    def gcd(x, y):
        while y.norm() > 0:
            x,y = y, x % y
        return x

    def lcm(x, y):
        return x*y // GaussInt.gcd(x, y)

g1 = GaussInt(3,4)
g2 = GaussInt(1,3)

print("c:", GaussInt.gcd(g1, g2))
print("d:", GaussInt.lcm(g1, g2))