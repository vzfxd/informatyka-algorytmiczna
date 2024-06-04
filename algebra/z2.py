class CustomPoly:
    def __init__(self, coeffs):
        self.coeffs = coeffs
        self.degree = len(coeffs) - 1

    def __eq__(self, other):
        return self.coeffs == other.coeffs and self.degree == other.degree
    
    def __str__(self):
        return str(self.coeffs)

    def __add__(self, other):
        p,deg_g,deg_l = (self,self.degree,other.degree) if self.degree > other.degree else (other,other.degree,self.degree)
        coeffs = [other.coeffs[i] + self.coeffs[i] if i <= deg_l else p.coeffs[i] for i in range(deg_g+1)]
        while coeffs[-1] == 0 and len(coeffs) > 1:
             coeffs.pop()
        return CustomPoly(coeffs)


    def __sub__(self, other):
        p,deg_g,deg_l = (self,self.degree,other.degree) if self.degree > other.degree else (other,other.degree,self.degree)
        coeffs = [self.coeffs[i] - other.coeffs[i] if i <= deg_l else p.coeffs[i] if self.degree >= other.degree else -other.coeffs[i] for i in range(deg_g+1)]
        while coeffs[-1] == 0 and len(coeffs) > 1:
             coeffs.pop()
        return CustomPoly(coeffs)
    
    def __mul__(self, other):
        result_coeffs = [0 for _ in range(self.degree + other.degree + 1)]

        for i, coeff1 in enumerate(self.coeffs):
            for j, coeff2 in enumerate(other.coeffs):
                result_coeffs[i + j] += coeff1 * coeff2

        return CustomPoly(result_coeffs)

    def __truediv__(self, other):
        if other.degree == 0:
            if other.coeffs[0] == 0:
                raise ZeroDivisionError
            
            for i in range(self.degree + 1):
                self.coeffs[i] /= other.coeffs[0]
            return self, CustomPoly([0])
        
        q = CustomPoly([0])
        p = self

        while p.degree > 0 and p.degree >= other.degree:
            LT_div = CustomPoly([0 for _ in range(p.degree - other.degree)] + [p.coeffs[-1] / other.coeffs[-1]])
            q += LT_div
            p -= LT_div * other

        r = p

        return [q,r]
    
    def __mod__(self, other):
        return (self / other)[1]
    
    def __floordiv__(self, other):
        return (self / other)[0]

    def gcd(p, q):
        while q.degree > 0 or q.coeffs[0] != 0:
            p, q = q, p % q
        return p

    def lcm(p, q):
        return p*q // CustomPoly.gcd(p, q)
    
    def extended_gcd(a, b):
        print(f"{a},{b},{CustomPoly([0])}")
        if(b.degree == 0 and b.coeffs[0]==0):
            return (a,CustomPoly([1]),CustomPoly([0]))
        qr = a / b
        q = qr[0]
        r = qr[1]
        d, s, t = CustomPoly.extended_gcd(b,r)
        return (d, t, s - q*t)
    
if __name__ == "__main__":
    p1 = CustomPoly([1, 0, 1])
    p2 = CustomPoly([1, 2, 1]) 

    print("c:", CustomPoly.gcd(p1, p2))
    print("d:", CustomPoly.lcm(p1, p2))


