class GF:
    def __init__(self, val: int, characteristic = 1234577 ):
        self.characteristic = characteristic
        self.val = val % characteristic

    def __str__(self):
        return str(f"{self.val}, {self.characteristic}")
    
    def __lt__(self, other):
        GF.__check_err(self, other)
        return self.val < other.val

    def __gt__(self, other):
        GF.__check_err(self, other)
        return self.val > other.val

    def __le__(self, other):
        GF.__check_err(self, other)
        return self.val <= other.val

    def __ge__(self, other):
        GF.__check_err(self, other)
        return self.val >= other.val

    def __eq__(self, other):
        GF.__check_err(self, other)
        return self.val == other.val

    def __ne__(self, other):
        GF.__check_err(self, other)
        return self.val != other.val

    def __add__(self, other):
        GF.__check_err(self, other)
        return GF( (self.val + other.val ) % self.characteristic, self.characteristic )

    def __sub__(self, other):
        GF.__check_err(self, other)
        return GF( (self.val + (-other.val % other.characteristic) ) % self.characteristic, self.characteristic )

    def __mul__(self, other):
        GF.__check_err(self, other)
        return GF( (self.val * other.val ) % self.characteristic, self.characteristic )

    def __truediv__(self, other):
        GF.__check_err(self, other)
        gcd, x, _ = GF.__euklides(other.val, other.characteristic)
        if(gcd != 1):
            return -1
        
        return GF( (self.val * (x % other.characteristic)) % self.characteristic, self.characteristic )

    def __euklides(a,b):
        if b == 0:
            return a, 1, 0
        else:
            d, x, y = GF.__euklides(b, a % b)
        return d, y, x - (a // b) * y
    
    def __check_err(c1, c2):
        if(c1.characteristic != c2.characteristic):
            raise ValueError