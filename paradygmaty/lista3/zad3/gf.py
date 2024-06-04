class GF:
    characteristic = None
    def __init__(self, val: int):
        self.val = val % GF.characteristic

    def set_characteristic(c):
        GF.characteristic = c

    def get_characteristic():
        return GF.characteristic

    def __str__(self):
        return str(f"{self.val}")
    
    def __lt__(self, other):
        return self.val < other.val

    def __gt__(self, other):
        return self.val > other.val

    def __le__(self, other):
        return self.val <= other.val

    def __ge__(self, other):
        return self.val >= other.val

    def __eq__(self, other):
        if isinstance(other, GF):
            return self.val == other.val
        elif isinstance(other, int):
            return self.val == other

    def __ne__(self, other):
        return self.val != other.val

    def __add__(self, other):
        return GF( (self.val + other.val ) % GF.characteristic)

    def __sub__(self, other):
        return GF( (self.val + (-other.val % GF.characteristic) ) % GF.characteristic )

    def __mul__(self, other):
        if isinstance(other, GF):
            return GF(self.val * other.val)
        elif isinstance(other, int):
            return GF(self.val * (other % GF.characteristic))

    def __truediv__(self, other):
        gcd, x, _ = GF.__euklides(other.val, GF.characteristic)
        if(gcd != 1):
            return -1
        
        return GF( (self.val * (x % GF.characteristic)) % GF.characteristic )

    def __euklides(a,b):
        if b == 0:
            return a, 1, 0
        else:
            d, x, y = GF.__euklides(b, a % b)
        return d, y, x - (a // b) * y