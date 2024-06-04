import random

class RSA:
    def __init__(self, p:int, q:int,e:int):
        if(False in [RSA.__is_prime(p),RSA.__is_prime(q)]):
            raise ValueError("p or q is not prime num")
        
        self.__generate_keys(p,q,e)

    def __generate_keys(self, p:int, q:int, e:int):
        phi = (p - 1) * (q - 1)
        n = p * q
        # e = random.randrange(2, phi-1)

        # while(RSA.gcd(e,phi) != 1):
        #     e = random.randrange(2,phi-1)

        gcd,d,_ = RSA.euklides(e,phi)
        d = d % phi

        if(gcd != 1):
            raise ValueError("gcd != 1")
        
        
        self.phi = phi
        self.sk = (d,n)
        self.pk = (e,n)

    def euklides(a,b):
        if b == 0:
            return a, 1, 0
        else:
            d, x, y = RSA.euklides(b, a % b)
        return d, y, x - (a // b) * y


    def gcd(a, b):
        while b != 0:
            a, b = b, a % b
        return a
    
    def __is_prime(x):
        if x <= 1:
            return False
        elif x <= 3:
            return True
        elif x % 2 == 0 or x % 3 == 0:
            return False
        
        i = 5
        while i * i <= x:
            if x % i == 0 or x % (i + 2) == 0:
                return False
            i += 6
        return True
    
    def __str__(self):
        return f"phi:{self.phi},sk:{self.sk},pk:{self.pk}"