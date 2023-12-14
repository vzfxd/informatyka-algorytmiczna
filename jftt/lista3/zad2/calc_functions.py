def euklides(a,P):
    if(a == 0):
        x, y = 0, 1
        return P, x, y

    gcd, x1, y1 = euklides(P % a, a)

    x = y1 - (P // a) * x1
    y = x1

    return gcd, x, y

def sub_p(a,b,P):
    return (a + (-b % P)) % P

def div_p(a,b,P):
    gcd, x, y = euklides(b, P)
    
    if(gcd != 1):
        return -1
    return (a * (x % P)) % P

def pow_p(base, exponent):
    P = 1234577
    if(exponent == 0):
        return 1

    result = 1
    base_mod = base % P

    while(exponent > 0):
        if(exponent % 2 == 1):
            result = (result * base_mod) % P

        exponent //= 2
        base_mod = (base_mod * base_mod) % P

    return result