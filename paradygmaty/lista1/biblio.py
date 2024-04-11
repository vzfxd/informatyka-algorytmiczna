def factorial_iterative(n):
    if(n<2): return n

    result = 1
    for i in range(1,n+1):
        result *= i
    return result

def factorial_recursive(n):
    if(n<2): return n
    return n * factorial_recursive(n-1)

def gcd_iterative(a, b):
    while b != 0:
        a, b = b, a % b
    return a

def gcd_recursive(a, b):
    if b == 0:
        return a
    else:
        return gcd_recursive(b, a % b)
    
def extended_gcd_recursive(a, b):
    if b == 0:
        return a, 1, 0
    else:
        d, x, y = extended_gcd_recursive(b, a % b)
        return d, y, x - (a // b) * y
    
def extended_gcd_iterative(a, b):
    x0, x1, y0, y1 = 1, 0, 0, 1
    while b != 0:
        q, a, b = a // b, b, a % b
        x0, x1 = x1, x0 - q * x1
        y0, y1 = y1, y0 - q * y1
    return a, x0, y0

def diophantine_solution(a, b, c, gcd_func):
    gcd, x0, y0 = gcd_func(a, b)
    if c % gcd != 0:
        return None
    else:
        factor = c // gcd
        x = x0 * factor
        y = y0 * factor
        return x, y

def main():
    print(factorial_recursive(3))
    print(factorial_iterative(3))
    print(gcd_iterative(54,36))
    print(gcd_recursive(54,36))

if __name__ == "__main__":
    main()