from rsa import RSA

def find_phi(n, e, d):
    p,q = 0,0
    kphi = d * e - 1
    t = kphi
    while t % 2 == 0:
        t //= 2
    a = 2
    while a < 100:
        k = t
        while k < kphi:
            x = pow(a, k, n)
            if x != t and x != n-1 and pow(x, 2, n) == 1:
                p = RSA.gcd(x-1, n)
                break
            k *= 2
        a += 2
    q = n//p
    return (p - 1) * (q - 1) // RSA.gcd(p-1, q-1)

# p = 100003
# q = 104729

p = 5737
q = 6547
e1 = 17
e2 = 23

x = RSA(p,q,e1)
y = RSA(p,q,e2)

n = x.pk[1]
eA = x.pk[0]
dA = x.sk[0]

eB = y.pk[0]

print(x)
print(y)

phiB = find_phi(n,eA,dA)
gcd,dB,_ = RSA.euklides(eB,phiB)
dB = dB % phiB
skB = (dB,n)

print(skB)

                