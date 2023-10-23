import sys

def computePrefixFunction(P):
    m = len(P)
    pi = [i for i in range(m)]
    k = 0
    for q in range(1, m):
        while(k > 0 and P[k] != P[q]):
            k = pi[k-1]
        if(P[k] == P[q]):
            k = k+1
        pi[q] = k
    return pi

def kmpMatcher(T,P):
    n = len(T)
    m = len(P)
    pi = computePrefixFunction(P)
    q = 0
    for i in range(1,n+1):
        while(q>0 and P[q] != T[i-1]):
            q = pi[q-1]
        if(P[q] == T[i-1]):
            q = q + 1
        if(q==m):
            print("Pattern occurs with shift", i-m)
            q = pi[q-1]

if __name__ == "__main__":
    P = sys.argv[1]
    with open(sys.argv[2]) as file:
        T = file.read()
    print("Pattern:",P)
    kmpMatcher(T, P)


