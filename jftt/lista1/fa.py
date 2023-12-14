import sys

def computeTransFunction(P,alph):
    m = len(P)
    transFunction = {(q,a): q for q in range(m+1) for a in alph}
    for q in range(m+1):
        for a in alph:
            k = min(m,q+1)
            while((P[:q] + a).endswith(P[:k]) == False):
                k = k - 1
            transFunction[q,a] = k
    return transFunction


def finiteAutomationMatcher(T,transFunction,m):
    q = 0
    n = len(T)
    for i in range(1,n+1):
        q = transFunction[q,T[i-1]]
        if(q == m):
            print("Pattern occurs with shift", i-m)

if __name__ == "__main__":
    P = sys.argv[1]
    with open(sys.argv[2]) as file:
        T = file.read()
    alph = set(T)
    print("Pattern:",P)
    finiteAutomationMatcher(T,computeTransFunction(P,alph),len(P))
