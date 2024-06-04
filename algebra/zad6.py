class NK:
    k = 1 
    def __init__(self, *args):
        if len(args) == 1 and isinstance(args[0], list):            
            self.tuples = args[0]
        else:
            self.tuples = list(args)
        
        if len(self.tuples) != NK.k:
            raise ValueError("Number of elements in tuple must be equal to k")
        
    def __le__(self, other):
        for i in range(NK.k):
            if self.tuples[i] > other.tuples[i]:
                return False
        return True

    def set_k(k):
        NK.k = k

    def __str__(self):
        return str(self.tuples)

    def __repr__(self):
        return str(self.tuples)

    @staticmethod
    def minimal_elements(A):
        M = set()
        for a in A:
            is_minimal = True
            M_copy = M.copy()
            for m in M_copy:
                if m <= a:
                    is_minimal = False
                    break
                if a <= m:
                    M.remove(m)

            if is_minimal:
                M.add(a)

        return M


if __name__ == "__main__":
    NK.set_k(2)
    A2 = {NK(n, k) for n in range(0, 16) for k in range(0, 16) if (n - 10)**2 + (k - 10)**2 <= 25}
    M2 = NK.minimal_elements(A2)

    A1 = {NK(n, k) for n in range(0, 16) for k in range(0, 16) if n*k >= 11}
    M1 = NK.minimal_elements(A1)

    print(f"M1 = {M1}")
    print(f"M2 = {M2}")