import sys
import random
if(__name__ == "__main__"):
    if len(sys.argv) < 4:
        print("python3 szum.py <p> <f1> <f2>")
    else:
        p,f1,f2 = float(sys.argv[1]),sys.argv[2],sys.argv[3]
        with open(f1, "br") as f:
            data = ''.join([bin(c)[2:].zfill(8) for c in f.read()])
            data = list(data)

        for i, bit in enumerate(data):
            if random.random() < p:
                data[i] = str((int(data[i])+1)%2)
                
        with open(f2,"wb") as f:
            data = ''.join(data)
            data = bytes([int(data[i:i+8], 2) for i in range(0, len(data), 8)])
            f.write(data)