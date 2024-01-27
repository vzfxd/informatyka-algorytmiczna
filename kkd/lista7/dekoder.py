import sys
from koder import bitstring_to_file

H_matrix =[
            [0, 0, 1, 0, 1, 1, 1],
            [0, 1, 0, 1, 1, 1, 0],
            [1, 0, 1, 1, 1, 0, 0]
          ]

two_err = 0
def decode(coded):
    global two_err
    
    coded_int = [int(coded[i]) for i in range(len(coded)-1)]
    parity = sum(coded_int) % 2
    old_parity = int(coded[7])
    decoded = [0 for _ in range(3)]

    for r in range(3):
        for c in range(7):
            decoded[r] += H_matrix[r][c] * coded_int[c]
        decoded[r] %= 2

    #bin to dec
    syndrome = decoded[0]*4 + decoded[1]*2 + decoded[2]  

    if(parity != old_parity):
        if(syndrome != 0):
            idx_map = [0,0,1,3,6,2,5,4]
            idx = idx_map[syndrome]
            coded_int[idx] = (coded_int[idx]+1)%2
    elif(syndrome != 0):
        two_err += 1
        return "0000"

    return str(coded_int[0]) + str((coded_int[1]-coded_int[0])%2) + str(coded_int[5]) + str(coded_int[6])

def decode_file(file_in, file_out):
    bs = read_file(file_in)
    decoded = ''.join([decode(bs[i:i+8]) for i in range(0, len(bs), 8)])
    bitstring_to_file(decoded, file_out)
    print(f"Two errors occured {two_err} times")

def read_file(filename):
    with open(filename, "br") as f:
        result = ''.join([bin(c)[2:].zfill(8) for c in f.read()])
    return result

def main():
    if len(sys.argv) < 3:
        print("Proper usage: python decoder.py <file_in> <file_out>")
        return
    else:
        decode_file(sys.argv[1], sys.argv[2])

if __name__ == "__main__":
    main()