import sys

# 1 + x + x^3
G_matrix = [
            [1, 0, 0, 0],
            [1, 1, 0, 0],
            [0, 1, 1, 0],
            [1, 0, 1, 1],
            [0, 1, 0, 1],
            [0, 0, 1, 0],
            [0, 0, 0, 1]
            ]

def hamming(bits):
    coded = [0 for _ in range(7)]
    for r in range(7):
        for c in range(4):
            coded [r] += G_matrix[r][c] * int(bits[c])
        coded[r] %= 2

    parity = sum(coded) % 2
    coded.append(parity)
    return ''.join(map(str,coded))


def read_file(filename):
    with open(filename, "br") as f:
        result = ''.join([bin(c)[2:].zfill(8) for c in f.read()])
    return result


def bitstring_to_file(bitstring, filename):
    with open(filename, "wb") as f:
        byte_arr = bytes([int(bitstring[i:i+8], 2) for i in range(0, len(bitstring), 8)])
        f.write(byte_arr)

def encode(bits):
    return ''.join([hamming(bits[i:i+4]) for i in range(0, len(bits), 4)])


def encode_file(file_in, file_out):
    in_bits = read_file(file_in)
    out_bits = encode(in_bits)
    bitstring_to_file(out_bits, file_out) 


def main():
    if len(sys.argv) < 3:
        print("Proper usage: python coder.py <in_file> <out_file>")
        return
    else:
        encode_file(sys.argv[1], sys.argv[2])

if __name__ == "__main__":
    main()