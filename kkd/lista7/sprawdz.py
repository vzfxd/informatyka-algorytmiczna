import sys

def compare_two_files(file1, file2):
    with open(file1, "br") as f1, open(file2, "br") as f2:
        counter = 0
        f1_chunk, f2_chunk = f1.read(1), f2.read(1)
        while f1_chunk and f2_chunk:
            f1_bits, f2_bits = bin(f1_chunk[0])[2:].zfill(8), bin(f2_chunk[0])[2:].zfill(8)
            if f1_bits[:4] != f2_bits[:4]:
                counter += 1
            if f1_bits[4:] != f2_bits[4:]:
                counter += 1
            f1_chunk, f2_chunk = f1.read(1), f2.read(1)
    return counter


def main():
    if len(sys.argv) < 3:
        print("Correct usage: python check.py <file1> <file2>")
    else:
        print(compare_two_files(sys.argv[1], sys.argv[2]))

if __name__ == "__main__":
    main()