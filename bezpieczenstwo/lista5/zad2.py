from zad1 import RC4

def same_key(c1, c2):
    c1_bytes = [int(c1[i:i+2], 16) for i in range(0, len(c1), 2)]
    c2_bytes = [int(c2[i:i+2], 16) for i in range(0, len(c2), 2)]

    for i in range(min(len(c1_bytes),len(c2_bytes))):
        if(c1_bytes[i] ^ c2_bytes[i] >= 128):
            return False
    return True

if __name__ == "__main__":
    C = RC4("secretkey").encrypt("Hello, World!")
    C_same = RC4("secretkey").encrypt("RC4 encryption")
    C_diff = RC4("differentkey").encrypt("RC4 encryption")

    print("Ciphertexts generated using the same key:")
    print("C:", C)
    print("C_same:", C_same)
    print("Same key used:", same_key(C, C_same))

    print("\nCiphertexts generated using different keys:")
    print("C:", C)
    print("C_diff:", C_diff)
    print("Same key used:", same_key(C, C_diff))
