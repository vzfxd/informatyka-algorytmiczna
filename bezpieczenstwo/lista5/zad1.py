class RC4:
    def __init__(self, key):
        self.key = [ord(c) for c in key]
        self.S = self.KSA()

    def KSA(self):
        key_length = len(self.key)
        S = list(range(256))
        j = 0
        for i in range(256):
            j = (j + S[i] + self.key[i % key_length]) % 256
            S[i], S[j] = S[j], S[i]
        return S

    def PRGA(self):
        S = self.S[:]
        i = 0
        j = 0
        while True:
            i = (i + 1) % 256
            j = (j + S[i]) % 256
            S[i], S[j] = S[j], S[i]
            K = S[(S[i] + S[j]) % 256]
            yield K

    def encrypt(self, plaintext):
        keystream = self.PRGA()
        ciphertext = []
        for c in plaintext:
            val = ("%02X" % (ord(c) ^ next(keystream)))
            ciphertext.append(val)
        return ''.join(ciphertext)

    def decrypt(self, ciphertext):
        keystream = self.PRGA()
        ciphertext_bytes = [int(ciphertext[i:i+2], 16) for i in range(0, len(ciphertext), 2)]
        return ''.join([chr(byte^next(keystream)) for byte in ciphertext_bytes])
    

if __name__ == "__main__":

    key1 = "secretkey"
    key2 = "differentkey"

    plaintext = "Hello, World!"

    rc4_key1 = RC4(key1)
    rc4_key2 = RC4(key2)

    encrypted1 = rc4_key1.encrypt(plaintext)
    encrypted2 = rc4_key2.encrypt(plaintext)

    print(f"Encrypted with key1:{encrypted1}")
    print(f"Encrypted with key2:{encrypted2}")


    decrypted1 = rc4_key1.decrypt(encrypted1)
    decrypted2 = rc4_key2.decrypt(encrypted2)

    print(f"decrypted with key1:{decrypted1}")
    print(f"decrypted with key2:{decrypted2}")

