import random

class user:
    def __init__(self, setup):
        self.setup = setup
        self.secret = random.randint(2, setup.T.get_characteristic() - 2)
        self.public_key = setup.power(setup.get_generator(), self.secret)
        self.encryption_key = None

    def get_public_key(self):
        return self.public_key

    def set_key(self, a):
        self.encryption_key = self.setup.power(a, self.secret)

    def encrypt(self, m):
        if self.encryption_key is None:
            raise ValueError("Encryption key is not set")
        return self.encryption_key * m

    def decrypt(self, c):
        if self.encryption_key is None:
            raise ValueError("Encryption key is not set")
        return c / self.encryption_key