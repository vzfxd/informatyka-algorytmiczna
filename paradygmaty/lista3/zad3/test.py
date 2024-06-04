from dhsetup import dhsetup 
from user import user
from gf import GF

GF.set_characteristic(1234567891)

setup = dhsetup(GF)
user1 = user(setup)
user2 = user(setup)

user1.set_key(user2.get_public_key())
user2.set_key(user1.get_public_key())

message = 111333
encrypted_message = user1.encrypt(message)
decrypted_message = user2.decrypt(encrypted_message)

print("Generator:", setup.get_generator())
print("Public Key User 1:", user1.get_public_key())
print("Public Key User 2:", user2.get_public_key())

print("msg:", message)
print("encrypted:", encrypted_message)
print("decrypted:", decrypted_message)