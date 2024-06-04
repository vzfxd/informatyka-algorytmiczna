#include "dhsetup.hpp"
#include "user.hpp"
#include "../../lista2/gf.hpp"

int main() {
    GF::set_characteristic(1234567891);
    dhsetup<GF> setup;

    user<GF> userA(setup);
    GF pub_key_a = userA.getPublicKey();
    user<GF> userB(setup);
    GF pub_key_b = userB.getPublicKey();

    userA.setKey(pub_key_b);
    userB.setKey(pub_key_a);

    GF msg = 111666;
    GF encrypted = userA.encrypt(msg);
    GF decrypted = userB.decrypt(encrypted);


    std::cout << "generator: " << setup.get_generator() << std::endl;
    std::cout << "pub key a: " << userA.getPublicKey() << std::endl;
    std::cout << "pub key b: " << userB.getPublicKey() << std::endl;
    std::cout << "msg: " << msg << std::endl;
    std::cout << "encrypted: " << encrypted << std::endl;
    std::cout << "decrypted: " << decrypted << std::endl;

    return 0;
}