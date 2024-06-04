#pragma once
#include "dhsetup.hpp"

template<typename T>
class user {
private:
    unsigned long secret;
    T publicKey;
    T privateKey;
    bool keySet;

public:
    user(dhsetup<T>& setup) : keySet(false)
    {
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<unsigned long> dis(2, T::get_characteristic() - 2);
        secret = dis(gen);

        publicKey = setup.get_generator();
        publicKey = dhsetup<T>::power(publicKey, secret);
    }

    T getPublicKey()
    {
        return publicKey;
    }

    void setKey(T a)
    {
        privateKey = dhsetup<T>::power(a, secret);
        keySet = true;
    }

    T encrypt(T m)
    {
        if (!keySet) {
            std::cerr << "Error: Klucz nie został jeszcze ustawiony!" << std::endl;
            return T();
        }
        return m * privateKey;
    }

    T decrypt(T c)
    {
        if (!keySet) {
            std::cerr << "Error: Klucz nie został jeszcze ustawiony!" << std::endl;
            return T();
        }
        return c / privateKey;
    }
};