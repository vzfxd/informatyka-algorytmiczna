#pragma once

#include <iostream>
#include <random>
#include <vector>

template<typename T>
class dhsetup
{
private:
    T generator;

    bool isGenerator(T a, unsigned long p, const std::vector<unsigned long>& primes)
    {
        for (unsigned long q : primes)
        {
            if (dhsetup<T>::power(a, (p - 1) / q) == 1)
                return false;
        }
        return true;
    }

public:
    dhsetup()
    {
        unsigned long p = T::get_characteristic();

        std::vector<unsigned long> primes;
        for (unsigned long i = 2; i * i <= (p - 1); ++i)
        {
            if ((p - 1) % i == 0)
            {
                primes.push_back(i);
                if (i != (p - 1) / i)
                    primes.push_back((p - 1) / i);
            }
        }

        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<unsigned long> dis(2, p - 1);

        unsigned long gen_value;
        T a;
        do {
            gen_value = dis(gen);
            a = T(gen_value);
        } while (!isGenerator(a, p, primes));

        generator = a;
    }

    T get_generator()
    {
        return generator;
    }


    static T power(T a, unsigned long b)
    {
        T result = 1;
        while (b > 0)
        {
            if (b % 2 == 1)
                result = result * a;
            a = (a * a);
            b /= 2;
        }
        return result;
    }
};
