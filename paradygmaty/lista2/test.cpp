#include <iostream>
#include "gf.hpp"

int main() {
    // Addition
    GF a(3);
    GF b(7);
    GF c(10);

    std::cout << "\na = GF(3)\nb = GF(7)\nc = GF(10)\n";
    std::cout << "a + b = " << a+b << "\n";
    a += b;
    std::cout << "a += b || a = " << a << "\n";
    std::cout << "a == c " << (a==c) << "\n";

    // Subtraction
    a = GF(915);
    b = GF(916);
    c = GF(1234576);

    std::cout << "\na = GF(915)\nb = GF_(916)\nc = GF_(1234576)\n";
    std::cout << "a - b = " << a-b << "\n";
    std::cout << "a - b == c " << (a-b==c) << "\n";
    a -= b;
    std::cout << "a -= b || a = " << a << "\n";
    std::cout << "a == c " << (a == c) << "\n";

    // Multiplication
    a = GF(12);
    b = GF(88);
    c = GF(1056);

    std::cout << "\na = GF(12)\nb = GF(88)\nc = GF(1056)\n";
    std::cout << "a * b = " << a*b << "\n";
    std::cout << "a * b == c " << (a*b==c) << "\n";
    a *= b;
    std::cout << "a *= b || a = " << a << "\n";
    std::cout << "a == c " << (a == c) << "\n";
    std::cout << "a > b " << (a > b) << "\n";
    std::cout << "a >= b " << (a >= b) << "\n";
    std::cout << "a < b " << (a < b) << "\n";
    std::cout << "a <= b " << (a <= b) << "\n";

    // Division
    a = GF(88);
    b = GF(11);
    c = GF(8);

    std::cout << "\na = GF(88)\nb = GF(11)\nc = GF(8)\n";
    std::cout << "a / b = " << a/b << "\n";
    std::cout << "a / b == c " << (a/b==c) << "\n";
    std::cout << "a /= b\n";
    a /= b;
    std::cout << "a == c " << (a == c) << "\n";

    return 0;
}
