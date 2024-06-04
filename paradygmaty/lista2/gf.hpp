#ifndef GF_HPP
#define GF_HPP

#include <stdexcept>
#include <iostream>

class GF {
private:
    int64_t val;
    inline static int64_t characteristic;

    int64_t extended_gcd_recursive(int64_t a, int64_t b, int64_t *x, int64_t *y) const
    {
        if (b == 0) {
            *x = 1;
            *y = 0;
            return a;
        } else {
            int64_t x1, y1;
            int64_t d = extended_gcd_recursive(b, a % b, &x1, &y1);
            *x = y1;
            *y = x1 - (a / b) * y1;
            return d;
        }
    }

public:
    GF() {}
    GF(int64_t val) {this->val = val % GF::characteristic;}
    
    int64_t getVal() const { return val; }
    static int64_t get_characteristic() { return GF::characteristic; }
    static void set_characteristic(int64_t c) { GF::characteristic = c; }

    bool operator<(const GF& other) const {
        return val < other.val;
    }

    bool operator>(const GF& other) const {
        return val > other.val;
    }

    bool operator<=(const GF& other) const {
        return val <= other.val;
    }

    bool operator>=(const GF& other) const {
        return val >= other.val;
    }

    bool operator==(const GF& other) const {
        return val == other.val;
    }

    bool operator!=(const GF& other) const {
        return val != other.val;
    }

    GF operator+(const GF& other) const {
        return GF((val + other.val) % GF::characteristic);
    }

    GF operator-(const GF& other) const {
        return GF((val + (GF::characteristic - other.val)) % GF::characteristic);
    }

    GF operator*(const GF& other) const {
        return GF((val * other.val) % GF::characteristic);
    }

    GF operator/(const GF& other) const {
        int64_t x,y;
        int64_t gcd = extended_gcd_recursive(other.val, GF::characteristic ,&x,&y);
        
        if (gcd != 1)
            throw std::invalid_argument("err");

        if(x < 0){
            x = GF::characteristic - abs(x);
        }

        return GF((val * (x % GF::characteristic)) % GF::characteristic);
    }

    GF& operator+=(const GF& other) {
        val = (val + other.val) % GF::characteristic;
        return *this;
    }

    GF& operator-=(const GF& other) {
        val = (val + (GF::characteristic - other.val)) % GF::characteristic;
        return *this;
    }

    GF& operator*=(const GF& other) {
        val = (val * other.val) % GF::characteristic;
        return *this;
    }

    GF& operator/=(const GF& other) {
        int64_t x,y;
        int64_t gcd = extended_gcd_recursive(other.val, GF::characteristic,&x,&y);
        if(x < 0){
            x = GF::characteristic - abs(x);
        }
        val = (val * (x % GF::characteristic)) % GF::characteristic;
        return *this;
    }

    std::ostream& operator<<(std::ostream& os) {
        os << val << "," << GF::characteristic;
        return os;
    }
};

std::ostream& operator<<(std::ostream& os, const GF& gf)
{
    os << gf.getVal();
    return os;
}

#endif // GF_HPP