#ifndef GF_HPP
#define GF_HPP

#include <stdexcept>
#include <iostream>

class GF {
private:
    int64_t val;
    int64_t characteristic;

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

    static void check_err(const GF& c1, const GF& c2) {
        if (c1.characteristic != c2.characteristic)
            throw std::invalid_argument("Characteristics of GF elements do not match.");
    }

public:
    GF(int64_t val, int64_t characteristic = 1234577) : val(val % characteristic), characteristic(characteristic) {}

    int64_t getVal() const { return val; }
    int64_t getCharacteristic() const { return characteristic; }

    bool operator<(const GF& other) const {
        check_err(*this, other);
        return val < other.val;
    }

    bool operator>(const GF& other) const {
        check_err(*this, other);
        return val > other.val;
    }

    bool operator<=(const GF& other) const {
        check_err(*this, other);
        return val <= other.val;
    }

    bool operator>=(const GF& other) const {
        check_err(*this, other);
        return val >= other.val;
    }

    bool operator==(const GF& other) const {
        check_err(*this, other);
        return val == other.val;
    }

    bool operator!=(const GF& other) const {
        check_err(*this, other);
        return val != other.val;
    }

    GF operator+(const GF& other) const {
        check_err(*this, other);
        return GF((val + other.val) % characteristic, characteristic);
    }

    GF operator-(const GF& other) const {
        check_err(*this, other);
        return GF((val + (other.characteristic - other.val)) % characteristic, characteristic);
    }

    GF operator*(const GF& other) const {
        check_err(*this, other);
        return GF((val * other.val) % characteristic, characteristic);
    }

    GF operator/(const GF& other) const {
        check_err(*this, other);
        int64_t x,y;
        int64_t gcd = extended_gcd_recursive(other.val, other.characteristic ,&x,&y);
        
        if (gcd != 1)
            throw std::invalid_argument("err");

        if(x < 0){
            x = other.characteristic - abs(x);
        }

        return GF((val * (x % other.characteristic)) % characteristic, characteristic);
    }

    GF& operator+=(const GF& other) {
        val = (val + other.val) % characteristic;
        return *this;
    }

    GF& operator-=(const GF& other) {
        val = (val + (characteristic - other.val)) % characteristic;
        return *this;
    }

    GF& operator*=(const GF& other) {
        val = (val * other.val) % characteristic;
        return *this;
    }

    GF& operator/=(const GF& other) {
        int64_t x,y;
        int64_t gcd = extended_gcd_recursive(other.val, other.characteristic,&x,&y);
        if(x < 0){
            x = other.characteristic - abs(x);
        }
        val = (val * (x % other.characteristic)) % characteristic;
        return *this;
    }

    std::ostream& operator<<(std::ostream& os) {
        os << val << "," << characteristic;
        return os;
    }
};

std::ostream& operator<<(std::ostream& os, const GF& gf)
{
    os << gf.getVal() << ',' << gf.getCharacteristic();
    return os;
}

#endif // GF_HPP
