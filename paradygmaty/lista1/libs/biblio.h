#ifndef biblio
#define biblio

#include "stdint.h"
#include <stddef.h>

struct coefficients {
    int x;
    int y;
};

size_t factorial_iterative(size_t n)
{
    if(n<2) return n;

    size_t result = 1;
    for(size_t i = 1; i <= n; i++)
        result *= i;

    return result;
}

size_t factorial_recursive(size_t n)
{
    if(n<2) return n;
    return n * factorial_recursive(n-1);
}

size_t gcd_iterative(size_t a, size_t b)
{
    size_t temp;
    while (b != 0)
    {
        temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

size_t gcd_recursive(size_t a, size_t b)
{
    if (b == 0)
        return a;
    else
        return gcd_recursive(b, a % b);
}

int64_t extended_gcd_recursive(int64_t a, int64_t b, int64_t *x, int64_t *y)
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

int64_t extended_gcd_iterative(int64_t a, int64_t b, int64_t *x, int64_t *y)
{
    int64_t x0 = 1, y0 = 0, x1 = 0, y1 = 1;
    while (b != 0) {
        int64_t q = a / b;
        int64_t temp = b;
        b = a % b;
        a = temp;
        temp = x1;
        x1 = x0 - q * x1;
        x0 = temp;
        temp = y1;
        y1 = y0 - q * y1;
        y0 = temp;
    }
    *x = x0;
    *y = y0;
    return a;
}

struct coefficients diophantine_solution_recursive(int64_t a, int64_t b, int64_t c)
{
    struct coefficients coeffs;
    int64_t x, y;
    int64_t d = extended_gcd_recursive(a, b, &x, &y);
    if (c % d == 0) {
        int64_t factor = c / d;
        x *= factor;
        y *= factor;
        coeffs.x = x;
        coeffs.y = y;
    }

    return coeffs;
}

struct coefficients diophantine_solution_iterative(int64_t a, int64_t b, int64_t c)
{
    struct coefficients coeffs;
    int64_t x, y;
    int64_t d = extended_gcd_iterative(a, b, &x, &y);
    if (c % d == 0) {
        int64_t factor = c / d;
        x *= factor;
        y *= factor;
        coeffs.x = x;
        coeffs.y = y;
    }

    return coeffs;
}

#endif