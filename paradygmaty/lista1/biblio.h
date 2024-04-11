#ifndef biblio
#define biblio

#include "stddef.h"

struct coefficients {
    int a;
    int b;
    int c;
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

size_t extended_gcd_recursive(int a, int b, int *x, int *y)
{
    if (b == 0) {
        *x = 1;
        *y = 0;
        return a;
    } else {
        int x1, y1;
        size_t d = extended_gcd_recursive(b, a % b, &x1, &y1);
        *x = y1;
        *y = x1 - (a / b) * y1;
        return d;
    }
}

size_t extended_gcd_iterative(int a, int b, int *x, int *y)
{
    int x0 = 1, y0 = 0, x1 = 0, y1 = 1;
    while (b != 0) {
        int q = a / b;
        int temp = b;
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

void diophantine_solution(int a, int b, int c, size_t (*gcd_func)(int,int,int,int))
{
    int x, y;
    int d = gcd_func(a, b, &x, &y);
    if (c % d != 0) {
        printf("No solution exists for the given equation.\n");
    } else {
        int factor = c / d;
        x *= factor;
        y *= factor;
        printf("x = %d, y = %d is a solution to %dx + %dy = %d\n", x, y, a, b, c);
    }
}

#endif