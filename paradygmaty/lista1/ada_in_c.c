#include <stdint.h>
#include <stdio.h>
#include "libs/biblio.h"

extern uint64_t ada_factorial_iterative(size_t n);
extern uint64_t ada_factorial_recursive(size_t n);
extern uint64_t ada_gcd_iterative(size_t a, size_t b);
extern uint64_t ada_gcd_recursive(size_t a, size_t b);
extern struct coefficients ada_diophantine_solution_iterative(int64_t a, int64_t b, int64_t c);
extern struct coefficients ada_diophantine_solution_recursive(int64_t a, int64_t b, int64_t c);



size_t factorial_iterative(size_t n)
{
    return ada_factorial_iterative(n);
}

size_t factorial_recursive(size_t n)
{
    return ada_factorial_recursive(n);
}

size_t gcd_iterative(size_t a, size_t b)
{
    return ada_gcd_iterative(a, b);
}

size_t gcd_recursive(size_t a, size_t b)
{
    return ada_gcd_recursive(a, b);
}

struct coefficients diophantine_solution_iterative(int64_t a, int64_t b, int64_t c){
    return ada_diophantine_solution_iterative(a, b ,c);
}

struct coefficients diophantine_solution_recursive(int64_t a, int64_t b, int64_t c){
    return ada_diophantine_solution_recursive(a, b ,c);
}

int main(void)
{
    size_t f = factorial_iterative(3);
    size_t f2 = factorial_recursive(3);
    size_t g = gcd_iterative(54,36);
    size_t g2 = gcd_recursive(54,36);
    struct coefficients c = diophantine_solution_iterative(10,6,14);
    struct coefficients c2 = diophantine_solution_recursive(10,6,14);

    printf("3! || %ld, %ld\n", f, f2);
    printf("gcd(54,36) || %ld, %ld\n", g, g2);
    printf("10x + 6y = 14 || %ld, %ld || %ld, %ld\n", c.x, c.y, c2.x, c2.y);
}