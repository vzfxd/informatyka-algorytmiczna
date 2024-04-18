#include "stddef.h"
#include "stdio.h"
#include "libs/biblio.h"


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
