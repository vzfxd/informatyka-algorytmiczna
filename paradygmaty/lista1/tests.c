#include "biblio.h"
#include "stddef.h"
#include "stdio.h"


int main(void)
{
    size_t f = factorial_iterative(3);
    size_t f2 = factorial_recursive(3);
    size_t g = gcd_iterative(54,36);
    size_t g2 = gcd_recursive(54,36);

    printf("%ld, %ld\n", f, f2);
    printf("%ld, %ld\n", g, g2);
}
