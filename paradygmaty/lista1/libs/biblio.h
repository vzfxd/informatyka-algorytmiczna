#ifndef biblio
#define biblio

#include "stdint.h"
#include <stddef.h>

struct coefficients {
    int64_t x;
    int64_t y;
};

size_t factorial_iterative(size_t n);
size_t factorial_recursive(size_t n);
size_t gcd_iterative(size_t a, size_t b);
size_t gcd_recursive(size_t a, size_t b);
struct coefficients diophantine_solution_recursive(int64_t a, int64_t b, int64_t c);
struct coefficients diophantine_solution_iterative(int64_t a, int64_t b, int64_t c);


#endif