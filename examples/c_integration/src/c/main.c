#include <stdio.h>
#include <stdint.h>

/* Declare the C ABI compatible factorial function */
extern int64_t factorial(int64_t n);

int main() {
    printf("Flux factorial via C integration\n");
    printf("==================================\n\n");

    for (int i = 0; i <= 10; i++) {
        int64_t result = factorial(i);
        printf("factorial(%d) = %ld\n", i, result);
    }

    printf("\n");
    printf("factorial(15) = %ld\n", factorial(15));
    printf("factorial(20) = %ld\n", factorial(20));

    return 0;
}
