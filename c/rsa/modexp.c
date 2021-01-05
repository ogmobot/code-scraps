/*
#include <gmp.h>

unsigned long modexp(unsigned long b, unsigned int e, unsigned long m) {
	unsigned long result;
	mpz_powm(&result, b, (unsigned long int) e, m);
	return result;
}
*/

/* Method of repeated squares */
unsigned long long modexp(unsigned long long b, unsigned long long e, unsigned long long m) {
	unsigned long long result = 1;
    while (e > 0) {
        if (e % 2) result = (result * b) % m;
        e >>= 1;
        b = (b * b) % m;
    }
    return result;
}

/*
int main() {
	printf("%lu\n", modexp(4, 13, 497));
	return 0;
}
*/
