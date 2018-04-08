/*
#include <gmp.h>

unsigned long modexp(unsigned long b, unsigned int e, unsigned long m) {
	unsigned long result;
	mpz_powm(&result, b, (unsigned long int) e, m);
	return result;
}
*/
unsigned long modexp(unsigned long b, unsigned int e, unsigned long m) {
	unsigned long c = 1;
	unsigned int i;
	for (i=0; i<e; i++) {
		c = (b * c) % m;
	}
	return c;
}

/*
int main() {
	printf("%lu\n", modexp(4, 13, 497));
	return 0;
}
*/
