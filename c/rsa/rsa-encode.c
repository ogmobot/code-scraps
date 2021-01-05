#include <stdio.h>

extern unsigned long long modexp(unsigned long long b, unsigned long long e, unsigned long long m);

int main() {
	printf("%llu\n", modexp(4, 13, 497)); /* Should print 445 */
	return 0;
}
