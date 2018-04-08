#include <stdio.h>

extern unsigned long modexp(unsigned long b, unsigned int e, unsigned long m);

int main() {
	printf("Foo\n");
	printf("%lu\n", modexp(4, 13, 497)); /* Should print 445 */
	return 0;
}
