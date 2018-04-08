#include <stdio.h>

extern unsigned long long fibonacci(unsigned long long n);

int main() {
  unsigned long long n;
  scanf("%llu", &n);
  n = fibonacci(n);
  printf("%llu\n", n);
  return 0;
}
