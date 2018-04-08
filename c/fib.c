#include <stdio.h>

int main() {
  unsigned long long x, y, z;
  unsigned int a, i;
  scanf("%u", &a);
  a--;
  x = 0;
  y = 1;
  for (i = 0; i < a; i++) {
    z = x + y;
    x = y;
    y = z;
  }
  printf("%llu\n", y);
  return 0;
}
