#include <stdio.h>

char DIGITS[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

void itob(unsigned int n, char* s, unsigned int b) {
  unsigned int i, j;
  char c;
  for (i = 0; n > 0; i++) {
    if ((n % b) >= 36) {
      fprintf(stderr, "Warning: unprintable digit (%u).\n", n % b);
      s[i] = '*';
    } else {
      s[i] = DIGITS[n % b];
    }
    n /= b;
  }
  s[i] = '\0';
  i--;
  for (j = 0; j < i; j++, i--) {
    c = s[j];
    s[j] = s[i];
    s[i] = c;
  }
  return;
}

int main() {
  int n, b;
  char s[64];
  printf("Number: ");
  scanf("%u", &n);
  printf("Base: ");
  scanf("%u", &b);
  itob(n, s, b);
  printf("%s\n", s);
  return 0;
}
