#include <stdio.h>

int main() {
  int c;

  while ((c=getchar()) != EOF) {
    if (c == '\t') {
      putchar('\\'); putchar('t');
    } else {
      putchar(c);
    }
  }
  printf("%d\n",c);
  return 0;
}
