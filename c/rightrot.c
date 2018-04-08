#include <stdio.h>

/*
 * Write a function rightrot(x,n) that
 * returns the value of the integer x
 * rotated to the right by n bit
 * positions.
 */

unsigned int rightrot(unsigned int x, unsigned int n) {
  unsigned int rightmost, bithack, i;
  bithack = ~0;            /*11111111*/
  bithack = bithack >> 1;  /*01111111*/
  bithack = ~bithack;      /*10000000*/
  /*printf("bithack is 0x%x\n", bithack);*/
  for (i = 0; i < n; i++) {
    /*printf("%x\n", x);*/
    rightmost = x & 1;
    x = x >> 1;
    if (rightmost) {
      x = x | bithack;
    }
  }
  return x;
}

int main() {
  unsigned int x;
  unsigned int n;
  printf("x: ");
  scanf("%u", &x);
  printf("x is 0x%x.\n", x);
  printf("n: ");
  scanf("%u", &n);
  printf("Rotating %x by %u bits.\n", x, n);
  printf("0x%x\n", rightrot(x, n));
  return 0;
}
