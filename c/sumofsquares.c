#include <stdio.h>
#define MAXNUM 100000

int main() {
  unsigned int i, j, z, t;
  char numbers[MAXNUM] = {0};
  for (i = 0; i < MAXNUM; i++) {
    for (j = 0; j < i; j++) {
      z = (i*i)+(j*j);
      if (z < MAXNUM) {
        numbers[z] = 1;
      }
    }
  }
  t = 0;
  for (i = 0; i < MAXNUM; i++) {
    if (numbers[i]) {
      t++;
      /*printf("%d\n", i);*/
    }
  }
  printf("There are %d numbers less than %d\n"
         "that can be expressed as the sum of two squares.\n",
         t, MAXNUM);
  return 0;
}
