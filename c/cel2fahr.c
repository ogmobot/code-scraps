#include <stdio.h>

int main() {
  float cel, fahr;
  int lower, upper, step;

  lower = -40;
  upper = 110;
  step = 10;
  cel = lower;

  while (cel < upper) {
    fahr = ((9.0/5.0) * cel) + 32;
    printf("%3.0f\t%6.0f\n", cel, fahr);
    cel += step;
  }

  return 0;
}
