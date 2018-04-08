#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#define STACKSIZE 200
#define STRINGSIZE 100

/* Evaluates expressions given in reverse Polish notation.
 * Use p to print the top number on the stack.
 */

/* From The C Programming Language:
 * *p++ = val;   push val onto stack
 * val = *--p;   pop top of stack into val
 */

long double the_stack[STACKSIZE];
long double* sptr = the_stack;
char input_string[STRINGSIZE];

void push(double val) {
  extern long double* sptr;
  *sptr++ = val;
  return;
}

int main() {
  long double val;
  int i;

  scanf("%s", input_string);
  while (input_string[0] != 'q') {
    scanf("%s", input_string);
  }

  return 0;
}
