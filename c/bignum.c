#include <stdio.h>
#include <malloc.h>
#define MAXCELLVALUE 10000000000
#define CELLPRINTLEN 10 /* Each cell is 10 decimals long */

/*
 * Bignums are kept as linked lists of long integers.
 * First elements are more significant than later elements.
 * Each element is MAXCELLVALUE times more significant than the next.
 */

/* Structures and helper functions */
typedef struct bignum {
  char sign;
  struct node* next;
} bignum;

typedef struct node {
  unsigned long int n;
  struct node* next;
} node;

bignum* create_bignum(long n) {
  bignum* new_bignum = (bignum*) malloc(sizeof(bignum));
  if (n < 0) {
    new_bignum->sign = -1;
    n = -n;
  } else {
    new_bignum->sign = 1;
  }
  new_bignum->next = create_node((unsigned long) n);
  return new_bignum;
}

static node* create_node(long n) {
  node* new_node = (node*) malloc(sizeof(node));
  new_node->n = n;
  new_node->next = NULL;
  return new_node;
}

/* Input/output functions */

void bignum2str(bignum* num, charbuffer) {
  char* cursor = charbuffer;
  node* printnode;
  /* Print sign */
  if (num->sign == -1) {
    sprintf("-", charbuffer);
    cursor++;
  }
  printnode = num->next;
  /* Print first int without zeros */
  printf("%u", printnode->n, charbuffer);
  printnode = printnode->next;
  /* Print subsequent ints with zeros if necessary */
  for ( ; printnode != NULL; printnode = printnode->next) {
    /*TODO*/
    sprintf("something");
  }
  return;
}

int main() {
  return 0;
}
