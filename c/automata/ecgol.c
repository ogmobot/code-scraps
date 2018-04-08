#include <stdio.h>
#include <malloc.h>
#include <unistd.h>
#include <stdlib.h>
#define TRUE_HEIGHT 16
#define BYTE_WIDTH 4
#define TRUE_WIDTH (4*BYTE_WIDTH)
#define S_LIVE "# "
#define S_DEAD "  "

/* (Memory-)Efficient C Game of Life */

/* A lot of rules hard-coded in, sorry... */

char getcellvalue(char* grid, unsigned int row, unsigned int col) {
  return (grid[(BYTE_WIDTH * row) + (col >> 2)] >> (2*(col % 4))) % 4;
}

void initialise(char* grid) {
  unsigned int i;
  for (i = 0; i < TRUE_HEIGHT*BYTE_WIDTH; i++) {
    grid[i] = (char) random();
  }
  return;
}

void update(char* grid) {
  char sum;
  for (i = 0; i < TRUE_HEIGHT; i++) {
    for (j = 0; j < TRUE_WIDTH; j++) {
      sum = 0;
      sum += getcellvalue(grid,i-1,j-1)%2;
      sum += getcellvalue(grid,i-1,j  )%2;
      sum += getcellvalue(grid,i-1,j+1)%2;
      sum += getcellvalue(grid,i  ,j-1)%2;
      sum += getcellvalue(grid,i  ,j+1)%2;
      sum += getcellvalue(grid,i+1,j-1)%2;
      sum += getcellvalue(grid,i+1,j  )%2;
      sum += getcellvalue(grid,i+1,j+1)%2;

      switch(getcellvalue % 2) {
        case 0:
          if (sum == 3) {
            /* Set cell value alive */
          }
          break;
        case 1:
          if (sum < 2 || sum > 3) {
            /* Set cell value dead */
          }
          break;
        default:
          break;
      }
    }
  }
  for (i = 0; i < TRUE_HEIGHT*TRUE_WIDTH; i++) {
    /* Cell value -= magic number (10101010) */
  }
  return;
}

void display(char* grid) {
  unsigned int i,j;
  for (i = 0; i < TRUE_HEIGHT; i++) {
    for (j = 0; j < TRUE_WIDTH; j++) {
      printf("%s", getcellvalue(grid,i,j) % 2 ? S_LIVE : S_DEAD);
    }
    printf("\n");
  }
  printf("\n");
  return;
}

int main() {
  char* grid;
  unsigned int i;

  grid = malloc(TRUE_HEIGHT*BYTE_WIDTH*sizeof(char));
  initialise(grid);

  for (i = 0; i < 10; i++) {
    update(grid);
    display(grid);
    usleep(100000);
  }
  return 0;
}
