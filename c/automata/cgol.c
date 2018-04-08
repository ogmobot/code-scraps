#include <stdio.h>

#define INITFILEPATH "/home/paul/prog/c/automata/.cgol.init"

struct {
  unsigned int : 8;
} s0rules, s1rules;

int main() {
  int rules[2];
  int rows;
  int cols;
  FILE* inputfile = fopen(INITFILEPATH, "r");
  fscanf(inputfile, "%d", &rows);
  fscanf(inputfile, "%d", &cols);
  fscanf(inputfile, "%d", &(rules[0]));
  fscanf(inputfile, "%d", &(rules[1]));
  return 0;
}
