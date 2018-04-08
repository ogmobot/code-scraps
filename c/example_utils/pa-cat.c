#include <stdio.h>

/* A simplistic implementation of cat. */
/* Does not support command line options. */

int main(int argc, char** argv) {
  FILE *input;
  char c;

  if (argc <= 1) {                   /* If no command line arguments are provided ... */
    while ((c = getchar()) != EOF) {
      putchar(c);
    }
  } else {
    for (int i = 1; i < argc; i++) { /* For each argument ... */
      input = fopen(argv[i], "r");
      if (input == NULL) {           /* If the file couldn't be opened ... */
        if (argv[i][0] == '-' && argv[i][1] == '\0') {
          input = stdin;
        } else {
          fprintf(stderr, "%s: %s: Failed to open file\n", argv[0], argv[i]);
          continue;
        }
      }
      while ((c = fgetc(input)) != EOF) {
        putchar(c);
      }
      fclose(input);
    }
  }
  return 0;
}
