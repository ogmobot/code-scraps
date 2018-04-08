/* Hunt the Wumpus
 *  by Gregory Yob
 *
 * This version implemented in C
 *  (a work-in-progress)
 *  by Paul Anderson
 *  based on code found at the URL
 *  http://www.atariarchives.org/bcc1/showpage.php?page=250
 */

#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>
#include <time.h>
#define p_loc locations[PLAYER]
#define w_loc locations[WUMPUS]

enum obj_ids {
  PLAYER,
  WUMPUS,
  PITS1,
  PITS2,
  BATS1,
  BATS2
};

int d12_data[] = {
 1, 4, 7,   0, 2, 9,   1, 3,11,   2, 4,13,   0, 3, 5,
 4, 6,14,   5, 7,16,   0, 6, 8,   7, 9,17,   1, 8,10,
 9,11,18,   2,10,12,  11,13,19,   3,12,14,   5,13,15,
14,16,19,   6,15,17,   8,16,18,  10,17,19,  12,15,18
};

char get_input() {
  char char_buffer, result;
  result = getchar();
  /*Flush input buffer*/
  while ((char_buffer = getchar()) != '\n' && char_buffer != EOF) {}
  return result;
}

/* set up cave (dedecahedral node list) */
typedef struct node { int tun[3]; } node;
node* init_caves () { 
  node* caves = (node*) calloc(20, sizeof(node)); 
  for (int x = 0, r = 0; x < 20; x++, r = x * 3) { 
    for (int c = r, d = 0; c < r + 3; c++, d++) {
      caves[x].tun[d] = d12_data[c];
    }
  }
  return caves;
}

/* instructions */
void instructions() {
  char* s = "WELCOME TO 'HUNT THE WUMPUS'\n\
  THE WUMPUS LIVES IN A CAVE OF 20 ROOMS. EACH ROOM\n\
HAS 3 TUNNELS LEADING TO OTHER ROOMS. (LOOK AT A\n\
DODECAHEDRON TO SEE HOW THIS WORKS-IF YOU DON'T KNOW\n\
WHAT A DODECAHEDRON IS, ASK SOMEONE)\n\
\n\
     HAZARDS:\n\
 BOTTOMLESS PITS - TWO ROOMS HAVE BOTTOMLESS PITS IN THEM\n\
     IF YOU GO THERE, YOU FALL INTO THE PIT (& LOSE!)\n\
 SUPER BATS - TWO OTHER ROOMS HAVE SUPER BATS. IF YOU\n\
     GO THERE, A BAT GRABS YOU AND TAKES YOU TO SOME OTHER\n\
     ROOM AT RANDOM. (WHICH MIGHT BE TROUBLESOME)\n\
\n\
     WUMPUS:\n\
 THE WUMPUS IS NOT BOTHERED BY THE HAZARDS (HE HAS SUCKER\n\
 FEET AND IS TOO BIG FOR A BAT TO LIFT).  USUALLY\n\
 HE IS ASLEEP. TWO THINGS WAKE HIM UP: YOUR ENTERING\n\
 HIS ROOM OR YOUR SHOOTING AN ARROW.\n\
     IF THE WUMPUS WAKES, HE MOVES (P=.75) ONE ROOM\n\
 OR STAYS STILL (P=.25). AFTER THAT, IF HE IS WHERE YOU\n\
 ARE, HE EATS YOU UP (& YOU LOSE!)\n\
\n\
     YOU:\n\
 EACH TURN YOU MAY MOVE OR SHOOT A CROOKED ARROW\n\
   MOVING: YOU CAN GO ONE ROOM (THRU ONE TUNNEL)\n\
   ARROWS: YOU HAVE 5 ARROWS. YOU LOSE WHEN YOU RUN OUT.\n\
   EACH ARROW CAN GO FROM 1 TO 5 ROOMS. YOU AIM BY TELLING\n\
   THE COMPUTER THE ROOM#S YOU WANT THE ARROW TO GO TO.\n\
   IF THE ARROW CAN'T GO THAT WAY (IE NO TUNNEL) IT MOVES\n\
   AT RAMDOM TO THE NEXT ROOM.\n\
     IF THE ARROW HITS THE WUMPUS, YOU WIN.\n\
     IF THE ARROW HITS YOU, YOU LOSE.\n\
\n\
    WARNINGS:\n\
     WHEN YOU ARE ONE ROOM AWAY FROM WUMPUS OR HAZARD,\n\
    THE COMPUTER SAYS:\n\
 WUMPUS-  'I SMELL A WUMPUS'\n\
 BAT   -  'BATS NEARBY'\n\
 PIT   -  'I FEEL A DRAFT'\n\
\n";
  printf("%s", s);
  return;
}

int main() {
  unsigned int seed = 0;
  char use_same_seed = 'n',
       play_again = 'y',
       action = 0,
       finished = 0;
  int room_select, i;
  node* caves;
  caves = init_caves();
  int locations[6] = {0};

  while (play_again == 'y') {
    /* Pre-game */
    if (use_same_seed != 'y') {
      seed = time(NULL);
    }
    srand(seed);
    for (i = 0; i < 6; i++) {
      locations[i] = rand()%20;
    }
    /* Game */
    finished = 0;
    while (!finished) {
      /* Get current state */
      printf("YOU ARE IN ROOM %d\n", p_loc);
      printf("TUNNELS LEAD TO %d %d %d\n",
             caves[p_loc].tun[0],
             caves[p_loc].tun[1],
             caves[p_loc].tun[2]);
      /* Get action from player */
      action = 0;
      while (action != 's' && action != 'm') {
        printf("SHOOT OR MOVE (S-M)\n");
        action = get_input();
      }
      if (action == 's') {
        /*Arrow routine*/
      }
      if (action == 'm') {
        /*Move routine*/
        room_select = 20;
        while (1) {
          printf("WHERE TO\n");
          scanf("%d", &room_select);
          if (room_select == p_loc) { break; }
          if (room_select < 0 || room_select > 19) { continue; }
          for (i = 0; i < 3; i++) {
            if (caves[p_loc].tun[i] == room_select) {
              break;
            }
          }
          if (i < 3) { break; }
          printf("NOT POSSIBLE -\n");
        }
        p_loc = room_select;
        if (p_loc == w_loc) {
          printf("...OOPS! BUMPED A WUMPUS!\n");
          finished = 1;
          break;
        }
      }
      /* Update game state */
      finished = 1;
      /* Check for bats */
      if (p_loc == locations[BATS1] || p_loc == locations[BATS2]) {
        break;
      }
      /* Check if game is over */
      if (p_loc == w_loc) {
        finished = 1;
      }
      if (p_loc == locations[PITS1] || p_loc == locations[PITS2]) {
        finished = 1;
        printf("YYYIIIIEEEE . . . FELL IN PIT\n");
      }
    }
    /* Post-game */
    printf("Play again?\n");
    play_again = get_input();
    printf("Use same seed?\n");
    use_same_seed = get_input();
  }
  free(caves);
  return 0;
}
