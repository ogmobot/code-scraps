#include <stdio.h>
#include <time.h>

int main() {
  time_t current, target;
  struct tm cal_target;
  char* timestr;
  int total, d_sec, d_min, d_hour, d_day;
  cal_target.tm_year = 118; /* Years since 1900 */
  cal_target.tm_mon = 4;    /* (0, 11) */
  cal_target.tm_mday = 22;  /* (1, 31) */
  cal_target.tm_hour = 12;  /* (0, 23) */
  cal_target.tm_min = 0;    /* (0, 59) */
  cal_target.tm_sec = 0;    /* (0, 61) */

  current = time(NULL);
  target = mktime(&cal_target);

  total = (int) difftime(target, current);
  d_sec = total % 60;
  d_min = (total / 60) % 60;
  d_hour = (total / 3600) % 24;
  d_day = (total / 86400);

  timestr = asctime(&cal_target);
  printf("Time until %s", timestr);
  printf("%d days, %d hours, %d minutes, and %d seconds.\n",
          d_day, d_hour % 24, d_min % 60, d_sec % 60);
  return 0;
}
