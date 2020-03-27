#include <stdio.h>
#include "jlp_ctime.h"
int main(int argc, char *argv[])
{
char str[64];
int status;
long int i;
for (i=0; i < 10; i++) {
  JLP_TIME_MSEC(str, &status);
  printf("Result=>%s< \n", str);
  }
}
