/**********************************************************************
*  jlp_ctime.h 
*
* Contains:
* int JLP_CTIME(string,istat)
* int JLP_TIMER(double *cpu_time)
*
*  JLP
* Version 05-05-2013
**********************************************************************/
#include <stdio.h> 
#include <time.h> 
#include "sys/time.h" 
#include <string.h> 
#include "jlp_ctime.h"

/***************************************************************** 
* JLP_TIME_MSEC  to get the time in micro-seconds since
* Epoch 1970 01 01 00:00:00 +0000 (UTC):
******************************************************************/
int JLP_TIME_MSEC(char *string, int *istat)
{
 char str[22];
 struct timeval tt;
  JLP_CTIME(str, istat);
  gettimeofday(&tt, NULL);
// tt_tv_sec: time as the number of seconds 
// since the Epoch 1970-01-01 00:00:00(UTC)
  sprintf(string, "%s [%ld.%06ld sec]", str, tt.tv_sec, tt.tv_usec);
  return(0);
}
/***************************************************************** 
* JLP_CTIME  to get the date and time:
******************************************************************/
int JLP_CTIME(char *string, int *istat)
{
// JLP2015:
time_t current_time;
char *p;

  *istat=1;
  time(&current_time);
  if( (p=ctime(&current_time)) != NULL) {
       *istat=0; 
       strncpy(string, p, 20);
       string[19] = '\0';
   } 
return(0);
}
/*****************************************************************
* JLP_TIMER to compute the cpu time
*
* Usage:
* First call to initialize the timer
* Second call to read the value of the elapsed time
******************************************************************/
 int JLP_TIMER(double *cpu_time)
 {
   *cpu_time = (double)clock() / CLOCKS_PER_SEC;
   return(0);
 }
