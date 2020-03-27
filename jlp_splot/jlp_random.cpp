/**************************************************
* Set of routines to generate 
* pseudo-random series of numbers
*
* Three modes according to the definition of:
* ibm
* JLP_MY_RANDOM
* standard C
*
* Contains:
* int JLP_RANDOM_INIT(long seed)
* int JLP_RANDOM(double *x)
* int JLP_RANDOM_GAUSS(double *x)
* (and main program to test...)
*
* JLP
* Version 23-07-2013
*************************************************/
/* To select which software to use: 
#define ibm 
#define JLP_MY_RANDOM
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "jlp_random.h" 


#define TWOPI 6.28318530717959
#ifdef JLP_MY_RANDOM
static long int next = 1;
#endif

/*
int JLP_RANDOM_INIT(long *seed);
int JLP_RANDOM(double *x);
int JLP_RANDOM_GAUSS(double *x);
*/

#ifdef TEST
int main(int argc, char *argv[])
{
long seed;
int i,nn;
double x, y;
double meanx, meany, mm, ss;
double meansqx, meansqy;
double sigmax, sigmay;
char string[41];

printf(" Test of jlp random generator Version 27/03/2007\n");
seed = 1;
JLP_RANDOM_INIT(&seed);
meanx = 0.;
meany = 0.;
meansqx = 0.;
meansqy = 0.;
nn = 500;
 printf(" Number of points? \n"); 
 scanf("%d",&nn);
 printf(" mean and sigma \n"); 
 scanf("%lf,%lf",&mm, &ss);
 printf(" Attempt with %d points with mean=%f and sigma=%f\n",nn, mm, ss);
for(i = 0; i < nn; i++)
{
  JLP_RANDOM(&x);
  JLP_RANDOM_GAUSS(&y);
  if(i < 20) printf(" x (uniform), y (gauss) %f %f\n",x,y);
  y = y * ss + mm;
  meanx = meanx + (double)x; 
  meany = meany + (double)y; 
  meansqx = meansqx + (double)x*(double)x; 
  meansqy = meansqy + (double)y*(double)y; 
}

  meanx = meanx / (double)((double)nn);
  meany = meany / (double)((double)nn);
  printf(" mean: (should be 0.5 and %f) %f %f \n", mm, meanx,meany);
 
  sigmax = meansqx / (double)((double)nn) - meanx * meanx;
  sigmay = meansqy / (double)((double)nn) - meany * meany;
  sigmax = sqrt(sigmax);
  sigmay = sqrt(sigmay);
  printf(" sigma: (should be ? and %f) : %f %f \n", ss, sigmax,sigmay);

return(0);
}
#endif

/************************************************
*
* To initialize the series of random numbers
************************************************/
int JLP_RANDOM_INIT(long *seed)
{

/* IBM routines */
#ifdef ibm 
srandom(*seed);
#else 

/* JLP version: (should be odd to increase the period) */
#ifdef JLP_MY_RANDOM
  next = *seed;
  if((next/2)*2 == next) next++;

/* standard, but not very good: */
#else
          srand(*seed);
#endif
#endif

return(0);
}
/************************************************
*
* To generate random numbers between 0. and 1.
************************************************/
int JLP_RANDOM(double *x)
{

#ifdef ibm 
 *x = (double)(random())/2147483647.00;

#else
/* JLP version:
*/
#ifdef JLP_MY_RANDOM
next = (125*next + 12345) % 17171717;
*x = (double)(next)/17171716.0;
printf("JLP_RANDOM/Sorry not working yet with this option \n");
exit(-1);
#else
/* standard, but not always not very good: */
       *x = (double)(rand())/(double)RAND_MAX;
#endif
#endif

return(0);
}
/************************************************
*
* To generate random numbers following
* a Gaussian probability law with sigma=1. and mean=0. 
*
* From Numerical recipees in C, p289
************************************************/
int JLP_RANDOM_GAUSS(double *x)
{
static int is_ready = 0;
double r1, r2, val1, val2, fac, rad2;

if(is_ready) {
  *x = val2; 
  is_ready = 0;
} else {

/* Obtain val1 and val2, uniform random values inside the circle 
* of radius unity */

  do {
  JLP_RANDOM(&r1);
  JLP_RANDOM(&r2);
  val1 = 2. * r1 - 1.;
  val2 = 2. * r2 - 1.;
  rad2 = val1 * val1 + val2 * val2;
  } while (rad2 >= 1.0 || rad2 == 0);

  if(r2 == 0) r2 = 1.0e-12;
  fac = sqrt(-2.*log((double)rad2) / rad2);
  val1 *= fac;
  val2 *= fac;
  *x = val1;
  is_ready = 1;
}

return(0);
}
