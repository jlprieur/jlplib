/******************************************************************************
* Name:     jlp_process_curves.cpp 
*
* Purpose:  simple curve processing used by JLP_cGDev_wxWID and JLP_wx_cGDProc1 
*
* Author:   JLP 
* Version:  07/02/2016 
******************************************************************************/
#include <stdio.h>
#include <math.h>
#include <string.h>

#include "jlp_process_curves.h"
#include "jlp_macros.h"  // NINT, SQUARE, etc

// #define DEBUG
/*
int GetRectangleBoxLimits1(double *x1_1, double *y1_1, double *x1_2,
                           double *y1_2);
int StatisticsFromBox1(double *xplot0, double *yplot0, int npts0, double x1,
                       double x2, char *results);
int PhotometryInBox1(double *xplot0, double *yplot0, int npts0, double x1,
                     double x2, char *results);
int AstrometryInBox1(double *xplot0, double *yplot0, int npts0, double x1,
                     double x2, char *results);
int SumBox1(double *xplot0, double *yplot0, int npts0, double x1,
            double x2, double *sum1);
*/


static void SWAP1(double *x1, double *x2) {
double x0; x0 = *x1; *x1 = *x2; *x2 = x0; return;
}
static void SWAP2(double x1, double x2) {
double x0; x0 = x1; x1 = x2; x2 = x0; return;
}
static int GetBoundedIndices(double *xplot0, int npts0, double x1, double x2,
                             int *ix1, int *ix2);
/*****************************************************************************
* Get rectangle box limits: LowerLeft and UpperRight corner
* Check if box selected by the user is contained in the image
* and swap the limits so that the first point corresponds to the lower left
*     corner, and the second to the upper right corner
*
* INPUT:
*  x1_1, y1_1 : first point entered by the user (user coord.)
*  x1_2, y1_2 : second point entered by the user (user coord.)
*
* OUTPUT:
*  x1_1, y1_1 : lower left point (user coord.)
*  x1_2, y1_2 : upper right point (user coord.)
*****************************************************************************/
int GetRectangleBoxLimits1(double *x1_1, double *y1_1, double *x1_2, 
                           double *y1_2)
{

  if(*x1_1 > *x1_2) SWAP1(x1_1, x1_2);
  if(*y1_1 > *y1_2) SWAP1(y1_1, y1_2);

return (0);
}
/***********************************************************************
* Compute statistics of yplot0 
* in the interval of xplot0 bounded with (x1, x2)
* results should be large enough (e.g., results[512])
***********************************************************************/
int StatisticsFromBox1(double *xplot0, double *yplot0, int npts0, 
                       double x1, double x2, char *results)
{
int i, npts, ix_mini, ix_maxi, ix1, ix2;
double sum, sumsq, w1, mean, sigma; 
double mini, maxi, sum_x, sum_y; 

sum = 0.;
sumsq = 0.;
npts = 0;
strcpy(results," ");

// Look for the boundaries:
 GetBoundedIndices(xplot0, npts0, x1, x2, &ix1, &ix2);

/* Min and max, barycenter in y: */
npts = 0;
mini = yplot0[ix1]; 
maxi = mini;
ix_mini = ix1; 
ix_maxi = ix1;
sum_x = 0.; sum_y = 0.;
sum = 0.; sumsq = 0.;
for(i = ix1; i < ix2; i++) {
    w1 = yplot0[i];
// Test if is "not a number" since there are bad values in MUSE files:
    if(!isnan(w1)) {
      if(w1 < mini) {ix_mini = i; mini = w1;}
      if(w1 > maxi) {ix_maxi = i; maxi = w1;}
      sum += w1;
      sumsq += SQUARE(w1);
      sum_x += xplot0[i] * w1;
      sum_y += yplot0[i] * w1;
      npts++;
      }
  }


if(npts == 0) {
 fprintf(stderr,"StatisticsFromBox2/Error: window is empty!\n");
 return(-1);
 }

if(sum == 0.) {
  sum_x = 0.;
  sum_y = 0.;
  } else {
  sum_x = sum_x / sum;
  sum_y = sum_y / sum;
  }
mean = sum / (double) npts;
sigma = sumsq / (double) npts - mean * mean;
sigma = sqrt(sigma);

// Prepare output string:
sprintf(results, "Statistics on a rectangular Box\n\
 from (ix1=%d  to ix2=%d) (included)\n\
 Mean=%.5g  Sigma=%.4g\n\
 Number of points = %d    Sum = %.5g\n\
 Min = %.5g at %d    Max = %.5g at %d \n\
 Barycenter in x = %f in y = %f",
      ix1, ix2, mean, sigma, npts, sum, 
      mini, ix_mini, maxi, ix_maxi, sum_x, sum_y);

return(0);
}
/***********************************************************************
* Compute the flux of yplot0 
* in the interval of xplot0 bounded with (x1, x2)
***********************************************************************/
int PhotometryInBox1(double *xplot0, double *yplot0, int npts0, 
                     double x1, double x2, char *results)
{
double sum2, npts2;

 SumBox1(xplot0, yplot0, npts0, x1, x2, &sum2, &npts2);
 if(npts2 == 0) return(-1);

// Prepare output string:
sprintf(results,"Photometry in box\n\
 x1=%.1f x2=%.1f \n\
 Sum=%.3g Mean=%.3g\n", 
  x1, x2, sum2, sum2/npts2);

return(0);
}
/***********************************************************************
* Compute the mean x location (weighted by yplot0)
* in the interval of xplot0 bounded with (x1, x2)
***********************************************************************/
int AstrometryInBox1(double *xplot0, double *yplot0, int npts0, 
                     double x1, double x2, char *results)
{
double w1 = 0., sum, sumx, sumy, x_cent, y_cent;
int i, ix1, ix2;

// Look for the boundaries:
 GetBoundedIndices(xplot0, npts0, x1, x2, &ix1, &ix2);

 sum = 0.;
 sumx = 0.;
 sumy = 0.;
 for(i = ix1; i < ix2; i++){
   w1 = yplot0[i];
// Test if is "not a number" since there are bad values in MUSE files:
     if(!isnan(w1)) {
       sum += w1;
       sumx += xplot0[i] * w1;
       sumy += yplot0[i] * w1;
       }
   }

if(w1 == 0) return(-1);

x_cent = sumx / sum;
y_cent = sumy / sum;

// Prepare output string:
sprintf(results, "Astrometry in box\n\
 x1=%.1f x2=%.1f \n\
 Centroid: x=%.2f y=%.2f\n", 
 x1, x2, x_cent, y_cent);

return(0);
}
/*********************************************************************
* Get the indices ix1, ix2 corresponding to x1, x2
*********************************************************************/
static int GetBoundedIndices(double *xplot0, int npts0, double x1, double x2,
                             int *ix1, int *ix2)
{
int i;

// Look for the boundaries (assume xplot0 is ascending-order sorted):
*ix1 = 0;
for(i = 0; i < npts0; i++) {
 if(x1 < xplot0[i]) {
   *ix1 = i;
   break;
   }
 }

*ix2 = 0;
for(i = 0; i < npts0; i++) {
 if(x2 < xplot0[i]) {
   *ix2 = i;
   break;
   }
 }

if(*ix1 > *ix2) SWAP2(*ix1, *ix2);

return(0);
}
/*********************************************************************
* Sum of the intensities of the points included in a rectangular box 
* bounded by (x1, x2) in the x axis of xplot0
*
* INPUT:
*
* OUTPUT:
*   sum: sum of intensities
*********************************************************************/
int SumBox1(double *xplot0, double *yplot0, int npts0, double x1, double x2,
            double *sum, double *npts)
{
double w1;
int i, ix1, ix2;

// Look for the boundaries:
 GetBoundedIndices(xplot0, npts0, x1, x2, &ix1, &ix2);

*sum = 0.;
*npts = 0.;
   for(i = ix1; i < ix2; i++) {
// Test if is "not a number" since there are bad values in MUSE files:
     w1 = yplot0[i];
     if(!isnan(w1)) {
       *sum += w1;
       *npts += 1.;
       }
     }

return(0);
}
