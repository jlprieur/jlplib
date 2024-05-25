/******************************************************************************
* Name:     jlp_process_images.cpp 
*
* Purpose:  simple image processing used by JLP_iGDev_wxWID and JLP_wx_iGDProc1 
*
* Author:   JLP 
* Version:  27/12/2015 
******************************************************************************/
#include <stdio.h>
#include <math.h>
#include <string.h>

#include "jlp_process_images.h"
#include "jlp_macros.h"  // NINT, SQUARE, etc
#include "jlp_auto_scale1.h"             // For auto_scale_double ...

// #define DEBUG
/*
  int GetRectangleBoxLimits2(double *x1_1, double *y1_1, double *x1_2, 
                             double *y1_2, int nx1, int ny1);
  int StatisticsFromBox2(double *array1, int nx1, int ny1, double x1, 
                         double y1, double x2, double y2, char *results);
  int PhotometryInCircle2(double *array1, int nx1, int ny1, double xc, 
                          double yc, double radius, char *results);
  int AstrometryInCircle2(double *array1, int nx1, int ny1, double xc, 
                          double yc, double radius, char *results);
  int CircleFromBox2(int nx1, int ny1, double x1_1,  double y1_1, double x1_2,  
                     double y1_2, double *xc, double *yc, double *radius);
  int SumCircle2(double *array1, int nx, int ny, double xc, double yc,
                 double radius, double *sum, double *npts)
  int SliceFromLine2(double *array1, int nx1, int ny1, double x1,  double y1,
                     double x2,  double y2, double *xplot, double *yplot,
                     int *nplot);
*/


static void SWAP(double *x1, double *x2) {
double x0; x0 = *x1; *x1 = *x2; *x2 = x0; return;
}
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
*  return 0 if in_frame
*         -1 if out of frame
*****************************************************************************/
int GetRectangleBoxLimits2(double *x1_1, double *y1_1, double *x1_2, 
                           double *y1_2, int nx1, int ny1)
{
int in_frame; 

  in_frame = (*x1_1 < 0 || *x1_1 >= nx1 || *y1_1 < 0 || *y1_1 >= ny1) ? 0 : 1;
  if(!in_frame) return(-1);
  in_frame = (*x1_2 < 0 || *x1_2 >= nx1 || *y1_2 < 0 || *y1_2 >= ny1) ? 0 : 1;
  if(!in_frame) return(-1);

  if(*x1_1 > *x1_2) SWAP(x1_1, x1_2);
  if(*y1_1 > *y1_2) SWAP(y1_1, y1_2);

return (0);
}
/***********************************************************************
* Set Thresholds from a rectangular box, given with its boundaries 
***********************************************************************/
int ComputeThresholdsFromBox2(double *array1, int nx1, int ny1, double x1, 
                            double y1, double x2, double y2, 
                            double *lower_itt, double *upper_itt,
                            char *results)
{
int ix1, iy1, ix2, iy2, min_max_scale = 1;
double low, up;

// Compute min/max (need to have min=max as input values)
   *lower_itt = 0; *upper_itt = 0;
   ix1 = NINT(x1);
   iy1 = NINT(y1);
   ix2 = NINT(x2);
   iy2 = NINT(y2);
// min_max or high contrast scale according to min_max_scale value (1 or 0)
/*
   auto_scale_double_box(array1, nx1, ny1, nx1,
                ix1, ix2, iy1, iy2, lower_itt, upper_itt, min_max_scale);
*/
   auto_scale_double_box(array1, nx1, ny1, nx1, ix1, ix2, iy1, iy2, 
                     &low, &up, min_max_scale);


*lower_itt = low;
*upper_itt = up;

// Prepare output string:
sprintf(results,"Automatic thresholds on a rectangular Box\n\
 from (ix1=%d iy1=%d) to (ix2=%d iy2=%d) (included)\n\
 lower = %.2f  upper = %.2f", ix1, iy1, ix2, iy2, *lower_itt, *upper_itt);

return(0);
}
/***********************************************************************
* Compute statistics of array1 
* in a rectangular box, given with its boundaries 
* x1,y1,x2,y2
* results should be large enough (e.g., results[512])
***********************************************************************/
int StatisticsFromBox2(double *array1, int nx1, int ny1, double x1, double y1, 
                       double x2, double y2, char *results)
{
int i, j, npts, ix_mini, iy_mini, ix_maxi, iy_maxi; 
int ix1, ix2, iy1, iy2;
double sum, sumsq, w1, mean, sigma; 
double mini, maxi, sum_x, sum_y; 

sum = 0.;
sumsq = 0.;
npts = 0;
strcpy(results," ");

ix1 = NINT(x1);
iy1 = NINT(y1);
ix2 = NINT(x2);
iy2 = NINT(y2);

if(ix1 < 0 || ix1 >= nx1 || iy1 < 0 || iy1 >= ny1) return(-1);

/* Min and max, barycenter in x and y: */
  npts = 0;
  mini = array1[ix1 + iy1 * nx1]; 
  maxi = mini;
  ix_mini = ix1; iy_mini = iy1;
  ix_maxi = ix1; iy_maxi = iy1;
  sum_x = 0.; sum_y = 0.;
  sum = 0.; sumsq = 0.;
  for(j = iy1; j < iy2; j++) {
    for(i = ix1; i < ix2; i++) {
      w1 = array1[i + j * nx1];
// Test if is "not a number" since there are bad values in MUSE files:
      if(!isnan(w1)) {
        if(w1 < mini) {ix_mini = i; iy_mini = j; mini = w1;}
        if(w1 > maxi) {ix_maxi = i; iy_maxi = j; maxi = w1;}
        sum += w1;
        sumsq += SQUARE(w1);
        sum_x = sum_x + (double)i * w1;
        sum_y = sum_y + (double)j * w1;
        npts++;
        }
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
 from (ix1=%d iy1=%d) to (ix2=%d iy2=%d) (included)\n\
 Mean=%.5g  Sigma=%.4g\n\
 Number of points = %d    Sum = %.5g\n\
 Min = %.5g at (%d,%d)    Max = %.5g at (%d,%d) \n\
 Barycenter in x = %.2f   Barycenter in y = %.2f",
      ix1, iy1, ix2, iy2, mean, sigma, npts, sum, 
      mini, ix_mini, iy_mini, maxi, ix_maxi, iy_maxi, sum_x, sum_y);

return(0);
}
/***********************************************************************
* Compute the flux within a circle
***********************************************************************/
int PhotometryInCircle2(double *array1, int nx1, int ny1, double xc, 
                        double yc, double radius, char *results) 
{
double sum, npts;

 SumCircle2(array1, nx1, ny1, xc, yc, radius, &sum, &npts);
 if(npts == 0) return(-1);

// Prepare output string:
sprintf(results,"Photometry in circle\n\
 xc=%.1f yc=%.1f rad=%.2f \n\
 Sum=%.3g Mean=%.3g\n", 
  xc, yc, radius, sum, sum/npts);

return(0);
}
/***********************************************************************
* Compute the location of a pattern within a circle
***********************************************************************/
int AstrometryInCircle2(double *array1, int nx1, int ny1, double xc, 
                        double yc, double radius, char *results) 
{
double radd, radius2, w1 = 0., sum, sumx, sumy, x_cent, y_cent;
int i, j;

 radius2 = SQUARE(radius);

 sum = 0.;
 sumx = 0.;
 sumy = 0.;
 for(j = 0; j < ny1; j++)
   for(i = 0; i < nx1; i++){
   radd = SQUARE(i - xc) + SQUARE(j - yc);
   if(radd < radius2) {
     w1 = array1[i + j * nx1];
// Test if is "not a number" since there are bad values in MUSE files:
     if(!isnan(w1)) {
       sum += w1;
       sumx += i * w1;
       sumy += j * w1;
       }
     }
   }

if(w1 == 0) return(-1);

x_cent = sumx / sum;
y_cent = sumy / sum;

// Prepare output string:
sprintf(results, "Astrometry in circle\n\
 xc=%.1f yc=%.1f rad=%.2f \n\
 Centroid: x=%.2f y=%.2f\n", 
 xc, yc, radius, x_cent, y_cent);

return(0);
}
/***********************************************************************
* Compute the circle parameters from the two points entered
* by the user
* 
* INPUT:
*  x1_1, y1_1, x1_2, y1_2: user coordinates of the two points
*
* OUTPUT:
*  xc, yc: user coordinates of the center
*  radius: radius of the circle
*  status = 0 is OK
***********************************************************************/
int CircleFromBox2(int nx1, int ny1, double x1_1,  double y1_1, double x1_2,  
                   double y1_2, double *xc, double *yc, double *radius)
{ 
int in_frame;

   in_frame = (x1_1 < 0 || x1_1 >= nx1 || y1_1 < 0 || y1_1 >= ny1) ? 0 : 1;
   if(!in_frame) return(-1);
   in_frame = (x1_2 < 0 || x1_2 >= nx1 || y1_2 < 0 || y1_2 >= ny1) ? 0 : 1;
   if(!in_frame) return(-1);

*xc = x1_1;
*yc = y1_1;
*radius = sqrt (SQUARE(x1_1 - x1_2) + SQUARE(y1_1 - y1_2));

return(0);
}
/*********************************************************************
* Sum of the intensities of the points included in a circle
*
* INPUT:
*   array1: array of data points
*   nx, ny: size of array1
*   radius: radius of the circle
*   xc, yc: coordinates of the center of the circle
*
* OUTPUT:
*   sum: sum of intensities
*********************************************************************/
int SumCircle2(double *array1, int nx, int ny, double xc, double yc,
               double radius, double *sum, double *npts)
{
// Something strange with g++/Cygnus: rad2 is not allowed as a variable!
double radd, radius2, w1;
int i, j;

radius2 = SQUARE(radius);
*sum = 0.;
*npts = 0.;
 for(j = 0; j < ny; j++) {
   for(i = 0; i < nx; i++) {
   radd = SQUARE((double)i - xc) + SQUARE((double)j - yc);
   if(radd < radius2) {
// Test if is "not a number" since there are bad values in MUSE files:
     w1 = array1[i + j * nx];
     if(!isnan(w1)) {
       *sum += w1;
       *npts += 1.;
       }
     }
   }
 }

return(0);
}
/*********************************************************************
* Slice within the array
*
* INPUT:
*   array1: array of data points
*   nx, ny: size of array1
*   x1, y1: coordinates of the first point of the line 
*   x2, y2: coordinates of the last point of the line 
*   nplot : maximum number of points of the slice
*
* OUTPUT:
*   xplot, yplot: arrays to be plotted later
*   nplot : number of points of the slice
*********************************************************************/
int SliceFromLine2(double *array1, int nx1, int ny1, double x1,  double y1,
                   double x2,  double y2, double *xplot, double *yplot,
                   int *nplot, char *title, char *xlabel, char *ylabel)
{
double stepx, stepy, dx1, dy1, xrange, yrange, rho1, angle1;
double w1, w2;
bool swap_limits, x_slice;
int i, iw, ix, iy, ix_start, iy_start, ix_end, iy_end;

// Line limits
ix_start = (int)x1;
iy_start = (int)y1;
ix_end = (int)x2;
iy_end = (int)y2;
#ifdef DEBUG
printf("SliceFromLine2/ x1=%f y1=%f x2=%f y2=%f\n", x1, y1, x2, y2);
#endif

// Check that limits are correct: 
ix_start  = MAXI(0, MINI(nx1, ix_start));
ix_end  = MAXI(0, MINI(nx1, ix_end));
iy_start  = MAXI(0, MINI(ny1, iy_start));
iy_end  = MAXI(0, MINI(ny1, iy_end));

if(ix_start == ix_end && iy_start == iy_end) {
 fprintf(stderr,"slice1/Error: ix_start=ix_end=%d and iy_start=iy_end=%d\n",
         ix_start, iy_start);
 return(-1);
 }

/* Compute angle and distance: */
dx1 = ix_end - ix_start; 
dy1 = iy_end - iy_start; 
rho1 = sqrt(SQUARE(dx1) + SQUARE(dy1));
if(rho1 > 0) angle1 = (180./PI) * acos(dx1/rho1);
 else angle1 = 0.;
// Conversion of angles to the [-180,+180] range: 
 if(angle1 > 180.) angle1 -=360.;

// using acos introduces a problem with negative angles
if(dy1 < 0.) angle1 *= -1;

#ifdef DEBUG
printf("SliceFromLine2/ x1=%f y1=%f x2=%f y2=%f\n", x1, y1, x2, y2);
printf("SliceFromLine2/ xstart=%d ystart=%d xend=%d yend=%d\n", 
        ix_start, iy_start, ix_end, iy_end);
#endif

/* Slice along a line or a column: */
swap_limits = false;
xrange = ABS(dx1);
yrange = ABS(dy1);
x_slice = (xrange >= yrange) ? true : false;

/* Compute max number of pixels: */
if(x_slice) {
  *nplot = xrange + 1;
// Swap (x1,y1) and (x2,y2) limits to have increasing coordinates: 
  if(ix_start > ix_end) swap_limits = true;
  }
else {
  *nplot = yrange + 1;
// Swap (x1,y1) and (x2,y2) limits to have increasing coordinates: 
  if(iy_start > iy_end) swap_limits = true; 
  }

// Swap (x1,y1) and (x2,y2) limits to have increasing coordinates: 
  if(swap_limits) {
    iw = ix_end;
    ix_end = ix_start;
    ix_start = iw;
    iw = iy_end;
    iy_end = iy_start;
    iy_start = iw;
    }

// Caption:
sprintf(title, "From (%d,%d) to (%d,%d), (rho=%.1f angle=%.1f)\n",
         ix_start, iy_start, ix_end, iy_end, rho1, angle1);
if(x_slice) 
  strcpy(xlabel,"X/cos(angle)  (pixels)"); 
else 
  strcpy(xlabel," Y/sin(angle)  (pixels)");

strcpy(ylabel,"Flux"); 

/* Look for the nearest neighbour: */
stepx = (double)xrange / (double)(*nplot - 1);
if(ix_start > ix_end) stepx *= -1;
stepy = (double)yrange / (double)(*nplot - 1);
if(iy_start > iy_end) stepy *= -1;

// Conversion to radians:
angle1 *= (PI / 180.);
for(i = 0; i < *nplot; i++)
  {
   w1 = (double)ix_start + stepx * (double)i;
   w2 = (double)iy_start + stepy * (double)i;
   if(x_slice) 
      xplot[i] = w1 / cos(angle1); 
   else 
      xplot[i] = w2 / sin(angle1); 
   ix = NINT(w1);
   iy = NINT(w2);
   yplot[i] = (double)array1[ix + iy * nx1];
#ifdef DEBUG_1
   printf(" xplot(%d)=%f yplot(%d),%f (ix=%d iy=%d)\n",
            i,xplot[i],i,yplot[i], ix, iy);
#endif
  }

return(0);
}
