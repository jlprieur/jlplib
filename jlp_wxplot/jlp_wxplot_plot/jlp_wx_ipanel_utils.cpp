/*********************************************************************
* jlp_wx_ipanel_utils.cpp
* Miscellaneous programs used by many classes
*
* JLP
* Version 03/08/2013
**********************************************************************/
#include "jlp_wx_ipanel_utils.h"  // Prototype of functions included here

#include "jlp_fitsio.h"         // for JLP_RDFITS_2D
#include "jlp_macros.h"         // NINT, SQUARE

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
int gdp_sum_circle(double *array1, int nx, int ny, double xc, double yc,
                   double radius, double *sum, double *npts)
{
// Something strange with g++/Cygnus: rad2 is not allowed as a variable!
double radd, radius2;
register int i, j;

radius2 = SQUARE(radius);
*sum = 0.;
*npts = 0.;
 for(j = 0; j < ny; j++) {
   for(i = 0; i < nx; i++) {
   radd = SQUARE((double)i - xc) + SQUARE((double)j - yc);
   if(radd < radius2) {
     *sum += array1[i + j * nx];
     *npts += 1.;
     }
   }
 }

return(0);
}
/*********************************************************************
* Offset correction: subtract offset map from input image 
*
* INPUT:
*   offset1: offset map to be used for correction 
*   array1: array of data points
*   nx, ny: size of both arrays
*   positive: flag set to one to impose positive constraint
*
* OUTPUT:
*   array1: corrected array of data points
*********************************************************************/
int gdp_offset_correction(double *array1, double *offset1, int nx1, int ny1,
                          int positive)
{
int i;
if(positive) {
  for(i = 0; i < nx1 * ny1; i++) {
    if(array1[i] > offset1[i]) array1[i] -= offset1[i];
    else array1[i] = 0.;
    }
  } else {
  for(i = 0; i < nx1 * ny1; i++) array1[i] -= offset1[i];
  }
return(0);
}
/*********************************************************************
* Flat field correction: divide input image by flat field map 
*
* INPUT:
*   ffield1: flat field map to be used for correction 
*   array1: array of data points
*   nx, ny: size of both arrays
*   sigma_level: constraint on the range of the flat field values  
*
* OUTPUT:
*   array1: corrected array of data points
*   ffield1: normalized thresholded flat field
*********************************************************************/
int gdp_ffield_correction(double *array1, double *ffield1, int nx1, int ny1,
                          int sigma_level)
{
int i;
double mean, sigma, upper_threshold, lower_threshold;

gdp_statistics(ffield1, nx1, ny1, &mean, &sigma);

// Thresholding to min +/- sigma_level * sigma:
if(sigma_level > 0) {
  lower_threshold = mean - sigma_level * sigma;
  upper_threshold = mean + sigma_level * sigma;
  for(i = 0; i < nx1 * ny1; i++) {
    if(ffield1[i] < lower_threshold) ffield1[i] = lower_threshold;
    else if(ffield1[i] > upper_threshold) ffield1[i] = upper_threshold;
    }
  }

// Normalisation to unity:
  for(i = 0; i < nx1 * ny1; i++) {
    if(ffield1[i] > 0.) ffield1[i] /= mean;
    else ffield1[i] = 1.;
    }

// Division by normalized flat field:
  for(i = 0; i < nx1 * ny1; i++) array1[i] /= ffield1[i];

return(0);
}
/***********************************************************************
* Compute statistics of array1 
* in a rectangular box, given with its boundaries 
* x1,y1,x2,y2
***********************************************************************/
int gdp_statistics(double *array1, int nx1, int ny1, double* mean, 
                   double* sigma)
{
int i, npts;
double sum, sumsq, w1;

sum = 0.;
sumsq = 0.;

sum = 0.; sumsq = 0.;
for(i = 0; i < nx1 * ny1; i++) {
      w1 = array1[i];
      sum += w1;
      sumsq += SQUARE(w1);
    }

npts = nx1 * ny1;
*mean = sum / (double) npts;
*sigma = sumsq / (double) npts - (*mean) * (*mean);
if(*sigma > 0) *sigma = sqrt(*sigma);

return(0);
}
