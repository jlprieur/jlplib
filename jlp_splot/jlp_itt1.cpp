/*****************************************************************
* jlp_itt1.cpp
* Set of ITT routines used to display images with wxWidgets
*
* JLP Version
* Version 10/10/2015
******************************************************************/
#include <stdio.h>
#include <math.h>
#include "jlp_itt1.h"
#include "jlp_numeric.h"  // jlp_sort.c (sort, MINI, etc)

static void LinearItt(BYTE* out_ima_k, const double* in_ima, const int nx,
                      const int ny, double min_val, double max_val,
                      const double bad_value);
static void Thresholds_MinMax(const double* in_ima, const int nx, const int ny,
                              const int x1_box, const int y1_box, const int x2_box,
                              const int y2_box, double *min_val, double *max_val,
                              const double bad_value);
static void Thresholds_Median(const double* in_ima, const int nx, const int ny,
                              double *min_val, double *max_val,
                              const double bad_value);
static void Thresholds_HC(const double* in_ima, const int nx, const int ny,
                          const int x1_box, const int y1_box, const int x2_box,
                          const int y2_box, double *min_val, double *max_val,
                          const double bad_value);
static void Thresholds_VHC(const double* in_ima, const int nx, const int ny,
                           const int x1_box, const int y1_box, const int x2_box,
                           const int y2_box, double *min_val, double *max_val,
                           const double bad_value);
static void LogItt(BYTE* out_ima_k, const double* in_ima, const int nx,
                   const int ny, double min_val, double max_val,
                   const double bad_value);
/*********************************************************************
* Intensity Transfer Table:
* This table is filled with the indices k to be used for LUT conversion
* of a double precision image
*
* ITT_thresh: Threshold determination
*      "FromBox" (thresholds computed here)
*      "DirectInput" (thresholds provided by the user)
*      "Median" (Linear Min Max with median computation: slow!)
*      "MinMax" (Linear Min Max)
*      "HC"  (High contrast)
*      "VHC" (Very High contrast)
* otherwise, take the values chosen previously by the user
*
* INPUT:
*   ITT_thresh: ITT option
*   ITT_is_linear: flag set to one if linear ITT scale,
*                           to zero if logarithmic ITT scale
*   in_ima: array to be displayed
*   nx, ny: size of in_ima
*
* OUTPUT:
*   out_ima_k: BYTE version of in_ima according to ITT value
*              (index to be used for LUT conversion)
*
* INPUT/OUTPUT:
* min,max: lower and upper thresholds (INPUT or OUTPUT according to ITT value)
**********************************************************************/
void LoadITT(BYTE* out_ima_k, const double* in_ima, const int nx, const int ny,
             const int x1_box, const int y1_box, const int x2_box, const int y2_box,
             double *min_val, double *max_val, const char* ITT_thresh,
             const int ITT_is_linear)
{
double bad_value = -123456789.;

// Compute new thresholds if needed:
// (if Direct Input do not change the input values)
if(!strncmp(ITT_thresh, "FromBox", 7))
   Thresholds_MinMax(in_ima, nx, ny, x1_box, y1_box, x2_box, y2_box,
                     min_val, max_val, bad_value);
else if(!strncmp(ITT_thresh, "MinMax", 6))
   Thresholds_MinMax(in_ima, nx, ny, 0, 0, nx, ny, min_val, max_val, bad_value);
else if(!strncmp(ITT_thresh, "Median", 6))
   Thresholds_Median(in_ima, nx, ny, min_val, max_val, bad_value);
else if(!strncmp(ITT_thresh, "HC", 2) || !strncmp(ITT_thresh, "HighC", 5))
   Thresholds_HC(in_ima, nx, ny, 0, 0, nx, ny, min_val, max_val, bad_value);
else if(!strncmp(ITT_thresh, "VHC", 3))
   Thresholds_VHC(in_ima, nx, ny, 0, 0, nx, ny, min_val, max_val, bad_value);

/* Now compute the output image: */
if(ITT_is_linear)
  LinearItt(out_ima_k, in_ima, nx, ny, *min_val, *max_val, bad_value);
else
  LogItt(out_ima_k, in_ima, nx, ny, *min_val, *max_val, bad_value);

return;
}
/*****************************************************************
* Linear ITT with values entered by the user
*
* INPUT:
*   in_ima: array to be displayed
*   nx, ny: size of in_ima
*   min_val, max_val: minimum and maximum thresholds
*   bad_value: value of pixels to be rejected
*
* OUTPUT:
*   out_ima_k: BYTE version of in_ima according to ITT value
*              (index to be used for LUT conversion)
*
******************************************************************/
static void LinearItt(BYTE* out_ima_k, const double* in_ima, const int nx,
                      const int ny, double min_val, double max_val,
                      const double bad_value)
{
int iw;
double gain1, range;
register int i;

range = max_val - min_val;
if(range < 1.e-9) range = 1;

// Linear scale:
 gain1 = 255. / range;
 for(i = 0; i < nx * ny; i++) {
     if(in_ima[i] == bad_value) {
     out_ima_k[i] = 0;
     } else {
     iw = (int)((in_ima[i] - min_val) * gain1);
     iw = MAXI(0, iw);
     iw = MINI(255, iw);
     out_ima_k[i] = (BYTE)iw;
     }
 }
return;
}
/********************************************************************
* Compute the thresholds for "MinMax" or "FromBox" ITT
*
* INPUT:
*   in_ima: array to be displayed
*   nx, ny: size of in_ima
*   bad_value: value of pixels to be rejected
*
* OUTPUT:
*   min_val, max_val: lower and upper thresholds
********************************************************************/
static void Thresholds_MinMax(const double* in_ima, const int nx, const int ny,
                              const int x1_box, const int y1_box, const int x2_box,
                              const int y2_box, double *min_val, double *max_val,
                              const double bad_value)
{

Min_Max_itt(in_ima, nx, ny, x1_box, y1_box, x2_box, y2_box, min_val, max_val, bad_value);
if(*max_val == *min_val) *max_val += 1;

return;
}
/********************************************************************
* Compute the thresholds for a median ITT
*
* INPUT:
*   in_ima: array to be displayed
*   nx, ny: size of in_ima
*   bad_value: value of pixels to be rejected
*
* OUTPUT:
*   min_val, max_val: lower and upper thresholds
********************************************************************/
static void Thresholds_Median(const double* in_ima, const int nx, const int ny,
                              double *min_val, double *max_val,
                              const double bad_value)
{

Min_Max_Median_itt(in_ima, nx, ny, min_val, max_val, bad_value);
if(*max_val == *min_val) *max_val += 1;

return;
}
/********************************************************************
* Compute the thresholds for a high contrast ITT
*
* INPUT:
*   in_ima: array to be displayed
*   nx, ny: size of in_ima
*   bad_value: value of pixels to be rejected
*
* OUTPUT:
*   min_val, max_val: lower and upper thresholds
********************************************************************/
static void Thresholds_HC(const double* in_ima, const int nx, const int ny,
                          const int x1_box, const int y1_box, const int x2_box,
                          const int y2_box, double *min_val, double *max_val,
                          const double bad_value)
{
double min0, max0, mean, sigma;

Min_Max_itt(in_ima, nx, ny, x1_box, y1_box, x2_box, y2_box, &min0, &max0, bad_value);

Mean_Sigma(in_ima, nx, ny, x1_box, y1_box, x2_box, y2_box, &mean, &sigma, bad_value);

// Check if limits are within [min,max]:
*min_val = MAXI(mean - 3 * sigma, min0);
*max_val = MINI(mean + 3 * sigma, max0);

if(*max_val == *min_val) *max_val += 1;

return;
}
/********************************************************************
* Compute the thresholds for "Very High contrast"
* Removing upper part (specially designed for square moduli of FFT's)
*
* INPUT:
*   in_ima: array to be displayed
*   nx, ny: size of in_ima
*   bad_value: value of pixels to be rejected
*
* OUTPUT:
*   min_val, max_val: lower and upper thresholds
********************************************************************/
static void Thresholds_VHC(const double* in_ima, const int nx, const int ny,
                           const int x1_box, const int y1_box, const int x2_box,
                           const int y2_box, double *min_val, double *max_val,
                           const double bad_value)
{
double lower_itt, upper_itt, min0, max0, mean, sigma;

Mean_Sigma(in_ima, nx, ny, x1_box, y1_box, x2_box, y2_box, &mean, &sigma, bad_value);
lower_itt = mean - 3 * sigma;
upper_itt = mean + 2. * sigma;

// Check if limits are within [min,max]:
Min_Max_itt(in_ima, nx, ny, x1_box, y1_box, x2_box, y2_box, &min0, &max0, bad_value);

lower_itt = MAXI(min0, lower_itt);
upper_itt = MINI(max0, upper_itt);

// Tuning for looking at very low levels:
lower_itt -= 0.8 * sigma;

*min_val = lower_itt;
*max_val = upper_itt;

return;
}
/********************************************************************
* Log ITT with values entered by the user
*
* INPUT:
*   in_ima: array to be displayed
*   nx, ny: size of in_ima
*   min_val, max_val: minimum and maximum thresholds
*   bad_value: value of pixels to be rejected
*
* OUTPUT:
*   out_ima_k: BYTE version of in_ima according to ITT
*              (index to be used for LUT conversion)
*
********************************************************************/
static void LogItt(BYTE* out_ima_k, const double* in_ima, const int nx,
                   const int ny, double min_val, double max_val,
                   const double bad_value)
{
double work, range;
register int i;

range = max_val - min_val;
if(range < 1.e-9) range = 1;

  for (i=0; i < nx * ny; i++)
    {
     if(in_ima[i] == bad_value) {
     out_ima_k[i] = 0;
     } else {
/* Converted to a value between 1 and 10: */
     work = 1. + 9. * (in_ima[i] - min_val) / range;
     work = MAXI(work, 1.);
/* And then between 0. and 255: */
     work = 255. * log10(work);
     work = MAXI(work, 0.);
     work = MINI(work, 255.);
     out_ima_k[i] = (BYTE)work;
    }
  }
return;
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
// Mean_Sigma
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int Mean_Sigma(const double* image, const int nx, const int ny,
                const int x1_box, const int y1_box, const int x2_box,
                const int y2_box, double* mean, double *sigma,
                const double bad_value)
{
double max0, min0, v, sum, sumsq;
int npts, i, j;

// Look for mean and std deviation:
 sum = 0.; sumsq = 0.;  npts = 0;
 for(j = y1_box; j < y2_box; j++) {
  for(i = x1_box; i < x2_box; i++) {
    v = image[i + j * nx];
    if(v != bad_value) {
      npts++;
      sum += v; sumsq += (v * v);
    }
  }
}

// If no valid points, return (0,0)
if(npts == 0) {
  *mean = 0.;
  *sigma = 0.;
  return(-1);
  }

// Mean:
sum /= (double)npts;
sumsq /= (double)npts;
sumsq = sumsq - sum * sum;
if(sumsq > 0) sumsq = sqrt(sumsq);
min0 = sum - 3 * sumsq;
max0 = sum + 3 * sumsq;

// Do a second passage to remove possible (min/max) aberrant pixels:
 npts = 0;
 sum = 0.; sumsq = 0.;
 for(j = y1_box; j < y2_box; j++) {
  for(i = x1_box; i < x2_box; i++) {
    v = image[i + j * nx];
    if(( v >= min0 && v < max0) && (v != bad_value))
    {
    npts++;
    sum += v; sumsq += (v * v);
    }
  }
 }

// Mean:
if(npts > 0) {
  sum /= (double)npts;
  sumsq /= (double)npts;
  sumsq = sumsq - sum * sum;
  if(sumsq > 0) sumsq = sqrt(sumsq);
  *mean = sum;
  *sigma = sumsq;
// If no valid points, return (0,0)
  } else {
  *mean = 0.;
  *sigma = 0.;
  }

return(0);
}

/*************************************************************************
* Determine the minimum and maximum of an image that will be used for display
* (with the median in order to select the best range: VERY SLOW !)
**************************************************************************/
void Min_Max_Median_itt(const double* image, const int nx, const int ny,
                        double* min, double* max, const double bad_value)
{
double *tmp_array, ww;
int npts, i1, i2, nvalues;
int i, k;

*min = 0.;
*max = 0.;

  npts = nx * ny;
  tmp_array = new double[npts];
  k = 0;
  for(i = 0; i < npts; i++) {
      ww = image[i];
      if(ww != bad_value) tmp_array[k++] = ww;
    }
  nvalues = k;

// Sort the array corresponding to bin #i in increasing order
// Defined in "jlp_sort.c" (prototypes in jlp_numeric.h):
// int JLP_QSORT_DBLE(double *array, int *nn);
  JLP_QSORT_DBLE(tmp_array, &nvalues);
// Median corresponds to the middle point tmp_array[ npts / 2]

// Minimum taken at 3%, maximum at 97%:
 i1 = (int)(0.03 * (double) nvalues);
 i2 = (int)(0.97 * (double) nvalues);
 i2 = MINI(i2, nvalues - 1);

 *min = tmp_array[i1];
 *max = tmp_array[i2];

#ifdef DEBUG
printf("MinMax_Median: min=%f max=%f i1=%d i2=%d nvalues=%d npts=%d\n",
        *min, *max, i1, i2, nvalues, npts);
#endif

delete[] tmp_array;

return;
}
/*************************************************************************
* Determine the minimum and maximum of an image that will be used for display
* (hence do not take into account the two pixels corresponding to the largest
* and smallest intensities)
* see also:
*   auto_scale_double_box(array1, nx1, ny1, nx1,
*                ix1, ix2, iy1, iy2, lower_itt, upper_itt, min_max_scale);
* where min_max or high contrast scale according to min_max_scale value (1 or 0)
*   auto_scale_double_box(array1, nx1, ny1, nx1, ix1, ix2, iy1, iy2,
*                     &low, &up, min_max_scale);
**************************************************************************/
void Min_Max_itt(const double* image, const int nx, const int ny,
                 const int x1_box, const int y1_box, const int x2_box,
                 const int y2_box, double* min, double* max,
                 const double bad_value)
{
double v, min0, max0;
int imin, imax, jmin, jmax;
int i, j;

// Look for mean, minimum, maximum:

 imin = x1_box; jmin = y1_box;
 min0 = image[imin + jmin * nx];
 imax = imin; jmax = jmin;
 max0 = image[imax + jmax * nx];
 for(j = y1_box; j < y2_box; j++) {
  for(i = x1_box; i < x2_box; i++) {
    v = image[i + j * nx];
      if(v != bad_value) {
       if(v > max0) {imax = i; jmax = j; max0 = v;}
       if(v < min0) {imin = i; jmin = j; min0 = v;}
      }
    }
  }

// Do a second passage to remove possible (min/max) aberrant pixels:
 max0 = -1.e+12; min0 = +1.e+12;
 for(j = y1_box; j < y2_box; j++) {
  for(i = x1_box; i < x2_box; i++) {
    if((i != imax && i != imin)
       && (j != jmax && j != jmin)) {
      v = image[i + j * nx];
      if(v != bad_value) {
        if(v > max0) max0 = v;
        if(v < min0) min0 = v;
       }
     }
  }
 }

*min = min0;
*max = max0;
return;
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Determine the location and value
// of the minimum and maximum intensity of an image
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void Min_Max_with_location(const double* image, const int nx, const int ny,
                           double *min, double *max,
                           int *ix_min, int *iy_min, int *ix_max, int *iy_max,
                           const double bad_value)
{
double work;
int imax, imin;
register int i;

// Look for mean, minimum, maximum:
 *max = -1.e+12; *min = +1.e+12;
 for(i = 0; i < nx * ny; i++)
    {
    work = image[i];
    if(work != bad_value) {
      if(work > *max) {imax = i; *max = work;}
      if(work < *min) {imin = i; *min = work;}
      }
    }

*iy_min = imin / nx;
*ix_min = imin - (*iy_min) * nx;
*iy_max = imax / nx;
*ix_max = imax - (*iy_max) * nx;
return;
}
