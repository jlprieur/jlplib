/********************************************************************
* Set of routines to sort arrays in increasing order
*
* JLP
* Version 21/02/2020
********************************************************************/
#include <stdio.h>
#include <math.h>
#include "jlp_numeric.h"  // Where the prototypes are defined

/* Contained in this file and defined in photon_corr.h:
int jlp_sort_array_intensities(double *image0, int nx0, int ny0, 
                               double *out_intens, int n_intens);
int JLP_QSORT_INDEX(double *array, int *index, int *nn);
int JLP_QSORT(double *array, int *nn);
*/
static void qs(double *array, int left, int right);
static void qs2(double *array, int *index, int left, int right);

/****************************************************************************
* Quicksort (Cf. "C: The complete reference", Herbert Schildt)
*
* INPUT:
*  array[nn]: array to be sorted
*
* OUTPUT:
*  array[nn]: sorted array
****************************************************************************/
int JLP_QSORT(double *array, int *nn)
{
if(*nn < 2) return(0);
 qs(array, 0, (*nn)-1);
return(0);
}
/**************************************************************************
* The Quicksort
***************************************************************************/
static void qs(double *array, int left, int right)
{
register int i, j;
double x, y;

i = left; j = right;
/* Take the element in the middle as reference to partition the array: */
x = array[(left+right)/2];

/* Put the elements < x to the left and those > x to the right: */
do {
  while(array[i] < x && i < right) i++;
  while(x < array[j] && j > left) j--;
  if(i <= j) {
/* Exchange array[i] and array[j]: */
    y = array[i];
    array[i] = array[j];
    array[j] = y;
    i++; j--;
  }
} while(i<=j);

if(left < j) qs(array, left, j);
if(i < right) qs(array, i, right);
}
/****************************************************************************
* Quicksort (Cf. "C: The complete reference", Herbert Schildt)
*
* INPUT:
*  array[nn]: array to be sorted
*
* OUTPUT:
*  array[nn]: sorted array
*  index[nn]: array giving the index of the input array,
*             to sort other arrays in the same way if necessary
*             (f.i. array2[i] := array2[index[i]])
****************************************************************************/
int JLP_QSORT_INDEX(double *array, int *index, int *nn)
{
register int i;

/* Initialization of index array: */
for(i = 0; i < *nn; i++) index[i] = i;

if(*nn < 2) return(0);
 qs2(array, index, 0, (*nn)-1);

return(0);
}
/**************************************************************************
* The Quicksort, with index array
***************************************************************************/
static void qs2(double *array, int *index, int left, int right)
{
register int i, j;
int iy;
double x, y;

i = left; j = right;
/* Take the element in the middle as reference to partition the array: */
x = array[(left+right)/2];

/* Put the elements < x to the left and those > x to the right: */
do {
  while(array[i] < x && i < right) i++;
  while(x < array[j] && j > left) j--;
  if(i <= j) {
/* Exchange array[i] and array[j]: */
    y = array[i];
    array[i] = array[j];
    array[j] = y;
/* Exchange index[i] and index[j]: */
    iy = index[i];
    index[i] = index[j];
    index[j] = iy;
    i++; j--;
  }
} while(i<=j);

if(left < j) qs2(array, index, left, j);
if(i < right) qs2(array, index, i, right);
}
/**************************************************************************
* jlp_sort_array_intensities
* sort the values contained in the input array
*
* INPUT:
* image0: input 2D array
* nx0, ny0: dimension of the array
* out_intens input array used for memory space only
* n_intens: size of out_intens
*
* OUTPUT:
* out_intens: sorted intensities 
**************************************************************************/
int jlp_sort_array_intensities(double *image0, int nx0, int ny0, 
                               double *out_intens, int n_intens)
{
int i, npts;

if (n_intens < nx0*ny0) {
  fprintf(stderr, "Error: n_intens=%d < (nx0=%d * ny0=%d)\n", 
          n_intens, nx0, ny0);
  return(-1);
  }
npts = nx0 * ny0;
for(i= 0; i < npts; i++) out_intens[i] = image0[i];

/* Sort the array in increasing order: */
JLP_QSORT(out_intens, &npts);

return(0);
}
/**************************************************************************
* jlp_median_array
* to compute the median of the values contained in the input array
*
* INPUT:
* image0: input 2D array
* nx0, ny0: dimension of the array
*
* OUTPUT:
* value0: value corresponding to the median
**************************************************************************/
int jlp_median_array(double *image0, int nx0, int ny0, double *value0)
{
double *tmp_ima;
int i, npts;

npts = nx0 * ny0;
tmp_ima = new double[npts];
for(i= 0; i < npts; i++) tmp_ima[i] = image0[i];

/* Sort the array in increasing order: */
JLP_QSORT(tmp_ima, &npts);

*value0 = tmp_ima[npts/2];

delete[] tmp_ima;
return(0);
}
/**************************************************************************
* Compute the minimum circular profile of an image 
*
* INPUT:
* image0: input 2D array
* nx0, ny0: dimension of the array
*
* OUTPUT:
* value0: value corresponding to the median
**************************************************************************/
int subtract_min_circ_profile(double *image0, int nx0, int ny0, 
                              int ixc0, int iyc0)
{
double *tmp_ima;
double *profile, rho;
int i, j, npts, nprof, iprof;

npts = nx0 * ny0;
tmp_ima = new double[npts];
for(i = 0; i < npts; i++) tmp_ima[i] = image0[i];

nprof = (int)sqrt((double)(SQUARE(nx0)+SQUARE(ny0));
profile = new double[nprof];

for(i = 0; i < nprof; i++) {
   profile[i] = 1.e+9;
   }

// Compute the minimum profile
  for(i = 0; i < nx0; i++) 
    {
    for(j = 0; j < ny0; j++) 
      {
      rho = SQUARE(i - ixc0) + SQUARE(j - iyc0); 
      if(rho > 0.) rho = sqrt(rho);
       iprof = (int)rho;
       if(image0[i] < valprofile[iprof])
            valprofile[iprof] = image0[i];
      }
    }

// Subtract the minimum profile
  for(i = 0; i < nx0; i++) 
    {
    for(j = 0; j < ny0; j++) 
      {
      rho = SQUARE(i - ixc0) + SQUARE(j - iyc0); 
      if(rho > 0.) rho = sqrt(rho);
       iprof = (int)rho;
       image0[i] -= valprofile[iprof];
      }
    }

delete[] profile;
delete[] tmp_ima;
return(0);
}
