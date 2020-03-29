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
*/
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
JLP_QSORT_DBLE(out_intens, &npts);

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
JLP_QSORT_DBLE(tmp_ima, &npts);

*value0 = tmp_ima[npts/2];

delete[] tmp_ima;
return(0);
}
