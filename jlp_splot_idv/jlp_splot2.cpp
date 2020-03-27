/***************************************************************
*
* NAME: jlp_splot2
*
* Contains:
*  int jlp_string_copy(char *in, char *out, int len)
*  JLP_CURVE(xplot,yplot,errx,erry,npts0,nchar,pcolor,error_bars,...);
*
* VERSION: 13/01/2017 
*
* AUTHOR: JLP 
*
***************************************************************/
#include <stdio.h>
#include <string.h>
#include <ctype.h>       /* isgraph, isprint, etc... */
#include <malloc.h>
#include <math.h>
#include "jlp_splot_idv.h"
#include "jlp_gdev_idv.h"
#include "jlp_macros.h"

int jlp_string_copy(char *in, char *out, int len);
int JLP_CURVE(float *xplot, float *yplot, float *errx, float *erry, 
               int *npts0, char *nchar, char *pcolor, int *error_bars, 
               int *x_is_log10, int *y_is_log10, int *idv1);
int JLP_CURVE_LINE(float *xplot, float *yplot, int *npts0, char *nchar1,
                   char *pcolor1, int *idv1);
int JLP_CURVE_HISTO(float *xplot, float *yplot, int *npts0, char *nchar1,
                    char *pcolor1, int *idv1);
int compute_min_max_curves(float *xplot, float *yplot, float *errx, 
                           float *erry, int *npts0, int nmax, int ncurves,
                           float *xmindata, float *xmaxdata, float *ymindata,
                           float *ymaxdata);

/*****************************************************************************
* Filter for Fortran character strings
*****************************************************************************/
int jlp_string_copy(char *in, char *out, int len)
{
int imax;
register int i;

imax =  MINI(len - 1, (int)strlen(in));
for(i = 0; i < imax && isprint(in[i]); i++) out[i] = in[i];

out[i] = '\0';
return(0);
}
/*****************************************************************************
*
* FUNCTION: JLP_CURVE
*
* PURPOSE: To plot a set of locations stored in the arrays (xplot,yplot)
*          with lines, symbols or histogram-like format (i.e., L1, 42, H, ...)
*
* INPUT:  
*  xplot0[] = float array with X user coordinates
*  yplot0[] = float array with Y user coordinates
*  npts0 = number of points to draw
*  nchar   = type of character (L0= solid line, L1= dashed,
*            H0=histogram with solid line, 
*            H1= histogram with dashed line)
*  pcolor = color to be used (Purple, Aquamarine, ...)
*  error_bars = Flag (1 if error bars,  0 otherwise)
*
****************************************************************************/
int JLP_CURVE(float *xplot0, float *yplot0, float *errx0, float *erry0, 
              int *npts0, char *nchar, char *pcolor, int *error_bars, 
              int *idv1)
{
double *d_xplot0, *d_yplot0, *d_errx0, *d_erry0;
int  i, status = -1;

// Check first if curve device:
if (GDev_from_idv(*idv1) == NULL) {
  fprintf(stderr,"JLP_CURVE/Error: opened device is image device igdev (not cgdev)\n");
  return(-1);
  }  

/* Return if no points to be plotted: */
if(*npts0 <= 0) {
  fprintf(stderr,"JLP_CURVE/Error: npts0=%d\n", *npts0);
  return(-1);
  }

// Conversion to double:
 d_xplot0 = new double[*npts0];
 d_yplot0 = new double[*npts0];
 for(i = 0; i < *npts0; i++) {
  d_xplot0[i] = xplot0[i];
  d_yplot0[i] = yplot0[i];
  }
// Leave the possibility of NULL pointer for errx0
 d_errx0 = new double[*npts0];
 d_erry0 = new double[*npts0];
 if(*error_bars != 0) {
  for(i = 0; i < *npts0; i++) {
    d_errx0[i] = errx0[i];
    d_erry0[i] = erry0[i];
    }
  } else {
  for(i = 0; i < *npts0; i++) {
    d_errx0[i] = 0.;
    d_erry0[i] = 0.;
    }
  } // EOF error_bars

status = GDev_from_idv(*idv1)->draw_curve(d_xplot0, d_yplot0, d_errx0, d_erry0,
                                          *npts0, nchar, pcolor, *error_bars);

delete[] d_xplot0;
delete[] d_yplot0;
delete[] d_errx0;
delete[] d_erry0;
return(status);
}
/*****************************************************************************
*
* FUNCTION: JLP_CURVE_LINE
*
* PURPOSE:
*
* INPUT:  
*  xplot0[] = float array with X user coordinates
*  yplot0[] = float array with Y user coordinates
*  npts0    = number of points to draw
*  nchar0   = type of character (L0= solid line, L1= dashed)
*  pcolor0  = color to be used for drawing the lines ("Default", "Red", etc)
*
****************************************************************************/
int JLP_CURVE_LINE(float *xplot0, float *yplot0, int *npts0, char *nchar0, 
                   char *pcolor0, int *idv1)
{
int i, status = -1;
double *d_xplot0, *d_yplot0;

 if(!GDev_from_idv(*idv1)) {
   fprintf(stderr," JLP_CURVE_LINE/Error: invalid device number (idv=%d)!\n",
           *idv1);
   return(-1);
  }

if (Jgdev_box_xmin(*idv1) == Jgdev_box_xmax(*idv1) 
    || Jgdev_box_ymin(*idv1) == Jgdev_box_ymax(*idv1)) {
  fprintf(stderr," JLP_CURVE_LINE/Error: range in x or y is null! \n");
  return(-1);
 }

/* Return if no points to be plotted: */
if(*npts0 <= 0) {
  fprintf(stderr,"JLP_CURVE_LINE/Error: npts0=%d\n", *npts0);
  return(-1);
  }

// Conversion to double:
 d_xplot0 = new double[*npts0];
 d_yplot0 = new double[*npts0];
 for(i = 0; i < *npts0; i++) {
  d_xplot0[i] = xplot0[i];
  d_yplot0[i] = yplot0[i];
  }

status = GDev_from_idv(*idv1)->draw_curve_line(d_xplot0, d_yplot0, *npts0, 
                                               nchar0, pcolor0);

delete[] d_xplot0;
delete[] d_yplot0;
return(status);
}
/*****************************************************************************
* FUNCTION: JLP_CURVE_HISTO
* To draw a histogram
*
* INPUT:  
*  xplot0[] = float array with X user coordinates
*  yplot0[] = float array with Y user coordinates
*  npts0 = number of points to draw
*  nchar0   = type of character (H0=Solid black line, H1= Dashed black line, 
*  pcolor0  = color to be used: Default (no filling), Aquamarine, Purple)
*
****************************************************************************/
int JLP_CURVE_HISTO(float *xplot0, float *yplot0, int *npts0, char *nchar0, 
                    char *pcolor0, int *idv1)
{
int i, status = -1; 
char pcolor1[32], nchar1[4];
double *d_xplot0, *d_yplot0;

 if(!GDev_from_idv(*idv1)) {
   fprintf(stderr," JLP_CURVE_HISTO/Error: invalid device number (idv=%d)!\n",
           *idv1);
   return(-1);
  }

/* Transfer (for fortran interface) */
for(i = 0; i < MINI(4, (int)strlen(nchar0)) 
    && nchar0[i] && isprint(nchar0[i]); i++) nchar1[i] = nchar0[i];
nchar1[i] = '\0';

for(i = 0; i < MINI(32, (int)strlen(pcolor0)) 
    && pcolor0[i] && isprint(pcolor0[i]); i++) pcolor1[i] = pcolor0[i];
pcolor1[i] = '\0';

/* Return if no points to be plotted: */
if(*npts0 <= 0) {
  fprintf(stderr,"JLP_CURVE_HISTO/Error: npts0=%d\n", *npts0);
  return(-1);
  }

// Conversion to double:
 d_xplot0 = new double[*npts0];
 d_yplot0 = new double[*npts0];
 for(i = 0; i < *npts0; i++) {
  d_xplot0[i] = xplot0[i];
  d_yplot0[i] = yplot0[i];
  }

status = GDev_from_idv(*idv1)->draw_curve_histo(d_xplot0, d_yplot0, *npts0, 
                                                nchar1, pcolor1);

delete[] d_xplot0;
delete[] d_yplot0;

return(status);
}
/***************************************************************************
* Compute min, max for plots
***************************************************************************/
int jlp_splot_min_max_for_curves(float *xplot, float *yplot, float *errx, 
                                 float *erry, int *npts0, int nmax, int ncurves,
                                 float *xmindata, float *xmaxdata, 
                                 float *ymindata, float *ymaxdata, 
                                 int error_bars, int iplan)
{
float val[3], xrange, yrange, new_range, xcent, ycent;
int i, j, k;

*xmindata = 1.e12;
*xmaxdata = -1.e12;
*ymindata = 1.e12;
*ymaxdata = -1.e12;

// If error bars, use the errors as well to compute the limits of the frame:
if(error_bars == 1) {
  for(k = 0; k < ncurves; k++) {
    for(i = 0; i < npts0[k]; i++) {
      val[0] = xplot[i + k * nmax] - errx[i + k * nmax];
      val[1] = xplot[i + k * nmax]; 
      val[2] = xplot[i + k * nmax] + errx[i + k * nmax];
      for(j = 0; j < 3; j++) {
        if(val[j] > *xmaxdata) *xmaxdata = val[j]; 
        if(val[j] < *xmindata) *xmindata = val[j]; 
        }
      val[0] = yplot[i + k * nmax] - erry[i + k * nmax];
      val[1] = yplot[i + k * nmax]; 
      val[2] = yplot[i + k * nmax] + erry[i + k * nmax];
      for(j = 0; j < 3; j++) {
        if(val[j] > *ymaxdata) *ymaxdata = val[j]; 
        if(val[j] < *ymindata) *ymindata = val[j]; 
        }
     }
  }
} else {
  for(k = 0; k < ncurves; k++) {
    for(i = 0; i < npts0[k]; i++) {
      j = 0;
      val[j] = xplot[i + k * nmax]; 
      if(val[j] > *xmaxdata) *xmaxdata = val[j]; 
      if(val[j] < *xmindata) *xmindata = val[j]; 
      val[j] = yplot[i + k * nmax]; 
      if(val[j] > *ymaxdata) *ymaxdata = val[j]; 
      if(val[j] < *ymindata) *ymindata = val[j]; 
     }
  }
}

#ifdef DEBUG
printf("ompute_min_max_curves/ncurves=%d nmax=%d xmin/max: %f %f ymin/max = %f %f (error_bars=%d, iplan=%d)\n",
      ncurves, nmax, *xmindata, *xmaxdata, *ymindata, *ymaxdata, error_bars,
      iplan);
#endif

// Same scale in X and Y (assuming a square box)
if(iplan == 1) {
  xrange = *xmaxdata - *xmindata;
  yrange = *ymaxdata - *ymindata;
  xcent = (*xmaxdata + *xmindata) / 2.; 
  ycent = (*ymaxdata + *ymindata) / 2.; 
printf("xcent=%f ycent=%f\n", xcent, ycent);
  new_range = MAXI(xrange, yrange);
  *xmindata = xcent - new_range / 2.; 
  *xmaxdata = xcent + new_range / 2.; 
  *ymindata = ycent - new_range / 2.; 
  *ymaxdata = ycent + new_range / 2.; 
  }

return(0);
}
