/***************************************************************
*
* NAME: jlp_splot2
*
* Contains:
*  int jlp_string_copy(char *in, char *out, int len)
*  JLP_CURVE(xplot,yplot,errx,erry,npoints,nchar,pcolor,error_bars,...);
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
#include "jlp_ctime.h"   // JLP_CTIME

int jlp_string_copy(char *in, char *out, int len);
int JLP_CURVE(float *xplot, float *yplot, float *errx, float *erry, 
               int *npoints, char *nchar, char *pcolor, int *error_bars, 
               int *x_is_log10, int *y_is_log10, int *idv1);
int JLP_CURVE_LINE(float *xplot, float *yplot, int *npoints, char *nchar1,
                   char *pcolor1, int *idv1);
int JLP_CURVE_HISTO(float *xplot, float *yplot, int *npoints, char *nchar1,
                    char *pcolor1, int *idv1);
int compute_min_max_curves(float *xplot, float *yplot, float *errx, 
                           float *erry, int *npoints, int nmax, int ncurves,
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
*  xplot[] = float array with X user coordinates
*  yplot[] = float array with Y user coordinates
*  npoints = number of points to draw
*  nchar   = type of character (L0= solid line, L1= dashed,
*            H0=histogram with solid line, 
*            H1= histogram with dashed line)
*  pcolor = color to be used (Purple, Aquamarine, ...)
*  error_bars = Flag (1 if error bars,  0 otherwise)
*
****************************************************************************/
int JLP_CURVE(float *xplot, float *yplot, float *errx, float *erry, 
              int *npoints, char *nchar, char *pcolor, int *error_bars, 
              int *idv1)
{
register int  i;
char ctest[2], default_color[40];
float min_xval, max_xval, min_yval, max_yval;
int isymb, isize, idv;
idv = *idv1;

// Check first if curve device:
if (GDev_from_idv(idv) == NULL) {
  fprintf(stderr,"JLP_CURVE/Error: opened device is image device igdev (not cgdev)\n");
  return(-1);
  } 

if (Jgdev_box_xmin(idv) == Jgdev_box_xmax(idv) 
    || Jgdev_box_ymin(idv) == Jgdev_box_ymax(idv)) {
  fprintf(stderr," jlp_curve/Fatal error range in x or y is null! \n");
  fprintf(stderr,"xmin=%f xmax=%f ymin=%f ymax=%f \n", 
          Jgdev_box_xmin(idv), Jgdev_box_xmax(idv), Jgdev_box_ymin(idv), 
          Jgdev_box_ymax(idv));
  return(-1);
}

/* Return if no points to be plotted: */
if(*npoints <= 0) {
  fprintf(stderr,"JLP_CURVE/Error: npoints=%d\n", *npoints);
  return(-1);
  }

/* Begin to plot: */
if(nchar[0] == 'L' || nchar[0] == 'l') {
  JLP_CURVE_LINE(xplot, yplot, npoints, nchar, pcolor, idv1);
 } else if(nchar[0] == 'H' || nchar[0] == 'h') {
  JLP_CURVE_HISTO(xplot, yplot, npoints, nchar, pcolor, idv1);
 } else {

/* Decoding the symbol and its size: */
   ctest[0] = nchar[0]; ctest[1] = '\0';
   isymb = atoi(ctest);
   ctest[0] = nchar[1]; ctest[1] = '\0';
   isize = atoi(ctest);
   ctest[0] = nchar[2]; ctest[1] = '\0';
   isize = atoi(ctest) + isize * 10;
/* If null size, draw a tiny point: */
   if(isize == 0) isymb = 1;
   JLP_SETPCOLOR(pcolor,&idv);

/* To handle the cases when box_xmin > box_xmax: (JLP2007) */
   if(Jgdev_box_xmin(idv) <= Jgdev_box_xmax(idv)) {
     min_xval = Jgdev_box_xmin(idv); max_xval = Jgdev_box_xmax(idv);
     } else {
     min_xval = Jgdev_box_xmax(idv); max_xval = Jgdev_box_xmin(idv);
     }
   if(Jgdev_box_ymin(idv) <= Jgdev_box_ymax(idv)) {
     min_yval = Jgdev_box_ymin(idv); max_yval = Jgdev_box_ymax(idv);
     } else {
     min_yval = Jgdev_box_ymax(idv); max_yval = Jgdev_box_ymin(idv);
     }

/* Drawing the points
* JLP2002: do not draw the symbols on the axes
*/
   for(i=0; i < *npoints; ++i) {
    if(xplot[i] >= min_xval && xplot[i] <= max_xval
       && yplot[i] >= min_yval && yplot[i] <= max_yval) {
          JLP_SYMBOL1(&(xplot[i]),&(yplot[i]),&isize,&isymb,&idv);
          }
    }

/* Drawing the error bars */
   if(*error_bars == 1) 
   {
    for(i=0; i<*npoints; ++i) 
       if(xplot[i] >= min_xval && xplot[i] <= max_xval
          && yplot[i] >= min_yval && yplot[i] <= max_yval) {
           JLP_SYMBOL_ERRORY1(&(xplot[i]),&(yplot[i]),&(erry[i]),&isize,&idv);
           JLP_SYMBOL_ERRORX1(&(xplot[i]),&(yplot[i]),&(errx[i]),&isize,&idv);
           }
   }
}

// Back to default options:
  strcpy(default_color, "Default");
  JLP_SETPCOLOR(default_color, &idv);

return(0);
}
/*****************************************************************************
*
* FUNCTION: JLP_CURVE_LINE
*
* PURPOSE:
*
* INPUT:  
*  xplot[] = float array with X user coordinates
*  yplot[] = float array with Y user coordinates
*  npoints = number of points to draw
*  nchar   = type of character (L0= solid line, L1= dashed)
*  pcolor  = color to be used for drawing the lines ("Default", "Red", etc)
*
****************************************************************************/
int JLP_CURVE_LINE(float *xplot, float *yplot, int *npoints, char *nchar, 
                   char *pcolor, int *idv1)
{
int i, status = -1, ltype = 0;
double *d_xplot, *d_yplot;

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
if(*npoints <= 0) {
  fprintf(stderr,"JLP_CURVE_LINE/Error: npoints=%d\n", *npoints);
  return(-1);
  }

// Conversion to double:
 d_xplot = new double[*npoints];
 d_yplot = new double[*npoints];
 for(i = 0; i < *npoints; i++) {
  d_xplot[i] = xplot[i];
  d_yplot[i] = yplot[i];
  }

/* Begins to plot: */
if(nchar[0] == 'L' || nchar[0] == 'l')
{
/* ltype = 0 for solid,  = 1 for dashed curves: */
   ltype = atoi(nchar+1);

/* Connecting the points */
   GDev_from_idv(*idv1)->draw_curved_line(d_xplot, d_yplot, *npoints, ltype, 
                                          pcolor);
   status = 0;
}
else {
   fprintf(stderr,"PLOT_CURVE_LINE/Error: bad line type, nchar[0] = %c\n", 
           nchar[0]);
   status = -1;
}

delete[] d_xplot;
delete[] d_yplot;
return(status);
}
/*****************************************************************************
* FUNCTION: JLP_CURVE_HISTO
* To draw a histogram
*
* INPUT:  
*  xplot[] = float array with X user coordinates
*  yplot[] = float array with Y user coordinates
*  npoints = number of points to draw
*  nchar   = type of character (H0=Solid black line, H1= Dashed black line, 
*  pcolor  = color to be used: Default (no filling), Aquamarine, Purple)
*
****************************************************************************/
int JLP_CURVE_HISTO(float *xplot, float *yplot, int *npoints, char *nchar1, 
                    char *pcolor1, int *idv1)
{
float x0, x1, y0, y1, min_xval, max_xval, min_yval, max_yval;
char nchar[4], pcolor[32];
int status = -1, lwidth = 0, ltype = 0, old_ltype = 0, coloured_histo = 0; 
register int i;

 if(!GDev_from_idv(*idv1)) {
   fprintf(stderr," JLP_CURVE_HISTO/Error: invalid device number (idv=%d)!\n",
           *idv1);
   return(-1);
  }

/* Transfer (for fortran interface) */
for(i = 0; i < MINI(4, (int)strlen(nchar1)) 
    && nchar1[i] && isprint(nchar1[i]); i++) nchar[i] = nchar1[i];
nchar[i] = '\0';

for(i = 0; i < MINI(32, (int)strlen(pcolor1)) 
    && pcolor1[i] && isprint(pcolor1[i]); i++) pcolor[i] = pcolor1[i];
pcolor[i] = '\0';
coloured_histo = (strncmp(pcolor1,"Default",7) == 0) ? 0 : 1;

if (Jgdev_box_xmin(*idv1) == Jgdev_box_xmax(*idv1) 
    || Jgdev_box_ymin(*idv1) == Jgdev_box_ymax(*idv1)) {
  fprintf(stderr," JLP_CURVE_HISTO/Error: range in x or y is null! \n");
  return(-1);
 }

/* Return if no points to be plotted: */
if(*npoints <= 0) {
  fprintf(stderr,"JLP_CURVE_HISTO/Error: npoints=%d\n", *npoints);
  return(-1);
  }

min_xval = GDev_from_idv(*idv1)->box_xmin();
max_xval = GDev_from_idv(*idv1)->box_xmax();
min_yval = GDev_from_idv(*idv1)->box_ymin();
max_yval = GDev_from_idv(*idv1)->box_ymax();
/* To handle cases when box_min > box_max: (JLP2007) */
if(min_xval > max_xval) {
   min_xval = max_xval; max_xval = GDev_from_idv(*idv1)->box_xmin();
   }
if(min_yval > max_yval) {
   min_yval = max_yval; max_yval = GDev_from_idv(*idv1)->box_ymin();
   }

/* Begins to plot: */
if(nchar[0] == 'H' || nchar[0] == 'h')
{

/* ltype = 0 for solid,  = 1 for dashed curves: 
*/
   ltype = atoi(nchar+1);
   if(ltype >= 1 && ltype <= 4) {
     lwidth = GDev_from_idv(*idv1)->lwidth();
     old_ltype = GDev_from_idv(*idv1)->ltype();
     printf("JLP_CURVE_HISTO/Setting lwidth=%d ltype=%d\n", lwidth, ltype);
     GDev_from_idv(*idv1)->SetLineWidthAndType(lwidth, ltype);
   }

/* Connecting the points */

/* First line to draw the first point (starting from 0) */
    i = 0;
    x0 = MAXI(xplot[i], min_xval);
    y0 = 0.;
    x1 = xplot[i];
    GDev_from_idv(*idv1)->gdev_line1(x0, y0, x1, y0); 

/* The other points: */
   for(i = 0; i < *npoints - 1; i++) {
      x0 = MINI(MAXI(xplot[i], min_xval), max_xval);
      x1 = MINI(MAXI(xplot[i+1], min_xval), max_xval);
      y1 = MINI(MAXI(yplot[i], min_yval), max_yval);
      if(coloured_histo) GDev_from_idv(*idv1)->FilledRect1(x0, 0., x1, y1, pcolor); 
/* Draw the line after the rectangle in order to see it (to be superimposed)*/
      GDev_from_idv(*idv1)->gdev_line1(x0, y0, x0, y1); 
      GDev_from_idv(*idv1)->gdev_line1(x0, y1, x1, y1); 
      y0 = y1;
     }
/* Last point: */
    i = *npoints - 1;
    x0 = MINI(MAXI(xplot[i], min_xval), max_xval);
    x1 = MINI(MAXI(xplot[i] + (xplot[i] - xplot[i-1]), min_xval), max_xval);
    y1 = MINI(MAXI(yplot[i], min_yval), max_yval);
    if(coloured_histo) GDev_from_idv(*idv1)->FilledRect1(x0, 0., x1, y1, pcolor); 
/* Draw the line after the rectangle in order to see it */
    GDev_from_idv(*idv1)->gdev_line1(x0, y0, x0, y1); 
    GDev_from_idv(*idv1)->gdev_line1(x0, y1, x1, y1); 
    GDev_from_idv(*idv1)->gdev_line1(x1, y1, x1, 0.); 

/* Restore initial settings: */
   if(ltype >= 1 && ltype <= 4)
     GDev_from_idv(*idv1)->SetLineWidthAndType(lwidth, old_ltype);
}
else {
   fprintf(stderr,"PLOT_CURVE_HISTO/Error: bad line type, nchar[0] = %c\n", 
           nchar[0]);
   status = -1;
}

return(status);
}
/***************************************************************************
* Compute min, max for plots
***************************************************************************/
int compute_min_max_curves(float *xplot, float *yplot, float *errx, 
                           float *erry, int *npoints, int nmax, int ncurves,
                           float *xmindata, float *xmaxdata, float *ymindata,
                           float *ymaxdata)
{
float val[3];
int i, j, k;

*xmindata = 1.e12;
*xmaxdata = -1.e12;
*ymindata = 1.e12;
*ymaxdata = -1.e12;

if(errx != NULL && erry != NULL) {
  for(k = 0; k < ncurves; k++) {
    for(i = 0; i < npoints[k]; i++) {
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
    for(i = 0; i < npoints[k]; i++) {
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
printf("compute_min_max_curves/ncurves=%d nmax=%d xmin/max: %f %f ymin/max = %f %f\n",
      ncurves, nmax, *xmindata, *xmaxdata, *ymindata, *ymaxdata);
#endif

return(0);
}
