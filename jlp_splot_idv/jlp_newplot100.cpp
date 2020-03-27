/*****************************************************************
* Subroutines to draw multiple curves X1,Y1,X2,Y2,... 
* with possibility of error bars. 
*
* Only one package is possible :
* SPLOT (calling NEWPLOT21)
*
* Contains:
* NEWPLOT0
* From "newplot_set1.for", version 10/01/2012, 
*
* JLP
* Version : 13/02/2019
******************************************************************/

#include "jlp_splot_idv.h"

/*****************************************************************
* From Fortran routine (in "newplot_set1.for", version 10/01/2012) 
*  NEWPLOT0(X,Y,ERRX,ERRY,NPTS,NMAX,KCURVE,CHAR1,
*    1   CHAR2,TITLE,NCHAR,PCOLOR,PLOTDEV,ERROR_BARS,FILENAME,COMMENTS,
*    1   JLP_AXES,XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,Y_IS_LOG10,
*    1   EXPAND)
* and CPP interface (in "jlp_newplot.cpp", version 13/10/2016) 
*  NEWPLOT21(X,Y,ERRX,ERRY,NPTS,NMAX,KCURVE,XMIN,XMAX,YMIN,YMAX,AUTO_SCALE,
*    1   IPLAN,CHAR1,CHAR2,TITLE,NCHAR,PCOLOR,XOUT,YOUT,NOUT,NOUT_MAX,
*    1   PLOTDEV,ERROR_BARS,FILENAME,COMMENTS,FULL_CAPTION,JLP_AXES,
*    1   XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,Y_IS_LOG10,EXPAND,TICKS_IN,
*    1    ___ and idv1)
*
* INPUT PARAMETERS (if this is not stated otherwise):
*  X(NMAX,*) Y(NMAX, *) : arrays of the curves to display (REAL*4)
*  ERRX(NMAX, *) : corresponding errors in X to be displayed (REAL*4)
*  ERRY(NMAX, *) : corresponding errors in Y to be displayed (REAL*4)
*  NPTS(*) : array with the number of points for each curve
*  NMAX : maxi. number of points (declaration of the arrays)
*  KCURVE : number of curves (declaration of the arrays)
*  XMIN, XMAX, YMIN, YMAX : box limits (user coordinates, REAL*4)
*  AUTO_SCALE : flag set to 1 if automatic scale, 0 if XMIN, XMAX, YMIN, YMAX
*  IPLAN : flag set to 1 if same magnification in X and Y (plan), 0 otherwise
*  CHAR1 : X AXIS title
*  CHAR2 : Y AXIS title (IF CHAR(30:30).EQ.'H' : MONGO HISTO ???)
*  TITLE : general title of the graph
*  NCHAR(*)*4 : array of the symbols for the different curves
*  PCOLOR(*)*30 : color to be used for drawing the curves
*  XOUT(200) Y(200) : arrays of output interactive points (REAL*4)
*  NOUT : number of output interactive points
*  NOUT_MAX : maximum number of output interactive points
*  ERROR_BARS: flag set to 1 for error bars, 0 otherwise    
*  PLOTDEV*40 : output plotting device
*  FILENAME*60, COMMENTS*80: only used for the caption in case of hardcopy
*  FULL_CAPTION: flag, 1 for caption with filename and comments, 0 otherwise    
*  JLP_AXES : flag set to 1 for new box (compatible with grid), 0 otherwise
*  XGRID_IS_WANTED : flag set to 1 if underlying X grid is wanted, 0 otherwise
*  YGRID_IS_WANTED : flag set to 1 if underlying Y grid is wanted, 0 otherwise
*  X_IS_LOG10,Y_IS_LOG10 : flags set to 1 if logarithmic axes, 0 otherwise
*  Y_IS_REVERSED : flag set to 1 if Y axis is to be inverted (from big to small)
*  EXPAND : magnification scale for label fonts (default value is 1.2) 
*  TICKS_IN : flag set to 1 if box ticks are inside the box, 0 otherwise
*
* OUTPUT:
*  XOUT, YOUT: coordinates of interactive points
*  NOUT : number of output points 
*
******************************************************************/
int JLP_NEWPLOT0(float *X, float *Y, float *ERRX, float *ERRY, int *NPTS,
                 int *NMAX, int *KCURVE, float *XMIN, float *XMAX, 
                 float *YMIN, float *YMAX, int *AUTO_SCALE, int *IPLAN,
                 int *Y_IS_REVERSED, char *CHAR1, char *CHAR2,
                 char *TITLE, char *NCHAR, char *PCOLOR, float *XOUT,
                 float *YOUT, int *NOUT, int *NOUT_MAX, int *ERROR_BARS,
                 char *PLOTDEV, char *FILENAME, char *COMMENTS, 
                 int *FULL_CAPTION, int *JLP_AXES, int *XGRID_IS_WANTED, 
                 int *YGRID_IS_WANTED, int *X_IS_LOG10, int *Y_IS_LOG10, 
                 float *EXPAND, int *TICKS_IN)
{ 
int status = 0;

 status = jlp_newplot100(X, Y, ERRX, ERRY, NPTS, *NMAX, *KCURVE, *XMIN, *XMAX, 
                         *YMIN, *YMAX, *AUTO_SCALE, *IPLAN, *Y_IS_REVERSED, 
                         CHAR1, CHAR2, TITLE, NCHAR, PCOLOR, XOUT, YOUT, NOUT,
                         *NOUT_MAX, *ERROR_BARS, PLOTDEV, FILENAME, COMMENTS, 
                         *FULL_CAPTION, *JLP_AXES, *XGRID_IS_WANTED, 
                         *YGRID_IS_WANTED, *X_IS_LOG10, *Y_IS_LOG10, *EXPAND, 
                         *TICKS_IN);
return(status);
}
/**************************************************************************
*
**************************************************************************/
int jlp_newplot110(float *xplot, float *yplot, float *errx, float *erry,
                   int *npoints, int nmax, int ncurves, int iplan, 
                   int y_is_reversed, char *xlabel, char *ylabel, 
                   char *title, char *nchar, char *pcolor, int error_bars, 
                   char *plotdev, int jlp_axes_are_wanted,
                   int xgrid_is_wanted, int ygrid_is_wanted,
                   int x_is_log10, int y_is_log10, float expand,
                   int ticks_in)
{
int status, auto_scale = 1, nout_max = 128, full_caption = 0, nout;
float xmin_user, xmax_user, ymin_user, ymax_user; 
float xout[128], yout[128];
char filename[128], comments[80];

// Load dummy values for filename and comments, since full_caption == 0 :
strcpy(filename, "");
strcpy(comments, "");

// Compute xmin_user, xmax_user, ymin_user, ymax_user since auto_scale == 1 :
 jlp_splot_min_max_for_curves(xplot, yplot, errx, erry, npoints, nmax,
                              ncurves, &xmin_user, &xmax_user, &ymin_user,
                              &ymax_user, error_bars, iplan);

status = jlp_newplot100(xplot, yplot, errx, erry, npoints, nmax, ncurves, 
                        xmin_user, xmax_user, ymin_user, ymax_user, 
                        auto_scale, iplan, y_is_reversed,
                        xlabel, ylabel, title, nchar, pcolor, 
                        xout, yout, &nout, nout_max, error_bars, plotdev,
                        filename, comments, full_caption, jlp_axes_are_wanted,
                        xgrid_is_wanted, ygrid_is_wanted, x_is_log10, 
                        y_is_log10, expand, ticks_in);
return(status);
}
/**************************************************************************
*
**************************************************************************/
int jlp_newplot100(float *xplot, float *yplot, float *errx, float *erry,
                   int *npoints, int nmax, int ncurves, float xmin_user, 
                   float xmax_user, float ymin_user, float ymax_user, 
                   int auto_scale, int iplan, int y_is_reversed,
                   char *xlabel, char *ylabel, char *title, char *nchar,
                   char *pcolor, float *xout, float *yout, int *nout,
                   int nout_max, int error_bars, char *plotdev,
                   char *filename, char *comments,
                   int full_caption, int jlp_axes_are_wanted,
                   int xgrid_is_wanted, int ygrid_is_wanted,
                   int x_is_log10, int y_is_log10, float expand,
                   int ticks_in)
{ 
// Graphic help:
// INCLUDE 'jlpsub:jlp_graphic_help.for'

 char out_filename1[256];
 int idv1, status;
 
 if(ncurves == 0) {
   fprintf(stderr, "jlp_newplot100/Fatal error: kcurve = 0 \n");
   exit(-1);
   }
 if(npoints[0] == 0) {
   fprintf(stderr, "jlp_newplot100/Fatal error: npoints(1) = 0 \n");
   exit(-1);
   }
 if(nmax == 0) {
   fprintf(stderr, "jlp_newplot100/Fatal error: nmax = 0 \n");
   exit(-1);
   }
 

// Open a postscript device only (after 2014...)

/*
int JLP_DEVICE_CURVE(char *plotdev, char *out_filename,
                     float *xmin_user1, float *xmax_user1,
                     float *ymin_user1, float *ymax_user1, int *plan,
                     char *title, int *idv1);
*/
 strcpy(out_filename1, "tmp.ps");
 JLP_DEVICE_CURVE(plotdev, out_filename1, &xmin_user, &xmax_user, &ymin_user, 
                  &ymax_user, &iplan, title, &idv1);

/*
int newplot210(float *xplot, float *yplot, float *errx, float *erry,
              int *npoints, int nmax, int ncurves,
              float xmin_user, float xmax_user, float ymin_user,
              float ymax_user, int auto_scale, int iplan, int y_is_reversed,
              char *xlabel, char *ylabel, char *title, char *nchar,
              char *pcolor, float *xout, float *yout, int *nout,
              int nout_max, int error_bars, char *filename, char *comments,
              int full_caption, int jlp_axes_are_wanted,
              int xgrid_is_wanted, int ygrid_is_wanted,
              int x_is_log10, int y_is_log10, float expand,
              int ticks_in, int idv1)
*/
 status = newplot210(xplot, yplot, errx, erry, npoints, nmax, ncurves,
                     xmin_user, xmax_user, ymin_user, ymax_user, auto_scale, 
                     iplan, y_is_reversed, xlabel, ylabel, title, nchar,
                     pcolor, xout, yout, nout, nout_max, error_bars, 
                     filename, comments, full_caption, jlp_axes_are_wanted,
                     xgrid_is_wanted, ygrid_is_wanted, x_is_log10, y_is_log10, 
                     expand, ticks_in, idv1);

 JLP_SPCLOSE(&idv1);
 
 return(0);
}
