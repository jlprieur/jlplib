/****************************************************************** 
* jlp_display1.cpp
* Routines to display one or two X,Y (float) curves 
*
* jlp_display1(xx, yy, istart, iend, xlabel, ylabel, title, plotdev);
* jlp_display2(xx1, yy1, istart1, iend1, xx2, yy2, istart2, iend2,
*              xlabel, ylabel, title, plotdev, nchar1, nchar2, 
*              pcolor1, pcolor2);
*
* JLP
* Version 02/04/2007
*******************************************************************/ 
#include <stdio.h>
#include <malloc.h>
#include "jlp_splot_idv.h"
#include "jlp_macros.h"

/***********************************************************************
* To display a X,Y (float) curve 
*
***********************************************************************/
int jlp_display1(float *xx, float *yy, int istart, int iend,
                 char *xlabel, char *ylabel, char *title, char *plotdev)
{
float *xplot, *yplot;
float errx[1],erry[1],xout[20],yout[20];
float xmin, xmax, ymin, ymax, expand = 1.2;
int ncurves, nout, error_bars, npts[2], iplan, y_is_reversed, dim, full_caption;
int idv, nout_max = 20, ticks_in, auto_scale;
char xlabel_0[41], ylabel_0[41], title_0[81], nchar[8], pcolor[60];
char filename[60], comments[80], out_filename[256];
int status;
register int i;

strcpy(out_filename, "tmp.ps");

/* Allocation of memory: */
dim = iend - istart + 1;
if( (xplot = (float*) malloc(dim * sizeof(float))) == NULL
  || (yplot = (float*) malloc(dim * sizeof(float))) == NULL)
  {
  printf("jlp_display1/Fatal error allocating memory: dim=%d\n", dim);
  exit(-1);
  }

for(i = istart; i <= iend; i++)
   {
   xplot[i - istart] = xx[i];
   yplot[i - istart] = yy[i];
   }

ymin = yplot[0];
ymax = yplot[0];
for(i = 1; i < dim; i++)
   {if(ymin > yplot[i]) ymin = yplot[i];
   if(ymax < yplot[i]) ymax = yplot[i];}
xmin = xplot[0];
xmax = xplot[0];
for(i = 1; i < dim; i++)
   {if(xmin > xplot[i]) xmin = xplot[i];
   if(xmax < xplot[i]) xmax = xplot[i];}
strcpy(nchar,"L0");
strcpy(pcolor,"Default");

strcpy(xlabel_0,xlabel);
strcpy(ylabel_0,ylabel);
strcpy(title_0,title);
strcpy(filename," ");
strcpy(comments," ");
full_caption = 1;

iplan = 0;
y_is_reversed = 0;
ncurves = 1;
error_bars = 0;
strcpy(&nchar[4],"L");
strcpy(&pcolor[32],"Default");

/* Initialize plotting device: */
status = JLP_DEVICE_CURVE(plotdev, out_filename, &xmin, &xmax, &ymin, &ymax, 
          &iplan, title_0, &idv);
if(status) return(-1);

/* Display the curve: */
npts[0] = dim;
ticks_in = 0;
auto_scale = 0;
NEWPLOT2(xplot,yplot,errx,erry,npts,&dim,&ncurves, 
         &xmin, &xmax, &ymin, &ymax, 
         &auto_scale, &iplan, &y_is_reversed, xlabel_0, ylabel_0, title_0,
         nchar,pcolor,xout,yout,&nout,&nout_max,&error_bars,filename,comments,
         &full_caption, &expand, &ticks_in, &idv);

/* Close display device: */
JLP_SPCLOSE(&idv);

free(xplot);
free(yplot);

return(0);
}
/*****************************************************************
* To display two curves
*
*****************************************************************/
int jlp_display2(float *xx1, float *yy1, int istart1, int iend1,
                 float *xx2, float *yy2, int istart2, int iend2,
                 char *xlabel, char *ylabel, char *title, char *plotdev,
                 char *nchar1, char *nchar2, char *pcolor1, char *pcolor2)
{
float *xplot, *yplot;
float errx[2],erry[2],xout[20],yout[20];
float xmin, xmax, ymin, ymax, expand = 1.2;
int ncurves, nout, error_bars, npts[2], iplan, idim; 
int full_caption, y_is_reversed;
int idv, nout_max = 20, ticks_in, auto_scale, status;
char xlabel_0[41], ylabel_0[41], title_0[81], nchar[8], pcolor[60];
char filename[60], comments[80], out_filename[256];
register int i, k;

strcpy(out_filename, "tmp.ps");

/* Allocation of memory: */
idim = MAXI(iend1 - istart1 + 1, iend2 - istart2 +1);
if( (xplot = (float*) malloc(2 * idim * sizeof(float))) == NULL
  || (yplot = (float*) malloc(2 * idim * sizeof(float))) == NULL)
  {
  printf("jlp_display2/Fatal error allocating memory: 2*idim=%d\n", 2*idim);
  exit(-1);
  }

/* 1st curve: */
for(i = istart1; i <= iend1; i++)
   {
   xplot[i - istart1] = xx1[i];
   yplot[i - istart1] = yy1[i];
   }
npts[0] = iend1 - istart1 + 1;

/* 2nd curve: */
for(i = istart2; i <= iend2; i++)
   {
   xplot[i - istart2 + idim] = xx2[i];
   yplot[i - istart2 + idim] = yy2[i];
   }
npts[1] = iend1 - istart1 + 1;

ymin = yplot[0];
ymax = yplot[0];
for(k = 0; k < 2; k++)
 {
 for(i = 0; i < npts[k]; i++)
   {
   if(ymin > yplot[i]) ymin = yplot[i];
   if(ymax < yplot[i]) ymax = yplot[i];
   }
 }  
xmin = xplot[0];
xmax = xplot[0];
for(k = 0; k < 2; k++)
 {
 for(i = 0; i < npts[k]; i++)
   {
   if(xmin > xplot[i]) xmin = xplot[i];
   if(xmax < xplot[i]) xmax = xplot[i];
   }
 }
strcpy(nchar,nchar1);
strcpy(&nchar[4],nchar2);
strcpy(pcolor,pcolor1);
strcpy(&pcolor[32],pcolor2);

strcpy(xlabel_0,xlabel);
strcpy(ylabel_0,ylabel);
strcpy(title_0,title);
strcpy(filename," ");
strcpy(comments," ");
full_caption = 1;

iplan = 0;
y_is_reversed = 0;
ncurves = 2;
error_bars = 0;

/* Initialize plotting device: */
status = JLP_DEVICE_CURVE(plotdev, out_filename, &xmin, &xmax, &ymin, &ymax, 
                          &iplan, title_0, &idv);
if(status) return(-1);

/* Display the curve: */
npts[0] = idim;
ticks_in = 0;
auto_scale = 0;
NEWPLOT2(xplot,yplot,errx,erry,npts,&idim,&ncurves,
         &xmin, &xmax, &ymin, &ymax, &auto_scale, &iplan, &y_is_reversed,
         xlabel_0,ylabel_0,title_0,nchar,pcolor,
         xout,yout,&nout,&nout_max,&error_bars,filename,comments,
         &full_caption,&expand,&ticks_in,&idv);

/* Close display device: */
JLP_SPCLOSE(&idv);

free(xplot);
free(yplot);

return(0);
}
