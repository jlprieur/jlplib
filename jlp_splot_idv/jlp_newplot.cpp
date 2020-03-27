/***************************************************************
* NAME: jlp_newplot
*
* Contains:
*  NEWPLOT2(xplot,yplot,errx,erry,npoints,nmax,ncurves,xmin_user,xmax_user,
            ymin_user, ymax_user,auto_scale,iplan,xlabel1,ylabel1,title1,
*           nchar1,pcolor1,xout,yout,nout,nout_max,error_bars,filename,comments,
*           full_caption,expand,ticks_in,idv1)
*  NEWPLOT21(xplot,yplot,errx,erry,npoints,nmax,ncurves,xlabel1,ylabel1,title1,
*           nchar1,pcolor1,xout,yout,nout,nout_max,error_bars,filename,comments,
*           full_caption,jlp_axes_are_wanted, xgrid_is_wanted,ygrid_is_wanted,
*           x_is_log10,y_is_log10,expand,idv1)
*  newplot210(float *xplot, float *yplot, float *errx, float *erry,
*              int *npoints, int nmax, int ncurves,
*              char *xlabel, char *ylabel, char *title, char *nchar,
*              char *pcolor, float *xout, float *yout, int *nout,
*              int nout_max, int error_bars, char *filename, char *comments,
*              int full_caption, int jlp_axes_are_wanted,
*              int xgrid_is_wanted, int ygrid_is_wanted,
*              int x_is_log10, int y_is_log10, float expand,
*              int ticks_in, int idv1)
*  NEWPLOT2_HARDCOPY(float *xplot, float *yplot, float *errx, float *erry,...)
*
* VERSION: 13/10/2016 
*
* AUTHOR: JLP 
*
***************************************************************/
#include <stdio.h>
#include <string.h>
#include <ctype.h>       /* isgraph, isprint, etc... */
#include <malloc.h>
#include <math.h>
#include "jlp_time0.h"   // JLP_CTIME

#include "jlp_splot_idv.h"
#include "jlp_gdev_idv.h"

/* defined in "jlp_splot_idv_prototypes.h" that is called by "jlp_splot_idv.h"
int NEWPLOT2(float *xplot, float *yplot, float *errx, float *erry,
             int *npoints, int *nmax, int *ncurves,
             float *xmin_user, float *xmax_user, float *ymin_user, 
             float *ymax_user, int *auto_scale, int *iplan, int *y_is_reversed,
             char *xlabel, char *ylabel, char *title, char *nchar,
             char *pcolor, float *xout, float *yout, int *nout,
             int *nout_max, int *error_bars, char *filename, char *comments,
             int *full_caption, float *expand, int *ticks_in, int *idv1);
int NEWPLOT21(float *xplot, float *yplot, float *errx, float *erry, 
              int *npoints, int *nmax, int *ncurves,
              float *xmin_user, float *xmax_user, float *ymin_user, 
              float *ymax_user, int *auto_scale, int *iplan, int *y_is_reversed,
	      char *xlabel, char *ylabel, char *title, char *nchar,
              char *pcolor, float *xout, float *yout, int *nout, 
              int *nout_max, int *error_bars, char *filename, char *comments, 
              int *full_caption, int *jlp_axes_are_wanted,
              int *xgrid_is_wanted, int *ygrid_is_wanted,
              int *x_is_log10, int *y_is_log10, float *expand, 
              int *ticks_in, int *idv1);
int newplot210(float *xplot, float *yplot, float *errx, float *erry, 
              int *npoints, int nmax, int ncurves,
              float *xmin_user, float *xmax_user, float *ymin_user, 
              float *ymax_user, int *auto_scale, int *iplan, int *y_is_reversed,
	      char *xlabel, char *ylabel, char *title, char *nchar,
              char *pcolor, float *xout, float *yout, int *nout, 
              int nout_max, int error_bars, char *filename, char *comments, 
              int full_caption, int jlp_axes_are_wanted,
              int xgrid_is_wanted, int ygrid_is_wanted,
              int x_is_log10, int y_is_log10, float expand, 
              int ticks_in, int idv1);
int NEWPLOT2_HARDCOPY(float *xplot, float *yplot, float *errx, float *erry,
                      int *npoints, int *nmax, int *ncurves,
                      float *xmin_user, float *xmax_user, float *ymin_user, 
                      float *ymax_user, int *auto_scale, int *iplan,
                      int *y_is_reversed,
                      char *xlabel, char *ylabel, char *title, char *nchar,
                      char *pcolor, int *error_bars, char *filename,
                      char *comments, int *full_caption, 
                      int *jlp_axes_are_wanted, int *xgrid_is_wanted,
                      int *ygrid_is_wanted, int *x_is_log10, int *y_is_log10,
                      float *expand, int *idv1, char *pst_filename);
int jlp_comments_for_curves(char *filename, char *comments, float expand,
                            int idv);
*/
/***************************************************************
* NEWPLOT2
* To plot a curve with splot
*
* INPUT:
* xplot[i + j * nmax]: x value #i of curve #j
* errx[i + j * nmax]: x error value #i of curve #j
* erry[i + j * nmax]: y error value #i of curve #j
* nchar[]: type of symbols used for plotting the curves
* pcolor[]: color to be used for drawing the curves
* error_bars: flag (1 if error bars, 0 otherwise)
*
* OUTPUT:
* nout: number of values entered with the cursor
* xout[nout_max],yout[nout_max]: arrays x,y of positions entered
*                                with the cursor (nout values)
***************************************************************/
int NEWPLOT2(float *xplot, float *yplot, float *errx, float *erry,
             int *npoints, int *nmax, int *ncurves,
             float *xmin_user, float *xmax_user, float *ymin_user, 
             float *ymax_user, int *auto_scale, int *iplan, int *y_is_reversed,
             char *xlabel, char *ylabel, char *title, char *nchar,
             char *pcolor, float *xout, float *yout, int *nout,
             int *nout_max, int *error_bars, char *filename, char *comments,
             int *full_caption, float *f_expand, int *ticks_in, int *idv1)
{
int status, x_is_log10, y_is_log10;
int jlp_axes_are_wanted, xgrid_is_wanted, ygrid_is_wanted; 
char title1[80], xlabel1[80], ylabel1[80], filename1[80], comments1[80];

/* Filter on labels (for Fortran): */
jlp_string_copy(title, title1, 80);
jlp_string_copy(xlabel, xlabel1, 80);
jlp_string_copy(ylabel, ylabel1, 80);
jlp_string_copy(filename, filename1, 80);
jlp_string_copy(comments, comments1, 80);

x_is_log10 = 0;
y_is_log10 = 0;
jlp_axes_are_wanted = 0;
xgrid_is_wanted = 0;
ygrid_is_wanted = 0;

// Calling newplot210
status =  newplot210(xplot, yplot, errx, erry, npoints, *nmax, *ncurves,
                     *xmin_user, *xmax_user, *ymin_user, *ymax_user,
                     *auto_scale, *iplan, *y_is_reversed,
	             xlabel1, ylabel1, title1, nchar, pcolor, xout, yout, nout, 
                     *nout_max, *error_bars, filename1, comments1, 
                     *full_caption, jlp_axes_are_wanted, xgrid_is_wanted, 
                     ygrid_is_wanted, x_is_log10, y_is_log10, *f_expand, 
                     *ticks_in, *idv1);
return(status);
}
/***************************************************************
* NEWPLOT21 
* To plot a curve with splot (same as NEWPLOT2 but with possibility
* of logarithmic axes, and grids) 
*
* INPUT:
* xplot[i + j * nmax]: x value #i of curve #j
* errx[i + j * nmax]: x error value #i of curve #j
* erry[i + j * nmax]: y error value #i of curve #j
* nchar[]: type of symbols used for plotting the curves
* pcolor[]: color to be used for drawing the curves
* error_bars: flag (1 if error bars, 0 otherwise)
* x_is_log10 : 0 if linear, 1 if log10 axis
* y_is_log10 : 0 if linear, 1 if log10 axis
* jlp_axes_are_wanted: flag set to one if JLP_axes are wanted
* xgrid_is_wanted: flag set to one if X grid is wanted
* ygrid_is_wanted: flag set to one if Y grid is wanted
*
* NB: In the case of logarithmic axes, xplot, errx, erry should already
*     converted to log10.
*
* OUTPUT:
* nout: number of values entered with the cursor
* xout[nout_max],yout[nout_max]: arrays x,y of positions entered 
*                                with the cursor (nout values)
***************************************************************/
int NEWPLOT21(float *xplot, float *yplot, float *errx, float *erry, 
              int *npoints, int *nmax, int *ncurves,
              float *xmin_user, float *xmax_user, float *ymin_user, 
              float *ymax_user, int *auto_scale, int *iplan, int *y_is_reversed,
	      char *xlabel, char *ylabel, char *title, char *nchar,
              char *pcolor, float *xout, float *yout, int *nout, 
              int *nout_max, int *error_bars, char *filename, char *comments, 
              int *full_caption, int *jlp_axes_are_wanted,
              int *xgrid_is_wanted, int *ygrid_is_wanted,
              int *x_is_log10, int *y_is_log10, float *f_expand, 
              int *ticks_in, int *idv1)
{
int status;
char title1[80], xlabel1[80], ylabel1[80], filename1[80], comments1[80];

/* Filter on labels (for Fortran): */
jlp_string_copy(title, title1, 80);
jlp_string_copy(xlabel, xlabel1, 80);
jlp_string_copy(ylabel, ylabel1, 80);
jlp_string_copy(filename, filename1, 80);
jlp_string_copy(comments, comments1, 80);

// Calling newplot210
status =  newplot210(xplot, yplot, errx, erry, npoints, *nmax, *ncurves,
                     *xmin_user, *xmax_user, *ymin_user, *ymax_user,
                     *auto_scale, *iplan, *y_is_reversed,
	             xlabel1, ylabel1, title1, nchar, pcolor, xout, yout, nout, 
                     *nout_max, *error_bars, filename1, comments1, 
                     *full_caption, *jlp_axes_are_wanted, *xgrid_is_wanted, 
                     *ygrid_is_wanted, *x_is_log10, *y_is_log10, *f_expand, 
                     *ticks_in, *idv1);
return(status);
}
/***********************************************************************
* C,CPP version (i.e., call with values and without references) of NEWPLOT21 
***********************************************************************/
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
{
int k, hardcopy_device;
int npts;
char title1[80], xlabel1[80], ylabel1[80], filename1[80], comments1[80];
float xmindata, xmaxdata, ymindata, ymaxdata, yyy;
char axis_color[40];

// Output parameters:
*nout = 0;

strcpy(axis_color, "Black");

if(GDev_from_idv(idv1) == NULL) {
 fprintf(stderr, "newplot210/Sorry only graphic device cgdev is needed here\n");
 return(-1);
 } 

hardcopy_device = (Jgdev_dev_type(idv1) == 2) ? 1 : 0;

// If auto_scale, compute min max x-y-values of curves:
// TOBEDONE :If JLP axes: take only min/max (will compute exact boundaries in splot)
// but if SMongo axes: take +/- 5 percents larger frame
if(auto_scale == 1) {
 jlp_splot_min_max_for_curves(xplot, yplot, errx, erry, npoints, nmax, 
                                  ncurves, &xmindata, &xmaxdata, &ymindata, 
                                  &ymaxdata, error_bars, iplan);
 } else {
 xmindata = xmin_user;
 xmaxdata = xmax_user;
 ymindata = ymin_user;
 ymaxdata = ymax_user;
 }

if(y_is_reversed == 1) {
 yyy = ymindata;
 ymindata = ymaxdata;
 ymaxdata = yyy;
 }

// DEBUG
printf("ZZZ: jlp_axes=%d expand=%f ticks_in=%d full_caption=%d\n", 
       jlp_axes_are_wanted, expand, ticks_in, full_caption);

/* Filter on labels (for Fortran): */
jlp_string_copy(title, title1, 80);
jlp_string_copy(xlabel, xlabel1, 80);
jlp_string_copy(ylabel, ylabel1, 80);
jlp_string_copy(filename, filename1, 80);
jlp_string_copy(comments, comments1, 80);
 jlp_splot_box(xmindata, xmaxdata, ymindata, ymaxdata, xlabel1, ylabel1, 
               title1, ticks_in, 1, full_caption, jlp_axes_are_wanted, 
               xgrid_is_wanted, ygrid_is_wanted, x_is_log10, y_is_log10, 
               expand, axis_color, idv1);

 if(hardcopy_device && full_caption) 
        jlp_comments_for_curves(filename1, comments1, expand, idv1);

/* Draw in loop with JLP_CURVE */
  for(k=0; k < ncurves; k++) {
    if(npoints[k] > 0) {
      npts = npoints[k];
// JLP_CURVE contained in "jlp_splot2.cpp"
      JLP_CURVE(&xplot[nmax * k],&yplot[nmax * k],
                 &errx[nmax * k], &erry[nmax * k],
                 &npts, &nchar[4*k], &pcolor[32*k], &error_bars, 
                 &idv1);
     }
   }

   JLP_GFLUSH(&idv1);

// If interactive device, possibility of simple menu: 
#ifdef JLP_USE_X11
interactive_device = (Jgdev_dev_type(*idv1) == 1) ? 1 : 0;
if(interactive_device)
   jlp_newplot_menu(xplot,yplot,errx,erry,npoints,nmax,ncurves,xlabel,ylabel,
                    title,nchar,pcolor,xout,yout,nout,nout_max,error_bars,
                    filename,comments,full_caption,jlp_axes_are_wanted,
                    xgrid_is_wanted, ygrid_is_wanted, x_is_log10, 
                    y_is_log10, idv1);
#endif

return(0);
}
/************************************************************************
* Routine to create a postscript file to be able to print the current plot
* with the same appearance as with NEWPLOT2
* (called by jlp_newplot_menu)
*
************************************************************************/
int NEWPLOT2_HARDCOPY(float *xplot, float *yplot, float *errx, float *erry,
                      int *npoints, int *nmax, int *ncurves,
                      float xmin_user, float xmax_user, float ymin_user, 
                      float ymax_user, int auto_scale, int iplan, 
                      int y_is_reversed,
                      char *xlabel, char *ylabel, char *title, char *nchar,
                      char *pcolor, int *error_bars, char *filename,
                      char *comments, int *full_caption, 
                      int *jlp_axes_are_wanted, int *xgrid_is_wanted,
                      int *ygrid_is_wanted, int *x_is_log10, int *y_is_log10,
                      float *f_expand, int *idv1, char *pst_filename)
{
int idv_pst, offx1, offy1, axlen1, aylen1, plan, nout;
int ticks_in;
float xmin, xmax, ymin, ymax, xout[1], yout[1]; 
char plotdev2[80];
int interactive_device, status;

 if(GDev_from_idv(*idv1) == NULL) {
  fprintf(stderr, "NEWPLOT2_HARDCOPY/Sorry only graphic device cgdev is allowed here\n");
  return(-1);
  } 

 if(!(*pst_filename)) strcpy(pst_filename,"pst.tmp");
 sprintf(plotdev2,"square/%s",pst_filename);

/* Prompt for hardcopy options */
 interactive_device = (Jgdev_dev_type(*idv1) == 1) ? 1 : 0;

 JLP_GET_PLOT_PARAM(&offx1, &offy1, &axlen1, &aylen1, &xmin, &xmax,
                    &ymin, &ymax, &plan, idv1);


/* Initialize plotting device: */
 status = JLP_DEVICE_CURVE(plotdev2, pst_filename, &xmin, &xmax, &ymin, &ymax,
                           &plan, filename, &idv_pst);
 if(status) {
    fprintf(stderr, 
            "NEWPLOT2_HARDCOPY/Fatal error opening graphic device: %s \n", 
            plotdev2);
    exit(-1);
    }

// The cursor option is neutralized with nout = 0
nout = 0;
ticks_in = 0;

 NEWPLOT21(xplot,yplot,errx,erry,npoints,nmax,ncurves,
           &xmin_user, &xmax_user, &ymin_user, &ymax_user, 
           &auto_scale, &iplan, &y_is_reversed,
           xlabel,ylabel,title,nchar,pcolor,xout,yout,&nout,&nout,error_bars,
           filename,comments, full_caption, jlp_axes_are_wanted,
           xgrid_is_wanted, ygrid_is_wanted, x_is_log10, y_is_log10, 
           f_expand, &ticks_in, &idv_pst);


/* Close postscript file and free idv number: */
 JLP_SPCLOSE(&idv_pst);

return(0);
}
/**************************************************************************
* Write comments on upper right corner
* for curves
* (called by newplot and display2)
*
**************************************************************************/
int jlp_comments_for_curves(char *filename, char *comments, float expand,
                            int idv)
{
float expand1;
int ix, iy, istat;
char date_time[80], buffer[80];
register int i;

/* If hardcopy and full_caption 
* writes filename, date and time on top right corner: */
   ix=29000;
   iy=35000;
/* Discard blanks and undrawable characters: */
   for(i = 0; i < 60; i++) if(!isgraph(filename[i])) break;
   filename[i] = '\0';
/* Filename (if present): */
   if(*filename && *filename != ' ')
     {
     sprintf(buffer,"File: %s",filename);
/* Truncation at 22 characters to remain within the page boundaries... */
     buffer[22]='\0';
/* Filename will be printed with smaller charecters than X and Y labels: */
     expand1 = 0.8 * expand;
     jlp_label(buffer,ix,iy,0.,expand1,1,idv); 
     iy -= 600;
     }

/* Two lines of comments (if present): */
/* Discard undrawable characters: */
   for(i = 0; i < 80; i++) if(!isprint(comments[i])) break;
   comments[i] = '\0';
   if(*comments && *comments != ' ')
     {
/* Comments will be printed with smaller charecters than X and Y labels: */
     expand1 = 0.7 * expand;
     sprintf(buffer,"Comments: %s",comments);
/* Truncation at 28 characters to remain within the page boundaries... */
     buffer[28]='\0';
     jlp_label(buffer,ix,iy,0.,expand1,1,idv); 
     iy -= 500;
/* Warning "Comments: " uses 10 characters, 
   hence we go on at location 28-10+1 */
     sprintf(buffer,"%s",&comments[19]);
     buffer[30]='\0';
     jlp_label(buffer,ix,iy,0.,expand1,1,idv); 
     iy -= 500;
     }

/* Date and time: */
   JLP_CTIME(date_time,&istat);
#ifdef DEBUG
   printf(" OK hardcopy device: %s \n",date_time);
#endif
   sprintf(buffer,"Date: %s",date_time);
/* Date and time  will be printed with smaller charecters than X and Y labels: */
   expand1 = 0.7 * expand;
   jlp_label(date_time,ix,iy,0.,expand1,1,idv); 

return(0);
}
