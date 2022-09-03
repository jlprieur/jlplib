/***************************************************************
* NAME: jlp_splot2_set
* complement for jlp_splot2.cpp
*
* Contains:
*  jlp_string_copy(char *in, char *out, int len)
*  jlp_newplot_menu(double *xplot, double *yplot, double *errx, double *erry, ...) 
*  jlp_format(char *string, double value, int TeX_flag)
*  CURSOR_GET_COORDINATES(double *xout, double *yout, int *nout, ...)
*  old_jlp_axis(xorigin,yorigin,axis_length,
*               min_user,max_user,angle,label_flag,ticks_up,TeX_flag,idv1)
*
*
* VERSION: 21/07/2007 
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
#include "jlp_splot_idv_prototypes.h"   // SYMBOL1
#include "jlp_macros.h"                 // MAXI 
#include "jlp_gdev_def.h"
#include "jlp_dialog.h"

static int CURSOR_GET_COORDINATES(double *xout, double *yout, 
                                  int *nout, int *nout_max, int *idv1);

/**********************************************************************
* Display a simple menu
* with the possibility of getting some coordinates of pixels with the cursor
*
**********************************************************************/
int jlp_newplot_menu(double *xplot, double *yplot, double *errx, double *erry, 
                     int *npoints, int *nmax, int *ncurves,
                     char *xlabel, char *ylabel, char *title, char *nchar,
                     char *pcolor, double *xout, double *yout, int *nout, 
                     int *nout_max, int *error_bars, char *filename, 
                     char *comments, int *full_caption, 
                     int *jlp_axes_are_wanted, int *xgrid_is_wanted,
                     int *ygrid_is_wanted, int *xaxis_type, int *yaxis_type,
                     int *idv1)
{
const int MENU_NITEMS=5;
/* Menu string length: */
const int MENU_SLEN=40;
/* Max number of submenus: */
const int MENU_NSUB=1;
int menu_nsub, menu_slen, menu_select, menu_subselect, vertical;
int  nitems, exit_item, status;
char items[MENU_NITEMS*MENU_NSUB*MENU_SLEN], pst_filename[40];
double expand = 1.2;
register int i;

 *nout = 0;

/* Displays menu: */
 menu_slen = MENU_SLEN;
 menu_nsub = MENU_NSUB;
/* Initialization of items and subitems to zero */
 for(i = 0; i < menu_slen * menu_nsub * MENU_NITEMS; i++) items[i] = '\0';

/* Filling menu items and subitems (from bottom): */
 i = 0;
 strcpy(&items[i*menu_slen*menu_nsub + 0*menu_slen], "Cursor");
 i = 1;
 strcpy(&items[i*menu_slen*menu_nsub + 0*menu_slen], "Hardcopy");
 i = 2;
 strcpy(&items[i*menu_slen*menu_nsub + 0*menu_slen], "Exit");
 nitems = i+1;

/* Displays the menu: */
  vertical = 0;
 jlp_setup_menu(items,&nitems,&menu_nsub,&menu_slen,&vertical,idv1);

/* Flushes graphic to screen; */
  JLP_GFLUSH(idv1);

/* MENU loop: */
 menu_select = -1;
 exit_item = 2;
 while(menu_select != exit_item)
 {
/* Next event */
  status = jlp_select_menu(&menu_select,&menu_subselect,idv1);

  if(status != 0) break;

  switch (menu_select) {

/* Cursor : */
    case 0:
      CURSOR_GET_COORDINATES(xout, yout, nout, nout_max, idv1);
      break;
/* Hardcopy : */
    case 1:
      pst_filename[0] = '\0';
/* TOBEDONE
      NEWPLOT2_HARDCOPY(xplot,yplot,errx,erry,npoints,nmax,ncurves,
                        xlabel,ylabel,title,nchar,pcolor,error_bars,
                        filename,comments, full_caption, jlp_axes_are_wanted,
                        xgrid_is_wanted, ygrid_is_wanted, xaxis_type, 
                        yaxis_type, &expand, idv1, pst_filename);
*/
      printf(" to be converted to double precision later..\n");
      break;
    default:
      break;
    } /* EOF switch */
 } /* EOF while */

return(0);
}
/**********************************************************************
* Display a simple menu
* with the possibility of getting some coordinates of pixels with the cursor
*
**********************************************************************/
static int CURSOR_GET_COORDINATES(double *xout, double *yout, int *nout, 
                                  int *nout_max, int *idv1)
{
double xc, yc;
int status, draw_cross;
int isymbol, isize, in_frame, pressed_button;
char label[80];
int interactive_device;

*nout = MAXI(0,*nout);
if(*nout >= *nout_max) {
  printf("CURSOR_GET_COORDINATES/Error: *nout = %d  >= nout_max = %d\n",
         *nout, *nout_max);
  return(-1);
  }


/* Device type:
  1 = X11
  2 = Postscript file
  3 = wxWidgets
No longer used:
  4 = HPGL
  5 = Tektronix
*/
if(GDev_from_idv(*idv1))
 interactive_device = (GDev_from_idv(*idv1)->Jgc0_dev_type() == 1) ? 1 : 0;
else 
 interactive_device = 0;

if(!interactive_device) return(-1);

strcpy(label,"To exit, click outside of the frame");
JLP_DRAW_TO_STATUS_BAR(label, idv1);

  isize = 4; isymbol = 4;
  draw_cross = 0;
  status = -1;
/*
  status = JLP_WHERE(&xc,&yc,&in_frame,&pressed_button,&draw_cross,idv1);
  while(status == 0 && in_frame == 1 && *nout < *nout_max) {
    JLP_SYMBOL1(&xc,&yc,&isize,&isymbol,idv1);
    printf("CURSOR_GET_COORDINATES/Point #%d entered (x=%f, y=%f)\n",
            *nout,xc,yc);
    xout[*nout] = xc;
    yout[*nout] = yc;
    (*nout)++;
    status = JLP_WHERE(&xc,&yc,&in_frame,&pressed_button,&draw_cross,
                       idv1);
    }
if(*nout >= *nout_max) {
  printf("CURSOR_GET_COORDINATES/Error: *nout = %d  >= nout_max = %d\n",
         *nout, *nout_max);
  status = -1;
  } else { 
    status = 0;
  }
*/
 printf("ZZZZ to be converted to double precision later..\n");

JLP_ERASE_STATUS_BAR(idv1);
return(status);
}

/***************************************************************************
*
* FUNCTION: old_jlp_axis
*           (before July 2007: linear axes only and without grids)
*
* PURPOSE:
*
* INPUT:  
*         xorigin = x coordinate of the origin in pixels
*         yorigin = y coordinate of the origin in pixels
*         axis_length = width of the box in pixels
*         min_user = lower user value in X
*         max_user = upper user value in X
*         angle = [0. or 90] angle with horizontal reference (only discrete:)
*         label_flag = [0 or 1] draw labels if flag set to 1
* For ticks out: upper axis: ticks_up = 1;  lower axis: ticks_up = 0
* For ticks out: right axis: ticks_up = 0;  left axis: ticks_up = 1
*
****************************************************************************/
int old_jlp_axis(int xorigin, int yorigin, int axis_length,
                 double min_user, double max_user, double angle, int label_flag,
                 int ticks_up, int TeX_flag, int idv)
{
int   xnow, ynow, ix, iy, itick;
int   xoff, yoff;             /* size of the labels in x and y */
int   ticklen;                /* tick length */
int   nlabels;                /* minimum number of labels */
int   istep, nsticks, iexpo, ioffset, clength;
double x_tick, xstep, sxstep, sign0, cos0, sin0;
double range_user, log_step, mantissa;
double cheight, expand, xvec;
char  string[15];

/* Size of numbers: */
expand = 1.2;
/*
if(TeX_flag) expand = 1.0;
*/

// WARNING: only after calling jlp_label is cheight initialized 
// So I use a dummy call to jlp_label:
ix = 0; iy = 0;
strcpy(string, "test");
jlp_label(string,ix,iy,0.,expand,0,idv);

cheight = GDev_from_idv(idv)->Jgc0_cheight();

/* tick length */
ticklen=600; 

if(max_user < min_user) sign0 = -1.;
  else sign0 = 1.;

/* Get the power of ten for the step (assuming nlabels to start): */
nlabels = 3;
range_user = max_user - min_user; 
log_step = log10( (double)(sign0*range_user)/(double)nlabels);
iexpo = (int)log_step;
if(log_step < 0) iexpo = iexpo - 1;

/* Computes istep according to the value of the mantissa of the step */
/* istep: step defining the intervals between two big ticks
*  nsticks: number of small ticks in each interval bounded with two big ticks
*/
mantissa = log_step - iexpo;
if( mantissa < .15) {istep = 1; nsticks = 5;}
else if( mantissa < .5) {istep = 2; nsticks = 4;}
else if( mantissa < .85) {istep = 5; nsticks = 5;}
else {istep = 10; nsticks = 5;}

/* Value of the step for labeled ticks (big ones)*/
xstep = sign0 * (double)istep * pow( 10.0, (double)iexpo); 

/* Offset, since the first tick is not always in the first pixel: */
ioffset = (int)(min_user/xstep);
if(min_user/xstep < 0) ioffset = ioffset -1;
x_tick = xstep*(double)ioffset;
if(sign0*x_tick < sign0*min_user) x_tick = xstep * (double)(ioffset+1);

if (angle == 0.)
{
/* Draw axis without ticks */
  jlp_line(xorigin,yorigin,xorigin+axis_length,yorigin,idv);

/* For ticks out: upper axis: ticks_up = 1;  lower axis: ticks_up = 0 */
  if (ticks_up) ticklen = -ticklen;

/* Draw big ticks */
  while( sign0*x_tick <= sign0*max_user)
  {
#ifdef JLP_FORM
   jlp_format(string,x_tick,TeX_flag);
#else
   sprintf(string,"%.3g",x_tick); 
#endif
  xnow = (int)(axis_length * (x_tick - min_user)/range_user);
  if (label_flag == 1) 
    {
#ifdef DEBUG
printf("jlp_axis/DEBUG: string=%s xorigin=%d yorigin=%d expand=%.1f\n",
        string, xorigin, yorigin, expand);
#endif
    xoff = (int)jlp_label(string,xorigin,yorigin,0.,expand,0,idv);
    yoff = (int)(ticklen + cheight + 600.);
/* JLP2006, I replace (xoff / 2.) with (xoff * 0.7) */
    jlp_label(string,(int)(xorigin+xnow-xoff*0.7),yorigin-yoff,0.,expand,1,idv);
    }

/* Draw big tick now: */
  jlp_line(xorigin+xnow,yorigin,xorigin+xnow,yorigin-ticklen,idv);

  x_tick += xstep;
  }  
} /* End of X axis case */

/* Y vertical case: */
else if(angle == 90.)
 {
/* Draw axis without ticks */
  jlp_line(xorigin,yorigin,xorigin,yorigin+axis_length,idv);

/* For ticks out: right axis: ticks_up = 0;  left axis: ticks_up = 1 */
  if (!ticks_up) ticklen = -ticklen;

/* Draw big ticks */
  while( sign0*x_tick <= sign0*max_user)
  {
#ifdef JLP_FORM
   jlp_format(string,x_tick,TeX_flag);
#else
   sprintf(string,"%.3g",x_tick); 
#endif
   ynow = (int)(axis_length * (x_tick - min_user)/range_user);
   if (label_flag == 1) 
    {
    clength = (int)jlp_label(string,xorigin,yorigin,0.,expand,0,idv);
/* JLP2004: before: 400, but slightly too far from axis: 
* Remember: ticklen=600 
*/
    xoff = 300 + ticklen + clength;
    yoff = (int)(cheight / 2.);
    jlp_label(string,xorigin-xoff,yorigin+ynow-yoff,0.,expand,1,idv);
    }

/* Draw big tick now: */
  jlp_line(xorigin,yorigin+ynow,xorigin-ticklen,yorigin+ynow,idv);

  x_tick += xstep;
  }  
} /* End of Y axis case */
/* Case of any angle not zero or 90 degrees: */
else
{
 cos0 = cos((double)(angle*3.14159/180.));
 sin0 = sin((double)(angle*3.14159/180.));
 ix = (int)((double)axis_length * cos0);
 iy = (int)((double)axis_length * sin0);
/* Draw axis without ticks */
  jlp_line(xorigin,yorigin,xorigin+ix,yorigin+iy,idv);

/* For ticks out: upper axis: ticks_up = 1;  lower axis: ticks_up = 0 */
  itick = (ticks_up) ? -1 : 1;

/* Draw big ticks */
  while( sign0*x_tick <= sign0*max_user)
  {
#ifdef JLP_FORM
   jlp_format(string,x_tick,TeX_flag);
#else
   sprintf(string,"%.3g",x_tick); 
#endif
  xvec = axis_length * (x_tick - min_user)/range_user;
  ynow = (int)(xvec * sin0);
  xnow = (int)(xvec * cos0);

  if (label_flag == 1) 
    {
    xoff = (int)jlp_label(string,xorigin,yorigin,0.,expand,0,idv);
/* Vertical labels: */
#if 0
    yoff = ticklen + (int)(cheight*expand) + 600;
    if(yoff <= 800) yoff = 800;
/* Assume that labels are to the left for vertical axes and to the
*  bottom for horizontal axis: */
    if(angle < -45.)
      {
      ix = (double)(- xoff/2) * cos0 + (double)yoff * sin0 - 500;
      iy = (double)(- xoff/2) * sin0 - (double)yoff * cos0 - 500;
      }
    else if(angle >= -45 && angle < 45.)
      {
      ix = (double)(- xoff/2) * cos0 + (double)yoff * sin0;
      iy = (double)(- xoff/2) * sin0 - (double)yoff * cos0;
      }
    else
      {
      ix = -(double)(xoff + yoff - 200) * sin0;
      iy = (double)(xoff + yoff - 200) * cos0;
      }
    jlp_label(string,xorigin+xnow+ix,yorigin+ynow+iy,0.,expand,1,idv);

/* Labels parallel to axes: */
#else
    xoff = (int)((double)xoff * expand);
    yoff = ticklen + (int)(cheight*expand) + 600;
/* Assume that labels are to the left for vertical axes and to the
*  bottom for horizontal axis: */
    if(angle < 45.)
      {
      ix = (int)((double)(- xoff/2) * cos0 + (double)yoff * sin0); 
      iy = (int)((double)(- xoff/2) * sin0 - (double)yoff * cos0); 
      }
    else
      {
      ix = (int)((double)(- xoff/2) * cos0 - (double)yoff * sin0); 
      iy = (int)((double)(- xoff/2) * sin0 + (double)yoff * cos0); 
      }
    jlp_label(string,xorigin+xnow+ix,yorigin+ynow+iy,angle,expand,1,idv);
#endif
    }

/* Draw big tick now: */
  ix = (int)((double)(-itick*ticklen) * sin0); 
  iy = (int)((double)(itick*ticklen) * cos0); 
  jlp_line(xorigin+xnow,yorigin+ynow,xorigin+xnow-ix,yorigin+ynow-iy,idv);

  x_tick += xstep;
  }  
} /* End of angle not zero or 90 degrees */

/* For small ticks: */
ticklen = ticklen/2;
sxstep = xstep/(double)nsticks;
ioffset = (int)(min_user/sxstep);
if(min_user/sxstep < 0) ioffset = ioffset -1;
x_tick = sxstep*ioffset;
if(sign0*x_tick < sign0*min_user) x_tick = sxstep*(double)(ioffset+1);

/* Draw small ticks */
  while( sign0*x_tick <= sign0*max_user)
  {
  xnow = (int)(axis_length * (x_tick - min_user)/range_user);

  if (angle == 0.)
    jlp_line(xorigin+xnow,yorigin,xorigin+xnow,yorigin-ticklen,idv);
  else if(angle == 90.)
    jlp_line(xorigin,yorigin+xnow,xorigin-ticklen,yorigin+xnow,idv);
  else
    {
    ynow = (int)((double)xnow * sin0);
    xnow = (int)((double)xnow * cos0);
    ix = -(int)((double)(itick * ticklen) * sin0); 
    iy = (int)((double)(itick * ticklen) * cos0); 
    jlp_line(xorigin+xnow,yorigin+ynow,xorigin+xnow-ix,yorigin+ynow-iy,idv);
    }

  x_tick += sxstep;
  }

return(0);
}
/***************************************************************
* jlp_format
* to plot the labels for the ticks.
* Keep only 3 significant numbers since it assumes that
* the roundoff routine has worked correctly before...
* Ex: 52.50000    5.25 10**1  i=5 j=2 k=5 l=1
*             becomes: 52.5
*
* TeX_flag : if flag set to one put 5.10^{-2} for instance
*            otherwise              5.E-2 
***************************************************************/
int jlp_format(char *string, double value, int TeX_flag)
{
double val0, val1, w1;
int i, j, k, l, sign0;

/* Check the sign of the value: */
val0 = value;
if(value < 0) {sign0 = -1; val0 = -val0;}
else if (value > 0) sign0 = 1;
else {strcpy(string,"0"); return(0);}

/* Power of ten; */
 w1 = log10( (double)val0);
 l = (int)w1;
/* Actually should be the closest minimum (signed) integer number, so: */
 if(w1 < 0) l--;

/* Coefficient: */
 val1 = val0 * pow(10.0,(double)(-l));
 i = (int)val1;
 j = (int)(10.*(val1 - (double)i));
 k =(int)( 10.*(10.*(val1 - (double)i) - (double)j));

#ifdef MAIN_DEB94
 printf(" val0=%f val1=%f \n",val0,val1);
 printf(" i=%d, j=%d, k=%d, l=%d \n",i,j,k,l);
#endif

/* Case E+3 or E+2: do not process it, but troncate after the dot...*/
if(l == 3 || l == 2)
  {
/* Case 0.523 E+3 --->   523. */
  sprintf(string,"%.0f",value);
  }
else if(l == 1)
  {
/* Case 5.0 E+1 --->   5 */
  if(k == 0)sprintf(string,"%.0f",value);
/* Case 5.3 E+1 --->   53 */
     else sprintf(string,"%.1f",value);
  }
else if(l == 0)
  {
  if(k == 0)
    {
/* Case 5.0 E+0 --->   5 */
     if(j == 0)sprintf(string,"%.0f",value);
/* Case 5.3 E+0 --->   5.3 */
          else sprintf(string,"%.1f",value);
    }
/* Case 5.34 E+0 --->   5.34 */
    else sprintf(string,"%.2f",value);
  }
else if(l == -1)
  {
  if(k == 0)
    {
/* Case 5.0 E-1 --->   0.5 */
     if(j == 0)sprintf(string,"%.1f",value);
/* Case 5.3 E-1 --->   0.53 */
          else sprintf(string,"%.2f",value);
    }
/* Case 5.34 E-1 --->   0.534 */
    sprintf(string,"%.3f",value);
  }
/* if(l > 3 || l < -1) */
else
 {
  val1 = val1 * sign0;
  if(TeX_flag)
    {
       if(k == 0) 
       {
/* Case 5.0 E-5 --->   5. 10^{-5} */
        if(j == 0) sprintf(string,"%.0f. 10^{%d}",val1,l);
/* Case 5.2 E-5 --->   5.2 10^{-5} */
        else
         sprintf(string,"%.1f 10^{%d}",val1,l);
       }
/* Case 5.2 E-5 --->   5.2 10^{-5} */
       else 
         sprintf(string,"%.2f 10^{%d}",val1,l);
    }
  else
    {
     if(k == 0) 
       {
          if(j == 0) 
          {
           if(l > 0)
/* Case 5.0 E+5 --->   5. E+5 */
             sprintf(string,"%.0f. E+%d",val1,l);
           else
/* Case 5.0 E-5 --->   5. E-5 */
             sprintf(string,"%.0f. E%d",val1,l);
          }
          else
          {
           if(l > 0)
/* Case 5.2 E+5 --->   5.2 E+5 */
             sprintf(string,"%.1f E+%d",val1,l);
           else
/* Case 5.2 E-5 --->   5.2 E-5 */
             sprintf(string,"%.1f E%d",val1,l);
          }
       }
     else 
       {
       if(l > 0)
/* Case 5.23 E+5 --->   5.23 E+5 */
         sprintf(string,"%.2f E+%d",val1,l);
       else
/* Case 5.23 E-5 --->   5.23 E-5 */
         sprintf(string,"%.2f E%d",val1,l);
       }
/* End of TeX_flag... */
    }
/* End of cases l=... */
 } 

/* End of routine */
return(0);
}
