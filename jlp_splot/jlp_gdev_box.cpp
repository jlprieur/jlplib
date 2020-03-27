/***************************************************************
* NAME: jlp_gdev_box.cpp JLP_GDev class
* with elaborated frames (grids, log, etc)
*
* Static functions:
*  jlp_grid21(int xorigin, int yorigin, int axis_length, ...)
*
* VERSION: 09/07/2012 
*
* AUTHOR: JLP 
*
***************************************************************/
#include <stdio.h>
#include <string.h>
#include <ctype.h>       /* isgraph, isprint, etc... */
#include <malloc.h>
#include <math.h>
#include "jlp_gdev.h"
#include "jlp_macros.h"  // MAXI, etc
#include "jlp_time0.h"   // JLP_CTIME

// Contained here :
int jlp_box(double box_xmin0, double box_xmax0, double box_ymin0,    
            double box_ymax0, char *xlabel, char *ylabel, char *title,    
            int ticks_in, int box_numbers, int full_caption,
            int jlp_axes_are_wanted, int xgrid_is_wanted, int ygrid_is_wanted,
            int x_is_log10, int y_is_log10, double expand, char *axis_color);
int jlp_box_for_image(char *xlabel, char *ylabel, char *title, int ticks_in,
                      int box_numbers, int full_caption, double expand, 
                      char *axis_color);
int jlp_box_type0(char *xlabel, char *ylabel, char *title, int ticks_in,
             int box_numbers, int full_caption, int xgrid_is_wanted, 
             int ygrid_is_wanted, double box_xmin0, double box_xmax0,    
             double box_ymin0, double box_ymax0, int ix0, int iy0,    
             int ixlen, int iylen, double expand, char *axis_color);
int jlp_axis(int xorigin, int yorigin, int axis_length,
             double box_min, double box_max, double angle, int label_flag,
             int ticks_up, int TeX_flag, double expand);
int jlp_grid21(int xorigin, int yorigin, int axis_length,
               int other_axis_length, double box_min, double box_max, 
               double axis_angle, double other_axis_angle, char *nchar,
               char *axis_color);
static int steps_for_linear_axes(double box_min, double box_max, 
                                 double *small_step, double *big_step,
                                 double *start_small, double *start_big);
/*****************************************************************************
*
* FUNCTION: jlp_box
*
* PURPOSE: Draw a box for coordinate information 
* (using static variables 
*  box_xmin, box_xmax, box_ymin and box_ymax for limits of axes)
*
* INPUT:
* ticks_in: flag set to 1 if ticks inside the frame
* box_numbers: flag set to 1 if axes are labeled 
* x_is_log10 : 0 if linear, 1 if log10 axis
* y_is_log10 : 0 if linear, 1 if log10 axis
* jlp_axes_are_wanted: flag set to one if JLP_axes are wanted
* xgrid_is_wanted: flag set to one if X grid is wanted
* ygrid_is_wanted: flag set to one if Y grid is wanted
* expand: size of characters (1.2 for instance) for labels
******************************************************************************/
int JLP_GDev::jlp_box(double box_xmin0, double box_xmax0, double box_ymin0, 
            double box_ymax0, char *xlabel, char *ylabel, char *title, 
            int ticks_in, int box_numbers, int full_caption, 
            int jlp_axes_are_wanted, int xgrid_is_wanted, int ygrid_is_wanted, 
            int x_is_log10, int y_is_log10, double expand, char *axis_color) 
/*****************************************************************************/
{
double xmin_data, xmax_data, ymin_data, ymax_data;
int ix0, iy0, ixlen, iylen;
int istat;

  ix0 = Jgc0_offx();
  iy0 = Jgc0_offy();
  ixlen = Jgc0_axlen();
  iylen = Jgc0_aylen(); 

/* New version from sciplot (in "jlp_gdev_sciplot.cpp") */
if(jlp_axes_are_wanted ||
  (xgrid_is_wanted || ygrid_is_wanted || x_is_log10 || y_is_log10)) {
  xmin_data = box_xmin0;
  xmax_data = box_xmax0;
  ymin_data = box_ymin0;
  ymax_data = box_ymax0;
// WARNING: the resulting box may be changed by this routine...
  istat = jlp_box_sciplot(xlabel, ylabel, title, ticks_in, box_numbers, 
                          full_caption, xgrid_is_wanted, ygrid_is_wanted, 
                          x_is_log10, y_is_log10, xmin_data, xmax_data, 
                          ymin_data, ymax_data, ix0, iy0, ixlen, iylen, 
                          axis_color);
/* Old version (without logarithmic axes): better very often...
* The box limits remain unchanged by this routine
*/
} else {
  istat = jlp_box_type0(xlabel, ylabel, title, ticks_in, box_numbers, 
                        full_caption, xgrid_is_wanted, ygrid_is_wanted, 
                        box_xmin0, box_xmax0, box_ymin0, box_ymax0, ix0, iy0, 
                        ixlen, iylen, expand, axis_color);
}

return(istat);
}
/*****************************************************************************
*
* FUNCTION: jlp_box_for_image
* Called by Xdisp1
*
* PURPOSE: Draw a box for coordinate information 
* (using static variables 
*  box_xmin, box_xmax, box_ymin and box_ymax for limits of axes)
*  expand: character size for labels
*
* INPUT:
* ticks_in: flag set to 1 if ticks inside the frame
* box_numbers: flag set to 1 if axes are labeled 
* x_is_log10 : 0 if linear, 1 if log10 axis
* y_is_log10 : 0 if linear, 1 if log10 axis
******************************************************************************/
int JLP_GDev::jlp_box_for_image(char *xlabel, char *ylabel, char *title, 
                                int ticks_in, int box_numbers, 
                                int full_caption, double expand, 
                                char *axis_color)
/*****************************************************************************/
{
double xmin_data, xmax_data, ymin_data, ymax_data;
int ix0, iy0, ixlen, iylen; 
int jlp_axes_are_wanted, xgrid_is_wanted, ygrid_is_wanted;
int istat;

xmin_data = Jgc0_xmin_user();
xmax_data = Jgc0_xmax_user();
ymin_data = Jgc0_ymin_user();
ymax_data = Jgc0_ymax_user();
xgrid_is_wanted = 0;
ygrid_is_wanted = 0;
jlp_axes_are_wanted = 0;

ix0 = Jgc0_offx();
iy0 = Jgc0_offy();
ixlen = Jgc0_axlen();
iylen = Jgc0_aylen(); 

/* Old version (without logarithmic axes):
 but does not change the coordinates, which is compulsory for X11!
*/
istat = jlp_box_type0(xlabel, ylabel, title, ticks_in, box_numbers, 
                      full_caption, xgrid_is_wanted, ygrid_is_wanted, 
                      xmin_data, xmax_data, ymin_data, ymax_data, ix0, iy0, 
                      ixlen, iylen, expand, axis_color);

return(istat);
}
/*****************************************************************************
* FUNCTION: jlp_box_type0
* Called by jlp_box (for main frame) or directly by Xdisp1, LUT key, etc 
*
* PURPOSE: Draw a box for coordinate information 
* (using box_xmin, box_xmax, box_ymin and box_ymax for limits of axes)
* Compared to other box routines, take the input values
* as the limits without any change.
* It is needed when images are displayed (e.g., for SCIDAR wind parameters)
*
* INPUT:
* ticks_in: flag set to 1 if ticks inside the frame
* box_numbers: flag set to 1 if axes are labeled 
* full_caption: flag set to 1 if title is displayed
* jlp_axes_are_wanted: flag set to one if JLP_axes are wanted
* xgrid_is_wanted: flag set to one if X grid is wanted
* ygrid_is_wanted: flag set to one if Y grid is wanted
* ix0, iy0: lower-left position of the box (mgo coordinates)
* ixlen, iylen: x and y size of the box (mgo coordinates)
* expand: size of characters (1.2 for instance) for labels
******************************************************************************/
int JLP_GDev::jlp_box_type0(char *xlabel, char *ylabel, char *title, 
                            int ticks_in, int box_numbers, int full_caption, 
                            int xgrid_is_wanted, int ygrid_is_wanted, 
                            double box_xmin0, double box_xmax0, 
                            double box_ymin0, double box_ymax0, int ix0, 
                            int iy0, int ixlen, int iylen, double expand, 
                            char *axis_color)
/*****************************************************************************/
{
int label_flag, ticks_up, TeX_flag, smallest_ix, smallest_iy, sm_ix, sm_iy;
char line_type[20], color[40]; 
double expand1;

/* Update box_xmin, box_xmax ... */
SetBoxLimits(box_xmin0, box_xmax0, box_ymin0, box_ymax0, 0., 1.);

#ifdef DEBUG
printf("Calling jlp_box_type0 %f %f %f %f xgrid=%d, ygrid=%d\n", 
       box_xmin0, box_xmax0, box_ymin0, box_ymax0, xgrid_is_wanted, 
       ygrid_is_wanted);
#endif

/* Fonts used for numbers will be smaller than for those used for X and Y captions
*/
expand1 = 0.8 * expand;

/* If postscript file */
TeX_flag = (Jgc0_TeX_flag() == 1) ? 1 : 0;

/* label_flag = [0 or 1] draw labels if flag set to 1 */
label_flag = box_numbers;

/* Lower X axis with labels if label_flag is equal to one: */
   ticks_up = ticks_in;
   jlp_axis(ix0, iy0, ixlen, box_xmin0, box_xmax0, 0., label_flag, ticks_up,
            TeX_flag, expand1, &sm_ix, &sm_iy);
   smallest_iy = sm_iy;

/* Upper X axis: */
   ticks_up = 1 - ticks_in;
   jlp_axis(ix0, iy0 + iylen, ixlen, box_xmin0, box_xmax0, 0., 0, ticks_up,
            TeX_flag, expand1, &sm_ix, &sm_iy);

/* Left Y axis with labels if label_flag is equal to one: */
   ticks_up = 1 - ticks_in;
   jlp_axis(ix0, iy0, iylen, box_ymin0, box_ymax0, 90., label_flag, 
            ticks_up, TeX_flag, expand1, &sm_ix, &sm_iy);
   smallest_ix = sm_ix;

/* Right Y axis: */
   ticks_up = ticks_in;
   jlp_axis(ix0 + ixlen, iy0, iylen, box_ymin0, box_ymax0, 90., 0, ticks_up,
            TeX_flag, expand1, &sm_ix, &sm_iy);

/* Draw X and Y labels and title: */
  jlp_DrawBoxLabels(xlabel, ylabel, title, full_caption, ix0, iy0, ixlen,
                 iylen, expand, smallest_ix, smallest_iy);

/* Draw X grid (parallel to Y axis): */
   if(xgrid_is_wanted) {
     strcpy(line_type, "L1");
     strcpy(color, "Yellow");
     jlp_grid21(ix0, iy0, ixlen, iylen, box_xmin0, box_xmax0, 0., 90., 
                line_type, color);
     }

/* Draw Y grid (parallel to X axis): */
   if(ygrid_is_wanted) {
     strcpy(line_type, "L2");
     strcpy(color, "Orange");
     jlp_grid21(ix0, iy0, iylen, ixlen, box_ymin0, box_ymax0, 90., 0., 
                line_type, color);
     }

return(0);
}
/***************************************************************************
*
* FUNCTION: jlp_axis
*
* PURPOSE:
*
* INPUT:  
*    xorigin : x coordinate of the origin in mgo pixels
*    yorigin : y coordinate of the origin in mgo pixels
*    axis_length : length of the axis in mgo pixels
*    box_min : lower user box value in X
*    box_max : upper user box value in X
*    expand : size of X and Y captions (1.2, or 1.8 for instance)
*    angle : [0. or 90] angle with horizontal reference
*    label_flag : [0 or 1] draw labels if flag set to 1
* For ticks out: upper axis: ticks_up = 1;  lower axis: ticks_up = 0
* For ticks out: right axis: ticks_up = 0;  left axis: ticks_up = 1
*
* OUTPUT:
*    smallest_ix : smallest MGO x coordinate used in this routine
*    smallest_iy : smallest MGO y coordinate used in this routine
*
****************************************************************************/
int JLP_GDev::jlp_axis(int xorigin, int yorigin, int axis_length,
                       double box_min, double box_max, double angle, 
                       int label_flag, int ticks_up, int TeX_flag, 
                       double expand, int *smallest_ix, int *smallest_iy)
{
int  xnow, ynow, xend, yend, ix, iy, iw;
int  xoff, yoff;              /* size of the labels in x and y */
int  ticklen, big_ticklen;    /* tick length */
int  clength, sign_ticks_up, pst_device;
double x_tick, y_tick, big_step, small_step, start_big, start_small;
double cos0, sin0, ww, xvec, box_range, cheight;
char  string[15];

// Default values:
*smallest_ix = xorigin; 
*smallest_iy = yorigin;; 

/* Device type:
  1 = X11
  2 = Postscript file
  3 = wxWidgets
No longer used:
  4 = HPGL
  5 = Tektronix
*/
pst_device = 0;
if(Jgc0_dev_type() == 2) pst_device = 1;

// Set Jgc0 font size from device:
 Jgc0_set_font_size_from_device(expand);
 cheight = Jgc0_cheight();

/* tick length (small ticks) */
ticklen=600; 
big_ticklen=800; 

 cos0 = cos((double)(angle*PI/180.));
 sin0 = sin((double)(angle*PI/180.));
 xend = xorigin + (int)((double)axis_length * cos0);
 yend = yorigin + (int)((double)axis_length * sin0);

/* First draw axis without ticks */
  gdev_line(xorigin,yorigin,xend,yend);

/* Swap end and origin if box_max < box_min: 
*/
if(box_max < box_min) {
  angle += 180.;
  ww = box_min; box_min = box_max; box_max = ww;
  iw = xorigin; xorigin = xend; xend = iw;
  iw = yorigin; yorigin = yend; yend = iw;
  ticks_up = 1 - ticks_up;
  cos0 = cos((double)(angle * PI / 180.));
  sin0 = sin((double)(angle * PI / 180.));
  }

/* Compute the steps for linear scale: */
steps_for_linear_axes(box_min, box_max, &small_step, &big_step,
                      &start_small, &start_big);

box_range = box_max - box_min;
sign_ticks_up = (ticks_up == 1) ? 1 : -1; 
/********************* X axis ***********************************/
if (angle == 0.)
{
/* Draw big ticks and labels */
  for(x_tick = start_big; x_tick <= box_max; x_tick += big_step) {

/* Draw big ticks */
    xnow = (int)(axis_length * (x_tick - box_min)/box_range);

/* For ticks out: upper axis: ticks_up = 1;  lower axis: ticks_up = 0 */
    gdev_line(xorigin+xnow, yorigin, xorigin+xnow,
             yorigin+sign_ticks_up*big_ticklen);

/* Draw labels */
  if (label_flag == 1) {

#ifdef JLP_FORM
    jlp_format(string,x_tick,TeX_flag);
#else
    sprintf(string,"%.3g",x_tick); 
#endif
    xoff = (int)gdev_label(string, xorigin, yorigin, 0., expand, 0);
    xoff = (int)((double)xoff * 0.5);
// JLP2017: checked that it is OK: 
    if(ticks_up)
      yoff = (int)(cheight + 100.);
    else
      yoff = (int)(big_ticklen + cheight + 100.);
// JLP2018:
      if(pst_device) {
        if(TeX_flag == 1) {
         yoff += 400;
        } else {
         yoff += 1000;
        }
      }
      gdev_label(string, xorigin+xnow-xoff, yorigin-yoff, 0., expand, 1);
      if(yorigin - yoff < *smallest_iy) *smallest_iy = yorigin - yoff; 
    }
  }  
} /* End of X axis case */

/********************* Y axis (i.e., vertical case) **************************/
else if(angle == 90.)
 {

/* Draw big ticks */
/* For ticks out: right axis: ticks_up = 0;  left axis: ticks_up = 1 */
  for(y_tick = start_big; y_tick <= box_max; y_tick += big_step) {
     ynow = (int)(axis_length * (y_tick - box_min)/box_range);
// Draw big ticks:
     gdev_line(xorigin,yorigin+ynow,
              xorigin-sign_ticks_up*big_ticklen,yorigin+ynow);

/* Draw labels if needed: */
   if (label_flag == 1) {
#ifdef JLP_FORM
     jlp_format(string,y_tick,TeX_flag);
#else
     sprintf(string,"%.3g",y_tick); 
#endif
     clength = (int)gdev_label(string,xorigin,yorigin,0.,expand,0);
// JLP 2018: error in clength, too large
     if(pst_device) clength *= 0.8; 

/* JLP2017: checked that it is OK: with  
for jlp_gdev_pst.cpp
expanded_font = 2. * FONT_SIZE_FOR_12PT * expand1;
Jgc0.cwidth = 0.8 * expanded_font / g_dx;
Jgc0.cheight = 0.1 * expanded_font / g_dy;
with FONT_SIZE_FOR_12PT = 12...
and 
for jlp_gdev_wxwid_plot.cpp
expanded_font = FONT_SIZE_FOR_12PT * expand0;
Jgc0.cwidth = 0.8 * expanded_font / g_dx;
Jgc0.cheight = 1.1 * expanded_font / g_dy;
with FONT_SIZE_FOR_12PT = 12...
*
* Remember: big_ticklen=800 
*/

     if(ticks_up)
       xoff = 500 + big_ticklen + clength;
     else
       xoff = 400 + clength;
     yoff = -300 + cheight;
     if(pst_device) {
       xoff += 50;
       yoff -= 100;
// JLP2019: (OK with expand=1.8)
        if(Jgc0.TeX_flag == 1) {
        xoff += 200;
        }
      }
     gdev_label(string,xorigin-xoff,yorigin+ynow+yoff,0.,expand,1);
     if(xorigin - xoff < *smallest_ix) *smallest_ix = xorigin - xoff; 
    }

  }  
} /* End of Y axis case */
/* Case of any other value of the angle (different from 0. and 90 degrees): */
else {
/* Draw big ticks and labels */
  for(x_tick = start_big; x_tick <= box_max; x_tick += big_step) {

/* For ticks out: upper axis: ticks_up = 1;  lower axis: ticks_up = 0 */
/* Draw big ticks: */
    ix = (int)((double)(-sign_ticks_up*big_ticklen) * sin0); 
    iy = (int)((double)(sign_ticks_up*big_ticklen) * cos0); 
    gdev_line(xorigin+xnow,yorigin+ynow,xorigin+xnow-ix,yorigin+ynow-iy);
 
/* Draw labels if needed: */
  if (label_flag == 1) {
#ifdef JLP_FORM
     jlp_format(string,x_tick,TeX_flag);
#else
     sprintf(string,"%.3g",x_tick); 
#endif
     xvec = axis_length * (x_tick - box_min)/box_range;
     xnow = (int)(xvec * cos0);
     ynow = (int)(xvec * sin0);
     xoff = (int)gdev_label(string,xorigin,yorigin,0.,expand,0);
/* Two possible modes: vertical labels or labels parallel to axes
* 
* Vertical labels: */
#if 1
    yoff = big_ticklen + (int)(cheight * expand) + 600;
    if(yoff <= 800) yoff = 800;
/* Assume that labels are on the left for vertical axes and on the
*  bottom for horizontal axis: */
    if(angle == 270.)
      {
      ix = - big_ticklen - 300 - xoff;
      iy = - (int)(cheight / 2.);
      }
/* Following not tested yet: */
    else if(-90. <= angle && angle < -45.)
      {
      ix = (int)((double)(- xoff/2) * cos0 + (double)yoff * sin0 - 500);
      iy = (int)((double)(- xoff/2) * sin0 - (double)yoff * cos0 - 500);
      }
    else if(angle >= -45 && angle < 45.)
      {
      ix = (int)((double)(- xoff/2) * cos0 + (double)yoff * sin0);
      iy = (int)((double)(- xoff/2) * sin0 - (double)yoff * cos0);
      }
    else
      {
      ix = -(int)((double)(xoff + yoff - 200) * sin0);
      iy = (int)((double)(xoff + yoff - 200) * cos0);
      }
    gdev_label(string,xorigin+xnow+ix,yorigin+ynow+iy,0.,expand,1);

/* Labels parallel to axes: */
#else
    xoff = (int)((double)xoff * expand);
    yoff = big_ticklen + (int)(cheight * expand) + 600;
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
    gdev_label(string,xorigin+xnow+ix,yorigin+ynow+iy,angle,expand,1);
#endif
    }


  } /* EOF loop on x_tick for big ticks */  
} /* End of angle not zero or 90 degrees */

/* Draw small ticks */
for(x_tick = start_small; x_tick <= box_max; x_tick += small_step) {
  xvec = (int)(axis_length * (x_tick - box_min)/box_range);
  xnow = (int)((double)xvec * cos0);
  ynow = (int)((double)xvec * sin0);
  if (angle == 0.) {
    gdev_line(xorigin+xnow,yorigin,xorigin+xnow,
             yorigin+sign_ticks_up*ticklen);
/* For ticks out: right axis: ticks_up = 0;  left axis: ticks_up = 1 */
  } else if(angle == 90.) {
    gdev_line(xorigin,yorigin+ynow,xorigin-sign_ticks_up*ticklen,
             yorigin+ynow);
  } else {
    ix = -(int)((double)(sign_ticks_up * ticklen) * sin0); 
    iy = (int)((double)(sign_ticks_up * ticklen) * cos0); 
    gdev_line(xorigin+xnow,yorigin+ynow,xorigin+xnow-ix,yorigin+ynow-iy);
    }

  }

return(0);
}
/***************************************************************************
*
* FUNCTION: jlp_grid21
* To draw a grid on the frame, for curves.
* (X grid is parallel to Y axis)
*
* PURPOSE:
*
* INPUT:  
*    xorigin = x coordinate of the origin in mgo pixels
*    yorigin = y coordinate of the origin in mgo pixels
*    axis_length = length of the axis in mgo pixels
*    other_axis_length = length of the other axis in mgo pixels
*    box_min = lower user value in X
*    box_max = upper user value in X
*    axis_angle = [0. or 90] angle of the axis with the horizontal reference
*    other_axis_angle = [0. or 90] angle of the other axis with 
*                        the horizontal reference
*    nchar[]: type of symbols used for plotting the grid (L0, L1, etc) 
*    axis_color[]: color to be used for drawing the grid 
*
****************************************************************************/
int JLP_GDev::jlp_grid21(int xorigin, int yorigin, int axis_length,
                         int other_axis_length, double box_min, double box_max, 
                         double axis_angle, double other_axis_angle, 
                         char *nchar, char *axis_color)
{
int  xnow, ynow, ix, iy, ltype, lweight, xend, yend, iw;
int line_options;
double cos0, sin0, cos1, sin1, ww;
double x_tick, big_step, small_step, start_big, start_small;
double   box_range, xvec;
char default_color[40];

cos0 = cos((double)(axis_angle*PI/180.));
sin0 = sin((double)(axis_angle*PI/180.));
xend = xorigin + (int)((double)axis_length * cos0);
yend = yorigin + (int)((double)axis_length * sin0);

/* Swap end and origin if box_max < box_min:
*/
if(box_max < box_min) {
  axis_angle += 180.;
  ww = box_min; box_min = box_max; box_max = ww;
  iw = xorigin; xorigin = xend; xend = iw;
  iw = yorigin; yorigin = yend; yend = iw;
  cos0 = cos((double)(axis_angle*PI/180.));
  sin0 = sin((double)(axis_angle*PI/180.));
  }

box_range = box_max - box_min;

/* Decode line style: */
if(nchar[0] == 'L' || nchar[0] == 'l') {
/* ltype = 0 for solid,  = 1 for dashed curves: */
   ltype = atoi(nchar+1);
   lweight = 0;
  } else {
   ltype = 0;
   lweight = 0;
  } 
line_options = (ltype != 0) ? 1 : 0;

/********* Start drawing *****************************/
SetPColor(axis_color);
if(line_options) SetLineWidthAndType(ltype, lweight);

 cos1 = cos((double)(other_axis_angle*PI/180.));
 sin1 = sin((double)(other_axis_angle*PI/180.));
 ix = (int)((double)other_axis_length * cos1); 
 iy = (int)((double)other_axis_length * sin1); 

/* Compute the steps for linear scale: */
steps_for_linear_axes(box_min, box_max, &small_step, &big_step,
                      &start_small, &start_big);

/* Draw grid at the location of the big ticks (except at the edges) */
if(box_min == start_big) start_big += big_step;
for(x_tick = start_big; x_tick < box_max; x_tick += big_step) {
  xvec = axis_length * (x_tick - box_min)/box_range;
  xnow = (int)(xvec * cos0);
  ynow = (int)(xvec * sin0);
  gdev_line(xorigin+xnow,yorigin+ynow,xorigin+xnow+ix,yorigin+ynow+iy);
  x_tick += big_step;
  }  

/* Draw grid at the location of the small ticks (except at the edges) */
if(box_min == start_small) start_small += small_step;
for(x_tick = start_small; x_tick < box_max; x_tick += small_step) {
    xvec = (int)(axis_length * (x_tick - box_min)/box_range);
    xnow = (int)((double)xvec * cos0);
    ynow = (int)((double)xvec * sin0);
    gdev_line(xorigin+xnow,yorigin+ynow,xorigin+xnow+ix,yorigin+ynow+iy);
  }

// Back to default options:
 strcpy(default_color, "Default");
 SetPColor(default_color);
 if(line_options) {
    ltype = 0;
    lweight = 0;
    SetLineWidthAndType(ltype, lweight);
   }
return(0);
}
/***************************************************************************
*
* INPUT:
* box_min, box_max
*
* OUTPUT:
* small_step, big_step
***************************************************************************/
static int steps_for_linear_axes(double box_min, double box_max, 
                                 double *small_step, double *big_step,
                                 double *start_small, double *start_big)
{
int nlabels, nsticks, ioffset, iexpo, istep;
double box_range, log_step, mantissa;

/* nlabels: minimum number of labels */
nlabels = 3;

/* Get the power of ten for the step (assuming nlabels to start): */
box_range = box_max - box_min;
log_step = log10( (double)(box_range)/(double)nlabels);
iexpo = (int)log_step;
if(log_step < 0) iexpo = iexpo - 1;

/* Computes istep according to the value of the mantissa of the step */
/* big_step: step defining the intervals between two big ticks
*  nsticks: number of small ticks in each interval bounded with two big ticks
*/
mantissa = log_step - iexpo;
if( mantissa < .15) {istep = 1; nsticks = 5;}
else if( mantissa < .5) {istep = 2; nsticks = 4;}
else if( mantissa < .85) {istep = 5; nsticks = 5;}
else {istep = 10; nsticks = 5;}

/* Value of the step for labeled ticks (big ones)*/
*big_step = (double)istep * pow( 10.0, (double)iexpo);

/* Offset, since the first tick is not always in the first pixel: */
ioffset = (int)(box_min/(*big_step));
*start_big = *big_step * (double)ioffset;
if(*start_big < box_min) *start_big += *big_step;

/* Step for small ticks: */
*small_step = *big_step/(double)nsticks;
ioffset = (int)(box_min/(*small_step));
*start_small = *small_step * ioffset;
if(*start_small < box_min) *start_small += *small_step;

return(0);
}
/*******************************************************************
* Draw X label, Y label and Title
*
* INPUT:
*    ixlen : length of the X axis in mgo pixels
*    iylen : length of the Y axis in mgo pixels
*    expand : size of X and Y captions (1.2, or 1.8 for instance)
*    smallest_ix : smallest MGO x coordinate used when displaying the Y label 
*    smallest_iy : smallest MGO y coordinate used  when displaying the X label
*
*******************************************************************/
int JLP_GDev::jlp_DrawBoxLabels(char *xlabel, char *ylabel, char *title, 
                                int full_caption, int ix0, int iy0, int ixlen, 
                                int iylen, double expand, int smallest_ix, 
                                int smallest_iy)
{
double clength, cheight;
int TeX_flag, ioffset, joffset; 
int pst_device;

/* Device type:
  1 = X11
  2 = Postscript file
  3 = wxWidgets
No longer used:
  4 = HPGL
  5 = Tektronix
*/
pst_device = 0;
if(Jgc0_dev_type() == 2) pst_device = 1;

/* Check if curve or image device */
/* Set Tex flag if postscript file */
 TeX_flag = (Jgc0_TeX_flag() == 1) ? 1 : 0;

// Set Jgc0 font size from device:
 Jgc0_set_font_size_from_device(expand);
 cheight = Jgc0_cheight();

/****************** X caption *************************************/
/* double gdev_label(char *s, int ixstart, int iystart, double angle1,
                double expand1, int drawit);
*/
   clength = gdev_label(xlabel, ix0, iy0, 0., expand,0);
// JLP 2018: error in clength, too large
   if(pst_device) clength *= 0.8; 
   ioffset = MAXI(0, (int)(ixlen/2. - clength * 0.3));
    if(TeX_flag == 1) {
      ioffset += 1400;
      }
   if(pst_device) {
     gdev_label(xlabel, ix0+ioffset-200, smallest_iy-(int)(2500.*expand/1.8), 
                0., expand, 1);
     } else {
     gdev_label(xlabel, ix0+ioffset, smallest_iy-(int)(2500.*expand/1.8), 
                0., expand, 1);
     }
/****************** Y caption *************************************/
   clength = gdev_label(ylabel, ix0, iy0, 90., expand, 0);
// JLP 2018: error in clength, too large
   if(pst_device) clength *= 0.8; 
   joffset = MAXI(0, (int)(iylen/2. - clength * 0.3));
// JLP2018: error in length0, corrected here with this trick:
   joffset = MAXI(joffset, iy0);
/* Twice cheight, since labels for ticks and space for title: */
   if(pst_device &&( TeX_flag == 1)) {
// JLP2019 (OK with expand=1.8)
       joffset += 400;
      }
   if(pst_device) {
     gdev_label(ylabel, smallest_ix-(int)(1500.*expand/1.8), 
                iy0+joffset, 90., expand, 1);
     } else {
     gdev_label(ylabel, smallest_ix-(int)(1500.*expand/1.8), 
                iy0+joffset, 90., expand, 1);
     }

/****************** Title *************************************/
   if(full_caption) {
     clength = gdev_label(title, ix0, iy0, 0., expand, 0);
// JLP 2018: error in clength, too large
     if(pst_device) clength *= 0.8; 
     ioffset = MAXI(0, (int)(ixlen - clength)/2);
/* JLP95: pb with big images with Xdisp1: caption outside the window... */
     joffset = (int)(iy0*0.2);
     gdev_label(title, ix0+ioffset, iy0+iylen+joffset, 0., expand, 1);
     }

return(0);
}
