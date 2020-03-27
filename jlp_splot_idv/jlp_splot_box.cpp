/***************************************************************
*
* NAME: jlp_splot_box
*  Now with elaborated frames (grids, log, etc)
*
* Contains:
* int jlp_splot_box(float xmin_data, float xmax_data, float ymin_data, 
*             float ymax_data,
*             char *xlabel, char *ylabel, char *title, int ticks_in,
*             int box_numbers, int full_caption, int jlp_axes_are_wanted,
*             int xgrid_is_wanted, int ygrid_is_wanted, int x_is_log10,
*             int y_is_log10, float expand, char *axis_color, int idv)
*  jlp_splot_box_type0(char *xlabel, char *ylabel, char *title, int ticks_in,...)
*  jlp_splot_axis(xorigin,yorigin,axis_length,box_min,box_max,angle,label_flag,
*           ticks_up,TeX_flag,expand1,idv1)
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
#include "jlp_splot_idv.h"
#include "jlp_gdev_idv.h"
#include "jlp_time0.h"   // JLP_CTIME

// Contained here :
int jlp_splot_box(float box_xmin0, float box_xmax0, float box_ymin0,    
                  float box_ymax0, char *xlabel, char *ylabel, char *title,    
                  int ticks_in, int box_numbers, int full_caption,
                  int jlp_axes_are_wanted, int xgrid_is_wanted, 
                  int ygrid_is_wanted, int x_is_log10, int y_is_log10, 
                  float expand, char *axis_color, int idv);
int jlp_splot_box_for_image(char *xlabel, char *ylabel, char *title, 
                            int ticks_in, int box_numbers, int full_caption, 
                            float expand, char *axis_color, int idv);
int jlp_splot_box_type0(char *xlabel, char *ylabel, char *title, int ticks_in,
                        int box_numbers, int full_caption, int xgrid_is_wanted, 
                        int ygrid_is_wanted, float box_xmin0, float box_xmax0,  
                        float box_ymin0, float box_ymax0, int ix0, int iy0,    
                        int ixlen, int iylen, float expand, char *axis_color, 
                        int idv);
int jlp_splot_axis(int xorigin, int yorigin, int axis_length,
                   float box_min, float box_max, float angle, int label_flag,
                   int ticks_up, int TeX_flag, float expand, int idv);

/*****************************************************************************
*
* FUNCTION: jlp_splot_box
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
int jlp_splot_box(float box_xmin0, float box_xmax0, float box_ymin0, 
                  float box_ymax0, char *xlabel, char *ylabel, char *title, 
                  int ticks_in, int box_numbers, int full_caption, 
                  int jlp_axes_are_wanted, int xgrid_is_wanted, 
                  int ygrid_is_wanted, int x_is_log10, int y_is_log10, 
                  float expand, char *axis_color, int idv1)
{
int status = -1;
 
 if(GDev_from_idv(idv1)) {
   status = GDev_from_idv(idv1)->jlp_box(box_xmin0, box_xmax0, box_ymin0, 
                                         box_ymax0, xlabel, ylabel, title,
                                         ticks_in, box_numbers, full_caption,  
                                         jlp_axes_are_wanted, xgrid_is_wanted,
                                         ygrid_is_wanted, x_is_log10, 
                                         y_is_log10, expand, axis_color);
   }

return(status);
}
/*****************************************************************************
*
* FUNCTION: jlp_splot_box_for_image
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
int jlp_splot_box_for_image(char *xlabel, char *ylabel, char *title, 
                            int ticks_in, int box_numbers, int full_caption, 
                            float expand, char *axis_color, int idv1)
{
int status = -1;
 
 if(GDev_from_idv(idv1)) {
   status = GDev_from_idv(idv1)->jlp_box_for_image(xlabel, ylabel, title, 
                                                   ticks_in, box_numbers, 
                                                   full_caption, expand,
                                                   axis_color);
   }

return(status);
}
/*****************************************************************************
* FUNCTION: jlp_splot_box_type0
* Called by jlp_splot_box (for main frame) or directly by Xdisp1, LUT key, etc 
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
int jlp_splot_box_type0(char *xlabel, char *ylabel, char *title, int ticks_in,
             int box_numbers, int full_caption, int xgrid_is_wanted, 
             int ygrid_is_wanted, float box_xmin0, float box_xmax0, 
             float box_ymin0, float box_ymax0, int ix0, int iy0, 
             int ixlen, int iylen, float expand, char *axis_color, int idv1)
{
int status = -1;
 
 if(GDev_from_idv(idv1)) {
   status = GDev_from_idv(idv1)->jlp_box_type0(xlabel, ylabel, title, ticks_in,
                                     box_numbers, full_caption, xgrid_is_wanted,
                                     ygrid_is_wanted, box_xmin0, box_xmax0,
                                     box_ymin0, box_ymax0, ix0, iy0, ixlen,
                                     iylen, expand, axis_color);
   }

return(status);
}
/***************************************************************************
*
* FUNCTION: jlp_splot_axis
*
* PURPOSE:
*
* INPUT:  
*    xorigin = x coordinate of the origin in mgo pixels
*    yorigin = y coordinate of the origin in mgo pixels
*    axis_length = length of the axis in mgo pixels
*    box_min = lower user box value in X
*    box_max = upper user box value in X
*    expand (= 1.2 for instance): size of X and Y captions 
*    angle = [0. or 90] angle with horizontal reference
*    label_flag = [0 or 1] draw labels if flag set to 1
* For ticks out: upper axis: ticks_up = 1;  lower axis: ticks_up = 0
* For ticks out: right axis: ticks_up = 0;  left axis: ticks_up = 1
*
****************************************************************************/
int jlp_splot_axis(int xorigin, int yorigin, int axis_length,
             float box_min, float box_max, float angle, int label_flag,
             int ticks_up, int TeX_flag, float expand, int *smallest_ix, 
             int *smallest_iy, int idv1)
{
int status = -1;

 if(GDev_from_idv(idv1)) {
   status = GDev_from_idv(idv1)->jlp_axis(xorigin, yorigin, axis_length,
                                          box_min, box_max, angle, label_flag,
                                          ticks_up, TeX_flag, expand, 
                                          smallest_ix, smallest_iy);
   }

return(status);
}
/*******************************************************************
* Draw X label, Y label and Title
*******************************************************************/
int jlp_splot_DrawBoxLabels(char *xlabel, char *ylabel, char *title, 
                   int full_caption, int ix0, int iy0, int ixlen, 
                   int iylen, float expand, int smallest_ix,
                   int smallest_iy, int idv1)
{
int status = -1;

 if(GDev_from_idv(idv1)) {
   status = GDev_from_idv(idv1)->jlp_DrawBoxLabels(xlabel, ylabel, title,
                                                   full_caption, ix0, iy0, 
                                                   ixlen, iylen, expand,
                                                   smallest_ix, smallest_iy);
   }

return(status);
}
