/******************************************************************
* "jlp_plot.cpp" 
* Basic functions of "splot" software
* New version with C++ Classes
*
* JLP
* Version 15/05/2017
*************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>  
#include <math.h>

#include "jlp_splot_idv.h"  
#include "jlp_gdev.h"    // JLP Graphic Device class
#include "jlp_gdev_pst.h"          // JLP PST Curve Graphic class
#include "jlp_splot_idv.h"

#if JLP_USE_X11          /* New flag to disable X11 if necessary */
#include "jlp_gdev_x11.h"       // JLP X11 graphic class (in jlp_x11plot/)
#endif
// No longer used (October 2014 ...)
#if JLP_USE_WXWID          /* New flag to disable wxWidgets if necessary */
//#include "jlp_cgdev_wxwid.h"     // JLP wxWidgets graphic class (in jlp_wxplot/)
#endif

/*
#define DEBUG
*/

/***** Contains:
int JLP_GET_DATA_PLOT_PARAM(float *xmin_user0, float *xmax_user0,
                            float *ymin_user0, float *ymax_user0,
                            float *zmin_user0, float *zmax_user0,
                            int *idv1);
int JLP_GET_PLOT_PARAM(int *offx1, int *offy1, int *axlen1,
                       int *aylen1, float *xmin, float *xmax,
                       float *ymin, float *ymax, int *plan, int *idv1);
int JLP_SET_PLOT_PARAM(int *offx1, int *offy1, int *axlen1,
                       int *aylen1, float *xmin, float *xmax,
                       float *ymin, float *ymax, int *plan, int *idv1);
int JLP_SET_NEW_LIMITS(float *box_xmin, float *box_xmax, float *box_ymin,
                       float *box_ymax, int *idv1);
int JLP_DEVICE_CURVE(char *plotdev, char *out_filename,
                     float *xmin_user1, float *xmax_user1,
                     float *ymin_user1, float *ymax_user1, int plan,
                     char *title, int *idv1);
int JLP_DRAW(int *x, int *y, int *idv1);
int JLP_RELOC(int *x, int *y, int *idv1);
int JLP_LINE1(float *xx1, float *yy1, float *xx2, float *yy2, int *idv1);
int JLP_LINE1_BACKUP(float *xx1, float *yy1, float *xx2, float *yy2,
                     int *line_width, int *backup_to_file, int *idv1);
int JLP_SETCOLOR(int *r, int *g, int *b, int *idv1);
int JLP_EVENTS(int *idv1);
int JLP_GFLUSH(int *idv1);
int CONV_USER_TO_MGO(float *x_user, float *y_user, int *ix, int *iy,
                     int *idv1);
int CONV_MGO_TO_USER(int *ix, int *iy, float *x_user, float *y_user,
                     int *in_frame, int *idv1);
int JLP_WHERE(float *x, float *y, int *in_frame, int *pressed_button,
              int *draw_cross, int *idv1);
int JLP_SPCLOSE(int *idv1);
int JLP_DRAW_TO_STATUS_BAR(char *label, int *idv1);
int JLP_ERASE_STATUS_BAR(int *idv1);
int JLP_SETLINEPARAM(int *lwidth, int *ltype, int *idv1);
int JLP_SETPCOLOR(char *pcolor, int *idv1);
*****/

/***********************************************************
* JLP_GET_DATA_PLOT_PARAM
*
* To read data plot parameters 
************************************************************/
int JLP_GET_DATA_PLOT_PARAM(float *xmin_user0, float *xmax_user0, 
                            float *ymin_user0, float *ymax_user0, 
                            float *zmin_user0, float *zmax_user0, 
                            int *idv1)
{
int status = 0;

 *xmin_user0 = Jgdev_xmin_user(*idv1);
 *xmax_user0 = Jgdev_xmax_user(*idv1);
 *ymin_user0 = Jgdev_ymin_user(*idv1);
 *ymax_user0 = Jgdev_ymax_user(*idv1);
 *zmin_user0 = Jgdev_zmin_user(*idv1);
 *zmax_user0 = Jgdev_zmax_user(*idv1);

return(status);
}
/***********************************************************
* JLP_GET_PLOT_PARAM
*
* To read plot variables
************************************************************/
int JLP_GET_PLOT_PARAM(int *offx1, int *offy1, int *axlen1, 
                       int *aylen1, float *box_xmin, float *box_xmax, 
                       float *box_ymin, float *box_ymax, int *plan, int *idv1)
{
int status = -1;

 *offx1 = Jgdev_offx(*idv1);
 *offy1 = Jgdev_offy(*idv1);
 *axlen1 = Jgdev_axlen(*idv1);
 *aylen1 = Jgdev_aylen(*idv1);
 *box_xmin = Jgdev_box_xmin(*idv1);
 *box_xmax = Jgdev_box_xmax(*idv1);
 *box_ymin = Jgdev_box_ymin(*idv1);
 *box_ymax = Jgdev_box_ymax(*idv1);
 *plan = Jgdev_box_plan(*idv1);
 status = 0;

return(status);
}
/***********************************************************
* JLP_SET_PLOT_PARAM
*
* To write plot variables
************************************************************/
int JLP_SET_PLOT_PARAM(int *offx1, int *offy1, int *axlen1, 
                       int *aylen1, float *box_xmin, float *box_xmax, 
                       float *box_ymin, float *box_ymax, int *plan1,
                       int *idv1)
{
int status = -1;

if(GDev_from_idv(*idv1)) {
 status = GDev_from_idv(*idv1)->SetBoxLimits(*box_xmin, *box_xmax, 
                                             *box_ymin, *box_ymax, 0., 1.);
 if(status == 0)
   status = GDev_from_idv(*idv1)->SetNewBoxSetup(*offx1, *offy1, 
                                                 *axlen1, *aylen1);
 }

return(status);
}
/***********************************************************
* JLP_SET_NEW_LIMITS
*
*
* To set plot variables
************************************************************/
int JLP_SET_NEW_LIMITS(float *box_xmin, float *box_xmax, float *box_ymin, 
                       float *box_ymax, int *idv1)
{
int status = -1;

if(GDev_from_idv(*idv1)) {
 status = GDev_from_idv(*idv1)->SetBoxLimits(*box_xmin, *box_xmax, 
                                             *box_ymin, *box_ymax, 0., 1.);
 }

return(status);
}
/***************************************************************
* To open a graphic device for curve plots
*
* OUTPUT:
*  idv1 : number of the plotting device
*
***************************************************************/
int JLP_OPEN_DEVICE_FOR_CURVES(char *plotdev, char *out_filename, 
                               double user_xmin1, double user_xmax1, 
                               double user_ymin1, double user_ymax1,
                               int plan, char *title, int *idv1)
{
int status = -1;
char err_messg[128];
JLP_GDev *Jgd0;

switch (plotdev[0])
{
// Postscript: Postscript, postscript, SQUARE, square, LANDSCAPE, landscape
  case 'P':
  case 'p':
  case 'S':
  case 's':
  case 'l':
  case 'L':
    Jgd0 = new JLP_GDev_PST(plotdev, out_filename, title, 
                            user_xmin1, user_xmax1, user_ymin1, user_ymax1,
                            plan, &status, err_messg);
    *idv1 = Jgd0->Jgc0_dev_idv();
    break;
  default:
    fprintf(stderr,"JLP_OPEN_DEVICE_FOR_CURVES/Error: unknown device type: %s\n",
            plotdev);
    break;
}

/* In case of failure free idv1 number */
  if(status) {
    fprintf(stderr,"JLP_OPEN_DEVICE_FOR_CURVES/Error opening %s \n", plotdev);
    }
  else {
/* Look for an identifier (idv1) and initialize it with current opened device */
    status = GDev_alloc_idv(Jgd0, idv1, err_messg);
#ifdef DEBUG
    printf("JLP_OPEN_DEVICE_FOR_CURVES/graphic device #idv1 = %d sucessfully opened\n",
           *idv1);
#endif

// Initialize common bloc PARAMETERS for fortran interface:
// Should be avoided when many devices are opened simultaneously...
/*
    JGC_TO_COMMON(idv1);
*/
    }

 return(status);
}


/***************************************************************
* To open a graphic device for curve plots
*
* OUTPUT:
*  idv1 : number of the plotting device
*
***************************************************************/
int JLP_DEVICE_CURVE(char *plotdev, char *out_filename, float *user_xmin1, 
                     float *user_xmax1, float *user_ymin1, float *user_ymax1,
                     int *plan, char *title, int *idv1)
{
int status = -1;
char err_messg[128];
JLP_GDev *Jgd0;

switch (plotdev[0]) 
{
// Postscript: Postscript, postscript, SQUARE, square, LANDSCAPE, landscape
  case 'P':
  case 'p':
  case 'S':
  case 's':
  case 'l':
  case 'L':
    Jgd0 = new JLP_GDev_PST(plotdev, out_filename, title, 
                            *user_xmin1, *user_xmax1, *user_ymin1, *user_ymax1, 
                            *plan, &status, err_messg);
    *idv1 = Jgd0->Jgc0_dev_idv();
    break;
#if JLP_USE_X11
// X11: XDISPLAY, xdisplay, XTERM, xterm: 
  case 'X':
  case 'x':
    Jgd0 = new JLP_GDev_X11(plotdev, out_filename, title,
                            *user_xmin1, *user_xmax1, *user_ymin1, *user_ymax1, 
                            *plan, status, err_messg);
    *idv1 = Jgd0->Jgc0_dev_idv();
    break;
#endif
// No longer used (October 2014 ...)
#if JLP_USE_WXWID
// wxWidgets: wterm, wdisplay
  case 'W':
  case 'w':
   fprintf(stderr,"Sorry wxwidget curve device not ready yet\n");
   exit(-1);
/*
    Jgd0 = new JLP_GDev_wxWID(plotdev, out_filename, title,
                              *user_xmin1, *user_xmax1, *user_ymin1, 
                              *user_ymax1, *plan, status, err_messg);
    *idv1 = Jgd0->Jgc0_dev_idv();
*/
    break;
#endif
  default:
    fprintf(stderr,"JLP_DEVICE_CURVE/Error: unknown device type: %s\n", 
            plotdev);
    break;
}

/* In case of failure free idv1 number */
  if(status) {
    fprintf(stderr,"JLP_DEVICE_CURVE/Error opening %s \n", plotdev);
    }
  else {
/* Look for an identifier (idv1) and initialize it with current opened device */
    status = GDev_alloc_idv(Jgd0, idv1, err_messg);
#ifdef DEBUG
    printf("JLP_DEVICE_CURVE/graphic device #idv1 = %d sucessfully opened\n", 
           *idv1);
#endif

// Initialize common bloc PARAMETERS for fortran interface:
// Should be avoided when many devices are opened simultaneously...
/*
    JGC_TO_COMMON(idv1);
*/
    }

 return(status);
}
/* *************************************************************
 * Draw from current location to (x,y)
 * "Mongo" coordinates
 ***************************************************************/
int JLP_DRAW(int *x, int *y, int *idv1)
{
int status = -1;

 if(GDev_from_idv(*idv1)) {
   GDev_from_idv(*idv1)->gdev_lineto(*x, *y);
   status = 0;
   }

return(status);
}
/* *************************************************************
 * Set current location to (x,y)
 * "Mongo" coordinates
 ***************************************************************/
int JLP_RELOC(int *x, int *y, int *idv1)
{
int status = -1;

 if(GDev_from_idv(*idv1)) {
   GDev_from_idv(*idv1)->gdev_moveto(*x, *y);
   status = 0;
   }

return(status);
}
/***************************************************************
 * JLP_LINE1
 * To draw a line from xx1,yy1 to xx2,yy2 (user coordinates)
 * Same as jlp_line but with user coordinates
 * 
 * Simpler version of JLP_LINE1_BACKUP
 ***************************************************************/
int JLP_LINE1(float *xx1, float *yy1, float *xx2, float *yy2, int *idv1)
{
int status;
int line_width, backup_to_file;

line_width = 0;
backup_to_file = 0;

status = JLP_LINE1_BACKUP(xx1, yy1, xx2, yy2, &line_width, &backup_to_file,
                          idv1);

return(status);
}
/***************************************************************
* JLP_LINE1_BACKUP
* To draw a line from xx1,yy1 to xx2,yy2 (user coordinates)
* Same as jlp_line but with user coordinates
* 
* Possibility of backup 
***************************************************************/
int JLP_LINE1_BACKUP(float *xx1, float *yy1, float *xx2, float *yy2, 
                     int *line_width, int *backup_to_file, int *idv1)
{
int status = -1;

 if(GDev_from_idv(*idv1)) {
   GDev_from_idv(*idv1)->gdev_line1(*xx1, *yy1, *xx2, *yy2, *line_width, *backup_to_file);
   status = 0;
   }

return(status);
}
/* *************************************************************
 * Set pen color for drawing lines
 *
 * INPUT:
 * r,g,b between 0 and 255
 ***************************************************************/
int JLP_SETCOLOR(int *r, int *g, int *b, int *idv1)
{
int status = -1;
float rr, gg, bb;

   rr = ((float)*r)/256.;
   gg = ((float)*g)/256.;
   bb = ((float)*b)/256.;

/* setrgbcolor: input values between 0. and 1. 
*/
 if(GDev_from_idv(*idv1)) {
   GDev_from_idv(*idv1)->setrgbcolor(rr,gg,bb);
   status = 0;
   }

return(status);
}
/* *************************************************************
 * Wait for next event (mouse button for X11) 
 *
 ***************************************************************/
int JLP_EVENTS(int *idv1)
{
int status = -1;

 if(GDev_from_idv(*idv1)) {
   GDev_from_idv(*idv1)->wait_for_events();
   status = 0;
   }

return(status);
}
/* *************************************************************
 * Flush graphic to device
 *
 ***************************************************************/
int JLP_GFLUSH(int *idv1)
{
int status = -1;

 if(GDev_from_idv(*idv1)) {
   GDev_from_idv(*idv1)->gdev_gflush();
   status = 0;
   }

return(status);
}
/**************************************************************
* Conversion from user coordinates to mgo coordinates
***************************************************************/
int CONV_USER_TO_MGO(float *x_user, float *y_user, int *ix, int *iy, 
                     int *idv1)
{
int jx, jy, status = -1;

 if(GDev_from_idv(*idv1)) {
   GDev_from_idv(*idv1)->conv_user_to_mgo(*x_user, *y_user, &jx, &jy);
   *ix = jx; *iy = jy;
   status = 0;
   }

return(status);
}
/**************************************************************
* Conversion from mgo coordinates to user coordinates
***************************************************************/
int CONV_MGO_TO_USER(int *ix, int *iy, float *x_user, float *y_user, 
                     int *in_frame, int *idv1)
{
int status = -1, in_frm = 0;
double x = 0, y = 0;

 if(GDev_from_idv(*idv1)) {
   GDev_from_idv(*idv1)->conv_mgo_to_user(*ix, *iy, &x, &y, &in_frm);
   status = 0;
   }

/* Transfer to output values: */
*in_frame = in_frm;
*x_user = x;
*y_user = y;
return(status);
}
/**************************************************************
* Returns user coordinates (assuming linear scale)
* and flag in_frame (set to 1 if in the user frame)
*
***************************************************************/
int JLP_WHERE(float *x, float *y, int *in_frame, int *pressed_button,
              int *draw_cross, int *idv1)
{
int status = -1, size, symbol;
int ix, iy, p_button = 0, in_frm = 0;
char cursor_name[80];

/* From 1996: "big_crosshair" (before: "circle") */
 strcpy(cursor_name, "big_crosshair");
 if(GDev_from_idv(*idv1)) {
   status = GDev_from_idv(*idv1)->cursor(ix, iy, cursor_name, p_button);
   }

/* Conversion to user coordinates and check if in the frame: */
 if(status == 0) {
   CONV_MGO_TO_USER(&ix, &iy, x, y, &in_frm, idv1);
   if(*draw_cross) {
     size = 2; symbol = 4;
     JLP_SYMBOL(&ix,&iy,&size,&symbol,idv1);
     JLP_GFLUSH(idv1);
     }
   } else { 
     *x = *y = -1; *in_frame = 0; 
   }

/* Transfer to output values: */
*pressed_button = p_button;
*in_frame = in_frm;

return(status);
}
/***************************************************************
* Close graphic device
***************************************************************/
int JLP_SPCLOSE(int *idv1)
{
int status = -1;

if(GDev_from_idv(*idv1)) {
  status = GDev_from_idv(*idv1)->gdev_close();
  if(status == 0) {
    GDev_free_idv(*idv1);
// Delete Gdev 
//(SHOULD BE DONE HERE and not in jlp_idv.cpp for compatiblity with jlp_wxplot!)
    delete GDev_from_idv(*idv1);
    }
  }


return(status);
}
/****************************************************************
* DrawToStatusBar(char *label)
****************************************************************/
int JLP_DRAW_TO_STATUS_BAR(char *label, int *idv1)
{
int status;

 if(GDev_from_idv(*idv1)) {
   status = GDev_from_idv(*idv1)->DrawToStatusBar(label);
   }

return(status);
}
/****************************************************************
* Erase StatusBar(char *label)
****************************************************************/
int JLP_ERASE_STATUS_BAR(int *idv1)
{
int status;

 if(GDev_from_idv(*idv1)) {
   status = GDev_from_idv(*idv1)->EraseStatusBar();
   }

return(status);
}
/*****************************************************************************
* Set the color for further drawings
*
* ltype =0 for solid, =1  for dash curves
*****************************************************************************/
int JLP_SETLINEPARAM(int *lwidth, int *ltype, int *idv1)
{
int status = -1;

 if(GDev_from_idv(*idv1)) {
   status = GDev_from_idv(*idv1)->SetLineWidthAndType(*lwidth, *ltype);
   }

return(status);
}
/*****************************************************************************
* Set the color for further drawings
*****************************************************************************/
int JLP_SETPCOLOR(char *pcolor, int *idv1)
{
int status = -1;

 if(GDev_from_idv(*idv1)) {
   status = GDev_from_idv(*idv1)->SetPColor(pcolor);
   }

return(status);
}
