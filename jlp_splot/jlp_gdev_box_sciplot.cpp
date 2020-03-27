/**************************************************************************
* jlp_myaxes.cpp
*
* Logarithmic or Cartesian axes
* (from SciSoft version July 2007, but strongly modified)
* called when box_type == 1 
*
* Contains:
* jlp_box_type1(char *xlabel, char *ylabel, char *title, int ticks_in, ...)
*
* 2015: tested with offx1 = 5000; offy1 = 5000; axlen1 = 26000; aylen1 = 26000
*       for screen display with Linux/wxwidgets
* JLP
* Version 17/02/2017
*************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>      // strcpy
#include <math.h>
#include "jlp_gdev.h"
#include "jlp_macros.h"  // NINT

/*
#define DEBUG
#define DEBUG_AXIS
*/

/* Also defined in "jlp_gdev_def.h"
typedef struct {
  double Origin;
  double Size;
  double AxisPos;
  double DrawOrigin;
  double DrawSize;
  double DrawMax;
  double MajorInc;
  int MajorNum;
  int MinorNum;
  int Precision;
  int Axis_is_log10;
} SciPlotAxis;
*/

#define powi(a,i)    (double)pow(a,(double)((int)i))
static void ComputeAxis(SciPlotAxis *axis, double min_data, double max_data,
                        double *box_min, double *box_max, 
                        int axis_is_log);
static int ScaleToAxis(SciPlotAxis Axis, double xin);

/*
int jlp_sciplot_InitializeAxes(SciPlotAxis *x_axis, SciPlotAxis *y_axis);
int jlp_sciplot_DrawAxis(SciPlotAxis Axis, int xorigin, int yorigin,
                        int axis_length, double min_data, double max_data,
                        double angle, int plot_AxisNumbers,
                        int ticks_up, int labels_down, int Axis_is_log10);
int jlp_sciplot_DrawAxis_part2(SciPlotAxis Axis, int xorigin, int yorigin,
                        int axis_length, double box_min, double box_max,
                        double angle, int plot_AxisNumbers,
                        int ticks_up, int labels_down, int Axis_is_log10, 
                        int xgrid, int big_ticklen, int small_ticklen);
int jlp_sciplot_DrawGrid(SciPlotAxis Axis, int xorigin, int yorigin, 
                        int axis_length, int other_axis_length, 
                        double box_min, double box_max, double axis_angle, 
                        double other_axis_angle, char *nchar,
                        char *pcolor, int axis_is_log10);
*/

#ifdef MAIN_PROG
int main(int argc, char *argv[])
{
char plotdev[60], title[60], buffer[20];
double xmindata,xmaxdata,ymindata,ymaxdata;
int plan, idv1;
int xgrid_is_wanted, ygrid_is_wanted; 
int x_is_log10, y_is_log10; 
int ticks_in, box_numbers, status;

strcpy(plotdev,"xterm");
strcpy(title,"Curve plot with C");
xmindata = 2.;
xmaxdata = 50.;
ymindata = 8.;
ymaxdata = 6500.;
plan = 0;

status = JLP_DEVICE_CURVE(plotdev, &xmindata, &xmaxdata, &ymindata,
                          &ymaxdata, plan, title, &idv1);

printf("Output from JLP_DEVICE_CURVE: status=%d \n", status);
if(status) return(-1);

xgrid_is_wanted = 0;
ygrid_is_wanted = 0;
x_is_log10 = 1;
y_is_log10 = 1;
ticks_in = 0;
box_numbers = 1;
jlp_box_type1("Xlabel", "Ylabel", "Title", ticks_in, box_numbers,
            1, xgrid_is_wanted, ygrid_is_wanted, 
            x_is_log10, y_is_log10,
            xmindata, xmaxdata, ymindata, ymaxdata, 4000, 6000, 22000, 22000,
            idv1);

JLP_GFLUSH(&idv1);

 printf(" OK for exit ? \n");
 gets(buffer);

JLP_GFLUSH(&idv1);

 printf(" OK for exit ? \n");
 gets(buffer);

JLP_SPCLOSE(&idv1);

return(0);
}
#endif

/*****************************************************************************
* FUNCTION: jlp_box_sciplot
*
* Called by jlp_box (for main frame) or directly by Xdisp1, LUT key, etc
* when jlp_axes are wanted
*
* PURPOSE: Draw a box for coordinate information
* (using box_xmin, box_xmax, box_ymin and box_ymax for the axes limits)
* It is needed when images are displayed (e.g., for SCIDAR wind parameters)
*
* INPUT:
* ticks_in: flag set to 1 if ticks inside the frame
* plot_axis_numbers: flag set to 1 if axes are labeled
* full_caption: flag set to 1 if title is displayed
* xgrid_is_wanted: flag set to one if X grid is wanted
* ygrid_is_wanted: flag set to one if Y grid is wanted
* x_is_log10: 0 if linear, 1 if log10 axis
* y_is_log10: 0 if linear, 1 if log10 axis
* ix0, iy0: lower-left position of the box (mgo coordinates)
* ixlen, iylen: x and y size of the box (mgo coordinates)
******************************************************************************/
int JLP_GDev::jlp_box_sciplot(char *xlabel, char *ylabel, char *title, 
                              int ticks_in, int box_numbers, int full_caption, 
                              int xgrid_is_wanted, int ygrid_is_wanted, 
                              int x_is_log10, int y_is_log10,
                              double xmin_data, double xmax_data, 
                              double ymin_data, double ymax_data, int ix0, 
                              int iy0, int ixlen, int iylen,
                              char *axis_color)
{
SciPlotAxis x_axis, y_axis;
double angle, expand = 1.2;
int plot_axis_numbers, ticks_up, labels_down, smallest_ix, smallest_iy;
double box_xmin, box_xmax, box_ymin, box_ymax;
char buf1[20], buf2[20], default_color[40];

#ifdef DEBUG
printf("box_sciplot/xmin=%f xmax=%f ymin_data=%f, ymax_data=%f \n", 
        xmin_data, xmax_data, ymin_data, ymax_data);
#endif

// Set color: 
 SetPColor(axis_color);

jlp_sciplot_InitializeAxes(&x_axis, &y_axis);

ComputeAxis (&x_axis, xmin_data, xmax_data, &box_xmin, &box_xmax, x_is_log10);

ComputeAxis (&y_axis, ymin_data, ymax_data, &box_ymin, &box_ymax, y_is_log10);

#ifdef DEBUG
printf("box_sciplot/box_xmin, box_xmax = %f %f (x_is_log10=%d)\n", 
        box_xmin, box_xmax, x_is_log10);
printf("box_sciplot/box_ymin, box_ymax = %f %f (y_is_log10=%d)\n", 
        box_ymin, box_ymax, y_is_log10);
#endif

/* Update box_xmin, box_xmax that have just been modified in ComputeAxis ... */
SetBoxLimits(box_xmin, box_xmax, box_ymin, box_ymax, 1., 0.);

/* Y axis (angle=90.) */
angle = 90.;
plot_axis_numbers = box_numbers;
labels_down = 0;
if(ticks_in) ticks_up = 0; else ticks_up = 1;
jlp_sciplot_DrawAxis(y_axis, ix0, iy0, iylen, box_ymin, box_ymax, angle, 
                     plot_axis_numbers, ticks_up, labels_down, y_is_log10); 

angle = 90.;
plot_axis_numbers = 0;
labels_down = 1;
if(ticks_in) ticks_up = 1; else ticks_up = 0;
jlp_sciplot_DrawAxis(y_axis, ix0 + ixlen, iy0, iylen, box_ymin, box_ymax, 
                     angle, plot_axis_numbers, ticks_up, labels_down, 
                     y_is_log10); 
/* X axis (angle=0.) */
angle = 0.;
labels_down = 1;
plot_axis_numbers = box_numbers;
if(ticks_in) ticks_up = 1; else ticks_up = 0;
jlp_sciplot_DrawAxis(x_axis, ix0, iy0, ixlen, box_xmin, box_xmax, angle, 
                     plot_axis_numbers, ticks_up, labels_down, x_is_log10); 

angle = 0.;
labels_down = 0;
plot_axis_numbers = 0;
if(ticks_in) ticks_up = 0; else ticks_up = 1;
jlp_sciplot_DrawAxis(x_axis, ix0, iy0 + iylen, ixlen, box_xmin, box_xmax,
                     angle, plot_axis_numbers, ticks_up, labels_down, 
                     x_is_log10); 

// JLP2019: temporary values (precise tuning TOBEDONE)
smallest_ix = ix0;
smallest_iy = iy0;
jlp_DrawBoxLabels(xlabel, ylabel, title, full_caption, ix0, iy0, ixlen, 
                  iylen, expand, smallest_ix, smallest_iy);

/* Draw X grid (parallel to Y axis): */
    if(xgrid_is_wanted) {
        strcpy(buf1, "L1");
        strcpy(buf2, "Yellow");
    	jlp_sciplot_DrawGrid(x_axis, ix0, iy0, ixlen, iylen, box_xmin, 
                             box_xmax, 0., 90., buf1, buf2, x_is_log10);
    }

/* Draw Y grid (parallel to X axis): */
    if(ygrid_is_wanted) {
        strcpy(buf1, "L2");
        strcpy(buf2, "Orange");
        jlp_sciplot_DrawGrid(y_axis, ix0, iy0, iylen, ixlen, box_ymin, 
                             box_ymax, 90., 0., buf1, buf2, y_is_log10);
    }

// Back to default color: 
 strcpy(default_color,"Default");
 SetPColor(default_color);

return(0);
}
/***************************************************************************
* Conversion from user coordinates to screen (mgo) coordinates
*
* INPUT:
*   xin: user coordinate
* OUTPUT:
*   xout: screen mgo coordinate
***************************************************************************/
static int ScaleToAxis(SciPlotAxis Axis, double xin)
{
  double xout;

  if (Axis.Axis_is_log10)
    xout = ((log10(xin) - log10(Axis.DrawOrigin)) *
             (Axis.Size / Axis.DrawSize));
  else
    xout = ((xin - Axis.DrawOrigin) * (Axis.Size / Axis.DrawSize));
  return(NINT(xout));
}

/************************************************************************
* ComputeAxis:
* compute parameters used for drawing the axes
* (From SciPlot, version of July 2007)
*
*
* INPUT:
* mindata, maxdata (in log10 if axis_is_log!) : minimum and maximum data values
*
* OUTPUT:
* box_min, box_max (in log10 if axis_is_log!)
************************************************************************/

/* JLP2007 
* Too many labels! 
#define NUMBER_MINOR	8
#define MAX_MAJOR	8
static double CAdeltas[8] = {0.1, 0.2, 0.25, 0.5, 1.0, 2.0, 2.5, 5.0};
static int CAdecimals[8] = {0, 0, 1, 0, 0, 0, 1, 0};
static int CAminors[8] = {4, 4, 4, 5, 4, 4, 4, 5};
*/
/* Better now: (?)
*/
#define NUMBER_MINOR	6
#define MAX_MAJOR	6
static double CAdeltas[6] = {0.1, 0.2, 0.5, 1.0, 2.0, 5.0};
static int CAdecimals[6] = {0, 0, 0, 0, 0, 0};
static int CAminors[6] = {4, 4, 5, 4, 4, 5};

static void ComputeAxis(SciPlotAxis *axis, double mindata, double maxdata,
                        double *box_min, double *box_max, 
                        int axis_is_log)
{
double range, rnorm, delta, calcmin, calcmax;
double new_box_min, new_box_max;
int nexp, majornum, minornum, majordecimals, decimals, i;

#ifdef DEBUG
printf("ComputeAxis/Input values: mindata=%f, maxdata=%f\n",
                mindata, maxdata);
#endif

/* Swap end and origin if maxdata < mindata:
*/
if(mindata == maxdata) {
  mindata -= 1.; 
  maxdata += 1.; 
  }
if(mindata < maxdata) {
  new_box_min = mindata; 
  new_box_max = maxdata; 
  } else {
  new_box_max = mindata; 
  new_box_min = maxdata; 
  }
if(axis_is_log) {
  new_box_min = pow(10.0, (double)new_box_min);
  new_box_max = pow(10.0, (double)new_box_max);
  }

range = new_box_max - new_box_min;

/************* Logarithmic scale **************************/
if (axis_is_log) {
    if(new_box_min <= 0) {
      printf("ComputeAxis/Log scale/Fatal error: negative/null values: box_min=%f, box_max=%f\n",
              new_box_min, new_box_max);
      exit(-1);
      }
    if (range==0.0) {
      calcmin = powi(10.0, (int) floor(log10(new_box_min)));
      calcmax = 10.0*calcmin;
    } else {
      calcmin = powi(10.0, (int) floor(log10(new_box_min)));
      calcmax = powi(10.0, (int) ceil(log10(new_box_max)));
    }
    
#ifdef DEBUG
     printf("Log scale/calcmin=%e box_min=%e calcmax=%e box_max=%e\n",
             calcmin, new_box_min, calcmax, new_box_max); 
#endif
    
    delta = 10.0;

    axis->DrawOrigin = calcmin;
    axis->DrawMax = calcmax;
    axis->DrawSize = log10(calcmax) - log10(calcmin);
    axis->MajorInc = delta;
    axis->MajorNum = (int) (log10(calcmax) - log10(calcmin)) + 1;
    axis->MinorNum = 10;
    axis->Precision = -(int) (log10(calcmin) * 1.0001);
    if (axis->Precision < 0) axis->Precision = 0;

#ifdef DEBUG
    printf("Log scale/calcmin=%e log=%e (int)log=%d  Precision=%d\n",
      calcmin, log10(calcmin), (int) (log10(calcmin) * 1.0001), axis->Precision);
#endif
}
/************* Linear scale **************************/
else {
    if (range==0.0) nexp=0;
    else nexp = (int) floor(log10(range));
    rnorm = range / powi(10.0, nexp);
    for (i = 0; i < NUMBER_MINOR; i++) {
      delta = CAdeltas[i];
      minornum = CAminors[i];
      majornum = (int) ((rnorm + 0.9999 * delta) / delta);
      majordecimals = CAdecimals[i];
      if (majornum <= MAX_MAJOR) break;
    }
    delta *= powi(10.0, nexp);
#ifdef DEBUG
    printf("Linear scale/nexp=%d range=%f rnorm=%f delta=%f\n", nexp, range, rnorm, delta);
#endif

    if (new_box_min < 0.0)
      calcmin = ((double) ((int) ((new_box_min - .9999 * delta) / delta))) * delta;
    else if ((new_box_min > 0.0) && (new_box_min < 1.0))
      calcmin = ((double) ((int) ((1.0001 * new_box_min) / delta))) * delta;
    else if (new_box_min >= 1.0)
      calcmin = ((double) ((int) ((.9999 * new_box_min) / delta))) * delta;
    else
      calcmin = new_box_min;

    if (new_box_max < 0.0)
      calcmax = ((double) ((int) ((.9999 * new_box_max) / delta))) * delta;
    else if (new_box_max > 0.0)
      calcmax = ((double) ((int) ((new_box_max + .9999 * delta) / delta))) * delta;
    else
      calcmax = new_box_max;

    axis->DrawOrigin = calcmin;
    axis->DrawMax = calcmax;
    axis->DrawSize = calcmax - calcmin;
    axis->MajorInc = delta;
    axis->MajorNum = majornum;
    axis->MinorNum = minornum;

    delta = log10(axis->MajorInc);
    if (delta > 0.0)
      decimals = -(int) floor(delta) + majordecimals;
    else
      decimals = (int) ceil(-delta) + majordecimals;
    if (decimals < 0) decimals = 0;
    axis->Precision = decimals;

#ifdef DEBUG
    printf("delta=%f majordecimals=%d decimals=%d\n",
            delta, majordecimals, decimals);
#endif
}   /* EOF case of linear scale */

#ifdef DEBUG
printf("Ticks: min=%f max=%f size=%f  major inc=%f #major=%d #minor=%d decimals=%d\n",
       axis->DrawOrigin, axis->DrawMax, axis->DrawSize,
       axis->MajorInc, axis->MajorNum, axis->MinorNum, axis->Precision);
#endif

/***************************************************
* Save box_min, box_max values 
*   (swapping end and origin if box_max < box_min) :
*/
if(mindata < maxdata) {
  *box_min = calcmin; 
  *box_max = calcmax; 
  } else {
  *box_min = calcmax; 
  *box_max = calcmin; 
  }

if(axis_is_log) {
  *box_min = log10(*box_min);
  *box_max = log10(*box_max);
  }

return;
}
/***************************************************************************
* jlp_sciplot_DrawAxis
*
* INPUT:
*    xorigin = x coordinate of the origin in mgo pixels
*    yorigin = y coordinate of the origin in mgo pixels
*    axis_length = length of the axis in mgo pixels
*    box_min = lower box value in X
*    box_max = upper box value in X
*    angle = [0. or 90] angle with horizontal reference
*    label_flag = [0 or 1] draw labels if flag set to 1
*    axis_type = 0 if linear axis, 1 if log10 axis
* For ticks out: upper axis: ticks_up = 1;  lower axis: ticks_up = 0
* For ticks out: right axis: ticks_up = 0;  left axis: ticks_up = 1
*    labels_down: (1 if down for X axis, 0 if left for Y axis)
*    Axis_is_log10 = flag set to one if logarithmic axis
***************************************************************************/
int JLP_GDev::jlp_sciplot_DrawAxis(SciPlotAxis Axis, int xorigin, int yorigin,
                                   int axis_length, double box_min, 
                                   double box_max, double angle, 
                                   int plot_AxisNumbers, int ticks_up, 
                                   int labels_down, int Axis_is_log10)
{
int iy1, iy2, ix, iy, big_ticklen = 600, small_ticklen = 350;
int iscale, ixt, iyt, ixl, iyl; 
double val, wlength, cos0, sin0, cos1, sin1, ww, aspect;
double cheight, expand1 = 1.2;        /* Size of fonts */
int s_tick, s_label, iw, pst_device;
int precision, xend, yend, idist_for_labels;
int xgrid = 0;   /* To allow the possibility of drawing a grid */
char numberformat[16], label[16];

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
 Jgc0_set_font_size_from_device(expand1);
 cheight = Jgc0_cheight();

/* Sign for ticks (up/down) */
s_tick = (ticks_up == 1) ? 1 : -1;
s_label = (labels_down == 1) ? -1 : 1;

 cos0 = cos((double)(angle*PI/180.));
 sin0 = sin((double)(angle*PI/180.));
 xend = xorigin + (int)((double)axis_length * cos0);
 yend = yorigin + (int)((double)axis_length * sin0);

/* For ticks and labels: (should not be changed when box_max < box_min) */
 cos1 = cos0; sin1 = sin0;

/* Swap end and origin if box_max < box_min:
*/
if(box_max < box_min) {
  angle += 180.;
  ww = box_min; box_min = box_max; box_max = ww;
  iw = xorigin; xorigin = xend; xend = iw;
  iw = yorigin; yorigin = yend; yend = iw;
  cos0 = cos((double)(angle*PI/180.));
  sin0 = sin((double)(angle*PI/180.));
  }

// Ticks_in: smaller distance
if(ticks_up && labels_down) 
  idist_for_labels = (int)(cheight + big_ticklen + 50.);
// Ticks_out: larger distance
else 
  idist_for_labels = (int)(cheight + big_ticklen + 200.);
iy1 = yorigin;
iy2 = yend;
Axis.Size = axis_length;
Axis.Axis_is_log10 = Axis_is_log10; 

// aspect = height/width 
 aspect = (double)Jgc0_dev_height() / (double)Jgc0_dev_width();

/* First draw axis without ticks */
  gdev_line(xorigin,yorigin,xend,yend);

/********************************************************** 
* Now draw ticks and labels: 
**********************************************************/

// Handle special case when big tick is the first tick to be drawn:
  precision = Axis.Precision;
  sprintf(numberformat, "%%.%df", precision);
  if (Axis_is_log10) {
    if (precision > 0) precision--;
  }
  val = Axis.DrawOrigin;
  iscale = ScaleToAxis(Axis, val);
  ix = NINT((double)iscale * cos0) + xorigin;
  iy = NINT((double)iscale * sin0) + yorigin;
  ixt = - NINT(s_tick * big_ticklen * sin1);
/* Division with aspect to correct difference of size 
* in the case of landscape format
*/
  iyt = NINT(s_tick * big_ticklen * cos1 / aspect);    
/* Draw big ticks: */
  gdev_line(ix, iy, ix + ixt, iy + iyt);
  if (plot_AxisNumbers) {
    ixl = - NINT(s_label * idist_for_labels * sin1);
    iyl = NINT(s_label * idist_for_labels * cos1);
// Print the corresponding value to "label":
    sprintf(label, numberformat, val);
#ifdef DEBUG_AXIS
    printf("JLP_sciplot_DrawAxis/First_part: bigtick for: %f\n", val);
#endif

    wlength =  gdev_label(label, ix + ixl, iy + iyl, 0., expand1, 0);
    if(angle == 0. || angle == 180.)
// JLP 2015: I add +500
      if(pst_device) {
      gdev_label(label, ix + ixl - NINT(wlength/2.), 
                iy + iyl - NINT(cheight), 0., 
                expand1, 1);
      } else {
      gdev_label(label, ix + ixl - NINT(wlength/2.), 
                iy + iyl - NINT(cheight) + 500, 0., 
                expand1, 1);
      }
// JLP 2015: I add +800 for linux, +900 for windows 
    else if(angle == 90. || angle == 270.) 
      if(pst_device) {
      gdev_label(label, ix + ixl - NINT(wlength), 
                iy + iyl - NINT(cheight/2), 0., expand1, 1);
      } else {
      gdev_label(label, ix + ixl - NINT(wlength), 
                iy + iyl - NINT(cheight/2) + 900, 0., expand1, 1);
      }
    else
      gdev_label(label, ix + ixl, iy + iyl, 0., expand1, 1);
  }

// Display all other ticks (big and small ticks)
jlp_sciplot_DrawAxis_part2(Axis, xorigin, yorigin, axis_length, box_min, 
                           box_max, angle, plot_AxisNumbers, ticks_up, 
                           labels_down, Axis_is_log10, xgrid, big_ticklen, 
                           small_ticklen);
return(0);
}
/************************************************************************
* Draw ticks and labels
*
*************************************************************************/
int JLP_GDev::jlp_sciplot_DrawAxis_part2(SciPlotAxis Axis, int xorigin, 
                                         int yorigin, int axis_length, 
                                         double box_min, double box_max,
                                         double angle, int plot_AxisNumbers,
                                         int ticks_up, int labels_down, 
                                         int Axis_is_log10, int xgrid, 
                                         int big_ticklen, int small_ticklen)
{
int iy1, iy2, ix, iy;
double tic, val, majorval, wlength, cos0, sin0, cos1, sin1, aspect;
int j, precision, xend, yend, idist_for_labels;
int iscale, ixt, iyt, ixl, iyl; 
int s_tick, s_label, pst_device;
double cheight, expand1 = 1.2;        /* Size of fonts */
char numberformat[16], label[16];

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

/* Sign for ticks (up/down) */
s_tick = (ticks_up == 1) ? 1 : -1;
s_label = (labels_down == 1) ? -1 : 1;

 cos0 = cos((double)(angle*PI/180.));
 sin0 = sin((double)(angle*PI/180.));
 xend = xorigin + (int)((double)axis_length * cos0);
 yend = yorigin + (int)((double)axis_length * sin0);
 iy1 = yorigin;
 iy2 = yend;

/* For ticks and labels: (should not be changed when box_max < box_min) */
 cos1 = cos0; sin1 = sin0;

// aspect = height/width 
 aspect = (double)Jgc0_dev_height() / (double)Jgc0_dev_width();

// Set Jgc0 font size from device:
 Jgc0_set_font_size_from_device(expand1);
 cheight = Jgc0_cheight();

// Ticks_in: smaller distance
if(ticks_up && labels_down)
  idist_for_labels = (int)(cheight + big_ticklen + 50.);
// Ticks_out: larger distance
else
  idist_for_labels = (int)(cheight + big_ticklen + 200.);

  precision = Axis.Precision;
  sprintf(numberformat, "%%.%df", precision);
  if (Axis_is_log10) {
    if (precision > 0) precision--;
  }


/**************** Main loop on big ticks (X axis) ************************/
  val = Axis.DrawOrigin;
#ifdef DEBUG_AXIS
    printf("JLP_sciplot_DrawAxis_part2/ val=%f max=%f\n", val, Axis.DrawMax);
#endif

  while(1) {

// JLP2015: handle case of negative values:
    if(Axis.DrawMax > 0){
       if(val * 1.0001 > Axis.DrawMax) break; 
      } else {
       if(-val * 1.0001 < -Axis.DrawMax) break; 
      }
    majorval = val;

/*********** Logarithmic X scale *******************/
    if (Axis_is_log10) {

/* Hack to make sure that 9.99999e? still gets interpreted as 10.0000e? */
// JLP2015: handle case of negative values:
    if(Axis.DrawMax > 0){
      if (majorval * 1.1 > Axis.DrawMax) break;
      } else {
      if (-majorval * 1.1 < -Axis.DrawMax) break;
      }

/**** Secondary loop on small ticks (X axis) ************************/
      tic = majorval;
	for (j = 2; j < Axis.MinorNum; j++) {
	  val = tic * (double) j;
          if(Axis.DrawMax > 0){
            if(val * 1.1 > Axis.DrawMax) break; 
            } else {
            if(-val * 1.1 < -Axis.DrawMax) break; 
            }
          iscale = ScaleToAxis(Axis, val);
          ix = NINT((double)iscale * cos0) + xorigin;
          iy = NINT((double)iscale * sin0) + yorigin;
          ixt = - NINT(s_tick * small_ticklen * sin1);
          iyt = NINT(s_tick * small_ticklen * cos1 / aspect);
          if(xgrid) gdev_line(ix, iy1, ix, iy2);
          gdev_line(ix, iy, ix + ixt, iy + iyt);
	}
      
      sprintf(numberformat, "%%.%df", precision);
      if (precision > 0) precision--;

/* Compute location of big tick: */
      val = tic * (double) Axis.MinorNum;
    }
/*********** Linear X scale *******************/
    else {
/**** Secondary loop on small ticks ************************/
      tic = majorval;
	for (j = 1; j < Axis.MinorNum; j++) {
	  val = tic + Axis.MajorInc * (double) j / Axis.MinorNum;
          if(Axis.DrawMax > 0){
            if(val * 1.0001 > Axis.DrawMax) break; 
            } else {
            if(-val * 1.0001 < -Axis.DrawMax) break; 
            }
          iscale = ScaleToAxis(Axis, val);
          ix = NINT((double)iscale * cos0) + xorigin;
          iy = NINT((double)iscale * sin0) + yorigin;
          ixt = - NINT(s_tick * small_ticklen * sin1);
          iyt = NINT(s_tick * small_ticklen * cos1 / aspect);
          if(xgrid) gdev_line(ix, iy1, ix, iy2);
          gdev_line(ix, iy, ix + ixt, iy + iyt);
	}
/* Compute location of big tick: */
      val = tic + Axis.MajorInc;
    }

  if(Axis.DrawMax > 0){
  if(val * 1.0001 > Axis.DrawMax) break; 
    } else {
    if(-val * 1.0001 < -Axis.DrawMax) break; 
    }
/*** Draw big tick (regular spacing for log or linear scales): ***********/
#ifdef DEBUG_AXIS
    printf("JLP_sciplot_DrawAxis_part2/ bigtick for: %f\n", val);
#endif

    iscale = ScaleToAxis(Axis, val);
    ix = NINT((double)iscale * cos0) + xorigin;
    iy = NINT((double)iscale * sin0) + yorigin;
    ixt = - NINT(s_tick * big_ticklen * sin1);
    iyt = NINT(s_tick * big_ticklen * cos1 / aspect);
    if(xgrid) gdev_line(ix, iy1, ix, iy2);
    gdev_line(ix, iy, ix + ixt, iy + iyt);

/*** Draw label if needed: */
    if (plot_AxisNumbers) {
      ixl = - NINT(s_label * idist_for_labels * sin1);
      iyl = NINT(s_label * idist_for_labels * cos1);
// Print the corresponding value to "label":
      sprintf(label, numberformat, val);
      wlength =  gdev_label(label, ix + ixl, iy + iyl, 0., expand1, 0);
      if(angle == 0. || angle == 180.)
// JLP 2015: I add +500 
        if(pst_device){
        gdev_label(label, ix + ixl - NINT(wlength/2.), 
                  iy + iyl - NINT(cheight), 0., 
                  expand1, 1);
        } else {
        gdev_label(label, ix + ixl - NINT(wlength/2.), 
                  iy + iyl - NINT(cheight) + 500, 0., 
                  expand1, 1);
        }
// JLP 2015: I add +800 for linux, +900 for windows 
      else if(angle == 90. || angle == 270.) 
        if(pst_device){
        gdev_label(label, ix + ixl - NINT(wlength), 
                  iy + iyl - NINT(cheight/2), 0., expand1, 1);
        } else {
        gdev_label(label, ix + ixl - NINT(wlength), 
                  iy + iyl - NINT(cheight/2) + 900, 0., expand1, 1);
        }
      else
        gdev_label(label, ix + ixl, iy + iyl, 0., expand1, 1);
    }
  }
return(0);
}
/***************************************************************************
* To initialize x_axis and y_axis SciPlotAxis structures
*
***************************************************************************/
int JLP_GDev::jlp_sciplot_InitializeAxes(SciPlotAxis *x_axis, 
                                         SciPlotAxis *y_axis)
{

x_axis->Origin = Jgc0_offx(); 
x_axis->Size = Jgc0_axlen(); 
y_axis->Origin = Jgc0_offy(); 
y_axis->Size = Jgc0_aylen(); 
/* ? (used for labels) */
x_axis->AxisPos = (Jgc0_offx() * 70) / 100; 
y_axis->AxisPos = (Jgc0_offy() * 70) / 100; 

return(0);
}
/***************************************************************************
*
* FUNCTION: jlp_sciplot_DrawGrid
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
*    box_min = lower box value of the axis 
*    box_max = upper box value of the axis
*    axis_angle = [0. or 90] angle of the axis with the horizontal reference
*    other_axis_angle = [0. or 90] angle of the other axis with
*                        the horizontal reference
*    nchar[]: type of symbols used for plotting the grid (L0, L1, etc)
*    pcolor[]: color to be used for drawing the grid
*    axis_is_log10: 0 if linear, 1 if log10
*
****************************************************************************/
int JLP_GDev::jlp_sciplot_DrawGrid(SciPlotAxis Axis, int xorigin, int yorigin, 
                                   int axis_length, int other_axis_length, 
                                   double box_min, double box_max, 
                                   double axis_angle, double other_axis_angle, 
                                   char *nchar, char *pcolor, int Axis_is_log10)
{
int iy1, iy2, ix, iy, iscale, ixt, iyt, ltype, lweight;
double tic, val, majorval, cos0, sin0, cos1, sin1, ww;
int iw, j, xend, yend, line_options;
char buffer[128];

cos0 = cos((double)(axis_angle*PI/180.));
sin0 = sin((double)(axis_angle*PI/180.));
xend = xorigin + (int)((double)axis_length * cos0);
yend = yorigin + (int)((double)axis_length * sin0);

cos1 = cos((double)(other_axis_angle*PI/180.));
sin1 = sin((double)(other_axis_angle*PI/180.));
ixt = NINT(other_axis_length * cos1);
iyt = NINT(other_axis_length * sin1);

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

iy1 = yorigin;
iy2 = yend;
Axis.Size = axis_length;
Axis.Axis_is_log10 = Axis_is_log10; 

/********** Decode line style: */
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
SetPColor(pcolor);
if(line_options) SetLineWidthAndType(ltype, lweight);

/********************************************************** 
* Now draw grid 
**********************************************************/

  val = Axis.DrawOrigin;
  iscale = ScaleToAxis(Axis, val);
  ix = NINT((double)iscale * cos0) + xorigin;
  iy = NINT((double)iscale * sin0) + yorigin;

/**************** Main loop on big ticks (X axis) ************************/
  majorval = val;
/*
  while ((majorval * 1.0001) < Axis.DrawMax) {
*/
  while (majorval < Axis.DrawMax) {

/*********** Logarithmic X scale *******************/
    if (Axis_is_log10) {

/**** Secondary loop on small ticks (X axis) ************************/
      tic = majorval;
	for (j = 2; j < Axis.MinorNum; j++) {
	  val = tic * (double) j;
          iscale = ScaleToAxis(Axis, val);
          ix = NINT((double)iscale * cos0) + xorigin;
          iy = NINT((double)iscale * sin0) + yorigin;
          gdev_line(ix, iy, ix + ixt, iy + iyt);
	}
      
/* Compute location of big tick: */
      val = tic * (double) Axis.MinorNum;
    }
/*********** Linear X scale *******************/
    else {
/**** Secondary loop on small ticks ************************/
      tic = majorval;
	for (j = 1; j < Axis.MinorNum; j++) {
	  val = tic + Axis.MajorInc * (double) j / Axis.MinorNum;
          iscale = ScaleToAxis(Axis, val);
          ix = NINT((double)iscale * cos0) + xorigin;
          iy = NINT((double)iscale * sin0) + yorigin;
          gdev_line(ix, iy, ix + ixt, iy + iyt);
	}
/* Compute location of big tick: */
      val = tic + Axis.MajorInc;
    }

/* Grid should not be superimposed on the limits of the box: */
    if(val >= Axis.DrawMax) break;

/*** Draw big tick (regular spacing for log or linear scales): ***********/
    iscale = ScaleToAxis(Axis, val);
    ix = NINT((double)iscale * cos0) + xorigin;
    iy = NINT((double)iscale * sin0) + yorigin;
    gdev_line(ix, iy, ix + ixt, iy + iyt);

    majorval = val;
  }

/*********** Restore old settings **************/
 strcpy(buffer, "Default");
 SetPColor(buffer);
 if(line_options) {
    ltype = 0;
    lweight = 0;
    SetLineWidthAndType(ltype, lweight);
   }

return (0);
}
