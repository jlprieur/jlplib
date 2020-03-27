/*******************************************************************************
* jlp_gsegraf_UserFromDeviceCoord
*
* JLP
* Version 15/04/2017
*******************************************************************************/
#include <stdio.h>
#include <math.h>
#include "jlp_gsegraf.h" 
#include "jlp_gseg_axes.h"      // JLP_GsegAxes class
#include "jlp_gseg_data.h"      // JLP_GsegData class

#define DEG2RAD (PI/180.0)
#ifndef PI
#define PI 3.14159
#endif

/**********************************************************************
* JLP2017: new version with iplot:
*
* INPUT:
*  xmouse, ymouse: device coordinates
*  iplot0 : plot number 
*
* OUTPUT:
*  x_coords_data : x user coordinates
*  y_coords_data : y user coordinates
*  z_coords_data : z user coordinates
*
**********************************************************************/
int JLP_Gsegraf::GSEG_UserFromDeviceCoord(double xmouse, double ymouse, 
                                          int iplot0,
                                          double *x_coords_data,
                                          double *y_coords_data,
                                          double *z_coords_data) 
{
int status = -1;
int flag_2d, flag_3d, flag_2d_rect, flag_polar, flag_linear, flag_logx;
int flag_logy, flag_loglog, gseg_plot_type0;
int nx, ny, ncoords;
double dev_x1_box, dev_x2_box, dev_y1_box, dev_y2_box,
       xmin, xmax, ymin, ymax, zmin, zmax, rmin, rmax,
       xscale, yscale, zscale, rscale,
       xorigin, yorigin, radius, origin[3], Ry[9], Rz[9], Ryz[9],
       rx, ry;
double xcol1, xcol2, ycol1, ycol2;
double xcont1, xcont2, ycont1, ycont2;
double *xcolor0, *ycolor0, *zcolor0;
double *xcontour0, *ycontour0, *zcontour0;
static double x, y, z, theta, r;

  *x_coords_data = -1234.;
  *y_coords_data = -1234.;
  *z_coords_data = -1234.;

/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
  jlp_gseg_data1->GetGsegPlotType(iplot0, &gseg_plot_type0);

// Get current plot settings
  jlp_gseg_axes1->GetBoxSettings(&dev_x1_box, &dev_x2_box, &dev_y1_box,
                                 &dev_y2_box, &xmin, &xmax, &ymin, &ymax,
                                 &zmin, &zmax, &rmin, &rmax, &xscale, &yscale,
                                 &zscale, &rscale, &xorigin, &yorigin,
                                 &radius, origin, Ry, Rz, Ryz, &ncoords);

  jlp_gseg_axes1->GetAxisTypeFlags(&flag_2d, &flag_3d, &flag_2d_rect, 
                                   &flag_polar, &flag_linear, &flag_logx, 
                                   &flag_logy, &flag_loglog);

/******************** Rectangular plot (i.e. non polar plot) ****************/
  if ( flag_2d_rect == 1 )
  {
/* Get mouse coordinates */
  x = xmin + (xmouse - dev_x1_box) / xscale;
  y = ymin - (ymouse - dev_y2_box) / yscale;
  if ( x < xmin || x > xmax || y < ymin || y > ymax ) {
     return(0);
   } else {
      if ( flag_logx == 1 )
         x = pow(10.0, x);
      else if ( flag_logy == 1 )
         y = pow(10.0, y);
      else if ( flag_loglog == 1 )
         {
         x = pow(10.0, x);
         y = pow(10.0, y);
         }
      *x_coords_data = x;
      *y_coords_data = y;
   }

/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
/* Interpolate z-axis data for color plots */
  if (gseg_plot_type0 == 4 )
  {
    nx = jlp_gseg_data1->NXColor(iplot0);
    ny = jlp_gseg_data1->NYColor(iplot0);
    xcol1 = jlp_gseg_data1->XColor(iplot0, 0);
    xcol2 = jlp_gseg_data1->XColor(iplot0, nx -1);
    ycol1 = jlp_gseg_data1->YColor(iplot0, 0);
    ycol2 = jlp_gseg_data1->YColor(iplot0, ny -1);
    xcolor0 = jlp_gseg_data1->XColorPtr(iplot0);
    ycolor0 = jlp_gseg_data1->YColorPtr(iplot0);
    zcolor0 = jlp_gseg_data1->ZColorPtr(iplot0);
      if(( xcol1 <= x) && (x <= xcol2) && (ycol1 <= y) && (y <= ycol2))
         {
         interp2(nx, ny, 1, xcolor0, ycolor0, zcolor0, &x, &y, &z);
         *z_coords_data = z;
         status = 0;
         }
      }

/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
/* Interpolate z-axis data for contour plots */
   else if (gseg_plot_type0 == 3 )
   {
    nx = jlp_gseg_data1->NXContour(iplot0);
    ny = jlp_gseg_data1->NYContour(iplot0);
    xcont1 = jlp_gseg_data1->XContour(iplot0, 0);
    xcont2 = jlp_gseg_data1->XContour(iplot0, nx -1);
    ycont1 = jlp_gseg_data1->YContour(iplot0, 0);
    ycont2 = jlp_gseg_data1->YContour(iplot0, ny -1);
    xcontour0 = jlp_gseg_data1->XContourPtr(iplot0);
    ycontour0 = jlp_gseg_data1->YContourPtr(iplot0);
    zcontour0 = jlp_gseg_data1->ZContourPtr(iplot0);
    if ( xcont1 <= x && x <= xcont2 && ycont1 <= y && y <= ycont2 )
      {
      interp2(nx, ny, 1, xcontour0, ycontour0, zcontour0, &x, &y, &z);
      *z_coords_data = z;
      status = 0;
      }
    }
 } 
/*************** Polar plot: ****************************************/
 else if ( flag_polar == 1 )
 {
/* Get mouse coordinates */
  rx = xmouse - xorigin;
  ry = ymouse - yorigin;
  r = rmin + sqrt(rx*rx + ry*ry)/rscale;
  if ( r > rmax )
     return(-1);
  if ( rx == 0.0 && ry == 0.0 )
     theta = 0.0;
  else if ( rx > 0.0 && ry == 0.0 )
     theta = 0.0;
  else
     theta = atan2(-ry, rx)/DEG2RAD;
  if ( theta < 0.0 )
     theta = theta + 360.0;
  *x_coords_data = theta;
  *y_coords_data = r;
  status = 0;
 }

return(status);
}
