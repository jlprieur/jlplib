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
*
* INPUT:
*  xmouse, ymouse: device coordinates
*  plot_type_data0: plot type information (>0 if color plot, <0 if contour)
*
* OUTPUT:
*  x_coords_data : x user coordinates
*  y_coords_data : y user coordinates
*  z_coords_data : z user coordinates
*
**********************************************************************/
int JLP_Gsegraf::GSEG_UserFromDeviceCoord(double xmouse, double ymouse, 
                                           int plot_type_data0,
                                           double *x_coords_data,
                                           double *y_coords_data,
                                           double *z_coords_data) 
{
int status = -1;
int flag_2d, flag_3d, flag_2d_rect, flag_polar, flag_linear, flag_logx;
int flag_logy, flag_loglog;
int i, nxvalues, nyvalues, nx, ny, xindex, yindex, zindex;
int icolor_plots0, icontour_plots0, ncoords;
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

// Get current plot settings
  jlp_gseg_axes1->GetBoxSettings(&dev_x1_box, &dev_x2_box, &dev_y1_box,
                                 &dev_y2_box, &xmin, &xmax, &ymin, &ymax,
                                 &zmin, &zmax, &rmin, &rmax, &xscale, &yscale,
                                 &zscale, &rscale, &xorigin, &yorigin,
                                 &radius, origin, Ry, Rz, Ryz, &ncoords);

  jlp_gseg_axes1->GetAxisTypeFlags(&flag_2d, &flag_3d, &flag_2d_rect, 
                                   &flag_polar, &flag_linear, &flag_logx, 
                                   &flag_logy, &flag_loglog);

/* Get mouse coordinates */
  if ( flag_2d_rect == 1 )
  {
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

/* Interpolate z-axis data for color plots */
  if ( plot_type_data0 > 0 )
  {
  xindex = 0;
  yindex = 0;
  zindex = 0;
  nx = 0;
  ny = 0;
  icolor_plots0 = plot_type_data0;
  for ( i=1; i<=icolor_plots0; i++ )
         {
         xindex = xindex + nx;
         yindex = yindex + ny;
         zindex = zindex + nx*ny;
         nx = jlp_gseg_data1->NXColor(i-1);
         ny = jlp_gseg_data1->NYColor(i-1);
         }
      xcol1 = jlp_gseg_data1->XColor(xindex);
      xcol2 = jlp_gseg_data1->XColor(xindex + nx -1);
      ycol1 = jlp_gseg_data1->YColor(yindex);
      ycol2 = jlp_gseg_data1->YColor(yindex + ny - 1);
      xcolor0 = jlp_gseg_data1->XColorPtr(xindex);
      ycolor0 = jlp_gseg_data1->YColorPtr(yindex);
      zcolor0 = jlp_gseg_data1->ZColorPtr(zindex);
      if(( xcol1 <= x) && (x <= xcol2) && (ycol1 <= y) && (y <= ycol2))
         {
         interp2(nx, ny, 1, xcolor0, ycolor0, zcolor0, &x, &y, &z);
         *z_coords_data = z;
         status = 0;
         }
      return(status);
      }

   /* Interpolate z-axis data for contour plots */
      else if ( plot_type_data0 < 0 )
      {
      xindex = 0;
      yindex = 0;
      zindex = 0;
      nx = 0;
      ny = 0;
      icontour_plots0 = -plot_type_data0;
      for ( i=1; i<=icontour_plots0; i++ )
         {
         xindex = xindex + nx;
         yindex = yindex + ny;
         zindex = zindex + nx*ny;
         nx = jlp_gseg_data1->NXContour(i-1);
         ny = jlp_gseg_data1->NYContour(i-1);
         }
      xcont1 = jlp_gseg_data1->XContour(xindex);
      xcont2 = jlp_gseg_data1->XContour(xindex + nx -1);
      ycont1 = jlp_gseg_data1->YContour(yindex);
      ycont2 = jlp_gseg_data1->YContour(yindex + ny - 1);
      xcontour0 = jlp_gseg_data1->XContourPtr(xindex);
      ycontour0 = jlp_gseg_data1->YContourPtr(yindex);
      zcontour0 = jlp_gseg_data1->ZContourPtr(zindex);
      if ( xcont1 <= x && x <= xcont2 && ycont1 <= y && y <= ycont2 )
         {
         interp2(nx, ny, 1, xcontour0, ycontour0, zcontour0, &x, &y, &z);
         *z_coords_data = z;
         status = 0;
         }
      return(status);
      }
   }

else if ( flag_polar == 1 )
   {
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
   return(0);
   }

return(0);
}
/**********************************************************************
* Determine the rank of the plot type data of the plot (color or contour plots)
*
**********************************************************************/
int JLP_Gsegraf::GSEG_plot_type_data(int *plot_type_data_value, 
                                     unsigned int iplot0)
{
unsigned int index_plot_types;
char plot_types0[64];
int iplot, nplots, *plot_type_data, icount_color, icount_contour;

/* Get number of color or contour plots */
 GSEG_get_nplots(&nplots);

 plot_type_data = new int[nplots];
 icount_color = 0;
 icount_contour = 0;
  for ( iplot=1; iplot<=nplots; iplot++ )
     {
     index_plot_types = 10*(iplot - 1);
     jlp_gseg_data1->PlotTypes(plot_types0, index_plot_types);
     if ( strcmp(plot_types0, "color") == 0 )
        {
        icount_color++;
        plot_type_data[iplot-1] = icount_color;
        }
     else if ( strcmp(plot_types0, "contour") == 0 )
        {
        icount_contour++;
        plot_type_data[iplot-1] = -icount_contour;
        }
     else
        plot_type_data[iplot-1] = 0;
     }


*plot_type_data_value = plot_type_data[iplot0 - 1];

delete[] plot_type_data;

return(0);
}
