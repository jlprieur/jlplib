/*******************************************************************************
* jlp_GsegData_ContourLabels.cpp 
*
* JLP
* Version 01/04/2017
*******************************************************************************/
#include <stdio.h>
#include <math.h>

#include "jlp_gseg_data.h"
#include "jlp_gseg_axes.h"    // JLP_GsegAxes class

/************************************************************************
*
* INPUT:
*  x_user, y_user: user coordinates of the location of the mouse
*
* OUTPUT:
*  zcontour_data : z value (intensity in user coordinates)
*  labelled_contour_value : value of the nearest contour
*************************************************************************/
int JLP_GsegData::CreateContourLabel(double x_user, double y_user, 
                                     int icontour_plots, double *zcontour_data,
                                     const double zmin0, const double zmax0,
                                     double *labelled_contour_value) 
{
int i, nxvalues, nyvalues, nzvalues, in_frame;
int nc, nx, ny, xindex, yindex, zindex;
double xdata_min, xdata_max, ydata_min, ydata_max; 
double diff_min, diff, contour, zz;

/* Initialize output values */
 *zcontour_data = -1.;
 *labelled_contour_value = -1.;

/* Get data indices */
 xindex = 0;
 yindex = 0;
 zindex = 0;
 nx = 0;
 ny = 0;
 for(i = 1; i <= icontour_plots; i++) {
   xindex = xindex + nx;
   yindex = yindex + ny;
   zindex = zindex + nx*ny;
   nx = nxcontour[i-1];
   ny = nycontour[i-1];
   }

/* Get the contour x, y minimum and maximum values */
 xdata_min = xcontour[xindex];
 xdata_max = xcontour[xindex+nx-1];
 ydata_min = ycontour[yindex];
 ydata_max = ycontour[yindex+ny-1];

// Check if level belongs to the contour range
 if((x_user < xdata_min) || (x_user > xdata_max) ||
    (y_user < ydata_min) || (y_user > ydata_max)) return(-1);

/* Interpolate z-axis data */
 interp2(nx, ny, 1, &xcontour[xindex], &ycontour[yindex], 
         &zcontour[zindex], &x_user, &y_user, &zz);
 *zcontour_data = zz;

/* Get number of contour lines */
 if(ncontours1 < 2)
  nc = 2*nzvalues - 1;
 else
  nc = ncontours1;

/* Calculate contour-line value */
 diff_min = DBLE_MAX_VALUE;
 for(i = 1; i <= nc; i++ )
   {
    contour = zmin0 + (i - 1)*(zmax0 - zmin0)/(nc - 1);
    diff = fabs(zz - contour);
    if(diff < diff_min )
      {
      diff_min = diff;
// Nearest contour value is then:
      *labelled_contour_value = contour;
      }
    }

return(0);
}
