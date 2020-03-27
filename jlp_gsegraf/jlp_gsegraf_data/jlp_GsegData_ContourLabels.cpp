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
*  icontourplot : number of the contour plot (from 1 to ncontour_plots)
*  x_user, y_user: user coordinates of the location of the mouse
*
* OUTPUT:
*  zcontour_data : z value (intensity in user coordinates)
*  labelled_contour_value : value of the nearest contour
*************************************************************************/
int JLP_GsegData::CreateContourLabel(int icontourplot0, double x_user, 
                                     double y_user, double *zcontour_data,
                                     const double zmin0, const double zmax0,
                                     double *labelled_contour_value) 
{
int i, iplot, icontourplot1, ncontour_plots1, nzvalues, nc, nx, ny;
double xdata_min, xdata_max, ydata_min, ydata_max; 
double diff_min, diff, contour, zz;

/* Initialize output values */
 *zcontour_data = -1.;
 *labelled_contour_value = -1.;

// Get the number of contour plots:
Get_ncontour_plots(&ncontour_plots1);

if(icontourplot0 < 1 || icontourplot0 > ncontour_plots1) {
  fprintf(stderr, "CreateContourLabel/Error: icontourplot0=%d ncontour_plots=%d\n",
           icontourplot0, ncontour_plots1);
  }

// Determine the iplot index corresponding 
// to icontourplot (from 1 to ncontour_plots)
iplot = 0;
icontourplot1 = 0;
 for(i = 1; i <= nplots1; i++) {
// 3="contour"
   if(gseg_plotdata1[i].gseg_plot_type == 3) { 
// Increase at each occurrence of a countour plot:
     icontourplot1++;
     if(icontourplot1 == icontourplot0) {
        iplot = i;
        break;
        }
     }
   }

 if(iplot == 0) {
  fprintf(stderr, "CreateContourLabel/no countours #%d found !\n", 
          icontourplot0);
  return(-1);
  }

/* Get the contour x, y minimum and maximum values */
 nx = gseg_plotdata1[iplot].nxcontour;
 ny = gseg_plotdata1[iplot].nycontour;
 xdata_min = gseg_plotdata1[iplot].xcontour[0];
 xdata_max = gseg_plotdata1[iplot].xcontour[nx-1];
 ydata_min = gseg_plotdata1[iplot].ycontour[0];
 ydata_max = gseg_plotdata1[iplot].ycontour[ny-1];

// Check if level belongs to the contour range
 if((x_user < xdata_min) || (x_user > xdata_max) ||
    (y_user < ydata_min) || (y_user > ydata_max)) return(-1);

/* Interpolate z-axis data */
 interp2(nx, ny, 1, gseg_plotdata1[iplot].xcontour, 
         gseg_plotdata1[iplot].ycontour, gseg_plotdata1[iplot].zcontour, 
         &x_user, &y_user, &zz);
 *zcontour_data = zz;

/* Get number of contour lines */
 jlp_gsegraf1->Get_nz_values(&nzvalues);
 if(gseg_plotdata1[iplot].ncontours < 2)
  nc = 2*nzvalues - 1;
 else
  nc = gseg_plotdata1[iplot].ncontours;

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
