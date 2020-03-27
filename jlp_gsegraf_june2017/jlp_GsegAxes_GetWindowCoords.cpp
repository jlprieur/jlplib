/******************************************************************************
* jlp_GsegAxes_GetWindowCoords.cpp
*
* Calculate window coordinates from plot coordinates.
*
* Copyright © 2008, 2009, 2010, 2011 Spencer A. Buckner
* http://savannah.gnu.org/projects/gsegrafix
*
* This file is part of GSEGrafix, a scientific and engineering plotting program.
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
* JLP
* Version 21/04/2017
******************************************************************************/
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"    // JLP_GsegAxes class

void JLP_GsegAxes::GetWindowCoords(double *plot_coords, double *window_coords)
{
/* Declare variables */
 int i, nx, ny, nz, ncoords;
 double dev_x1_box, dev_x2_box, dev_y1_box, dev_y2_box,
        xmin, xmax, ymin, ymax, zmin, zmax, rmin, rmax,
        xscale, yscale, zscale, rscale, xorigin, yorigin, radius, 
        *ppp, origin[3], Ry[9], Rz[9], Ryz[9], r[3];

/* Get box settings */
   GetBoxSettings(&dev_x1_box, &dev_x2_box, &dev_y1_box,
                  &dev_y2_box, &xmin, &xmax, &ymin, &ymax,
                  &zmin, &zmax, &rmin, &rmax, &xscale, &yscale,
                  &zscale, &rscale, &xorigin, &yorigin,
                  &radius, origin, Ry, Rz, Ryz, &ncoords);

/* Modify coordinates for logarithmic axes */
 if ( strcmp(p_plot_param->axis_type, "semilogx") == 0 )
    plot_coords[0] = log10(fabs(plot_coords[0]));

 else if ( strcmp(p_plot_param->axis_type, "semilogy") == 0 )
    plot_coords[1] = log10(fabs(plot_coords[1]));

 else if ( strcmp(p_plot_param->axis_type, "loglog") == 0 )
    {
    plot_coords[0] = log10(fabs(plot_coords[0]));
    plot_coords[1] = log10(fabs(plot_coords[1]));
    }


/* Modify coordinates for polar axes */
 if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
    plot_coords[0] = plot_coords[0]*DEGTORAD;


/* Initialize window coordinates */
 window_coords[0] = -1.0;
 window_coords[1] = -1.0;


/* Calculate plot coordinates */
 if ( strcmp(p_plot_param->axis_type, "linear")   == 0 ||
      strcmp(p_plot_param->axis_type, "semilogx") == 0 ||
      strcmp(p_plot_param->axis_type, "semilogy") == 0 ||
      strcmp(p_plot_param->axis_type, "loglog")   == 0 )
    {
    if ( xmin <= plot_coords[0] && plot_coords[0] <= xmax &&
         ymin <= plot_coords[1] && plot_coords[1] <= ymax )
       {
       window_coords[0] = dev_x1_box + (plot_coords[0] - xmin)*xscale;
       window_coords[1] = dev_y2_box - (plot_coords[1] - ymin)*yscale;
       }
    }

 else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
    {
    if ( rmin <= plot_coords[1] && plot_coords[1] <= rmax )
       {
       window_coords[0] = xorigin + (plot_coords[1] - rmin)
                          * cos(plot_coords[0]) * rscale;
       window_coords[1] = yorigin - (plot_coords[1] - rmin)
                          * sin(plot_coords[0]) * rscale;
         }
    }

 else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
    {
    if ( xmin <= plot_coords[0] && plot_coords[0] <= xmax &&
         ymin <= plot_coords[1] && plot_coords[1] <= ymax &&
         zmin <= plot_coords[2] && plot_coords[2] <= zmax )
       {
       r[0] = (plot_coords[0] - xmin)*xscale;
       r[1] = (plot_coords[1] - ymin)*yscale;
       r[2] = (plot_coords[2] - zmin)*zscale;

       ppp = multiply_mv(Ryz, r);
       for ( i=1; i<=3; i++, ppp++ )
          r[i-1] = *ppp;

       window_coords[0] = origin[1] + r[1];
       window_coords[1] = origin[2] - r[2];
       }
    }

return;
}
/************************************************************************
* Conversion from device to user coordinates 
* Called by JLP_Gsegraf::GSEG_CreateContourLabel.
************************************************************************/
int JLP_GsegAxes::ContourLabel_FromDevToUser(const double x1, const double y1,
                                             double *user_x1, double *user_y1,
                                             int *in_frame)
{
/* Declare variables */
double dev_x1_box, dev_x2_box, dev_y1_box, dev_y2_box, xmin, xmax, ymin, ymax, zmin, zmax;
double xscale, yscale;

*in_frame = 1;
*user_x1 = 0.;
*user_y1 = 0.;

/* Get plot box minimum and maximum values */
 GetBoxSettingsForLinear(&dev_x1_box, &dev_x2_box, &dev_y1_box,               
                         &dev_y2_box, &xmin, &xmax, &ymin,                
                         &ymax, &zmin, &zmax, &xscale, &yscale);

/* Get user coordinates */
 *user_x1 = xmin + (x1 - dev_x1_box) / xscale;
 *user_y1 = ymin - (y1 - dev_y2_box) / yscale;

/* DEBUG
printf("GSEG_CreateContourLabel/xmin=%f dev_x1_box=%f x1=%f dev_x1=%f dev_y1=%f \n",
        xmin, dev_x1_box, x1, *dev_x1, *dev_y1);
*/
 if((*user_x1 < xmin) || (*user_x1 > xmax) || (*user_y1 < ymin)
    || (*user_y1 > ymax) ) *in_frame = 0;

return(0);
}
