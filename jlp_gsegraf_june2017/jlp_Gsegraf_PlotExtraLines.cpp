/*******************************************************************************
* PlotExtraLines.cpp
*
* Plots extra lines from the list contained in JLP_GsegData object.
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
* Version 09/05/2017
*******************************************************************************/
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"         // JLP_GsegAxes class
#include "jlp_gseg_data.h"         // JLP_GsegData class

/******************************************************************************
* Plots extra lines from the list contained in JLP_GsegData object.
*
*******************************************************************************/
void JLP_Gsegraf::PlotExtraLines()
{
/* Declare variables */
int i, ncoords, iline, nlines, status;
unsigned int line_width;
UINT32 line_color;
double dev_x1_box, dev_x2_box, dev_y1_box, dev_y2_box,
       xmin, xmax, ymin, ymax, zmin, zmax, rmin, rmax,
       xscale, yscale, zscale, rscale, xorigin, yorigin, radius,
       *ppp, origin[3], Ry[9], Rz[9], Ryz[9], r1[3], r2[3], 
       line_coords[6] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
char line_char, color_char, axis_type0[64];
JLP_CanvasPoints *points;

// Get plotbox axis type
jlp_gseg_axes1->GetAxisType(axis_type0);

/* Get box settings */
jlp_gseg_axes1->GetBoxSettings(&dev_x1_box, &dev_x2_box, &dev_y1_box,
                               &dev_y2_box, &xmin, &xmax, &ymin, &ymax, 
                               &zmin, &zmax, &rmin, &rmax, &xscale, &yscale,
                               &zscale, &rscale, &xorigin, &yorigin, 
                               &radius, origin, Ry, Rz, Ryz, &ncoords);

/* Draw lines */
 nlines = jlp_gseg_data1->NExtraLines();
 for(iline = 0; iline < nlines; iline++) {

// Get data for plotting line of index iline
   status = jlp_gseg_data1->GetExtraLineData(line_coords, &line_color, 
                                             &line_char, &line_width, iline);
   if(status) {
     fprintf(stderr, "PlotExtraLines/Error getting line data for iline=%d\n", 
             iline);
     return;
     }

/* Modify line coordinates for logarithmic and polar axes */
     if ( strcmp(axis_type0, "semilogx") == 0 )
         for ( i=1; i<=3; i=i+2 )
            line_coords[i-1] = log10(fabs(line_coords[i-1]));

         else if ( strcmp(axis_type0, "semilogy") == 0 )
            for ( i=2; i<=4; i=i+2 )
               line_coords[i-1] = log10(fabs(line_coords[i-1]));

         else if ( strcmp(axis_type0, "loglog") == 0 )
            for ( i=1; i<=4; i++ )
               line_coords[i-1] = log10(fabs(line_coords[i-1]));

         else if ( strcmp(axis_type0, "polar") == 0 )
            for ( i=1; i<=3; i=i+2 )
               line_coords[i-1] = line_coords[i-1] * DEGTORAD;

/* Draw line */
      if ( line_char == 'l' )
         {
         if ( strcmp(axis_type0, "linear")    == 0 ||
              strcmp(axis_type0, "semilogx")  == 0 ||
              strcmp(axis_type0, "semilogy")  == 0 ||
              strcmp(axis_type0, "loglog")    == 0 )
            {
            if ( Clip2d(xmin, xmax, ymin, ymax, &line_coords[0]) == 1 )
               {
               points = jlp_canvas_points_new(2);
               points->coords[0] = dev_x1_box + (line_coords[0] - xmin)*xscale;
               points->coords[1] = dev_y2_box - (line_coords[1] - ymin)*yscale;
               points->coords[2] = dev_x1_box + (line_coords[2] - xmin)*xscale;
               points->coords[3] = dev_y2_box - (line_coords[3] - ymin)*yscale;
               jlp_gseg1->GSEG_DrawLine(points, line_color, line_width);
               jlp_canvas_points_free(points);
               }
            }

         else if ( strcmp(axis_type0, "polar") == 0 )
            {
            if ( ClipPolar(rmin, rmax, &line_coords[0]) == 1 )
               {
               points = jlp_canvas_points_new(2);
               points->coords[0] = xorigin + (line_coords[1] - rmin)
                                              *cos(line_coords[0])*rscale;
               points->coords[1] = yorigin - (line_coords[1] - rmin)
                                              *sin(line_coords[0])*rscale;
               points->coords[2] = xorigin + (line_coords[3] - rmin)
                                              *cos(line_coords[2])*rscale;
               points->coords[3] = yorigin - (line_coords[3] - rmin)
                                              *sin(line_coords[2])*rscale;
               jlp_gseg1->GSEG_DrawLine(points, line_color, line_width);
               jlp_canvas_points_free(points);
               }
            }

         else if ( strcmp(axis_type0, "3d") == 0 )
            {
            if ( Clip3d(xmin, xmax, ymin, ymax, zmin, zmax, &line_coords[0]) == 1 )
               {
               r1[0] = (line_coords[0] - xmin)*xscale;
               r1[1] = (line_coords[1] - ymin)*yscale;
               r1[2] = (line_coords[2] - zmin)*zscale;

               r2[0] = (line_coords[3] - xmin)*xscale;
               r2[1] = (line_coords[4] - ymin)*yscale;
               r2[2] = (line_coords[5] - zmin)*zscale;

// Matrix multiplication with a vector:
               ppp = multiply_mv(Ryz, r1);
               for ( i=1; i<=3; i++, ppp++ )
                  r1[i-1] = *ppp;

// Matrix multiplication with a vector:
               ppp = multiply_mv(Ryz, r2);
               for ( i=1; i<=3; i++, ppp++ )
                  r2[i-1] = *ppp;

               points = jlp_canvas_points_new(2);
               points->coords[0] = origin[1] + r1[1];
               points->coords[1] = origin[2] - r1[2];
               points->coords[2] = origin[1] + r2[1];
               points->coords[3] = origin[2] - r2[2];
               jlp_gseg1->GSEG_DrawLine(points, line_color, line_width);
               jlp_canvas_points_free(points);
               }
            }
         }

      /* Draw dashed line */
      else if ( line_char == 'd' )
         {
         if ( strcmp(axis_type0, "linear")    == 0 ||
              strcmp(axis_type0, "semilogx")  == 0 ||
              strcmp(axis_type0, "semilogy")  == 0 ||
              strcmp(axis_type0, "loglog")    == 0 )
            {
            if ( Clip2d(xmin, xmax, ymin, ymax, &line_coords[0]) == 1 )
               {
               points = jlp_canvas_points_new(2);
               points->coords[0] = dev_x1_box + (line_coords[0] - xmin)*xscale;
               points->coords[1] = dev_y2_box - (line_coords[1] - ymin)*yscale;
               points->coords[2] = dev_x1_box + (line_coords[2] - xmin)*xscale;
               points->coords[3] = dev_y2_box - (line_coords[3] - ymin)*yscale;
               DrawDashedLine(points, line_color, line_width);
               jlp_canvas_points_free(points);
               }
            }

         else if ( strcmp(axis_type0, "polar") == 0 )
            {
            if ( ClipPolar(rmin, rmax, &line_coords[0]) == 1 )
               {
               points = jlp_canvas_points_new(2);
               points->coords[0] = xorigin + (line_coords[1] - rmin)
                                              *cos(line_coords[0])*rscale;
               points->coords[1] = yorigin - (line_coords[1] - rmin)
                                              *sin(line_coords[0])*rscale;
               points->coords[2] = xorigin + (line_coords[3] - rmin)
                                              *cos(line_coords[2])*rscale;
               points->coords[3] = yorigin - (line_coords[3] - rmin)
                                              *sin(line_coords[2])*rscale;
               DrawDashedLine(points, line_color, line_width);
               jlp_canvas_points_free(points);
               }
            }

         else if ( strcmp(axis_type0, "3d") == 0 )
            {
            if ( Clip3d(xmin, xmax, ymin, ymax, zmin, zmax, &line_coords[0]) == 1 )
               {
               r1[0] = (line_coords[0] - xmin)*xscale;
               r1[1] = (line_coords[1] - ymin)*yscale;
               r1[2] = (line_coords[2] - zmin)*zscale;

               r2[0] = (line_coords[3] - xmin)*xscale;
               r2[1] = (line_coords[4] - ymin)*yscale;
               r2[2] = (line_coords[5] - zmin)*zscale;

// Matrix multiplication with a vector:
               ppp = multiply_mv(Ryz, r1);
               for ( i=1; i<=3; i++, ppp++ )
                  r1[i-1] = *ppp;

// Matrix multiplication with a vector:
                  ppp = multiply_mv(Ryz, r2);
               for ( i=1; i<=3; i++, ppp++ )
                  r2[i-1] = *ppp;

               points = jlp_canvas_points_new(2);
               points->coords[0] = origin[1] + r1[1];
               points->coords[1] = origin[2] - r1[2];
               points->coords[2] = origin[1] + r2[1];
               points->coords[3] = origin[2] - r2[2];
               DrawDashedLine(points, line_color, line_width);
               jlp_canvas_points_free(points);
               }
            }
         }

      else if ( line_char == '.' )
         {
         if ( strcmp(axis_type0, "linear")    == 0 ||
              strcmp(axis_type0, "semilogx")  == 0 ||
              strcmp(axis_type0, "semilogy")  == 0 ||
              strcmp(axis_type0, "loglog")    == 0 )
            {
            if ( Clip2d(xmin, xmax, ymin, ymax, &line_coords[0]) == 1 )
               {
               points = jlp_canvas_points_new(2);
               points->coords[0] = dev_x1_box + (line_coords[0] - xmin)*xscale;
               points->coords[1] = dev_y2_box - (line_coords[1] - ymin)*yscale;
               points->coords[2] = dev_x1_box + (line_coords[2] - xmin)*xscale;
               points->coords[3] = dev_y2_box - (line_coords[3] - ymin)*yscale;
               DrawDottedLine(points, line_color, line_width);
               jlp_canvas_points_free(points);
               }
            }

         else if ( strcmp(axis_type0, "polar") == 0 )
            {
            if ( ClipPolar(rmin, rmax, &line_coords[0]) == 1 )
               {
               points = jlp_canvas_points_new(2);
               points->coords[0] = xorigin + (line_coords[1] - rmin)*cos(line_coords[0])*rscale;
               points->coords[1] = yorigin - (line_coords[1] - rmin)*sin(line_coords[0])*rscale;
               points->coords[2] = xorigin + (line_coords[3] - rmin)*cos(line_coords[2])*rscale;
               points->coords[3] = yorigin - (line_coords[3] - rmin)*sin(line_coords[2])*rscale;
               DrawDottedLine(points, line_color, line_width);
               jlp_canvas_points_free(points);
               }
            }

         else if ( strcmp(axis_type0, "3d") == 0 )
            {
            if ( Clip3d(xmin, xmax, ymin, ymax, zmin, zmax, &line_coords[0]) == 1 )
               {
               r1[0] = (line_coords[0] - xmin)*xscale;
               r1[1] = (line_coords[1] - ymin)*yscale;
               r1[2] = (line_coords[2] - zmin)*zscale;

               r2[0] = (line_coords[3] - xmin)*xscale;
               r2[1] = (line_coords[4] - ymin)*yscale;
               r2[2] = (line_coords[5] - zmin)*zscale;

// Matrix multiplication with a vector:
               ppp = multiply_mv(Ryz, r1);
               for ( i=1; i<=3; i++, ppp++ )
                  r1[i-1] = *ppp;

// Matrix multiplication with a vector:
               ppp = multiply_mv(Ryz, r2);
               for ( i=1; i<=3; i++, ppp++ )
                  r2[i-1] = *ppp;

               points = jlp_canvas_points_new(2);
               points->coords[0] = origin[1] + r1[1];
               points->coords[1] = origin[2] - r1[2];
               points->coords[2] = origin[1] + r2[1];
               points->coords[3] = origin[2] - r2[2];
               DrawDottedLine(points, line_color, line_width);
               jlp_canvas_points_free(points);
               }
            }
         }

   } // EOF loop on iline

return;
}
