/*******************************************************************************
*
* jlp_Gsegraf_PlotNormal3d.cpp
*
* Plots 3-dimensional mesh data for polygons that are not truncated.
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
* Version 20/04/2017
*******************************************************************************/
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_data.h"     // JLP_GsegData class

/******************************************************************************
* Plots 3-dimensional mesh data for polygons that are not truncated.
*
*******************************************************************************/
void JLP_Gsegraf::PlotNormal3d ( int iplot, 
                    double xmin, double ymin, double zmin, double zmax,
                    double xscale, double yscale, double zscale,
                    double *origin, double *Ryz, UINT32 *fill_color,
                    double *xpoints, double *ypoints, double *zpoints )
{
/* Declare variables */
int i, j, ifill, gseg_plot_type0, style_flag0;
UINT32 color_polygon, alpha_color0, mesh_color0;
double *p, cross_prod, r[4][3]; 
double zmin_polygon, zmax_polygon, zavg_polygon, fraction;
JLP_CanvasPoints *points_polygon, *points_line;

/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
jlp_gseg_data1->GetGsegPlotType(iplot, &gseg_plot_type0);
jlp_gseg_data1->GetStyleFlag(iplot, &style_flag0);
jlp_gseg_data1->GetAlphaColor(iplot, &alpha_color0);
// Warning: no lines in polygons if mesh_color0 == 0 ...
jlp_gseg_data1->GetMeshColor(iplot, &mesh_color0);
/* DEBUG
printf("plot_type=%d style_flag=%d initial mesh_color=%d (255=0x0000FF is black)\n", 
       gseg_plot_type0, style_flag0, mesh_color0);
*/

/* Specify polygon position vectors */
 for ( i=1; i<=4; i++ )
    {
    r[i-1][0] = (xpoints[i-1] - xmin)*xscale;
    r[i-1][1] = (ypoints[i-1] - ymin)*yscale;
    r[i-1][2] = (zpoints[i-1] - zmin)*zscale;
    }


/* Rotate polygon position vectors */
 for ( i=1; i<=4; i++ )
    {
    p = multiply_mv(Ryz, &r[i-1][0]);
    for ( j=1; j<=3; j++, p++ )
       r[i-1][j-1] = *p;
    }


/* Specify points structure components */
 points_polygon = jlp_canvas_points_new(4);
 for ( i=1; i<=4; i++ )
    {
    points_polygon->coords[2*i-2] = origin[1] + r[i-1][1];
    points_polygon->coords[2*i-1] = origin[2] - r[i-1][2];
    }


/* Calculate line perpendicular to polygon */
cross_prod = (r[2][1] - r[0][1])*(r[3][2] - r[1][2]) 
              - (r[2][2] - r[0][2])*(r[3][1] - r[1][1]);

/* Draw polygon */
// 5="mesh"
 if (gseg_plot_type0 == 5) 
    {
    if (style_flag0 == 7 )
       {
       zmin_polygon = zmax;
       zmax_polygon = zmin;
       for ( i=1; i<=4; i++ )
          {
          if ( zpoints[i-1] < zmin_polygon )
             zmin_polygon = zpoints[i-1];
          if ( zpoints[i-1] > zmax_polygon )
             zmax_polygon = zpoints[i-1];
          }
       zavg_polygon = (zmin_polygon + zmax_polygon)/2.0;

       fraction = (zavg_polygon - zmin)/(zmax - zmin);
       if ( cross_prod >= 0.0 )
           color_polygon = interp_color_spectrum(fraction, n_color_spectrum_1,
                                                  color_spectrum_1);
         else
            color_polygon = interp_color_spectrum(fraction, n_color_spectrum_2,
                                                  color_spectrum_2);
       color_polygon = color_polygon - 0xFF + alpha_color0;

// Draw the polygon, with line_width=1 
       jlp_gseg1->GSEG_DrawPolygon(points_polygon, color_polygon, 
                                   mesh_color0, 1);
       }
    else
       {
       if ( cross_prod >= 0.0 )
          ifill = 0;
       else
          ifill = 1;

// Draw the polygon, with line_width=1 
       jlp_gseg1->GSEG_DrawPolygon(points_polygon, fill_color[ifill], 
                                   mesh_color0, 1);
       }
    }

// 3="contour"
 else if (gseg_plot_type0 == 3) 
    {
    if ( cross_prod >= 0.0 )
       ifill = 0;
    else
       ifill = 1;

// Draw the polygon, with line_width=0
    jlp_gseg1->GSEG_DrawPolygon(points_polygon, fill_color[ifill], 
                     fill_color[ifill], 0);

    points_line = jlp_canvas_points_new(3);
    for ( i=1; i<=3; i++ )
       {
       points_line->coords[2*i-2] = origin[1] + r[i-1][1];
       points_line->coords[2*i-1] = origin[2] - r[i-1][2];
       }

    jlp_gseg1->GSEG_DrawLine(points_line, fill_color[ifill], 1);

    jlp_canvas_points_free(points_line);
    }


/* Free points structure */
jlp_canvas_points_free(points_polygon);

return;
}
