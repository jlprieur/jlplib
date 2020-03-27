/*******************************************************************************
* jlp_Gsegraf_PlotInterp3d.c
*
* Plots 3-dimensional mesh data for truncated polygons.
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
#include "jlp_gsegraf.h"
#include "jlp_gseg_data.h"   // JLP_GsegData class

void JLP_Gsegraf:: PlotInterp3d ( int iplot, int imesh,
                     double xmin, double ymin, double zmin, double zmax,
                     double xscale, double yscale, double zscale,
                     double *origin, double *Ryz, UINT32 *fill_color,
                     double *xpoints, double *ypoints, double *zpoints )
{
/* Declare variables */
int i, j, ifill;
/* flag_points = -1: point < zmin 
   flag_points =  0: zmin <= point <= zmax
   flag_points =  1: zmax < point
*/
int flag_points[] = { 0, 0, 0, 0 };  
/* flag_interp_min =  1: interpolation for point < zmin */
int flag_interp_min[] = { 0, 0, 0, 0 };   
/* flag_interp_max =  1: interpolation for point > zmax */
int flag_interp_max[] = { 0, 0, 0, 0 };   
int npoints, ninterp_min, ninterp_max, npolygon, imin, imax;
unsigned int index_plot_types;
double *p, cross_prod;
double xinterp_min[4], yinterp_min[4], zinterp_min[4];
double xinterp_max[4], yinterp_max[4], zinterp_max[4];
double r[6][3], zmin_polygon, zmax_polygon, zavg_polygon, fraction;
UINT32 color_polygon;
JLP_CanvasPoints *points_polygon, *points_line;
char plot_types0[64];

/* Check all points not <= zmin or all points not >= zmax */
   if ( (zpoints[0] <= zmin && zpoints[1] <= zmin  && zpoints[2] <= zmin  && zpoints[3] <= zmin) ||
        (zpoints[0] >= zmax && zpoints[1] >= zmax  && zpoints[2] >= zmax  && zpoints[3] >= zmax) )
      return;


/* Find points less than zmin and greater than zmax */
   for ( i=1; i<=4; i++ )
      {
      if ( zpoints[i-1] < zmin )
         flag_points[i-1] = -1;
      else if ( zpoints[i-1] > zmax )
         flag_points[i-1] =  1;
      }


/* Interpolate between points 0 and 1 */
   if ( (flag_points[0] == -1 && flag_points[1] >= 0) || (flag_points[0] >= 0 && flag_points[1] == -1) )
      {
      flag_interp_min[0] = 1;
      xinterp_min[0] = ((zpoints[1] - zmin)*xpoints[0] + (zmin - zpoints[0])*xpoints[1])/(zpoints[1] - zpoints[0]);
      yinterp_min[0] = ypoints[0];
      zinterp_min[0] = zmin;
      }

   if ( (flag_points[0] == 1 && flag_points[1] <= 0) || (flag_points[0] <= 0 && flag_points[1] == 1) )
      {
      flag_interp_max[0] = 1;
      xinterp_max[0] = ((zpoints[1] - zmax)*xpoints[0] + (zmax - zpoints[0])*xpoints[1])/(zpoints[1] - zpoints[0]);
      yinterp_max[0] = ypoints[0];
      zinterp_max[0] = zmax;
      }


/* Interpolate between points 1 and 2 */
   if ( (flag_points[1] == -1 && flag_points[2] >= 0) || (flag_points[1] >= 0 && flag_points[2] == -1) )
      {
      flag_interp_min[1] = 1;
      xinterp_min[1] = xpoints[1];
      yinterp_min[1] = ((zpoints[2] - zmin)*ypoints[1] + (zmin - zpoints[1])*ypoints[2])/(zpoints[2] - zpoints[1]);
      zinterp_min[1] = zmin;
      }

   if ( (flag_points[1] == 1 && flag_points[2] <= 0) || (flag_points[1] <= 0 && flag_points[2] == 1) )
      {
      flag_interp_max[1] = 1;
      xinterp_max[1] = xpoints[1];
      yinterp_max[1] = ((zpoints[2] - zmax)*ypoints[1] + (zmax - zpoints[1])*ypoints[2])/(zpoints[2] - zpoints[1]);
      zinterp_max[1] = zmax;
      }


/* Interpolate between points 2 and 3 */
   if ( (flag_points[2] == -1 && flag_points[3] >= 0) || (flag_points[2] >= 0 && flag_points[3] == -1) )
      {
      flag_interp_min[2] = 1;
      xinterp_min[2] = ((zpoints[3] - zmin)*xpoints[2] + (zmin - zpoints[2])*xpoints[3])/(zpoints[3] - zpoints[2]);
      yinterp_min[2] = ypoints[2];
      zinterp_min[2] = zmin;
      }

   if ( (flag_points[2] == 1 && flag_points[3] <= 0) || (flag_points[2] <= 0 && flag_points[3] == 1) )
      {
      flag_interp_max[2] = 1;
      xinterp_max[2] = ((zpoints[3] - zmax)*xpoints[2] + (zmax - zpoints[2])*xpoints[3])/(zpoints[3] - zpoints[2]);
      yinterp_max[2] = ypoints[2];
      zinterp_max[2] = zmax;
      }


   /* Interpolate between points 3 and 0 */
   if ( (flag_points[3] == -1 && flag_points[0] >= 0) || (flag_points[3] >= 0 && flag_points[0] == -1) )
      {
      flag_interp_min[3] = 1;
      xinterp_min[3] = xpoints[3];
      yinterp_min[3] = ((zpoints[0] - zmin)*ypoints[3] + (zmin - zpoints[3])*ypoints[0])/(zpoints[0] - zpoints[3]);
      zinterp_min[3] = zmin;
      }

   if ( (flag_points[3] == 1 && flag_points[0] <= 0) || (flag_points[3] <= 0 && flag_points[0] == 1) )
      {
      flag_interp_max[3] = 1;
      xinterp_max[3] = xpoints[3];
      yinterp_max[3] = ((zpoints[0] - zmax)*ypoints[3] + (zmax - zpoints[3])*ypoints[0])/(zpoints[0] - zpoints[3]);
      zinterp_max[3] = zmax;
      }

/* Calculate number of good points and interpolated points */
   npoints = 0;
   ninterp_min = 0;
   ninterp_max = 0;
   for ( i=1; i<=4; i++ )
      {
      /* point i-1 within range */
      if ( flag_points[i-1] == 0 )
         npoints++;

      /* interpolated point between points i-1 and i */
      if ( flag_interp_min[i-1] == 1 )
         ninterp_min++;

      /* interpolated point between points i-1 and i */
      if ( flag_interp_max[i-1] == 1 )
         ninterp_max++;
      }

   npolygon = npoints + ninterp_min + ninterp_max;
   if ( npolygon < 3 )
      return;


   /* Specify polygon position vectors */
   i = 0;
   for ( j=1; j<=4; j++ )
      {
      /* point j-1 within range */
      if ( flag_points[j-1] == 0 )
         {
         i++;
         r[i-1][0] = (xpoints[j-1] - xmin)*xscale;
         r[i-1][1] = (ypoints[j-1] - ymin)*yscale;
         r[i-1][2] = (zpoints[j-1] - zmin)*zscale;
         }

      /* interpolated point between points j-1 and j */
      if ( flag_interp_min[j-1] == 1 )
         {
         i++;
         imin = i;
         r[imin-1][0] = (xinterp_min[j-1] - xmin)*xscale;
         r[imin-1][1] = (yinterp_min[j-1] - ymin)*yscale;
         r[imin-1][2] = (zinterp_min[j-1] - zmin)*zscale;
         }

      /* interpolated point between points j-1 and j */
      if ( flag_interp_max[j-1] == 1 )
         {
         i++;
         imax = i;
         r[imax-1][0] = (xinterp_max[j-1] - xmin)*xscale;
         r[imax-1][1] = (yinterp_max[j-1] - ymin)*yscale;
         r[imax-1][2] = (zinterp_max[j-1] - zmin)*zscale;
         }

/* if two interpolated points between points j-1 and j, switch if necessary */
      if ( flag_interp_min[j-1] == 1 && flag_interp_max[j-1] == 1 )
         if ( (j == 1 && xinterp_min[j-1] > xinterp_max[j-1]) ||
              (j == 2 && yinterp_min[j-1] > yinterp_max[j-1]) ||
              (j == 3 && xinterp_min[j-1] < xinterp_max[j-1]) ||
              (j == 4 && yinterp_min[j-1] < yinterp_max[j-1]) )
            {
            imax--;
            imin++;
            r[imin-1][0] = (xinterp_min[j-1] - xmin)*xscale;
            r[imin-1][1] = (yinterp_min[j-1] - ymin)*yscale;
            r[imin-1][2] = (zinterp_min[j-1] - zmin)*zscale;
            r[imax-1][0] = (xinterp_max[j-1] - xmin)*xscale;
            r[imax-1][1] = (yinterp_max[j-1] - ymin)*yscale;
            r[imax-1][2] = (zinterp_max[j-1] - zmin)*zscale;
            }
      }


/* Find polygon minimum and maximum z coordinates */
   if ( jlp_gseg_data1->StyleFlags(iplot-1) == 7 )
      {
      zmin_polygon = (zmax - zmin)*zscale;
      zmax_polygon = 0.0;
      for ( i=1; i<=npolygon; i++ )
         {
         if ( r[i-1][2] < zmin_polygon )
            zmin_polygon = r[i-1][2];
         if ( r[i-1][2] > zmax_polygon )
            zmax_polygon = r[i-1][2];
         }
      zavg_polygon = (zmin_polygon + zmax_polygon)/2.0;
      zavg_polygon = zavg_polygon/zscale + zmin;
      }


/* Rotate polygon position vectors */
   for ( i=1; i<=npolygon; i++ )
      {
      p = multiply_mv(Ryz, &r[i-1][0]);
      for ( j=1; j<=3; j++, p++ )
         r[i-1][j-1] = *p;
      }


/* Specify points structure components */
   points_polygon = jlp_canvas_points_new(npolygon);
   for ( i=1; i<=npolygon; i++ )
      {
      points_polygon->coords[2*i-2] = origin[1] + r[i-1][1];
      points_polygon->coords[2*i-1] = origin[2] - r[i-1][2];
      }


/* Calculate line perpendicular to polygon */
   if ( npolygon >= 4 )
      cross_prod = (r[2][1] - r[0][1])*(r[3][2] - r[1][2]) - (r[2][2] - r[0][2])*(r[3][1] - r[1][1]);
   else
      cross_prod = (r[1][1] - r[0][1])*(r[2][2] - r[0][2]) - (r[1][2] - r[0][2])*(r[2][1] - r[0][1]);


/* Draw polygon */
   index_plot_types = 10*(iplot - 1);
   jlp_gseg_data1->PlotTypes(plot_types0, index_plot_types); 
   if ( strcmp(plot_types0, "mesh") == 0 )
      {
      if ( jlp_gseg_data1->StyleFlags(iplot-1) == 7 )
         {
         fraction = (zavg_polygon - zmin)/(zmax - zmin);
         if ( cross_prod >= 0.0 )
            color_polygon = interp_color_spectrum(fraction, n_color_spectrum_1,
                                                  color_spectrum_1);
         else
            color_polygon = interp_color_spectrum(fraction, n_color_spectrum_2,
                                                  color_spectrum_2);
         color_polygon = color_polygon - 0xFF + alphacolor[iplot-1];

// Draw the polygon 
         jlp_gseg1->GSEG_DrawPolygon(points_polygon, color_polygon, 
                               meshcolors[imesh-1], 1);
         }
      else
         {
         if ( cross_prod >= 0.0 )
            ifill = 0;
         else
            ifill = 1;

// Draw the polygon 
         jlp_gseg1->GSEG_DrawPolygon(points_polygon, fill_color[ifill], 
                               meshcolors[imesh-1], 1);
         }
      }

   else if ( strcmp(plot_types0, "contour") == 0 )
      {
      if ( cross_prod >= 0.0 )
         ifill = 0;
      else
         ifill = 1;

// Draw the polygon 
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
