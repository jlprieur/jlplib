/*******************************************************************************
*
* jlp_Gsegraf_ColorPlot2d.cpp
*
* Plots two-dimensional contour information as a function of color.
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
* Version 05/07/2017
*******************************************************************************/
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"         // JLP_GsegAxes class
#include "jlp_gseg_data.h"         // JLP_GsegData class

// #define DEBUG

/****************************************************************************
*
****************************************************************************/
void JLP_Gsegraf::ColorPlot2d(int iplot, int iplot_color_scale)
{
/* Declare variables */
int i, j, i1, j1, in, jn, width, height, nx, ny, style_flag0;
UINT32 color;
double dev_x1_box, dev_x2_box, dev_y1_box, dev_y2_box; 
double xmin, xmax, ymin, ymax, zmin, zmax,
       xscale, yscale, zscale, x1, x2, y1, y2, fraction,
       dx1, dx2, dy1, dy2, *xi, *yi, *zi, x, y, z;
double xcolor_1, xcolor_2, ycolor_1, ycolor_2, zblack0, zwhite0;
double *xcolor0, *ycolor0, *zcolor0;
char text_anchor0[64];
JLP_DPixbuf pixbuf_color;

 jlp_gseg_data1->GetStyleFlag(iplot, &style_flag0);

/* Get plot box settings */
 jlp_gseg_axes1->GetBoxSettingsForLinear(&dev_x1_box, &dev_x2_box, &dev_y1_box,
                                         &dev_y2_box, &xmin, &xmax, &ymin,
                                         &ymax, &zmin, &zmax, &xscale, &yscale);

/* Draw color-bar pixbuf */
 if ( iplot_color_scale == 1 ) jlp_gseg_axes1->DrawColorScale(1, NULL, 0);

/* Calculate pixbuf width and height */
 dx1 = 0.0;
 dx2 = 0.0;
 dy1 = 0.0;
 dy2 = 0.0;
 nx = jlp_gseg_data1->NXColor(iplot);
 ny = jlp_gseg_data1->NYColor(iplot);
 xcolor_1 = jlp_gseg_data1->XColor(iplot, 0);
 xcolor_2 = jlp_gseg_data1->XColor(iplot, nx - 1);
 ycolor_1 = jlp_gseg_data1->YColor(iplot, 0);
 ycolor_2 = jlp_gseg_data1->YColor(iplot, ny - 1);
 jlp_gseg_data1->GetZBlackWhite(iplot, &zblack0, &zwhite0);

#ifdef DEBUG
printf("ColorPlot2d/Debug: nx=%d ny=%d xcolormin,max=%f %f, ycolormin,max=%f %f\n style_flag=%d zblack=%f zwhite=%f\n", 
        nx, ny, xcolor_1, xcolor_2, ycolor_1, ycolor_2, style_flag0, 
        zblack0, zwhite0);
#endif

 if ( xmin >= xcolor_1 )
    x1 = xmin;
 else
    {
    x1 = xcolor_1;
    dx1 = xcolor_1 - xmin;
    }

 if ( xmax <= xcolor_2 )
    x2 = xmax;
 else
    {
    x2 = xcolor_2;
    dx2 = xmax - xcolor_2;
    }

 if ( ymin >= ycolor_1 )
    y1 = ymin;
 else
    {
    y1 = ycolor_1;
    dy1 = ycolor_1 - ymin;
    }

 if ( ymax <= ycolor_2 )
    y2 = ymax;
 else
    {
    y2 = ycolor_2;
    dy2 = ymax - ycolor_2;
    }

 width  = roundint(dev_x2_box - dev_x1_box - (dx1 + dx2)*xscale);
 height = roundint(dev_y2_box - dev_y1_box - (dy1 + dy2)*yscale);

/* Check pixbuf width and height */
 if ( width <= 0 || height <= 0 ) { 
   fprintf(stderr, "ColorPlot2d/Error: width=%d height=%d\n", width, height);
   return;
   }

/* Get interpolated values of x and y */
 xi = new double[width];
 yi = new double[height];
 zi = new double[width*height];
 for ( i=1; i<=width; i++ )
    xi[i-1] = x1 + (i - 1)*(x2 - x1)/(width - 1);
 for ( j=1; j<=height; j++ )
    yi[j-1] = y1 + (j - 1)*(y2 - y1)/(height - 1);

 xcolor0 = jlp_gseg_data1->XColorPtr(iplot);
 ycolor0 = jlp_gseg_data1->YColorPtr(iplot);
 zcolor0 = jlp_gseg_data1->ZColorPtr(iplot);

/* Get interpolated values of z (bilinear interpolation) */
// WARNING: swap X/Y here ... height*(i-1)+(j-1) !!!
 if (style_flag0 == 8 ) {
   for ( i=1; i<=width; i++ )
      for ( j=1; j<=height; j++ )
         interp2(nx, ny, 1, xcolor0, ycolor0, zcolor0, &xi[i-1], &yi[j-1], 
                  &zi[height*(i-1)+(j-1)]);
   }
/* Get interpolated values of z (nearest-neighbor interpolation) */
 else if ( style_flag0 == 9 ) {
    for ( i=1; i<=width; i++ )
       {
       if ( xi[i-1] <= xcolor_1 )
          i1 = 0;
       else if ( xi[i-1] >= xcolor_2 )
          i1 = nx - 2;
       else
          i1 = find_indices(0, nx-1, xcolor0, xi[i-1]);

       for ( j=1; j<=height; j++ )
          {
          if ( yi[j-1] <= ycolor_1 )
             j1 = 0;
          else if ( yi[j-1] >= ycolor_2 )
             j1 = ny - 2;
          else
             j1 = find_indices(0, ny-1, ycolor0, yi[j-1]);

          if ( fabs(xi[i-1] - jlp_gseg_data1->XColor(iplot, i1)) 
                        < fabs(xi[i-1] - jlp_gseg_data1->XColor(iplot, i1+1)))
             in = i1;
          else
             in = i1 + 1;

          if ( fabs(yi[j-1] - jlp_gseg_data1->YColor(iplot, j1)) 
                         < fabs(yi[j-1] - jlp_gseg_data1->YColor(iplot, j1+1)))
             jn = j1;
          else
             jn = j1 + 1;

          zi[height*(i-1)+(j-1)] = jlp_gseg_data1->ZColor(iplot, ny*in+jn);
          }
       }
   }

/* Allocate memory for the  pixbuf */
 GSEG_NewDPixbuf(&pixbuf_color, width, height);

/* Draw color plot */
 zscale = 1.0/(zmax - zmin);
 for ( i=1; i<=width; i++ )
    for ( j=height; j>=1; j-- )
       {
       z = zi[height*(i-1)+(j-1)];
       fraction = (z - zmin)*zscale;
       if ( fraction < 0.0 )
          fraction = 0.0;
       else if ( fraction > 1.0 )
          fraction = 1.0;
       if ( z < zblack0 )
          color = 0x000000FF;
       else if ( z > zwhite0 )
          color = 0xFFFFFFFF;
       else
          color = interp_color_spectrum(fraction, n_color_spectrum_1,
                                         color_spectrum_1);
       GSEG_PutPixelDPixbuf(&pixbuf_color, i-1, height-j, color);
       }

/* Draw pixbuf on canvas */
 x = (dev_x1_box + dev_x2_box + (dx1 - dx2)*xscale) / 2.0;
 y = (dev_y2_box + dev_y1_box + (dy2 - dy1)*yscale) / 2.0;

 strcpy(text_anchor0, "CENTER");
 jlp_gseg1->GSEG_DrawDPixbuf(&pixbuf_color, x, y, text_anchor0);

/* Free memory */
 GSEG_FreeDPixbuf(&pixbuf_color);

/* Draw grid */
 jlp_gseg_axes1->DrawGrid2d();
 jlp_gseg_axes1->DrawGridLog();

/* Free memory */
 free(xi);
 free(yi);
 free(zi);

 return;
 }
/**************************************************************************
*
**************************************************************************/
void JLP_Gsegraf::DrawColorBarFor2dPlot(const double x1_bar0, 
                                        const double y1_bar0,
                                        const int width0, const int height0)
{
int i, j;
double fraction;
JLP_DPixbuf pixbuf_colorbar;
char text_anchor0[64];
UINT32 color0;

/* Allocate memory for the  pixbuf */
 GSEG_NewDPixbuf(&pixbuf_colorbar, width0, height0);

 for (j = height0; j >= 1; j-- )
   {
   fraction = (double)(j - 1.0) / (height0 - 1.0);
   color0 = interp_color_spectrum(fraction, n_color_spectrum_1,
                                  color_spectrum_1);
   for ( i=1; i<=width0; i++ )
      GSEG_PutPixelDPixbuf(&pixbuf_colorbar, i-1, height0-j, color0);
   }

/* Draw pixbuf on canvas */ 
 strcpy(text_anchor0, "SOUTH_WEST");
 jlp_gseg1->GSEG_DrawDPixbuf(&pixbuf_colorbar, x1_bar0, y1_bar0,
                             text_anchor0);

/* Free memory */
 GSEG_FreeDPixbuf(&pixbuf_colorbar);

return;
}
