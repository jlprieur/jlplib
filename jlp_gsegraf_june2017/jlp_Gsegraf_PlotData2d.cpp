/*******************************************************************************
*
* jlp_Gsegraf_PlotData2d.cpp
*
* Plots data points.
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
* Version 17/04/2017
*******************************************************************************/
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"         // JLP_GsegAxes class
#include "jlp_gseg_data.h"         // JLP_GsegData class

/* Contains:
void PlotData2d (void);
void DrawLineSegments ( int iplot, int index, int npts,
                        double xmin, double xmax, double ymin, double ymax,
                        double xscale, double yscale, int linechar );
*/

/*****************************************************************************
*
*****************************************************************************/
void JLP_Gsegraf::PlotData2d (void)
{
/* Declare variables */
int i, ifunc, iplot, nplots0, npts,
    index_plot_types, index_stemflags, index,
    ipoints, ihist, icontour, icolor,
    nxcontourplot, nycontourplot; 
double xindex_contour, yindex_contour, zindex_contour,
    nxcolorplot, nycolorplot, xindex_color, yindex_color, zindex_color;
double dev_x1_box, dev_x2_box, dev_y1_box, dev_y2_box, xmin, xmax, ymin, ymax,
       zmin, zmax, xscale, yscale, x, y, xdata1, ydata1;
char *pchar, *plot_types0;
JLP_CanvasPoints *points;

/* Get plot box settings */
jlp_gseg_axes1->GetBoxSettingsForLinear(&dev_x1_box, &dev_x2_box, &dev_y1_box,
                                        &dev_y2_box, &xmin, &xmax, &ymin,
                                        &ymax, &zmin, &zmax, &xscale, &yscale);


/* Plot data */
index_plot_types = 0;
index = 0;
ipoints = 0;
ihist = 0;
icontour = 0;
icolor = 0;
xindex_contour = 0;
yindex_contour = 0;
zindex_contour = 0;
xindex_color = 0;
yindex_color = 0;
zindex_color = 0;
jlp_gseg_data1->Get_nplots(&nplots0);
plot_types0 = jlp_gseg_data1->PlotTypesPtr();
for ( iplot = 1; iplot <= nplots0; iplot++ )
   {
   if ( strcmp(&plot_types0[index_plot_types], "points") == 0 )
      {
      ipoints++;
      npts = jlp_gseg_data1->NData(ipoints+ihist-1);

/* Draw stem lines */
      index_stemflags = (iplot - 1)*4;
      if ( strcmp(&stemflags[index_stemflags],  "on") == 0 ||
           strcmp(&stemflags[index_stemflags], "num") == 0 )
         {
         points = jlp_canvas_points_new(2);

/* Calculate y coordinate of stem point 1 */
         if ( strcmp(&stemflags[index_stemflags], "on") == 0 )
            y = dev_y2_box;
         else if ( strcmp(&stemflags[index_stemflags], "num") == 0 )
            {
            if ( ymin <= stemvalues[iplot-1] && stemvalues[iplot-1] <= ymax )
               y = dev_y2_box - (stemvalues[iplot-1] - ymin)*yscale;
            else if ( stemvalues[iplot-1] < ymin )
               y = dev_y2_box;
            else if ( stemvalues[iplot-1] > ymax )
               y = dev_y1_box;
            }
         points->coords[1] = y;

         for ( i=1; i<=npts; i++ )
            xdata1 = jlp_gseg_data1->XData(index + i - 1);
            ydata1 = jlp_gseg_data1->YData(index + i - 1);
            if ( xmin <= xdata1 && xdata1 <= xmax &&
                 ymin <= ydata1 && ydata1 <= ymax )
               {
/* Calculate x coordinate of stem point 1 */
               x = dev_x1_box + (xdata1 - xmin)*xscale;
               points->coords[0] = x;

/* Calculate x and y coordinates of stem point 2 */
               x = dev_x1_box + (xdata1 - xmin)*xscale;
               y = dev_y2_box - (ydata1 - ymin)*yscale;
               points->coords[2] = x;
               points->coords[3] = y;

               jlp_gseg1->GSEG_DrawLine(points, 
                                        outline_colors_rgba1[iplot-1], 1);
               }

         jlp_canvas_points_free(points);
         }

/* Draw lines */
      if ( jlp_gseg_data1->StyleChar1(iplot-1) == 'l' )
         DrawLineSegments(iplot, index, npts, xmin, xmax, ymin, ymax,
                          xscale, yscale, 'l');


/* Draw dashed lines */
      else if ( jlp_gseg_data1->StyleChar1(iplot-1) == 'd' )
         DrawLineSegments(iplot, index, npts, xmin, xmax, ymin, ymax,
                          xscale, yscale, 'd');


/* Draw dotted lines */
      else if ( jlp_gseg_data1->StyleChar1(iplot-1) == '.' )
         DrawLineSegments(iplot, index, npts, xmin, xmax, ymin, ymax,
                          xscale, yscale, '.');


/* Draw symbols in symbol_string1 ("cCtTsSiIpPhH") */
      else if (SymbolFromStyleChar1(symbol_string1, iplot-1, &ifunc) == 0) 
         {
         for ( i=1; i<=npts; i++ ) {
            xdata1 = jlp_gseg_data1->XData(index + i - 1);
            ydata1 = jlp_gseg_data1->YData(index + i - 1);
            if ( xmin <= xdata1 && xdata1 <= xmax &&
                 ymin <= ydata1 && ydata1 <= ymax )
               {
               x = dev_x1_box + (xdata1 - xmin)*xscale;
               y = dev_y2_box - (ydata1 - ymin)*yscale;
               DrawSymbol1(ifunc, x, y, fill_colors_rgba1[iplot-1], 
                           outline_colors_rgba1[iplot-1],
                           jlp_gseg_data1->StyleSizes(iplot-1));
               }
            }
         }


/* Draw symbols in symbol_string2 ("+xra") */
      else if (SymbolFromStyleChar1(symbol_string2, iplot-1, &ifunc) == 0)
         {
         for ( i=1; i<=npts; i++ ) {
            xdata1 = jlp_gseg_data1->XData(index + i - 1);
            ydata1 = jlp_gseg_data1->YData(index + i - 1);
            if ( xmin <= xdata1 && xdata1 <= xmax &&
                 ymin <= ydata1 && ydata1 <= ymax )
               {
               x = dev_x1_box + (xdata1 - xmin)*xscale;
               y = dev_y2_box - (ydata1 - ymin)*yscale;
               DrawSymbol2(ifunc, x, y, fill_colors_rgba1[iplot-1], 
                           jlp_gseg_data1->StyleSizes(iplot-1));
               }
            }
         }

      index = index + jlp_gseg_data1->NData(ipoints+ihist-1);
      }


   else if ( strcmp(&plot_types0[index_plot_types], "histogram") == 0 )
      {
      ihist++;
      DrawHistogram(iplot, ihist);
      index = index + jlp_gseg_data1->NData(ipoints+ihist-1);
      }


   else if ( strcmp(&plot_types0[index_plot_types], "contour") == 0 )
      {
      icontour++;
      nxcontourplot = jlp_gseg_data1->NXContour(icontour-1);
      nycontourplot = jlp_gseg_data1->NYContour(icontour-1);
      ContourPlot2d(iplot, icontour, xindex_contour, yindex_contour, 
                    zindex_contour, nxcontourplot, nycontourplot);
      xindex_contour = xindex_contour + nxcontourplot;
      yindex_contour = yindex_contour + nycontourplot;
      zindex_contour = zindex_contour + nxcontourplot*nycontourplot;
      }


   else if ( strcmp(&plot_types0[index_plot_types], "color")  == 0 )
      {
      icolor++;
      nxcolorplot = jlp_gseg_data1->NXColor(icolor-1);
      nycolorplot = jlp_gseg_data1->NYColor(icolor-1);
      ColorPlot2d(iplot, icolor, xindex_color, yindex_color, zindex_color, 
                  nxcolorplot, nycolorplot);
      xindex_color = xindex_color + nxcolorplot;
      yindex_color = yindex_color + nycolorplot;
      zindex_color = zindex_color + nxcolorplot*nycolorplot;
      }


/* Increment indices */
   index_plot_types = index_plot_types + 10;
   }

return;
}


/**************************************************************************
*
***************************************************************************/
void JLP_Gsegraf::DrawLineSegments ( int iplot, int index, int npts,
                            double xmin, double xmax, double ymin, double ymax,
                            double xscale, double yscale, int linechar )
{
/* Declare variables */
int idraw, iseg, nseg, npts_seg, iseg1;
int nlinebrk1;
double *xdata0, *ydata0;
char linetype[7];

/* Create linetype string */
memset(linetype, 0, sizeof(linetype));
if ( linechar == 'l' )
   strcpy(linetype, "solid");
else if ( linechar == 'd' )
   strcpy(linetype, "dashed");
else if ( linechar == '.' )
   strcpy(linetype, "dotted");

/* Draw all line segments except last */
idraw = 0;
nseg = jlp_gseg_data1->NLinebreaks();
iseg1 = index;
for (iseg=1; iseg<=nseg; iseg++ )
   {
   nlinebrk1 = jlp_gseg_data1->NLinebreak(iseg - 1);
   xdata0 = jlp_gseg_data1->XDataPtr(iseg1);
   ydata0 = jlp_gseg_data1->YDataPtr(iseg1);
   if ( index < nlinebrk1 && nlinebrk1 < index + npts )
      {
      idraw++;
      npts_seg = nlinebrk1 - iseg1;
      DrawLines2d(npts_seg, xdata0, ydata0, xmin, xmax, ymin, 
                  ymax, xscale, yscale, fill_colors_rgba1[iplot-1], 
                  jlp_gseg_data1->StyleSizes(iplot-1), linetype);
      iseg1 = nlinebrk1;
      }
   }


/* Draw last line segment */
if ( idraw > 0 )
   {
   npts_seg = index + npts - iseg1;
   xdata0 = jlp_gseg_data1->XDataPtr(iseg1);
   ydata0 = jlp_gseg_data1->YDataPtr(iseg1);
   DrawLines2d(npts_seg, xdata0, ydata0, xmin, xmax, ymin, ymax, xscale, yscale,
               fill_colors_rgba1[iplot-1], jlp_gseg_data1->StyleSizes(iplot-1),
               linetype);
   }

/* Draw continuous line */
else 
   {
   xdata0 = jlp_gseg_data1->XDataPtr(index);
   ydata0 = jlp_gseg_data1->YDataPtr(index);
   DrawLines2d(npts, xdata0, ydata0, xmin, xmax, ymin, ymax, xscale, yscale, 
               fill_colors_rgba1[iplot-1], jlp_gseg_data1->StyleSizes(iplot-1),
               linetype);
   }

return;
}
