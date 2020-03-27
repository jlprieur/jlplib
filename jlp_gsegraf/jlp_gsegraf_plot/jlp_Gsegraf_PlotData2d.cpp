/*******************************************************************************
*
* jlp_Gsegraf_PlotData2d.cpp
*
* Draws data points, contours and color plots.
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
* Version 11/04/2018
*******************************************************************************/
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"         // JLP_GsegAxes class
#include "jlp_gseg_data.h"         // JLP_GsegData class

/* Contains:
void PlotData2d (void);
void DrawLineSegments ( int iplot, int npts,
                        double xmin, double xmax, double ymin, double ymax,
                        double xscale, double yscale, int linechar );
*/

/*
#define DEBUG
*/

/*****************************************************************************
*
*****************************************************************************/
void JLP_Gsegraf::PlotData2d (void)
{
/* Declare variables */
int i, ifunc, iplot, nplots0, npts, gseg_plot_type0,
    icolor_scale, index_plot_types, index_stemflags;
int reversed_axis0[3], status;
unsigned style_size0;
double dev_x1_box, dev_x2_box, dev_y1_box, dev_y2_box, xmin, xmax, ymin, ymax,
       zmin, zmax, xscale, yscale, x, y, xdata1, ydata1, xdata2, ydata2;
char style_char10;
UINT32 outline_color_rgba0, fill_color_rgba0;
JLP_CanvasPoints *points;

/* Get plot box settings */
jlp_gseg_axes1->GetBoxSettingsForLinear(&dev_x1_box, &dev_x2_box, &dev_y1_box,
                                        &dev_y2_box, &xmin, &xmax, &ymin,
                                        &ymax, &zmin, &zmax, &xscale, &yscale);

/* Plot data */
index_plot_types = 0;
jlp_gseg_data1->Get_nplots(&nplots0);
for ( iplot = 1; iplot <= nplots0; iplot++ )
  {
/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
  status = jlp_gseg_data1->GetGsegPlotType(iplot, &gseg_plot_type0);
  if(status != 0) {
   fprintf(stderr, "PlotData2d/Error in GetGsegPlotType: status=%d\n", status);
   return;
   }

// Style parameters:
  status = jlp_gseg_data1->GetStyleChar1(iplot, &style_char10);
  if(status != 0) {
   fprintf(stderr, "PlotData2d/Error in GetStyleChar1: status=%d\n", status);
   return;
   }
  status = jlp_gseg_data1->GetStyleSize(iplot, &style_size0);
  if(status != 0) {
   fprintf(stderr, "PlotData2d/Error in GetStyleSize: status=%d\n", status);
   return;
   }
  status = jlp_gseg_data1->GetOutlineColor(iplot, &outline_color_rgba0);
  if(status != 0) {
   fprintf(stderr, "PlotData2d/Error in GetOutlineColor: status=%d\n", status);
   return;
   }
  status = jlp_gseg_data1->GetFillColor(iplot, &fill_color_rgba0);
  if(status != 0) {
   fprintf(stderr, "PlotData2d/Error in GetFillColor: status=%d\n", status);
   return;
   }

// "points"
   if (gseg_plot_type0 == 1 )
      {
#ifdef DEBUG
      printf("PlotData2d/DEBUG: plot_type=1 : iplot=%d style_char1=%c style_size=%d outline_color=%d fill_color=%d\n",
             iplot,  style_char10, style_size0, outline_color_rgba0, 
             fill_color_rgba0);
#endif
      npts = jlp_gseg_data1->NPts(iplot);

/* Draw stem lines */
      index_stemflags = (iplot - 1)*4;
      if ( strcmp(&stemflags[index_stemflags],  "on") == 0 ||
           strcmp(&stemflags[index_stemflags], "num") == 0 )
         {
         points = jlp_canvas_points_new(2);

/* Calculate y coordinate of stem point 1 */
         if ( strcmp(&stemflags[index_stemflags], "on") == 0 ) {
// y = dev_y2_box;
           jlp_gseg_axes1->DrawYPoint(0., &y);
         } else if ( strcmp(&stemflags[index_stemflags], "num") == 0 ) {
            if ( ymin <= stemvalues[iplot-1] && stemvalues[iplot-1] <= ymax ) {
// y = dev_y2_box - (stemvalues[iplot-1] - ymin)*yscale;
              ydata2 = (stemvalues[iplot-1] - ymin) * yscale;
              jlp_gseg_axes1->DrawYPoint(ydata2, &y);
            } else if ( stemvalues[iplot-1] < ymin ) {
// y = dev_y2_box;
              jlp_gseg_axes1->DrawYPoint(0., &y);
            } else if ( stemvalues[iplot-1] > ymax ) {
// y = dev_y1_box;
              jlp_gseg_axes1->DrawYPoint(-1., &y);
            }
          }
         points->coords[1] = y;

         for ( i=1; i<=npts; i++ )
            xdata1 = jlp_gseg_data1->XData(iplot, i - 1);
            ydata1 = jlp_gseg_data1->YData(iplot, i - 1);
            if ( xmin <= xdata1 && xdata1 <= xmax &&
                 ymin <= ydata1 && ydata1 <= ymax )
               {
/* Calculate x coordinate of stem point 1 */
               xdata2 = (xdata1 - xmin) * xscale;
               jlp_gseg_axes1->DrawXPoint(xdata2, &x);
               points->coords[0] = x;

/* Calculate x and y coordinates of stem point 2 */
               ydata2 = (ydata1 - ymin) * yscale;
               jlp_gseg_axes1->DrawYPoint(ydata2, &y);
               points->coords[2] = x;
               points->coords[3] = y;

               jlp_gseg1->GSEG_DrawLine(points, outline_color_rgba0, 1);
               }

         jlp_canvas_points_free(points);
         }

/* Draw lines */
      if ( style_char10 == 'l' )
         DrawLineSegments(iplot, npts, xmin, xmax, ymin, ymax,
                          xscale, yscale, 'l');


/* Draw dashed lines */
      else if ( style_char10 == 'd' )
         DrawLineSegments(iplot, npts, xmin, xmax, ymin, ymax,
                          xscale, yscale, 'd');


/* Draw dotted lines */
      else if ( style_char10 == '.' )
         DrawLineSegments(iplot, npts, xmin, xmax, ymin, ymax,
                          xscale, yscale, '.');


/* Draw symbols in symbol_string1 ("cCtTsSiIpPhH") */
      else if (jlp_gseg_data1->DrawSymbol1OptionFromStyleChar1(iplot, &ifunc) 
                == 0) 
         {
         for ( i=1; i<=npts; i++ ) {
            xdata1 = jlp_gseg_data1->XData(iplot, i - 1);
            ydata1 = jlp_gseg_data1->YData(iplot, i - 1);
            if ( xmin <= xdata1 && xdata1 <= xmax &&
                 ymin <= ydata1 && ydata1 <= ymax )
               {
//               x = dev_x1_box + (xdata1 - xmin)*xscale;
//               y = dev_y2_box - (ydata1 - ymin)*yscale;
                 xdata2 = (xdata1 - xmin)*xscale;
                 ydata2 = (ydata1 - ymin)*yscale;
                 jlp_gseg_axes1->DrawXPoint(xdata2, &x);
                 jlp_gseg_axes1->DrawYPoint(ydata2, &y);
#ifdef DEBUG
printf("PlotData2d/DEBUG/ symbol1: ifunc1=%d outline_color=%d fill_color=%d size=%d\n",
                      ifunc, outline_color_rgba0, fill_color_rgba0, 
                      style_size0);
#endif
                 DrawSymbol1(ifunc, x, y, fill_color_rgba0, 
                             outline_color_rgba0, style_size0);
               }
            }
         }


/* Draw symbols in symbol_string2 ("+xra") */
      else if (jlp_gseg_data1->DrawSymbol2OptionFromStyleChar1(iplot, &ifunc) 
                == 0) 
         {
         for ( i=1; i<=npts; i++ ) {
            xdata1 = jlp_gseg_data1->XData(iplot, i - 1);
            ydata1 = jlp_gseg_data1->YData(iplot, i - 1);
            if ( xmin <= xdata1 && xdata1 <= xmax &&
                 ymin <= ydata1 && ydata1 <= ymax )
               {
//               x = dev_x1_box + (xdata1 - xmin)*xscale;
//               y = dev_y2_box - (ydata1 - ymin)*yscale;
                 xdata2 = (xdata1 - xmin)*xscale;
                 ydata2 = (ydata1 - ymin)*yscale;
                 jlp_gseg_axes1->DrawXPoint(xdata2, &x);
                 jlp_gseg_axes1->DrawYPoint(ydata2, &y);
#ifdef DEBUG
printf("PlotData2d/DEBUG/ symbol2: ifunc2=%d outline_color=%d fill_color=%d size=%d\n",
                      ifunc, outline_color_rgba0, fill_color_rgba0, 
                      style_size0);
#endif
                 DrawSymbol2(ifunc, x, y, outline_color_rgba0, style_size0);
               }
            }
         }
      }

// "histogram"
   else if (gseg_plot_type0 == 2 )
      {
      DrawHistogram(iplot);
      }

// "contour"
   else if (gseg_plot_type0 == 3 )
      {
      ContourPlot2d(iplot);
      }

// "color"
   else if (gseg_plot_type0 == 4 )
      {
// Select option for drawing color scale
      icolor_scale = 1;
// "color" plot
      ColorPlot2d(iplot, icolor_scale);
      }

   }

return;
}


/**************************************************************************
*
***************************************************************************/
void JLP_Gsegraf::DrawLineSegments ( int iplot, int npts,
                            double xmin, double xmax, double ymin, double ymax,
                            double xscale, double yscale, int linechar )
{
/* Declare variables */
int idraw, iseg, nseg, npts_seg, iseg1;
int linebrk1;
unsigned style_size0;
double *xdata0, *ydata0;
UINT32 outline_color_rgba0, fill_color_rgba0;
char linetype[7];

// Get style parameters: 
 jlp_gseg_data1->GetStyleSize(iplot, &style_size0);
 jlp_gseg_data1->GetOutlineColor(iplot, &outline_color_rgba0);
 jlp_gseg_data1->GetFillColor(iplot, &fill_color_rgba0);

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
nseg = jlp_gseg_data1->NLinebreaks(iplot);
iseg1 = 0;
for (iseg=1; iseg<=nseg; iseg++ )
   {
   linebrk1 = jlp_gseg_data1->Linebreak(iplot, iseg);
   xdata0 = jlp_gseg_data1->XDataPtr(iplot, iseg1);
   ydata0 = jlp_gseg_data1->YDataPtr(iplot, iseg1);
   if ( 0 < linebrk1 && linebrk1 < npts )
      {
      idraw++;
      npts_seg = linebrk1 - iseg1;
      DrawLines2d(npts_seg, xdata0, ydata0, xmin, xmax, ymin, 
                  ymax, xscale, yscale, fill_color_rgba0, 
                  style_size0, linetype);
      iseg1 = linebrk1;
      }
   }


/* Draw last line segment */
if ( idraw > 0 )
   {
   npts_seg = npts - iseg1;
   xdata0 = jlp_gseg_data1->XDataPtr(iplot, iseg1);
   ydata0 = jlp_gseg_data1->YDataPtr(iplot, iseg1);
   DrawLines2d(npts_seg, xdata0, ydata0, xmin, xmax, ymin, ymax, xscale, yscale,
               fill_color_rgba0, style_size0, linetype);
   }

/* Draw continuous line */
else 
   {
   xdata0 = jlp_gseg_data1->XDataPtr(iplot, 0);
   ydata0 = jlp_gseg_data1->YDataPtr(iplot, 0);
   DrawLines2d(npts, xdata0, ydata0, xmin, xmax, ymin, ymax, xscale, yscale, 
               fill_color_rgba0, style_size0, linetype);
   }

return;
}
