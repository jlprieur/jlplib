/*******************************************************************************
*
* jlp_Gsegraf_DrawTheLegend.cpp
*
* Draws the legend items.
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
* Version 19/04/2017
*******************************************************************************/
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"    // JLP_GsegAxes class
#include "jlp_gseg_data.h"    // JLP_GsegData class

static int compute_legend_geometry(double x1, double y1, double x2, double y2,
                                   char *anchor_text0,
                                   double *width_text0, double *height_text0, 
                                   double *width_legend0, 
                                   double *height_legend0,
                                   double *dx_text0, double *dx_symbol0, 
                                   double *dy_symbol0);
/******************************************************************************
*
*******************************************************************************/
int JLP_Gsegraf::DrawTheLegend()
{
/* Declare variables */
int i, nlines, status; 
double x1, y1, x2, y2, dev_x1_box, dev_y1_box, dev_x2_box, dev_y2_box,
          xlegend, ylegend, zlegend, xanchor, yanchor,
          xanchor_text, yanchor_text,
          width_text, height_text, width_legend, height_legend,
          dx_text, dx_symbol, dy_symbol,
          plot_coords[3], window_coords[2];
double xmin0, xmax0, ymin0, ymax0, zmin0, zmax0, xscale0, yscale0;
char *legend_str, legend_coords_flag[4];
char anchor_text[64], axis_type0[64];

/* Get plot box minimum and maximum values */
 jlp_gseg_axes1->GetBoxSettingsForLinear(&dev_x1_box, &dev_x2_box, 
                                         &dev_y1_box, &dev_y2_box, 
                                         &xmin0, &xmax0, &ymin0, &ymax0,
                                         &zmin0, &zmax0, &xscale0, &yscale0);
 jlp_gseg_axes1->GetAxisType(axis_type0);

// Get data for plotting the legend 
  status = jlp_gseg_data1->GetLegendData(&legend_str, anchor_text,
                                          &xlegend, &ylegend, &zlegend,
                                          legend_coords_flag, &nlines);
// Return from here if no legend:
  if(status) return(-1);

// Get legend overall limits 
 jlp_gseg1->GSEG_DrawLegendGetSize(legend_str, &x1, &y1, &x2, &y2);

// Determine the legend geometry:
 compute_legend_geometry(x1, y1, x2, y2, anchor_text, &width_text, 
                         &height_text, &width_legend, &height_legend, &dx_text, 
                         &dx_symbol, &dy_symbol);

// Determine the legend location:
   if ( strcmp(legend_coords_flag, "abs") == 0 )
      {
      if(strcmp(axis_type0, "3d") == 0 )
         {
         plot_coords[0] = xlegend;
         plot_coords[1] = ylegend;
         plot_coords[2] = zlegend;
         }
      else
         {
         plot_coords[0] = xlegend;
         plot_coords[1] = ylegend;
         }
      jlp_gseg_axes1->GetWindowCoords(plot_coords, window_coords);
      xanchor = window_coords[0];
      yanchor = window_coords[1];
      }
   else if ( strcmp(legend_coords_flag, "rel") == 0 )
      {
      xanchor = (1.0 - xlegend)*dev_x1_box + xlegend*dev_x2_box;
      yanchor = (1.0 - ylegend)*dev_y2_box + ylegend*dev_y1_box;
      }


   /* Check legend is within plot box for absolute coordinates */
   if ( strcmp(legend_coords_flag, "abs") == 0 &&
        (xanchor < 0 || yanchor < 0) )
      {
      fprintf(stderr, "DrawTheLegend/Error: legend is outside plot box\n");
      return(-1);
      }

#ifdef DEBUG
printf("DrawLegendFromFile/DEBUG/anchor_text=%s x/y: %f %f x/yanchor: %f %f\n", 
        anchor_text, xlegend, ylegend, xanchor, yanchor);
#endif

// Draw the legend text:
xanchor_text = xanchor + dx_text;
yanchor_text = yanchor;
jlp_gseg1->GSEG_DrawLegend(legend_str, xanchor_text, yanchor_text, 
                           anchor_text, canvas_fg_color1, canvas_bg_color1, 
                           nlines);

// Draw the legend symbols 
DrawTheLegendSymbols(xanchor, yanchor, dx_symbol, dy_symbol,
                     height_legend, nlines);

return(0);
}
/*************************************************************************
* Draw the legend symbols 
*************************************************************************/
int JLP_Gsegraf::DrawTheLegendSymbols(double xanchor, double yanchor,
                                      double dx_symbol, double dy_symbol,
                                      double height_legend, int nlines)
{
int i, iplot, ifunc, index_plot_types, imesh, icontour, index, nplots0; 
double x, y, x1, y1, x2, y2, xanchor_symbol, yanchor_symbol, yinc1, yinc2;
double dy_bar[] = { 0.0, 6.0, 0.0 };
char *plot_types0, *pchar;
JLP_CanvasPoints *points;

/* Specify legend symbol coordinate parameters */
xanchor_symbol = xanchor + dx_symbol;
yanchor_symbol = yanchor + dy_symbol;
yinc1 = height_legend / (double)nlines;
yinc2 = yinc1 / 2.0;

// Draw the legend symbols:
index_plot_types = 0;
plot_types0 = jlp_gseg_data1->PlotTypesPtr();
imesh = 0;
icontour = 0;
jlp_gseg_data1->Get_nplots(&nplots0);
for ( iplot=1; iplot<=nplots0; iplot++ )
   {
   if ( strcmp(&plot_types0[index_plot_types], "points")    == 0 ||
        strcmp(&plot_types0[index_plot_types], "histogram") == 0 )
      {
/* Draw line */
      if ( jlp_gseg_data1->StyleChar1(iplot-1) == 'l' )
         {
         points = jlp_canvas_points_new(2);
         points->coords[0] = xanchor_symbol;
         points->coords[1] = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
         points->coords[2] = xanchor_symbol + 60.0;
         points->coords[3] = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
         jlp_gseg1->GSEG_DrawLine(points, fill_colors_rgba1[iplot-1], 
                                  jlp_gseg_data1->StyleSizes(iplot-1));
         jlp_canvas_points_free(points);
         }


/* Draw dashed line */
      else if ( jlp_gseg_data1->StyleChar1(iplot-1) == 'd' )
         {
         points = jlp_canvas_points_new(2);
         points->coords[0] = xanchor_symbol;
         points->coords[1] = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
         points->coords[2] = xanchor_symbol + 60.0;
         points->coords[3] = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
         DrawDashedLine(points, fill_colors_rgba1[iplot-1], 
                        jlp_gseg_data1->StyleSizes(iplot-1));
         jlp_canvas_points_free(points);
         }


/* Draw dotted line */
      else if ( jlp_gseg_data1->StyleChar1(iplot-1) == '.' )
         {
         points = jlp_canvas_points_new(2);
         points->coords[0] = xanchor_symbol;
         points->coords[1] = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
         points->coords[2] = xanchor_symbol + 60.0;
         points->coords[3] = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
         DrawDottedLine(points, fill_colors_rgba1[iplot-1], 
                        jlp_gseg_data1->StyleSizes(iplot-1));
         jlp_canvas_points_free(points);
         }


/* Draw symbols in symbol_string1 ("cCtTsSiIpPhH") */
      else if ( (pchar = strchr(symbol_string1, 
                            jlp_gseg_data1->StyleChar1(iplot-1))) != NULL )
         {
         ifunc = pchar - symbol_string1;
         for ( i=1; i<=3; i++ )
            {
            x = xanchor_symbol + 10.0 + (i-1)*20.0;
            y = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
            DrawSymbol1(ifunc, x, y, fill_colors_rgba1[iplot-1], 
                        outline_colors_rgba1[iplot-1], 
                        jlp_gseg_data1->StyleSizes(iplot-1));
            }
         }


/* Draw symbols in symbol_string2 ("+xra") */
      else if ( (pchar = strchr(symbol_string2, 
                             jlp_gseg_data1->StyleChar1(iplot-1))) != NULL )
         {
         ifunc = pchar - symbol_string2;
         for ( i=1; i<=3; i++ )
            {
            x = xanchor_symbol + 10.0 + (i-1)*20.0;
            y = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
            DrawSymbol2(ifunc, x, y, fill_colors_rgba1[iplot-1], 
                        jlp_gseg_data1->StyleSizes(iplot-1));
            }
         }


/* Draw bars */
      else if ( jlp_gseg_data1->StyleChar1(iplot-1) == 'b' 
                || jlp_gseg_data1->StyleChar1(iplot-1) == 'B' )
         {
         for ( i=1; i<=3; i++ )
            {
            x = xanchor_symbol + 10.0 + (i-1)*20.0;
            y = yanchor_symbol + yinc1*(iplot - 1) + yinc2;

            x1 = x - 10.0;
            x2 = x + 10.0;
            y1 = y + 6.0;
            y2 = y - dy_bar[i-1];

            DrawBar(x1+1.0, y1, x2-1.0, y2, fill_colors_rgba1[iplot-1], 
                    outline_colors_rgba1[iplot-1]);
            DrawBar(x1, y1+1.0, x2, y2-1.0, 0xFFFFFF00, canvas_bg_color1);
            }
         }
      }
   else if ( strcmp(&plot_types0[index_plot_types], "mesh") == 0 )
      {
      imesh++;
      x1 = xanchor_symbol;
      y1 = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
      x2 = xanchor_symbol + 60.0;
      y2 = yanchor_symbol + yinc1*(iplot - 1) + yinc2;

      if ( jlp_gseg_data1->StyleFlags(iplot-1) == 2 
           || jlp_gseg_data1->StyleFlags(iplot-1) == 4 )
         {
         if ( (pchar = strchr(color_string, 
                              jlp_gseg_data1->StyleChar1(iplot-1))) != NULL )
            {
            index = pchar - &color_string[0];
            DrawMesh(x1, y1, x2, y2, color_rgba1[index], meshcolors[imesh-1],
                     jlp_gseg_data1->StyleFlags(iplot-1));
            }
         }

      else if ( jlp_gseg_data1->StyleFlags(iplot-1) == 5 
                || jlp_gseg_data1->StyleFlags(iplot-1) == 6 )
         DrawMesh(x1, y1, x2, y2, jlp_gseg_data1->StyleColor1(iplot-1), 
                  meshcolors[imesh-1], jlp_gseg_data1->StyleFlags(iplot-1));

      else if ( jlp_gseg_data1->StyleFlags(iplot-1) == 7 )
         DrawMesh(x1, y1, x2, y2, alphacolor[iplot-1], meshcolors[imesh-1], 
                  jlp_gseg_data1->StyleFlags(iplot-1));
      }


   else if ( strcmp(&plot_types0[index_plot_types], "contour") == 0 )
      {
      icontour++;
      x1 = xanchor_symbol;
      y1 = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
      x2 = xanchor_symbol + 60.0;
      y2 = yanchor_symbol + yinc1*(iplot - 1) + yinc2;

      if ( jlp_gseg_data1->StyleFlags(iplot-1) == 1 )
         {
/* 2d contour plot */
         if ( (pchar = strchr(color_string, 
                              jlp_gseg_data1->StyleChar1(iplot-1))) != NULL )
            {
            index = pchar - &color_string[0];
            DrawContour(x1, y1, x2, y2, color_rgba1[index], 0xFFFFFF00, 
                        jlp_gseg_data1->StyleFlags(iplot-1));
            }
         }

      else if ( jlp_gseg_data1->StyleFlags(iplot-1) == 3 )
         {
/* 2d contour plot */
         DrawContour(x1, y1, x2, y2, jlp_gseg_data1->StyleColor1(iplot-1), 
                     0xFFFFFF00, jlp_gseg_data1->StyleFlags(iplot-1));
         }

      else if ( jlp_gseg_data1->StyleFlags(iplot-1) == 7 )
         {
/* 2d contour plot */
         DrawContour(x1, y1, x2, y2, color_rgba1[3], 0xFFFFFF00, 
                     jlp_gseg_data1->StyleFlags(iplot-1));
         }

      else if ( jlp_gseg_data1->StyleFlags(iplot-1) == 2 || jlp_gseg_data1->StyleFlags(iplot-1) == 4 )
         {
/* 3d contour plot */
         if ( (pchar = strchr(color_string, 
                              jlp_gseg_data1->StyleChar1(iplot-1))) != NULL )
            {
            index = pchar - &color_string[0];
            DrawContour(x1, y1, x2, y2, color_rgba1[index], 
             contourcolors[icontour-1], jlp_gseg_data1->StyleFlags(iplot-1));
            }
         }

      else if ( jlp_gseg_data1->StyleFlags(iplot-1) == 5 || jlp_gseg_data1->StyleFlags(iplot-1) == 6 )
         {
/* 3d contour plot */
         DrawContour(x1, y1, x2, y2, jlp_gseg_data1->StyleColor1(iplot-1), 
            contourcolors[icontour-1], jlp_gseg_data1->StyleFlags(iplot-1));
         }
      }


   else if ( strcmp(&plot_types0[index_plot_types], "color") == 0 )
      {
      x1 = xanchor_symbol;
      y1 = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
      x2 = xanchor_symbol + 60.0;
      y2 = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
      DrawColorPlot(x1, y1, x2, y2);                                            /* 2d color plot */
      }


/* Increment indices */
   index_plot_types = index_plot_types + 10;
   }


return(0);
}
/*************************************************************************
* Calculate legend coordinates according to anchor_text0 
*
**************************************************************************/
static int compute_legend_geometry(double x1, double y1, double x2, double y2,
                                   char *anchor_text0,
                                   double *width_text0, double *height_text0, 
                                   double *width_legend0, 
                                   double *height_legend0,
                                   double *dx_text0, double *dx_symbol0, 
                                   double *dy_symbol0)
{
double width_text, height_text, width_legend, height_legend;
double dx_text, dx_symbol, dy_symbol;

   width_text = x2 - x1;
   height_text = y2 - y1;
   width_legend = 70.0 + width_text;
   height_legend = height_text;
   if ( strcmp(anchor_text0, "CENTER") == 0 )
      {
      dx_text = (width_legend - width_text)/2.0;
      dx_symbol = -width_legend/2.0;
      dy_symbol = -height_legend/2.0;
      }
   else if ( strcmp(anchor_text0, "NORTH") == 0 )
      {
      dx_text = (width_legend - width_text)/2.0;
      dx_symbol = -width_legend/2.0;
      dy_symbol = 0.0;
      }
   else if ( strcmp(anchor_text0, "NORTH_EAST") == 0 )
      {
      dx_text = 0.0;
      dx_symbol = -width_legend;
      dy_symbol = 0.0;
      }
   else if ( strcmp(anchor_text0, "EAST") == 0 )
      {
      dx_text = 0.0;
      dx_symbol = -width_legend;
      dy_symbol = -height_legend/2.0;
      }
   else if ( strcmp(anchor_text0, "SOUTH_EAST") == 0 )
      {
      dx_text = 0.0;
      dx_symbol = -width_legend;
      dy_symbol = -height_legend;
      }
   else if ( strcmp(anchor_text0, "SOUTH") == 0 )
      {
      dx_text = (width_legend - width_text)/2.0;
      dx_symbol = -width_legend/2.0;
      dy_symbol = -height_legend;
      }
   else if ( strcmp(anchor_text0, "SOUTH_WEST") == 0 )
      {
      dx_text = width_legend - width_text;
      dx_symbol = 0.0;
      dy_symbol = -height_legend;
      }
   else if ( strcmp(anchor_text0, "WEST") == 0 )
      {
      dx_text = width_legend - width_text;
      dx_symbol = 0.0;
      dy_symbol = -height_legend/2.0;
      }
// Default is NORTH_WEST
//   else if ( strcmp(anchor_text0, "NORTH_WEST") == 0 )
     else
      {
      dx_text = width_legend - width_text;
      dx_symbol = 0.0;
      dy_symbol = 0.0;
      }

*width_text0 = width_text;
*height_text0 = height_text;
*width_legend0 = width_legend;
*height_legend0 = height_legend;
*dx_text0 = dx_text; 
*dx_symbol0 = dx_symbol; 
*dy_symbol0 = dy_symbol; 

return(0);
}
