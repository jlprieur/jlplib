/*******************************************************************************
*
* jlp_Gsegraf_DrawMiscel.cpp
*
* Contains miscellaneous functions:
*    DrawBar
*    DrawMesh
*    DrawContour
*    DrawColorPlot
*
* Functions draw lines and plot symbols.
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
*******************************************************************************/
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"    // JLP_GsegAxes class

/**************************************************************************
*
**************************************************************************/
void JLP_Gsegraf::DrawBar ( double x1, double y1, double x2, double y2,
               UINT32 fill_color_rgba, UINT32 outline_color_rgba )
{

/* Draw bar */
 jlp_gseg1->GSEG_DrawRectangle(x1, x2, y1, y2, fill_color_rgba, outline_color_rgba, 1);

return;
}

/**************************************************************************
*
**************************************************************************/
void JLP_Gsegraf::DrawMesh(double x1, double y1, double x2, double y2,
                            UINT32 fill_color_rgba, UINT32 outline_color_rgba,
                            int flag )
{
/* Declare variables */
int i;
double x11, x22, y11, y22;
UINT32 fill_color[] = {0xFF000000, 0xFF800000, 0xFFFF0000, 0x80FF0000, 0x00FF0000 }, alpha_mesh;
double dx[] = {0.0, 6.0, 12.0, 18.0, 24.0, 30.0, 36.0, 42.0, 48.0, 54.0, 60.0};
JLP_CanvasPoints *points;
int line_width_one = 1;

/* Draw rectangle */
 if ( flag != 7 )
   {
   y11 = y1 - 6.0;
   y22 = y2 + 6.0;
   jlp_gseg1->GSEG_DrawRectangle(x1, x2, y11, y22, fill_color_rgba, outline_color_rgba, 1);
    }
 else
    {
    alpha_mesh = fill_color_rgba;
    for ( i=1; i<=5; i++ )
       {
       fill_color[i-1] = fill_color[i-1] + alpha_mesh;
       x11 = x1 + dx[2*(i-1)];
       x22 = x1 + dx[2*i];         
       y11 = y1 - 6.0;
       y22 = y2 + 6.0;
       jlp_gseg1->GSEG_DrawRectangle(x11, x22, y11, y22, fill_color[i-1],
                          outline_color_rgba, 1);
       }
    }


/* Draw horizontal line */
 points = jlp_canvas_points_new(2);

 points->coords[0] = x1;
 points->coords[1] = y1;
 points->coords[2] = x2;
 points->coords[3] = y2;

 jlp_gseg1->GSEG_DrawLine(points, outline_color_rgba, line_width_one);

/* Draw vertical lines */
 for ( i=2; i<=10; i++ )
    {
    points->coords[0] = x1 + dx[i-1];
    points->coords[1] = y1 - 6.0;
    points->coords[2] = x1 + dx[i-1];
    points->coords[3] = y2 + 6.0;

    jlp_gseg1->GSEG_DrawLine(points, outline_color_rgba, line_width_one);

    }

jlp_canvas_points_free(points);

return;
}

/**************************************************************************
*
**************************************************************************/
void JLP_Gsegraf::DrawContour(double x1, double y1, double x2, double y2,
                 UINT32 fill_color_rgba, UINT32 outline_color_rgba, int flag)
{
double x11, x12, x21, x22, y11, y12, y21, y22;
char axis_type0[64];

jlp_gseg_axes1->GetAxisType(axis_type0);

/* Draw filled rectangle */
 if(strcmp(axis_type0, "3d") == 0 )
   {
   y11 = y1 - 6.0;
   y12 = y1 + 6.0;
   jlp_gseg1->GSEG_DrawRectangle(x1, x2, y11, y12, fill_color_rgba,
                      fill_color_rgba, 1);
  }


/* Initialize ellipses */

// Ellipse1 :
     x11 = x1;
     x12 = x2;
     y11 = y1 - 6.0;
     y12 = y1 + 6.0;

// Ellipse2 :
     x21 = 0.75 * x1 + 0.25 * x2;
     x22 = 0.25 * x1 + 0.75 * x2;
     y21 = y1 - 3.0;
     y22 = y1 + 3.0;

   if ( flag == 1 || flag == 3 )
      {
/* 2d contour plot: ellipse1, ellipse2 */
      jlp_gseg1->GSEG_DrawEllipse(x11, x12, y11, y12, 0xFFFFFF00, fill_color_rgba, 1); 
      jlp_gseg1->GSEG_DrawEllipse(x21, x22, y21, y22, 0xFFFFFF00, fill_color_rgba, 1); 
      }

   else if ( flag == 7 )
      {
/* 2d contour plot: ellipse1, ellipse2 */
      jlp_gseg1->GSEG_DrawEllipse(x11, x12, y11, y12, 0xFFFFFF00, 0xFF8000FF, 1); 
      jlp_gseg1->GSEG_DrawEllipse(x21, x22, y21, y22, 0xFFFFFF00, 0xFF8000FF, 1); 
      }

   else if ( flag == 2 || flag == 4 || flag == 5 || flag == 6 )
      {
/* 3d contour plot: ellipse1, ellipse2 */
      jlp_gseg1->GSEG_DrawEllipse(x11, x12, y11, y12, 0xFFFFFF00, outline_color_rgba, 1); 
      jlp_gseg1->GSEG_DrawEllipse(x21, x22, y21, y22, 0xFFFFFF00, outline_color_rgba, 1); 
      }
   }

/**************************************************************************
*
**************************************************************************/
void JLP_Gsegraf::DrawColorPlot(double x1, double y1, double x2, double y2)
{
int i, j, nx = 11, ny = 3;
UINT32 color;
double x[11] = { -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5 },
       y[3] = { -1, 0, 1 },
       z[11][3] = { { -1.9407e-02,  3.8980e-17, -1.9407e-02 },
                    {  2.0208e-01,  2.3387e-01,  2.0208e-01 },
                    {  4.6034e-01,  5.0455e-01,  4.6034e-01 },
                    {  7.0200e-01,  7.5683e-01,  7.0200e-01 },
                    {  8.7350e-01,  9.3549e-01,  8.7350e-01 },
                    {  9.3549e-01,  1.0000e+00,  9.3549e-01 },
                    {  8.7350e-01,  9.3549e-01,  8.7350e-01 },
                    {  7.0200e-01,  7.5683e-01,  7.0200e-01 },
                    {  4.6034e-01,  5.0455e-01,  4.6034e-01 },
                    {  2.0208e-01,  2.3387e-01,  2.0208e-01 },
                    { -1.9407e-02,  3.8980e-17, -1.9407e-02 } };
double xi[61], yi[13], zi[61][13], zinterp, zmin, zmax, zscale, fraction;
double xx, yy;
JLP_DPixbuf pixbuf;

   /* Normalize values of x and y */
   for ( i=1; i<=11; i++ )
      x[i-1] = x[i-1] * PI / 5.0;
   for ( j=1; j<=3; j++ )
      y[j-1] = y[j-1] * PI / 5.0;


   /* Calculate interpolated values of x, y, and z */
   for ( i=1; i<=61; i++ )
      xi[i-1] = x[0] + (i - 1)*(x[10] - x[0])/60;
   for ( j=1; j<=13; j++ )
      yi[j-1] = y[0] + (j - 1)*(y[2] - y[0])/12;

   for ( i=1; i<=61; i++ )
      for ( j=1; j<=13; j++ )
         interp2(nx, ny, 1, &x[0], &y[0], &z[0][0],
                 &xi[i-1], &yi[j-1], &zi[i-1][j-1]);

/* Allocate memory for the  pixbuf */
   GSEG_NewDPixbuf(&pixbuf, 61, 13);

/* Draw color-plot symbol */
   zmin = min(793, &zi[0][0]);
   zmax = max(793, &zi[0][0]);
   zscale = 1.0/(zmax - zmin);
   for ( i=1; i<=61; i++ )
      for ( j=13; j>=1; j-- )
         {
         zinterp = zi[i-1][j-1];
         fraction = (zinterp - zmin)*zscale;
         color = interp_color_spectrum(fraction, n_color_spectrum_1,
                                       color_spectrum_1);
         GSEG_PutPixelDPixbuf(&pixbuf, i-1, 13-j, color);
         }

/* Draw pixbuf on canvas */
   xx = (x1 + x2)/2.0; 
   yy = (y1 + y2)/2.0; 
   jlp_gseg1->GSEG_DrawDPixbuf(&pixbuf, xx, yy, "CENTER");

/* Free memory */
   GSEG_FreeDPixbuf(&pixbuf);
   }
