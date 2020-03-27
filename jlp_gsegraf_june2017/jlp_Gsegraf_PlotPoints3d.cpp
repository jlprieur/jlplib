/*******************************************************************************
* jlp_Gsegraf_PlotPoints3d.cpp
*
* Plots a two-dimensional projection of three-dimensional points data.
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
#include "jlp_gseg_axes.h"     // JLP_GsegAxes class
#include "jlp_gseg_data.h"     // JLP_GsegData class

/***********************************************************************
*
***********************************************************************/
void JLP_Gsegraf::PlotPoints3d ( int iplot, int index, int npts )
{
/* Declare variables */
int i, j, ifunc, nxvalues, nyvalues, nzvalues, index_stemflags;
int stylesizes_0;
double phi, theta, axis_length, origin[3], Ry[9], Rz[9], Ryz[9]; 
double r[3], rstem[3], *ppp,
       xmin, xmax, ymin, ymax, zmin, zmax, xscale, yscale, zscale, x, y;
double xdata1, ydata1, zdata1;
char *pchar, stylechar1_0;
JLP_CanvasPoints *points;

 stylechar1_0 = jlp_gseg_data1->StyleChar1(iplot - 1);
 stylesizes_0 = jlp_gseg_data1->StyleSizes(iplot - 1);

// Get plot settings
 jlp_gseg_axes1->GetBoxSettingsFor3d(origin, Ry, Rz, Ryz, &xmin, &xmax, &ymin,
                                     &ymax, &zmin, &zmax, &xscale, &yscale,
                                     &zscale);

// Specify view angles 
// phi: view-direction azimuth (deg) from x axis in x-y plane
// theta: view-direction elevation (deg) from x-y plane
 jlp_gseg_axes1->Get3dViewAngles(&phi, &theta);

/* Draw stem lines */
 index_stemflags = (iplot - 1)*4;
 if ( strcmp(&stemflags[index_stemflags],  "on") == 0 || 
      strcmp(&stemflags[index_stemflags], "num") == 0 )
    {
    points = jlp_canvas_points_new(2);

/* Calculate z coordinate of stem point 1 */
    if ( strcmp(&stemflags[index_stemflags], "on") == 0 )
       rstem[2] = 0.0;
    else if ( strcmp(&stemflags[index_stemflags], "num") == 0 )
       {
       if ( zmin <= stemvalues[iplot-1] && stemvalues[iplot-1] <= zmax )
          rstem[2] = (stemvalues[iplot-1] - zmin)*zscale;
       else if ( stemvalues[iplot-1] < zmin )
          rstem[2] = 0.0;
       else if ( stemvalues[iplot-1] > zmax )
          rstem[2] = axis_length;
       }

    for ( i=1; i<=npts; i++ ) {
       xdata1 = jlp_gseg_data1->XData(index + i - 1);
       ydata1 = jlp_gseg_data1->YData(index + i - 1);
       zdata1 = jlp_gseg_data1->ZData(index + i - 1);
       if ( xmin <= xdata1 && xdata1 <= xmax &&
            ymin <= ydata1 && ydata1 <= ymax &&
            zmin <= zdata1 && zdata1 <= zmax )
          {
/* Calculate coordinates of stem point 1 */
          rstem[0] = (xdata1 - xmin)*xscale;
          rstem[1] = (ydata1 - ymin)*yscale;

          ppp = multiply_mv(Ryz, rstem);
          for ( j=1; j<=3; j++, ppp++ )
             r[j-1] = *ppp;

          x = origin[1] + r[1];
          y = origin[2] - r[2];
          points->coords[0] = x;
          points->coords[1] = y;

/* Calculate coordinates of stem point 2 */
            r[0] = (xdata1 - xmin)*xscale;
            r[1] = (ydata1 - ymin)*yscale;
            r[2] = (zdata1 - zmin)*zscale;
            ppp = multiply_mv(Ryz, r);
            for ( j=1; j<=3; j++, ppp++ )
               r[j-1] = *ppp;

            x = origin[1] + r[1];
            y = origin[2] - r[2];
            points->coords[2] = x;
            points->coords[3] = y;

            jlp_gseg1->GSEG_DrawLine(points, 
                                     outline_colors_rgba1[iplot-1], 1);
            }

      jlp_canvas_points_free(points);
      }
    }


/* Draw lines */
   if ( stylechar1_0 == 'l' )
      DrawLineSegments3d(iplot, index, npts, &origin[0], &Ryz[0],
                         xmin, xmax, ymin, ymax, zmin, zmax,
                         xscale, yscale, zscale, 'l');


/* Draw dashed lines */
   else if ( stylechar1_0 == 'd' )
      DrawLineSegments3d(iplot, index, npts, &origin[0], &Ryz[0],
                         xmin, xmax, ymin, ymax, zmin, zmax,
                         xscale, yscale, zscale, 'd');


/* Draw dotted lines */
   else if ( stylechar1_0 == '.' )
      DrawLineSegments3d(iplot, index, npts, &origin[0], &Ryz[0],
                         xmin, xmax, ymin, ymax, zmin, zmax,
                         xscale, yscale, zscale, '.');


/* Draw symbols in symbol_string1 ("cCtTsSiIpPhH") */
   else if ( (pchar = strchr(symbol_string1, stylechar1_0)) != NULL )
      {
      ifunc = pchar - symbol_string1;
      for ( i=1; i<=npts; i++ ) {
         xdata1 = jlp_gseg_data1->XData(index + i - 1);
         ydata1 = jlp_gseg_data1->YData(index + i - 1);
         zdata1 = jlp_gseg_data1->ZData(index + i - 1);
         if ( xmin <= xdata1 && xdata1 <= xmax &&
              ymin <= ydata1 && ydata1 <= ymax &&
              zmin <= zdata1 && zdata1 <= zmax )
            {
            r[0] = (xdata1 - xmin)*xscale;
            r[1] = (ydata1 - ymin)*yscale;
            r[2] = (zdata1 - zmin)*zscale;

            ppp = multiply_mv(Ryz, r);
            for ( j=1; j<=3; j++, ppp++ )
               r[j-1] = *ppp;

            x = origin[1] + r[1];
            y = origin[2] - r[2];
            DrawSymbol1(ifunc, x, y, fill_colors_rgba1[iplot-1], 
                        outline_colors_rgba1[iplot-1], stylesizes_0);
            }
        }
      }


/* Draw symbols in symbol_string2 ("+xra") */
   else if ( (pchar = strchr(symbol_string2, stylechar1_0)) != NULL )
      {
      ifunc = pchar - symbol_string2;
      for ( i=1; i<=npts; i++ ) {
         xdata1 = jlp_gseg_data1->XData(index + i - 1);
         ydata1 = jlp_gseg_data1->YData(index + i - 1);
         zdata1 = jlp_gseg_data1->ZData(index + i - 1);
         if ( xmin <= xdata1 && xdata1 <= xmax &&
              ymin <= ydata1 && ydata1 <= ymax &&
              zmin <= zdata1 && zdata1 <= zmax )
            {
            r[0] = (xdata1 - xmin)*xscale;
            r[1] = (ydata1 - ymin)*yscale;
            r[2] = (zdata1 - zmin)*zscale;

            ppp = multiply_mv(Ryz, r);
            for ( j=1; j<=3; j++, ppp++ )
               r[j-1] = *ppp;

            x = origin[1] + r[1];
            y = origin[2] - r[2];
            DrawSymbol2(ifunc, x, y, fill_colors_rgba1[iplot-1], 
                        stylesizes_0);
            }
         }
      }

 return;
 }
/**************************************************************************
*
**************************************************************************/
void JLP_Gsegraf::DrawLineSegments3d(int iplot, int index, int npts, 
                                     double *origin, double *Ryz,
                                     double xmin, double xmax, double ymin, 
                                     double ymax, double zmin, double zmax,
                                     double xscale, double yscale, 
                                     double zscale, int linechar )
{
int idraw, iseg, nseg, npts_seg, iseg1, nlinebrk1;
int stylesizes_0;
double *xdata0, *ydata0, *zdata0;
char linetype[7];

 stylesizes_0 = jlp_gseg_data1->StyleSizes(iplot - 1);


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
   xdata0 = jlp_gseg_data1->XDataPtr(iseg1);
   ydata0 = jlp_gseg_data1->YDataPtr(iseg1);
   zdata0 = jlp_gseg_data1->ZDataPtr(iseg1);
   nlinebrk1 = jlp_gseg_data1->NLinebreak(iseg - 1);
   if ( index < nlinebrk1 && nlinebrk1 < index + npts )
      {
      idraw++;
      npts_seg = nlinebrk1 - iseg1;
      DrawLines3d(npts_seg, xdata0, ydata0, zdata0, origin, Ryz,
                  xmin, xmax, ymin, ymax, zmin, zmax, xscale, yscale, zscale,
                  fill_colors_rgba1[iplot-1], stylesizes_0, linetype);
      iseg1 = nlinebrk1;
      }
   }


/* Draw last line segment */
if ( idraw > 0 )
   {
   xdata0 = jlp_gseg_data1->XDataPtr(iseg1);
   ydata0 = jlp_gseg_data1->YDataPtr(iseg1);
   zdata0 = jlp_gseg_data1->ZDataPtr(iseg1);
   npts_seg = index + npts - iseg1;
   DrawLines3d(npts_seg, xdata0, ydata0, zdata0, origin, Ryz,
               xmin, xmax, ymin, ymax, zmin, zmax, xscale, yscale, zscale,
               fill_colors_rgba1[iplot-1], stylesizes_0, linetype);
   }

/* Draw continuous line */
else
   {
   xdata0 = jlp_gseg_data1->XDataPtr(index);
   ydata0 = jlp_gseg_data1->YDataPtr(index);
   zdata0 = jlp_gseg_data1->ZDataPtr(index);
   DrawLines3d(npts, xdata0, ydata0, zdata0, origin, Ryz,
               xmin, xmax, ymin, ymax, zmin, zmax, xscale, yscale, zscale,
               fill_colors_rgba1[iplot-1], stylesizes_0, linetype);
   }

return;
}
