/*******************************************************************************
*
* PolarPlot.c
*
* Calculates polar plot of input data.
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
#include <string.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"    // JLP_GsegAxes class
#include "jlp_gseg_data.h"    // JLP_GsegData class

/***************************************************************************
*
***************************************************************************/
void JLP_Gsegraf::PolarPlot()
{
/* Declare variables */
int i, j, ifunc, nyvalues, iplot, npts, nplots0; 
int index, index_stemflags;
char text_anchor[64];
double rmin0, rmax0, xorigin0, yorigin0, radius0, rscale0, ww;
double x, y, x1, y1, x2, y2, xtext1, xtext2, ytext1, ytext2;
double diam1, theta, theta_max, theta_minor, r, r1, dr, width_xtick_label;
char string[21];
JLP_CanvasPoints *points;

/* Specify plot-circle location and radius0 */
 jlp_gseg_axes1->GetBoxSettingsForPolar(&xorigin0, &yorigin0, &radius0, 
                                        &rmin0, &rmax0, &rscale0);

 jlp_gseg_axes1->DrawPolarBox_PartI(xorigin0, yorigin0, radius0, rmin0, rmax0, 
                                    rscale0);

/* Plot data */
   index = 0;
   jlp_gseg_data1->Get_nplots(&nplots0);
   for ( iplot=1; iplot <= nplots0; iplot++ )
      {
      npts = jlp_gseg_data1->NData(iplot - 1);

      /* Draw stem lines */
      index_stemflags = (iplot - 1)*4;
      if ( strcmp(&stemflags[index_stemflags],  "on") == 0 ||
           strcmp(&stemflags[index_stemflags], "num") == 0 )
         {
         points = jlp_canvas_points_new(2);

         /* Calculate r coordinate of stem point 1 */
         if ( strcmp(&stemflags[index_stemflags], "on") == 0 )
            r1 = rmin0;
         else if ( strcmp(&stemflags[index_stemflags], "num") == 0 )
            {
            if ( rmin0 <= stemvalues[iplot-1] && stemvalues[iplot-1] <= rmax0 )
               r1 = stemvalues[iplot-1];
            else if ( stemvalues[iplot-1] < rmin0 )
               r1 = rmin0;
            else if ( stemvalues[iplot-1] > rmax0 )
               r1 = rmax0;
            }

         for ( i=1; i<=npts; i++ )
            {
            theta = jlp_gseg_data1->XData(index+i-1);
            r     = jlp_gseg_data1->YData(index+i-1);
            if ( r <= rmax0 )
               {
               /* Calculate coordinates of stem point 1 */
               x = xorigin0 + (r1 - rmin0)*cos(theta)*rscale0;
               y = yorigin0 - (r1 - rmin0)*sin(theta)*rscale0;
               points->coords[0] = x;
               points->coords[1] = y;

               /* Calculate coordinates of stem point 2 */
               if ( r < rmin0 )
                  r = rmin0;
               x = xorigin0 + (r - rmin0)*cos(theta)*rscale0;
               y = yorigin0 - (r - rmin0)*sin(theta)*rscale0;
               points->coords[2] = x;
               points->coords[3] = y;
               jlp_gseg1->GSEG_DrawLine(points, 
                                        outline_colors_rgba1[iplot-1], 1);
               }
            }

         jlp_canvas_points_free(points);
         }


/* Draw lines */
      if ( jlp_gseg_data1->StyleChar1(iplot - 1) == 'l' )
         DrawLineSegmentsPolar(iplot, index, npts, xorigin0, yorigin0, rmin0, rmax0, rscale0, 'l');


/* Draw dashed lines */
      else if (jlp_gseg_data1->StyleChar1(iplot - 1) == 'd' )
         DrawLineSegmentsPolar(iplot, index, npts, xorigin0, yorigin0, rmin0, rmax0, rscale0, 'd');


/* Draw dashed lines */
      else if (jlp_gseg_data1->StyleChar1(iplot - 1) == '.' )
         DrawLineSegmentsPolar(iplot, index, npts, xorigin0, yorigin0, rmin0, rmax0, rscale0, '.');


/* Draw symbols in symbol_string1 ("cCtTsSiIpPhH") */
      else if ( SymbolFromStyleChar1(symbol_string1, iplot-1, &ifunc) == 0 )
         {
         for ( i=1; i<=npts; i++ )
            {
            theta = jlp_gseg_data1->XData(index+i-1);
            r     = jlp_gseg_data1->YData(index+i-1);
            if ( rmin0 <= r && r <= rmax0 )
               {
               x = xorigin0 + (r - rmin0)*cos(theta)*rscale0;
               y = yorigin0 - (r - rmin0)*sin(theta)*rscale0;
               DrawSymbol1(ifunc, x, y, fill_colors_rgba1[iplot-1], 
                           outline_colors_rgba1[iplot-1], 
                           jlp_gseg_data1->StyleSizes(iplot-1));
               }
            }
         }


/* Draw symbols in symbol_string2 ("+xra") */
      else if ( SymbolFromStyleChar2(symbol_string2, iplot-1, &ifunc) == 0 )
         {
         for ( i=1; i<=npts; i++ )
            {
            theta = jlp_gseg_data1->XData(index+i-1);
            r     = jlp_gseg_data1->YData(index+i-1);
            if ( rmin0 <= r && r <= rmax0 )
               {
               x = xorigin0 + (r - rmin0)*cos(theta)*rscale0;
               y = yorigin0 - (r - rmin0)*sin(theta)*rscale0;
               DrawSymbol2(ifunc, x, y, fill_colors_rgba1[iplot-1], 
                           jlp_gseg_data1->StyleSizes(iplot-1));
               }
            }
         }

      index = index + jlp_gseg_data1->NData(iplot - 1);
      }


/* Plot extra data of lines and symbols */
 PlotExtraLines();
 PlotExtraSymbols();

/* Draw r tick-mark labels on translucent rectangles */
 jlp_gseg_axes1->DrawPolarBox_PartII(xorigin0, yorigin0, rmin0, rmax0, rscale0);

return;
}
/***********************************************************************
*
***********************************************************************/
void JLP_Gsegraf::DrawLineSegmentsPolar(int iplot, int index, int npts,
                                        double xorigin0, double yorigin0, 
                                        double rmin0, double rmax0,
                                        double rscale0, int linechar)
{
/* Declare variables */
int idraw, iseg, nseg, npts_seg, iseg1, nlinebrk;
char linetype[7];
double *xdat, *ydat;

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
 nlinebrk = jlp_gseg_data1->NLinebreak(iseg-1);
 for (iseg=1; iseg<=nseg; iseg++ ) {
    if(( index < nlinebrk) && (nlinebrk < index + npts )) {
       idraw++;
// npts_seg : number of points in the segment to be plotted:
       npts_seg = nlinebrk - iseg1;
       xdat = jlp_gseg_data1->XDataPtr(iseg1);
       ydat = jlp_gseg_data1->YDataPtr(iseg1);
       DrawLinesPolar(npts_seg, xdat, ydat, xorigin0, 
                      yorigin0, rmin0, rmax0, rscale0, 
                      fill_colors_rgba1[iplot-1], 
                      jlp_gseg_data1->StyleSizes(iplot-1), linetype);
       iseg1 = nlinebrk;
       }
    }

/* Draw last line segment */
 if ( idraw > 0 ) {
    npts_seg = index + npts - iseg1;
    xdat = jlp_gseg_data1->XDataPtr(iseg1);
    ydat = jlp_gseg_data1->YDataPtr(iseg1);
    DrawLinesPolar(npts_seg, xdat, ydat, xorigin0, yorigin0, 
                   rmin0, rmax0, rscale0, fill_colors_rgba1[iplot-1], 
                   jlp_gseg_data1->StyleSizes(iplot-1), linetype);
/* Draw continuous line */
   } else {
     xdat = jlp_gseg_data1->XDataPtr(index);
     ydat = jlp_gseg_data1->YDataPtr(index);
     DrawLinesPolar(npts, xdat, ydat, xorigin0, yorigin0, 
                    rmin0, rmax0, rscale0, fill_colors_rgba1[iplot-1], 
                    jlp_gseg_data1->StyleSizes(iplot-1), linetype);
   }

return;
}

/***************************************************************************
*
***************************************************************************/
void JLP_Gsegraf::DrawDashedCircle(double xorigin0, double yorigin0, 
                                   double radius0, UINT32 fill_color_rgba0)
{
/* Declare variables */
int i, j, ndashes;
double theta1, theta2, dtheta1, dtheta2;
JLP_CanvasPoints *points;

/* Check radius0 */
 if ( radius0 == 0.0 ) return;

/* Draw dashed circle */
   ndashes = roundint( 2.0 * PI * radius0 / (dash1 + space_dash1) );
   dtheta1 = 2.0 * PI / (double)ndashes;
   dtheta2 = 0.5 * PI * dash1 /((double)ndashes * (dash1 + space_dash1) );

   points = jlp_canvas_points_new(5);

   for ( i=1; i<=ndashes; i++ )
      {
      theta1 = (i - 1)*dtheta1;
      points->coords[0] = xorigin0 + radius0*cos(theta1);
      points->coords[1] = yorigin0 - radius0*sin(theta1);
      for ( j=1; j<=4; j++ )
         {
         theta2 = theta1 + j*dtheta2;
         points->coords[2*j]   = xorigin0 + radius0*cos(theta2);
         points->coords[2*j+1] = yorigin0 - radius0*sin(theta2);
         }

      jlp_gseg1->GSEG_DrawLine(points, fill_color_rgba0, 1);
      }

jlp_canvas_points_free(points);

return;
}

/***************************************************************************
*
***************************************************************************/
void JLP_Gsegraf::DrawDottedCircle(double xorigin0, double yorigin0, 
                                   double radius0, UINT32 fill_color_rgba0)
{
/* Declare variables */
int i, npoints, ndots;
double theta[361], length[361], length_total, inc, inc_calc, length_interp, theta_interp;

/* Check radius0 */
 if(radius0 == 0) return;

/* Calculate circle length */
 npoints = 361;
 for ( i=1; i<=npoints; i++ )
    {
    theta[i-1] = (i - 1) * PI / 180.0;
    length[i-1] = radius0 * theta[i-1];
    }
 length_total = length[npoints-1];

/* Calculate number of dots */
 inc = space_dot1 + 1.0;
 ndots = roundint(length_total/inc + 1.0);
 inc_calc = length_total/(ndots - 1);

/* Interpolate and draw dots */
 for ( i=1; i<ndots; i++ ) {
    length_interp = length[0] + (i - 1)*inc_calc;
    interp1(npoints, 1, length, theta, &length_interp, &theta_interp);
    jlp_gseg1->GSEG_DrawCircle(xorigin0+radius0*cos(theta_interp), 
                      yorigin0-radius0*sin(theta_interp), 1,
                      fill_color_rgba0, fill_color_rgba0, 1);
    }

return;
}
