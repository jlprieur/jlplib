/*******************************************************************************
* jlp_Gsegraf_PlotExtraEllipses.cpp
*
* Plots extra ellipses from the list contained in JLP_GsegData object.
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
* Version 10/05/2017
*******************************************************************************/
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"         // JLP_GsegAxes class
#include "jlp_gseg_data.h"         // JLP_GsegData class

/******************************************************************************
* Plots extra ellipses from the list contained in JLP_GsegData object.
*
*******************************************************************************/
void JLP_Gsegraf::PlotExtraEllipses()
{
/* Declare variables */
int i, npts, iell, nell, status;
unsigned int line_width;
UINT32 line_color;
double dev_x1_box, dev_x2_box, dev_y1_box, dev_y2_box,
       xmin, xmax, ymin, ymax, zmin, zmax, xscale, yscale,
       x0, y0, width, height, angle, a, b, t,
       R[4], x[361], y[361];
char line_char, line_type[16];

/* Get plot box settings */
jlp_gseg_axes1->GetBoxSettingsForLinear(&dev_x1_box, &dev_x2_box, &dev_y1_box,
                                        &dev_y2_box, &xmin, &xmax, &ymin, 
                                        &ymax, &zmin, &zmax, &xscale, &yscale);

/* Draw ellipses */
 nell = jlp_gseg_data1->NExtraEllipses();
 for(iell = 0; iell < nell; iell++) {
     status = jlp_gseg_data1->GetExtraEllipseData(&x0, &y0, &width, 
                                                  &height, &angle, &line_color, 
                                                  &line_char, &line_width, 
                                                  iell);
     if(status) return;

/* Calculate ellipse centered at offset point;   */
/* equations of ellipse:                         */
/*    x^2/a^2 + y^2/b^2 = 1                      */
/*    x = a*cos(t), y = b*sin(t), 0 <= t <= 2 * PI */
        a = width/2.0;    /* semi-major axis */
        b = height/2.0;   /* semi-minor axis */

        npts = 361;
        if ( angle == 0.0 )
           {
/* Calculate unrotated ellipse */
           for ( i=1; i<=npts; i++ )
              {
              t = 2.0 * PI * (i - 1) / (double)(npts - 1);
              x[i-1] = x0 + a*cos(t);
              y[i-1] = y0 - b*sin(t);
              }
           }
        else
           {
/* Calculate rotation-matrix elements; */
/* R elements labeled: [ 0 1; 2 3 ]    */
           angle = angle * DEGTORAD;
           R[0] =  cos(angle);
           R[1] =  sin(angle);
           R[2] = -sin(angle);
           R[3] =  cos(angle);

/* Calculate rotated ellipse */
           for ( i=1; i<=npts; i++ )
              {
              t = 2.0 * PI * (i - 1) / (double)(npts - 1);
              x[i-1] = x0 + (R[0]*a*cos(t) + R[1]*b*sin(t));
              y[i-1] = y0 - (R[2]*a*cos(t) + R[3]*b*sin(t));
              }
           }

/* Draw ellipse */
        if ( line_char == 'l' )
           strcpy(line_type, "solid");
        else if ( line_char == 'd' )
           strcpy(line_type, "dotted");
        else if ( line_char == '.' )
           strcpy(line_type, "dotted");

        DrawLines2d(npts, &x[0],  &y[0], xmin, xmax, ymin, ymax, xscale, 
                    yscale, line_color, line_width, line_type);

     } // EOF loop on iell

return;
}
