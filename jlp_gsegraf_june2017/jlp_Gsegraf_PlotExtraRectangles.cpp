/*******************************************************************************
* PlotExtraRectangles.cpp
*
* Plots the extra rectangles from the list contained in JLP_GsegData object.
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
* Version 04/05/2017
*******************************************************************************/
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"         // JLP_GsegAxes class
#include "jlp_gseg_data.h"         // JLP_GsegData class

/******************************************************************************
* Plots the extra rectangles from the list contained in JLP_GsegData object.
*
*******************************************************************************/
void JLP_Gsegraf::PlotExtraRectangles()
{
/* Declare variables */
int i, irect, nrect, status;
unsigned int line_width;
UINT32 line_color;
double dev_x1_box, dev_x2_box, dev_y1_box, dev_y2_box;
double xmin, xmax, ymin, ymax, zmin, zmax, xscale, yscale;
double x0, y0, width, height, angle, a, b, R[4], x[5], y[5];
char line_char, color_char, line_type[16];

/* Get plot box settings */
 jlp_gseg_axes1->GetBoxSettingsForLinear(&dev_x1_box, &dev_x2_box, &dev_y1_box,
                                         &dev_y2_box, &xmin, &xmax, &ymin, 
                                         &ymax, &zmin, &zmax, &xscale, &yscale);

/* Draw rectangles */
 nrect = jlp_gseg_data1->NExtraEllipses();
 for(irect = 0; irect < nrect; irect++) {
     status = jlp_gseg_data1->GetExtraEllipseData(&x0, &y0, &width,
                                                  &height, &angle, &line_color,
                                                  &line_char, &line_width,
                                                  irect);
     if(status) return;

/* Calculate rectangle centered at offset point */
         a = width/2.0;    /* half long dimension */
         b = height/2.0;   /* half short dimension */

         if ( angle == 0.0 )
            {
            /* Calculate unrotated rectangle */
            x[0] = x0 - a;
            y[0] = y0 - b;
            x[1] = x0 + a;
            y[1] = y0 - b;
            x[2] = x0 + a;
            y[2] = y0 + b;
            x[3] = x0 - a;
            y[3] = y0 + b;
            x[4] = x[0];
            y[4] = y[0];
            }
         else
            {
            /* Calculate rotation matrix elements; */
            /* R elements labeled: [ 0 1; 2 3 ]    */
            angle = angle * DEGTORAD;
            R[0] =  cos(angle);
            R[1] =  sin(angle);
            R[2] = -sin(angle);
            R[3] =  cos(angle);

            /* Calculate rotated rectangle */
            x[0] = x0 + (- R[0]*a - R[1]*b);
            y[0] = y0 - (- R[2]*a - R[3]*b);
            x[1] = x0 + (+ R[0]*a - R[1]*b);
            y[1] = y0 - (+ R[2]*a - R[3]*b);
            x[2] = x0 + (+ R[0]*a + R[1]*b);
            y[2] = y0 - (+ R[2]*a + R[3]*b);
            x[3] = x0 + (- R[0]*a + R[1]*b);
            y[3] = y0 - (- R[2]*a + R[3]*b);
            x[4] = x[0];
            y[4] = y[0];
            }

/* Draw rectangle */
        if ( line_char == 'l' )
           strcpy(line_type, "solid");
        else if ( line_char == 'd' )
           strcpy(line_type, "dotted");
        else if ( line_char == '.' )
           strcpy(line_type, "dotted");

        DrawLines2d(5, &x[0],  &y[0], xmin, xmax, ymin, ymax, 
                        xscale, yscale, line_color, line_width, line_type);

      } //EOF loop on irect

return;
}
