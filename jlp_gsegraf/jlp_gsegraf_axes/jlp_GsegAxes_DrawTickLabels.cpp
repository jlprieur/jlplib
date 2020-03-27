/*******************************************************************************
*
* jlp_GsegAxes_DrawTickLabels.cpp
*
* Draws tick-mark labels for linear and logarithmic axes.
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
* Version 01/01/2018
*******************************************************************************/
#include <math.h>
#include "jlp_gseg_axes.h"

// #define DEBUG

/*****************************************************************************
*
*****************************************************************************/
double JLP_GsegAxes::DrawTickLabels(char *axis_type, 
                      double x1_screen, double y1_screen, 
                      double x2_screen, double y2_screen, int nticks, 
                      double *tick_values, double offset1, double offset2,
                      double xoffset, double yoffset, char *anchor,
                      int reversed_axis0)
{
/* Declare variables */
int i, ndecades, logtickvalue, ix_sign, iy_sign, xstart_screen, ystart_screen;
double axis_min, axis_max, xscale, yscale;
double increment, xtick, ytick, tickvalue;
double x, y, x1, x2, y1, y2, width_tick_labels;
char string[64];
UINT32 canvas_fg_color0, canvas_bg_color0;

 if(reversed_axis0 == 0) {
   xstart_screen = x1_screen;
   ystart_screen = y1_screen;
   ix_sign = +1;
   iy_sign = +1;
   } else {
   xstart_screen = x2_screen;
   ystart_screen = y2_screen;
   ix_sign = -1;
   iy_sign = -1;
   }

// Get canvas foreground/background colors :
 jlp_gsegraf1->Get_canvas_fg_color(&canvas_fg_color0);
 jlp_gsegraf1->Get_canvas_bg_color(&canvas_bg_color0);

/* Draw linear-axis tick-mark labels */
 if(strcmp(axis_type, "linear") == 0 ) {
    axis_min = tick_values[0] - offset1;
    axis_max = tick_values[nticks-1] + offset2;
    xscale = (x2_screen - x1_screen)/(axis_max - axis_min);
    yscale = (y2_screen - y1_screen)/(axis_max - axis_min);
#ifdef DEBUG
  printf("DrawTickLabels/axis_min=%f axis_max=%f\n", axis_min, axis_max);
#endif


    increment = (tick_values[nticks-1] - tick_values[0])/(nticks - 1);
    width_tick_labels = 0.0;
    for(i = 1; i <= nticks; i++) {
       xtick = xstart_screen + ix_sign * (tick_values[i-1] - axis_min) * xscale;
       ytick = ystart_screen + iy_sign * (tick_values[i-1] - axis_min) * yscale;

       memset(string, 0, sizeof(string));
       if(fabs(tick_values[i-1]) < 0.01*increment)
         snprintf(string, sizeof(string), "%1.0f", 0.0);
       else
         snprintf(string, sizeof(string), "%g", tick_values[i-1]);

       x = xtick + xoffset;
       y = ytick + yoffset;
       jlp_gseg1->GSEG_DrawTickLabels(string, x, y, anchor, canvas_fg_color0, 
                                      canvas_bg_color0, 0, 
                                      &x1, &y1, &x2, &y2);

       if ( x2 - x1 > width_tick_labels )
          width_tick_labels = x2 - x1;
       }
    }

/* Draw logarithmic-axis tick-mark labels */
   else if ( strcmp(axis_type, "log") == 0 )
      {
      axis_min = floor(tick_values[0]);
      axis_max = ceil(tick_values[nticks-1]);
      ndecades = roundint(axis_max - axis_min);
      if ( ndecades <= 10 )
         nticks = ndecades + 1;
      else
         {
         axis_min = tick_values[0];
         axis_max = tick_values[nticks-1];
         }

      xscale = (x2_screen - x1_screen)/(nticks - 1);
      yscale = (y2_screen - y1_screen)/(nticks - 1);

      increment = (tick_values[nticks-1] - tick_values[0])/(nticks - 1);
      width_tick_labels = 0.0;
      for ( i=1; i<=nticks; i++ )
         {
         xtick = xstart_screen + ix_sign * (i - 1) * xscale;
         ytick = ystart_screen + iy_sign * (i - 1) * yscale;

         logtickvalue = roundint(axis_min 
                                 + (i - 1)*(axis_max - axis_min)/(nticks - 1));
         tickvalue = pow(10.0, (double) logtickvalue);
         memset(string, 0, sizeof(string));
         snprintf(string, sizeof(string), "%g", tickvalue);

         x = xtick + xoffset;
         y = ytick + yoffset;
         jlp_gseg1->GSEG_DrawTickLabels(string, x, y, anchor, 
                                        canvas_fg_color0, canvas_bg_color0, 0, 
                                        &x1, &y1, &x2, &y2);

         if ( x2 - x1 > width_tick_labels )
            width_tick_labels = x2 - x1;
         }
      }

return(width_tick_labels);
}
