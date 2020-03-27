/*******************************************************************************
*
* DrawTickMarks.c
*
* Draws linear or logarithmic major and minor tick marks.
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
#include "jlp_gseg_axes.h"

void JLP_GsegAxes::DrawTickMarks(char *axis_type, int minor_ticks_flag, 
                                 int center_flag,
                                 double x1_screen, double y1_screen, 
                                 double x2_screen, double y2_screen,
                                 int nticks, double *tick_values, 
                                 double offset1, double offset2,
                                 double tick_angle)
{
/* Declare variables */
int i, j, i1, i2, increment_type, n_minor_ticks, ndecades,
    n_offset1_minor_ticks, n_offset2_minor_ticks, xsign = 1, ysign = 1;
double axis_min, axis_max, xscale, yscale;
double  major_increment, minor_increment, x1_tick, y1_tick, ww;
JLP_CanvasPoints *points;
UINT32 canvas_fg_color0;

// Get reversed axis:
 if(p_plot_param->reversed_axis[0] == 1) {
   ww = x1_screen;
   x1_screen = x2_screen;
   x2_screen = x1_screen;
   xsign = -1;
   }
 if(p_plot_param->reversed_axis[1] == 1) {
   ww = y1_screen;
   y1_screen = y2_screen;
   y2_screen = y1_screen;
   ysign = -1;
   }
printf("ZZZAAA: xsign=%d ysign=%d\n", xsign, ysign);

// Get canvas foreground color :
 jlp_gsegraf1->Get_canvas_fg_color(&canvas_fg_color0);

 /* Correct tick_angle for screen coordinates that increase downward */
 tick_angle = -tick_angle;

 /* Draw linear-axis tick marks */
 if ( strcmp(axis_type, "linear") == 0 )
      {
      axis_min = tick_values[0] - offset1;
      axis_max = tick_values[nticks-1] + offset2;
// xscale < 0 if reversed_axis...
      xscale = (x2_screen - x1_screen) / (axis_max - axis_min);
      yscale = (y2_screen - y1_screen) / (axis_max - axis_min);
printf("ZZZAAA: xscale=%f yscale=%f\n", xscale, yscale);

      /* Calculate minor tick-mark parameters */
      if ( minor_ticks_flag == 1 )
         {
         major_increment = (tick_values[nticks-1] - tick_values[0])
                           / (nticks - 1);
         increment_type = roundint(major_increment
                           / pow(10.0, floor(log10(1.001*major_increment))));
         if ( increment_type == 1 )
            n_minor_ticks = 4;
         else if ( increment_type == 2 )
            n_minor_ticks = 3;
         else if ( increment_type == 5 )
            n_minor_ticks = 4;
         minor_increment = major_increment / (n_minor_ticks + 1);
         }

      /* Draw major tick marks */
      i1 = 2;
      i2 = nticks - 1;
      if ( offset1 > 0.0 )
         i1 = 1;
      if ( offset2 > 0.0 )
         i2 = nticks;
      points = jlp_canvas_points_new(2);
      for ( i=i1; i<=i2; i++ )
         {
         points->coords[0] = x1_screen + (tick_values[i-1] - axis_min)*xscale;
         points->coords[1] = y1_screen + (tick_values[i-1] - axis_min)*yscale;
         points->coords[2] = points->coords[0] 
                             + xsign*tick_major_1*cos(tick_angle);
         points->coords[3] = points->coords[1] 
                             + ysign*tick_major_1*sin(tick_angle);

         if ( center_flag == 1 )
            {
            points->coords[0] = points->coords[0] 
                                - xsign * 0.5 * tick_major_1 * cos(tick_angle);
            points->coords[1] = points->coords[1] 
                                - ysign * 0.5 * tick_major_1 * sin(tick_angle);
            points->coords[2] = points->coords[2] 
                                - xsign * 0.5 * tick_major_1 * cos(tick_angle);
            points->coords[3] = points->coords[3] 
                                - ysign * 0.5 * tick_major_1 * sin(tick_angle);
            }

         jlp_gseg1->GSEG_DrawLine(points, canvas_fg_color0, 1);
         }

      /* Draw minor tick marks */
      if ( minor_ticks_flag == 1 )
         {
         for ( i=1; i<nticks; i++ )
            {
            x1_tick = x1_screen + (tick_values[i-1] - axis_min)*xscale;
            y1_tick = y1_screen + (tick_values[i-1] - axis_min)*yscale;

            for ( j=1; j<=n_minor_ticks; j++ )
               {
               points->coords[0] = x1_tick + j * minor_increment * xscale;
               points->coords[1] = y1_tick + j * minor_increment * yscale;
               points->coords[2] = points->coords[0] 
                                   + xsign * tick_minor_1 * cos(tick_angle);
               points->coords[3] = points->coords[1] 
                                   + ysign * tick_minor_1 * sin(tick_angle);

               if ( center_flag == 1 )
                  {
                  points->coords[0] = points->coords[0] 
                                 - xsign * 0.5 * tick_minor_1 * cos(tick_angle);
                  points->coords[1] = points->coords[1] 
                                 - ysign * 0.5 * tick_minor_1 * sin(tick_angle);
                  points->coords[2] = points->coords[2] 
                                 - xsign * 0.5 * tick_minor_1 * cos(tick_angle);
                  points->coords[3] = points->coords[3] 
                                 - ysign * 0.5 * tick_minor_1 * sin(tick_angle);
                  }

               jlp_gseg1->GSEG_DrawLine(points, canvas_fg_color0, 1);
               }
            }
         }

      /* Draw offset minor tick marks */
      if ( minor_ticks_flag == 1 )
         {
         if ( offset1 > 0.0 )
            {
            n_offset1_minor_ticks = roundint(floor(offset1/minor_increment));
            for ( j=1; j<=n_offset1_minor_ticks; j++ )
               {
               points->coords[0] = x1_screen 
                             + (offset1 - j * minor_increment) * xscale;
               points->coords[1] = y1_screen 
                             + (offset1 - j * minor_increment) * yscale;
               points->coords[2] = points->coords[0] 
                             + xsign * tick_minor_1 * cos(tick_angle);
               points->coords[3] = points->coords[1] 
                             + ysign * tick_minor_1 * sin(tick_angle);

               if ( center_flag == 1 )
                  {
                  points->coords[0] = points->coords[0] 
                               - xsign * 0.5 * tick_minor_1 * cos(tick_angle);
                  points->coords[1] = points->coords[1] 
                               - ysign * 0.5 * tick_minor_1 * sin(tick_angle);
                  points->coords[2] = points->coords[2] 
                               - xsign * 0.5 * tick_minor_1 * cos(tick_angle);
                  points->coords[3] = points->coords[3] 
                               - ysign * 0.5 * tick_minor_1 * sin(tick_angle);
                  }

               jlp_gseg1->GSEG_DrawLine(points, canvas_fg_color0, 1);
               }
            }

         if ( offset2 > 0.0 )
            {
            n_offset2_minor_ticks = roundint(floor(offset2/minor_increment));
            for ( j=1; j<=n_offset2_minor_ticks; j++ )
               {
               points->coords[0] = x2_screen 
                             - (offset2 - j * minor_increment) * xscale;
               points->coords[1] = y2_screen 
                             - (offset2 - j * minor_increment) * yscale;
               points->coords[2] = points->coords[0] 
                             + xsign * tick_minor_1 * cos(tick_angle);
               points->coords[3] = points->coords[1] 
                             + ysign * tick_minor_1 * sin(tick_angle);

               if ( center_flag == 1 )
                  {
                  points->coords[0] = points->coords[0] 
                              - xsign * 0.5 * tick_minor_1 * cos(tick_angle);
                  points->coords[1] = points->coords[1] 
                              - ysign * 0.5 * tick_minor_1 * sin(tick_angle);
                  points->coords[2] = points->coords[2] 
                              - xsign * 0.5 * tick_minor_1 * cos(tick_angle);
                  points->coords[3] = points->coords[3] 
                              - ysign * 0.5 * tick_minor_1 * sin(tick_angle);
                  }

               jlp_gseg1->GSEG_DrawLine(points, canvas_fg_color0, 1);
               }
            }
         }

      jlp_canvas_points_free(points);
      }


   /* Draw logarithmic-axis tick marks */
   else if ( strcmp(axis_type, "log") == 0 )
      {
      ndecades = roundint(ceil(tick_values[nticks-1]) - floor(tick_values[0]));
      if ( ndecades <= 10 )
         nticks = ndecades + 1;

// xscale < 0 if reversed_axis...
      xscale = (x2_screen - x1_screen)/(nticks - 1);
// yscale < 0 if reversed_axis...
      yscale = (y2_screen - y1_screen)/(nticks - 1);

      /* Draw major tick marks */
      points = jlp_canvas_points_new(2);
      for ( i=2; i<nticks; i++ )
         {
         points->coords[0] = x1_screen + (i - 1) * xscale;
         points->coords[1] = y1_screen + (i - 1) * yscale;
         points->coords[2] = points->coords[0] 
                             + xsign * tick_major_1 * cos(tick_angle);
         points->coords[3] = points->coords[1] 
                             + ysign * tick_major_1 * sin(tick_angle);

         if ( center_flag == 1 )
            {
            points->coords[0] = points->coords[0] 
                          - xsign * 0.5 * tick_major_1 * cos(tick_angle);
            points->coords[1] = points->coords[1] 
                          - ysign * 0.5 * tick_major_1 * sin(tick_angle);
            points->coords[2] = points->coords[2] 
                          - xsign * 0.5 * tick_major_1 * cos(tick_angle);
            points->coords[3] = points->coords[3] 
                          - ysign * 0.5 * tick_major_1 * sin(tick_angle);
            }

         jlp_gseg1->GSEG_DrawLine(points, canvas_fg_color0, 1);
         }

      /* Draw minor tick marks */
      if ( minor_ticks_flag == 1 && ndecades <= 10 )
         for ( i=1; i<nticks; i++ )
            {
            x1_tick = x1_screen + (i - 1) * xscale;
            y1_tick = y1_screen + (i - 1) * yscale;

            for ( j=2; j<=9; j++ )
               {
               points->coords[0] = x1_tick + log10((double) j) * xscale;
               points->coords[1] = y1_tick + log10((double) j) * yscale;
               points->coords[2] = points->coords[0] 
                                 + xsign * tick_minor_1 * cos(tick_angle);
               points->coords[3] = points->coords[1] 
                                 + xsign * tick_minor_1 * sin(tick_angle);

               if ( center_flag == 1 )
                  {
                  points->coords[0] = points->coords[0] 
                                 - xsign * 0.5 * tick_minor_1 * cos(tick_angle);
                  points->coords[1] = points->coords[1] 
                                 - ysign * 0.5 * tick_minor_1 * sin(tick_angle);
                  points->coords[2] = points->coords[2] 
                                 - xsign * 0.5 * tick_minor_1 * cos(tick_angle);
                  points->coords[3] = points->coords[3] 
                                 - ysign * 0.5 * tick_minor_1 * sin(tick_angle);
                  }

               jlp_gseg1->GSEG_DrawLine(points, canvas_fg_color0, 1);
               }
            }

      jlp_canvas_points_free(points);
      }

return;
}
