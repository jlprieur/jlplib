/*******************************************************************************
*
* DrawTickMarks3d.c
*
* Draws tick marks and tick-mark labels for 3-dimensional plots.
*
* Copyright © 2008, 2009, 2010, 2011 Spencer A. Buckner
* http://savannah.gnu.org/projects/gsegrafix
*
* This file is part of GNUGraphics.
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

void JLP_GsegAxes::DrawTickMarks3d (void)
{
/* Declare variables */
int quadrant, i, nxticks, nyticks, nzticks, nticks;
int reversed_axis0 = 0;
double origin[3], axis1[3], axis2[3], axis3[3];
double xmin, xmax, ymin, ymax, zmin, zmax, delta, offset1, offset2,
       *tickvalues, xscale, yscale, axismin,
       x1, x2, y1, y2, x11, y11, x22, y22, angle;
char text_anchor0[64], axis_type0[64];

/* Check plot box on */
   if ( strcmp(p_plot_param->plot_box, "off") == 0 )
      return;

/* Get quadrant */
   quadrant = p_plot_param_3d->quadrant;

/* Get axes */
   for ( i=1; i<=3; i++ )
      {
      axis1[i-1] = p_plot_param_3d->axis1[i-1];
      axis2[i-1] = p_plot_param_3d->axis2[i-1];
      axis3[i-1] = p_plot_param_3d->axis3[i-1];
      }

/* Get origin */
   for ( i=1; i<=3; i++ )
      origin[i-1] = p_plot_param_3d->origin[i-1];

/* Get minimum and maximum axis values */
   nxticks = p_ticklabels->nxvalues;
   nyticks = p_ticklabels->nyvalues;
   nzticks = p_ticklabels->nzvalues;
   xmin = p_ticklabels->xvalues[0];
   xmax = p_ticklabels->xvalues[nxticks-1];
   ymin = p_ticklabels->yvalues[0];
   ymax = p_ticklabels->yvalues[nyticks-1];
   zmin = p_ticklabels->zvalues[0];
   zmax = p_ticklabels->zvalues[nzticks-1];
   xmin = xmin - p_ticklabels->xoffset1;
   xmax = xmax + p_ticklabels->xoffset2;
   ymin = ymin - p_ticklabels->yoffset1;
   ymax = ymax + p_ticklabels->yoffset2;
   zmin = zmin - p_ticklabels->zoffset1;
   zmax = zmax + p_ticklabels->zoffset2;

/* Axis 1 tick-mark parameters */
   if ( quadrant == 1 )
      {
      x1 = origin[1] + axis2[1];
      y1 = origin[2] - axis2[2];
      x2 = origin[1] + axis1[1] + axis2[1];
      y2 = origin[2] - axis1[2] - axis2[2];
      xscale =  axis1[1]/(xmax - xmin);
      yscale = -axis1[2]/(xmax - xmin);
      axismin = xmin;
      delta = (xmax - xmin)/(nxticks - 1.0);
      nticks = nxticks;
      tickvalues = &p_ticklabels->xvalues[0];
      offset1 = p_ticklabels->xoffset1;
      offset2 = p_ticklabels->xoffset2;
      }

   else if ( quadrant == 2 )
      {
      x1 = origin[1] + axis2[1];
      y1 = origin[2] - axis2[2];
      x2 = origin[1] + axis1[1] + axis2[1];
      y2 = origin[2] - axis1[2] - axis2[2];
      xscale =  axis1[1]/(ymax - ymin);
      yscale = -axis1[2]/(ymax - ymin);
      axismin = ymin;
      delta = (ymax - ymin)/(nyticks - 1.0);
      nticks = nyticks;
      tickvalues = &p_ticklabels->yvalues[0];
      offset1 = p_ticklabels->yoffset1;
      offset2 = p_ticklabels->yoffset2;
      }

   else if ( quadrant == 3 )
      {
      x1 = origin[1] + axis1[1] + axis2[1];
      y1 = origin[2] - axis1[2] - axis2[2];
      x2 = origin[1] + axis2[1];
      y2 = origin[2] - axis2[2];
      xscale = -axis1[1]/(xmax - xmin);
      yscale =  axis1[2]/(xmax - xmin);
      axismin = xmin;
      delta = (xmax - xmin)/(nxticks - 1.0);
      nticks = nxticks;
      tickvalues = &p_ticklabels->xvalues[0];
      offset1 = p_ticklabels->xoffset1;
      offset2 = p_ticklabels->xoffset2;
      }

   else if ( quadrant == 4 )
      {
      x1 = origin[1] + axis1[1] + axis2[1];
      y1 = origin[2] - axis1[2] - axis2[2];
      x2 = origin[1] + axis2[1];
      y2 = origin[2] - axis2[2];
      xscale = -axis1[1]/(ymax - ymin);
      yscale =  axis1[2]/(ymax - ymin);
      axismin = ymin;
      delta = (ymax - ymin)/(nyticks - 1.0);
      nticks = nyticks;
      tickvalues = &p_ticklabels->yvalues[0];
      offset1 = p_ticklabels->yoffset1;
      offset2 = p_ticklabels->yoffset2;
      }


   /* Draw axis1 tick marks and labels */
   if ( ((quadrant == 1 || quadrant == 3) 
        && strcmp(p_plot_param->x_tick_marks, "on") == 0) ||
        ((quadrant == 2 || quadrant == 4) 
        && strcmp(p_plot_param->y_tick_marks, "on") == 0) )
      {
      angle = atan2(axis2[2], axis2[1]) - PI;
      DrawTickMarks("linear", minor_ticks_flag, 0, x1, y1, x2, y2,
                    nticks, tickvalues, offset1, offset2,
                    angle, reversed_axis0);

      if ( ((quadrant == 1 || quadrant == 3) 
           && strcmp(p_plot_param->x_tick_labels, "on") == 0) ||
           ((quadrant == 2 || quadrant == 4) 
           && strcmp(p_plot_param->y_tick_labels, "on") == 0) )
         {
         strcpy(axis_type0, "linear");
         strcpy(text_anchor0, "NORTH_WEST");
         width_axis1_tick_labels = DrawTickLabels(axis_type0, x1, y1, x2, y2,
                                   nticks, tickvalues, offset1, offset2,
                                   -tick_major_1 * cos(angle), 
                                   tick_major_1 * sin(angle),
                                   text_anchor0, reversed_axis0);

         }

      strcpy(axis_type0, "linear");
      x11 = x1 - axis2[1];
      y11 = y1 + axis2[2];
      x22 = x2 - axis2[1];
      y22 = y2 + axis2[2];
      angle = atan2(axis2[2], axis2[1]);
      DrawTickMarks(axis_type0, minor_ticks_flag, 0,
                    x11, y11, x22, y22,
                    nticks, tickvalues, offset1, offset2,
                    angle, reversed_axis0);

      x11 = x1 - axis2[1];
      y11 = y1 + axis2[2];
      x22 = x2 - axis2[1];
      y22 = y2 + axis2[2];
      angle = atan2(axis3[2], axis3[1]);
      DrawTickMarks(axis_type0, minor_ticks_flag, 0,
                    x11, y11, x22, y22,
                    nticks, tickvalues, offset1, offset2,
                    angle, reversed_axis0);

      x11 = x1 - axis2[1] + axis3[1];
      y11 = y1 + axis2[2] - axis3[2];
      x22 = x2 - axis2[1] + axis3[1];
      y22 = y2 + axis2[2] - axis3[2];
      angle = atan2(axis3[2], axis3[1]) - PI;
      DrawTickMarks(axis_type0, minor_ticks_flag, 0,
                    x11, y11, x22, y22,
                    nticks, tickvalues, offset1, offset2,
                    angle, reversed_axis0);
      }


   /* Axis 2 tick-mark parameters */
   if ( quadrant == 1 )
      {
      x1 = origin[1] + axis1[1];
      y1 = origin[2] - axis1[2];
      x2 = origin[1] + axis1[1] + axis2[1];
      y2 = origin[2] - axis1[2] - axis2[2];
      xscale =  axis2[1]/(ymax - ymin);
      yscale = -axis2[2]/(ymax - ymin);
      axismin = ymin;
      delta = (ymax - ymin)/(nyticks - 1.0);
      nticks = nyticks;
      tickvalues = &p_ticklabels->yvalues[0];
      offset1 = p_ticklabels->yoffset1;
      offset2 = p_ticklabels->yoffset2;
      }

   else if ( quadrant == 2 )
      {
      x1 = origin[1] + axis1[1] + axis2[1];
      y1 = origin[2] - axis1[2] - axis2[2];
      x2 = origin[1] + axis1[1];
      y2 = origin[2] - axis1[2];
      xscale = -axis2[1]/(xmax - xmin);
      yscale =  axis2[2]/(xmax - xmin);
      axismin = xmin;
      delta = (xmax - xmin)/(nxticks - 1.0);
      nticks = nxticks;
      tickvalues = &p_ticklabels->xvalues[0];
      offset1 = p_ticklabels->xoffset1;
      offset2 = p_ticklabels->xoffset2;
      }

   else if ( quadrant == 3 )
      {
      x1 = origin[1] + axis1[1] + axis2[1];
      y1 = origin[2] - axis1[2] - axis2[2];
      x2 = origin[1] + axis1[1];
      y2 = origin[2] - axis1[2];
      xscale = -axis2[1]/(ymax - ymin);
      yscale =  axis2[2]/(ymax - ymin);
      axismin = ymin;
      delta = (ymax - ymin)/(nyticks - 1.0);
      nticks = nyticks;
      tickvalues = &p_ticklabels->yvalues[0];
      offset1 = p_ticklabels->yoffset1;
      offset2 = p_ticklabels->yoffset2;
      }

   else if ( quadrant == 4 )
      {
      x1 = origin[1] + axis1[1];
      y1 = origin[2] - axis1[2];
      x2 = origin[1] + axis1[1] + axis2[1];
      y2 = origin[2] - axis1[2] - axis2[2];
      xscale =  axis2[1]/(xmax - xmin);
      yscale = -axis2[2]/(xmax - xmin);
      axismin = xmin;
      delta = (xmax - xmin)/(nxticks - 1.0);
      nticks = nxticks;
      tickvalues = &p_ticklabels->xvalues[0];
      offset1 = p_ticklabels->xoffset1;
      offset2 = p_ticklabels->xoffset2;
      }


   /* Draw axis2 tick marks and labels */
   if ( ((quadrant == 2 || quadrant == 4) && strcmp(p_plot_param->x_tick_marks, "on") == 0) ||
        ((quadrant == 1 || quadrant == 3) && strcmp(p_plot_param->y_tick_marks, "on") == 0) )
      {
      angle = atan2(axis1[2], axis1[1]) - PI;
      strcpy(axis_type0, "linear");
      DrawTickMarks(axis_type0, minor_ticks_flag, 0,
                    x1, y1, x2, y2,
                    nticks, tickvalues, offset1, offset2,
                    angle, reversed_axis0);

      if ( ((quadrant == 2 || quadrant == 4) 
           && strcmp(p_plot_param->x_tick_labels, "on") == 0) ||
           ((quadrant == 1 || quadrant == 3) 
           && strcmp(p_plot_param->y_tick_labels, "on") == 0) )
         {
         width_axis2_tick_labels = DrawTickLabels("linear",
                                   x1, y1, x2, y2,
                                   nticks, tickvalues, offset1, offset2,
                                   -tick_major_1 * cos(angle), 
                                   tick_major_1 * sin(angle),
                                   "NORTH_EAST", reversed_axis0);

         }

      x11 = x1 - axis1[1];
      y11 = y1 + axis1[2];
      x22 = x2 - axis1[1];
      y22 = y2 + axis1[2];
      angle = atan2(axis1[2], axis1[1]);
      DrawTickMarks("linear", minor_ticks_flag, 0,
                    x11, y11, x22, y22,
                    nticks, tickvalues, offset1, offset2,
                    angle, reversed_axis0);

      x11 = x1 - axis1[1];
      y11 = y1 + axis1[2];
      x22 = x2 - axis1[1];
      y22 = y2 + axis1[2];
      angle = atan2(axis3[2], axis3[1]);
      DrawTickMarks("linear", minor_ticks_flag, 0,
                    x11, y11, x22, y22,
                    nticks, tickvalues, offset1, offset2,
                    angle, reversed_axis0);

      x11 = x1 - axis1[1] + axis3[1];
      y11 = y1 + axis1[2] - axis3[2];
      x22 = x2 - axis1[1] + axis3[1];
      y22 = y2 + axis1[2] - axis3[2];
      angle = atan2(axis3[2], axis3[1]) - PI;
      DrawTickMarks("linear", minor_ticks_flag, 0,
                    x11, y11, x22, y22,
                    nticks, tickvalues, offset1, offset2,
                    angle, reversed_axis0);
      }


   /* Draw axis3 tick marks and labels */
   if ( strcmp(p_plot_param->z_tick_marks, "on") == 0 )
      {
      x1 = origin[1] + axis1[1];
      y1 = origin[2] - axis1[2];
      x2 = origin[1] + axis1[1] + axis3[1];
      y2 = origin[2] - axis1[2] - axis3[2];
      angle = atan2(axis1[2], axis1[1]) - PI;
      DrawTickMarks("linear", minor_ticks_flag, 0,
                    x1, y1, x2, y2,
                    p_ticklabels->nzvalues, &p_ticklabels->zvalues[0],
                    p_ticklabels->zoffset1, p_ticklabels->zoffset2,
                    angle, reversed_axis0);

      if ( strcmp(p_plot_param->z_tick_labels, "on") == 0 )
         {
         width_axis3_tick_labels = DrawTickLabels("linear",
                                   x1, y1, x2, y2,
                                   p_ticklabels->nzvalues, 
                                   &p_ticklabels->zvalues[0],
                                   p_ticklabels->zoffset1, 
                                   p_ticklabels->zoffset2,
                                   -tick_major_1, 0.0, "EAST", reversed_axis0);

         }

      x1 = origin[1];
      y1 = origin[2];
      x2 = origin[1] + axis3[1];
      y2 = origin[2] - axis3[2];
      angle = atan2(axis1[2], axis1[1]);
      DrawTickMarks("linear", minor_ticks_flag, 0,
                    x1, y1, x2, y2,
                    p_ticklabels->nzvalues, &p_ticklabels->zvalues[0],
                    p_ticklabels->zoffset1, p_ticklabels->zoffset2,
                    angle, reversed_axis0);

      x1 = origin[1];
      y1 = origin[2];
      x2 = origin[1] + axis3[1];
      y2 = origin[2] - axis3[2];
      angle = atan2(axis2[2], axis2[1]);
      DrawTickMarks("linear", minor_ticks_flag, 0,
                    x1, y1, x2, y2,
                    p_ticklabels->nzvalues, &p_ticklabels->zvalues[0],
                    p_ticklabels->zoffset1, p_ticklabels->zoffset2,
                    angle, reversed_axis0);

      x1 = origin[1] + axis2[1];
      y1 = origin[2] - axis2[2];
      x2 = origin[1] + axis2[1] + axis3[1];
      y2 = origin[2] - axis2[2] - axis3[2];
      angle = atan2(axis2[2], axis2[1]) - PI;
      DrawTickMarks("linear", minor_ticks_flag, 0,
                    x1, y1, x2, y2,
                    p_ticklabels->nzvalues, &p_ticklabels->zvalues[0],
                    p_ticklabels->zoffset1, p_ticklabels->zoffset2,
                    angle, reversed_axis0);
      }

return;
}
