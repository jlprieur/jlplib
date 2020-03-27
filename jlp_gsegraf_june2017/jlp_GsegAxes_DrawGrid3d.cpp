/*******************************************************************************
*
* DrawGrid3d.c
*
* Draws grid lines for 3-dimensional plots.
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
#include "jlp_gseg_axes.h"

/******************************************************************************
*
*******************************************************************************/
void JLP_GsegAxes::DrawGrid3d (void)
{
/* Declare variables */
int quadrant, i, nticks;
double origin[3], axis1[3], axis2[3], axis3[3],
       offset1, offset2, *tickvalues,
       x1, x2, x3, x4, y1, y2, y3, y4,
       x11, y11, x22, y22, x33, y33, x44, y44;

/* Check plot_box and grid parameters */
  if ( strcmp(p_plot_param->plot_box, "off") == 0 ||
       strcmp(p_plot_param->grid, "off") == 0 )
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


/* Axis 1 tick-mark parameters */
 if ( quadrant == 1 )
    {
    x1 = origin[1] + axis2[1];
    y1 = origin[2] - axis2[2];
    x2 = origin[1] + axis1[1] + axis2[1];
    y2 = origin[2] - axis1[2] - axis2[2];
    nticks = p_ticklabels->nxvalues;
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
    nticks = p_ticklabels->nyvalues;
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
    nticks = p_ticklabels->nxvalues;
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
    nticks = p_ticklabels->nyvalues;
    tickvalues = &p_ticklabels->yvalues[0];
    offset1 = p_ticklabels->yoffset1;
    offset2 = p_ticklabels->yoffset2;
    }

/* Draw axis1 grid lines */
   if ( ((quadrant == 1 || quadrant == 3) 
        && strcmp(p_plot_param->x_tick_marks, "on") == 0) ||
        ((quadrant == 2 || quadrant == 4) 
        && strcmp(p_plot_param->y_tick_marks, "on") == 0) )
      {
      x11 = x1;
      y11 = y1;
      x22 = x2;
      y22 = y2;
      x33 = x1 - axis2[1];
      y33 = y1 + axis2[2];
      x44 = x2 - axis2[1];
      y44 = y2 + axis2[2];
      DrawGrid("linear", x11, y11, x22, y22, x33, y33,
               nticks, tickvalues, offset1, offset2);

      x11 = x1 - axis2[1];
      y11 = y1 + axis2[2];
      x22 = x2 - axis2[1];
      y22 = y2 + axis2[2];
      x33 = x1 - axis2[1] - axis3[1];
      y33 = y1 + axis2[2] - axis3[2];
      x44 = x2 - axis2[1] - axis3[1];
      y44 = y2 + axis2[2] - axis3[2];
      DrawGrid("linear", x11, y11, x22, y22, x33, y33,
               nticks, tickvalues, offset1, offset2);
      }

/* Axis 2 tick-mark parameters */
   if ( quadrant == 1 )
      {
      x1 = origin[1] + axis1[1];
      y1 = origin[2] - axis1[2];
      x2 = origin[1] + axis1[1] + axis2[1];
      y2 = origin[2] - axis1[2] - axis2[2];
      nticks = p_ticklabels->nyvalues;
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
      nticks = p_ticklabels->nxvalues;
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
      nticks = p_ticklabels->nyvalues;
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
      nticks = p_ticklabels->nxvalues;
      tickvalues = &p_ticklabels->xvalues[0];
      offset1 = p_ticklabels->xoffset1;
      offset2 = p_ticklabels->xoffset2;
      }

/* Draw axis2 grid lines */
   if ( ((quadrant == 1 || quadrant == 3) 
         && strcmp(p_plot_param->y_tick_marks, "on") == 0) ||
        ((quadrant == 2 || quadrant == 4) 
         && strcmp(p_plot_param->x_tick_marks, "on") == 0) )
      {
      x11 = x1;
      y11 = y1;
      x22 = x2;
      y22 = y2;
      x33 = x1 - axis1[1];
      y33 = y1 + axis1[2];
      x44 = x2 - axis1[1];
      y44 = y2 + axis1[2];
      DrawGrid("linear",
               x11, y11, x22, y22, x33, y33,
               nticks, tickvalues, offset1, offset2);

      x11 = x1 - axis1[1];
      y11 = y1 + axis1[2];
      x22 = x2 - axis1[1];
      y22 = y2 + axis1[2];
      x33 = x1 - axis1[1] + axis3[1];
      y33 = y1 + axis1[2] - axis3[2];
      x44 = x2 - axis1[1] + axis3[1];
      y44 = y2 + axis1[2] - axis3[2];
      DrawGrid("linear",
               x11, y11, x22, y22, x33, y33,
               nticks, tickvalues, offset1, offset2);
      }


/* Draw axis3 grid lines */
 if ( strcmp(p_plot_param->z_tick_marks, "on") == 0 )
    {
    x1 = origin[1];
    y1 = origin[2];
    x2 = origin[1] + axis3[1];
    y2 = origin[2] - axis3[2];
    x3 = origin[1] + axis1[1];
    y3 = origin[2] - axis1[2];
    x4 = origin[1] + axis1[1] + axis3[1];
    y4 = origin[2] - axis1[2] - axis3[2];
    DrawGrid("linear",
             x1, y1, x2, y2, x3, y3,
             p_ticklabels->nzvalues, &p_ticklabels->zvalues[0],
             p_ticklabels->zoffset1, p_ticklabels->zoffset2);

    x1 = origin[1];
    y1 = origin[2];
    x2 = origin[1] + axis3[1];
    y2 = origin[2] - axis3[2];
    x3 = origin[1] + axis2[1];
    y3 = origin[2] - axis2[2];
    x4 = origin[1] + axis2[1] + axis3[1];
    y4 = origin[2] - axis2[2] - axis3[2];
    DrawGrid("linear",
             x1, y1, x2, y2, x3, y3,
             p_ticklabels->nzvalues, &p_ticklabels->zvalues[0],
             p_ticklabels->zoffset1, p_ticklabels->zoffset2);
    }

return;
}
