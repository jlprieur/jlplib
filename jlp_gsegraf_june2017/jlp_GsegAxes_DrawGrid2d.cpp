/*******************************************************************************
* jlp_GsegAxes_DrawGrid2d.cpp
*
* Draws grid lines and tick marks for linear axes of two-dimensional plots.
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

/*******************************************************************************
*
*******************************************************************************/
void JLP_GsegAxes::DrawGrid2d (void)
{
double dev_x1_box, dev_x2_box, dev_y1_box, dev_y2_box;

/* Check plot_box parameter */
#ifdef DEBUG
printf("DrawGrid2d/plot_box=%s axis_type=%s\n", 
        p_plot_param->plot_box, p_plot_param->axis_type);
printf("DrawGrid2d/grid=%s x_tick_marks=%s y_tick_marks=%s\n", 
        p_plot_param->grid, p_plot_param->x_tick_marks,
        p_plot_param->y_tick_marks);
#endif

if ( strcmp(p_plot_param->plot_box, "off") == 0 ) return;

/* Check axis_type parameter */
if ( strcmp(p_plot_param->axis_type, "loglog") == 0 ) return;

/* Get plot box minimum and maximum values */
   dev_x1_box = p_plot_box_data->xmin;
   dev_x2_box = p_plot_box_data->xmax;
   dev_y1_box = p_plot_box_data->ymin;
   dev_y2_box = p_plot_box_data->ymax;

   /* Draw grid lines */
   if ( strcmp(p_plot_param->grid, "on1") == 0 ||
        strcmp(p_plot_param->grid, "on2") == 0 )
      {
      if ( strcmp(p_plot_param->x_tick_marks, "on") == 0 &&
           (strcmp(p_plot_param->axis_type, "linear")   == 0 ||
            strcmp(p_plot_param->axis_type, "semilogy") == 0) )
         {
         DrawGrid("linear", dev_x1_box, dev_y2_box, dev_x2_box, 
                  dev_y2_box, dev_x1_box, dev_y1_box,
                  p_ticklabels->nxvalues, &p_ticklabels->xvalues[0],
                  p_ticklabels->xoffset1, p_ticklabels->xoffset2);
         }

      if ( strcmp(p_plot_param->y_tick_marks, "on") == 0 &&
           (strcmp(p_plot_param->axis_type, "linear")   == 0 ||
            strcmp(p_plot_param->axis_type, "semilogx") == 0) )
         {
         DrawGrid("linear", dev_x1_box, dev_y2_box, dev_x1_box, 
                  dev_y1_box, dev_x2_box, dev_y2_box,
                  p_ticklabels->nyvalues, &p_ticklabels->yvalues[0],
                  p_ticklabels->yoffset1, p_ticklabels->yoffset2);
         }
      }


   /* Draw x-axis tick marks */
   if ( strcmp(p_plot_param->x_tick_marks, "on") == 0 &&
        (strcmp(p_plot_param->axis_type, "linear")   == 0 ||
         strcmp(p_plot_param->axis_type, "semilogy") == 0) )
      {
      DrawTickMarks("linear", minor_ticks_flag, 0,
                    dev_x1_box, dev_y2_box, dev_x2_box, dev_y2_box,
                    p_ticklabels->nxvalues, &p_ticklabels->xvalues[0],
                    p_ticklabels->xoffset1, p_ticklabels->xoffset2,
                    90.0*DEGTORAD);   /* lower x-axis */

      DrawTickMarks("linear", minor_ticks_flag, 0,
                    dev_x1_box, dev_y1_box, dev_x2_box, dev_y1_box,
                    p_ticklabels->nxvalues, &p_ticklabels->xvalues[0],
                    p_ticklabels->xoffset1, p_ticklabels->xoffset2,
                    -90.0*DEGTORAD);   /* upper x-axis */
      }


   /* Draw y-axis tick marks */
   if ( strcmp(p_plot_param->y_tick_marks, "on") == 0 &&
        (strcmp(p_plot_param->axis_type, "linear")   == 0 ||
         strcmp(p_plot_param->axis_type, "semilogx") == 0) )
      {
      DrawTickMarks("linear", minor_ticks_flag, 0,
                    dev_x1_box, dev_y2_box, dev_x1_box, dev_y1_box,
                    p_ticklabels->nyvalues, &p_ticklabels->yvalues[0],
                    p_ticklabels->yoffset1, p_ticklabels->yoffset2,
                    0.0*DEGTORAD);   /* left y-axis */

      DrawTickMarks("linear", minor_ticks_flag, 0,
                    dev_x2_box, dev_y2_box, dev_x2_box, dev_y1_box,
                    p_ticklabels->nyvalues, &p_ticklabels->yvalues[0],
                    p_ticklabels->yoffset1, p_ticklabels->yoffset2,
                    180.0*DEGTORAD);   /* right y-axis */
      }

   return;
   }
