/*******************************************************************************
*
* AxesEqual.c
*
* Converts axis scaling from "normal" (units on x and y axes not necessarily
* the same length) to "equal" (units on x and y axes equal in length).
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

/*****************************************************************************
*
*****************************************************************************/
void JLP_GsegAxes::AxesEqual ( int flag_ref )
{
/* Declare variables */
int nx, ny, flag_ref0, set_axis_limits_local[5];
double dev_x1_box, dev_y1_box, dev_x2_box, dev_y2_box,
       box_xmin, box_xmax, box_ymin, box_ymax, ratio,
       data_xmin0, data_xmax0, data_ymin0, data_ymax0, data_zmin0, data_zmax0,
       x1, y1, xmid, ymid, x2, y2;

/* Return if equal axes not requested */
 if ( strcmp(p_plot_param->axis_scale, "equal") != 0 )
    return;

/* Get plot box minimum and maximum values */
 dev_x1_box = p_plot_box_data->xmin;
 dev_x2_box = p_plot_box_data->xmax;
 dev_y1_box = p_plot_box_data->ymin;
 dev_y2_box = p_plot_box_data->ymax;
 ratio = (dev_y2_box - dev_y1_box)/(dev_x2_box - dev_x1_box);

/* Get initial tick-mark label values */
 nx = p_ticklabels->nxvalues;
 ny = p_ticklabels->nyvalues;
 box_xmin = p_ticklabels->xvalues[0];
 box_xmax = p_ticklabels->xvalues[nx-1];
 box_ymin = p_ticklabels->yvalues[0];
 box_ymax = p_ticklabels->yvalues[ny-1];

 box_xmin = box_xmin - p_ticklabels->xoffset1;
 box_xmax = box_xmax + p_ticklabels->xoffset2;
 box_ymin = box_ymin - p_ticklabels->yoffset1;
 box_ymax = box_ymax + p_ticklabels->yoffset2;

/* Compute data minimum and maximum values (each time, since those values
* may be modified in AxisLimits()
*/
 jlp_gsegraf1->GSEG_DataMinMax(high_contrast_for_z_axis1);
 jlp_gsegraf1->GSEG_GetDataMinMax(&data_xmin0, &data_xmax0, &data_ymin0, 
                                  &data_ymax0, &data_zmin0, &data_zmax0);

/* Expand x data range if necessary */
 if ( ratio*(box_xmax - box_xmin) < (box_ymax - box_ymin) )
   {
   xmid = (data_xmin0 + data_xmax0)/2.0;
   x1 = xmid - 0.6 * (data_ymax0 - data_ymin0)/ratio;
   x2 = xmid + 0.6 * (data_ymax0 - data_ymin0)/ratio;
   set_axis_limits_local[0] = 1;
   set_axis_limits_local[1] = 1;
   set_axis_limits_local[2] = 0;
   set_axis_limits_local[3] = 0;
   set_axis_limits_local[4] = 0;
   set_axis_limits_local[5] = 0;
   p_plot_param->axis_limits[0] = x1;
   p_plot_param->axis_limits[1] = x2;

// Call AxisLimits (without storing those values as reference values)
   flag_ref0 = 0;
   AxisLimits(flag_ref0, set_axis_limits_local);

   nx = p_ticklabels->nxvalues;
   box_xmin = p_ticklabels->xvalues[0];
   box_xmax = p_ticklabels->xvalues[nx-1];
   box_xmin = box_xmin - p_ticklabels->xoffset1;
   box_xmax = box_xmax + p_ticklabels->xoffset2;
   }

/* Calculate new y-axis limits */
 ymid = (data_ymin0 + data_ymax0)/2.0;
 y1 = ymid - 0.6 * ratio*(box_xmax - box_xmin);
 y2 = ymid + 0.6 * ratio*(box_xmax - box_xmin);
 set_axis_limits_local[0] = 0;
 set_axis_limits_local[1] = 0;
 set_axis_limits_local[2] = 1;
 set_axis_limits_local[3] = 1;
 set_axis_limits_local[4] = 0;
 set_axis_limits_local[5] = 0;
 p_plot_param->axis_limits[2] = y1;
 p_plot_param->axis_limits[3] = y2;

// Call AxisLimits (without storing those values as reference values)
 flag_ref0 = 0;
 AxisLimits(flag_ref, set_axis_limits_local);

 return;
}
