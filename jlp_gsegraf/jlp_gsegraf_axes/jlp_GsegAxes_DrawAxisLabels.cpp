/*******************************************************************************
*
* jlp_GsegAxes_DrawAxisLabels.cpp
*
* Draws plot box, axis labels, and title for two-dimensional plots.
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
#include "jlp_gseg_axes.h"

/******************************************************************************
* xdata2 = (xdata1 - xmin) * xscale; (cf jlp_Gsegraf_PlotData2d.cpp)
*
*******************************************************************************/
int JLP_GsegAxes::DrawXPoint(double xdata2, double *x)
{
double dev_x1_box, dev_x2_box;

// Box corners (also used for 3d (see ColorPlot3d)
 dev_x1_box = p_plot_box_data->xmin;
 dev_x2_box = p_plot_box_data->xmax;

 if(p_plot_param->reversed_axis[0] == 0)
   *x = dev_x1_box + xdata2;
 else
   *x = dev_x2_box - xdata2;

return(0);
}
/******************************************************************************
* ydata2 = (ydata1 - ymin) * yscale; (cf jlp_Gsegraf_PlotData2d.cpp)
*
*******************************************************************************/
int JLP_GsegAxes::DrawYPoint(double ydata2, double *y)
{
double dev_y1_box, dev_y2_box;

// Box corners (also used for 3d (see ColorPlot3d)
 dev_y1_box = p_plot_box_data->ymin;
 dev_y2_box = p_plot_box_data->ymax;

 if(p_plot_param->reversed_axis[0] == 0)
   *y = dev_y2_box - ydata2;
 else
   *y = dev_y1_box + ydata2;

// Case of saturation on the left:
  if(ydata2 < 0) {
     if(p_plot_param->reversed_axis[0] == 0)
       *y = dev_y2_box;
     else
       *y = dev_y1_box;
   }

return(0);
}
/******************************************************************************
*
*******************************************************************************/
void JLP_GsegAxes::DrawAxisLabels (void)
{
/* Declare variables */
 double dev_x1_box, dev_x2_box, dev_y1_box, dev_y2_box, x, y;
 UINT32 fill_color_rgba, canvas_fg_color0;

/* Get plot box minimum and maximum values */
 dev_x1_box = p_plot_box_data->xmin;
 dev_x2_box = p_plot_box_data->xmax;
 dev_y1_box = p_plot_box_data->ymin;
 dev_y2_box = p_plot_box_data->ymax;

/* Draw plot box rectangle */
 if ( strcmp(p_plot_param->plot_box, "on") == 0 )
// White, transparent color, for fill color:
    fill_color_rgba = 0xFFFFFF00;
    jlp_gsegraf1->Get_canvas_fg_color(&canvas_fg_color0);
    jlp_gseg1->GSEG_DrawRectangle(dev_x1_box, dev_x2_box, dev_y1_box, 
                                  dev_y2_box, fill_color_rgba, 
                                  canvas_fg_color0, 2);

/* Draw x-axis label if not empty */
 if ( strcmp(pixbuf_xlabel.text, "") )
    {
     x = (dev_x1_box + dev_x2_box)/2.0;
     if ( strcmp(p_plot_param->plot_box, "on") == 0 &&
       strcmp(p_plot_param->x_tick_marks, "on") == 0 &&
       strcmp(p_plot_param->x_tick_labels, "on") == 0 )
     y = dev_y2_box + 8.0 + font_size_tick_labels1 + 8.0;
  else
    y = dev_y2_box + 8.0;
    jlp_gseg1->GSEG_DrawTPixbuf(&pixbuf_xlabel, x, y, 0., 0., 0., "NORTH");
  }


/* Draw y-axis label if not empty */
 if ( strcmp(pixbuf_ylabel.text, "") )
    {
    x = -(dev_y1_box + dev_y2_box)/2.0;
   if ( strcmp(p_plot_param->plot_box, "on") == 0 &&
       strcmp(p_plot_param->y_tick_marks, "on") == 0 &&
       strcmp(p_plot_param->y_tick_labels, "on") == 0 )
       y = dev_x1_box - 8.0 - width_ytick_labels - 8.0;
   else
       y = dev_x1_box - 8.0;
// Draw ylabel pixbuf canvas item with a rotation of -90 deg and a translation
    jlp_gseg1->GSEG_DrawTPixbuf(&pixbuf_ylabel, 0.0, 0.0, -90., x, y, "SOUTH");
    }


 /* Draw plot title if not empty */
 if ( strcmp(pixbuf_title.text, "") )
    {
    x = (dev_x1_box + dev_x2_box)/2.0;
    y = dev_y1_box - 8.0;
    jlp_gseg1->GSEG_DrawTPixbuf(&pixbuf_title, x, y, 0., 0., 0., "SOUTH");
    }

 return;
 }
