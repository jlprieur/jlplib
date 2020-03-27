/*******************************************************************************
*
* DrawBackgroundImage.c
*
* Draws plot background image.
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

void JLP_GsegAxes::DrawBackgroundImage (void)
{
/* Declare variables */
int width_plot, height_plot;
double dev_x1_box, dev_x2_box, dev_y1_box, dev_y2_box, xcenter0, ycenter0;
double radius0, rmin0, rmax0, rscale0, outer_radius;
char text_anchor0[64];
UINT32 canvas_bg_color0;

/* Check background_image_file */
 if ( background_image_file == NULL )
    return;

/* Get plot box minimum and maximum values */
 if ( strcmp(p_plot_param->axis_type, "linear")   == 0 ||
     strcmp(p_plot_param->axis_type, "semilogx") == 0 ||
     strcmp(p_plot_param->axis_type, "semilogy") == 0 ||
     strcmp(p_plot_param->axis_type, "loglog")   == 0 )
   {
    dev_x1_box = p_plot_box_data->xmin;
    dev_x2_box = p_plot_box_data->xmax;
    dev_y1_box = p_plot_box_data->ymin;
    dev_y2_box = p_plot_box_data->ymax;
    xcenter0 = (dev_x1_box + dev_x2_box)/2.0;
    ycenter0 = (dev_y1_box + dev_y2_box)/2.0;
    width_plot = roundint(dev_x2_box - dev_x1_box);
    height_plot = roundint(dev_y2_box - dev_y1_box);
   }

 else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
   {
/* Specify plot-circle location and radius0 */
    GetBoxSettingsForPolar(&xcenter0, &ycenter0, &radius0, &rmin0, &rmax0, 
                           &rscale0);

    width_plot = roundint(2.0 * radius0);
    height_plot = width_plot;
   }

/* Make image pixels outside plot circle transparent */
  if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
    outer_radius = radius0;
  else
    outer_radius = 0.;

/* Draw background image on canvas */
  strcpy(text_anchor0, "CENTER");
  jlp_gseg1->GSEG_DrawBackgroundImage(background_image_file, 
                                      background_image_style,
                                      xcenter0, ycenter0, width_plot, 
                                      height_plot, outer_radius, text_anchor0);

// Draw frame around the image:
   jlp_gsegraf1->Get_canvas_bg_color(&canvas_bg_color0);
   jlp_gseg1->GSEG_DrawRectangle(xcenter0 - radius0, xcenter0 + radius0, 
                                 ycenter0 + radius0, ycenter0 - radius0, 
                                 0xFFFFFF00, canvas_bg_color0, 2);

return;
}
