/*******************************************************************************
* PlotImage.c
*
* Draws image from image file.
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
* Version 02/05/2017
*******************************************************************************/
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"    // JLP_GsegAxes class
#include "jlp_gseg_data.h"    // JLP_GsegData class

/******************************************************************************
*
*******************************************************************************/
void JLP_Gsegraf::PlotImage ()
{
/* Declare variables */
int i, status; 
char text_image_anchor[64], axis_type0[64];
double dev_x1_box, dev_x2_box, dev_y1_box, dev_y2_box, ximage, yimage, zimage; 
double xanchor, yanchor, plot_coords[3], window_coords[2];
char image_filename[128], image_coords_flag[4];

jlp_gseg_axes1->GetAxisType(axis_type0);

/* Get the device coordinates of the plot box four corners */
 jlp_gseg_axes1->GetPlotBoxDataLimits(&dev_x1_box, &dev_x2_box, &dev_y1_box, 
                                      &dev_y2_box);

// Get image plot settings: 
 status = jlp_gseg_data1->GetImagePlotSettings(image_filename,
                                               image_coords_flag, 
                                               text_image_anchor,
                                               &ximage, &yimage, &zimage);
 if (status != 0 ) return;

/* Draw the image */
   if ( strcmp(image_coords_flag, "abs") == 0 )
      {
      if ( strcmp(axis_type0, "3d") == 0 )
         {
         plot_coords[0] = ximage;
         plot_coords[1] = yimage;
         plot_coords[2] = zimage;
         }
      else
         {
         plot_coords[0] = ximage;
         plot_coords[1] = yimage;
         }
      jlp_gseg_axes1->GetWindowCoords(plot_coords, window_coords);
      xanchor = window_coords[0];
      yanchor = window_coords[1];
      }
   else if ( strcmp(image_coords_flag, "rel") == 0 )
      {
      xanchor = (1.0 - ximage) * dev_x1_box + ximage * dev_x2_box;
      yanchor = (1.0 - yimage) * dev_y2_box + yimage * dev_y1_box;
      }

/* Check image is within plot box for absolute coordinates */
  if (!( strcmp(image_coords_flag, "abs") == 0 &&
               (xanchor < 0 || yanchor < 0) ))
  jlp_gseg1->GSEG_PlotImage(image_filename, xanchor, yanchor, 
                            text_image_anchor);


return;
}
