/*******************************************************************************
*
* DrawDateTime.c
*
* Draws date-time string.
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

void JLP_GsegAxes::DrawDateTime (void)
{
/* Declare variables */
char anchor[64];
int x1, width1, y1, height1;
double xx1, xx2, yy1, yy2;
double xposition, yposition;
const char *error_str[] =
   { "Invalid date_time location." };
UINT32 canvas_fg_color0, canvas_bg_color0;

// Get canvas foreground/background colors :
 jlp_gsegraf1->Get_canvas_fg_color(&canvas_fg_color0);
 jlp_gsegraf1->Get_canvas_bg_color(&canvas_bg_color0);

   if ( strcmp(p_plot_param->date_time_anchor, "off") == 0 )
      return;


/* Get window minimum and maximum values */
   jlp_gseg1->GSEG_GetWindowLimits(&x1, &width1, &y1, &height1);

   /* Define anchor and text position */
   if ( strcmp(p_plot_param->date_time_anchor, "north") == 0 )
      {
      strcpy(anchor, "NORTH");
      xposition = (x1 + width1)/2.0;
      yposition = y1 + 2.0;
      }
   else if ( strcmp(p_plot_param->date_time_anchor, "northeast") == 0 )
      {
      strcpy(anchor, "NORTH_EAST");
      xposition = width1 - 2.0;
      yposition = y1 + 2.0;
      }
   else if ( strcmp(p_plot_param->date_time_anchor, "southeast") == 0 )
      {
      strcpy(anchor, "SOUTH_EAST");
      xposition = width1 - 2.0;
      yposition = height1 - 2.0;
      }
   else if ( strcmp(p_plot_param->date_time_anchor, "south") == 0 )
      {
      strcpy(anchor, "SOUTH");
      xposition = (x1 + width1)/2.0;
      yposition = height1 - 2.0;
      }
   else if ( strcmp(p_plot_param->date_time_anchor, "southwest") == 0 )
      {
      strcpy(anchor, "SOUTH_WEST");
      xposition = x1 + 2.0;
      yposition = height1 - 2.0;
      }
   else if ( strcmp(p_plot_param->date_time_anchor, "northwest") == 0 )
      {
      strcpy(anchor, "NORTH_WEST");
      xposition = x1 + 2.0;
      yposition = y1 + 2.0;
      }
   else
      {
      JLP_ErrorDialog(error_str[0]);
      exit(1);
      }

/* Draw text */
   jlp_gseg1->GSEG_DrawTickLabels(date_time1, xposition, yposition,
                                  anchor, canvas_fg_color0, 
                                  canvas_bg_color0, 0,
                                  &xx1, &yy1, &xx2, &yy2);

return;
}
