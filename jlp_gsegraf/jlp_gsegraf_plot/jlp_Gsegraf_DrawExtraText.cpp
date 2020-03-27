/*******************************************************************************
*
* jlp_Gsegraf_DrawExtraText.cpp
*
* Contains functions:
*    DrawExtraText
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
* Version 16/05/2007
*******************************************************************************/
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"            // JLP_GsegAxes class
#include "jlp_gseg_data.h"            // JLP_GsegData class

/******************************************************************************
*
*******************************************************************************/
int JLP_Gsegraf::DrawExtraText()
{
/* Declare variables */
int itext, status, nlines, raise_to_top, n_text_items;;
double dev_x1_box, dev_x2_box, dev_y1_box, dev_y2_box,
       xtext, ytext, ztext, xanchor, yanchor,
       plot_coords[3], window_coords[2], x1, y1, x2, y2;
double xmin0, xmax0, ymin0, ymax0, zmin0, zmax0, xscale0, yscale0;
char *text_str = NULL, text_coords_flag[4];
char text_anchor[64], axis_type0[64];
UINT32 canvas_fg_color0, canvas_bg_color0;

/* Get plot box minimum and maximum values */
 jlp_gseg_axes1->GetBoxSettingsForLinear(&dev_x1_box, &dev_x2_box,
                                         &dev_y1_box, &dev_y2_box,
                                         &xmin0, &xmax0, &ymin0, &ymax0,
                                         &zmin0, &zmax0, &xscale0, &yscale0);
 jlp_gseg_axes1->GetAxisType(axis_type0);

// Get canvas background/foreground colors :
 canvas_fg_color0 = jlp_gseg_axes1->canvas_fg_color();
 canvas_bg_color0 = jlp_gseg_axes1->canvas_bg_color();

// Get number of extra text items:
 n_text_items = jlp_gseg_data1->NExtraTextItems();

for(itext = 0; itext < n_text_items; itext++) {

// Get data for plotting the extra text 
  status = jlp_gseg_data1->GetExtraTextData(&text_str, text_anchor,
                                          &xtext, &ytext, &ztext, &nlines,
                                          text_coords_flag, itext);
  if(status) {
    fprintf(stderr, "DrawExtraText/Error getting text data for itext=%d\n", 
            itext);
    return(-1);
    }

/* Draw text */
 if ( text_str != NULL )
    {
// "absolute" coordinates:
    if ( strcmp(text_coords_flag, "abs") == 0 )
       {
       if ( strcmp(axis_type0, "3d") == 0 )
          {
          plot_coords[0] = xtext;
          plot_coords[1] = ytext;
          plot_coords[2] = ztext;
          }
       else
          {
          plot_coords[0] = xtext;
          plot_coords[1] = ytext;
          }
       jlp_gseg_axes1->GetWindowCoords(plot_coords, window_coords);
       xanchor = window_coords[0];
       yanchor = window_coords[1];
       }
// "relative" coordinates:
    else if ( strcmp(text_coords_flag, "rel") == 0 )
       {
       xanchor = (1.0 - xtext) * dev_x1_box + xtext * dev_x2_box;
       yanchor = (1.0 - ytext) * dev_y2_box + ytext * dev_y1_box;
       }

/* Check text is within plot box for absolute coordinates */
     if ( strcmp(text_coords_flag, "abs") == 0 &&
         (xanchor < 0 || yanchor < 0) ) return(-1);

/* Draw zlabel pixbuf canvas item */
    raise_to_top = 1;
    jlp_gseg1->GSEG_DrawLabel(text_str, xanchor, yanchor, nlines, 
                              canvas_fg_color0, canvas_bg_color0, 
                              text_anchor, raise_to_top, 
                              &x1, &x2, &y1, &y2);
    }

} // EOF loop on itext

 return(0);
 }
