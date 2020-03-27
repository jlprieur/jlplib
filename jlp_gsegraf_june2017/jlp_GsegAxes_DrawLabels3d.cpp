/*******************************************************************************
*
* jlp_GsegAxes_DrawLabels3d.c
*
* Draws plot box, axes, axis labels, and title for 3-dimensional plots.
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
*
*******************************************************************************/
void JLP_GsegAxes::DrawLabels3d (void)
{
/* Declare variables */
int i, quadrant;
double axis1[3], axis2[3], axis3[3], origin[3],
       x, y, x0, y0, x1, y1, x2, y2,
       angle1, angle2, phi, theta,
       affine1[6], affine2[6];
JLP_CanvasPoints *points;
JLP_TPixbuf pixbuf_label;
UINT32 canvas_fg_color0;

/* Get quadrant */
quadrant = p_plot_param_3d->quadrant;

/* Get azimuth and elevation */
phi   = p_plot_param_3d->phi;
if ( quadrant == 2 )
   phi = phi - 90.0;
else if ( quadrant == 3 )
   phi = phi - 180.0;
else if ( quadrant == 4 )
   phi = phi - 270.0;
theta = p_plot_param_3d->theta;
phi   = phi*DEGTORAD;
theta = theta*DEGTORAD;

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

// Get foreground color:
jlp_gsegraf1->Get_canvas_fg_color(&canvas_fg_color0);

/* Draw axes */
   if ( strcmp(p_plot_param->plot_box, "on") == 0 )
      {
      points = jlp_canvas_points_new(2);

      for ( i=1; i<=3; i++ )
         {
         points->coords[0] = origin[1];
         points->coords[1] = origin[2];

         if ( i == 1 )
            {
            points->coords[2] = origin[1] + axis1[1];
            points->coords[3] = origin[2] - axis1[2];
            }

         else if ( i == 2 )
            {
            points->coords[2] = origin[1] + axis2[1];
            points->coords[3] = origin[2] - axis2[2];
            }

         else if ( i == 3 )
            {
            points->coords[2] = origin[1] + axis3[1];
            points->coords[3] = origin[2] - axis3[2];
            }

         jlp_gseg1->GSEG_DrawLine(points, canvas_fg_color0, 2);
         }

      jlp_canvas_points_free(points);
      }


   /* Draw plot box */
   if ( strcmp( p_plot_param->plot_box, "on") == 0 )
      {
      points = jlp_canvas_points_new(6);
      points->coords[0]  = origin[1] + axis1[1];
      points->coords[1]  = origin[2] - axis1[2];
      points->coords[2]  = origin[1] + axis1[1] + axis3[1];
      points->coords[3]  = origin[2] - axis1[2] - axis3[2];
      points->coords[4]  = origin[1] + axis3[1];
      points->coords[5]  = origin[2] - axis3[2];
      points->coords[6]  = origin[1] + axis2[1] + axis3[1];
      points->coords[7]  = origin[2] - axis2[2] - axis3[2];
      points->coords[8]  = origin[1] + axis2[1];
      points->coords[9]  = origin[2] - axis2[2];
      points->coords[10] = origin[1] + axis1[1] + axis2[1];
      points->coords[11] = origin[2] - axis1[2] - axis2[2];

      jlp_gseg1->GSEG_DrawPolygon(points, 0xFFFFFF00, canvas_fg_color0, 2); 

      jlp_canvas_points_free(points);
      }


   /* Draw axis-1 label */
   if ( quadrant == 1 || quadrant == 3 )
      GSEG_CopyTPixbuf(&pixbuf_xlabel, &pixbuf_label);
   else
      GSEG_CopyTPixbuf(&pixbuf_ylabel, &pixbuf_label);

   if ( pixbuf_label.text != NULL )
      {
      x1 = origin[1] + axis2[1];
      y1 = origin[2] - axis2[2];
      x2 = origin[1] + axis1[1] + axis2[1];
      y2 = origin[2] - axis1[2] - axis2[2];
      x0 = (x1 + x2)/2.0;
      y0 = (y1 + y2)/2.0;
      angle2 = atan2(axis2[2], axis2[1]);

      x = x0 + (8.0 + width_axis1_tick_labels + 8.0)
                      * fabs(cos(angle2)) * sin(phi) * sin(theta)
             + (8.0 + width_axis1_tick_labels + 8.0)*cos(phi);
      y = y0 + (8.0 + width_axis1_tick_labels + 8.0)
                      * fabs(sin(angle2)) * sin(phi) * sin(theta)
             + (8.0 + font_size_tick_labels1 + 8.0)*sin(phi)*cos(theta);
                      
      jlp_gseg1->GSEG_DrawTPixbuf(&pixbuf_label, x, y, 0., 0., 0., "NORTH_WEST");
      }


   /* Draw axis-2 label */
   if ( quadrant == 1 || quadrant == 3 )
      GSEG_CopyTPixbuf(&pixbuf_ylabel, &pixbuf_label);
   else
      GSEG_CopyTPixbuf(&pixbuf_xlabel, &pixbuf_label);

   if ( pixbuf_label.text != NULL )
      {
      x1 = origin[1] + axis1[1];
      y1 = origin[2] - axis1[2];
      x2 = origin[1] + axis1[1] + axis2[1];
      y2 = origin[2] - axis1[2] - axis2[2];
      x0 = (x1 + x2)/2.0;
      y0 = (y1 + y2)/2.0;
      angle1 = atan2(axis1[2], axis1[1]);

      x = x0 - (8.0 + width_axis2_tick_labels + 8.0)
                  * fabs(cos(angle1)) * cos(phi) * sin(theta)
             - (8.0 + width_axis2_tick_labels + 8.0) * sin(phi);
      y = y0 + (8.0 + width_axis2_tick_labels + 8.0)
                  * fabs(sin(angle1)) * cos(phi)*sin(theta)
             + (8.0 + font_size_tick_labels1 + 8.0) * cos(phi) * cos(theta);

      jlp_gseg1->GSEG_DrawTPixbuf(&pixbuf_label, x, y, 0., 0., 0., "NORTH_EAST");
      }


   /* Draw z-axis label */
   if ( zlabel1 != NULL && pixbuf_zlabel.text != NULL )
      {
      x1 = origin[1] + axis1[1];
      y1 = origin[2] - axis1[2];
      x2 = origin[1] + axis1[1] + axis3[1];
      y2 = origin[2] - axis1[2] - axis3[2];
      x = -(y1 + y2)/2.0;
      y = x1 - 8.0 - width_axis3_tick_labels - 8.0;
/* Draw zlabel pixbuf canvas item with rotation of -90 deg and translation */
      jlp_gseg1->GSEG_DrawTPixbuf(&pixbuf_zlabel, 0.0, 0.0, -90., x, y, "SOUTH");
      }


   /* Draw plot title */
   if ( title1 != NULL && pixbuf_title.text != NULL )
      {
      x = origin[1] + (axis1[1] + axis2[1])/2.0;
      y = origin[2] - axis3[2] - 8.0;
      jlp_gseg1->GSEG_DrawTPixbuf(&pixbuf_title, x, y, 0., 0., 0., "SOUTH");
      }

   return;
   }
