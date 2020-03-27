/*******************************************************************************
*
* PolarPlot.c
*
* Calculates polar plot of input data.
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
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"    // JLP_GsegAxes class

/***************************************************************************
*
* PART I 
*
***************************************************************************/
void JLP_GsegAxes::DrawPolarBox_PartI(double xorigin0, double yorigin0, 
                                      double radius0, double rmin0, 
                                      double rmax0, double rscale0)  
{
/* Declare variables */
int i, j, nrvalues, reversed_axis0 = 0; 
char text_anchor[64];
double radius_grid, ww, x, y, x1, y1, x2, y2;
double diam1, theta, theta_max, theta_minor, width_xtick_label;
char string[21];
JLP_CanvasPoints *points;
UINT32 canvas_fg_color0, canvas_bg_color0;

// Get canvas foreground/background colors :
 jlp_gsegraf1->Get_canvas_fg_color(&canvas_fg_color0);
 jlp_gsegraf1->Get_canvas_bg_color(&canvas_bg_color0);

/* Save plot-box data */
 p_plot_box_data->xmin = xorigin0 - radius0;
 p_plot_box_data->xmax = xorigin0 + radius0;
 p_plot_box_data->ymin = yorigin0 - radius0;
 p_plot_box_data->ymax = yorigin0 + radius0;
 nrvalues = p_ticklabels->nyvalues;

/* Draw grid lines */
 if(strcmp(p_plot_param->plot_box, "on") == 0 &&
      (strcmp(p_plot_param->grid, "on1") == 0 ||
       strcmp(p_plot_param->grid, "on2") == 0 ) )
    {
/* Draw constant-theta grid lines */
    if(strcmp(p_plot_param->x_tick_marks, "on") == 0)
       {
       points = jlp_canvas_points_new(2);
       for ( i=1; i<=12; i++ )
          {
          theta = (i - 1.0)*30.0*DEGTORAD;
          points->coords[0] = xorigin0;
          points->coords[1] = yorigin0;
          points->coords[2] = xorigin0 + radius0*cos(theta);
          points->coords[3] = yorigin0 - radius0*sin(theta);

          if ( gridchar1 == 'l' )
             jlp_gseg1->GSEG_DrawLine(points, gridcolor1, 1);
          else if ( gridchar1 == 'd' )
             jlp_gsegraf1->DrawDashedLine(points, gridcolor1, 1);
          else if ( gridchar1 == '.' )
             jlp_gsegraf1->DrawDottedLine(points, gridcolor1, 1);
          }
       jlp_canvas_points_free(points);
       }

/* Draw constant-r grid lines */
    if(strcmp(p_plot_param->y_tick_marks, "on") == 0)
       {
       for(i=1; i<=nrvalues; i++)
         {
         diam1 = 2. * (p_ticklabels->yvalues[i-1] - rmin0)*rscale0;
         if(gridchar1 == 'l') {
             jlp_gseg1->GSEG_DrawCircle(xorigin0, yorigin0, diam1, 0xFFFFFF00, 
                                        gridcolor1, 1);
             }
          else if(gridchar1 == 'd')
             {
             radius_grid = (p_ticklabels->yvalues[i-1] - rmin0) * rscale0;
             jlp_gsegraf1->DrawDashedCircle(xorigin0, yorigin0, radius_grid, gridcolor1);
             }
          else if(gridchar1 == '.')
             {
             radius_grid = (p_ticklabels->yvalues[i-1] - rmin0) * rscale0;
             jlp_gsegraf1->DrawDottedCircle(xorigin0, yorigin0, radius_grid, gridcolor1);
             }
          }
       }
    }


/* Draw theta tick marks */
  if(strcmp(p_plot_param->plot_box, "on") == 0 &&
        strcmp(p_plot_param->x_tick_marks, "on") == 0)
    {
    points = jlp_canvas_points_new(2);
     for ( i=1; i<=12; i++ )
       {
       theta = (i - 1)*30.0*DEGTORAD;
       points->coords[0] = xorigin0 + radius0 * cos(theta);
       points->coords[1] = yorigin0 - radius0 * sin(theta);
       points->coords[2] = xorigin0 + (radius0 - tick_major_1) * cos(theta);
       points->coords[3] = yorigin0 - (radius0 - tick_major_1) * sin(theta);

       jlp_gseg1->GSEG_DrawLine(points, canvas_fg_color0, 1);

       if(minor_ticks_flag == 1)
         {
         for(j=1; j<=5; j++)
           {
           theta_minor = ((i - 1)*30.0 + j*5.0)*DEGTORAD;
           points->coords[0] = xorigin0 + radius0 * cos(theta_minor);
           points->coords[1] = yorigin0 - radius0 * sin(theta_minor);
           points->coords[2] = xorigin0 
                               + (radius0 - tick_minor_1) * cos(theta_minor);
           points->coords[3] = yorigin0 
                               - (radius0 - tick_minor_1) * sin(theta_minor);
           jlp_gseg1->GSEG_DrawLine(points, canvas_fg_color0, 1);
           }
         }
       }
      jlp_canvas_points_free(points);
    }


/* Draw r axis and tick marks */
   if(strcmp(p_plot_param->plot_box, "on") == 0 &&
        strcmp(p_plot_param->y_tick_marks, "on") == 0)
      {
      points = jlp_canvas_points_new(2);
      theta = 45.0*DEGTORAD;
      points->coords[0] = xorigin0;
      points->coords[1] = yorigin0;
      points->coords[2] = xorigin0 + radius0*cos(theta);
      points->coords[3] = yorigin0 - radius0*sin(theta);

      jlp_gseg1->GSEG_DrawLine(points, canvas_fg_color0, 1);

      ww = 135.0 * DEGTORAD;
      DrawTickMarks("linear", minor_ticks_flag, 1,
                    xorigin0, yorigin0, 
                    xorigin0 + radius0 * cos(45.0*DEGTORAD), 
                    yorigin0 - radius0 * sin(theta),
                    p_ticklabels->nyvalues, &p_ticklabels->yvalues[0],
                    p_ticklabels->yoffset1, p_ticklabels->yoffset2, ww,
                    reversed_axis0);

      jlp_canvas_points_free(points);
      }

/* Draw plot circle */
   if(strcmp(p_plot_param->plot_box, "on") == 0){
      jlp_gseg1->GSEG_DrawCircle(xorigin0, yorigin0, 2.*radius0, 0xFFFFFF00, 
                      canvas_fg_color0, 2);
      }

/* Draw theta tick-mark labels */
   if(strcmp(p_plot_param->plot_box, "on") == 0 &&
       strcmp(p_plot_param->x_tick_marks, "on") == 0 &&
       strcmp(p_plot_param->x_tick_labels, "on") == 0 )
     {
     width_xtick_label = 0.0;
     for ( i=1; i<=12; i++ )
       {
       theta = (i - 1.0)*30.0*DEGTORAD;
       x = xorigin0 + (radius0 + 8.0) * cos(theta);
       y = yorigin0 - (radius0 + 8.0) * sin(theta);
       memset(string, 0, sizeof(string));
       snprintf(string, sizeof(string), "%d", (i - 1)*30);

       if ( i == 1 )
          strcpy(text_anchor, "WEST");
       else if ( i == 2  || i == 3 )
          strcpy(text_anchor, "SOUTH_WEST");
       else if ( i == 4 )
          strcpy(text_anchor, "SOUTH");
       else if ( i == 5  || i == 6 )
          strcpy(text_anchor, "SOUTH_EAST");
       else if ( i == 7 )
          strcpy(text_anchor, "EAST");
       else if ( i == 8  || i == 9 )
          strcpy(text_anchor, "NORTH_EAST");
       else if ( i == 10 )
          strcpy(text_anchor, "NORTH");
       else if ( i == 11  || i == 12 )
          strcpy(text_anchor, "NORTH_WEST");

       jlp_gseg1->GSEG_DrawTickLabels(string, x, y, text_anchor, 
                                      canvas_fg_color0, canvas_bg_color0, 0, 
                                      &x1, &y1, &x2, &y2);

       if(i == 1) {
          width_xtick_label = x2 - x1;
          }
       }
    }

/* Draw theta-axis label if not empty */
  if( strcmp(pixbuf_xlabel.text, "") )
    {
     if(strcmp(p_plot_param->plot_box, "on") == 0)
       {
       points = jlp_canvas_points_new(2);
       for ( i=1; i<=9; i++ )
         {
         theta_max = 45.0/(radius0 + 20.0 + width_xtick_label);
         theta = (i - 1)*theta_max/10.0;
         points->coords[0] = xorigin0 
                         + (radius0 + 8.0 + width_xtick_label + 10.0)*cos(theta);
         points->coords[1] = yorigin0 
                         - (radius0 + 8.0 + width_xtick_label + 10.0)*sin(theta);
         theta = i*theta_max/10.0;
         points->coords[2] = xorigin0 
                         + (radius0 + 8.0 + width_xtick_label + 10.0)*cos(theta);
         points->coords[3] = yorigin0 
                         - (radius0 + 8.0 + width_xtick_label + 10.0)*sin(theta);
         jlp_gseg1->GSEG_DrawLine(points, canvas_fg_color0, 1);
         }

        theta = 0.9*theta_max;
        points->coords[0] = xorigin0 
                      + (radius0 + 8.0 + width_xtick_label + 10.0)*cos(theta);
        points->coords[1] = yorigin0 
                      - (radius0 + 8.0 + width_xtick_label + 10.0)*sin(theta);
        theta = theta_max;
        points->coords[2] = xorigin0 
                      + (radius0 + 8.0 + width_xtick_label + 10.0)*cos(theta);
        points->coords[3] = yorigin0 
                      - (radius0 + 8.0 + width_xtick_label + 10.0)*sin(theta);

        jlp_gseg1->GSEG_DrawLineWithArrow(points, canvas_fg_color0, 1);
        jlp_canvas_points_free(points);
        }

        x = xorigin0 + radius0 + 28.0 + width_xtick_label;
        y = yorigin0;
        strcpy(text_anchor, "WEST");
        jlp_gseg1->GSEG_DrawTPixbuf(&pixbuf_xlabel, x, y, 0., 0., 0., 
                                    text_anchor);

      }

/* Draw r-axis label if not empty */
  if( strcmp(pixbuf_ylabel.text, "") )
      {
      if ( strcmp(p_plot_param->plot_box, "on") == 0 )
         {
         points = jlp_canvas_points_new(2);
         theta = 45.0*DEGTORAD;
         points->coords[0] = xorigin0 + (radius0 + 15.0)*cos(theta);
         points->coords[1] = yorigin0 - (radius0 + 15.0)*sin(theta);
         points->coords[2] = xorigin0 + (radius0 + 60.0)*cos(theta);
         points->coords[3] = yorigin0 - (radius0 + 60.0)*sin(theta);

         jlp_gseg1->GSEG_DrawLineWithArrow(points, canvas_fg_color0, 1);
         jlp_canvas_points_free(points);
         }

      theta = 45.0*DEGTORAD;
      x = xorigin0 + (radius0 + 70.0)*cos(theta);
      y = yorigin0 - (radius0 + 70.0)*sin(theta);
      strcpy(text_anchor, "WEST");
      jlp_gseg1->GSEG_DrawTPixbuf(&pixbuf_ylabel, x, y, 0., 0., 0., 
                                  text_anchor);

      }


/* Draw plot title if not empty */
  if( strcmp(pixbuf_title.text, "") )
      {
      x = xorigin0;
      if ( strcmp(p_plot_param->plot_box, "on") == 0 &&
           strcmp(p_plot_param->x_tick_marks, "on") == 0 &&
           strcmp(p_plot_param->x_tick_labels, "on") == 0 )
         y = yorigin0 - radius0 - 8.0 - font_size_tick_labels1 - 8.0;
      else
         y = yorigin0 - radius0 - 8.0;
      strcpy(text_anchor, "SOUTH");
      jlp_gseg1->GSEG_DrawTPixbuf(&pixbuf_title, x, y, 0., 0., 0., 
                                  text_anchor);
      }

return;
}
/***************************************************************************
*
* PART II 
*
***************************************************************************/
void JLP_GsegAxes::DrawPolarBox_PartII(double xorigin0, double yorigin0, 
                                      double rmin0, double rmax0, 
                                      double rscale0)
{
UINT32 canvas_fg_color0, canvas_bg_color0;
double dr, x, y, xtext1, xtext2, ytext1, ytext2, theta;
int i, nrvalues;
char string[64];
UINT32 fill_color0, outline_color0;

 nrvalues = p_ticklabels->nyvalues;

// Get canvas foreground/background colors :
 jlp_gsegraf1->Get_canvas_fg_color(&canvas_fg_color0);
 jlp_gsegraf1->Get_canvas_bg_color(&canvas_bg_color0);

/* Draw r tick-mark labels on translucent rectangles */
 if ( strcmp(p_plot_param->plot_box, "on")      == 0 &&
      strcmp(p_plot_param->y_tick_marks, "on")  == 0 &&
      strcmp(p_plot_param->y_tick_labels, "on") == 0 )
      {
      dr = (rmax0 - rmin0)/(nrvalues - 1);
      for ( i=1; i<=nrvalues; i++ )
         {
         theta = 45.0*DEGTORAD;
         x = xorigin0 + (p_ticklabels->yvalues[i-1] - rmin0)*rscale0*cos(theta);
         y = yorigin0 - (p_ticklabels->yvalues[i-1] - rmin0)*rscale0*sin(theta);

         memset(string, 0, sizeof(string));
         if ( fabs(p_ticklabels->yvalues[i-1]) < 0.01*dr )
            snprintf(string, sizeof(string), "%1.0f", 0.0);
         else
            snprintf(string, sizeof(string), "%g", p_ticklabels->yvalues[i-1]);

// raise_to_top = 1 here:
         jlp_gseg1->GSEG_DrawTickLabels(string, x - 10.0, y, "EAST",
                                        canvas_fg_color0, canvas_bg_color0, 1, 
                                        &xtext1, &ytext1, &xtext2, &ytext2);

// Greyish color by playing with the transparency:
         fill_color0 = canvas_bg_color0 - 0xFF + 0xC0;
         outline_color0 = canvas_bg_color0 - 0xFF + 0xC0;
         jlp_gseg1->GSEG_DrawRectangle(xtext1, xtext2, ytext1, ytext2, 
                                       fill_color0, outline_color0, 1);

// JLP2017: I do it again (necessary for wxwidgets)
// raise_to_top = 1 here:
         jlp_gseg1->GSEG_DrawTickLabels(string, x - 10.0, y, "EAST",
                                        canvas_fg_color0, canvas_bg_color0, 1, 
                                        &xtext1, &ytext1, &xtext2, &ytext2);

         }
      }

return;
}
