/*******************************************************************************
*
* jlp_GsegAxes_DrawColorScale.cpp
*
* JLP
* Version 05/04/2017
*******************************************************************************/
#include <math.h>
#include "jlp_gseg_axes.h" 

/****************************************************************************
* Draw Color bar and labels for 2d color plot (called by ColorPlot2d)
*
* INPUT:
*  color_plot_type: 1=ColorPlot2d 2=ContourPlot2d
*  color0[nc0] : color levels of the contours (as defined in ContourPlot2d)
*  nc0 : number of contours (as defined in ContourPlot2d)
****************************************************************************/
void JLP_GsegAxes::DrawColorScale(const int color_plot_type,
                                  UINT32 *color0, const int nc0)
{
/* Declare variables */
int i, j, width, height;
UINT32 color;
double dev_x1_box, dev_x2_box, dev_y1_box, dev_y2_box; 
double xmin, xmax, ymin, ymax, zmin, zmax,
       xscale, yscale, zscale, dz, width_bar, height_bar,
       x1_bar, x2_bar, y1_bar, y2_bar, width_ztick_labels,
       x1, x2, y1, y2, x, y, z, ww;
char string[64], text_anchor0[64];
JLP_DPixbuf pixbuf_colorbar;
JLP_CanvasPoints *points;
UINT32 canvas_fg_color0, canvas_bg_color0;

// Get canvas foreground/background colors :
 jlp_gsegraf1->Get_canvas_fg_color(&canvas_fg_color0);
 jlp_gsegraf1->Get_canvas_bg_color(&canvas_bg_color0);

/* Get plot box settings */
 GetBoxSettingsForLinear(&dev_x1_box, &dev_x2_box, &dev_y1_box,
                         &dev_y2_box, &xmin, &xmax, &ymin,
                         &ymax, &zmin, &zmax, &xscale, &yscale);

/* Draw color-bar pixbuf */
 width_bar = 20.0;
 height_bar = 0.875*(dev_y2_box - dev_y1_box);
 x1_bar = dev_x2_box + 20.0;
 x2_bar = x1_bar + width_bar;
 y1_bar = (dev_y1_box + dev_y2_box)/2.0 + height_bar/2.0;
 y2_bar = (dev_y1_box + dev_y2_box)/2.0 - height_bar/2.0;

 if( strcmp(p_plot_param->plot_box, "on") == 0 )
    {
    width  = roundint(width_bar);
    height = roundint(height_bar);
// Draw color bar if called by ColorPlot2d:
    if(color_plot_type == 1) {
       jlp_gsegraf1->DrawColorBarFor2dPlot(x1_bar, y1_bar, width, height); 
// Else draw a series of lines if called by ContourPlot2d:
      } else {
       points = jlp_canvas_points_new(2);
       for ( i=1; i <= nc0; i++ )
         {
         points->coords[0] = x1_bar;
         points->coords[1] = y1_bar
                             + (i - 1.0)*(y2_bar - y1_bar)/(nc0 - 1.0);
         points->coords[2] = x2_bar;
         points->coords[3] = points->coords[1];
         jlp_gseg1->GSEG_DrawLine(points, color0[i-1], 2);
         }
       jlp_canvas_points_free(points);
      }

/* Draw and label color-bar tick marks */
/* Draw vertical line */
    points = jlp_canvas_points_new(4);
    points->coords[0] = x2_bar + 8.0;
    points->coords[1] = y1_bar;
    points->coords[2] = x2_bar + 8.0 + tick_major_1;
    points->coords[3] = y1_bar;
    points->coords[4] = x2_bar + 8.0 + tick_major_1;
    points->coords[5] = y2_bar;
    points->coords[6] = x2_bar + 8.0;
    points->coords[7] = y2_bar;
    jlp_gseg1->GSEG_DrawLine(points, canvas_fg_color0, 2);
    jlp_canvas_points_free(points);

/* Draw tick marks and tick-mark labels */
    if ( strcmp(p_plot_param->z_tick_marks, "on") == 0 )
       {
       ww = 180.0 * DEGTORAD;
       strcpy(p_plot_param->axis_type, "linear");
       DrawTickMarks(p_plot_param->axis_type, minor_ticks_flag, 0,
                     x2_bar + 8.0 + tick_major_1, y1_bar, 
                     x2_bar + 8.0 + tick_major_1, y2_bar,
                     p_ticklabels->nzvalues, &p_ticklabels->zvalues[0],
                     p_ticklabels->zoffset1, p_ticklabels->zoffset2, ww);

       if ( strcmp(p_plot_param->z_tick_labels, "on") == 0 )
          {
          zscale = height_bar/(zmax - zmin);
          dz = (zmax - zmin)/(p_ticklabels->nzvalues - 1);
          width_ztick_labels = 0.0;
          for ( i=1; i<=p_ticklabels->nzvalues; i++ )
             {
             memset(string, 0, sizeof(string));
             if ( fabs(p_ticklabels->zvalues[i-1]) < 0.01*dz )
                snprintf(string, sizeof(string), "%1.0f", 0.0);
             else
                snprintf(string, sizeof(string), "%g", 
                         p_ticklabels->zvalues[i-1]);

             strcpy(text_anchor0, "WEST");
             jlp_gseg1->GSEG_DrawTickLabels(string, 
                       x2_bar + 8.0 + tick_major_1 + 8.0,
                       y1_bar - (p_ticklabels->zvalues[i-1] - zmin)*zscale,
                       text_anchor0, canvas_fg_color0, canvas_bg_color0, 0,
                       &x1, &y1, &x2, &y2);

             if ( x2 - x1 > width_ztick_labels )
                   width_ztick_labels = x2 - x1;
             }
          }
       }
  }

/* Label color-bar */
  if ( strcmp(p_plot_param->plot_box, "on") == 0 
       && zlabel1 != NULL && pixbuf_zlabel.text != NULL)
     {
     x = -(dev_y1_box + dev_y2_box)/2.0;
     if ( strcmp(p_plot_param->z_tick_marks, "on") == 0 &&
          strcmp(p_plot_param->z_tick_labels, "on") == 0 )
        y = x2_bar + 8.0 + tick_major_1 + 8.0 + width_ztick_labels + 8.0;
     else
        y = x2_bar + 8.0 + tick_major_1 + 8.0;

/* Draw zlabel pixbuf canvas item (rotation -90, translation x, y) */
     strcpy(text_anchor0, "NORTH");
     jlp_gseg1->GSEG_DrawTPixbuf(&pixbuf_zlabel, 0.0, 0.0, -90., x, y, 
                                 text_anchor0);
     }

return;
}
