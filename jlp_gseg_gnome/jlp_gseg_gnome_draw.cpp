/*******************************************************************************
*
* jlp_gseg_gnome_draw.cpp
* Drawing routines of JLP_Gseg_Gnome class
* JLP
* Version 30/10/2016
*******************************************************************************/
#include <stdlib.h>
#include <math.h>
#include "jlp_gseg_gnome.h"

/*************************************************************************
* Clear screen (not used for gnome since not needed) 
*
*************************************************************************/
void JLP_Gseg_Gnome::GSEG_Clear()
{
}
/*************************************************************************
* Specify window minimum and maximum values
*
*************************************************************************/
void JLP_Gseg_Gnome::GSEG_SetWindowLimits(const int x0, const int width0,
                                         const int y0, const int height0)
{
  p_parent_window_data.x = x0;
  p_parent_window_data.width = width0;
  p_parent_window_data.y = y0;
  p_parent_window_data.height =  height0 + parent_height_menu_bar;

return;
}
/*************************************************************************
* Get window minimum and maximum values
*
*************************************************************************/
int JLP_Gseg_Gnome::GSEG_GetWindowLimits(int *x0, int *width0, int *y0, 
                                         int *height0)
{
  *x0 = p_parent_window_data.x;
  *width0 = p_parent_window_data.width;
  *y0 = p_parent_window_data.y;
  *height0 = p_parent_window_data.height - parent_height_menu_bar;

return(0);
}
/**************************************************************************
* JLP interface with gnome: straight line (GNOME_TYPE_CANVAS_LINE)
**************************************************************************/
void JLP_Gseg_Gnome::GSEG_DrawLine(JLP_CanvasPoints *points0, 
                                   UINT32 fill_color_rgba0, 
                                   unsigned int line_width0 )
{
/* Declare variables */
GnomeCanvasItem *line;
GnomeCanvasPoints *my_gnome_points;
int i, npts;

npts = points0->num_points;

/* Check linewidth and number of points */
 if((line_width0 == 0) || (npts == 0)) return;

// Allocate memory and transfer data to GNOME structure:
my_gnome_points = gnome_canvas_points_new(npts);
for(i = 0; i < 2 * npts; i++) my_gnome_points->coords[i] = points0->coords[i];

/* Draw line */
line = gnome_canvas_item_new(parent_group,
                             GNOME_TYPE_CANVAS_LINE,
                             "points", my_gnome_points,
                             "fill_color_rgba", fill_color_rgba0,
                             "width_pixels", line_width0,
                             NULL);

// Free memory
gnome_canvas_points_unref(my_gnome_points);

return;
}
/**************************************************************************
* JLP interface with gnome: straight line (GNOME_TYPE_CANVAS_LINE)
**************************************************************************/
void JLP_Gseg_Gnome::GSEG_DrawLineWithArrow(JLP_CanvasPoints *points0, 
                                            UINT32 fill_color_rgba0, 
                                            unsigned int line_width0 )
{
/* Declare variables */
GnomeCanvasItem *line;
GnomeCanvasPoints *my_gnome_points;
int i, npts;

npts = points0->num_points;

/* Check linewidth and number of points */
 if((line_width0 == 0) || (npts == 0)) return;

// Allocate memory and transfer data to GNOME structure:
my_gnome_points = gnome_canvas_points_new(npts);
for(i = 0; i < 2 * npts; i++) my_gnome_points->coords[i] = points0->coords[i];

/* Draw line with an arrow at the end */
  line = gnome_canvas_item_new(parent_group,
                               GNOME_TYPE_CANVAS_LINE,
                               "points", my_gnome_points,
                               "fill_color_rgba", fill_color_rgba0,
                               "width_pixels", line_width0,
                               "last_arrowhead", TRUE,
                               "arrow_shape_a", 10.0,
                               "arrow_shape_b", 12.0,
                               "arrow_shape_c", 4.0,
                               NULL);


// Free memory
gnome_canvas_points_unref(my_gnome_points);

return;
}
/**************************************************************************
* JLP interface with gnome: polygon (GNOME_TYPE_CANVAS_POLYGON)
**************************************************************************/
void JLP_Gseg_Gnome::GSEG_DrawPolygon(JLP_CanvasPoints *points0, 
                                      UINT32 fill_color_rgba0, 
                                      UINT32 outline_color_rgba0, 
                                      unsigned int line_width0)
{
/* Declare variables */
 GnomeCanvasItem *polygon;
 GnomeCanvasPoints *my_gnome_points;
 int i, npts;

 npts = points0->num_points;

/* Check the number of points */
 if(npts == 0) return;

// Allocate memory and transfer data to GNOME structure:
 my_gnome_points = gnome_canvas_points_new(npts);
 for(i = 0; i < 2 * npts; i++) my_gnome_points->coords[i] = points0->coords[i];

/* Draw polygon */
if(line_width0 != 0) {
 polygon = gnome_canvas_item_new(parent_group,
                                 GNOME_TYPE_CANVAS_POLYGON,
                                 "points", my_gnome_points,
                                 "fill_color_rgba", fill_color_rgba0,
                                 "outline_color_rgba", outline_color_rgba0, 
                                 "width_pixels", line_width0,
                                 NULL);
 } else {
 polygon = gnome_canvas_item_new(parent_group,
                                 GNOME_TYPE_CANVAS_POLYGON,
                                 "points", my_gnome_points,
                                 "fill_color_rgba", fill_color_rgba0,
                                 "outline_color_rgba", outline_color_rgba0, 
                                 NULL);
  }

// Free memory
 gnome_canvas_points_unref(my_gnome_points);

 return;
}
/************************************************************************
*
************************************************************************/
void JLP_Gseg_Gnome::GSEG_DrawRectangle(double x1, double x2, 
                                        double y1, double y2,
                                        const UINT32 fill_color_rgba,
                                        const UINT32 outline_color_rgba,
                                        const unsigned int pixel_width )
{   
 GnomeCanvasItem *rectangle;

 rectangle = gnome_canvas_item_new(parent_group,
                                   GNOME_TYPE_CANVAS_RECT,
                                   "x1", x1, 
                                   "x2", x2,
                                   "y1", y1,
                                   "y2", y2,
                                   "fill_color_rgba", fill_color_rgba,
                                   "outline_color_rgba", outline_color_rgba,
                                   "width_pixels", pixel_width,
                                   NULL);
return;
}
/************************************************************************
*
* INPUT:
*  x, y : center coordinates
*  diameter : diameter
************************************************************************/
void JLP_Gseg_Gnome::GSEG_DrawCircle(double x, double y, 
                                     const unsigned int diameter,
                                     UINT32 fill_color_rgba, 
                                     UINT32 outline_color_rgba, 
                                     const unsigned int pixel_width)
{
/* Declare variables */
double r;
GnomeCanvasItem *circle;

/* Draw circle */
 if ( diameter == 0 )
    return;
 else
    {
    r = 0.5* diameter;
    circle = gnome_canvas_item_new(parent_group,
                                   GNOME_TYPE_CANVAS_ELLIPSE,
                                   "x1", x - r,
                                   "x2", x + r,
                                   "y1", y - r,
                                   "y2", y + r,
                                   "fill_color_rgba", fill_color_rgba,
                                   "outline_color_rgba", outline_color_rgba,
                                   "width_pixels", pixel_width,
                                   NULL);
    }
return;
}
/************************************************************************
*
************************************************************************/
void JLP_Gseg_Gnome::GSEG_DrawEllipse(double x1, double x2, double y1, 
                                      double y2, UINT32 fill_color_rgba,
                                      UINT32 outline_color_rgba, 
                                      unsigned int pixel_width )
{
GnomeCanvasItem *ellipse;

 ellipse = gnome_canvas_item_new(parent_group,
                                 GNOME_TYPE_CANVAS_ELLIPSE,
                                 "x1", x1,
                                 "x2", x2,
                                 "y1", y1,
                                 "y2", y2,
                                 "fill_color_rgba", fill_color_rgba,
                                 "outline_color_rgba", outline_color_rgba,
                                 "width_pixels", pixel_width,
                                 NULL);

return;
}
/************************************************************************
*
* INPUT:
*  x, y : center coordinates
*  size : side of the square 
************************************************************************/
void JLP_Gseg_Gnome::GSEG_DrawSquare(double x, double y, 
                                     const unsigned int size,
                                     UINT32 fill_color_rgba,
                                     UINT32 outline_color_rgba, 
                                     unsigned int pixel_width )
{
/* Declare variables */
 double dx, dy;
 GnomeCanvasItem *square;


/* Draw square */
  if ( size == 0 )
     return;
  else
     {
     dx = 0.5*size;
     dy = 0.5*size;

     square = gnome_canvas_item_new(parent_group,
                                    GNOME_TYPE_CANVAS_RECT,
                                    "x1", x - dx,
                                    "x2", x + dx,
                                    "y1", y - dy,
                                    "y2", y + dy,
                                    "fill_color_rgba", fill_color_rgba,
                                    "outline_color_rgba", outline_color_rgba,
                                    "width_pixels", pixel_width,
                                    NULL);
     }
}
