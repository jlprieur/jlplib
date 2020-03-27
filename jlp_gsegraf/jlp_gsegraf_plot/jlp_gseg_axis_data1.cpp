/*************************************************************************
* jlp_gseg_axis_data.cpp
*
* JLP
* Version 28/07/2017
**************************************************************************/
#include <stdio.h> 
#include <string.h> 
#include "jlp_gseg_axis_data1.h"

/*************************************************************************
* Initialize GSEG_AXIS_DATA structure
**************************************************************************/
int jlp_gseg_init_axis_data(int gdev_graphic_type0, GSEG_AXIS_DATA *axdata0)
{
int i;
/* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
*/
/* axis_type:
* 1="linear"   (points, histogram, contour, and color plot types)
* 2="semilogx" (points plot type only)
* 3="semilogy"  (points plot type only)
* 4="loglog"  (points plot type only)
* 5="polar" (points plot type only)
* 6="3d" (points, contour, color, and mesh plot types)
*/
  switch(gdev_graphic_type0) {
// 1 = jlp_splot_curves
// 2 = jlp_splot_images
// 3 = wx_scrolled/jlp_splot_images
// 4 = gsegraf_2d_curves
// 5 = gsegraf_2d_images
    default:
    case 1:
    case 2:
    case 3:
    case 4:
    case 5:
      strcpy(axdata0->axis_type, "linear");
      break;
// 6 = gsegraf_3d_curves
// 7 = gsegraf_3d_images
    case 6:
    case 7:
      strcpy(axdata0->axis_type, "3d");
      break;
// 8 = gsegraf_polar_curve
    case 8:
      strcpy(axdata0->axis_type, "polar");
      break;
  }

// Foreground and background colors:
 axdata0->canvas_fg_color = 0x000000FF; // opaque black
 axdata0->canvas_bg_color = 0xFFFFFFFF; // opaque white
/* background_image_style:
* 1 = "center"
* 2 = "fill"
* 3 = "scale"
* 4 = "zoom"
*/
 axdata0->background_image_style = 1;
 strcpy(axdata0->background_image_file, "");
// axis_scale: "auto" or "equal"
 strcpy(axdata0->axis_scale, "auto");
// axis_limits
 for(i = 0; i < 6; i++) {
   axdata0->axis_limits[i] = 0;
   }
// set_axis_limits : 0 if automatic scale, 1 if axis_limits are to be used
 for(i = 0; i < 6; i++) {
//   axdata0->set_axis_limits[i] = 1;
   axdata0->set_axis_limits[i] = 0;
   }
// x_rev =reversed_axis[0]
// y_rev =reversed_axis[1]
// z_rev =reversed_axis[2]
 for(i = 0; i < 3; i++) {
   axdata0->reversed_axis[i] = 0; 
   }
/* flag for minor ticks:
* 0 = "off"
* 1 = "on"
*/
 axdata0->minor_ticks_flag = 1;
// view3d: phi (azimuth) and theta (elevation) (default: phi=30 deg theta=30deg)
 axdata0->phi = 30.;
 axdata0->theta = 30.;
/* grid:
* "off"
* "on1" if color is directly set with gridcolor1
* "on2" if color is set with the code of gridchar2
*/
 strcpy(axdata0->grid, "off");
 axdata0->gridchar1 = ' ';
 axdata0->gridchar2 = ' ';
 axdata0->gridcolor1 = 0x000000FF;  // opaque black
// xlabel, ylabel, zlabel, title
// for polar plots: xlabel for angle axis, ylabel for the radial values
 strcpy(axdata0->xlabel, "");
 strcpy(axdata0->ylabel, "");
 strcpy(axdata0->zlabel, "");
 strcpy(axdata0->title, "");
/* date_time:
* "off"
* "north"
* "northeast"
* "southeast"
* "south"
* "southwest"
* "northwest"
*/
 strcpy(axdata0->date_time, "off");
// plot_box: "on" or "off"
 strcpy(axdata0->plot_box, "on");
// x,y,z tick marks: "on" or "off"
 strcpy(axdata0->x_tick_marks, "on");
 strcpy(axdata0->y_tick_marks, "on");
 strcpy(axdata0->z_tick_marks, "on");
// x,y,z tick labels: "on" or "off"
 strcpy(axdata0->x_tick_labels, "on");
 strcpy(axdata0->y_tick_labels, "on");
 strcpy(axdata0->z_tick_labels, "on");
// Fonts: (default: Sans serif)
 strcpy(axdata0->font_name, "Sans");
 axdata0->font_size_title = 12.;
 axdata0->font_size_axis_labels = 12.;
 axdata0->font_size_tick_labels = 8.;
 axdata0->font_size_text = 11.;
 axdata0->font_size_legend = 9.;
 axdata0->font_size_title = 12.;
 axdata0->font_size_date_time = 9.;
// Min-max z axis scale by default:
 axdata0->high_contrast_for_z_axis = 0;

return(0);
}
/*************************************************************************
* Copy GSEG_AXIS_DATA structures
*
* INPUT:
*  axdata1
*
* OUTPUT:
*  axdata0
**************************************************************************/
int jlp_gseg_copy_axis_data(GSEG_AXIS_DATA *axdata0, GSEG_AXIS_DATA axdata1)
{
int i;

 strcpy(axdata0->axis_type, axdata1.axis_type);
 axdata0->background_image_style = axdata1.background_image_style;
 strcpy(axdata0->background_image_file, axdata1.background_image_file);
 strcpy(axdata0->axis_scale, axdata1.axis_scale);

 for(i = 0; i < 6; i++) {
   axdata0->axis_limits[i] = axdata1.axis_limits[i]; 
   axdata0->set_axis_limits[i] = axdata1.set_axis_limits[i]; 
   }
 for(i = 0; i < 3; i++) {
   axdata0->reversed_axis[i] = axdata1.reversed_axis[i]; 
   }

 axdata0->minor_ticks_flag = axdata1.minor_ticks_flag;
 axdata0->phi = axdata1.phi;
 axdata0->theta = axdata1.theta;
 strcpy(axdata0->grid, axdata1.grid);
 axdata0->gridchar1 = axdata1.gridchar1;
 axdata0->gridchar2 = axdata1.gridchar2;
 axdata0->gridcolor1 = axdata1.gridcolor1; 
 strcpy(axdata0->xlabel, axdata1.xlabel);
 strcpy(axdata0->ylabel, axdata1.ylabel);
 strcpy(axdata0->zlabel, axdata1.zlabel);
 strcpy(axdata0->title, axdata1.title);
 strcpy(axdata0->date_time, axdata1.date_time);
 strcpy(axdata0->plot_box, axdata1.plot_box);
 strcpy(axdata0->x_tick_marks, axdata1.x_tick_marks);
 strcpy(axdata0->y_tick_marks, axdata1.y_tick_marks);
 strcpy(axdata0->z_tick_marks, axdata1.z_tick_marks);
 strcpy(axdata0->x_tick_labels, axdata1.x_tick_labels);
 strcpy(axdata0->y_tick_labels, axdata1.y_tick_labels);
 strcpy(axdata0->z_tick_labels, axdata1.z_tick_labels);
 strcpy(axdata0->font_name, axdata1.font_name);
 axdata0->font_size_title = axdata1.font_size_title;
 axdata0->font_size_axis_labels = axdata1.font_size_axis_labels;
 axdata0->font_size_tick_labels = axdata1.font_size_tick_labels;
 axdata0->font_size_text = axdata1.font_size_text;
 axdata0->font_size_legend = axdata1.font_size_legend;
 axdata0->font_size_title = axdata1.font_size_title;
 axdata0->font_size_date_time = axdata1.font_size_date_time;
 axdata0->high_contrast_for_z_axis = axdata1.high_contrast_for_z_axis;

return(0);
}
