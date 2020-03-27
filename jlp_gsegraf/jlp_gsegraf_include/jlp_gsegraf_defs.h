/*******************************************************************************
* jlp_gsegraf_defs.h
*
* Declares structures for gsegraf
*
* This file was modified from GSEGrafix (gsegrafix-1.0.6, sept. 2011)
* Copyright © 2008, 2009, 2010, 2011 Spencer A. Buckner
* http://savannah.gnu.org/projects/gsegrafix
*
* JLP
* Version 13/10/2016
*******************************************************************************/
#ifndef jlp_gsegraf_defs_h_
#define jlp_gsegraf_defs_h_

#include <cstddef>  // NULL

#define DBLE_MAX_VALUE 1.e+12
#define DBLE_MIN_VALUE 1.e-12
#ifndef PI
#define PI 3.14159
#endif
#define DEGTORAD (PI / 180.0)

typedef struct
   {
   double xmin, xmax, ymin, ymax;
   } plot_box_data_type;

typedef struct
   {
   char axis_type[10];
   char grid[4];
// reversed axis: 0 if no_reversed, 1 if reversed (0=x,1=y,2=z)  
   int reversed_axis[3];
   char minor_ticks[4];
   char axis_scale[6];
// axis_limits[6] : 0=xmin, 1=xmax, 2=ymin, 3=ymax, 4=zmin, 5=zmax
   double axis_limits[6];
   char date_time_anchor[10];
   char plot_box[4];
   char x_tick_marks[4], y_tick_marks[4], z_tick_marks[4];
   char x_tick_labels[4], y_tick_labels[4], z_tick_labels[4];
   } plot_param_type;

// data_min_max_type (may be different from axis_limits when xmin > xmax ...)
// Should always have xmin < xmax in data_min_max_type
// (see jlp_gsegraf_ZoomIn.cpp line 39)
typedef struct
   {
   double xmin, xmax, ymin, ymax, zmin, zmax;
   } data_min_max_type;

typedef struct
   {
   int nxvalues, nyvalues, nzvalues,
       nxvalues_ref, nyvalues_ref, nzvalues_ref;
   double xvalues[11], yvalues[11], zvalues[11],
          xoffset1, xoffset2, yoffset1, yoffset2, zoffset1, zoffset2,
          xvalues_ref[11], yvalues_ref[11], zvalues_ref[11],
          xoffset1_ref, xoffset2_ref, yoffset1_ref, yoffset2_ref,
          zoffset1_ref, zoffset2_ref;
   } ticklabels_type;

typedef struct
   {
   int quadrant;
   double phi, theta, phi_ref, theta_ref, axis_length, plot_width, plot_height,
          origin[3], axis1[3], axis2[3], axis3[3], Ryz[9];
   } plot_param_3d_type;

/************************************************************************
* Re-definition of UINT32 
***********************************************************************/
/* gnome/glib types:
#include <glibconfig.h>  // guint32
#ifndef guint32
typedef unsigned int guint32;
#endif
*/
typedef unsigned int UINT32;

/************************************************************************
* Re-definition of GnomeCanvasPoints structure
* The coords field contains an array of points,
* (alternating X and Y coordinates).
* You fill the coords array directly, after creating a GnomeCanvasPoints
* with gnome_canvas_points_new();
* the structure should be destroyed with gnome_canvas_points_unref().
* NB: the GnomeCanvasPoints structure is actually managed by a reference count.
* so it won't be freed until this count reaches 0.
* To increment its reference count call gnome_canvas_points_ref()
* and to decrement it call gnome_canvas_points_unref().
*************************************************************************/
typedef struct {
        int num_points;
        double *coords;
        int ref_count;
} JLP_CanvasPoints;

// DataPixBuffer
// nx : width of bitmap array
// ny : height of bitmap array
typedef struct {
int nx, ny;
UINT32 *array;
} JLP_DPixbuf;

// TextPixBuffer
typedef struct {
char text[128];
char font_name[128];
double font_size;
UINT32 color;
} JLP_TPixbuf;

/*************************************************************************
* JLP definitions to simplify the interfacing with the jlp_splot library:
**************************************************************************/
#define NPLOTS_MAXI 64 
#define N_EXTRA_LABELS_MAXI 128

typedef struct {
double x[N_EXTRA_LABELS_MAXI];
double y[N_EXTRA_LABELS_MAXI];
char string[128 * N_EXTRA_LABELS_MAXI];
char anchor[128 * N_EXTRA_LABELS_MAXI];
UINT32 canvas_fg_color[N_EXTRA_LABELS_MAXI];
UINT32 canvas_bg_color[N_EXTRA_LABELS_MAXI];
int raise_to_top[N_EXTRA_LABELS_MAXI];
int nlabels;
} GSEG_EXTRA_LABELS;

#define MAXI_FILENAME_LENGTH 256
#define MAXI_FORMAT_LENGTH 64

/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
/* style_flag 
* 1 or 3 = "Contour plot with constant-color contour lines"
* 7 = "Contour plot with variable-color contour lines" (2d)
*     or "Find polygon minimum and maximum z coordinates" (3d)
* 8 = "bilinear interpolation"
* 9 = "nearest-neighbor interpolation"
*************************/
typedef struct
{
int gseg_plot_type;
// Name of the file containing the data (if ascii curve for example)
char filename[MAXI_FORMAT_LENGTH];
char prnt_formats[MAXI_FORMAT_LENGTH], prnt_mod_formats[MAXI_FORMAT_LENGTH];
// Points:
int npts;
double *xdata, *ydata, *zdata;
int nlinebreaks;
int *linebreak;
// Contours:
int ncontours, nxcontour, nycontour;
double *xcontour, *ycontour, *zcontour;
// Color plots:
int nxcolor, nycolor;
double *xcolor, *ycolor, *zcolor;
// Histograms (Histogram type (bin_value ="number", "fraction", or "percent")
// (bin_ref ="mean", "zero", or "integers")
char bin_value[64], bin_ref[64];
double bin_width;
// Style:
int styleflag;
int nxmesh, nymesh;
double *xmesh, *ymesh, *zmesh;
UINT32 mesh_color, contour3d_color, style_color1, style_color2;
UINT32 outline_color_rgba, fill_color_rgba, alpha_color; 
double zblack, zwhite;
char style_char1, style_char2;
int style_flag;
unsigned int style_size;
int ninterp;
} GSEG_PLOT_DATA;

typedef struct
{
/* axis_type:
* "linear"   (points, histogram, contour, and color plot types)
* "semilogx" (points plot type only)
* "semilogy"  (points plot type only)
* "loglog"  (points plot type only)
* "polar" (points plot type only)
* "3d" (points, contour, color, and mesh plot types)
*/
char axis_type[32];
// Foreground and background colors:
UINT32 canvas_fg_color, canvas_bg_color;
/* background_image_style:
* 1 = "center"
* 2 = "fill"
* 3 = "scale"
* 4 = "zoom"
*/
int background_image_style;
char background_image_file[128];
// axis_scale: "auto" or "equal"
char axis_scale[32];
// axis_limits[6] : 0=xmin, 1=xmax, 2=ymin, 3=ymax, 4=zmin, 5=zmax
double axis_limits[6];
// set_axis_limits : 0 if automatic scale, 1 if axis_limits are to be used
int set_axis_limits[6];
// High contrast scale for z axis:
int high_contrast_for_z_axis;
// reversed axis: 0 if no_reversed, 1 if reversed (0=x,1=y,2=z)  
int reversed_axis[3];
/* flag for minor ticks: 
* 0 = "off"
* 1 = "on"
*/
int minor_ticks_flag;
// view3d: phi (azimuth) and theta (elevation) (default: phi=30 deg theta=30deg)
double phi, theta;
/* grid:
* "off"
* "on1" if color is directly set with gridcolor1
* "on2" if color is set with the code of gridchar2
*/
char grid[32], gridchar1, gridchar2;
UINT32 gridcolor1;
// xlabel, ylabel, zlabel, title
// for polar plots: xlabel for angle axis, ylabel for the radial values
char xlabel[128], ylabel[128], zlabel[128], title[128];
/* date_time:
* "off"
* "north"
* "northeast"
* "southeast"
* "south"
* "southwest"
* "northwest"
* "off"
*/
char date_time[32];
// plot_box: "on" or "off"
char plot_box[32];
// x,y,z tick marks: "on" or "off"
char x_tick_marks[32], y_tick_marks[32], z_tick_marks[32];
// x,y,z tick labels: "on" or "off"
char x_tick_labels[32], y_tick_labels[32], z_tick_labels[32];
// Fonts:
char font_name[64];
double font_size_title, font_size_axis_labels, font_size_tick_labels;
double font_size_text, font_size_legend, font_size_date_time;
} GSEG_AXIS_DATA;

#endif
