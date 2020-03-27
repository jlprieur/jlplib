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
/* ZZZ OLD VERSION :
   int  nplots;
/*/
   char axis_type[10];
   char grid[4];
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
char *text;
char *font_name;
double font_size;
UINT32 color;
} JLP_TPixbuf;

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

#endif
