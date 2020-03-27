/*******************************************************************************
*
* jlp_gseg_utils.h
*
* This file was modified from GSEGrafix (gsegrafix-1.0.6, sept. 2011)
* Copyright © 2008, 2009, 2010, 2011 Spencer A. Buckner
* http://savannah.gnu.org/projects/gsegrafix
*
* JLP
* Version 28/03/2017
*******************************************************************************/
#ifndef jlp_gseg_utils_h_
#define jlp_gseg_utils_h_

// Declaration of some structures used here
#include "jlp_gsegraf_defs.h"

// jlp_gseg_canvas.cpp"
JLP_CanvasPoints *jlp_canvas_points_new(const int npts0);
void jlp_canvas_points_free(JLP_CanvasPoints *jlp_canvas_points0);

// In "jlp_gseg_pixbuf.cpp"
// DataPixBuffer
int GSEG_NewDPixbuf(JLP_DPixbuf *pixbuf0, int nx0, int ny0);
int GSEG_PutPixelDPixbuf(JLP_DPixbuf *pixbuf0, int ix0, int iy0, UINT32 color0);
int GSEG_DrawDPixbuf(JLP_DPixbuf *pixbuf0, double x0, double y0,
                    const char *anchor0);
int GSEG_FreeDPixbuf(JLP_DPixbuf *pixbuf0);
// TextPixBuffer
int GSEG_NewTPixbuf(JLP_TPixbuf *pixbuf0, char *text0,
                    const char *font0, const double font_size0, 
                    const UINT32 color0);
int GSEG_DrawTPixbuf(JLP_TPixbuf *pixbuf0, double x0, double y0,
                    const double angle0, const double dx0, const double dy0,
                    const char *anchor0);
int GSEG_CopyTPixbuf(JLP_TPixbuf *pixbuf0, JLP_TPixbuf *pixbuf1);

/* jlp_gseg_utils.cpp */
double   min ( int n, double *x );
double   max ( int n, double *x );
int      roundint ( double  x );
double   dot ( double *vector1, double *vector2 );
double * cross ( double *vector1, double *vector2 );
double * multiply_mv ( double *matrix, double *vector );
double * multiply_mm ( double *matrix1, double *matrix2 );
void     interp1 ( int n, int ni, double *x, double *y, double *xi, double *yi );
void     interp2 ( int nx, int ny, int ni, double *x, double *y, double *z,
                   double *xi, double *yi, double *zi );
double   interp_rect ( double x1, double x2, double y1, double y2, double xi );
UINT32 interp_color_spectrum (double fraction0, int n_color_spectrum0,
                              UINT32 *color_spectrum0);
int      find_indices ( int n1, int n2, double *x, double xi );
int      search_compare_ascending ( const void *p1, const void *p2 );
int      search_compare_descending ( const void *p1, const void *p2 );
char *   gseg_get_string(char *get_string, char *string, unsigned int *i1_str, 
                         unsigned int *i2_str, unsigned int *size, int flag );

/* jlp_gseg_clip.cpp */
int      Clip2d(double xmin, double xmax, double ymin, double ymax, 
                double *line_coords );
int      ClipPolar(double rmin, double rmax, double *line_coords );
int      Clip3d(double xmin, double xmax, double ymin, double ymax, 
                double zmin, double zmax, double *line_coords);

/* In main program files (Dialogs.c) */
void JLP_ErrorDialog (const char *message_str);

#endif
