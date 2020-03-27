/********************************************************
* "jlp_splot_idv_prototypes.h"
* JLP
* Version 02-12-2018
*********************************************************/

#ifndef _jlp_splot_idv_prototypes_ // sentry

#define _jlp_splot_idv_prototypes_ 

#include "jlp_splot_idv_rename.h" 

/* Declaring linkage specification to have "correct names"
* that can be linked with C programs */

#ifdef __cplusplus
extern "C" {
#endif

/******************************************************
* Fortran programs used for graphic interface: 
*******************************************************/

/* jlp_splot_idv/splot_for.c */
void JLP_SPDEVICE_CURVE(char *plotdev1, float *xmin_user1, float *xmax_user1, 
                        float *ymin_user1, float *ymax_user1, int *plan, 
                        char *title, int *idv1);
int JLP_SPDEVICE_IMAGE(char *plotdev, char *title, char *filename,
                       float *image_f, int *nx, int *ny, int *gamma1,
                       int *gamma_d, int *idv1);
void JLP_SPLABEL(char *xlabel, int *max_length, int *ix, int *iy,
                 float *angle, float *expand, int *idrawit, float *length,
                 int *idv1);
void JLP_SPBOX(float box_xmin0, float box_xmax0, float box_ymin0, 
               float box_ymax0,
               char *xlabel, char *ylabel, char *title, int *ticks_in,
               int *box_numbers, char *filename, char *comments, int *idv1);
int JLP_SETPCOLOR(char *pcolor, int *idv1);
int JLP_SETLINEPARAM(int *lwidth, int *ltype, int *idv1);


/* jlp_splot_idv/jlp_splot_image.cpp */
int JLP_DEVICE_IMAGE(char *plotdev, char *out_filename, char *title,
                     float *image_f, int *nx, int *ny, int *gamma1,
                     int *gamma_d, int *idv1);
int JLP_PLOT_IMAGE(int *image, int *nx1, int *ny1, int *idim1,
                   int *xstart, int *ystart, int *gamma1, int *gamma_d,
                   int *black_and_white, int *idv1);
int jlp_hardcopy_image(float *image_f1, int *image_i, int *i_nx22,
                       int *i_ny22, int *nx2, int *ncolors, char *filename,
                       char *comments, int lut_scale, int black_and_white,
                       int high_resolution, int *nx1, int *ny1, 
                       int f1_xmin, int f1_ymin, int *f1_nx11,
                       int *f1_ny11, int width_frame, int height_frame,
                       int itt_is_linear, float lower_itt, float upper_itt,
                       char *lut_type, int inversed_lut, int lut_offset,
                       int lut_slope, char *graphic_file, char *pst_plotdev,
                       char *out_filename, char *title);
int JLP_GET_2CIRCLES(float *x_cent, float *y_cent, float *diam1,
                     float *diam2, int *ncirc, int *idv1);
int jlp_setup_menu(char *items, int *nitems, int *menu_nsub, int *menu_slen,
                   int *vertical, int *idv);
int jlp_select_menu(int *menu_select, int *menu_subselect, int *idv);

/* jlp_splot_idv/jlp_newplot100.cpp */
int JLP_NEWPLOT0(float *X, float *Y, float *ERRX, float *ERRY, int *NPTS,
                 int *NMAX, int *KCURVE, float *XMIN, float *XMAX,
                 float *YMIN, float *YMAX, int *AUTO_SCALE, int *IPLAN,
                 int *Y_IS_REVERSED, char *CHAR1, char *CHAR2,
                 char *TITLE, char *NCHAR, char *PCOLOR, float *XOUT,
                 float *YOUT, int *NOUT, int *NOUT_MAX, int *ERROR_BARS,
                 char *PLOTDEV, char *FILENAME, char *COMMENTS,
                 int *FULL_CAPTION, int *JLP_AXES, int *XGRID_IS_WANTED,
                 int *YGRID_IS_WANTED, int *X_IS_LOG10, int *Y_IS_LOG10,
                 float *EXPAND, int *TICKS_IN);
int jlp_newplot110(float *xplot, float *yplot, float *errx, float *erry,
                   int *npoints, int nmax, int ncurves, int iplan,
                   int y_is_reversed, char *xlabel, char *ylabel,
                   char *title, char *nchar, char *pcolor, int error_bars,
                   char *plotdev, int jlp_axes_are_wanted,
                   int xgrid_is_wanted, int ygrid_is_wanted,
                   int x_is_log10, int y_is_log10, float expand,
                   int ticks_in);
int jlp_newplot100(float *xplot, float *yplot, float *errx, float *erry,
                   int *npoints, int nmax, int ncurves, float xmin_user,
                   float xmax_user, float ymin_user, float ymax_user,
                   int auto_scale, int iplan, int y_is_reversed,
                   char *xlabel, char *ylabel, char *title, char *nchar,
                   char *pcolor, float *xout, float *yout, int *nout,
                   int nout_max, int error_bars, char *plotdev,
                   char *filename, char *comments,
                   int full_caption, int jlp_axes_are_wanted,
                   int xgrid_is_wanted, int ygrid_is_wanted,
                   int x_is_log10, int y_is_log10, float expand,
                   int ticks_in);

/* jlp_splot_idv/jlp_newplot.cpp */
int NEWPLOT2(float *xplot, float *yplot, float *errx, float *erry,
             int *npoints, int *nmax, int *ncurves,
             float *xmin_user, float *xmax_user, float *ymin_user, 
             float *ymax_user, int *auto_scale, int *iplan, int *y_is_reversed,
             char *xlabel, char *ylabel, char *title, char *nchar,
             char *pcolor, float *xout, float *yout, int *nout, 
             int *nout_max, int *error_bars, char *filename, char *comments,
             int *full_caption, float *expand, int *ticks_in, int *idv);

int NEWPLOT2_HARDCOPY(float *xplot, float *yplot, float *errx, float *erry,
                      int *npoints, int *nmax, int *ncurves,
                      float xmin_user, float xmax_user, float ymin_user, 
                      float ymax_user, int auto_scale, int iplan,
                      int y_is_reversed,
                      char *xlabel, char *ylabel, char *title, char *nchar,
                      char *pcolor, int *error_bars, char *filename,
                      char *comments, int *full_caption, 
                      int *jlp_axes_are_wanted, int *xgrid_is_wanted,
                      int *ygrid_is_wanted, int *xaxis_type, int *yaxis_type,
                      float *expand, int *idv1, char *pst_filename);
int NEWPLOT21(float *xplot, float *yplot, float *errx, float *erry,
              int *npoints, int *nmax, int *ncurves,
              float *xmin_user, float *xmax_user, float *ymin_user,
              float *ymax_user, int *auto_scale, int *iplan,
              int *y_is_reversed,
              char *xlabel, char *ylabel, char *title, char *nchar,
              char *pcolor, float *xout, float *yout, int *nout, 
              int *nout_max, int *error_bars, char *filename, char *comments,
              int *full_caption, int *jlp_axes_are_wanted, 
              int *xgrid_is_wanted, int *ygrid_is_wanted,
              int *xaxis_type, int *yaxis_type, float *expand, 
              int *ticks_in, int *idv1);
int newplot210(float *xplot, float *yplot, float *errx, float *erry,
              int *npoints, int nmax, int ncurves,
              float xmin_user, float xmax_user, float ymin_user, 
              float ymax_user, int auto_scale, int iplan,
              int y_is_reversed,
              char *xlabel, char *ylabel, char *title, char *nchar,
              char *pcolor, float *xout, float *yout, int *nout,
              int nout_max, int error_bars, char *filename, char *comments,
              int full_caption, int jlp_axes_are_wanted,
              int xgrid_is_wanted, int ygrid_is_wanted,
              int x_is_log10, int y_is_log10, float expand,
              int ticks_in, int idv1);
int jlp_comments_for_curves(char *filename, char *comments, float expand,
                            int idv);

/* jlp_splot_idv/jlp_splot.cpp */
int JLP_GET_DATA_PLOT_PARAM(float *xmin_user0, float *xmax_user0,          
                            float *ymin_user0, float *ymax_user0,
                            float *zmin_user0, float *zmax_user0,
                            int *idv1);
int JLP_GET_PLOT_PARAM(int *offx1, int *offy1, int *axlen1,
                       int *aylen1, float *xmin, float *xmax,
                       float *ymin, float *ymax, int *plan, int *idv1);
int JLP_SET_PLOT_PARAM(int *offx1, int *offy1, int *axlen1,
                       int *aylen1, float *xmin, float *xmax,
                       float *ymin, float *ymax, int *plan, int *idv1);
int JLP_SET_NEW_LIMITS(float *box_xmin, float *box_xmax, float *box_ymin, 
                       float *box_ymax, int *idv1);
int JLP_OPEN_DEVICE_FOR_CURVES(char *plotdev, char *out_filename, 
                     double xmin_user1, double xmax_user1, 
                     double ymin_user1, double ymax_user1, int plan, 
                     char *title, int *idv1);
int JLP_DEVICE_CURVE(char *plotdev, char *out_filename, 
                     float *xmin_user1, float *xmax_user1, 
                     float *ymin_user1, float *ymax_user1, int *plan, 
                     char *title, int *idv1);
int JLP_DRAW(int *x, int *y, int *idv1);
int JLP_RELOC(int *x, int *y, int *idv1);
int JLP_LINE1(float *xx1, float *yy1, float *xx2, float *yy2, int *idv1);
int JLP_LINE1_BACKUP(float *xx1, float *yy1, float *xx2, float *yy2, 
                     int *line_width, int *backup_to_file, int *idv1);
int JLP_SETCOLOR(int *r, int *g, int *b, int *idv1);
int JLP_EVENTS(int *idv1);
int JLP_GFLUSH(int *idv1);
int CONV_USER_TO_MGO(float *x_user, float *y_user, int *ix, int *iy,
                     int *idv1);
int CONV_MGO_TO_USER(int *ix, int *iy, float *x_user, float *y_user,
                     int *in_frame, int *idv1);
int JLP_WHERE(float *x, float *y, int *in_frame, int *pressed_button, 
              int *draw_cross, int *idv1);
int JLP_SPCLOSE(int *idv1);
int JLP_DRAW_TO_STATUS_BAR(char *label, int *idv1);
int JLP_ERASE_STATUS_BAR(int *idv1);
int JLP_SETLINEPARAM(int *lwidth, int *ltype, int *idv1);
int JLP_SETPCOLOR(char *pcolor, int *idv1);

/* jlp_splot_idv/jlp_splot_symbol.cpp */
int JLP_SYMBOL(int *x, int *y, int *isize, int *isymb, int *idv1);
int JLP_SYMBOL1(float *x, float *y, int *isize, int *isymb, int *idv1);
int JLP_SYMBOL2(float *x, float *y, float *size, int *isymb, int *idv1);
int JLP_SYMBOL_ERRORY1(float *x, float *y, float *erry, int *size, int *idv1);
int JLP_SYMBOL_ERRORX1(float *x, float *y, float *errx, int *size, int *idv1);
int JLP_CIRCLE1(float *x, float *y, float *diam, int *idv1);

/* jlp_splot_idv/jlp_splot2.cpp */
int jlp_string_copy(char *in, char *out, int len);
int JLP_CURVE(float *xplot, float *yplot, float *errx, float *erry, 
              int *npoints, char *nchar1, char *pcolor1, int *error_bars, 
              int *idv1);
int JLP_CURVE_LINE(float *xplot, float *yplot, int *npoints, char *nchar1,
                   char *pcolor1, int *idv1);
int JLP_CURVE_HISTO(float *xplot, float *yplot, int *npoints, char *nchar1,
                    char *pcolor1, int *idv1);
int jlp_splot_min_max_for_curves(float *xplot, float *yplot, float *errx,
                           float *erry, int *npoints, int nmax, int ncurves,
                           float *xmindata, float *xmaxdata, float *ymindata,
                           float *ymaxdata, int error_bars, int iplan);

/* jlp_splot_idv/jlp_display2.c */
int SPLOT_IMAGE(float *image_f1, int *nx1, int *ny1, int *ncolors,
               char *filename, char *comments, char *lut_type,
               int *itt_is_linear, float *lower_itt, float *upper_itt,
               char *xlabel, char *ylabel, char *zlabel, char *title,
               char *plotdev, int *idv1, float *xmin_user0, float *xmax_user0,
               float *ymin_user0, float *ymax_user0, int *nobox,
               int *lut_scale);
int SPLOT_DEVICE2(char *filename, float *image_f1, int *nx1, int *ny1,
                  int *nx2, int *ny2, int *max_size,
                  int *ncolors, int *color_output, char *lut_type,
                  int *lut_offset, int *lut_slope, char *title,
                  char *plotdev, int *idv1);
int SPLOT_IMAGE2(float *image_f1, int *nx1, int *ny1,
                 int *nx2, int *ny2, int *ncolors, char *filename, 
                 char *comments, int *itt_is_linear, float *lower_itt, 
                 float *upper_itt, char *xlabel, char *ylabel, char *zlabel, 
                 char *title, int *full_caption, float *xmin_user0, 
                 float *xmax_user0, float *ymin_user0, float *ymax_user0, 
                 int *nobox, int *lut_scale, int *idv1, int *color_output,
                 int *erase);

// Contained in "jlp_splot_box.cpp"
int jlp_splot_box(float box_xmin0, float box_xmax0, float box_ymin0,
                  float box_ymax0, char *xlabel, char *ylabel, char *title,
                  int ticks_in, int box_numbers, int full_caption,
                  int jlp_axes_are_wanted, int xgrid_is_wanted, 
                  int ygrid_is_wanted, int x_is_log10, int y_is_log10, 
                  float expand, char *axis_color, int idv);
int jlp_splot_box_for_image(char *xlabel, char *ylabel, char *title, 
                            int ticks_in, int box_numbers, int full_caption, 
                            float expand, char *axis_color, int idv);
int jlp_splot_box_type0(char *xlabel, char *ylabel, char *title, int ticks_in,
                        int box_numbers, int full_caption, int xgrid_is_wanted,
                        int ygrid_is_wanted, float box_xmin0, float box_xmax0,
                        float box_ymin0, float box_ymax0, int ix0, int iy0,
                        int ixlen, int iylen, float expand, char *axis_color, 
                        int idv);
int jlp_splot_axis(int xorigin, int yorigin, int axis_length,
                   float box_min, float box_max, float angle, int label_flag,
                   int ticks_up, int TeX_flag, float expand, 
                   int *smallest_ix, int *smallest_iy, int idv);
int jlp_splot_DrawBoxLabels(char *xlabel, char *ylabel, char *title,
                            int full_caption, int ix0, int iy0, int ixlen,
                            int iylen, float expand, int smallest_ix,
                            int smallest_iy, int idv1);

/* "jlp_splot1.cpp" */
int jlp_line(int x1, int y1, int x2, int y2, int idv);
int jlp_polygon(int x, int y, float expand, float angle, int nsides,
                int filled, int idv);
int jlp_erase(int idv);
int jlp_cursor(int *ix, int *iy, char *cursor_type, int *pressed_button,
               int idv);
float jlp_label_backup(char *s, int ixstart, int iystart, float angle1,
                       float expand1, int drawit, int backup_to_file,
                       int idv);
float jlp_label(char *s, int ixstart, int iystart, float angle1,
                float expand1, int drawit, int idv);
float jlp_label1(char *s, float xstart, float ystart, float angle1,
                 float expand1, int drawit, int idv);
float jlp_label1_backup(char *s, float xstart, float ystart, float angle1,
                        float expand1, int drawit, int backup_to_file,
                        int idv);
int jlp_open_backup_file(int idv, char * fname);
int jlp_close_backup_file(int idv);

/* "jlp_splot_lut1.cpp" */
int CONVERT_TO_LUT(float *image1, int *nx1, int *ny1, int *idim1,
                   int *image2, int *nx2, int *ny2, int *idim2,
                   int *ncolors, int *itt_is_linear, float *lower_itt,
                   float *upper_itt, int *idv1);
int JLP_ALLOC_LUT(int *ncolors, int *private_lut, int *idv1);
int JLP_LOAD_LUT(int *r, int *g, int *b, int *ncolors, int *idv1);
int JLP_REVERSE_LUT(int *idv1);
int jlp_splot_change_lut(char *lut_type, int reversed, int *ioff, int *islope,
                   int ncolors, int idv);
int jlp_splot_key(int nn1, float lower_itt, float upper_itt, char *zlabel,
                  int horiz, int axis_also, int gamma_d, float scale_x, 
                  float scale_y, int idv);

/* "jlp_graphfile.cpp" */
int jlp_read_graphic_file(char *graphic_file, int idv);

#ifdef __cplusplus
}
#endif

#endif /* End of _jlp_splot_idv sentry */
