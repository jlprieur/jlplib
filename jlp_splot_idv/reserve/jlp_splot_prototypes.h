/*********************************************************************
* jlp_splot.h
* prototypes of functions used by splot graphic library
*
* JLP
* Version 03/01/2015
*********************************************************************/
#ifndef __jlp_splot_h
#define __jlp_splot_h

#include <stdio.h>        // FILE
#include "jlp_macros.h"   // MAXI, MINI, NINT, etc
#include "jlp_lut1.h"  

// To be linked with c source :

#ifdef __cplusplus
extern "C" {
#endif

/* In jlp_graphfile.c */
int jlp_read_graphic_file(char *graphic_file, int idv);

/* "jlp_splot.cpp" */
int jlp_setup_plot(double offx1, double offy1, double axlen1, double aylen1,
                   double xmin, double xmax, double ymin, double ymax,
                   int width, int height, int idv);
int jlp_line(int x1, int y1, int x2, int y2, int idv);
int jlp_polygon(int x, int y, double expand, double angle, int nsides, 
                int filled, int idv);
int jlp_erase(int idv);
int jlp_cursor(int *ix, int *iy, char *cursor_type, int *pressed_button, 
               int idv);
int jlp_get_winlimits(double *x1, double *y1, double *x2, double *y2,
                      int type_of_win, int *pressed_button,
                      int *in_frame, int idv);
int jlp_getback_lut(int *r, int *g, int *b, int *mylut1, int ncolors,
                    int idv);
double jlp_label(char *s, int ixstart, int iystart, double angle1,
                double expand1, int drawit, int idv);
double jlp_label1(char *s, double xstart, double ystart, double angle1,
                 double expand1, int drawit, int idv);
double jlp_label1_backup(char *s, double xstart, double ystart, double angle1,
                        double expand1, int drawit, int backup_to_file, 
                        int idv);
int jlp_open_backup_file(int idv, char * fname);
int jlp_close_backup_file(int idv);
int jlp_load_font(int idv);
int jlp_hardcopy_image(double *image_f1, int *image_i, int *i_nx22,
                       int *i_ny22, int *nx2, int *ncolors, char *filename, 
                       char *comments, int lut_scale, int black_and_white,
                       int high_resolution,
                       int *nx1, int f1_xmin, int f1_ymin, int *f1_nx11,
                       int *f1_ny11, int width_frame, int height_frame,
                       int itt_is_linear, double lower_itt, double upper_itt,
                       char *lut_type, int inversed_lut, int lut_offset,
                       int lut_slope, char *graphic_file, char *pst_plotdev,
                       char *out_filename, char *title);
int jlp_select_menu(int *menu_select, int *menu_subselect, int *idv);
int jlp_setup_menu(char *items, int *nitems, int *menu_nsub, int *menu_slen,
                   int *vertical, int *idv);

/* Accessors: */
int iJgd_ncolors(int idv);
int iJgd_lut(int i, int idv);
int iJgd_TeX(int idv);
int iJgd_devtype(int idv);
int iJgd_offx(int idv);
int iJgd_offy(int idv);
int iJgd_axlen(int idv);
int iJgd_aylen(int idv);
int iJgd_lwidth(int idv);
int iJgd_ltype(int idv);
int iJgd_plan(int idv);
double iJgd_xminuser(int idv);
double iJgd_xmaxuser(int idv);
double iJgd_yminuser(int idv);
double iJgd_ymaxuser(int idv);
double iJgd_fsx(int idv);
double iJgd_fsy(int idv);
double iJgd_aspect(int idv);
double iJgd_cheight(int idv);
double iJgd_cwidth(int idv);
FILE *iJgd_fp_backup(int idv);

int cJgd_ncolors(int idv);
int cJgd_lut(int i, int idv);
int cJgd_TeX(int idv);
int cJgd_devtype(int idv);
int cJgd_offx(int idv);
int cJgd_offy(int idv);
int cJgd_axlen(int idv);
int cJgd_aylen(int idv);
int cJgd_lwidth(int idv);
int cJgd_ltype(int idv);
int cJgd_plan(int idv);
double cJgd_xminuser(int idv);
double cJgd_xmaxuser(int idv);
double cJgd_yminuser(int idv);
double cJgd_ymaxuser(int idv);
double cJgd_fsx(int idv);
double cJgd_fsy(int idv);
double cJgd_aspect(int idv);
double cJgd_cheight(int idv);
double cJgd_cwidth(int idv);
FILE *cJgd_fp_backup(int idv);

/* Possibility of setting some parameters directly: */
int iJgd_Set_ltype_and_lwidth(int ltype, int lwidth, int idv);
int jJgd_Set_ltype_and_lwidth(int ltype, int lwidth, int idv);
int iJgd_Set_gamma_d(int gamma_d, int idv);


int newplot210(double *xplot, double *yplot, double *errx, double *erry, 
              int *npoints, int nmax, int ncurves,
              char *xlabel, char *ylabel, char *title, char *nchar,
              char *pcolor, double *xout, double *yout, int *nout, 
              int nout_max, int error_bars, char *filename, char *comments, 
              int full_caption, int jlp_axes_are_wanted,
              int xgrid_is_wanted, int ygrid_is_wanted,
              int x_is_log10, int y_is_log10, double expand, 
              int ticks_in, int idv1);

/* "jlp_splot2.c" */
int jlp_string_copy(char *in, char *out, int len);
int jlp_box(double xmindata, double xmaxdata, double ymindata, double ymaxdata,
            char *xlabel, char *ylabel, char *title, int ticks_in,
            int box_numbers, int full_caption, int jlp_axes_are_wanted,
            int xgrid_is_wanted, int ygrid_is_wanted, int x_is_log10,
            int y_is_log10, double expand, char *axis_color, int idv);
int jlp_box_for_image(char *xlabel, char *ylabel, char *title, int ticks_in,
                      int box_numbers, int full_caption, double expand,
                      char *axis_color, int idv);
int jlp_box0(char *xlabel, char *ylabel, char *title, int ticks_in,
             int box_numbers, int full_caption, 
             int xgrid_is_wanted, int ygrid_is_wanted, double xminuser0, 
             double xmaxuser0, double yminuser0, double ymaxuser0, 
             int ix0, int iy0, int ixlen, int iylen, double expand, 
             char *axis_color, int idv);
int jlp_comments_for_curves(char *filename, char *comments, double expand,
                            int idv);
int jlp_axis(int xorigin, int yorigin, int axis_length,
             double min_user, double max_user, double angle, int label_flag,
             int ticks_up, int TeX_flag, double expand, int idv);
int jlp_string_copy(char *in, char *out, int len);
int jlp_newplot_menu(double *xplot, double *yplot, double *errx, double *erry,
                     int *npoints, int *nmax, int *ncurves,
                     char *xlabel, char *ylabel, char *title, char *nchar,
                     char *pcolor, double *xout, double *yout, int *nout,
                     int *nout_max, int *error_bars, char *filename,
                     char *comments, int *full_caption, 
                     int *jlp_axes_are_wanted, int *xgrid_is_wanted,
                     int *ygrid_is_wanted, int *xaxis_type, int *yaxis_type,
                     int *idv1);
int jlp_format(char *string, double value, int TeX_flag);
int compute_min_max_curves(double *xplot, double *yplot, double *errx, 
                           double *erry, int *npoints, int nmax, int ncurves,
                           double *xmindata, double *xmaxdata, double *ymindata,
                           double *ymaxdata);

/* jlp_myaxes.cpp */
int jlp_DrawBox(char *xlabel, char *ylabel, char *title, int ticks_in,
                int box_numbers, int full_caption,
                int xgrid_is_wanted, int ygrid_is_wanted,
                int x_is_log10, int y_is_log10,
                double xmindata, double xmaxdata, double ymindata,
                double ymaxdata, int ix0, int iy0, int ixlen, int iylen,
                char *axis_color, int idv1);
int jlp_DrawLabels(char *xlabel, char *ylabel, char *title,
                   int full_caption, int ix0, int iy0, int ixlen,
                   int iylen, double expand, int idv1);

/* "jlp_display1.c" */
int jlp_display1(double *xx, double *yy, int istart, int iend,
                 char *xlabel, char *ylabel, char *title, char *plotdev);
int jlp_display2(double *xx1, double *yy1, int istart1, int iend1,
                 double *xx2, double *yy2, int istart2, int iend2,
                 char *xlabel, char *ylabel, char *title, char *plotdev,
                 char *nchar1, char *nchar2, char *pcolor1, char *pcolor2);

/* In "jlp_display2.c" */
int jlp_pstcopy3(int *image1, int nx1, int ny1, int idim,
                 double low_thresh, double high_thresh,
                 int *r, int *g, int *b, int *pst_lut, int ncolors,
                 char *xlabel, char *ylabel, char *zlabel,
                 char *title, char *plotdev,
                 char *image_name, char *image_comments,
                 int lut_scale, int nobox, int color_output, 
                 double xminuser0, double xmaxuser0, double yminuser0, 
                 double ymaxuser0, double lower_itt, double upper_itt,
                 int idv);

/* in "jlp_contour.c" */
int sm_contour(double *image0, int nx, int ny, double levs[], int nlevs,
               int idv);
void sm_levels(double levs[], int nlevs);
void sm_minmax(double *min, double *max, int idv);
int sm_defimage(double *data, double xmin, double xmax, double ymin,
                double ymax, int nx, int ny);

/* in "jlp_surf3d.c" */
void sm_relocate(double x, double z, int idv);
void sm_draw(double x, double z, int idv);
void fan_compress (double xaray[], double yaray[], int nin, double eps,
                   int karay[], int *nout, int idv);
void sm_line(double x1, double z1, double x2, double z2, int idv);
int sm_limits(double ax1, double ax2, double ay1, double ay2, int idv);
void jlp_box_3d(char *xlab, char *ylab, char *zlab, int idv);

/* in splot/lib/jlp_gif.c" */
int gif_imageRGB(int *image, int ny, int nx, int idim,
                 int *red, int *green, int *blue, int ncolors, 
                 char *filename);

#ifdef __cplusplus
}
#endif

#endif  /* EOF sentry */
