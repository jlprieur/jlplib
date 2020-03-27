/*************************************************************************
* jlp_gseg_plot_data1.h
*
* JLP
* Version 20/03/2019
**************************************************************************/
#ifndef _jlp_gseg_plot_data1_h
#define _jlp_gseg_plot_data1_h

#include "jlp_gsegraf_defs.h"   // GSEG_PLOT_DATA structure

/* Declaring linkage specification to have "correct names"
* that can be linked with C programs */

#ifdef __cplusplus
extern "C" {
#endif

int jlp_gseg_init_plot_data(int gdev_graphic_type0, GSEG_PLOT_DATA *gspdata0);
int jlp_gseg_free_plot_data(GSEG_PLOT_DATA *gspdata0);
int jlp_gseg_copy_plot_data(GSEG_PLOT_DATA *gspdata0, GSEG_PLOT_DATA gspdata1);
int auto_scale_for_image(double *image1, int nx1, int ny1, int idim,
                         const int high_contrast, double *lower_itt, 
                         double *upper_itt);
int auto_scale_for_box(double *image1, int nx1, int ny1, int idim,
                       const int ix_min, const int ix_max,
                       const int iy_min, const int iy_max,
                       const int high_contrast,
                       double *lower_itt, double *upper_itt);

#ifdef __cplusplus
}
#endif

#endif
