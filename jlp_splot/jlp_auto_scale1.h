/***************************************************************
* Set of routines of general use:
* auto_scale1, auto_sky, auto_sky_d
*
* AUTHOR: JLP
* VERSION: 06-06-2017
***************************************************************/
#ifndef _jlp_auto_scale1_h
#define _jlp_auto_scale1_h

int auto_scale_float(float *image1, int nx1, int ny1, int idim,
                     float *lower_itt, float *upper_itt);
int auto_scale_double(double *image1, int nx1, int ny1, int idim,
                double *lower_itt, double *upper_itt);
int auto_scale_double_box(double *image1, int nx1, int ny1, int idim,
                int ix_min, int ix_max, int iy_min, int iy_max,
                double *lower_itt, double *upper_itt);
int auto_scale_double_box(double *image1, int nx1, int ny1, int idim,
                int ix_min, int ix_max, int iy_min, int iy_max,
                double *lower_itt, double *upper_itt, int min_max_scale);
int auto_sky_double(double *in_image, int nx, int ny, int idim, double *sky_level);
int auto_sky_double(double *in_image, int nx, int ny, int idim, 
                    double *sky_level, double *sky_noise);

#endif
