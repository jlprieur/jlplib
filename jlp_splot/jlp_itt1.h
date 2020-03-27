/* *****************************************************************
* jlp_itt1.h
* Create an Intensity Transfer Table to display a double precision image.
*
* This table is filled with the indices k to be used for LUT conversion
* of a double precision image
*
* From visui (Jean-Louis Prieur)
*
* JLP
* Version 10/12/2008
***************************************************************/
#if !defined(_jlp_itt1_h)
// Sentry, use file only if it's not already included.
#define _jlp_itt1_h

#include <stdio.h>
// #include <math.h>
#include <string.h>

typedef unsigned char BYTE;

void LoadITT(BYTE* out_ima_k, const double* in_ima, const int nx, const int ny,
             const int x1_box, const int y1_box, const int x2_box, const int y2_box,
             double *min, double *max, const char* ITT_1,
             const int ITT_is_linear);
int Mean_Sigma(const double* image, const int nx, const int ny,
               const int x1_box, const int y1_box, const int x2_box, const int y2_box,
               double* mean, double* sigma, const double bad_value);
void Min_Max_itt(const double* image, const int nx, const int ny,
                 const int x1_box, const int y1_box, const int x2_box,
                 const int y2_box, double* min, double* max, const double bad_value);
void Min_Max_Median_itt(const double* image, const int nx, const int ny,
                        double* min, double* max, const double bad_value);
void Min_Max_with_location(const double* image, const int nx, const int ny, double* min,
             double* max, int *ix_min, int *iy_min, int *ix_max, int *iy_max,
             const double bad_value);

#endif                                      // _jlp_itt1_h sentry.

