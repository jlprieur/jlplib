/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Prototypes for FFT from "fftw_set.c"
//
// JLP
// Version 15/12/2014
//------------------------------------------------------------*/
#ifndef _fftw_set_define
#define _fftw_set_define
#include "fftw3.h"

typedef int INT4;

#ifdef __cplusplus
extern "C" {
#endif                          /* __cplusplus */

int FFTW_1D_Y_FLT(float *re, float *im, INT4 *nx, INT4 *ny, INT4 *idim, 
                   INT4 *direct);
int fftw_1D_Y_float(float *re, float *im, int nx, int ny, int direct);
int FFTW_1D_Y_DBLE(double *re, double *im, INT4 *nx, INT4 *ny, INT4 *idim, 
             INT4 *direct);
int fftw_1D_Y_double(double *re, double *im, int nx, int ny, int direct);
int FFTW_2D_DBLE(double *re, double *im, int *nx, int *ny, int *direct);
int fftw_2D_double(double *re, double *im, int nx, int ny, int direct);
int fftw_2D_flt(float *re, float *im, INT4 *nx, INT4 *ny, INT4 *idim, 
                  INT4 *kod);
int fftw_2D_float(float *re, float *im, int nx, int ny, int direct);
int fftw_setup(char *fftw_directory, int nx, int ny, char *error_message);
int FFTW_SETUP(char *fftw_directory, int *nx, int *ny, char *error_message);
int fftw_fast(fftw_complex *image, int nx, int ny, int direct);
int FFTW_SHUTDOWN();
#ifdef __cplusplus
}                               /* extern "C" */
#endif                          /* __cplusplus */

#endif
