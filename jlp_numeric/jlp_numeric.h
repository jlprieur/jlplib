/****************************************************************************
* Prototypes of functions contained in "jlp_numeric.a" 
*
* JLP
* Version 09/02/2007
*****************************************************************************/
#ifndef _jlp_numeric_h
#define _jlp_numeric_h

#include <stdio.h>
#include <stdlib.h>   // exit()
#include <string.h>   
#include <math.h>

#include "fftw3.h" // fftw_complex, etc

#include "jlp_num_rename.h"

typedef int INT4;
typedef fftw_complex FFTW_COMPLEX; 
// Now in "fftw3.h":
// typedef double fftw_complex[2];
// So:
#define c_re(c)  ((c)[0])
#define c_im(c)  ((c)[1])

/* See also M_PI in math.h ... */
#ifndef PI
#define PI 3.14159265358979323846
#endif

#ifndef MAXI
#define MAXI(x,y) ((x) > (y) ? (x) : (y))
#endif
#ifndef MINI
#define MINI(x,y) ((x) > (y) ? (y) : (x))
#endif
#ifndef ABS
#define ABS(x) ((x) > 0. ? (x) : (-(x)))
#endif
#ifndef SQUARE
#define SQUARE(x) ((x) * (x))
#endif
#ifndef NINT
#define NINT(x) (int)((x) + 0.5)
#endif

#ifdef __cplusplus
extern "C" {
#endif                          /* __cplusplus */

/* In "fit_gauss.c" */
int jlp_fit_gauss(double *xx, double *yy, double *f1, INT4 *npts,
                  double *sigx, double *sigy, double *xc, double *yc,
                  double *rho, double *errors, INT4 *ifail);
int calpoly_0(double xx, double yy, double *phi, INT4 ncoeff, INT4 kmax,
              double *value);

int lu_decomp(double *aa, int nn, int *indx, double *dd);
int lu_backsub(double *aa, int nn, int *indx, double *bb);
int mat_lu_solve(double *aa, double *xx, double *bb, int nn);
int mat_lu_inverse(double *aa, double *aa_inv, int nn);
int mat_product(double *out, double *in1, double *in2, int ncol1, int nlin1);
int mat_printf(double *aa, int ncol, int nlin);

/* in "jlp_random.c" */
int JLP_RANDOM_INIT(long *seed);
int JLP_RANDOM(float *x);
int JLP_RANDOM_GAUSS(float *x);

/* in "jlp_conju_grad.c" */
int jlp_conjugate_gradients(double *aa, double *psi, double *phi,
                            int nx1, int ny1);
int JLP_CGRAD(double *aa, double *psi, double *phi, INT4 *nx1, INT4 *ny1,
              INT4 *ifail);

/* in "jlp_sort.c" */
int JLP_QSORT_INDX_CHAR(char *array, int *length, int *index, int *nn);
int JLP_QSORT_INDX_DBLE(double *array, int *index, int *nn);
int JLP_QSORT_INDX(float *array, int *index, int *nn);
int JLP_QSORT(float *array, int *nn);
int JLP_QSORT_DBLE(double *array, int *nn);
int JLP_MEDIAN(float *data, int npts, float *value);
int JLP_MEDIAN_DBLE(double *data, int npts, double *value);

/* fft/recent_fft.c (jlputil.make) */
int RECENT_FFT(float *in, float *out, INT4 *nx, INT4 *ny, INT4 *idim);
int RECENT_FFT_DOUBLE(double *in, double *out, INT4 *nx, INT4 *ny, INT4 *idim);
int RECENT_FFT_1D_Y_FLOAT(float *in, float *out, INT4 *nx, INT4 *ny, INT4 *idim);
int RECENT_FFT_1D_X_FLOAT(float *in, float *out, INT4 *nx, INT4 *ny, INT4 *idim);
int RECENT_FFT_1D_Y(double *in, double *out, INT4 *nx, INT4 *ny, INT4 *idim);
int RECENT_FFT_1D_X(double *in, double *out, INT4 *nx, INT4 *ny, INT4 *idim);
int TO_SINGLE(double *in, float *out, INT4 *nx, INT4 *ny, INT4 *idim);
int TO_DOUBLE(float *in, double *out, INT4 *nx, INT4 *ny, INT4 *idim);

// Prototypes for FFT from "fft/fftw_set.c"

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
int fftw_setup(char *fftw_directory, int nx, int ny, char *err_message);
int FFTW_SETUP(char *fftw_directory, int *nx, int *ny, char *err_message);
int fftw_fast(fftw_complex *image, int nx, int ny, int direct);
int FFTW_SHUTDOWN();
int fftw_shutdown();

// dcv_lbfgs.c
int DCV_LBFGS(int *n, double *x, int *positive, double *f,
              double *ftol, int *nmax, double (*func)(double *),
              void (*dfunc)(double *, double *));
int DCV_LBFGS_FULL(int *n, double *x, double *lbound, double *ubound,
                   int *nbound, double *f, double *ftol, int *nmax,
                   double (*func)(double *), void (*dfunc)(double *, double *));

// jlp_linear_regression.c
int jlp_linear_regression(double *yy, double *xx, int npts, double *aa,
                          double *bb);

#ifdef __cplusplus
}                               /* extern "C" */
#endif                          /* __cplusplus */

// jlp_median.cpp
int jlp_sort_array_intensities(double *image0, int nx0, int ny0,
                               double *out_intens, int n_intens);
int JLP_QSORT_INDEX(double *array, int *index, int *nn);
int JLP_QSORT(double *array, int *nn);

// rotate.cpp
int rotimage(double *ima, double *imarot, int nx, int ny, double angle);

#endif
