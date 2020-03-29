/********************************************************
* "jlp_rename.h"
* Interface C / Fortran
*           declared name in C ----> name when called from fortran
* IBM version      : JLP_EXAMPLE -----> jlp_example
* VAX/vms version  : JLP_EXAMPLE -----> JLP_EXAMPLE
* SUN version      : JLP_EXAMPLE -----> jlp_example_
* DEC/unix version : JLP_EXAMPLE -----> jlp_example_
* Linux version    : JLP_EXAMPLE -----> jlp_example__
* (GNU g77, Linux:
* With -funderscoring in effect, g77 appends two underscores to names with
* underscores and one underscore to external names with no underscores.)
*
Example in Cambridge:
printenv OSTYPE
solaris
linux
*
printenv uname
SunOS
* JLP
* Version 12-10-2008
 *********************************************************/
#if !defined(jlp_rename_included_) /* Beginning of sentry */
#define jlp_rename_included_

/* Concatenates name and extension: */
/******************************
#ifdef ibm
#define RENAME(name) name
#define RENAME_(name) name
#elif defined(sun) || defined(dec)
#define RENAME(name) name ## _
#define RENAME_(name) name ## _
#elif defined(linux) && !defined(gnu_f90)
#define RENAME(name) name ## _
#define RENAME_(name) name ## __
// Cambridge gcc and f90 in Solaris: 
// Debian gcc and f90 in Merate
#elif defined(solaris) || defined(gnu_f90)
#define RENAME(name) name ## _
#define RENAME_(name) name ## _
#else   // Just in case JLP_SYSTEM is not defined 
// #pragma message("jlp_ftoc_rename: JLP_SYSTEM is not defined!")
#define RENAME(name) name ## _
#define RENAME_(name) name ## __
#endif
******************************/
// Debian10
#define RENAME(name) name ## _
#define RENAME_(name) name ## _
#define JLP_RENAME_INCLUDED

/* jlp_sort.c (in sourcc) */
#define JLP_QSORT_FLOAT        RENAME_(jlp_qsort_float)
#define JLP_QSORT_DBLE         RENAME_(jlp_qsort_dble)
#define JLP_QSORT_INDX         RENAME_(jlp_qsort_indx)
#define JLP_QSORT_INDX_CHAR    RENAME_(jlp_qsort_indx_char)
#define JLP_QSORT_INDX_DBLE    RENAME_(jlp_qsort_indx_dble)
#define JLP_MEDIAN             RENAME_(jlp_median)
#define JLP_MEDIAN_DBLE        RENAME_(jlp_median_dble)

/* jlp_random.c */
#define JLP_RANDOM             RENAME_(jlp_random)
#define JLP_RANDOM_INIT        RENAME_(jlp_random_init)
#define JLP_RANDOM_GAUSS       RENAME_(jlp_random_gauss)

/* jlp_cgrad.c */
#define JLP_CGRAD              RENAME_(jlp_cgrad)
#define JLP_FSTEP              RENAME_(jlp_fstep)

/* polyfit.c */
#define POLYFIT                RENAME(polyfit)
#define CALPOLY                RENAME(calpoly)

/* FFT/RECENTRE  fftw_set.c, recent_fft.c */
/* fftw_set.c */
#define FFTW_1D_Y_FLT          RENAME_(fftw_1D_y_flt)
#define FFTW_1D_Y_DBLE         RENAME_(fftw_1D_y_dble)
#define FFTW_2D_FLT            RENAME_(fftw_2D_flt)
#define FFTW_2D_DBLE           RENAME_(fftw_2D_dble)
#define FFTW_FSETUP            RENAME_(fftw_fsetup)
#define FFTW_SHUTDOWN          RENAME_(fftw_shutdown)
/* jlp_fft.for */
#define FFT_1D_Y_FOURN         RENAME_(fft_1D_y_fourn_float)
#define FFT_1D_Y_FOURN_DBLE    RENAME_(fft_1D_y_fourn_dble)
#define FFT_2D_FOURN           RENAME_(fft_2D_fourn)
#define FFT_2D_FOURN_DBLE      RENAME_(fft_2D_fourn_dble)
/* recent_fft.c */
#define RECENT_FFT             RENAME_(recent_fft)
#define RECENT_FFT_1D_X_FLOAT  RENAME_(recent_fft_1D_x_float)
#define RECENT_FFT_1D_X        RENAME_(recent_fft_1D_x)
#define RECENT_FFT_1D_Y_FLOAT  RENAME_(recent_fft_1D_y_float)
#define RECENT_FFT_1D_Y        RENAME_(recent_fft_1D_y)
#define RECENT_FFT_DOUBLE      RENAME_(recent_fft_double)
#define FFT_SETUP              RENAME_(fft_setup)
#define FFT_FLOAT              RENAME_(fft_float)
#define TO_SINGLE              RENAME_(to_single)
#define TO_DOUBLE              RENAME_(to_double)

/* Warning:
 * (GNU g77, Linux:
 * With -funderscoring in effect, g77 appends two underscores to names with
 * underscores and one underscore to external names with no underscores.)
*/

#endif /* EOF sentry */
