/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* jlp_fitsio.h
* To read FITS formatted 1-D, 2-D, and 3-D image files
* Using "FITSIO" C package.
* Formats supported are : FITS 8,16,32,-32,-64
* (i.e. 1-byte, 2-byte or 4-byte integer, and 4-byte or 8-byte float values)
* JLP: comments and jlp_descriptors
*
* JLP
* Version 02-10-2015
---------------------------------------------------------------------*/
#ifndef _jlp_fitsio_h   /* sentry */
#define _jlp_fitsio_h
#include <stdio.h>
#include <string.h>
#include <stdint.h>    // uintptr

/* #include <cfitsio/fitsio.h>   fitsio.h should be in "/usr/include/cfitsio" */
// #define IS_JLP   // to avoid problem with TBYTE
#include "fitsio.h"   /* fitsio.h should be in $(CFITSIO_INCL_DIR) */

#include "jlp_fitsio_rename.h"

// typedef     long           INT_PNTR;
// JLP 2015 for WIN_64
typedef     uintptr_t           INT_PNTR;
typedef     unsigned char       UINT1;
typedef     unsigned short int  UINT2;

/* Declaring linkage specification to have "correct names"
* that can be linked with C programs */

#ifdef __cplusplus
extern "C" {
#endif

/********************* jlp0_fits_utils.c */

int get_bessel_epoch_from_fits_file(char *fits_filename, char *full_directory,
                                    double *epoch0, char *date0,
                                    int *epoch_was_found);
int descrip_bepoch_from_date(char *d_date, char *d_counters, char *date0,
                             double *year0, double *time0, double *epoch0);
int descrip_bepoch_from_obs_date(char *d_date, char *date0, double *year0,
                                 double *time0, double *epoch0);
int JLP_compute_besselian_epoch(char *date0, const double time0, 
		                double *b_epoch0);
int JLP_julian_day(double aa, int mm, int idd, double time, double *djul);
int JLP_besselian_epoch(double aa, int mm, int idd, double time,
                        double *b_date);
int JLP_julian_epoch(double aa, int mm, int idd, double time, double *j_date);
int JLP_besselian_to_julian_epoch(double b_date, double *j_date);

/********************* jlp_rdfitsio.cpp */
int jlp1_open_fits_file0(char *infile, fitsfile **fptr, char *err_mess);
int JLP_RDFITS_2D_UINT1(UINT1 **array1, int *nx1, int *ny1, int *nz1,
                        int iplane, char *infile, char *comments, char *err_mess);
int JLP_RDFITS_2D_UINT2(UINT2 **array1, int *nx1, int *ny1, int *nz1,
                        int iplane, char *infile, char *comments, char *err_mess);
int JLP_RDFITS_2D_flt(float **array1, int *nx1, int *ny1, int *nz1,
                      int iplane, char *infile, char *comments, char *err_mess);
int JLP_RDFITS_2D_dble(double **array1, int *nx1, int *ny1, int *nz1,
                      int iplane, char *infile, char *comments, char *err_mess);
int JLP_RDFITS_2D_dble_descr(double **array1, int *nx1, int *ny1, int *nz1,
                             int iplane, char *infile, char *comments,
                             char *jlp_descr, char *err_mess);
int JLP_RD_3DXFITS_2D_dble(double **array1, int *nx1, int *ny1, int *nz1,
                           int iplane, const int ihdu, char *infile,
                           char *comments, char *err_mess);
int JLP_RD_3DXFITS_3Dbox_dble(double **array2, const int nx2,
                              int *nx1, int *ny1, int *nz1,
                              const int ix_start2, const int iy_start2,
                              const int ihdu, char *infile, char *comments,
                              char *err_mess);
int JLP_RD_3DXFITS_RectCube_dble(double **array2, int *nx2,
                                 int *nx1, int *ny1, int *nz1,
                                 const int iy_start2, const int iy_end2,
                                 const int ihdu, char *infile, char *comments,
                                 char *err_mess);
int JLP_3DXFITS_rd_header(char *infile, char *header_fname,
                          char *err_mess);
int JLP_3DXFITS_rd_keywd(char *infile, char *keywd_name, const int ihdu,
                         char *keywd_value, char *keywd_comments,
                         char *err_mess);
// Float versions for old fortran routines:
int JLP_RDFITS(float *array, int *nx1, int *ny1, int *idim,
               char *infile, char *comments, char *jlp_descr,
               int *dflag, int *istatus);
int JLP_VM_RDFITS(INT_PNTR *pntr_array, int *nx1, int *ny1, char *infile,
                  char *comments, char *jlp_descr, int *dflag, int *istatus);
int JLP_VM_RDFITS_3D(INT_PNTR *pntr_array, int *nx1, int *ny1, int *nz1,
                     int *iplane, char *infile, char *comments,
                     char *jlp_descr, int *dflag, int *istatus);

/********************* jlp0_rdfitsio.cpp */

int jlp0_rdfits_2d_flt(INT_PNTR *pntr_array, float *array, int *nx1,
                       int *ny1, int *nz1, int *iplane, int *idim,
                       char *infile, char *comments, char *jlp_descr,
                       int *dflag, int *istatus, int vm_flag);
int jlp0_3dxfits_rd_2d_dble(double **array1, int *nx1, int*ny1, int *nz1,
                         int iplane, const int ihdu, char *infile,
                         char *comments, char *err_mess);
int jlp0_3dxfits_rd_3dbox_dble(double **array1, const int nx2,
                               int *nx1, int*ny1, int *nz1,
                               const int ix1, const int iy1, const int ihdu,
                               char *infile, char *comments, char *err_mess);
int jlp0_3dxfits_rd_rect_cube_dble(double **array2, int *nx2,
                               int *nx1, int*ny1, int *nz1,
                               const int iy_start2, const int iy_end2,
                               const int ihdu, char *infile, char *comments,
                               char *err_mess);
int jlp0_3dxfits_rd_header(char *infile, char *header_fname, char *err_mess);
int jlp0_3dxfits_rd_keywd(char *infile, char *keywd_name, const int ihdu,
                          char *keywd_value, char *keywd_comments,
                          char *err_mess);

int jlp0_dble_rdfits(double **array1, int *nx1, int *ny1, int *nz1,
                     int iplane, char *infile, char *comments,
                     char *jlp_descr, int dflag, char *err_mess);
int jlp1_read_xfits_header(fitsfile *fptr, const int ihdu,
                            int *nx1, int*ny1, int *nz1,
                            int *naxis, int *bitpix,
                            char *comments, char *err_mess);

/********************* jlp0_rdfitsio_utils.cpp */
/*
* dflag = -1 (no error (warning) messages and no descriptors)
*         = 0 (no descriptors)
*         = 1 (descriptors)
*/
int jlp1_rdfits_header(char *infile, int *nx1, int *ny1, int *nz1,
                       int *naxis, int *bitpix, char *comments,
                       char *jlp_descr, int dflag,
                       char *err_message);
int jlp0_rdfits_2d_data_UINT1(char *filename, fitsfile *fptr,
                            UINT1 *array1, int nx1, int ny1, int nz1,
                            int naxis, int bitpix, int iplane, int idim,
                            int *istatus);
int jlp0_rdfits_2d_data_UINT2(char *filename, fitsfile *fptr,
                            UINT2 *array1, int nx1, int ny1, int nz1,
                            int naxis, int bitpix, int iplane, int idim,
                            int *istatus);
int jlp0_rdfits_2d_data_flt(char *filename, fitsfile *fptr,
                            float *array1, int nx1, int ny1, int nz1,
                            int naxis, int bitpix, int iplane, int idim,
                            int *istatus);
int jlp0_rdfits_2d_data_dble(char *filename, fitsfile *fptr,
                             double *array1, int nx1, int ny1, int nz1,
                             int naxis, int bitpix, int iplane,
                             int idim, int *istatus);
int jlp0_rdfits_3dbox_data_dble(char *filename, fitsfile *fptr,
                                double *array1, const int nx2,
                                int nx1, int ny1, int nz1,
                                int naxis, int bitpix, int ix, int iy,
                                int *istatus);
int jlp0_rdfits_3d_UINT2(char *filename, fitsfile *fptr, UINT2 **array1,
                         int *nx1, int *ny1, int *nz1, int *istatus);

/********************* jlp0_wrfitsio.cpp */

int JLP_WRFITS(float *array, int *nx1, int *ny1, int *idim,
               char *filename, char *comments, char *jlp_descr, int *dflag,
               int *istatus, int *out_type);

/********************* jlp_wrfitsio.cpp */

int JLP_WRFITS_2D_UINT1(UINT1 *array1, int nx1, int ny1, int idim1,
                        char *filename, char *comments, char *err_mess);
int JLP_WRFITS_2D_UINT2(UINT2 *array1, int nx1, int ny1, int idim1,
                        char *filename, char *comments, char *err_mess);
int JLP_WRFITS_2D_flt(float *array1, int nx1, int ny1, int idim1,
                      char *filename, char *comments, char *err_mess);
int JLP_WRFITS_2D_dble(double *d_array1, int nx1, int ny1, int idim1,
                       char *filename, char *comments, char *err_mess);
int JLP_WRFITS_2D_dble_descr(double *d_array1, int nx1, int ny1, int idim1,
                             char *filename, char *comments, char *jlp_descr,
                             int *istatus, int out_type, char *err_mess);
/*
int JLP_VM_WRFITS_3D(double *array1, int nx1, int ny1, int nz1,
                     int iplane, char *infile, char *comments,
                     char *jlp_descr, int dflag, char *err_mess);
*/

/********************* jlp0_spectra_fitsio.cpp */

int jlp_read_spfits(char *infile, char *comments, float **wavelength,
                    float **flux_unnorm, float **flux_norm, float **snr,
                    long **order_nb, long *nrows, int firstrow,
                    int nelements, int icorot, int vm, int italk);
int jlp_read_spfits_fast(char *infile, char *comments, float *flux_norm,
                         int firstrow, int nelements, int icorot, int italk);
int jlp_flt_write_spfits(char *filename, char *comments, float *wavel,
                         float *flux, float *snr, long nrows, int italk);
int jlp_dble_write_spfits(char *filename, char *comments, double *wavel,
                          double *flux, double *snr, long nrows, int italk);

#ifdef __cplusplus
}
#endif

#endif   /* End of sentry */
