/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* jlp0_rdfitsio_utils.cpp
*
* To read FITS formatted 1-D, 2-D, and 3-D image files
* Using "FITSIO" C package.
* Formats supported are : FITS 8,16,32,-32,-64
* (i.e. 1-byte, 2-byte or 4-byte integer, and 4-byte or 8-byte float values)
* JLP: comments and jlp_descriptors
*
* Contains:
*  jlp1_rdfits_header
*  jlp0_rdfits_2d_data_flt
*  jlp0_rdfits_2d_data_dble
*  jlp0_rdfits_1d_data_dble
*
* JLP
* Version 06-01-2015
---------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include "jlp_fitsio.h"
/* #include <cfitsio/fitsio.h>   fitsio.h should be in "/usr/include/cfitsio" */
#include "fitsio.h"   /* fitsio.h should be in $(CFITSIO_INCL_DIR) */
#include "jlp_string.h"

/*
#define DEBUG
*/

typedef int INT4;
typedef short int INT2;
typedef unsigned char INT1;
/**********************************************************************
* jlp0_rdfits_2d_data_flt
*
* INPUT:
*   array2: pointer to a float array where to copy the FITS data
*   iplane > 0 if 2D image only is wanted for output
*          = 0 if 3D image with all data
*   bitpix: bits per pixel
*
* OUTPUT:
*   nx1, ny1: size of an image plane
*   nz1: number of image planes
**********************************************************************/
int jlp0_rdfits_2d_data_flt(char *filename, fitsfile *fptr,
                            float *array2, int nx1, int ny1, int nz1,
                            int naxis, int bitpix, int iplane, int idim,
                            int *istatus)
{
int status, i, j;
double *dble_array2;

dble_array2 = new double[nx1 * ny1];

status = jlp0_rdfits_2d_data_dble(filename, fptr, dble_array2, nx1, ny1, nz1,
                                  naxis, bitpix, iplane, nx1, istatus);

// Transfer data to float array:
for(j = 0; j < ny1; j++)
  for(i = 0; i < nx1; i++)
   array2[i + j * idim] = (float) dble_array2[i + j * nx1];

delete[] dble_array2;

return(status);
}
/**********************************************************************
* jlp0_rdfits_2d_data_dble
*
* INPUT:
*   array2: pointer to a double array where to copy the FITS data
*   iplane > 0 if 2D image only is wanted for output
*          = 0 if 3D image with all data
*   bitpix: bits per pixel
*
* OUTPUT:
*   nx1, ny1: size of an image plane
*   nz1: number of image planes
**********************************************************************/
int jlp0_rdfits_2d_data_dble(char *filename, fitsfile *fptr,
                             double *array2, int nx1, int ny1, int nz1,
                             int naxis, int bitpix, int iplane,
                             int idim, int *istatus)
{
 char         err_message[81];
 int          istat, any_null_values;
 long         nelements;
 float        *f_array2;
 INT1         *i1_array2;
 INT2         *i2_array2;
 INT4         *i4_array2;
 register int i, j;

*istatus = 0;
f_array2 = NULL;
i1_array2 = NULL;
i2_array2 = NULL;
i4_array2 = NULL;

if(naxis > 2 && iplane > nz1) {
  fprintf(stderr, "jlp0_rdfits/Fatal error: iplane = %d > nz_image = %d\n", iplane, nz1);
  *istatus = -3;  fits_close_file(fptr,&istat); return(-3);
  }

/* Correction for one-dimensional images, if ny1 = 0 */
  if(nx1 <= 0) {
    fprintf(stderr, " jlp0_rdfits/Fatal error: nx = %d \n", nx1);
    *istatus = -2;  fits_close_file(fptr,&istat); return(-2);
    }

#ifdef DEBUG
printf("jlp0_rdfits_2d_data_dble/iplane=%d\n", iplane);
if(iplane == 0) printf("WARNING: iplane=0 I will read all the data !!!\n");
#endif

   nelements = nx1;
/* 2-D or plane of 3D image: */
   if(naxis > 1) nelements *= ny1;
/* All the data of 3-D file: */
   if(naxis > 2 && iplane == 0) nelements *= nz1;

/* Allocate memory space: */
    if(bitpix < 0) {
      f_array2 = (float *) malloc(nelements * sizeof(float));
      if(f_array2 == NULL) {
        fprintf(stderr, "jlp0_rdfits/fatal error allocating memory, nel=%ld\n",
                 nelements);
         *istatus = -2;  fits_close_file(fptr,&istat); return(-2);
       }
    } else if(bitpix == 8){
// JLP2015: Allocate more space than needed:
      i1_array2 = (INT1 *) malloc(nelements * sizeof(INT4));
      if(i1_array2 == NULL) {
        fprintf(stderr, "jlp0_rdfits/fatal error allocating memory, nel=%ld\n",
                 nelements);
         *istatus = -2;  fits_close_file(fptr,&istat); return(-2);
       }
    } else if(bitpix == 16){
// JLP2009: Allocate more space than needed:
      i2_array2 = (INT2 *) malloc(nelements * sizeof(INT4));
      if(i2_array2 == NULL) {
        fprintf(stderr, "jlp0_rdfits/fatal error allocating memory, nel=%ld\n",
                 nelements);
         *istatus = -2;  fits_close_file(fptr,&istat); return(-2);
       }
    } else if(bitpix == 32){
      i4_array2 = (INT4 *) malloc(nelements * sizeof(INT4));
      if(i4_array2 == NULL) {
        fprintf(stderr, "jlp0_rdfits/fatal error allocating memory, nel=%ld\n",
                 nelements);
         *istatus = -2;  fits_close_file(fptr,&istat); return(-2);
       }
    } else {
     fprintf(stderr, "jlp0_rdfits_2d_data_dble/Fatal error: bitpix=%d is unsupported!\n",
             bitpix);
     exit(-1);
    }

#ifdef DEBUG
   printf(" nx=%d, ny=%d, nz=%d bitpix=%d nelements=%ld \n",
         nx1, ny1, nz1, bitpix, nelements);
#endif

   istat = 0;

/* Float data: */
 if(bitpix < 0) {
/* Read data:    */
    if(iplane == 0 || nz1 == 0)
       fits_read_img(fptr, TFLOAT, 1, nelements, 0, f_array2,
                     &any_null_values, &istat);
/* Case of extraction of a 2-D plane from 3-D array: */
    else
       fits_read_img(fptr, TFLOAT, 1 + (iplane - 1) * nelements,
                     nelements, 0, f_array2,
                     &any_null_values, &istat);
/* 8 bit integer data: */
 } else if(bitpix == 8) {
/* Read data:    */
    if(iplane == 0 || nz1 == 0)  {
       fits_read_img(fptr, TBYTE, 1, nelements, 0, i1_array2,
                     &any_null_values, &istat);
/* Case of extraction of a 2-D plane from 3-D array: */
    } else {
       fits_read_img(fptr, TBYTE, 1 + (iplane - 1) * nelements,
                     nelements, 0, i1_array2,
                     &any_null_values, &istat);
    }
/* 16 bit integer data: */
 } else if(bitpix == 16) {
/* Read data:    */
    if(iplane == 0 || nz1 == 0)  {
       fits_read_img(fptr, TSHORT, 1, nelements, 0, i2_array2,
                     &any_null_values, &istat);
/* Case of extraction of a 2-D plane from 3-D array: */
    } else {
       fits_read_img(fptr, TSHORT, 1 + (iplane - 1) * nelements,
                     nelements, 0, i2_array2,
                     &any_null_values, &istat);
    }
/* 32 bit integer data: */
 } else if(bitpix == 32) {
/* Read data:    */
    if(iplane == 0 || nz1 == 0)
       fits_read_img(fptr, TINT, 1, nelements, 0, i4_array2,
                     &any_null_values, &istat);
/* Case of extraction of a 2-D plane from 3-D array: */
    else
       fits_read_img(fptr, TINT, 1 + (iplane - 1) * nelements,
                     nelements, 0, i4_array2,
                     &any_null_values, &istat);
 } else {
  istat = 0;
 }
    if (istat) {
      fits_read_errmsg(err_message);
      fprintf(stderr, "jlp0_rdfits/fits_read_img; error reading file : >%s<\
 istat=%d (bitpix=%d)\n %s \n",
            filename, istat, bitpix, err_message);
    *istatus = -3;
    }

/* Transfer to output array: */
  if(bitpix < 0) {
   for(j = 0; j < ny1; j++)
      for(i = 0; i < nx1; i++)
         array2[i + j * idim] = (double)f_array2[i + j * nx1];
  } else if(bitpix == 8) {
   for(j = 0; j < ny1; j++)
      for(i = 0; i < nx1; i++)
         array2[i + j * idim] = (double)i1_array2[i + j * nx1];
  } else if(bitpix == 16) {
   for(j = 0; j < ny1; j++)
      for(i = 0; i < nx1; i++)
         array2[i + j * idim] = (double)i2_array2[i + j * nx1];
  } else if(bitpix == 32) {
   for(j = 0; j < ny1; j++)
      for(i = 0; i < nx1; i++)
         array2[i + j * idim] = (double)i4_array2[i + j * nx1];
  }

if(f_array2 != NULL) free(f_array2);
if(i1_array2 != NULL) free(i1_array2);
if(i2_array2 != NULL) free(i2_array2);
if(i4_array2 != NULL) free(i4_array2);

return(*istatus);
}
/**********************************************************************
* jlp0_rdfits_3dbox_data_dble
*
* INPUT:
* ix2, iy2: coordinates of the first pixel to be examined
* nx2: number of elements to be extracted from each plane(starting from ix2,iy2)
* bitpix: bits per pixel
*
* OUTPUT:
* nx1, ny1: size of an image plane
* nz1: number of image planes
**********************************************************************/
int jlp0_rdfits_3dbox_data_dble(char *filename, fitsfile *fptr,
                             double *array2, int nx2, int nx1, int ny1,
                             int nz1, int naxis, int bitpix, int ix2, int iy2,
                             int *istatus)
{
char   err_message[81];
int    i, k, istat, any_null_values;
long   nelements, ipixel, size_image;
float  *f_array2;
INT1   *i1_array2;
INT2   *i2_array2;
INT4   *i4_array2;

*istatus = 0;
f_array2 = NULL;
i1_array2 = NULL;
i2_array2 = NULL;
i4_array2 = NULL;

// Number of elements to extract from each plane of the cube:
 nelements = nx2;

/* Allocate memory space: */
 if(bitpix < 0) {
    f_array2 = (float *) malloc(nelements * sizeof(float));
 } else if(bitpix == 8){
// JLP2015: Allocate more space than needed:
    i1_array2 = (INT1 *) malloc(nelements * sizeof(INT4));
 } else if(bitpix == 16){
// JLP2009: Allocate more space than needed:
    i2_array2 = (INT2 *) malloc(nelements * sizeof(INT4));
 } else if(bitpix == 32){
    i4_array2 = (INT4 *) malloc(nelements * sizeof(INT4));
 } else {
  fprintf(stderr, "jlp0_rdfits_2d_data_dble/Fatal error: bitpix=%d is unsupported!\n",
          bitpix);
  return(-1);
  }

 if(f_array2 == NULL && i1_array2 == NULL && i2_array2 == NULL 
    && i4_array2 == NULL) {
   fprintf(stderr, "jlp0_rdfits_3dbox_data_dble/jlp0_rdfitsio_utils/Error allocating memory, bitpix=%d nx2=nel=%ld\n",
           bitpix, nelements);
   fprintf(stderr, " nx2=%d number of elements to be extracted from each plane(starting from ix2=%d,iy2=%d)\n",
           nx2, ix2, iy2);
   fprintf(stderr, " nx1=%d, ny1=%d size of an image plane, and nz1=%d image planes\n",
           nx1, ny1, nz1);
   *istatus = -2;  fits_close_file(fptr,&istat); return(-2);
  }

#ifdef DEBUG
 printf(" nx=%d, ny=%d, nz=%d bitpix=%d nelements=%ld \n",
        nx1, ny1, nz1, bitpix, nelements);
#endif

 istat = 0;

size_image = nx1 * ny1;
ipixel = 1 + ix2 + iy2 * nx1;

/* Read data:    */
for(k = 0; k < nz1; k++) {

/* Float data: */
 if(bitpix < 0) {
    fits_read_img(fptr, TFLOAT, ipixel + k * size_image,
                  nelements, 0, f_array2, &any_null_values, &istat);
/* 8 bit integer data: */
 } else if(bitpix == 8) {
/* Read data:    */
     fits_read_img(fptr, TBYTE, ipixel + k * size_image,
                   nelements, 0, i1_array2, &any_null_values, &istat);
/* 16 bit integer data: */
 } else if(bitpix == 16) {
/* Read data:    */
     fits_read_img(fptr, TSHORT, ipixel + k * size_image,
                   nelements, 0, i2_array2, &any_null_values, &istat);
/* 32 bit integer data: */
 } else if(bitpix == 32) {
     fits_read_img(fptr, TINT, ipixel + k * size_image,
                   nelements, 0, i4_array2, &any_null_values, &istat);
 } else {
  istat = 0;
 }
//*********************************************************
 if (istat) {
  fits_read_errmsg(err_message);
  fprintf(stderr, "jlp0_rdfits/fits_read_img; error reading file : >%s<\
 istat=%d (bitpix=%d)\n %s \n",
          filename, istat, bitpix, err_message);
  *istatus = -3;
  }

/* Transfer to output array: */
  if(bitpix < 0) {
     for(i = 0; i < nx2; i++) array2[i + k * nx2] = (double)f_array2[i];
  } else if(bitpix == 8) {
     for(i = 0; i < nx2; i++) array2[i + k * nx2] = (double)i1_array2[i];
  } else if(bitpix == 16) {
     for(i = 0; i < nx2; i++) array2[i + k * nx2] = (double)i2_array2[i];
  } else if(bitpix == 32) {
     for(i = 0; i < nx2; i++) array2[i + k * nx2] = (double)i4_array2[i];
  }
} // EOF loop on k

if(f_array2 != NULL) free(f_array2);
if(i1_array2 != NULL) free(i1_array2);
if(i2_array2 != NULL) free(i2_array2);
if(i4_array2 != NULL) free(i4_array2);

return(*istatus);
}
/**********************************************************************
* jlp1_rdfits_header
*
* INPUT:
*   dflag = -1 (no error (warning) messages and no descriptors)
*         = 0 (no descriptors)
*         = 1 (descriptors)
* OUTPUT:
*   nx1, ny1: size of an image plane
*   nz1: number of image planes
*   naxis: number of axes
*   istatus: -2 if not supported format
*   comments: comments (OBJECT keyword)
*   jlp_descr: descriptors
**********************************************************************/
int jlp1_rdfits_header(char *infile, int *nx1, int *ny1, int *nz1,
                              int *naxis, int *bitpix, char *comments,
                              char *jlp_descr, int dflag,
                              char *err_message)
{
 fitsfile *fptr;
 char         buffer[81], buf1[80];
 int          maxdim = 3, simple, istat, extend, status;
 long         naxes[3], pcount, gcount;
 register int i;

 *nx1 = 0;
 *ny1 = 0;
 *nz1 = 0;
 *naxis = 0;
 *bitpix = 0;
 strcpy(jlp_descr, "");
 strcpy(comments, "");
 strcpy(err_message, "");
 status = jlp1_open_fits_file0(infile, &fptr, err_message);
 if(status != 0) {
  fprintf(stderr, "jlp1_rdfits_header/Error opening %s : %s\n", infile, err_message);
  return(status);
  }

/* decode header    */
    istat = 0;
    fits_read_imghdr(fptr, maxdim, &simple, bitpix, naxis, naxes, &pcount,
                     &gcount, &extend, &istat);
    if (istat) {
          fprintf(stderr, "jlp1_rdfits_header/NOT supported FITS format! istat=%d\n",istat);
          fits_close_file(fptr,&istat); return(-1);
        }

/* Axes values: */
   *nx1 = naxes[0];
   if(*naxis > 1 ) *ny1 = naxes[1];
/* In the case of 3-D images, set nz1 to naxes[2], otherwise set it to 0: */
   if(*naxis > 2) *nz1 = naxes[2];

/* Try to fill "comments" with many keywords */

/* Try from the beginning each time: */
    istat = 0;
    fits_read_record(fptr,0,buffer,&istat);
/* fits_read_key needs istat=0 in input ! */
    istat = 0;
    fits_read_key(fptr,TSTRING,"COMMENT",comments,buffer,&istat);
#ifdef DEBUG
      printf("jlp1_rdfits_header/DEBUG/ istat=%d COMMENT = %s \n",
              istat, comments);
#endif
    if(istat == KEY_NO_EXIST) {
      istat = 0;
      fits_read_record(fptr,0,buffer,&istat);
      istat = 0;
      fits_read_key(fptr,TSTRING,"OBJECT",comments,buffer,&istat);
#ifdef DEBUG
      printf("jlp1_rdfits_header/DEBUG/ istat=%d OBJECT = %s \n",istat,comments);
#endif
// JLP2015: If OBJECT is empty, copy DESCR to comments:
      if(istat == KEY_NO_EXIST || comments[0] == '\0') {
         istat = 0;
         fits_read_record(fptr,0,buffer,&istat);
         istat = 0;
         fits_read_key(fptr,TSTRING,"DESCRIP",comments,buffer,&istat);
#ifdef DEBUG
         printf("jlp1_rdfits_header/DEBUG/ istat=%d DESCRIP = %s \n",istat,comments);
#endif
         if(istat == KEY_NO_EXIST)
             {
             istat = 0;
             fits_read_record(fptr,0,buffer,&istat);
             istat = 0;
             fits_read_key(fptr,TSTRING,"HISTORY",comments,buffer,&istat);
             }
         }
      }

#ifdef DEBUG
printf("jlp1_rdfits_header/DEBUG/comments: >%s<\n",comments);
#endif

/* Descriptors if present (jlp_descr[1024]) and if dflag is set to 1 */
 if(dflag == 1) {
   for(i = 0; i < (int)strlen(jlp_descr); i++) jlp_descr[i] = '\0';
   i = 0;
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"OBJECT",buf1,buffer,&istat);
   jlp_trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) {
     sprintf(&jlp_descr[i],"OBJECT= %s", buf1);
#ifdef DEBUG
   printf("jlp1_rdfits_header/DEBUG/ istat=%d OBJECT = %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;
      }
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"DESCRIP",buf1,buffer,&istat);
   jlp_trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) {
     sprintf(&jlp_descr[i],"DESCRIP= %s", buf1);
#ifdef DEBUG
   printf("jlp1_rdfits_header/DEBUG/ istat=%d DESCRIP = %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;
      }
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
/* DATE (compatibility with files created before september 2012) */
   istat = 0; fits_read_key(fptr,TSTRING,"DATE",buf1,buffer,&istat);
   jlp_trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) {
     sprintf(&jlp_descr[i],"DATE= %s", buf1);
#ifdef DEBUG
   printf("jlp1_rdfits_header/DEBUG/ istat=%d jlp_descr(DATE): %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;
      }
/* DATE-OBS (after september 2012) */
   istat = 0; fits_read_key(fptr,TSTRING,"DATE-OBS",buf1,buffer,&istat);
   jlp_trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) {
     sprintf(&jlp_descr[i],"DATE-OBS= %s", buf1);
#ifdef DEBUG
   printf("jlp1_rdfits_header/DEBUG/ istat=%d jlp_descr(DATE-OBS): %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;
      }
/* jlp_descr:
 OBJECT, DESCRIP, COUNTERS, ANDOR0, ANDOR1, ANDOR2, ANDOR3, ANDOR4, HISTORY
*/
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"COUNTERS",buf1,buffer,&istat);
   jlp_trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) {
     sprintf(&jlp_descr[i],"COUNTERS= %s", buf1);
#ifdef DEBUG
   printf("jlp1_rdfits_header/DEBUG/ istat=%d jlp_descr(COUNTERS): %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;
      }
/* jlp_descr:
 OBJECT, DESCRIP, COUNTERS, ANDOR0, ANDOR1, ANDOR2, ANDOR3, ANDOR4, HISTORY
*/
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"ANDOR0",buf1,buffer,&istat);
   jlp_trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) {
     sprintf(&jlp_descr[i],"ANDOR0= %s", buf1);
#ifdef DEBUG
   printf("jlp1_rdfits_header/DEBUG/ istat=%d jlp_descr(ANDOR0): %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;
      }
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"ANDOR1",buf1,buffer,&istat);
   jlp_trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) {
     sprintf(&jlp_descr[i],"ANDOR1= %s", buf1);
#ifdef DEBUG
   printf("jlp1_rdfits_header/DEBUG/ istat=%d jlp_descr(ANDOR1): %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;
      }
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"ANDOR2",buf1,buffer,&istat);
   jlp_trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) {
     sprintf(&jlp_descr[i],"ANDOR2= %s", buf1);
#ifdef DEBUG
   printf("jlp1_rdfits_header/DEBUG/ istat=%d jlp_descr(ANDOR2): %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;
      }
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"ANDOR3",buf1,buffer,&istat);
   jlp_trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) {
     sprintf(&jlp_descr[i],"ANDOR3= %s", buf1);
#ifdef DEBUG
   printf("jlp1_rdfits_header/DEBUG/ istat=%d jlp_descr(ANDOR3): %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;
      }
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"ANDOR4",buf1,buffer,&istat);
   jlp_trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) {
     sprintf(&jlp_descr[i],"ANDOR4= %s", buf1);
#ifdef DEBUG
   printf("jlp1_rdfits_header/DEBUG/ istat=%d jlp_descr(ANDOR4): %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;
      }
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"ANDOR5",buf1,buffer,&istat);
   jlp_trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) {
     sprintf(&jlp_descr[i],"ANDOR5= %s", buf1);
#ifdef DEBUG
   printf("jlp1_rdfits_header/DEBUG/ istat=%d jlp_descr(ANDOR5): %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;
      }
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"COUNTERS",buf1,buffer,&istat);
   jlp_trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) {
     sprintf(&jlp_descr[i],"COUNTERS= %s", buf1);
#ifdef DEBUG
   printf("jlp1_rdfits_header/DEBUG/ istat=%d jlp_descr(COUNTERS): %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;
      }
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"HISTORY",buf1,buffer,&istat);
   jlp_trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) {
     sprintf(&jlp_descr[i],"HISTORY= %s", buf1);
#ifdef DEBUG
   printf("jlp1_rdfits_header/DEBUG/ istat=%d jlp_descr(HISTORY): %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      }
 } /* EOF case dflag != 1 */

/* Close fits file: */
 istat = 0; fits_close_file(fptr,&istat);

return(0);
}
