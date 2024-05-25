/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* jlp0_rdfitsio.cpp
*
* To read FITS formatted 1-D, 2-D, and 3-D image files
* Using "FITSIO" C package.
* Formats supported are : FITS 8,16,32,-32,-64
* (i.e. 1-byte, 2-byte or 4-byte integer, and 4-byte or 8-byte float values)
* JLP: comments and jlp_descriptors
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
* jlp0_rdfits_2d_flt
*
* INPUT:
*   dflag = -1 (no error (warning) messages and no descriptors)
*         = 0 (no descriptors)
*         = 1 (descriptors)
*   iplane > 0 if 2D image only is wanted for output
*          = 0 if 3D image with all data
*   vm_flag = 1 if allocation of virtual memory is required
*           = 0 otherwise
* OUTPUT:
*   nx1, ny1: size of an image plane
*   nz1: number of image planes
**********************************************************************/
int jlp0_rdfits_2d_flt(INT_PNTR *pntr_array, float *array, int *nx1,
                       int *ny1, int *nz1, int *iplane, int *idim,
                       char *infile, char *comments, char *jlp_descr,
                       int dflag, int *istatus, int vm_flag)
{
char  err_message[128];
int   status, istat, naxis, bitpix, is_ok;
float *array1;
fitsfile *fptr;

*istatus = -1;

#ifdef DEBUG
printf("jlp0_rdfits_2d_flt/DEBUG/infile=%s\n", infile);
#endif

strcpy(err_message, "");
strcpy(comments, "");
strcpy(jlp_descr, "");
*nx1 = 0;
*ny1 = 0;
*nz1 = 0;
*iplane = 1;
*idim = 0;
*pntr_array = (INT_PNTR)NULL;
is_ok = jlp1_rdfits_is_ok(infile);

if(is_ok != 1) return(-1);

// decode header
 status = jlp1_rdfits_header(infile, nx1, ny1, nz1, &naxis, &bitpix,
                    comments, jlp_descr, dflag, err_message);
 if(status != 0) {
   fprintf(stderr, "jlp0_flt_rdfits/Error reading header from infile=%s \n", infile);
   return(-1);
   }

#ifdef DEBUG
printf("jlp0_rdfits_2d_flt/DEBUG: nx1=%d ny1=%d idim=%d dflag=%d\n",
        *nx1, *ny1, *idim, dflag);
printf("jlp0_flt_rdfits/DEBUG/comments=%s\n jlp_descr: \n%s\n", comments, jlp_descr);
#endif

 status = jlp1_open_fits_file0(infile, &fptr, err_message);
 if(status != 0) {
   fprintf(stderr, "jlp0_flt_rdfits/Error opening infile=%s \n", infile);
   return(status);
   }

/* If no allocation of virtual memory, check
* if buffer and image sizes are consistent
* (for 1D and 2D arrays) */
   if(vm_flag == 0) {
    if(idim < nx1) {
      fprintf(stderr, "jlp0_rdfits/Fatal error/Inconsistent size of array: nx1=%d ny1=%d idim=%d\n",
              *nx1, *ny1, *idim);
      fprintf(stderr, "whereas fits image (%s) is such that: naxis=%d naxes[0]=%d naxes[1]=%d\n",
              infile, naxis, *nx1, *ny1);
      istat = 0; fits_close_file(fptr,&istat);
      exit(-1);
      }
     array1 = array;
/* Allocation of memory if needed: */
   } else {
     array1 = (float *)malloc((*nx1) * (*ny1) * sizeof(float));
     *idim = *nx1;
     if(array1 == NULL) {
      fprintf(stderr, "jlp0_flt_rdfits_2d_flt/Fatal error allocating memory: nx1=%d ny1=%d\n",
              *nx1, *ny1);
      istat = 0; fits_close_file(fptr,&istat);
      exit(-1);
      }
   }

/* Read data and copy to float array: */
   jlp0_rdfits_2d_data_flt(infile, fptr, array1, *nx1, *ny1, *nz1,
                           naxis, bitpix, *iplane, *idim, istatus);

   if(*istatus == 0) {
/* Close fits file: */
    istat = 0; fits_close_file(fptr,&istat);

/* Copy pointer array (for FORTRAN): */
   *pntr_array = (INT_PNTR)array1;
   }

return(*istatus);
}
/**********************************************************************
* jlp0_dble_rdfits
*
* INPUT:
* dflag = -1 (no error (warning) messages and no descriptors)
*       = 0 (no descriptors)
*       = 1 (descriptors)
* iplane > 0 if 2D image only is wanted for output
*        = 0 if 3D image with all data
* err_mess: character string used to write error messages
*         (Warning: err_mess should be declared as char *err_mess[200])
* jlp_descr: character string to write the descriptors
*         (Warning: descriptors should be declared as char jlp_descr[1024])
*
* OUTPUT:
* nx1, ny1: size of an image plane
* nz1: number of images in input data cube
**********************************************************************/
int jlp0_dble_rdfits(double **array1, int *nx1, int*ny1, int *nz1,
                     int iplane, char *infile, char *comments,
                     char *jlp_descr, int dflag, char *err_mess)
{
int   status, istat, naxis, bitpix;
fitsfile *fptr;

#ifdef DEBUG
printf("jlp0_dble_rdfits/DEBUG/infile=%s\n", infile);
#endif
printf("jlp0_dble_rdfits/QQQ: infile=%s\n", infile);

strcpy(jlp_descr, "");

 status = jlp1_open_fits_file0(infile, &fptr, err_mess);
 if(status != 0) {
    fprintf(stderr, "jlp0_dble_rdfits/Error in jlp1_open_fits_file: %s\n", err_mess);
    return(-1);
 }

// Decode header
 istat = jlp1_rdfits_header(infile, nx1, ny1, nz1, &naxis, &bitpix,
                      comments, jlp_descr, dflag, err_mess);
 if (istat) {
   fprintf(stderr, "jlp0_dble_rdfits/Error reading header of >%s< istat=%d\n",
           infile, istat);
   sprintf(err_mess,"jlp0_dble_rdfits/Error reading header of >%s< istat=%d\n",
           infile, istat);
   istat = 0; fits_close_file(fptr,&istat);
   return(-1);
   }

#ifdef DEBUG
printf("jlp0_dble_rdfits/DEBUG: nx1=%d ny1=%d dflag=%d\n",
        *nx1, *ny1, dflag);
printf("jlp0_dble_rdfits/DEBUG/comments=>%s<\n DEBUG/jlp_descr: \n>%s<\n", comments, jlp_descr);
#endif


/* Allocation of memory if needed: */
    *array1 = (double *)malloc((*nx1) * (*ny1) * sizeof(double));
    if(array1 == NULL) {
     fprintf(stderr, "jlp0_dble_rdfits/Fatal error allocating memory: nx1=%d ny1=%d\n",
             *nx1, *ny1);
     istat = 0; fits_close_file(fptr,&istat);
     exit(-1);
     }

/* Read data and copy to float array: */
   jlp0_rdfits_2d_data_dble(infile, fptr, *array1, *nx1, *ny1, *nz1,
                            naxis, bitpix, iplane, *nx1, &istat);
   if (istat) {
     sprintf(err_mess,"jlp0_dble_rdfits/Error reading data of >%s< istat=%d\n",
            infile, istat);
     istat = 0; fits_close_file(fptr,&istat);
     return(-1);
   }

/* Close fits file: */
   istat = 0;
   fits_close_file(fptr,&istat);

return(0);
}
/**********************************************************************
* jlp0_3dxfits_rd_header
* Read header of 3D Fits file
*
* OUTPUT:
* err_mess: character string used to write error messages
*         (Warning: err_mess should be declared as char *err_mess[200])
* header_fname: name of the output file that will contain the complete headers
*
**********************************************************************/
int jlp0_3dxfits_rd_header(char *infile, char *header_fname, char *err_mess)
{
char err_message[81];
int  i, k, status, istat, hdu_type, nhdu, nkeys;
char kwd_card[FLEN_CARD];
fitsfile *fptr;
FILE *fp_out;

#ifdef DEBUG
printf("jlp0_3dxfits_rd_header/DEBUG/infile=%s\n", infile);
#endif
 if((fp_out = fopen(header_fname, "w")) == NULL) {
  sprintf(err_mess, "jlp0_3dxfits_rd_header/Error opening output file: >%s<\n",
          header_fname);
  fprintf(stderr, "%s", err_mess);
  return(-1);
  }

 status = jlp1_open_fits_file0(infile, &fptr, err_mess);
 if(status != 0) return(status);

// First look for the number of HDU's of this FITS file:
  istat = 0;
  fits_get_num_hdus(fptr, &nhdu, &istat);
  if(istat || (nhdu == 0)) {
     fits_read_errmsg(err_message);
     sprintf(err_mess, "jlp0_3dxfits_rd_header/Cannot find nber of hdus: nhdu=%d, istat=%d\n %s \n",
            nhdu, istat, err_message);
     fclose(fp_out); return(-1);
   }

  fprintf(fp_out, "%%***********************************************************************************************************\n");
  fprintf(fp_out, "%% infile=%s\n", infile);
  fprintf(fp_out, "%% nhdu=%d\n", nhdu);

// Read all the headers and copy them to output file:
// NB: first hdu in FITS files is starting at i=1
  for(i = 1; i <= nhdu; i++) {
   istat = 0;
   fits_movabs_hdu(fptr, i, &hdu_type, &istat);
// Get the number of keywords
   fits_get_hdrspace(fptr, &nkeys, NULL, &istat);
  fprintf(fp_out, "%%***********************************************************************************************************\n");
   fprintf(fp_out, "%% ihdu=%d nkeys=%d\n", i, nkeys);
  fprintf(fp_out, "%%***********************************************************************************************************\n");
    for(k = 1; k <= nkeys; k++) {
      fits_read_record(fptr, k, kwd_card, &istat);
      fprintf(fp_out, "%s\n", kwd_card);
      }
     if(istat) {
     sprintf(err_mess, "jlp0_3dxfits_rd_header/Error hdu=%d istat=%d !",
            i, istat);
     istat = 0; fits_close_file(fptr,&istat);
     }
   }

if(istat == 0) fits_close_file(fptr, &istat);
fclose(fp_out);

return(istat);
}
/**********************************************************************
* jlp0_3dxfits_rd_keywd
* Read a keyword in a 3D Fits file
*
* INPUT:
* keywd_name: keyword name
*
* OUTPUT:
* keywd_value: keyword value
* err_mess: character string used to write error messages
*         (Warning: err_mess should be declared as char *err_mess[200])
*
**********************************************************************/
int jlp0_3dxfits_rd_keywd(char *infile, char *keywd_name, const int ihdu,
                          char *keywd_value, char *keywd_comments,
                          char *err_mess)
{
int status, istat, hdu_type;
fitsfile *fptr;

#ifdef DEBUG
printf("jlp0_3dxfits_rd_keywd/DEBUG/infile=%s\n", infile);
#endif

 status = jlp1_open_fits_file0(infile, &fptr, err_mess);
 if(status != 0) return(status);

// NB: first hdu in FITS files is starting at i=1
  istat = 0;
  fits_movabs_hdu(fptr, ihdu, &hdu_type, &istat);
  fits_read_keyword(fptr, keywd_name, keywd_value, keywd_comments, &istat);
   if(istat) {
    sprintf(err_mess, "jlp0_3dxfits_rd_keywd/Error ihdu=%d istat=%d !",
            ihdu, istat);
    istat = 0; fits_close_file(fptr,&istat);
    }

if(istat == 0) fits_close_file(fptr, &istat);

return(istat);
}
/**********************************************************************
* jlp0_3dxfits_rd_3dbox_dble
* Read 1D data from 3D Fits file containing an XTENSION section
* with a primary table header
*
* INPUT:
* ix2, iy2: coordinates of the first pixel to be examined
* nx2: number of elements to be extracted from each plane(starting from ix2,iy2)
* err_mess: character string used to write error messages
*         (Warning: err_mess should be declared as char *err_mess[200])
* ihdu : index of HDU (should be >= 2)
*
* OUTPUT:
* nx1, ny1, nz1: size of the FITS data cube
**********************************************************************/
int jlp0_3dxfits_rd_3dbox_dble(double **array2, const int nx2,
                               int *nx1, int*ny1, int *nz1,
                               const int ix2, const int iy2, const int ihdu,
                               char *infile, char *comments, char *err_mess)
{
int status, istat, naxis, bitpix;
fitsfile *fptr;

#ifdef DEBUG
printf("jlp0_3dxfits_rd_3dbox_dble/DEBUG/infile=%s\n", infile);
#endif

 status = jlp1_open_fits_file0(infile, &fptr, err_mess);
 if(status != 0) return(status);

 status = jlp1_read_xfits_header(fptr, ihdu, nx1, ny1, nz1, &naxis, &bitpix,
                                 comments, err_mess);
 if(status != 0) return(status);

/* Allocation of memory if needed: */
 *array2 = (double *)malloc((*nz1) * nx2 * sizeof(double));
 if(array2 == NULL) {
   sprintf(err_mess, "jlp0_3dxfits_rd_3dbox_dble/Fatal error allocating memory: nx2=%d nz1=%d\n",
           nx2, *nz1);
   fprintf(stderr, "%s", err_mess);
   istat = 0; fits_close_file(fptr,&istat);
   return(-1);
   }

if((ix2 >= *nx1) || (iy2 > *ny1)) {
  sprintf(err_mess, "jlp0_3dxfits_redbox_dble/Error: ix2 = %d > nx_image = %d \
or iy2 = %d > ny_image = %d\n",
           ix2, *nx1, iy2, *ny1);
  fprintf(stderr, "%s", err_mess);
  fits_close_file(fptr,&istat); return(-3);
  }

/* Read data and copy to float array: */
 istat = 0;
 jlp0_rdfits_3dbox_data_dble(infile, fptr, *array2, nx2, *nx1, *ny1, *nz1,
                             naxis, bitpix, ix2, iy2, &istat);
 if (istat) {
   sprintf(err_mess,"jlp0_3dxfits_rd_3dbox_dble/Error reading data of >%s< istat=%d\n",
           infile, istat);
   fprintf(stderr, "%s", err_mess);
   istat = 0; fits_close_file(fptr,&istat);
   return(-1);
   }

/* Close FITS file: */
   istat = 0;
   fits_close_file(fptr,&istat);

return(istat);
}
/**********************************************************************
* jlp0_3dxfits_rd_rect_cube_dble
* Read 3D data from 3D Fits file containing an XTENSION section
* with a primary table header
*
* INPUT:
* iy_start, iy_end: y coordinates of the rectangle to be extracted
*                   (from x = 0 to x = nx1)
* err_mess: character string used to write error messages
*         (Warning: err_mess should be declared as char *err_mess[200])
* ihdu : index of HDU (should be >= 2)
*
* OUTPUT:
* nx1, ny1, nz1: size of the FITS data cube
* nx2: number of spectra that have been extracted from (0, iy_start)
*      to (0, iy_end)
**********************************************************************/
int jlp0_3dxfits_rd_rect_cube_dble(double **array2, int *nx2,
                               int *nx1, int*ny1, int *nz1,
                               const int iy_start2, const int iy_end2,
                               const int ihdu, char *infile, char *comments,
                               char *err_mess)
{
int status, istat, naxis, bitpix, ix2;
fitsfile *fptr;

#ifdef DEBUG
printf("jlp0_3dxfits_rd_3dbox_dble/DEBUG/infile=%s\n", infile);
#endif

 status = jlp1_open_fits_file0(infile, &fptr, err_mess);
 if(status != 0) return(status);

 status = jlp1_read_xfits_header(fptr, ihdu, nx1, ny1, nz1, &naxis, &bitpix,
                                 comments, err_mess);
 if(status != 0) return(status);

/* Allocation of memory if needed: */
 *nx2 = (*nx1) * (iy_end2 - iy_start2);
 *array2 = (double *)malloc((*nz1) * (*nx2) * sizeof(double));
 if(array2 == NULL) {
   sprintf(err_mess, "jlp0_3dxfits_rd_3dbox_dble/Fatal error allocating memory: nx2=%d nz1=%d\n",
           (*nx2), *nz1);
   fprintf(stderr, "%s", err_mess);
   istat = 0; fits_close_file(fptr,&istat);
   return(-1);
   }

if((iy_start2 >= iy_end2) || (iy_end2 > *ny1)) {
  sprintf(err_mess, "jlp0_3dxfits_redbox_dble/Error: iy_start2 = %d > iy_end2 = %d \
or iy_end2 = %d > ny_image = %d\n",
           iy_start2, iy_end2, iy_end2, *ny1);
  fprintf(stderr, "%s", err_mess);
  fits_close_file(fptr,&istat); return(-3);
  }

/* Read data and copy to float array: */
 istat = 0;
 ix2 = 0;
 jlp0_rdfits_3dbox_data_dble(infile, fptr, *array2, *nx2, *nx1, *ny1, *nz1,
                             naxis, bitpix, ix2, iy_start2, &istat);
 if (istat) {
   sprintf(err_mess,"jlp0_3dxfits_rd_3dbox_dble/Error reading data of >%s< istat=%d\n",
           infile, istat);
   fprintf(stderr, "%s", err_mess);
   istat = 0; fits_close_file(fptr,&istat);
   return(-1);
   }

/* Close FITS file: */
   istat = 0;
   fits_close_file(fptr,&istat);

return(istat);
}
/**********************************************************************
* jlp0_3dxfits_rd_2d_dble
* Read 2D data from 3D Fits file containing an XTENSION section
* with a primary table header
*
* INPUT:
* iplane > 0 if 2D image only is wanted for output
*        = 0 if 3D image with all data
* err_mess: character string used to write error messages
*         (Warning: err_mess should be declared as char *err_mess[200])
* ihdu : index of HDU (should be >= 2)
*
* OUTPUT:
* nx1, ny1: size of an image plane
* nz1: number of images in input data cube
**********************************************************************/
int jlp0_3dxfits_rd_2d_dble(double **array1, int *nx1, int*ny1, int *nz1,
                          int iplane, const int ihdu, char *infile,
                          char *comments, char *err_mess)
{
int  status, istat, naxis, bitpix;
fitsfile *fptr;

#ifdef DEBUG
printf("jlp0_3dxfits_rd_2d_dble/DEBUG/infile=%s\n", infile);
#endif

 status = jlp1_open_fits_file0(infile, &fptr, err_mess);
 if(status != 0) return(status);

 status = jlp1_read_xfits_header(fptr, ihdu, nx1, ny1, nz1, &naxis, &bitpix,
                                 comments, err_mess);
 if(status != 0) return(status);

/* Allocation of memory if needed: */
    *array1 = (double *)malloc((*nx1) * (*ny1) * sizeof(double));
    if(array1 == NULL) {
     fprintf(stderr, "jlp0_3dxfits_rd_dble/Fatal error allocating memory: nx1=%d ny1=%d\n",
             *nx1, *ny1);
     istat = 0; fits_close_file(fptr,&istat);
     exit(-1);
     }

/* Read data and copy to float array: */
   jlp0_rdfits_2d_data_dble(infile, fptr, *array1, *nx1, *ny1, *nz1,
                            naxis, bitpix, iplane, *nx1, &istat);
   if (istat) {
     sprintf(err_mess,"jlp0_3dxfits_rd_2d_dble/Error reading data of >%s< istat=%d\n",
            infile, istat);
     istat = 0; fits_close_file(fptr,&istat);
     return(-1);
   }

/* Close FITS file: */
   istat = 0;
   fits_close_file(fptr,&istat);

return(istat);
}
/**********************************************************************
* jlp1_open_fits_file0
*
**********************************************************************/
int jlp1_open_fits_file0(char *infile, fitsfile **fptr, char *err_mess)
{
int istat;
char  filename[256], *pcc, err_message[81];

   strcpy(filename,infile);
/* Check input characters strings (pb if fortran...) */
   pcc = filename;
   while(*pcc && *pcc != ' ') pcc++;
   if(*pcc == ' ') *pcc = '\0';

/* Check filename syntax and add ".fits" if no extension is present: */
   pcc = filename;
   while(*pcc && *pcc != '.') pcc++;
   if(*pcc != '.') strcpy(pcc,".fits");

   istat = 0;
   fits_open_file(fptr, filename, READONLY, &istat);
// Error status codes in fitsio.h
   if (istat != 0) {
     switch (istat) {
       case 104:
       default:
         fprintf(stderr," 104: FILE_NOT_OPENED\n");
        break;
       case 108:
         fprintf(stderr," 108: READ_ERROR\n");
        break;
     }
     fits_read_errmsg(err_message);
     sprintf(err_mess,"jlp1_open_fits_file0/ Cannot open input file : >%s< istat=%d\n %s \n",
            filename, istat, err_message);
     fprintf(stderr, "jlp1_open_fits_file0/ Cannot open input file : >%s< istat=%d\n %s \n",
            filename, istat, err_message);
   }
return(istat);
}
/**********************************************************************
* jlp1_rdfits_is_ok
*
* Return: 1 if ok for reading, 0 ik no_ok
**********************************************************************/
int jlp1_rdfits_is_ok(char *infile)
{
int istat;
fitsfile *fptr;

if(infile[0] == '\0') return(0);

 istat = 0;
 fits_open_file(&fptr, infile, READONLY, &istat);
 if (istat != 0) {
  fprintf(stderr, "jlp0_rdfits_is_ok/Error opening infile=%s \n", infile);
  return(0);
 }
 fits_close_file(fptr,&istat);

return(1);
}
/************************************************************************
*
************************************************************************/
int jlp1_read_xfits_header(fitsfile *fptr, const int ihdu, int *nx1, int*ny1,
                           int *nz1, int *naxis, int *bitpix,
                           char *comments, char *err_mess)
{
char err_message[81], comment[80];
int istat, hdu_type, nhdu;
long naxes[3];

// First look for the number of HDU's of this FITS file:
  istat = 0;
  fits_get_num_hdus(fptr, &nhdu, &istat);
  if(istat || (nhdu == 0)) {
     fits_read_errmsg(err_message);
     sprintf(err_mess, "jlp1_read_xfits_header/Cannot find nber of hdus: nhdu=%d, istat=%d\n %s \n",
            nhdu, istat, err_message);
     return(-1);
   }

// Check if ihdu is compatible with FITS file:
  if (nhdu < ihdu) {
     sprintf(err_mess, "jlp1_read_xfits_header/ nber of hdu=%d < ihdu=%d !",
            nhdu, ihdu);
     istat = 0; fits_close_file(fptr,&istat);
     return(-1);
   }

// move to the image HDU (absolute position, should be larger or equal to 2)
   istat = 0;
// NB: first hdu in FITS files is i=1
   fits_movabs_hdu(fptr, ihdu, &hdu_type, &istat);
   if (istat) {
     fits_read_errmsg(err_message);
     sprintf(err_mess, "jlp1_read_xfits_header/ Cannot move to hdu=%d istat=%d\n %s \n",
            ihdu, istat, err_message);
     istat = 0; fits_close_file(fptr,&istat);
     return(-1);
   }

// Check if XTENSION = 'IMAGE' was found :
   if (hdu_type == IMAGE_HDU) {
//   printf( "reading image data in HDU %d\n", ihdu);
   } else {
        sprintf(err_mess, "jlp1_read_xfits_header/HDU type of hdu=%d is not an image \n",
            ihdu);
       istat = 0; fits_close_file(fptr,&istat);
       return(-1);
      }

// Read nber of bits per pixel
  istat = 0;
  fits_read_key(fptr, TINT, "BITPIX", bitpix, comment, &istat);

// Read the NAXIS1 and NAXIS2 keyword to get image size
// first = 1, nber_max = 3
   istat = 0;
   fits_read_keys_lng(fptr, "NAXIS", 1, 3, naxes, naxis, &istat);
   if (istat) {
     fits_read_errmsg(err_message);
     sprintf(err_mess, "jlp1_read_xfits_header/ Cannot read naxes istat=%d\n %s \n",
            istat, err_message);
     istat = 0; fits_close_file(fptr,&istat);
     return(-1);
   }
   *nx1 = naxes[0];
   *ny1 = naxes[1];
   *nz1 = naxes[2];

// Read comments (if NAME is present):
  istat = 0;
  fits_read_key(fptr, TSTRING, "NAME", comments, comment, &istat);

#ifdef DEBUG
printf("jlp1_read_xfits_header/DEBUG: nx1=%d ny1=%d\n", *nx1, *ny1);
printf("jlp1_read_xfits_header/DEBUG/comments=>%s<\n", comments);
#endif

return(0);
}
