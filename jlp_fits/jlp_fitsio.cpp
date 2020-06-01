/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* jlp_fitsio.cpp 
*
* JLP 
* Version 23-03-2020
-------------------------------------------------------------------*/
#include "jlp_fitsio.h"

/*
#define DEBUG
*/

/* defined here:
int JLP_LoadFITSImage(char *filename1, char *comments1,
                      double **dble_image1, int *nx1, int *ny1);
int JLP_SaveFITSImage(char *filename1, char *comments1,
                      double *dble_image1, int nx1, int ny1);
*/

/******************************************************************
* Load image from FITS file
*
* INPUT:
* filename1: name of the file to be read 
*
* OUTPUT:
* comments1: comments of the file that was read
* dble_image1: array with the data contained in FITS file
* nx1, ny1: size of the image 
*
*******************************************************************/
int JLP_LoadFITSImage(char *filename1, char *comments1,
                      double **dble_image1, int *nx1, int *ny1)
{
char errmess1[200];
int status, nz1, iframe;

// iframe: image plane to be loaded (from 1 to nz1)
iframe = 1;
status = JLP_RDFITS_2D_dble(dble_image1, nx1, ny1, &nz1, iframe, filename1,
                             comments1, errmess1);
if (status) {
  fprintf(stderr, "Couldn't load image from %s \n %s\n", filename1, errmess1);
  }

return(status);
}
/******************************************************************
* Save image to FITS files
*
* INPUT:
* filename1: name of the file to be read
*
* OUTPUT:
* comments1: comments of the file that was read
* dble_image1: array with the data contained in FITS file
* nx1, ny1: size of the image
*
*******************************************************************/
int JLP_SaveFITSImage(char *filename1, char *comments1,
                      double *dble_image1, int nx1, int ny1)
{
char errmess1[200];
int status;

status = JLP_WRFITS_2D_dble(dble_image1, nx1, ny1, nx1, filename1,
                             comments1, errmess1);
if (status) {
  fprintf(stderr, "Couldn't save image to %s \n %s\n", filename1, errmess1);
  }

return(status);
}
