/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* jlp_rdfitsio.cpp
* To read FITS formatted 1-D, 2-D, and 3-D image files
* Using "FITSIO" C package.
* Formats supported are : FITS 8,16,32,-32,-64
* (i.e. 1-byte, 2-byte or 4-byte integer, and 4-byte or 8-byte float values)
* JLP: comments and jlp_descriptors
* 
* JLP
* Version 02-10-2016
---------------------------------------------------------------------*/
#include "jlp_fitsio.h"

/* Prototypes are defined in jlp_fitsio.h:

int JLP_RDFITS_2D_dble(double **array1, int *nx1, int *ny1, int *nz1,
                      int iplane, char *infile, char *comments, char *err_mess);
int JLP_RDFITS_2D_flt(float **array1, int *nx1, int *ny1, int *nz1,
                      int iplane, char *infile, char *comments, char *err_mess);
int JLP_RDFITS_2D_dble_descr(double **array1, int *nx1, int *ny1, int *nz1,
                             int iplane, char *infile, char *comments,
                             char *jlp_descr, char *err_mess);
int JLP_RD_3DXFITS_2D_dble(double **array1, int *nx1, int *ny1, int *nz1,
                           int iplane, const int ihdu, char *infile,
                           char *comments, char *err_mess);
int JLP_RD_3DXFITS_1D_dble(double **array1, int *nx1, int *ny1, int *nz1,
                           const int ix1, conts int iy1, const int ihdu,
                           char *infile, char *comments, char *err_mess);
int JLP_RD_3DXFITS_header(char *infile, char *comments, FILE *header_outfile,
                          char *err_mess);
*/

/*
#define DEBUG
*/

/* Main program to test JLP_RDFITS */
#ifdef TEST_PROGRAM
main()
{
  float        array[1000000];
  int         nx1, ny1, idim;
  char         infile[100], outfile[100];
  char         comments[81], jlp_descr[1024];
  int         istatus, dflag;
  register int i;

JLP_INQUIFMT();

  printf(" Test of jlp0_rdfits to read FITS files on disk\n");
  printf(" Version 21-01-2003\n");

      printf(" Input FITS file   : ");
      scanf("%s",infile);
      printf(" Output MIDAS/FITS file : ");
      scanf("%s",outfile);

/*
    printf(" Input FITS file   : >%s< \n",infile);
    printf(" Output FITSfile : >%s< \n",outfile);
*/

/* Set dflag to -1, since no descriptors are wanted */
idim = 1000; dflag = -1;
JLP_RDFITS(array,&nx1,&ny1,&idim,infile,comments,jlp_descr,&dflag,&istatus);
 printf(" JLP_RDFITS/istatus = %d \n",istatus);

 printf(" nx = %d, ny = %d \n",nx1,ny1);
 printf(" comments: %s \n",comments);
 printf(" image[0...20]: \n");
 for(i = 0; i < 20; i++) printf(" %f ",array[i]);

 JLP_WRITEIMAG(array,&nx1,&ny1,&idim,outfile,comments);
 printf(" JLP_WRFITS/istatus = %d \n",istatus);

JLP_END();
}
#endif

/**********************************************************************
* JLP_RDFITS_2D_UINT1
* Read 2D images and load result to a UINT1 array.
*
* INPUT:
* infile: filename
* iplane: number of the image plane of 3D data (from 1 to nz1)
* err_mess: character string used to write error messages
*         (Warning: err_mess should be declared as char *err_mess[200])
*
* OUTPUT:
* nx1, ny1: size of images
* nz1: number of images in input data cube
**********************************************************************/
int JLP_RDFITS_2D_UINT1(UINT1 **array1, int *nx1, int *ny1, int *nz1,
                      int iplane, char *infile, char *comments, char *err_mess)
{
double *dble_array1;
char jlp_descr[1024];
int dflag, status;
register int i;

/* Set dflag to 0, since descriptors are wanted */
dflag = 0;
status = jlp0_dble_rdfits(&dble_array1, nx1, ny1, nz1, iplane, infile, comments,
                         jlp_descr, dflag, err_mess);

if(!status) {
  *array1 = new UINT1[(*nx1) * (*ny1)];
  for(i = 0; i < (*nx1) * (*ny1); i++) (*array1)[i] = (UINT1)dble_array1[i];
  delete[] dble_array1;
} else {
  *array1 = NULL;
}

return(status);
}
/**********************************************************************
* JLP_RDFITS_2D_UINT2
* Read 2D images and load result to a UINT2 array.
*
* INPUT:
* infile: filename
* iplane: number of the image plane of 3D data (from 1 to nz1)
* err_mess: character string used to write error messages
*         (Warning: err_mess should be declared as char *err_mess[200])
*
* OUTPUT:
* nx1, ny1: size of images
* nz1: number of images in input data cube
**********************************************************************/
int JLP_RDFITS_2D_UINT2(UINT2 **array1, int *nx1, int *ny1, int *nz1,
                      int iplane, char *infile, char *comments, char *err_mess)
{
double *dble_array1;
char jlp_descr[1024];
int dflag, status;
register int i;

/* Set dflag to 0, since descriptors are wanted */
dflag = 0;
status = jlp0_dble_rdfits(&dble_array1, nx1, ny1, nz1, iplane, infile, comments,
                         jlp_descr, dflag, err_mess);

if(!status) {
  *array1 = new UINT2[(*nx1) * (*ny1)];
  for(i = 0; i < (*nx1) * (*ny1); i++) (*array1)[i] = (UINT2)dble_array1[i];
  delete[] dble_array1;
} else {
  *array1 = NULL;
}

return(status);
}
/**********************************************************************
* JLP_RDFITS_2D_flt
* Read 2D images and load result to a float array.
*
* INPUT:
* infile: filename
* iplane: number of the image plane of 3D data (from 1 to nz1)
* err_mess: character string used to write error messages
*         (Warning: err_mess should be declared as char *err_mess[200])
*
* OUTPUT:
* nx1, ny1: size of images
* nz1: number of images in input data cube
**********************************************************************/
int JLP_RDFITS_2D_flt(float **array1, int *nx1, int *ny1, int *nz1,
                      int iplane, char *infile, char *comments, char *err_mess)
{
double *dble_array1;
char jlp_descr[1024];
int dflag, status;
register int i;

/* Set dflag to 0, since descriptors are wanted */
dflag = 0;
status = jlp0_dble_rdfits(&dble_array1, nx1, ny1, nz1, iplane, infile, comments,
                         jlp_descr, dflag, err_mess);

if(!status) {
  *array1 = new float[(*nx1) * (*ny1)];
  for(i = 0; i < (*nx1) * (*ny1); i++) (*array1)[i] = dble_array1[i];
  delete[] dble_array1;
} else {
  *array1 = NULL;
}

return(status);
}
/**********************************************************************
* JLP_RDFITS_2D_dble
* Read 2D images and load result to a double precision array.
*
* INPUT:
* infile: filename
* iplane: number of the image plane of 3D data (from 1 to nz1)
* err_mess: character string used to write error messages
*         (Warning: err_mess should be declared as char *err_mess[200])
*
* OUTPUT:
* nx1, ny1: size of images
* nz1: number of images in input data cube
**********************************************************************/
int JLP_RDFITS_2D_dble(double **array1, int *nx1, int *ny1, int *nz1,
                       int iplane, char *infile, char *comments,
                       char *err_mess)
{
char jlp_descr[1024];
int dflag, status;

/* Set dflag to 0, since descriptors are wanted */
dflag = 0;
status = jlp0_dble_rdfits(array1, nx1, ny1, nz1, iplane, infile, comments,
                         jlp_descr, dflag, err_mess);

return(status);
}
/**********************************************************************
* JLP_RDFITS_2D_dble_descr
* Read 2D images and load result to a double array.
* Read also the descriptors, if present
*
* INPUT:
* infile: filename
* iplane: number of the image plane of 3D data (from 1 to nz1)
* err_mess: character string used to write error messages
*         (Warning: err_mess should be declared as char *err_mess[200])
* jlp_descr: character string to write the descriptors
*         (Warning: descriptors should be declared as char jlp_descr[1024])
*
* OUTPUT:
* nx1, ny1: size of images
* nz1: number of images in input data cube
**********************************************************************/
int JLP_RDFITS_2D_dble_descr(double **array1, int *nx1, int *ny1, int *nz1,
                            int iplane, char *infile,
                            char *comments, char *jlp_descr, char *err_mess)
{
int status, dflag = 1;
/* dflag = -1 (no error (warning) messages and no descriptors)
*       = 0 (no descriptors)
*       = 1 (descriptors)
*/

/* iplane: number of image plane to be read (for 2D only)*/
status = jlp0_dble_rdfits(array1,nx1,ny1,nz1,iplane,infile,comments,
                          jlp_descr,dflag,err_mess);

return(status);
}
/**********************************************************************
* JLP_VM_RDFITS_3D_dble
* Read a plane from a 3D cube and load result to a double array.
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
int JLP_VM_RDFITS_3D_dble(double **array1, int *nx1, int *ny1, int *nz1,
                     int *iplane, char *infile, char *comments,
                     char *jlp_descr, int dflag, char *err_mess)
{
int status;

status = jlp0_dble_rdfits(array1,nx1,ny1,nz1,*iplane,infile,comments,
                         jlp_descr,dflag,err_mess);

return(status);
}
/**********************************************************************
* JLP_RD_3DXFITS_2D_dble
* Read 2D images from 3D FITS files (MUSE format)
* with XTENSION = 'IMAGE' FITS data cubes
* Output to a double precision array.
*
* INPUT:
* infile: filename
* iplane: number of the image plane of 3D data (from 1 to nz1)
* ihdu: HDU number of the data cube
* err_mess: character string used to write error messages
*         (Warning: err_mess should be declared as char *err_mess[200])
*
* OUTPUT:
* nx1, ny1: size of images
* nz1: number of images in input data cube
**********************************************************************/
int JLP_RD_3DXFITS_2D_dble(double **array1, int *nx1, int *ny1, int *nz1,
                          int iplane, const int ihdu, char *infile,
                          char *comments, char *err_mess)
{
int status;

status = jlp0_3dxfits_rd_2d_dble(array1, nx1, ny1, nz1, iplane, ihdu, infile,
                                 comments, err_mess);

return(status);
}
/**********************************************************************
* JLP_RD_3DXFITS_3Dbox_dble
* Read a 2D spectrum from 3D FITS files (MUSE format)
* with XTENSION = 'IMAGE' FITS data cubes
* Output to a double precision array.
*
* INPUT:
* infile: filename
* ix2, iy2: coordinates of the first pixel to be extracted
* ihdu: HDU number of the data cube
* iplane: number of the image plane of 3D data (from 1 to nz1)
* err_mess: character string used to write error messages
*         (Warning: err_mess should be declared as char *err_mess[200])
*
* OUTPUT:
* nx1, ny1, nz1 : size of input FITS data cube
**********************************************************************/
int JLP_RD_3DXFITS_3Dbox_dble(double **array2, const int nx2,
                              int *nx1, int *ny1, int *nz1,
                              const int ix2, const int iy2,
                              const int ihdu, char *infile, char *comments,
                              char *err_mess)
{
int status;

status = jlp0_3dxfits_rd_3dbox_dble(array2, nx2, nx1, ny1, nz1, ix2, iy2,
                                    ihdu, infile, comments, err_mess);

return(status);
}
/**********************************************************************
* JLP_RD_3DXFITS_RectCube_dble
* Extract a cube bounded by a rectangular box from 3D FITS files (MUSE format)
* with XTENSION = 'IMAGE' FITS data cubes
* Output to a double precision array.
*
* INPUT:
* infile: filename
* iy_start, iy_end: y coordinates of the rectangle to be extracted
*                   (from x = 0 to x = nx1)
* ihdu: HDU number of the data cube
* iplane: number of the image plane of 3D data (from 1 to nz1)
* err_mess: character string used to write error messages
*         (Warning: err_mess should be declared as char *err_mess[200])
*
* OUTPUT:
* nx1, ny1, nz1 : size of input FITS data cube
* nx2: number of spectra that have been extracted from (0, iy_start)
*      to (0, iy_end)
**********************************************************************/
int JLP_RD_3DXFITS_RectCube_dble(double **array2, int *nx2,
                                 int *nx1, int *ny1, int *nz1,
                                 const int iy_start2, const int iy_end2,
                                 const int ihdu, char *infile, char *comments,
                                 char *err_mess)
{
int status;

status = jlp0_3dxfits_rd_rect_cube_dble(array2, nx2, nx1, ny1, nz1,
                                        iy_start2, iy_end2,
                                        ihdu, infile, comments, err_mess);

return(status);
}
/**********************************************************************
* JLP_3DXFITS_rd_header
* Read the header of 3D FITS files and write it to a new ASCII file
* (Tested in 2016 with Muse format with XTENSION = 'IMAGE' FITS data cubes)
*
* INPUT:
* infile: filename
* header_fname: name of the file that will contain the complete header
* err_mess: character string used to write error messages
*         (Warning: err_mess should be declared as char *err_mess[200])
**********************************************************************/
int JLP_3DXFITS_rd_header(char *infile, char *header_fname, char *err_mess)
{
int status;

status = jlp0_3dxfits_rd_header(infile, header_fname, err_mess);

return(status);
}
/**********************************************************************
* JLP_3DXFITS_rd_keywd
* Read a keyword from 3D FITS files
* (Tested in 2016 with Muse format with XTENSION = 'IMAGE' FITS data cubes)
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
int JLP_3DXFITS_rd_keywd(char *infile, char *keywd_name, const int ihdu,
                         char *keywd_value, char *keywd_comments,
                         char *err_mess)
{
int status;

status = jlp0_3dxfits_rd_keywd(infile, keywd_name, ihdu, keywd_value,
                               keywd_comments, err_mess);

return(status);
}
/**********************************************************************
* JLP_RDFITS : float version used by fortran routines
*
* INPUT:
* dflag = -1 (no error (warning) messages and no descriptors)
*       = 0 (no descriptors)
*       = 1 (descriptors)
* OUTPUT:
* nx1, ny1: size of images
**********************************************************************/
int JLP_RDFITS(float *array, int *nx1, int *ny1, int *idim1,
               char *infile, char *comments, char *jlp_descr,
               int *dflag, int *istatus)
{
int istat, vm_flag;
int nz1, iplane;
INT_PNTR pntr_array;

vm_flag = 0;
iplane = 1;
istat = jlp0_rdfits_2d_flt(&pntr_array,array,nx1,ny1,&nz1,&iplane,idim1,
                           infile,comments,jlp_descr,dflag,istatus,vm_flag);

return(istat);
}
/**********************************************************************
* JLP_VM_RDFITS : float version used by fortran routines.
*
* INPUT:
* dflag = -1 (no error (warning) messages and no descriptors)
*       = 0 (no descriptors)
*       = 1 (descriptors)
* OUTPUT:
* nx1, ny1: size of images
**********************************************************************/
int JLP_VM_RDFITS(INT_PNTR *pntr_array, int *nx1, int *ny1, char *infile,
                  char *comments, char *jlp_descr, int *dflag, int *istatus)
{
int idim, nz1, iplane;
int vm_flag;
float *array;
int istat;

/* iplane: number of image plane to be read (for 2D only)*/
vm_flag = 1;
iplane = 1;
array = NULL;
istat = jlp0_rdfits_2d_flt(pntr_array,array,nx1,ny1,&nz1,&iplane,&idim,
                        infile,comments,jlp_descr,dflag,istatus,vm_flag);

return(istat);
}
#ifdef SIMPLE_VERSION
int JLP_VM_READIMAG1(INT_PNTR *pntr_array, int *nx1, int *ny1, char *infile,
                     char *comments)
{
char jlp_descr[1024];
int dflag, istatus;
/* Set dflag to 0, since descriptors are wanted */
dflag = 0;
JLP_VM_RDFITS(pntr_array, nx1, ny1, infile, comments,
              jlp_descr, &dflag, &istatus);
return(0);
}
#endif
/**********************************************************************
* JLP_VM_RDFITS_3D : float version used by fortran routines
*
* INPUT:
*   dflag = -1 (no error (warning) messages and no descriptors)
*         = 0 (no descriptors)
*         = 1 (descriptors)
*   iplane > 0 if 2D image only is wanted for output
*          = 0 if 3D image with all data
* OUTPUT:
*   nx1, ny1: size of an image plane
*   nz1: number of image planes
**********************************************************************/
int JLP_VM_RDFITS_3D(INT_PNTR *pntr_array, int *nx1, int *ny1, int *nz1,
                     int *iplane, char *infile, char *comments,
                     char *jlp_descr, int *dflag, int *istatus)
{
int idim;
float *array;
int istat, vm_flag;

vm_flag = 1;
idim = 0;
array = NULL;
istat = jlp0_rdfits_2d_flt(pntr_array,array,nx1,ny1,nz1,iplane,&idim,
                           infile,comments,jlp_descr,dflag,istatus,vm_flag);

return(istat);
}
