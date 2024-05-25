/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* jlp_wrfits0.cpp
* To write FITS formatted 2-D image files
* Formats supported are : FITS 32,-32 
* i.e., 4-byte integer (JLP_FORMAT=7) or 4-byte float values (JLP_FORMAT=8)
* JLP comments and jlp_descriptors
*
* Contains:
* int JLP_WRFITS_2D_dble_descr(d_array,nx1,ny1,idim,filename,comments,jlp_descr,
*               istatus,out_type)
*
* JLP
* Version 02-10-2014
---------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include "jlp_fitsio.h"
/* #include <cfitsio/fitsio.h>   fitsio.h should be in "/usr/include/cfitsio" */
#include "fitsio.h"   /* fitsio.h should be in $(CFITSIO_INCL_DIR) */

/*
int JLP_WRFITS_2D_flt(float *array1, int nx1, int ny1, int idim1,
                      char *filename, char *comments, char *err_mess);
int JLP_WRFITS_2D_dble(double *d_array1, int nx1, int ny1, int idim1,
                       char *filename, char *comments, char *err_mess);
int JLP_WRFITS_2D_dble_descr(double *d_array1, int nx1, int ny1, int idim1,
                             char *filename, char *comments, char *jlp_descr, 
                             int *istatus, int out_type, char *err_mess);
*/
/*
#define DEBUG 1
*/

static int jlp_wdescr_fits(char *jlp_descr, fitsfile *fptr);

/**********************************************************************
*
**********************************************************************/
int JLP_WRFITS_2D_flt(float *array1, int nx1, int ny1, int idim1,
                      char *filename, char *comments, char *err_mess)
{
char jlp_descr[1]; 
double *d_array1;
int i, j, istatus, out_type;

out_type = 1;
jlp_descr[0] = '\0';

d_array1 = new double(nx1 * ny1);
for(j = 0; j < ny1; j++) {
  for(i = 0; i < nx1; i++) {
    d_array1[i + j * nx1] = array1[i + j * idim1];
  }
 }
JLP_WRFITS_2D_dble_descr(d_array1, nx1, ny1, idim1, filename, comments, 
                         jlp_descr, &istatus, out_type, err_mess);
delete d_array1;

return(istatus);
}
/**********************************************************************
*
**********************************************************************/
int JLP_WRFITS_2D_dble(double *d_array1, int nx1, int ny1, int idim1,
                       char *filename, char *comments, char *err_mess)
{
char jlp_descr[1]; 
int istatus, out_type;

out_type = 1;
jlp_descr[0] = '\0';
JLP_WRFITS_2D_dble_descr(d_array1, nx1, ny1, nx1, filename, comments, 
                         jlp_descr, &istatus, out_type, err_mess);
return(istatus);
}

/**********************************************************************
* JLP_WRFITS_2D_dble_descr
*
* Output format:
* out_type = 0 : integer (32)
* out_type = 1 : real (-32) 
**********************************************************************/
int JLP_WRFITS_2D_dble_descr(double *d_array1, int nx1, int ny1, int idim1,
                             char *filename, char *comments, char *jlp_descr, 
                             int *istatus, int out_type, char *err_mess)
{
char  *pcc, lhcuts[32], err_message[81], author[80];
/*
  char command[80];
*/
char      buf1[40], buf2[40], fname[128];
long int  naxes[2], nelements, nx, ny, idim; 
float     work, lcut, hcut, *tmp_array;
int       i, j, naxis, istat;
/* *fptr = pointer to FITS file, defined in fitsio.h */
fitsfile *fptr;      

#if DEBUG
   printf("Beginning of JLP_WRFITS_2D_dble_descr out_type=%d\n", out_type);
   printf("Output file >%s< nx=%ld ny=%ld\n", filename, nx1, ny1);
#endif

*istatus = 0;

/* Check input characters strings (pb if fortran...) */
   pcc = filename;
   while(*pcc && *pcc != ' ') pcc++;
   if(*pcc == ' ') *pcc = '\0';

   comments[79]='\0';

/* Transfer to long int variables (necessary for Fortran interface with OSF1)*/
nx = nx1; ny = ny1; idim = idim1;

/* Check filename syntax and add ".fits" if no extension is present: */
   pcc = filename;
   while(*pcc && *pcc != '.') pcc++;
   if(*pcc != '.') strcpy(pcc,".fits");

#if DEBUG
   printf("Opening output file >%s< nx=%ld ny=%ld idim=%ld\n",
          filename,nx,ny,idim);
#endif
   istat = 0;
   fits_create_file(&fptr, filename, &istat);
/* JLP99: overwrite file if already there: */
   if(istat)
    {
/* To be checked later...
    printf("File already here: I delete it \n");
    sprintf(command,"rm %s",filename);
    JLP_SYSTEM(command);
*/
// Filename should start with ! to overwrite old file:
    sprintf(fname, "!%s", filename);
    istat = 0;
    fits_create_file(&fptr, fname, &istat);
    if(istat)
     {
     fits_read_errmsg(err_message);
     printf("JLP_WRFITS_2D_dble_descr/Error when overwritting FITS file %s, istat=%d\n %s\n",
            filename, istat, err_message);
     *istatus = -1;
     return(-1);
     }
    }

/* compute max/min cuts of data */
  lcut = *d_array1; hcut = lcut;
  for(j=0; j < ny; j++)
   {for(i=0; i < nx; i++)
      { 
        work = d_array1[i + j * idim];
          if ( work < lcut) lcut = work;
           else if (hcut < work) hcut = work;
      }
   }

/* write FITS header      */
  naxis=2;
  naxes[0] = nx;
  naxes[1] = ny;
/* out_type = 0 : integer (32)
 out_type = 1 : real (-32) */ 
 if(out_type)
   fits_create_img(fptr, FLOAT_IMG, naxis, naxes, &istat);
 else
   fits_create_img(fptr, LONG_IMG, naxis, naxes, &istat);
  if(istat)
   {
   fits_read_errmsg(err_message);
   printf("JLP_WRFITS_2D_dble_descr/Error when writing header, istat=%d\n %s\n",
           istat, err_message);
   }

/* "LHCUTS  " */
  sprintf(lhcuts,"%12.5e %12.5e",lcut, hcut);
  strcpy(buf1, "LHCUTS  ");
  strcpy(buf2, "Low and high cuts");
  fits_update_key(fptr, TSTRING, buf1, lhcuts, buf2, &istat);
  if(istat)
   {
   fits_read_errmsg(err_message);
   printf("JLP_WRFITS_2D_dble_descr/Error when writing keyword LHCUTS, istat=%d\n %s\n %s\n",
           istat, lhcuts, err_message);
   }

/* Copy comments to OBJECT for compatibility with other software*/
/* "OBJECT " */
  strcpy(buf1, "OBJECT  ");
  strcpy(buf2, "Name of object");
  fits_update_key(fptr, TSTRING, buf1, comments, buf2, &istat);
  if(istat)
   {
   fits_read_errmsg(err_message);
   printf("JLP_WRFITS_2D_dble_descr/Error when writing keyword OBJECT, istat=%d\n %s\n %s\n",
           istat, comments, err_message);
   }

/* Date */
  fits_write_date(fptr, &istat);

/* Copy jlp_descr to JLPDESCR */
/* "JLPDESCR " */
  if(*jlp_descr) jlp_wdescr_fits(jlp_descr, fptr);

/* "AUTHOR  " */
  strcpy(author, "Jean-Louis Prieur");
  strcpy(buf1, "AUTHOR  ");
  strcpy(buf2, "With cfitsio (Version Oct 2014)");
  fits_update_key(fptr, TSTRING, buf1, author, buf2, &istat);
  if(istat)
   {
   fits_read_errmsg(err_message);
   printf("JLP_WRFITS_2D_dble_descr/Error when writing keyword AUTHOR, istat=%d\n %s\n",
           istat, err_message);
   }


/* write prime data       */
  nelements = nx * ny;
  if((tmp_array = (float*) malloc(nelements*sizeof(float))) == NULL)
  { printf("JLP_WRFITS_2D_dble_descr/Error when allocating memory for temporary array nel=%ld\n", 
            nelements); 
    return(-2);
  }
  for(j=0; j < ny; j++)
   for(i=0; i < nx; i++)
      tmp_array[i + j * nx] = d_array1[i + j * idim]; 

/* Write the array to the image: (automatic conversion to the correct type,
 but should give the type of "tmp_array")*/
  fits_write_img(fptr, TFLOAT, 1, nelements, tmp_array, &istat);
  free(tmp_array);
  if(istat)
   {
   fits_read_errmsg(err_message);
   printf("JLP_WRFITS_2D_dble_descr/Error when writing data to file, istat=%d\n %s\n",
           istat, err_message);
   }
  
/* Close file on disk: */
  fits_close_file(fptr, &istat);
  if(istat)
   {
   fits_read_errmsg(err_message);
   printf("JLP_WRFITS_2D_dble_descr/Error when closing file %s, istat=%d\n %s\n",
           filename, istat, err_message);
   }

return(0); 
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* Routine to write jlp_descriptors in FITS format
*
* JLP
---------------------------------------------------------------------*/
static int jlp_wdescr_fits(char *jlp_descr, fitsfile *fptr)
{
 int i, k, istat;
 char buf1[40], buf2[40];
 char buffer[81], mydescr[1024], *pc, err_message[81];

  strcpy(mydescr,jlp_descr);

/********* First fills the end of descriptors with zeroes ********/
/* Look for the first zero: */
  mydescr[1023] = '\0';
  pc = mydescr;
  while(*pc) pc++;

/* Then replaces the blanks at the end with zeroes */
  pc--;
  while(*pc == ' ') {*pc = '\0'; pc--;}

/***** Then copies the descriptors to FITS header: (1024/62=17 lines maxi) ****/
  for (k=0; k<17; k++)
  {
  strncpy(buffer,&mydescr[k*62],62);
  buffer[62] = '\0';
#ifdef DEBUG
  printf(" Writing keyword JLPDESCR \n %s\n",buffer);
#endif
     istat = 0;
     strcpy(buf1, "JLPDESCR");
     strcpy(buf2, "JLP descriptor");
     fits_write_key_str(fptr, buf1, buffer, buf2,&istat);
     if(istat)
      {
      fits_read_errmsg(err_message);
      printf("JLP_WRFITS_2D_dble_descr/Error when writing keyword JLPDESCR, istat=%d \n %s\n %s\n",
              istat, buffer, err_message);
      }
/* Stop when first zero has been found */
    for (i=0; i<62; i++) if(buffer[i] == '\0') break;
    if(i < 62) break;
  }

/* end: */
 return(istat);
}
