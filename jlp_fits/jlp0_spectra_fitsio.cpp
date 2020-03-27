/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* jlp0_spectra_fitsio.c
*
* To read and write FITS spectrum tables 
* Using "FITSIO" C package.
*
* Please use rd_fits_spectrum and wr_fits_spectrum (in ../sourcc) 
* to test those routines.
*
* Codes used by FITS:
* L: logical
* I: 16-bit integer
* J: 32-bit integer
* A: character
* E: 32-bit floating-point number
* D: 64-bit floating-point number
*
* int jlp_read_spfits(char *infile, char *comments, float **wavelength,
*                     float **flux_unnorm, float **flux_norm, float **snr, 
*                     long **order_nb, long *nrows, int firstrow,
*                     int nelements, int icorot, int vm, int italk);
* int jlp_read_spfits_fast(char *infile, char *comments, float *flux_norm, 
*                          long nrows, int firstrow, int nelements, 
*                          int icorot, int italk);
* int jlp_write_spfits(char *filename, char *comments, float *wavel, 
*                      float *flux, float *snr, long nrows, int italk);
*
***** Info about od ***********************************************
* To dump a file to the screen and see Ascii and Hexa codes:
*     od -cx file_name
* each line with 32 bytes
* first block of 512 bytes from first up to line starting with 1000 (Hexa)
* then from 2000 (2nd block), 3000 (3rd block), etc.
* (WARNING: all the lines are not displayed if filled with zeroes...)
* for decimal address:
*      od -Ad -cx file_name
*******************************************************************
*
* JLP
* Version 02/03/2016 
---------------------------------------------------------------------*/
#include <stdio.h>
#include <malloc.h>
/* #include <cfitsio/fitsio.h>   fitsio.h should be in "/usr/include/cfitsio" */
#include "fitsio.h"   /* fitsio.h should be in $(CFITSIO_INCL_DIR) */

#include   "jlp_fitsio.h"
#include   "jlp_spfits.h"

#define  MAXIMUM(x,y)    (((x) > (y)) ? (x) : (y))
#define  MINIMUM(x,y)    (((x) < (y)) ? (x) : (y))
#define  POW2(x)    (x)*(x)

/* talk  = 0 (no info) 
*       = 1 (some info output to screen) 
*       = 2 (all info output to screen) 
*/
static int talk=2;

static int jlp_read_sp_prim_hdu_info(fitsfile *fptr, spec_info *sp0);
static int jlp_fits_read_short(fitsfile *fptr, const char *key_label, 
                               int *value);
static int jlp_read_chdu_info(fitsfile *fptr, int *status);
static int jlp_rd_bintab_spec(fitsfile *fptr, long *nrows, float **wavelength, 
                              float **flux_unnorm, float **flux_norm, 
                              float **snr, long **order_nb, int icorot,
                              int firstrow, int nelements, int vm);
static int jlp_rd_bintable_corot(fitsfile *fptr, float **wavelength,
                                 float **flux_unnorm, float **flux_norm, 
                                 float **snr, long **order_nb, int firstrow,
                                 int elements, int vm);
static int jlp_rd_bintable_kurucz(fitsfile *fptr, float **wavelength, 
                                  float **flux_norm, int firstrow, 
                                  int nelements, int vm);
static int jlp_write_spheader(fitsfile **fptr, char *filename, char *comments, 
                              const long nrows, const int tfields);

/*
#define DEBUG 1
*/
/**********************************************************************
* jlp_read_spfits
* To read spectrum FITS files (with primary HDU and binary table)
* with virtual memory allocation
*
* INPUT:
* infile: name of FITS file
* icorot: 1 if COROT/GAUDI format
* vm: 1 if virtual memory allocation
*
* firstrow: first element in the column to read (0 if all)
* nelements: number of elements to read (0 if all)
*
* OUTPUT:
* comments: OBJECT field in FITS file
**********************************************************************/
int jlp_read_spfits(char *infile, char *comments, float **wavelength,
                    float **flux_unnorm, float **flux_norm, float **snr, 
                    long **order_nb, long *nrows, int firstrow,
                    int nelements, int icorot, int vm, int italk)
{
long     naxis, naxis1, naxis2;
int      istat, n_hdu, hdu_num, hdu_type, istatus;
char     filename[61], *pcc, err_message[81], commts[81];
fitsfile *fptr;
spec_info sp0;

if(nelements < 0 || firstrow < 0) {
 printf("jlp_read_spfits/Error: nelements = %d, firstrow = %d\n", nelements,
         firstrow);
 return(-1);
 }

istatus = -1;
/* Save value to static variable: */
talk = italk;

   strncpy(filename,infile,40);
   filename[40]='\0';
/* Check input characters strings (pb if fortran...) */
   pcc = filename;
   while(*pcc && *pcc != ' ') pcc++;
   if(*pcc == ' ') *pcc = '\0';

/* Check filename syntax and add ".fits" if no extension is present: */
   pcc = filename;
   while(*pcc && *pcc != '.') pcc++;
   if(*pcc != '.') strcpy(pcc,".fits");
   
/* Open file */
   istat = 0;
   fits_open_file(&fptr,filename,READONLY,&istat);
   if (istat) {
     fits_read_errmsg(err_message);
     printf("jlp0_rdfits/ Cannot open input file : >%s< istat=%d\n %s \n",
            filename,istat,err_message);
     return(-1);
   }      

/* Display keywords in the primary header: */
   istat = 0;
   if(talk) jlp_read_chdu_info(fptr, &istat);

/* General info about the FITS file: */
/* Number of HDU present in the file: */
   istat = 0;
   fits_get_num_hdus(fptr, &n_hdu, &istat);
   if(istat != 0) n_hdu = 1;

   if(n_hdu > 1)
     {
     if(talk > 1) printf("=> %d HDU's are present in this file\n", n_hdu);
     jlp_read_sp_prim_hdu_info(fptr, &sp0);

/* If more than one HDU: moves to next HDU: */
/* (Primary HDU has hdu_num=1) */
     for(hdu_num = 2; hdu_num <= n_hdu; hdu_num++) 
       {
       istat = 0;
/* Moves to absolute HDU number: */
       fits_movabs_hdu(fptr, hdu_num, &hdu_type, &istat);
       if(istat) 
           {
           fits_read_errmsg(err_message);
           printf("Error accessing #%d HDU istat=%d\n %s\n",
                  hdu_num,istat,err_message); 
           break;
           }
/* Read keywords in the header: */
       istat = 0;
       if(talk) jlp_read_chdu_info(fptr, &istat);
/* DATE is observing day (Julian Date): */
       istat = 0;
       fits_read_key_str(fptr, "FILENAME", sp0.filename, commts, &istat);
       switch(hdu_type)
         {
         case IMAGE_HDU:
           istat = 0;
           fits_read_key_lng(fptr, "NAXIS", &naxis, commts, &istat);
           istat = 0;
           fits_read_key_lng(fptr, "NAXIS1", &naxis1, commts, &istat);
           istat = 0;
           fits_read_key_lng(fptr, "NAXIS2", &naxis2, commts, &istat);
           printf("=> Sorry HDU #%d is an IMAGE, naxis=%d nx=%d ny=%d\n", 
                           hdu_num, (int)naxis, (int)naxis1, (int)naxis2);
           break;
         case ASCII_TBL:
           printf("=> Sorry HDU #%d is an ASCII Table\n", hdu_num);
           break;
         case BINARY_TBL:
           if(talk > 1) printf("=> OK: HDU #%d is a BINARY table\n", hdu_num);

/* Read binary table if UV table has not been found yet
* or if coordinates still need to be read: */
            istatus = jlp_rd_bintab_spec(fptr, nrows, wavelength, flux_unnorm,
                                         flux_norm, snr, order_nb, icorot,
                                         firstrow, nelements, vm);
/* Overwrite comments: */
            sprintf(comments,"%s, %s nrows=%ld", infile, sp0.date_obs, *nrows);
           break;
         default:
         case ANY_HDU:
           printf("=> HDU #%d: undefined type!\n", hdu_num);
           break;
/* EOF switch: */
         }
/* EOF "for" loop with hdu_num: */
       }

/* EOF n_hdu > 1 */
     }
/******** Case of only one HDU: ***********************/
    else
     {
     printf("=> Sorry only primary HDU in this file\n");
     }

/* Close file: */
    fits_close_file(fptr,&istat);

return(istatus);
}
/**********************************************************************
* jlp_read_spfits_fast
* To read spectrum FITS files (with primary HDU and binary table)
* without memory allocation and only read the flux column
*
* INPUT:
* infile: name of FITS file
* icorot: 1 if COROT/GAUDI format
*
* firstrow: first element in the column to read (0 if all)
* nelements: number of elements to read (0 if all)
*
* OUTPUT:
* comments: OBJECT field in FITS file
**********************************************************************/
int jlp_read_spfits_fast(char *infile, char *comments, float *flux_norm, 
                         int firstrow, int nelements, int icorot, int italk)
{
float    *flux_unnorm, *snr, *wavelength;
long     *order_nb, nrows;
int      istatus, vm;

 wavelength = NULL; flux_unnorm = NULL; snr = NULL; order_nb = NULL;
 vm = 0;
 istatus = jlp_read_spfits(infile, comments, &wavelength, &flux_unnorm, 
                           &flux_norm, &snr, &order_nb, &nrows, 
                           firstrow, nelements, icorot, vm, italk);

return(istatus);
}
/******************************************************************
* Binary tables
* Common mandatory keywords for FITS-IDI tables (p30, C. Flatters)
* EXTNAME  A  Table name
* TABREV   I  Revision number of the table definition
* NO_STKD  I  Number of Stokes parameters
* STK_1    I  First Stokes parameter 
* NO_BAND  I  Number of bands
* NO_CHAN  I  Number of spectral channels
* REF_FREQ E  File reference frequency in Hz
* CHAN_BW  E  Channel bandwidth in Hz for the 1st band ...
* REF_PIXL E  Reference pixel for the frequency axis 
*
* INPUT:
* fptr: FITS file pointer
*
* OUTPUT:
* nrows: number of rows of the binary table
* status: 0 ik OK
******************************************************************/
static int jlp_rd_bintab_spec(fitsfile *fptr, long *nrows, float **wavelength, 
                              float **flux_unnorm, float **flux_norm, 
                              float **snr, long **order_nb, int icorot,
                              int firstrow, int nelements, int vm)
{
int tfields, nfields=20, nmax, istat;
long naxis, naxis1, naxis2;
register int i;
char ext_name[30], xtension[30], keywd[8], commts[81];
char ttype[nfields][20], tform[nfields][20], tunit[nfields][20];

if(talk > 1) printf("\n ==> Binary table \n"),

istat=0;
fits_read_key_str(fptr, "XTENSION", xtension, commts, &istat);
if(talk && istat==0) printf("XTENSION=%s\n", xtension);

fits_read_key_str(fptr, "EXTNAME", ext_name, commts, &istat);

/* Number of fields per row: */
jlp_fits_read_short(fptr, "TFIELDS", &tfields);

/* Number of axes: (should be 2 for binary tables)*/
fits_read_key_lng(fptr, "NAXIS", &naxis, commts, &istat);

/* Number of 8-bit bytes in each row: */
fits_read_key_lng(fptr, "NAXIS1", &naxis1, commts, &istat);

/* Number of rows in the table: */
fits_read_key_lng(fptr, "NAXIS2", &naxis2, commts, &istat);
if(talk > 1) printf(" naxis=%d naxis1=%d (bytes/row) naxis2=%d (rows)\n",
                     (int)naxis, (int)naxis1, (int)naxis2);

/* If the user has not selected the number of elements, read all data: */
if(nelements == 0) nelements = naxis2;
if(firstrow == 0) firstrow = 1;
if(vm) *nrows = naxis2;

/* Description of the fields: */
nmax = MINIMUM(tfields,nfields);
for(i = 1; i <= nmax; i++)
  {
  sprintf(keywd,"TTYPE%d",i);
  fits_read_key_str(fptr, keywd, ttype[i], commts, &istat);
  sprintf(keywd,"TFORM%d",i);
  fits_read_key_str(fptr, keywd, tform[i], commts, &istat);
  sprintf(keywd,"TUNIT%d",i);
  fits_read_key_str(fptr, keywd, tunit[i], commts, &istat);
  if(talk > 0) printf(" Field #%d : %s %s %s \n",
                       i, ttype[i], tform[i], tunit[i]);
  }

/* Decode the content: */
  if(icorot)
   istat = jlp_rd_bintable_corot(fptr, wavelength, flux_unnorm, 
                                 flux_norm, snr, order_nb, firstrow, 
                                 nelements, vm);
  else
   istat = jlp_rd_bintable_kurucz(fptr, wavelength, flux_norm,
                                  firstrow, nelements, vm);


return(istat);
}
/****************************************************************
* JLP interface to read an int keyword ('I' = 16-bit)
****************************************************************/
static int jlp_fits_read_short(fitsfile *fptr, const char *key_label, 
                               int *value)
{
int istat;
short svalue;
char comment[81];
  istat = 0;
  fits_read_key(fptr, TSHORT, key_label, &svalue, comment, &istat);
  if(istat == KEY_NO_EXIST) svalue = 0; 
*value = (int)svalue;
 return(istat);
}
/****************************************************************
* Read useful information from primary HDU (Header + Data Unit)
****************************************************************/
static int jlp_read_sp_prim_hdu_info(fitsfile *fptr, spec_info *sp0)
{
float work; 
char commts[81];
int      istat;

/***********************************************************
*/
    istat = 0;
    fits_read_key_flt(fptr, "RA", &(sp0->obs_ra), commts, &istat);
    istat = 0;
    fits_read_key_flt(fptr, "DEC", &(sp0->obs_dec), commts, &istat);
    istat = 0;
    fits_read_key_str(fptr, "RASIX", sp0->obs_ra_str, commts, &istat);
    istat = 0;
    fits_read_key_str(fptr, "DECSIX", sp0->obs_dec_str, commts, &istat);

    if(talk > 1)
    {
    printf(" obs_ra=%.4e obs_dec=%.4e (%s, %s)\n",
            sp0->obs_ra, sp0->obs_dec, sp0->obs_ra_str, sp0->obs_dec_str);
    }

/* Miscellaneous: */
    istat = 0;
    fits_read_key_str(fptr, "OBJECT", sp0->x_name, commts, &istat);
    istat = 0;
    fits_read_key_str(fptr, "DATE", sp0->date, commts, &istat);
    istat = 0;
    fits_read_key_str(fptr, "DATE-OBS", sp0->date_obs, commts, &istat);
    if(talk > 1)
       printf(" Object: %s, date observation: %s  creation fichier: %s\n", 
                sp0->x_name, sp0->date_obs, sp0->date);

/* Velocity channel information */
    istat = fits_read_key_flt(fptr, "SUN_COR", &work, commts, &istat);
    sp0->sun_cor = work;
    if(talk > 1)
       printf(" Heliocentric correction: %.3e \n", sp0->sun_cor);

return(0);
}
/****************************************************************
* Read all keywords from the header of the current HDU 
*
****************************************************************/
static int jlp_read_chdu_info(fitsfile *fptr, int *status)
{
int nkeys, key_now;
char kwd_name[20], kwd_val[80], kwd_comments[80];
register int k;

/* Get key_now=current index key_pos and nkeys=number of keywords */
*status = 0;
fits_get_hdrpos(fptr, &nkeys, &key_now, status); 
printf(" jlp_read_chdu_info/nkeys=%d, key_now=%d \n",nkeys,key_now);

if(talk > 1)
 {
 for(k = 0; k < nkeys; k++)
   {
   *status = 0;
   fits_read_keyn(fptr, k, kwd_name, kwd_val, kwd_comments, status); 
   printf("%s = %s / %s \n", kwd_name, kwd_val, kwd_comments); 
   }
 }

return(*status);
}
/******************************************************************
* Spectrum binary table in COROT format 
* (i.e. FITS files extracted from GAUDI data base)
*
* COROT format:
* XTENSION=BINTABLE
* EXTNAME=SPECTRUM TABREV=0
* Field #1 : WAVELENGTH 1E ANGSTROMS
* Field #2 : FUNNORM 1E
* Field #3 : FNORM 1E
* Field #4 : SNR 1E
* Field #5 : ORDER 1J
*
* INPUT:
* fptr: FITS file pointer
*
* OUTPUT:
* status: 0 ik OK
*
******************************************************************/
static int jlp_rd_bintable_corot(fitsfile *fptr, float **wavelength, 
                                 float **flux_unnorm, float **flux_norm, 
                                 float **snr, long **order_nb,
                                 int firstrow, int nelements, int vm)
{
int any_null_values, colnum, status, istat;
char keywd[16];

if(nelements <= 0) {printf("jlp_rd_bintable_corot/ Error nelements=%d\n",
                            nelements); return(-1);
                   }
if(firstrow <= 0) firstrow = 1;


/* Allocate memory: */
if(vm) {
  istat = 0;
  if((*wavelength = (float *) malloc(nelements*sizeof(float))) == NULL) {
    printf("jlp_read_sp_bintable/Error allocating memory, nelements=%d\n",
        nelements);
    istat = -2;  fits_close_file(fptr,&istat); return(-2);
   }
  *order_nb = (long *)malloc(nelements*sizeof(long));
  *flux_unnorm = (float *)malloc(nelements*sizeof(float));
  *flux_norm = (float *)malloc(nelements*sizeof(float));
  *snr = (float *)malloc(nelements*sizeof(float));
}

/* Read binary table: */

/********* Wavelength **************************/
/*  Field #1 : WAVELENGTH 1E ANGS*/ 
  status = 0;
/* CASESEN: case-sensitive for matching the keyword
* CASEINSEN: case will be ignored for matching the keyword
*/
  strcpy(keywd, "WAVELENGTH");
  fits_get_colnum(fptr, CASESEN, keywd, &colnum, &status);
  if(status) {
    printf("ERROR/Column WAVELENGTH is not there! \n");
    istat = -1;
    }
  else {
  if(talk > 1) printf(" Wavelength (angstroms): colnum=%d \n",colnum);
  status = 0;
  fits_read_col_flt(fptr, colnum, 1, firstrow, nelements, 0,
                    *wavelength, &any_null_values, &status);
  if(status) {
     printf("fits_read_col/col#%d status=%d\n",colnum,status);
     if(status == 302) printf("Bad column number!");
     istat = -1;
     }
   }

/********* Flux (un-normalized) **************************/
/*  Field #2 : FUNNORM 1E */ 
  status = 0;
  strcpy(keywd, "FUNNORM");
  fits_get_colnum(fptr, CASESEN, keywd, &colnum, &status);
  if(status) {
    printf("ERROR/Column FUNNORM is not there! \n");
    istat = -1;
    }
  else {
  if(talk > 1) printf(" Flux (un-normalized): colnum=%d \n",colnum);
  status = 0;
  fits_read_col_flt(fptr, colnum, 1, firstrow, nelements, 0,
                    *flux_unnorm, &any_null_values, &status);
  if(status) {
     printf("fits_read_col/col#%d status=%d\n",colnum,status);
     if(status == 302) printf("Bad column number!");
     istat = -1;
     }
   }

/********* Flux (normalized) **************************/
/*  Field #3 : FNORM 1E */ 
  status = 0;
  strcpy(keywd, "FNORM");
  fits_get_colnum(fptr, CASESEN, keywd, &colnum, &status);
  if(status) {
    printf("ERROR/Column FNORM is not there! \n");
    istat = -1;
    }
  else {
  if(talk > 1) printf(" Flux (normalized): colnum=%d \n",colnum);
  status = 0;
  fits_read_col_flt(fptr, colnum, 1, firstrow, nelements, 0,
                    *flux_norm, &any_null_values, &status);
  if(status) {
     printf("fits_read_col/col#%d status=%d\n",colnum,status);
     if(status == 302) printf("Bad column number!");
     istat = -1;
     }
  }

/********* SNR  **************************/
/*  Field #4 : SNR 1E */ 
  status = 0;
  strcpy(keywd, "SNR");
  fits_get_colnum(fptr, CASESEN, keywd, &colnum, &status);
  if(status) {
    printf("ERROR/Column SNR is not there! \n");
    istat = -1;
    }
  else {
  if(talk > 1) printf(" Flux (normalized): colnum=%d \n",colnum);
  status = 0;
  fits_read_col_flt(fptr, colnum, 1, firstrow, nelements, 0,
                    *snr, &any_null_values, &status);
  if(status) {
     printf("fits_read_col/col#%d status=%d\n",colnum,status);
     if(status == 302) printf("Bad column number!");
     istat = -1;
     }
  }

/********* Order number **************************/
/* Field #5 : ORDER 1J */ 
  status = 0;
  strcpy(keywd, "ORDER");
  fits_get_colnum(fptr, CASESEN, keywd, &colnum, &status);
  if(status) {
    printf("ERROR/Column ORDER is not there! \n");
    istat = -1;
    }
  else {
  if(talk > 1) printf("ORDER (order number) colnum=%d \n",colnum);
  status = 0;
  fits_read_col_lng(fptr, colnum, 1, firstrow, nelements, 0,
                    *order_nb, &any_null_values, &status);
  if(status) {
     printf("fits_read_col/col#%d status=%d\n",colnum,status);
     if(status == 302) printf("Bad column number!");
     istat = -1;
     }
  }

return(istat);
}
/******************************************************************
* Spectrum binary table in JLP's FITS format used for Kurucz files 
*
* JLP format:
XTENSION=BINTABLE
 EXTNAME=SPECTRUM TABREV=0
 Field #1 : WAVELENGTH 1E NANOMETERS 
 Field #2 : FNORM 1E
*
* INPUT:
* fptr: FITS file pointer
* nrows: number of rows of the binary table
*
* OUTPUT:
* status: 0 ik OK
*
******************************************************************/
static int jlp_rd_bintable_kurucz(fitsfile *fptr, float **wavelength, 
                                  float **flux_norm, int firstrow, 
                                  int nelements, int vm)
{
int any_null_values, status, istat, colnum;

if(nelements <= 0) {printf("jlp_rd_bintable_kurucz/ Error nelements=%d\n",
                            nelements); return(-1);
                   }
if(firstrow <= 0) firstrow = 1;

/* Allocate memory: */
if(vm) {
  istat = 0;
  if((*wavelength = (float *) malloc(nelements*sizeof(float))) == NULL)
   {
   printf("jlp_read_sp_bintable/fatal error allocating memory, nelements=%d\n",
          nelements);
   istat = -2;  fits_close_file(fptr,&istat); return(-2);
   }
  *flux_norm = (float *)malloc(nelements*sizeof(float));
  }

/* Read binary table: */
 
/* If the user has selected the column number, read directly this column: */
/* In case of vm != 0, possibility of choice, by setting the array pointers
* of wavelength and flux_norm to NULL: 
*/
  if(*wavelength != NULL) { 
    colnum = 1;
    status = 0;
    status = fits_read_col_flt(fptr, colnum, 1, firstrow, nelements, 0,
                               *wavelength, &any_null_values, &status);
    if(status) printf("fits_read_col/col#%d status=%d\n",colnum,status);
    }
  if(*flux_norm != NULL) { 
    colnum = 2;
    status = 0;
/*
    status = fits_read_col_flt(fptr, colnum, 1, firstrow, nelements, 0,
                               *flux_norm, &any_null_values, &status);
*/
    status = fits_read_col_flt(fptr, colnum, firstrow, 1, nelements, 0,
                               *flux_norm, &any_null_values, &status);
    if(status) printf("fits_read_col/col#%d status=%d\n",colnum,status);
    }

return(istat);
}
/******************************************************************************
* To write FITS tables in a simple format
* with 2 or 3 columns
* (2 columns only, if snr == NULL)
*
******************************************************************************/
int jlp_flt_write_spfits(char *filename, char *comments, float *wavel, 
                         float *flux, float *snr, long nrows, int italk)
{
char err_message[81];
int istat, tfields;
/* *fptr = pointer to FITS file, defined in fitsio.h */
fitsfile *fptr;

/* Save value to static variable: */
  talk = italk;

  if(snr != NULL)
    tfields = 3;
  else
    tfields = 2;

// Write the header:
  jlp_write_spheader(&fptr, filename, comments, nrows, tfields);

/* Write data to columns: */
  istat = 0;
  fits_write_col_flt(fptr, 1, 1, 1, nrows, wavel, &istat);
  if(istat) {
   fits_read_errmsg(err_message);
   printf("jlp_write_fits_spect/Error writing wavelength data, istat=%d\n %s\n",
           istat, err_message);
   }
  istat = 0;
  fits_write_col_flt(fptr, 2, 1, 1, nrows, flux, &istat);
  if(istat) {
   fits_read_errmsg(err_message);
   printf("jlp_write_fits_spect/Error writing flux data, istat=%d\n %s\n",
           istat, err_message);
   }
  if(snr != NULL) {
  istat = 0;
  fits_write_col_flt(fptr, 3, 1, 1, nrows, snr, &istat);
  if(istat) {
   fits_read_errmsg(err_message);
   printf("jlp_write_fits_spect/Error writing snr data, istat=%d\n %s\n",
           istat, err_message);
   }
   }

/* Close file on disk: */
  istat = 0;
  fits_close_file(fptr, &istat);
  if(istat)
   {
   fits_read_errmsg(err_message);
   printf("jlp_write_fits_spect/Error when closing file %s, istat=%d\n %s\n",
           filename, istat, err_message);
   }

return(0);
}
/******************************************************************************
* To write FITS tables in a simple format
* with 2 or 3 columns
* (2 columns only, if snr == NULL)
*
******************************************************************************/
int jlp_dble_write_spfits(char *filename, char *comments, double *wavel, 
                          double *flux, double *snr, long nrows, int italk)
{
char err_message[81];
int istat, tfields;
/* *fptr = pointer to FITS file, defined in fitsio.h */
fitsfile *fptr;

/* Save value to static variable: */
  talk = italk;

  if(snr != NULL)
    tfields = 3;
  else
    tfields = 2;

// Write the header:
  jlp_write_spheader(&fptr, filename, comments, nrows, tfields);

/* Write data to columns: */
  istat = 0;
  fits_write_col_dbl(fptr, 1, 1, 1, nrows, wavel, &istat);
  if(istat) {
   fits_read_errmsg(err_message);
   printf("jlp_write_fits_spect/Error writing wavelength data, istat=%d\n %s\n",
           istat, err_message);
   }
  istat = 0;
  fits_write_col_dbl(fptr, 2, 1, 1, nrows, flux, &istat);
  if(istat) {
   fits_read_errmsg(err_message);
   printf("jlp_write_fits_spect/Error writing flux data, istat=%d\n %s\n",
           istat, err_message);
   }
  if(snr != NULL) {
  istat = 0;
  fits_write_col_dbl(fptr, 3, 1, 1, nrows, snr, &istat);
  if(istat) {
   fits_read_errmsg(err_message);
   printf("jlp_write_fits_spect/Error writing snr data, istat=%d\n %s\n",
           istat, err_message);
   }
   }

/* Close file on disk: */
  istat = 0;
  fits_close_file(fptr, &istat);
  if(istat)
   {
   fits_read_errmsg(err_message);
   printf("jlp_write_fits_spect/Error when closing file %s, istat=%d\n %s\n",
           filename, istat, err_message);
   }

return(0);
}

/******************************************************************************
* To write FITS tables in a simple format
* with 2 or 3 columns
* (2 columns only, if snr == NULL)
*
******************************************************************************/
static int jlp_write_spheader(fitsfile **fptr, char *filename, char *comments, 
                              const long nrows, const int tfields)
{
long int  naxis; 
int istat;
char buf1[40], buf2[40], buf3[40];
char *ttype[3], *tform[3], *tunit[3], err_message[81], extname[20];
register int i;

 istat = 0;
 fits_create_file(fptr, filename, &istat);
/* Overwrite file if already there: */
 if(istat) {
   printf("File already here: please delete it first\n");
/*
   sprintf(command,"rm %s",filename);
   JLP_SYSTEM(command);
   istat = 0;
   fits_create_file(fptr, filename, &istat);
   if(istat)
     {
     fits_read_errmsg(err_message);
     printf("jlp_write_fits_spect/Error when opening FITS file %s, istat=%d\n %s\n",
            filename, istat, err_message);
     return(-1);
     }
*/
   }

/* Primary HDU */
  naxis = 0;
  for(i = 0; i < 3; i++){
    ttype[i] = (char *)malloc(20 * sizeof(char));
    tform[i] = (char *)malloc(20 * sizeof(char));
    tunit[i] = (char *)malloc(20 * sizeof(char));
    }
/* Codes used by FITS:
* L: logical
* I: 16-bit integer
* J: 32-bit integer
* A: character
* E: 32-bit floating-point number
* D: 64-bit floating-point number
*/
  strcpy(ttype[0],"WAVELENGTH");
  strcpy(tform[0],"1E");
  strcpy(tunit[0],"NANOMETERS");
  strcpy(ttype[1],"FNORM");
  strcpy(tform[1],"1E");
  strcpy(tunit[1]," ");
  strcpy(ttype[2],"SNR");
  strcpy(tform[2],"1E");
  strcpy(tunit[2]," ");
  strcpy(extname,"SPECTRUM");
  istat = 0;
/* User Manual, p 38 */
  fits_create_tbl(*fptr, BINARY_TBL, naxis, tfields, ttype, tform, tunit, extname, &istat);
  for(i = 0; i < 3; i++){
    free(ttype[i]);
    free(tunit[i]);
    }
  if(istat)
   {
   fits_read_errmsg(err_message);
   printf("jlp_write_fits_spect/Error when writing header, istat=%d\n %s\n",
           istat, err_message);
     return(-1);
   }

/* Moves to primary HDU to add some more info: */
  fits_movabs_hdu(*fptr, 1, NULL, &istat);

/* Copy comments to OBJECT for compatibility with other software*/
/* "OBJECT " */
/* Makes it shorter to avoid problems: */
  comments[30] = '\0';
  istat = 0;
  strcpy(buf1, "OBJECT  ");
  strcpy(buf2,"");
  fits_update_key(*fptr, TSTRING, buf1, comments, buf2, &istat);
  if(istat)
   {
   fits_read_errmsg(err_message);
   printf("jlp_write_fits_spect/Error when writing keyword OBJECT, istat=%d\n >%s<\n %s\n",
           istat, comments, err_message);
   }

/* Date */
  istat = 0;
  fits_write_date(*fptr, &istat);

/* "AUTHOR  " */
  istat = 0;
  strcpy(buf1, "AUTHOR  ");
  strcpy(buf2, "Jean-Louis Prieur");
  strcpy(buf3, "With cftiso (version 03/10/2014)");
  fits_update_key(*fptr, TSTRING, buf1, buf2, buf3, &istat); if(istat) {
   fits_read_errmsg(err_message);
   printf("jlp_write_fits_spect/Error when writing keyword AUTHOR, istat=%d\n %s\n",
           istat, err_message);
   }
/* Moves to secondary HDU: */
  fits_movabs_hdu(*fptr, 2, NULL, &istat);
/* "NAXIS2  " */
  istat = 0;
  strcpy(buf1, "NAXIS2  ");
  strcpy(buf2, "number of rows in table");
  fits_update_key_lng(*fptr, buf1, nrows, buf2, &istat);
  if(istat) {
   fits_read_errmsg(err_message);
   printf("jlp_write_fits_spect/Error updating keyword NAXIS2, istat=%d\n %s\n",
           istat, err_message);
  }

return(0);
}
