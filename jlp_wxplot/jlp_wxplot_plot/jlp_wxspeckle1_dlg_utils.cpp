/********************************************************************
* jlp_wxspeckle1_dlg_utils.cpp
* To compute mean and std deviation from Xdisp1.log
* rho, theta (for binaries)
* Append results to a LateX file
*
* JLP
* Version 28/01/2015
********************************************************************/
#include "wx/wx.h"

#include <stdio.h>
#include <math.h>
#include <string.h>

#include "jlp_numeric.h"   // INT4, SQUARE, etc
#include "jlp_fitsio.h"   // descrip_decode_date
#include "jlp_string.h"   // jlp_cleanup_string 
#include "jlp_wxspeckle1_dlg_utils.h"
#include "ctype.h"  // toupper, isdigit, isalpha, etc

/*
#define DEBUG 1
*/

/* Defined here:
int speckle_process_data(const wxString data_fname,
                         const wxString LatexFilename,
                         const wxString original_FITS_fname,
                         const wxString processed_FITS_fname,
                         wxString &error_message);
*/
int read_int_from_cstring(char *cstring, char *mykey, int mykey_length,
                          int *ivalue, int *found);
static int speckle_astrom_mean(FILE *fp_data, double *mrho, double *drho,
                               double *mtheta, double *dtheta, int *nn);
static int speckle_photom_mean(FILE *fp_data, double *delta_mag,
                               double *d_delta_mag, int *nn);
static int speckle_output_mean(FILE *fp_tex, FILE *fp_data,
                               const wxString original_fits_fname,
                               const wxString processed_fits_fname, double mrho,
                               double drho, double mtheta, double dtheta,
                               double delta_mag, double d_delta_mag,
                               int n_astrom, int n_photom,
                               const wxString data_fname);
static int decode_merate_comments(char *comments, const char *processed_fname,
                                  char *date, char *filter, char *object_name,
                                  int *eyepiece, double *year,
                                  char *latex_filename);
static int decode_Nice_object_fits_fname(const char *processed_fname,
                                         char *latex_filename,
                                         char *upper_object_name);
static int decode_merate_object_name(const char *object_name,
                                     char *upper_object_name);

/***************************************************************************
* To process astrometric measurements and append them to Latex file
*
* INPUT:
*  original_fits_fname: FITS file obtained from the observations
*                        (full name with directory and extension)
*  processed_fits_fname: processed FITS file used for the measurements
*                        (name without directory and extension)
*
* OUTPUT:
* error_message
***************************************************************************/
int speckle_process_data(const wxString data_fname,
                         const wxString LatexFilename,
                         const wxString original_FITS_fname,
                         const wxString processed_FITS_fname,
                         wxString &error_message)
{
double mrho = 0., mtheta = 0., drho = 0., dtheta = 0., delta_mag = 0., d_delta_mag = 0.;
FILE *fp_data, *fp_tex;
int n_astrom, n_photom, status = 0;
char latex_fname[128], filename[128];

/* DEBUG
printf("speckle_process_data/original_fits_fname=%s\n",
        (const char *)original_FITS_fname.mb_str());
*/

fp_data = NULL;
fp_tex = NULL;
/* Open LaTeX file:
*   a+ : append or create a text file for read/write
*/
 strcpy(latex_fname, LatexFilename.mb_str());
 if((fp_tex = fopen(latex_fname,"a+")) == NULL) {
   error_message = wxT("speckle_process_data/Error opening LaTeX file >")
                   + LatexFilename + wxT("<\n");
   return(-1);
   }

/* Open input data file: */
 strcpy(filename, data_fname.mb_str());
if((fp_data = fopen(filename,"r")) == NULL) {
   error_message = wxT("speckle_process_data/Error opening data file >")
                   + data_fname + wxT("<\n");
  fclose(fp_tex);
  fp_tex = NULL;
  return(-1);
  }

/* Compute the mean assuming new format: */
  speckle_astrom_mean(fp_data, &mrho, &drho, &mtheta, &dtheta,
                               &n_astrom);
/* rewind file: (close and re-open it) */
  rewind(fp_data);

  speckle_photom_mean(fp_data, &delta_mag, &d_delta_mag, &n_photom);
  if(n_astrom != 0 || n_photom != 0) {
    status = speckle_output_mean(fp_tex, fp_data, original_FITS_fname,
                                 processed_FITS_fname, mrho, drho, mtheta,
                                 dtheta, delta_mag, d_delta_mag, n_astrom,
                                 n_photom, data_fname);
    }
  else {
   fprintf(stderr,"speckle_process_data/Error: no data in file (or badly formatted file) \n");
   error_message = wxT("speckle_process_data/Error: no data in file (or badly formatted file) \n");
   }

if(fp_data != NULL) fclose(fp_data);
if(fp_tex != NULL) fclose(fp_tex);

return(status);
}
/***************************************************************************
* speckle_astrom_mean
* To compute the mean of rho and theta values
* New format of Xdisp1.log (after 2005)
*
* INPUT:
*   fp_data: pointer to data file
*
* OUTPUT:
*   mrho, mtheta: mean values for rho and theta
*   drho, dtheta: corresponding standard deviations
*   nn: number of measurements
***************************************************************************/
static int speckle_astrom_mean(FILE *fp_data, double *mrho, double *drho,
                               double *mtheta, double *dtheta, int *nn)
{
double rho, theta;
double rho_sum, rho_sumsq, theta_sum, theta_sumsq;
double theta0, sig0;
char buffer[80];
int nvalues, status, isign, isign_is_set;

rho_sum = 0.;
rho_sumsq = 0.;
theta_sum = 0.;
theta_sumsq = 0.;
*nn = 0;

/* Speckle or DVA:
%% rho=13.76 theta=-78.26 xc=61.20 yc=77.47 (61.3,78.7,7.7,3) Gauss
*/
/* Lucky imaging:
%% rho=1.04 theta=-34.45 theta180=145.55 Dm=1.12
*/
isign_is_set = 0;
isign = 1;
// JLP2022: now theta meas. in [-180,+180] and corrected for quadrant by atan2
while(fgets(buffer,80,fp_data) != 0) {
   nvalues = sscanf(buffer,"%%%% rho=%lf theta=%lf",&rho, &theta);
   if(nvalues == 2) {
     if(rho > 0.01) {
        rho_sum += rho;
        rho_sumsq += rho*rho;
// theta is in [-180,+180]
//        printf("ZZZZ: isign_is_set=%d input: theta=%.2f\n", isign_is_set, theta);
        if (theta > 90.) theta -=180.;
        if (theta < -90.) theta +=180.;
// JLP2022: put theta in [-90,90]:
        if(isign_is_set == 0) {
          if(theta > 0.) isign = 1;
            else isign = -1;
          isign_is_set = 1;
          }
//        printf("ZZZZ: isign_is_set=%d isign=%d, output: theta=%.2f\n", isign_is_set, isign, theta);
        if((isign > 0) && (theta < -10.)) theta += 180.;
        if((isign < 0) && (theta > 10.)) theta -= 180.;
//        printf("ZZZZ: output2: theta=%.2f\n", theta);
        theta_sum += theta;
        theta_sumsq += theta*theta;
        (*nn)++;
     } // if rho > 0.01
   } // if nvalues == 2
} // EOF while

if(*nn >= 1) {

  theta0 = theta_sum / (double)(*nn);
  if(*nn > 2) {
    sig0 = theta_sumsq / (double)(*nn) - SQUARE(theta0);
    sig0 = sqrt((double)sig0);
  } else {
    sig0 = 0.;
  }

  *mtheta = theta0;
  *dtheta = sig0;

  *mrho = rho_sum / (double)(*nn);
  if(*nn > 2) {
    *drho = rho_sumsq / (double)(*nn) - SQUARE(*mrho);
    *drho = sqrt((double)*drho);
  } else {
    *drho = 0.;
  }
  status = 0;
} else { // EOF *nn >= 1
  status = -1;
}

// JLP2022: put mtheta in [-90,90]:
if (*mtheta < -90.) *mtheta +=180.;
if (*mtheta > 90.) *mtheta -=180.;

return(status);
}
/***************************************************************************
* speckle_photom_mean
* To compute the mean of delta_mag values
*
* INPUT:
*   fp_data: pointer to data file
*
* OUTPUT:
*   delta_mag: mean value of delta_mag
*   d_delta_mag: corresponding standard deviation
*   nn: number of measurements
***************************************************************************/
static int speckle_photom_mean(FILE *fp_data, double *delta_mag,
                               double *d_delta_mag, int *nn)
{
double dm;
double dm_sum, dm_sumsq;
char buffer[80], *pc;
int status;

dm_sum = 0.;
dm_sumsq = 0.;
*nn = 0;

/* Lucky imaging:
%% rho=1.04 theta=-34.45 theta180=145.55 Dm=1.12
*/
/* DVA:
%% Dm=0.00 (Bary.) Dm=0.20 (Gauss.)
*/
while(fgets(buffer,80,fp_data) != 0) {
/*
    printf(" line=>%s<\n", buffer);
*/
   if(!strncmp(buffer,"%%%%", 2)) {
    pc = buffer;
    while(*pc && *pc != 'D') pc++;
    if(*pc == 'D') {
      if(!strncmp(pc,"Dm=", 3)) {
      sscanf(pc,"Dm=%lf", &dm);
       dm_sum += dm;
       dm_sumsq += dm * dm;
       (*nn)++;
      }
    } // EOF pc = D
   } // EOF if stnrncmp buffer
} // EOF while fget

if(*nn >= 1) {
  *delta_mag = dm_sum / (double)(*nn);
if(*nn > 2) {
  *d_delta_mag = dm_sumsq / (double)(*nn) - SQUARE(*delta_mag);
} else {
  *d_delta_mag = 0.;
}
  status = 0;
  } else {
  status = -1;
  }

return(status);
}
/***************************************************************************
* Print the results in fp_tex file
*
* INPUT:
*   fp_tex: pointer to LaTeX file
*   original_FITS_fname: full name of the FITS file containing
*                        keyword information about the processed file
*                        that was used for the measurements
*   processed_FITS_fname: short version of the processed_fits_name
*                         that was used for the measurements
*                         without directory and extension
*   mrho, mtheta: mean values for rho and theta
*   drho, dtheta: corresponding standard deviations
*   nn: number of measurements
**************************************************************************/
static int speckle_output_mean(FILE *fp_tex, FILE *fp_data,
                               const wxString original_FITS_fname,
                               const wxString processed_FITS_fname, double mrho,
                               double drho, double mtheta, double dtheta,
                               double delta_mag, double d_delta_mag,
                               int n_astrom, int n_photom,
                               const wxString data_fname)
{
double epoch_bessel, epoch_julian, year;
int eyepiece, orig_fits_file_is_avail, xbin = 1, ybin = 1, status;
char processed_fits_fname[128], original_fits_fname[128], date[60], filter[20];
char buffer[180], object_name[20], upper_object_name[40], comments[80];
char epoch_string[32], bin_string[20], latex_fits_fname[128];
char dmag_string[30];

strcpy(original_fits_fname, original_FITS_fname.mb_str());
strcpy(processed_fits_fname, processed_FITS_fname.mb_str());
orig_fits_file_is_avail = jlp1_rdfits_is_ok(original_fits_fname);

eyepiece = 0;
year = 0.;
strcpy(date, "");
strcpy(filter, "");
strcpy(object_name, "");
// Defaut value for object name: 
strncpy(upper_object_name, processed_fits_fname,40);
// Filter for printable characters:
jlp_cleanup_string(upper_object_name, 40);

/**************************************************************************/
if(orig_fits_file_is_avail){
  status = decode_info_from_FITS_file(original_fits_fname, date, filter,
                                      object_name, &epoch_bessel, &year, 
                                      &xbin, &ybin, comments);
/**************************************************************************/
  if(status) {
    fprintf(stderr, "decode_info_from_FITS_file/Error, status = %d\n", status);
  } else {

// Epoch as a fraction of Besselian year
  if(epoch_bessel >= year) {
    JLP_besselian_to_julian_epoch(epoch_bessel, &epoch_julian);
    sprintf(epoch_string, "EP=%.4f EJUL=%.4f", epoch_bessel, epoch_julian);
   } else {
    epoch_string[0] = '\0';
   }

  if(xbin > 1 || ybin > 1)
     sprintf(bin_string, "XYBIN=%d,%d", xbin, ybin);
   else
     bin_string[0] = '\0';

//******************************************
// For PISCO in Merate files start with a digit (the date)
// example: 220214_ads2390_Rd_8_a.fits
   if(isdigit(processed_fits_fname[0]) && isdigit(processed_fits_fname[1])
    && (processed_fits_fname[6] == '_'))  {
/* Decode comments and extract information: */
// object_name is generally not valid in FITS file...
      decode_merate_comments(comments, processed_fits_fname, date, filter,
                             object_name, &eyepiece, &year, latex_fits_fname);

      decode_merate_object_name(object_name, upper_object_name);
//******************************************
// For PISCO2 in Nice files do not start with a digit:
   } else {
      eyepiece = xbin;
      decode_Nice_object_fits_fname(processed_fits_fname, latex_fits_fname,
                                    upper_object_name);
   } // EOF gili syntax
  } // EOF merate syntax
#ifdef DEBUG
  printf("%%%% mean: rho = %.2f \\pm %.2f  theta = %.2f \\pm %.2f (n=%d)\n",
        mrho, drho, mtheta, dtheta, n_astrom);
  printf("& %s & %s & %s & %d & %.2f & %.2f & %.2f & %.1f & %s %s\\\\\n",
        processed_fits_fname, date, filter, eyepiece, mrho, drho, mtheta, dtheta,
        epoch_string, bin_string);
#endif

// EOF if orig_file_is_avail...
} else {
 strcpy(latex_fits_fname, processed_fits_fname);
 strcpy(date, "");
 strcpy(filter, "");
 eyepiece = 0;
 strcpy(epoch_string, "");
 strcpy(bin_string, "");
}

/* Convert name to upper case:  ads2345Aa => ADS 2345 Aa
*/
/************ Example:
\hline % ---------------------------------------------------------------
& ADS 6538 & 2004. & & & & & & & \\
*/
  fprintf(fp_tex,"\\hline %% -------------------------------------------------------------\n");
  fprintf(fp_tex,"& %s & %.0f & & & & & & & \\\\\n", upper_object_name, year);
#ifdef DEBUG
  printf("& %s & %.0f & & & & & & & \\\\\n", upper_object_name, year);
#endif


/* Go to beginning of input data file: */
fseek(fp_data, 0L, 0);

/* Copying the content of the data file: */
 while(!feof(fp_data)){
    if(fgets(buffer,170,fp_data) == NULL) break;
#ifdef DEBUG_
    printf(" buffer = >%s<\n", buffer);
#endif
    if(buffer[0] == '%') fputs(buffer,fp_tex);
    }

/* Copying the results: */
  if((drho < 0.1) && (dtheta < 0.3)) {
      fprintf(fp_tex,
              "%%%% mean: rho = %.2f \\pm %.2f (too small->0.1) theta = %.2f \\pm %.2f (too small->0.3) (n=%d)\n",
              mrho, drho, mtheta, dtheta, n_astrom);
      drho = 0.1;
      dtheta = 0.3;
   }
  else if(drho < 0.1) {
      fprintf(fp_tex,
              "%%%% mean: rho = %.2f \\pm %.2f (too small->0.1) theta = %.2f \\pm %.2f (n=%d)\n",
              mrho, drho, mtheta, dtheta, n_astrom);
      drho = 0.1;
   }
  else if(dtheta < 0.3) {
      fprintf(fp_tex,
              "%%%% mean: rho = %.2f \\pm %.2f theta = %.2f \\pm %.2f (too small->0.3) (n=%d)\n",
              mrho, drho, mtheta, dtheta, n_astrom);
      dtheta = 0.3;
   }
  else {
      fprintf(fp_tex,
              "%%%% mean: rho = %.2f \\pm %.2f  theta = %.2f \\pm %.2f (n=%d)\n",
              mrho, drho, mtheta, dtheta, n_astrom);
   }

// Photometric results:
if(n_photom >= 1) {
   if(d_delta_mag < 0.02) {
      fprintf(fp_tex, "%%%% mean: Dmag = %.2f \\pm %.2f (too small->0.02) (n=%d)\n",
              delta_mag, d_delta_mag, n_photom);
      d_delta_mag = 0.02;
   } else {
      fprintf(fp_tex, "%%%% mean: Dmag = %.2f \\pm %.2f (n=%d)\n",
              delta_mag, d_delta_mag, n_photom);
   }
   sprintf(dmag_string, "Dm=%.2f+/-%.2f", delta_mag, d_delta_mag);
} else {
   dmag_string[0] = '\0';
}

/* Merate output/Example:
& 220406\_ads8630\_Vd & 22/04/2006 & V & 10 & 21.61 & 0.10 & 22.94 & 0.3 &
 EP=2006.2351 \\
* Nice output/Example:
& stf554 & 22/04/2006 &   & 1 & 21.61 & 0.10 & 22.94 & 0.3 & EP=2006.2351 \\
 */
  fprintf(fp_tex, "& %s & %s & %s & %d & %.2f & %.2f & %.2f & %.1f & %s %s %s\\\\\n",
          latex_fits_fname, date, filter, eyepiece, mrho, drho, mtheta, dtheta,
          epoch_string, bin_string, dmag_string);
  printf("& %s & %s & %s & %d & %.2f & %.2f & %.2f & %.1f & %s %s %s\\\\\n",
         latex_fits_fname, date, filter, eyepiece, mrho, drho, mtheta, dtheta,
         epoch_string, bin_string, dmag_string);

return(0);
}
/************************************************************************
* Decode information contained in fits file
* for PISCO and PISCO2
*
* (derived from:
* int get_bessel_epoch_from_fits_file(char *fits_filename, char *full_directory,
*                                     double *epoch0, char *date0, 
*                                     int *epoch_was_found)
* in jlp0_fits_utils.cpp)
*
* INPUT:
* original_fits_fname: name of original FITS file
*
* OUTPUT:
* epoch_bessel: Besselian epoch of observation as a fraction of year (e.g. 2006.2543)
************************************************************************/
int decode_info_from_FITS_file(const char *original_fits_fname, char *date, 
                               char *filter, char *object_name, 
                               double *epoch_bessel, 
                               double *year, int *xbin, int *ybin, 
                               char *comments)
{
char filename1[128], jlp_descr[1024];
char d_object[80], d_descrip[80], d_date[80], d_obs_date[80], d_counters[80];
char andor1[80], mykey[20];
double time0;
/*** JLP 2018
double *array;
****/
INT4 nx, ny, dflag, istatus;
INT_PNTR pntr_array;
int i, status, ivalue, found;

*epoch_bessel = 0.;
*year = 0.;
*date = '\0';

/* Get information from original FITS file: */
/* Descriptors are wanted */
dflag = 1;
strcpy(filename1, original_fits_fname);

JLP_VM_RDFITS(&pntr_array, &nx, &ny, filename1, comments,
              jlp_descr, &dflag, &istatus);
if(istatus != 0) {
 fprintf(stderr,"decode_info_from_FITS_file/Error reading file %s \n",
         filename1);
 return(-1);
 }

/* Free memory: */
/***** ERROR here (JLP 2018)
 array = (double *)pntr_array;
 free(array);
*****/

/* PISCO2 in NICE:
OBJECT  = '                f200' //
ANDOR0  = 'Head model:             DV897_BV' //
ANDOR1  = 'xbin=1 ybin=1 xc=788, yc=256, rot=0 deg, mirror=0' //
ANDOR2  = 'VS=2 (0.90) HS=1 (5.00) VAmp=1 Preamp=0 AD=0' //
ANDOR3  = 'Exp=20 ms (EM: mode=1 G=4095, Cooler on: T=-70 degC)' //
ANDOR4  = 'FrameTransfer=0 AcqMode=5' //
DESCRIP = 'Long int. n=1050 Thresh=0 Offset=0 FField=0' //
MAXVALSTAT= 'MaxVal=15738.03+/-706.67 Min=13055.00 Max=16383.00 (n=1050, ' //
FWHMSTAT= 'FWHM=0.89+/-0.06 Min=0.76 Max=1.10 (n=1050, rate=100%)' //
DATE-OBS= '1996-10-14T10:14:36.123' / Date and time of start of obs. in UTC.
DATE    = 'Mon Jan 24 20:38:55 2011
*/

*xbin = 1;
*ybin = 1;

/* Copy all descriptors
* jlp_descr:
 OBJECT, DESCRIP, COUNTERS, ANDOR0, ANDOR1, ANDOR2, ANDOR3, ANDOR4, HISTORY
*/
 *d_counters = '\0';
 *d_date = '\0';
 *d_obs_date = '\0';
 *d_object = '\0';
 *d_descrip = '\0';
 for(i = 0; (i < 1024) && (jlp_descr[i] != '\0') ; i+=80) {
   if(!strncmp(&jlp_descr[i], "COUNTERS", 8)) {
     strcpy(d_counters, &jlp_descr[i+9]);
/* Warning: OBJECT item correspond to "comments" */
   } else if(!strncmp(&jlp_descr[i], "OBJECT", 6)) {
     strcpy(d_object, &jlp_descr[i+7]);
   } else if(!strncmp(&jlp_descr[i], "DESCRIP", 7)) {
     strcpy(d_descrip, &jlp_descr[i+8]);
   } else if(!strncmp(&jlp_descr[i], "DATE-OBS", 8)) {
     strcpy(d_obs_date, &jlp_descr[i+9]);
   } else if(!strncmp(&jlp_descr[i], "DATE", 4)) {
     strcpy(d_date, &jlp_descr[i+5]);
   } else if(!strncmp(&jlp_descr[i], "ANDOR1", 6)) {
     strcpy(andor1, &jlp_descr[i+7]);
     strcpy(mykey, "xbin=");
     read_int_from_cstring(andor1, mykey, 5, &ivalue, &found);
     if(found) *xbin = ivalue;
     strcpy(mykey, "ybin=");
     read_int_from_cstring(andor1, mykey, 5, &ivalue, &found);
     if(found) *ybin = ivalue;
   }
}

/* MERATE Example:
OBJECT= ads684ab - Rd - 20mm - 15ms - guad.norm. - sm - soglia 8
DESCRIP= Autoc - interc n=19050 Preproc=2 Thresh=8 Dark=1 FField=1
DATE= Mon Jan 12 18:35:26 2009
COUNTERS= Start=18:25:43.0 End=18:33:34.0
*/
/*
printf("decode_info_from_FITS_file\n\
object=%s\ndescrip=%s\ndate=%s\ncounters=%s\n xbin=%d ybin=%d\n",
        d_object, d_descrip, d_date, d_counters, *xbin, *ybin);
*/

/* In "jlp_fits/jlp0_fits_utils.cpp"
*/
    if(d_obs_date[0] != '\0') {
// Bessel epoch from obs_date
     status = descrip_bepoch_from_obs_date(d_obs_date, date, year,
                                     &time0, epoch_bessel);
    } else {
// Bessel epoch from date
     status = descrip_bepoch_from_date(d_date, d_counters, date, year,
                                 &time0, epoch_bessel);
    }

/* DEBUG:
printf("descrip_decode_date: d_date=%s date=%s year=%.1f epoch_bessel=%.4f\n",
        d_date, date, *year, *epoch_bessel);
*/

return(0);
}
/****************** Decoding comments ****************************
* For PISCO in Merate, use all available information
* 1. filenames contain information: 220214_ads2390_Rd_8_a.fits
* 2. comments contain additionnal information
*
* Example of comments:
*  ads8630 - Vd - 10mm - 4ms - ND 0.4 - guad.norm. - soglia 8
*  ads2930 - Vd - 10mm - 4ms - g.n. - Q=4
*
* INPUT:
* processed_fits_fname: short version of the processed fits filename
*                       (without directory and extension)
*
* OUTPUT:
* date, filter, object_name, eyepiece, year, and
* latex_fits_fname: version of filename compatible with Latex
*                   and without ".fits"    (e.g. 010209\_ads342\_Rd\_a)
******************************************************************/
static int decode_merate_comments(char *comments,
                                  const char *processed_fits_fname,
                                  char *date, char *filter, char *object_name,
                                  int *eyepiece, double *year,
                                  char *latex_fits_fname)
{
char *pc, filename1[128];
int i, k, iyy;

//***************** Decoding the comments: ****************************
*eyepiece = 0;
pc = comments;
while(*pc) {
  if(!strncmp(pc, "10mm", 4)){*eyepiece = 10; break;}
  else if(!strncmp(pc, "15mm", 4)){*eyepiece = 15; break;}
  else if(!strncmp(pc, "20mm", 4)){*eyepiece = 20; break;}
  else if(!strncmp(pc, "24mm", 4)){*eyepiece = 24; break;}
  else if(!strncmp(pc, "32mm", 4)){*eyepiece = 32; break;}
  pc++;
  }

/****************** Decoding the FITS file name ****************************
* Example: 100105_ads8630_Vd
*/
strcpy(filename1, processed_fits_fname);

pc = filename1;
/* First part is the date */
k = 0;
i = 0;
while(*pc && *pc != '_') {
  date[i++] = *pc;
  if(i == 2) date[i++] = '/';
  if(i == 5) {date[i++] = '/'; date[i++] = '2'; date[i++] = '0';}
  latex_fits_fname[k++] = *(pc++);
  }
date[i] = '\0';

/**** Date is like 02/06/2006 ***/
/* So I can also extract the year: */
sscanf(date,"%*d/%*d/%d",&iyy);
*year = (double)iyy;

/* Change "_" to "\_" for LateX : */
if(*pc == '_') {
  latex_fits_fname[k++] = '\\';
  latex_fits_fname[k++] = '_';
  pc++;
  }

/* Second part is the object name */
i = 0;
while(*pc && *pc != '_') {
  object_name[i++] = *pc;
  latex_fits_fname[k++] = *(pc++);
  }
object_name[i] = '\0';

/* Change "_" to "\_" for LateX : */
if(*pc == '_') {
  latex_fits_fname[k++] = '\\';
  latex_fits_fname[k++] = '_';
  pc++;
  }

/* Third part is the filter: Rd, Rr, sfr, sfd, etc
 * (d for direct, r for recorded)
 */
i = 0;
while(*pc && *pc != '_') {
  if(*pc != 'r' && *pc != 'd') filter[i++] = *pc;
  latex_fits_fname[k++] = *pc;
  pc++;
  }
filter[i] = '\0';

/* Copy to the end: */
while(*pc) {
  while(*pc && *pc != '_') latex_fits_fname[k++] = *(pc++);
  if(*pc == '_') {
    latex_fits_fname[k++] = '\\';
    latex_fits_fname[k++] = '_';
    pc++;
    }
  }
  latex_fits_fname[k] = '\0';

return(0);
}
/************************************************************************
* Decode the object name that was derived from the FITS filename
* and convert it to upper case:  ads2345Aa => ADS 2345 Aa
*
* INPUT:
* object_name that was extracted from the FITS filename
*
* OUTPUT:
* upper_case_object_name: upper case version of object name
************************************************************************/
static int decode_merate_object_name(const char *object_name,
                                     char *upper_case_object_name)
{
int i, prefix_is_ended;
char *pc, object_name1[128];

// Convert name to upper case:  ads2345Aa => ADS 2345 Aa
strcpy(object_name1, object_name);
pc = object_name1;
i = 0;
prefix_is_ended = 0;
while(*pc) {
   if(!prefix_is_ended)
      upper_case_object_name[i++] = toupper(*pc);
   else
      upper_case_object_name[i++] = *pc;

/* Add a blank to separate letters and numbers:
*/
   if( isalpha(*pc) && isdigit(*(pc+1))) {
       upper_case_object_name[i++] = ' ';
       prefix_is_ended = 1;
       }
   else if( isalpha(*(pc+1)) &&  isdigit(*(pc))) {
       upper_case_object_name[i++] = ' ';
       prefix_is_ended = 1;
       }

   pc++;
    }
upper_case_object_name[i++] = '\0';

return(0);
}
/************************************************************************
* Decode object name from fits filename (ANDOR detector with PISCO2 in Nice)
* Convert name to upper case:  stf554Aa_a.fits => STF554Aa
*
* INPUT:
* processed_fits_fname: name of the processed fits file
*                       used for the measurements
*
* OUTPUT:
* upper_case_object_name: upper case version of object name
************************************************************************/
static int decode_Nice_object_fits_fname(const char *processed_fits_fname,
                                         char *latex_fits_fname,
                                         char *upper_case_object_name)
{
int i, k, prefix_is_ended;
char *pc, object_name1[128], filename1[256];

// Convert name to upper case:  stf554Aa => ADS 2345 Aa
strcpy(object_name1, processed_fits_fname);
pc = object_name1;
i = 0;
prefix_is_ended = 0;
while(*pc && *pc != '_') {
   if(!prefix_is_ended)
      upper_case_object_name[i++] = toupper(*pc);
   else
      upper_case_object_name[i++] = *pc;

/* Don't add a blank to separate letters and numbers:
*/
   if( isalpha(*pc) && isdigit(*(pc+1))) {
       prefix_is_ended = 1;
       }
   else if( isalpha(*(pc+1)) &&  isdigit(*(pc))) {
       prefix_is_ended = 1;
       }

   pc++;
    }
upper_case_object_name[i++] = '\0';

if(*pc == '_') sprintf(&(object_name1[i]),"\\_%s", &(processed_fits_fname[i+1]));

strcpy(filename1, processed_fits_fname);

pc = filename1;

/* Copy to the end: */
k = 0;
while(*pc) {
  while(*pc && *pc != '_') latex_fits_fname[k++] = *(pc++);
  if(*pc == '_') {
    latex_fits_fname[k++] = '\\';
    latex_fits_fname[k++] = '_';
    pc++;
    }
  }
latex_fits_fname[k] = '\0';

return(0);
}
/*****************************************************************************
*
*****************************************************************************/
int read_int_from_cstring(char *cstring, char *mykey, int mykey_length,
                          int *ivalue, int *found)
{
char *pc;

*ivalue = 0;
*found = 0;
pc = cstring;
while(*pc) {
  if(!strncmp(pc, mykey, mykey_length)) {
    pc += mykey_length;
    sscanf(pc, "%d", ivalue);
    *found = 1; break;
    }
  pc++;
  }

return(0);
}
