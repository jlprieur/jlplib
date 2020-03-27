/*************************************************************************
* jlp0_fits_utils.cpp
* Set of routines using information in the header of FITS files
*
* JLP
* Version 06/09/2018
*************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include "jlp_fitsio.h"
#include "jlp_string.h"

/*
#define DEBUG
*/

/* Contained in this file: 
int descrip_decode_date(char *d_date, char *d_counters, char *date0,
                        double *year0, double *time0, double *epoch0);
int JLP_compute_besselian_epoch(char *date0, const double time0, 
                                double *bepoch0);
int JLP_julian_day(double aa, int mm, int idd, double time, double *djul);
int JLP_besselian_epoch(double aa, int mm, int idd, double time, double *b_date);
*/
/*************************************************************************
* get_bessel_epoch_from_fits_file 
* suited to PISCO in Merate
* and called by src/orbits/jlp_astrom2_utils.c
*
* Warning: an equivalent for PISCO/PISCO2 is decode_info_from_FITS_file 
* contained in "jlp_wxplot/jlp_wxspeckle1_dlg_utils.cpp"
*
* INPUT:
* fits_filename: filename
* full_directory: directory name
*
* OUTPUT:
* epoch0: Bessel epoch as a fractional year (e.g., 2009.3425)
* date0: (e.g., 26/05/2010)
*************************************************************************/
int get_bessel_epoch_from_fits_file(char *fits_filename, char *full_directory, 
                                    double *epoch0, char *date0, 
                                    int *epoch_was_found)
{
double time0, year0;
float *array;
int nx, ny, dflag, istatus;
INT_PNTR pntr_array;
char filename[200], comments[80], jlp_descr[1024];
char d_object[80], d_descrip[80], d_date[80], d_obs_date[80], d_counters[80];
int i, status;

#ifdef DEBUG
printf("get_bessel_epoch_from_fits_file/infile=%s\n", fits_filename);
#endif

*epoch_was_found = 0;
sprintf(filename, "%s%s", full_directory, fits_filename);

/* Read input file
* Descriptors are wanted */
dflag = 1;
JLP_VM_RDFITS(&pntr_array, &nx, &ny, filename, comments,
              jlp_descr, &dflag, &istatus);

if(istatus) {
 fprintf(stderr, "get_bessel_epoch_from_fits_file/Fatal error opening %s\n", 
	  filename);
 exit(-1);
 }

/* Free memory: */
 array = (float *)pntr_array;
 free(array);

/* Copy all descriptors 
*/
 *d_counters = '\0';
 *d_date = '\0';
 *d_obs_date = '\0';
 *d_object = '\0';
 *d_descrip = '\0';
 for(i = 0; (i < 1024) && (jlp_descr[i] != '\0') ; i+=80) {
/* DEBUG: */
#ifdef DEBUG
   printf("%s\n", &jlp_descr[i]);
#endif
   if(!strncmp(&jlp_descr[i], "COUNTERS", 8)) {
     strcpy(d_counters, &jlp_descr[i+9]);
   } else if(!strncmp(&jlp_descr[i], "OBJECT", 6)) {
     strcpy(d_object, &jlp_descr[i+7]);
   } else if(!strncmp(&jlp_descr[i], "DESCRIP", 7)) {
     strcpy(d_descrip, &jlp_descr[i+8]);
   } else if(!strncmp(&jlp_descr[i], "DATE-OBS", 8)) {
     strcpy(d_obs_date, &jlp_descr[i+9]);
   } else if(!strncmp(&jlp_descr[i], "DATE", 4)) {
     strcpy(d_date, &jlp_descr[i+5]);
   }
}
/* Example:
OBJECT= ads684ab - Rd - 20mm - 15ms - guad.norm. - sm - soglia 8
DESCRIP= Autoc - interc n=19050 Preproc=2 Thresh=8 Dark=1 FField=1
DATE= Mon Jan 12 18:35:26 2009
DATE-OBS= '1996-10-14T10:14:36.123' / Date and time of start of obs. in UTC.
COUNTERS= Start=18:25:43.0 End=18:33:34.0
*/
#ifdef DEBUG
printf("object=%s\ndescrip=%s\ndate=%s\ndate-obs=%s\ncounters=%s\n", 
        d_object, d_descrip, d_date, d_obs_date, d_counters);
#endif

    if(d_obs_date[0] != '\0') {
    status = descrip_bepoch_from_obs_date(d_obs_date, date0, &year0, 
                                     &time0, epoch0);
    } else {
    status = descrip_bepoch_from_date(d_date, d_counters, date0, &year0, 
                                 &time0, epoch0);
    }
    if(!status) *epoch_was_found = 1;

return(status);
}
/********************************************************************
* New version of date of observation (after september 2012)
* Example:
* DATE-OBS= 1996-10-14T10:14:36.123
*
********************************************************************/
int descrip_bepoch_from_obs_date(char *d_date, char *date0,
                            double *year0, double *time0, double *epoch0)
{
int n_ddate, status;
int nval, dd, mm, iyy, h1, m1, sec1, istart; 
double tt1;

*time0 = 0.;
*date0 = '\0';
*year0 = 0.;
mm = 0;
dd = 0;
*epoch0 = 0.;
status = 0;

n_ddate = strlen(d_date);
if(n_ddate < 20) {
  fprintf(stderr, "descrip_decode_obs_date/Error: length of d_obs_date: %d\n", 
          n_ddate);
  fprintf(stderr, " date=>%s<\n", d_date);
  return(-1);
}

/* Decode date of observation:
* DATE-OBS= '1996-10-14T10:14:36.123'
* or:
* DATE-OBS= 1996-10-14T10:14:36.123
* or 1996-10-14T10:14:36.123
*/
if(d_date[0] == 'D' && d_date[1] == 'A' ) istart = 9;
  else istart = 0;
if(d_date[istart] == ' ') istart++;
if(d_date[istart] == '\'') istart++;

// Year:
nval = sscanf(&d_date[istart], "%4d", &iyy); 
if(nval == 1) *year0 = (double)iyy; 
 else status = -1;

if(status == 0) {
istart += 4;
if(d_date[istart] != '-') {
  status = -1;
 } else {
 istart++;
// Month:
 sscanf(&d_date[istart], "%02d", &mm); 
 istart += 2;
 if(d_date[istart] != '-') {
  status = -1;
 } else {
 istart++;
// Day:
 sscanf(&d_date[istart], "%02d", &dd); 
 istart += 2;
 if(d_date[istart] == 'T') {
   istart++;
// Time:
   nval = sscanf(&d_date[12],"%02d:%02d:%02d",
                 &h1, &m1, &sec1);
/* Compute the time from DATE: */
   tt1 = (double)h1 + ((double)m1) / 60. + (double)sec1 / 3600.;  
   *time0 = tt1;
  } /* EOF Time */
  } /* EOF day */
  } /* EOF month */
  } /* EOF year (status == 0)*/

if(status == -1) {
  fprintf(stderr, "descrip_decode_obs_date/Error decoding date_obs = >%s<\n", 
           d_date);
  mm = 0;
 } else {
   sprintf(date0, "%02d/%02d/%4d", dd, mm, iyy);
   status = JLP_compute_besselian_epoch(date0, *time0, epoch0);
 }

return(status);
}
/********************************************************************
* Old version (before september 2012)
* Example:
* DATE= Mon Jan 12 18:35:26 2009
* COUNTERS= Start=18:25:43.0 End=18:33:34.0
*
********************************************************************/
int descrip_bepoch_from_date(char *d_date, char *d_counters, char *date0,
                             double *year0, double *time0, double *epoch0)
{
int n_ddate, n_dcounters, status;
int nval, dd, mm, iyy, h1, m1, sec1, h2, m2; 
double s1, s2, tt1, tt2;
char month[4];

*time0 = 0.;
*date0 = '\0';
*year0 = 0.;
*epoch0 = 0.;
status = 0;

n_ddate = strlen(d_date);
if(n_ddate < 20) {
  fprintf(stderr, "descrip_decode_date/Error: length of d_date: %d\n", 
          n_ddate);
  fprintf(stderr, " date=>%s<\n", d_date);
  status = -1;
  } else {

/* Decode date:
DATE= Mon Jan 24 20:38:55 2011
" Mon Jan 24 20:38:55 2011"
*/

// Month:
strncpy(month, &d_date[5], 3);
month[3] = '\0';
// Day:
sscanf(&d_date[9], "%02d", &dd); 
// Year:
sscanf(&d_date[21], "%4d", &iyy); 
*year0 = (double)iyy; 
// Time:
nval = sscanf(&d_date[12],"%02d:%02d:%02d",
              &h1, &m1, &sec1);
/* Compute the time from DATE: */
tt1 = (double)h1 + ((double)m1) / 60. + (double)sec1 / 3600.;  
*time0 = tt1;

if(!strncmp(month, "Jan", 3)) {
  mm = 1;
 } else if (!strncmp(month, "Feb", 3)) {
  mm = 2;
 } else if (!strncmp(month, "Mar", 3)) {
  mm = 3;
 } else if (!strncmp(month, "Apr", 3)) {
  mm = 4;
 } else if (!strncmp(month, "May", 3)) {
  mm = 5;
 } else if (!strncmp(month, "Jun", 3)) {
  mm = 6;
 } else if (!strncmp(month, "Jul", 3)) {
  mm = 7;
 } else if (!strncmp(month, "Aug", 3)) {
  mm = 8;
 } else if (!strncmp(month, "Sep", 3)) {
  mm = 9;
 } else if (!strncmp(month, "Oct", 3)) {
  mm = 10;
 } else if (!strncmp(month, "Nov", 3)) {
  mm = 11;
 } else if (!strncmp(month, "Dec", 3)) {
  mm = 12;
 } else {
   fprintf(stderr, "descrip_decode_date/Error decoding date = >%s< (month = %s)\n", d_date, month);
  mm = 0;
 }

sprintf(date0, "%02d/%02d/%4d", dd, mm, iyy);
}

/* Decode time from counters (if present)
* COUNTERS= Start=18:25:43.0 End=18:33:34.0
*/
n_dcounters = strlen(d_counters);
if(n_dcounters >= 25) {
nval = sscanf(d_counters," Start=%02d:%02d:%lf End=%02d:%02d:%lf",
              &h1, &m1, &s1, &h2, &m2, &s2);
if(nval != 6) {
  fprintf(stderr, "descrip_decode_date/Error decoding time in counters= >%s<\n", 
          d_counters);
  return(-1);
 }
/* Compute the mean time from counters: */
tt1 = (double)h1 + ((double)m1) / 60. + s1 / 3600.;  
tt2 = (double)h2 + ((double)m2) / 60. + s2 / 3600.;  
*time0 = (tt1 + tt2) / 2.;
}

status = JLP_compute_besselian_epoch(date0, *time0, epoch0);

return(status);
}
/**************************************************************************
* Compute Besselian epoch from date and time
*
* INPUT:
*  date0: (input format: dd/mm/yy, e.g. 12/2/2004 or 01/12/1998 or 31/06/2002)
*  time0: (e.g., 23.3452 in hours)
*
* OUTPUT: 
*  b_epoch0: Besselian epoch, as a fraction of year, e.g. 2004.234
*
**************************************************************************/
int JLP_compute_besselian_epoch(char *date0, const double time0, 
		                double *b_epoch0) 
{
int nval, dd, mm, iyy, status;
double yy;
char date1[80];

*b_epoch0 = 0.;

/* Decode the input date: */
strcpy(date1, date0);
jlp_compact_string(date1, 80);
   nval = sscanf(date1, "%d/%d/%d", &dd, &mm, &iyy);
/* DEBUG:
printf("JLP_compute_besselian__epoch/date=>%s< dd=%d mm=%d iyy=%d nval=%d\n", 
        date0, dd, mm, iyy, nval);
*/
   if(nval != 3) {
    fprintf(stderr, "JLP_compute_ibesslian_epoch/error decoding input date = >%s< \n", 
            date0);
    return(-1);
    }

yy = (double)iyy;
status = JLP_besselian_epoch(yy, mm, dd, time0, b_epoch0);

/* DEBUG:
printf("JLP_compute_besselian_epoch/epoch=%f\n", *b_epoch0); 
*/

return(status);
}
/*********************************************************************
* Subroutine JULIAN to compute the Julian day of an observation:
* (from "cel_meca.c")
*
* The Julian day begins at Greenwich mean noon (at 12 U.T.)
*
* Here also the Gregorian calendar reform is taken into account.
* Thus the day following 1582 October 4 is 1582 October 15.
*
* The B.C. years are counted astronomically. Thus the year
* before the year +1 is the year 0.
*
* Input:
* AA, MM, IDD, TIME : year,month, day, time of the observation
* DJUL : Julian day
**********************************************************************/
int JLP_julian_day(double aa, int mm, int idd, double time, double *djul)
{
double day1, year1, date_obs, date_reform;
long month1, ia1, ib1;

  day1 = time/24. + (double)idd;
/* First the year after the 1st March ... */
  if(mm > 2) {
     year1 = aa;
     month1 = mm;
    }
   else {
     year1 = aa - 1;
     month1 = mm + 12;
    }

/* Then check if after the Gregorian reform: */
    date_obs = aa + ((int)(275 * mm / 9)
               - 2. * (int) ((mm + 9) / 12) + idd - 30 ) / 365.;
    date_reform = 1582. + 289./365.;
    if(date_obs >= date_reform) {
         ia1 = (int) (year1 / 100.);
         ib1 = 2 - ia1 + (int) (((double)ia1)/4.);
       }
    else
         ib1 = 0;

/* Now final formula: */
      *djul = (int)(365.25 * year1) + (int)(30.6001 * (month1 + 1))
              + day1 + 1720994.5 + ib1;

return(0);
}
/*********************************************************************
* Subroutine BESSELIAN to compute the Besselian epoch of an observation:
* Besselian year is a tropical year
* which starts when the mean sun has an ecliptic longitude of 280 degrees
*
* INPUT:
* aa, mm, idd, time : year,month, day, time of the observation
*
* OUTPUT:
* b_date : Besselian epoch
**********************************************************************/
int JLP_besselian_epoch(double aa, int mm, int idd, double time, double *b_date)
{
double djul;

JLP_julian_day(aa, mm, idd, time, &djul);

// My old formula:
// *b_date = 2000. + (djul - 2451544.53)/365.242189;

/****************************************************************
// My old formula:
*besselian_epoch = 2000. + (djul - 2451544.53)/365.242189;

pisco/Gdpisco$ runs jlp_test_julian_and_bessel
 Enter current date as year,month,day,time :
2018,7,11,6.6
Julian day: 2458310.50000 Besselian epoch: 2018.524612 Julian epoch: 2018.522930 (Besselian epoch converted to Julian with WDS formula: =2018.522938 )

****************************************************************
With new formula:
*besselian_epoch = 1900. + (djul - 2415020.31352)/365.242198781;

 Enter current date as year,month,day,time :
2018,7,11,6.6
ZZZ: besselian_epoch=2018.52461245
ZZZ: new besselian_epoch=2018.52460265
Julian day: 2458310.50000 Besselian epoch: 2018.524603 Julian epoch: 2018.522930 (Besselian epoch converted to Julian with WDS formula: 2018.522929 )
*/

// New formula, from Wikipedia:
*b_date = 1900. + (djul - 2415020.31352)/365.242198781;

return(0);
}
/*********************************************************************
* Subroutine JULIAN to compute the Jullian epoch of an observation:
*
* INPUT:
* aa, mm, idd, time : year,month, day, time of the observation
*
* OUTPUT:
* j_date : Julian epoch
**********************************************************************/
int JLP_julian_epoch(double aa, int mm, int idd, double time, double *j_date)
{
double djul;

JLP_julian_day(aa, mm, idd, time, &djul);

// From Wikipedia:
*j_date = 2000. + (djul - 2451545.0)/365.25;

return(0);
}
/*********************************************************************
* Subroutine BESSELIAN to JULIAN 
* to convert Julian epoch to Besselian epoch
*
* INPUT:
* b_date : Besselian epoch
*
* OUTPUT:
* j_date : Julian epoch
**********************************************************************/
int JLP_besselian_to_julian_epoch(double b_date, double *j_date)
{

// From IAU G1 Information Circular No 192,
// WDS formula: 
*j_date = (b_date * 0.999978641) + 0.041439661; 

return(0);
}
