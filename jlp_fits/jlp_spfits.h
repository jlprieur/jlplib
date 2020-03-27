/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* jlp_spfits.h 
*
* COROT keywords
*
* JLP
* Version 30/01/2006 
---------------------------------------------------------------------*/
typedef struct{
/* Name of object: */
  char x_name[30];        /* OBJECT: Name of object */ 
/* Coordinates of center of field (2000) (decimal in degrees): */
  float obs_ra;           /* RA */ 
  float obs_dec;          /* DEC */
/* Coordinates of center of field (2000) (in H M S and D ' "): */
  char obs_ra_str[30];    /* RASIX */ 
  char obs_dec_str[30];   /* DECSIX */
/* Date */
  char date[30];          /* DATE: date of creation of FITS UV table */ 
  char date_obs[30];      /* DATE-OBS: date of observation */
/* */
  float sun_cor;          /* SUN_COR: Heliocentric correction (km/s)*/ 
/* Universal Time */
  char UT_value[30];      /* UTSTART */
/* Filename  (in secondary HDU) */
  char filename[40];      /* FILENAME */
} spec_info;
