/******************************************************************
* gdp_speckle_set1.h
*
* Set of routines of speckle_wx_set1.cpp 
* used to obtain astrometry of close binaries from autocorrelations
* Subtraction of a model of the background 
*
* JLP
* Version 08/04/2011
*******************************************************************/
#ifndef _gdp_speckle_set1_h   // BOF sentry
#define _gdp_speckle_set1_h

int speckle_subtract_model(double *autoc_data, int nx, int ny, int idim, 
                           char *back_model_fname, char *error_message);
int speckle_patch_statistics(double *image_patch, int nx1, int ny1, double xc,
                             double yc, double radius, double *mean_sky,
                             double *sigma_sky, double *max_value,
                             double *negval_percent, int *bad_fit);
int astrom_output_to_logfile(double xac, double yac, double maxi, double flux,
                             double xc, double yc, double diam, int poly_order,
                             double mean_sky, double sigma_sky,
                             int nx1, int ny1, char *log_message, char* method,
                             int astrom_only, int centered_polar_coordinates);
int astrom_barycenter(double *array1, int nx1, int ny1,
                      double xc, double yc, double diam, double mean_sky,
                      double *xac, double *yac, double *maxi, double *flux);
int astrom_gaussian_fit(double *array1, int nx1, int ny1,
                        double xc, double yc, double diam, double mean_sky,
                        double *xac, double *yac, double *maxi, double *flux,
                        int *ifail);
int speckle_convert_to_centered_polar(double xm, double ym, int nx1, int ny1,
                             double *rho, double *theta, double *theta180);
int speckle_convert_to_polar(double xm, double ym, 
                             double *rho, double *theta, double *theta180);
#endif
