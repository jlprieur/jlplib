/******************************************************************
* jlp_wx_ipanel_speckle.cpp
* Set of routines used to obtain astrometry of close binaries
* from autocorrelations. Subtraction of a model of the background
*
* Contained here: (prototypes defined in "speckle_set1.h")
* int speckle_subtract_model(double *autoc_data, int nx, int ny, int idim,
*                           char *back_model_fname, char *error_message);
* int astrom_gaussian_fit(double *array1, int nx1, int ny1,
*                         double xc, double yc, double diam, int poly_order,
*                         double mean_sky, char *log_message);
* int astrom_barycenter(double *image_patch, int nx1, int ny1,
*                       double xc, double yc, double diam, int poly_order,
*                       double mean_sky, char *log_message);
* int speckle_patch_statistics(double *image_patch, int nx1, int ny1, double xc,
*                             double yc, double radius, double *mean_sky,
*                             double *sigma_sky, double *max_value,
*                             double *negval_percent, int *bad_fit);
*
* JLP
* Version 08/04/2011
*******************************************************************/
// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif

#include "jlp_numeric.h"         // MAXI, jlp_fit_gauss
#include "jlp_wx_ipanel_speckle.h"

/*
#define DEBUG
*/

#ifndef PI
#define PI 3.14159265358979323846
#endif

/*************************************************************************
* astrom_gaussian_fit
*
* INPUT:
* array1: image whose non-null values are to be fitted
* nx1, ny1: size of array1
* xc, yc: approximative coordinates (entered by the user with the cursor)
* mean_sky: mean value of the background
*
* OUTPUT:
*  xac, yac: center of the Gaussian
*  maxi: maximum of the Gaussian
*  flux: integral of the Gaussian
*************************************************************************/
int astrom_gaussian_fit(double *array1, int nx1, int ny1,
                        double xc, double yc, double diam, double mean_sky,
                        double *xac, double *yac, double *maxi, double *flux,
                        int *ifail)
{
double sigx, sigy, rg, errors[4];
double val, *xx, *yy, *f1, radd, radd_circle;
int status, npts;
register int i, j, k;

// Initialization of the output variables (in case of fitting error):
 *xac = xc;
 *yac = yc;
 *maxi = 0.;
 *flux = 0.;

// Something strange with g++/Cygnus: rad2 is not allowed as a variable!
/* First, computes the size of the arrays */
radd_circle = SQUARE(diam / 2.);
k = 0;
  for(j=0; j<ny1; j++)
      for(i=0; i<nx1; i++) {
        radd = SQUARE((double)i - xc) + SQUARE((double)j - yc);
        val = array1[i + j * nx1] - mean_sky;
        if(radd < radd_circle && val > 0.) k++;
      }
npts = k;
 npts = k;
  if(npts < 5) {
    fprintf(stderr, "astrom_gaussian_fit/error, too few points for the fit, npts=%d\n",
            npts);
    return(-1);
  }

/* Then allocate memory space: */
xx = (double *)malloc(npts * sizeof(double));
yy = (double *)malloc(npts * sizeof(double));
f1 = (double *)malloc(npts * sizeof(double));
if(xx == NULL || yy == NULL || f1 == NULL) {
  fprintf(stderr,"astrom_gaussian_fit/Fatal error allocating memory, npts=%d\n", npts);
  exit(-9);
  }

/* Load data to arrays */
status = 0;
  k = 0;
  for(j = 0; j < ny1; j++) {
    for(i = 0; i < nx1; i++) {
      radd = SQUARE((double)i - xc) + SQUARE((double)j - yc);
/* Should remove the value of the background offset for the fit: */
      val = array1[i + j * nx1] - mean_sky;
        if(radd < radd_circle && val > 0.) {
           f1[k] = val;
           xx[k] = (double)i;
           yy[k] = (double)j;
           k++;
        }
    }
  }
/* Performs the fit: */
  jlp_fit_gauss(xx,yy,f1,&npts,&sigx,&sigy,xac,yac,&rg,errors,ifail);

/* Free memory: */
  free(xx);
  free(yy);
  free(f1);

  if(*ifail == 0) {
#ifdef DEBUG
     printf("sigx=%10.4g  sigy=%10.4g  xc=%10.4g  yc=%10.4g  rg=%10.4g \n",
          sigx, sigy, *xac, *yac, rg);
     printf("errors: sigx=%10.4g  sigy=%10.4g  xac=%10.4g  yc=%10.4g\n",
          errors[0], errors[1], errors[2], errors[3]);
#endif

//  ww = SQUARE(((double)i - xac) / sigx) + SQUARE (((double)j - yac) / sigy);
//  f(i,j) = rho * exp(-ww);
 *maxi = rg;
// Integral of rg exp(- (x - xac)^2 /(2 sig_x^2) = rho sig_x \sqrt( 2 PI)
// Hence integral of rg exp(- (x - xac)^2 /(2 sig_x^2) - (y -yac)^2 / 2 sig_y^2)
// = rg * 2 * PI * sig_x * sig_y
// Here sigx^2 = 2 * sig_x^2, hence sig_x = sigx / sqrt(2)
// flux = rg * 2 * PI * (sigx / sqrt(2))^2 * (sigy / sqrt(2))^2;
 *flux = rg * PI * sigx * sigx * sigy * sigy / 2.;

  } else {
     fprintf(stderr,"astrom_gaussian_fit/ifail = %d \n", *ifail);
     status = -1;
  }

return(status);
}
/*************************************************************************
* astrom_ouput_to_logfile
*
* INPUT:
* nx1, ny1: size of array1
* xc, yc: approximative coordinates (entered by the user with the cursor)
* xac, yac: precise coordinates of the center (Gaussian fit of Barycenter)
* maxi: maximum found in the circle (Gaussian fit of Barycenter)
* flux: integral of the Gaussian or sum of intensities in the circle
* poly_order: polynomial order that was used for substracting the background
* mean_sky: mean value of the background
* sigma_sky: standard deviation of the background measured on the edges
*            of the circle
* astrom_only: flag set to one for astrometry only (no photometric parameters)
* centered_polar_coordinates: flag set to one if astrometry is to be displayed
*                    in polar coordinates relative to the center of the
*                    frame (used for speckle or DVA measurements)
*                    (DVA= Direct Vector Autocorrelation)
* method: "Gauss" or "Bary"
*
*************************************************************************/
int astrom_output_to_logfile(double xac, double yac, double maxi, double flux,
                             double xc, double yc, double diam, int poly_order,
                             double mean_sky, double sigma_sky,
                             int nx1, int ny1, char *log_message, char* method,
                             int astrom_only, int centered_polar_coordinates)
{
double rho, theta, theta180;
char buffer[80];

  if(centered_polar_coordinates) {
// Polar conversion relative to the center of the frame:
   speckle_convert_to_centered_polar(xac, yac, nx1, ny1, &rho, &theta, &theta180);
   sprintf(buffer,
           "%%%% rho=%.2f theta=%.2f xc=%.2f yc=%.2f (%.1f,%.1f,%.1f,%d) %s",
           rho, theta180, xac, yac, xc, yc, diam, poly_order, method);
  } else {
   sprintf(buffer, "%%%% xc=%.2f yc=%.2f (%.1f,%.1f,%.1f,%d) %s",
           xac, yac, xc, yc, diam, poly_order, method);
  }

  if(astrom_only) {
     strcpy(log_message,buffer);
  } else {
     sprintf(log_message, "%s \n%%%% maxi=%.2f flux=%.3g sky=%.2f+/-%.2f %s",
             buffer, maxi, flux, mean_sky, sigma_sky, method);
  }

return(0);
}
/*************************************************************************
* astrom_barycenter
*
* INPUT:
* array1: image whose non-null values will be used for computing
*              the barycenter
* nx1, ny1: size of array1
* xc, yc: approximative coordinates (entered by the user with the cursor)
* mean_sky: mean value of the background measured on the edges of the circle
*
* OUTPUT:
*  xac, yac: center of the barycenter
*  maxi: maximum inside of the circle (minus mean_sky)
*  flux: integral inside the circle (minus mean_sky)
*************************************************************************/
int astrom_barycenter(double *array1, int nx1, int ny1,
                      double xc, double yc, double diam, double mean_sky,
                      double *xac, double *yac, double *maxi, double *flux)
{
double sum, xm, ym, dx, dy;
double val, mean, radd, radd_circle;
int nval;
register int i, j;

radd_circle = SQUARE(diam / 2.);

/* Compute the mean and sigma on the difference: */
 sum = 0.; nval = 0; xm = 0.; ym = 0.; dx = 0.; dy = 0.;
 *maxi = -12000.;
 for(j = 0; j < ny1; j++)
   for(i = 0; i < nx1; i++) {
     val = array1[i + j * nx1] - mean_sky;
     radd = SQUARE((double)i - xc) + SQUARE((double)j - yc);
     if(radd < radd_circle && val > 0.) {
        nval++;
        if(val > *maxi) *maxi = val;
        sum += val;
        xm += val * (double)i;
        dx += val * (double)i * (double)i;
        ym += val * (double)j;
        dy += val * (double)j * (double)j;
     }
  }
 if(nval == 0 || sum == 0.) {
   fprintf(stderr, "astrom_barycenter/Error: central patch is null!\n\
Empty circle or null sum : nval=%d sum=%f\n", nval,sum);
   return(-1);
 }

  mean = sum / (double)nval;
  xm /= sum; ym /= sum;
  dx /= sum; dy /= sum;
  dx -= xm*xm; dx = sqrt(dx);
  dy -= ym*ym; dy = sqrt(dy);
#ifdef DEBUG
  printf(" Sum=%e Mean=%e maxi= %e xm=%.2f (+/-%.2f) ym=%.2f (+/-%.2f)\n",
           sum,mean,*maxi,xm,dx,ym,dy);
#endif

*flux = sum;
*xac = xm;
*yac = ym;
return(0);
}
/***************************************************************************
* Conversion from Cartesian to polar coordinates for speckle measurements
* relative to the center of the frame (for autocorrelations)
*
* INPUT:
*  xm, ym: Cartesian coordinates
*  nx1, ny1: size of input image (mean autocorrelation)
*
* OUTPUT:
*  rho: angular separation (in pixels)
*  theta: position angle in degrees (0 for Ox, 90 for Oy)
*  theta180: position angle in degrees (between -90 and +90 degrees)
***************************************************************************/
int speckle_convert_to_centered_polar(double xm, double ym, int nx1, int ny1,
                             double *rho, double *theta, double *theta180)
{
double v1, v2;

/* Polar conversion relative to the center of the frame (for autocorrelations)*/
  v1 = xm - (double)(nx1)/2.;
  v2 = ym - (double)(ny1)/2.;

 speckle_convert_to_polar(v1, v2, rho, theta, theta180);

return(0);
}
/***************************************************************************
* Conversion from Cartesian to polar coordinates for double star measurements
* (for Lucky Imaging)
*
* INPUT:
*  xm, ym: Cartesian coordinates
*
* OUTPUT:
*  rho: angular separation (in pixels)
*  theta: position angle in degrees (0 for Ox, 90 for Oy)
*  theta180: position angle in degrees (between -90 and +90 degrees)
***************************************************************************/
int speckle_convert_to_polar(double xm, double ym,
                             double *rho, double *theta, double *theta180)
{
/* Polar conversion */
  *rho = sqrt((double)(xm * xm + ym * ym));
  if(xm == 0.) {
     if(ym > 0.) *theta = 90.;
     else if (ym < 0.) *theta = -90.;
     else *theta = 0.;
  } else {
     *theta = 180. * atan2(ym, xm) / PI;
  }

/* For autocorrelations... */
   if(*theta < -90) {
      *theta180 = *theta + 180.;
   } else if(*theta > 90) {
      *theta180 = *theta - 180.;
   } else {
      *theta180 = *theta;
   }
return(0);
}
/**************************************************************************
* Computes statistics on the edges of the patch, and on the disk itself
* Proposes a diagnostic concerning the validity of the previous background
* determination by PATCH31
*
* INPUT:
* image_patch: image zeroed outside of the disk centered in xc,yc
*              of diameter diam
*
* OUTPUT:
* mean_sky: mean value of the background (determined in the edges)
* sigma_sky: standard deviation of the background measured on the edges
*            of the circle
* bad_fit: flag set to 1 if background was not properly estimated
**************************************************************************/
int speckle_patch_statistics(double *image_patch, int nx1, int ny1, double xc,
                             double yc, double radius, double *mean_sky,
                             double *sigma_sky, double *max_value,
                             double *negval_percent, int *bad_fit)
{
double sum_edges, sumsq_edges;
int n_edges, negative_nval, nval;
double val, radd, radd_circle, maxval;
double mean_edges, sigma_edges;
register int i, j;

radd_circle = SQUARE(radius);
maxval=image_patch[(int)xc + (int)yc * nx1];
negative_nval = 0;
nval = 0;
sum_edges = 0.; sumsq_edges = 0.; n_edges = 0;

   for(j = 1; j < ny1-1; j++)
     for(i = 1; i < nx1-1; i++) {
       val = image_patch[i + j * nx1];
       maxval = MAXI(maxval, val);
       radd = SQUARE((double)i - xc) + SQUARE((double)j - yc);
       if(radd < radd_circle) {

       nval++;
/* Count negative values inside of the patch: */
       if(val < 0.) negative_nval++;

/* Examine the edges: */
/* Compute the mean and sigma on the edges: */
       if(image_patch[i-1 + j * nx1] == 0.
           || image_patch[i + (j-1) * nx1] == 0.
           || image_patch[i + (j+1) * nx1] == 0.
           || image_patch[i+1 + j * nx1] == 0.) {
            n_edges++;
            sum_edges += val;
            sumsq_edges += (val * val);
           }
        }
     }
if(n_edges <= 3 || maxval == 0.) {
  fprintf(stderr,"edge_statistics/Error disk is null! n_edges=%d maxval=%.2f)\n",
    n_edges, maxval);
  *mean_sky = 0.;
  *bad_fit = -1;
  return(-1);
  }

/* Use mean and sigma values to estimate the quality
* of the background determination: */
 mean_edges = sum_edges / (double)n_edges;
 sigma_edges = (sumsq_edges / (double)n_edges) - SQUARE(mean_edges);
 if(sigma_edges > 0.) sigma_edges = sqrt(sigma_edges);
 *negval_percent = 100. * (double)negative_nval
                       / (double)(nval + negative_nval);

/* Test of quality */
if(*negval_percent > 33. || ABS(mean_edges/maxval) > 0.18) {
  *bad_fit = 1;
// Do not display a warning (to avoid problems with automatic processing)
//  wxLogError(_T("WARNING: the background could not be estimated properly\n"));
} else {
  *bad_fit = 0;
}

// Output values:
  *mean_sky = mean_edges;
  *sigma_sky = sigma_edges;
  *max_value = maxval;

return(0);
}
