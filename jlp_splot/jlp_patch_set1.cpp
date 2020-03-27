/***************************************************************************
* jlp_patch_set1.cpp
*
* Set of subroutines used by Xdisp1 and jlp_wx_widgets for "patching" images 
*
* Contains:
*    POLY_CIRC_PATCH, PROFILE_CIRC_PATCH
*
*     Package of routines for finding and removing defects from
*     an image array.  The position of the area to delete may
*     be specified manually using the cursor, or by reading a file. 
*     The user is prompted for the diameter of a circle
*     of pixels to delete around this position, which are
*     replaced by an interpolation of a surface fitted to
*     an annulus of surrounding pixels.  A constant, planar,
*     quadratic, or cubic surface may be specified (1,3,6, or 10
*     terms).  The interpolation is then displayed on the screen 
*     and the user judges the quality of fit.  If necessary, the
*     original values of the pixels may be restored and a
*     different interpolation attempted.
*
* From Fortran version of 05/04/2006
* Converted to C by JLP in August 2006
*
* JLP
* Version 26/01/2013
***************************************************************************/
#include <stdio.h>
#include <math.h>
#include "jlp_macros.h"
#include "jlp_random.h"
#include "jlp_patch_set1.h" 

/*
#define DEBUG
*/

/* Size of arrays used to fit the background in POLY_CIRC_PATCH */
#define NDIM_MAX 16384 
/* Maximum number of polynomial terms: */
#define NTERMS 30

/* Prototypes defined in "patch_set1.h": */
/*
int POLY_CIRC_PATCH(double *image1, int nx1, int ny1, int idim1,
               double xp, double yp, double diam, double *noise_array, 
               int noise_dim, int poly_order, double *sigma_sky, 
               char *err_message);
int PROFILE_CIRC_PATCH(double *image1, int nx1, int ny1, int idim1,
                       double xp, double yp, double diam, double *noise_array, 
                       int noise_dim, double *sigma_sky, char *err_message);
int CREATE_NOISE_ARRAY(double **noise_array, int noise_dim);
int DELETE_NOISE_ARRAY(double *noise_array);
*/

// Static functions
static int get_profile_value(double radc, double *rad, double *mean, int nprof, 
                             double *mean_value);

static int patch_neqsol(double *xx, double *yy, double *zz, double *wwork, 
                        int *ndim, int *npts, int *nterms, int *noto, 
                        int *ioor, double *dd, double *se, double *rdoe, 
                        double *sdor, char *err_message);
static int patch_reject(double *xx, double *yy, double *zz, int *ndim, double *dd,
                        int *nn, int *kk, double *tt, int *mm, int *jreject);
double patch_poly(double *z1, double *z2, double *dd);


/************************************************************************
* Create a noise array used by patch routines (needed for increasing speed)
*
* INPUT:
* noise_dim: dimension of noise_array (NB: 256 is a good value...)
*
* OUTPUT:
* noise_array: set of Gaussian random values in the range [0,1]
*************************************************************************/
int CREATE_NOISE_ARRAY(double **noise_array, int noise_dim)
{
double ww;
long int seed;
register int i;

*noise_array = new double[noise_dim];
  seed = 1;
  JLP_RANDOM_INIT(&seed);
  for(i = 0; i < noise_dim; i++) {
     JLP_RANDOM_GAUSS(&ww);
     (*noise_array)[i] = ww;
    }
return(0);
}
int DELETE_NOISE_ARRAY(double *noise_array)
{
delete[] noise_array;
return(0);
}

/******************************************************************
* Subroutine to fit a polynomial to an annulus around the center
* and replace input values by noised computed values
* 
* INPUT:
* poly_order: polynomial order
* diam: diameter of the patch (in pixels)
* noise_array: Gaussian random noise array (normalized to 1.)
* sigma_noise: sigma of the noise to be used for adding noise
* noise_dim: size of (monodimensional) noise_array
* sigma_sky: standard deviation of the background
*
* INPUT/OUTPUT:
* image1[nx1, ny1]: image to be processed
*******************************************************************/
int POLY_CIRC_PATCH(double *image1, int nx1, int ny1, int idim1,
                    double xp, double yp, double diam, double diam_factor,
                    double *noise_array, 
                    int noise_dim, double sigma_noise, int poly_order, 
                    double *sigma_sky, char *err_message)
{
int ixp, iyp, status, italk = 0;
/* NTERMS = maximum number of polynomial terms: */
double wwork[NDIM_MAX], dd[NTERMS], se[NTERMS], rdoe[NTERMS];
double xx[NDIM_MAX], yy[NDIM_MAX], zz[NDIM_MAX], xz, yz, xnoise;
double test, sdor, dx, dy, radmin2, diam_max, radmax, radmax2, rad2;
double xran;
int kq, nr, nterms, npts, imin, imax, jmin, jmax;
int noto, ioor, jreject, ndim = NDIM_MAX;
register int i, j, k;

*sigma_sky = 0.;

/* SET UP ARRAYS FOR patch_neqsol
*
* diam_factor=2 => annulus of diameters DIAM (inside) and 2*DIAM (outside)
* Before 2006:
*      diam_factor=2
* JLP2006: diam_factor=2 is bad: I reduce it to 1.5
*/
  diam_max = diam * diam_factor;
  radmax = (diam_max + 1) / 2;
  radmax2 = radmax * radmax;

/* Center coordinates: */
    ixp = NINT(xp);
    iyp = NINT(yp);

/* Boundaries: */
    imin = MAXI((int)(xp - radmax), 0);
    imax = MINI((int)(xp + radmax + 1), nx1);
    jmin = MAXI((int)(yp - radmax), 0);
    jmax = MINI((int)(yp + radmax + 1), ny1);

/*  Define normalizing factors such that the coords. of
*  all the points are between [-1,-1] and [1,1].
*/
    radmin2 = SQUARE(diam / 2.);
    dx = 2. / (double)(imax - imin);
    dy = 2. / (double)(jmax - jmin);
    npts = 0;

    for(j = jmin; j < jmax; j++)
      for(i = imin; i < imax; i++){
/*
* Calculate radius, and check if it lies between radmin and radmax 
* then use this point to compute the polynomial.
* JLP2006: The cursor gives (0.5,0.5) when the cursor
* is centered on the pixel (0,0) in the bottom-left corner:
*/
       rad2 = SQUARE((double)j - (yp - 0.5))
              + SQUARE((double)i - (xp - 0.5));
        if (rad2 > radmin2 && rad2 <= radmax2) {
/*
*  The coords. of all the points are between (-1,-1) and (1,1).
*/
          xx[npts] = (i - imin) * dx - 1.;     /* X coordinate */
          yy[npts] = (j - jmin) * dy - 1.;     /* Y coordinate */
          zz[npts] = image1[i + j * idim1];
          npts++;
          if(npts >= ndim) {
            fprintf(stderr, 
                 "POLY_CIRC_PATCH/Error: too many points: npts=%d (maxi=%d)\n", 
                    npts, ndim);
            return(-1);
            }
        }
     }

/* Number of terms according to the polynomial order: */
  switch (poly_order) {
    case 5:
     nterms = 21; break;
    case 4:
     nterms = 15; break;
    case 3:
     nterms = 10; break;
    case 2:
     nterms = 6; break;
    case 1:
     nterms = 3; break;
    default:
     nterms = 1; break;
    }

/* Check if enough points to solve the problem: */
    if(npts <= nterms) {
      fprintf(stderr,"POLY_CIRC_PATCH/Error: too few points to fit the background\n");
      fprintf(stderr,"npts = %d nterms = %d (polynomial order = %d)\n",
              npts, nterms, poly_order);
      fprintf(stderr,"xc=%.2f yc=%.2f diam=%.2f diam_fact=%.2f\n", 
              xp, yp, diam, diam_factor);
      return(-1);
      }
/*
* Initialize the solution:
*/
  for(i = 0; i < 30; i++) dd[i] = 0.;

/*
* Fit the polynomial with all points:
*/
  noto = 1;
  ioor = 0;
  status = patch_neqsol(xx, yy, zz, wwork, &ndim, &npts, &nterms, &noto, 
                        &ioor, dd, se, rdoe, &sdor, err_message);
  if(status) {
    fprintf(stderr,"POLY_CIRC_PATCH/Error (1) in patch_neqsol status=%d\n", status);
    return(-1);
    }

/* Perform twice a 2 sigma rejection (test = 2 * sdor):
*/
  for(kq = 0; kq < 2; kq++) {
    test = 2. * sdor;
    jreject = 1;
    patch_reject(xx, yy, zz, &ndim, dd, &npts, &nterms, &test, &nr, &jreject);

/* nr: number of points after rejection
*/
    npts = nr;
    if(npts <= nterms && italk) {
      fprintf(stderr,"POLY_CIRC_PATCH/Warning: too few points to fit the background\n");
      fprintf(stderr,"After %dth rejection: npts = %d nterms = %d\n",
                     kq+1, npts, nterms);
      *sigma_sky = sdor;
      }
    else
/* Refit polynomial with the selected points (i.e., 2-sigma rejection): 
*/
    status = patch_neqsol(xx, yy, zz, wwork, &ndim, &npts, &nterms, 
                          &noto, &ioor, dd, se, rdoe, &sdor, err_message);
    if(status) {
      fprintf(stderr,"POLY_CIRC_PATCH/Error (2) in patch_neqsol status=%d\n", 
              status);
      return(-2);
      }

  } /* EOF loop on kq */

#ifdef DEBUG
  for(i = 0; i < nterms; i++) printf("solution: dd[%d]=%f\n", i, dd[i]); 
  for(i = 0; i < 30; i++) printf("dd[%d]=%f\n", i, dd[i]); 
#endif
/*
* Evaluate polynomial at each point within circle
*/
  for(j = jmin; j < jmax; j++) {
/*
* Find a random starting point in the sequence of noise points
* to obtain random numbers between 0. and 1. using noise_array
*/
    JLP_RANDOM(&xran);
    k = (int)(xran * (double)noise_dim);
    k = MAXI(0, k);
    k = MINI(k, noise_dim - 1);

    yz = (j - jmin) * dy - 1.;
    for(i = imin; i < imax; i++) {
/* JLP2006: The cursor gives (0.5,0.5) when the cursor
* is centered on the pixel (0,0) in the bottom-left corner:
*/
    rad2 = SQUARE((double)j - (yp - 0.5))
            + SQUARE((double)i - (xp - 0.5));
    if (rad2 <= radmin2) {
       xz = (i - imin) * dx - 1.;
/* Handle k index in "noise_array": */
       k++;
       if(k >= noise_dim) k = 0;
/* Noise is normalized to the standard deviation obtained in the fit: */
       xnoise = sigma_noise * noise_array[k] * sdor;
/* Calling patch_poly function: 
*/
#ifdef DEBUG
if(yz < -0.2)
 printf("xz=%f yz=%f, image1=%f poly_value=%f\n", xz, yz, 
        image1[i + j * idim1], patch_poly(&xz, &yz, dd));
#endif
       image1[i + j * idim1] = patch_poly(&xz, &yz, dd) + xnoise;
       }
     }
   }

*sigma_sky = sdor;
return(0);
}
/******************************************************************
* Subroutine to compute a mean profile (centered in nx1/2, ny1/2)
* and replace input values by noised computed values
* inside of the circle of diameter diam centered in (xp, yp)  
* 
* INPUT:
* diam: diameter of the patch (in pixels)
* noise_array: Gaussian random noise array (normalized to 1.)
* sigma_noise: sigma of the noise to be used for adding noise
* noise_dim: size of (monodimensional) noise_array
* sigma_sky: standard deviation of the background
*
* INPUT/OUTPUT:
* image1[nx1, ny1]: image to be processed
*******************************************************************/
int PROFILE_CIRC_PATCH(double *image1, int nx1, int ny1, int idim1,
                       double xp, double yp, double diam, double diam_factor,
                       double *noise_array, int noise_dim, double sigma_noise, 
                       double *sigma_sky, char *err_message)
{
int ixp, iyp, ip;
double sum[NDIM_MAX], sumsq[NDIM_MAX], ssum;
double mean[NDIM_MAX], sig[NDIM_MAX], rad[NDIM_MAX];
double xnoise, gain = 3, mean_value;
double radmin2, diam_max, radmax, radmax2, rad2, radc, radc2, radc_min;
double xran;
int imin, imax, jmin, jmax;
int ndim = NDIM_MAX, npts[NDIM_MAX], nprof;
double xc, yc, ww;
register int i, j, k;

*sigma_sky = 0.;

/* 
* diam factor=1.5 => annulus of diameters DIAM (inside) and 1.5*DIAM (outside)
*  factor = 1.8;
*/
  diam_max = diam * diam_factor;
  radmax = (diam_max + 1) / 2;
  radmax2 = radmax * radmax;
  radmin2 = SQUARE(diam / 2.);

/* Center of image: */
xc = nx1/2;
yc = ny1/2;

/* Check if it is reasonable to compute such a profile: */
  radc2 = SQUARE(xp - (xc - 0.5)) + SQUARE(yp - (yc - 0.5));
  if(radc2 < 4.) {
    sprintf(err_message, "PROFILE_CIRC_PATCH/error: center of patch too close from center of image");
    return(-1);
  }
  radc_min = sqrt(radc2) - radmax - 1.; 

/* Center coordinates: */
  ixp = NINT(xp);
  iyp = NINT(yp);

/* Boundaries: */
  imin = MAXI((int)(xp - radmax), 0);
  imax = MINI((int)(xp + radmax + 1), nx1);
  jmin = MAXI((int)(yp - radmax), 0);
  jmax = MINI((int)(yp + radmax + 1), ny1);

/* Initialization of profile arrays: */
 for(i = 0; i < ndim; i++) {
   npts[i] = 0;
   sum[i] = 0.;
   sumsq[i] = 0.;
   mean[i] = 0.;
   rad[i] = 0.;
   sig[i] = 0.;
   }

/* Main loop for computing the profile: */
   for(j = jmin; j < jmax; j++)
     for(i = imin; i < imax; i++){
/*
* Calculate radius, and reject if too close to star centre
*
* JLP2006: The cursor gives (0.5,0.5) when the cursor
* is centered on the pixel (0,0) in the bottom-left corner:
*/
      rad2 = SQUARE((double)j - (yp - 0.5))
             + SQUARE((double)i - (xp - 0.5));
      if(rad2 > radmin2 && rad2 <= radmax2) {
/* JLP2007: should not remove 0.5 here ! */
         radc2 = SQUARE((double)j - xc) + SQUARE((double)i - yc);
/* Index of profile (higher resolution since it is necessary close to the
* center: */
         ip = NINT(gain * (sqrt(radc2) - radc_min));
         ip = MINI(ndim - 1, MAXI(0, ip));
/* Update profile values: */
         ww = image1[i + j * idim1];
         sum[ip] += ww; 
         sumsq[ip] += ww * ww; 
         npts[ip]++;
        }
     }

/* Compute mean profile with significant data (i.e. more than 3 points): */
 k = 0;
 for(i = 0; i < ndim; i++) {
  if(npts[i] >= 3) { 
    mean[k] = sum[i] / (double)npts[i];
    ww = sumsq[i] / (double)npts[i] - SQUARE(mean[k]);
    sig[k] = sqrt(ww);
    rad[k] = radc_min + (double)i / gain;
    k++;
    }
  }
nprof = k;

if(k < 3) {
  sprintf(err_message, "PROFILE_CIRC_PATCH/Error: too few points, nprof=%d", 
          nprof);
  return(-1);
  }

/* Mean sigma: */
ssum = 0.;
for(k = 0; k < nprof; k++) ssum += sig[k];
*sigma_sky = ssum / (double)nprof;

#ifdef DEBUG
printf("Profile nprof=%d, mean sigma = %f\n", nprof, *sigma_sky);
#endif

/*
* Affect profile value to each point within circle
*/
  for(j = jmin; j < jmax; j++) {
/*
* Find a random starting point in the sequence of noise points
* to obtain random numbers between 0. and 1. using noise_array
*/
    JLP_RANDOM(&xran);
    k = (int)(xran * (double)noise_dim);
    k = MAXI(0, k);
    k = MINI(k, noise_dim - 1);

    for(i = imin; i < imax; i++) {
/* JLP2006: The cursor gives (0.5,0.5) when the cursor
* is centered on the pixel (0,0) in the bottom-left corner:
*/
    rad2 = SQUARE((double)j - (yp - 0.5))
            + SQUARE((double)i - (xp - 0.5));
    if (rad2 <= radmin2) {
/* JLP2007: should not remove 0.5 here ! */
      radc2 = SQUARE((double)j - yc) + SQUARE((double)i - xc);
      radc = sqrt(radc2);
/* Index of profile (should be the same as above): */
      get_profile_value(radc, rad, mean, nprof, &mean_value);
/* Handle k index in "noise_array": */
       k++;
       if(k >= noise_dim) k = 0;
/* Noise is normalized to the standard deviation of the profile: */
       xnoise = sigma_noise * noise_array[k] * (*sigma_sky);
/* Use the mean profile value: */
       image1[i + j * idim1] = mean_value + xnoise;
       }
    } /* EOF loop on i */
   } /* EOF loop on j */

return(0);
}
/***********************************************************************
* Look for the index of "radc" in "rad" array
*
* rad: array sorted in increasing order
* radc: value to be looked for
************************************************************************/
static int get_profile_value(double radc, double *rad, double *mean, int nprof, 
                             double *mean_value)
{
register int i;
int status = -1, kp;

/* kp: index of closer value of radc in rad array */
kp = 0;
*mean_value = mean[0]; 
for(i = 0; i < nprof; i++) {
 if(radc < rad[i]) {
  if(i > 0) {
    *mean_value = (mean[i-1] * (rad[i] - radc) 
                 + mean[i] * (radc - rad[i-1])) / (rad[i] - rad[i-1]); 
/* DEBUGGG
printf("radc=%.2f rad[i]=%.2f rad[i-1]=%.2f (i=%d) \n", 
        radc, rad[i], rad[i-1], i);
printf("value=%.2f mean[i]=%.2f mean[i-1]=%.2f\n", 
        *mean_value, mean[i], mean[i-1]);
*/
    kp = i - 1;
  } else {
    *mean_value = mean[0]; 
    kp = 0;
  }
  status = 0;
  break;
 }
}

return(status);
}
/* **************************************************************
*
* SUBROUTINE patch_neqsol(X,Y,WWORK,NDIM,NPTS,NTERMS,NOTO,IOOR,D,SE,RDOE,SDOR)
*
* THIS SUBROUTINE COMPUTES THE COEFFICIENTS, D, WHICH DEFINE THAT LINEAR
* FUNCTION, Y, OF LINEARLY INDEPENDENT FUNCTIONS WHICH BEST FITS, IN THE
* LEAST SQUARES SENSE, A GIVEN SET OF DATA. OR EQUIVALENTLY, IT FINDS
* THE SOLUTION TO THE SYSTEM OF NORMAL EQUATIONS WHICH IS CALLED THE
* NORMAL EQUATIONS SOLUTION.
*
*     WRITTEN AND DOCUMENTED BY:
*
*     JONES, W B, OBITTS, D L, GALLET, R M, AND DE VAUCOULEURS, G,
*     'ASTRONOMICAL SURFACE PHOTOMETRY BY NUMERICAL MAPPING
*     TECHNIQUES', PUBLICATION OF THE ASTRONOMY DEPARTMENT, UNIV.
*     OF TEXAS, AUSTIN, SERIES II, VOL. I, NO. 8, FEB. 1967
*
*     MODIFIED BY W D PENCE, UNIV. OF SUSSEX, SEPT. 1980
*
*     X(NDIM), Y(NDIM) = COORDINATES OF POINTS (R*4)
*     Z(NPTS) = VALUE OF POINT AT X(NPTS),Y(NPTS)  (R*4)
*     WWORK = SCRATCH ARRAY  (DOUBLE PRECISION, R*8)
*     NDIM = FIRST DIMENSION OF X, Y AND WW (I*4)
*     NPTS = NO. OF POINTS IN LEAST SQUARES FIT  (I*4)
*     NTERMS = NO. OF COEFS TO BE SOLVED FOR  (I*4)
*     NOTO = NO. OF TOTAL ORTHOGONALIZATIONS TO PERFORM (I*4)
*     IOOR = POSITIVE INTEGER IF OPTIONAL OUTPUT IS REQUIRED
*     D(NTERMS) = COEFFICIENTS OF THE POLYNOMIAL (DOUBLE PRECISION, R*8)
*                 Maximum: NTERMS=30
*     SE(K) = RMS OF FIT USING ONLY THE FIRST K COEFFICIENTS (R*8)
*     RDOE(NTERMS) = INDICATES THE STATISTICAL SIGNIFICANCE OF EACH TERM (R*8)
*     SDOR = STANDARD DEVIATION OF RESIDUALS  (R*4)
*
* ***************************************************************/
static int patch_neqsol(double *xx, double *yy, double *zz, double *wwork, 
                        int *ndim, int *npts, int *nterms, int *noto, int *ioor,                        double *dd, double *se, double *rdoe, double *sdor, 
                        char *err_message)
{
double aa[30], akj[30][30], aaa[30], ss[30][30], yg[30];
double ggsqt[30], gg[30][30], qq[30][30][2];
double sum, flnpts = 0, zzz, zzbar, tt, gk;
int ix, jy, jul, kul, ixx, jyy;
register int i, j, k, l, m;
 
if(*nterms >= *npts) {
  sprintf(err_message,"NEQSOL/Error: too few points nterms=%d npts=%d", 
          *nterms, *npts);
  return(-1);
  }
if(*nterms > 30) {
  sprintf(err_message,"NEQSOL/Error: too many terms: nterms=%d", *nterms);
  return(-1);
  }
if(*noto < 1 || *noto > 2) {
  sprintf(err_message,"NEQSOL/Error: noto=%d (should be 1 or 2)\n", *noto);
  return(-1);
  }


 if(*noto == 1) {
/* COMPUTE THE SQUARE OF THE NORM OF THE VECTOR OF THE DEPENDENT VARIABLE,
* COMPUTE THE MEAN OF THE DEPENDENT VARIABLE, YBAR.
*/
   flnpts = *npts;
   zzz = 0.;
   zzbar = 0.;
   for(i = 0; i < *npts; i++) {
      zzz += zz[i] * zz[i];
      zzbar += zz[i];
      }
   zzbar /= (double)(*npts);

#ifdef DEBUG
printf("\n NEQSOL/nterms=%d noto=%d npts=%d\n", *nterms, *noto, *npts);
printf("NEQSOL/Input data: sumsq=zzz=%f mean=zzbar=%f\n", zzz, zzbar);
#endif

/* COMPUTE THE MATRIX OF INNER PRODUCTS OF THE NORMALIZED FITTING FUNCTIONS
*/
/* Generate the X and Y exponents for the polynomial: */
/* ( X**ix * Y**jy) */
  ix = 0; 
  jy = -1;
/* BOF loop on j */
  for(j = 0; j < *nterms; j++) {
      if(ix != 0) {
        ix--;
        jy++;
       } else {
        ix = jy + 1;
        jy = 0;
       }
/*
#ifdef DEBUG
  printf("NEQSOL/Term #%d Exponents: ix=%d jy=%d \n", j, ix, jy);
#endif
*/
    tt = 0.;
    sum = 0.;
    for(i = 0; i < *npts; i++) {
       gk = 1.;
       if(ix != 0) gk = pow(xx[i], ix);
       if(jy != 0) gk *= pow(yy[i], jy);
       wwork[i] = gk;
       tt += gk * gk;
       sum += zz[i] * gk;
    }
    ggsqt[j] = sqrt(tt);
    yg[j] = sum / ggsqt[j];
    gg[j][j] = 1.0;
       if(j > 0) { 
         kul = j - 1;
/* Generate the X and Y exponents for the polynomial: */
         ixx = 0;
         jyy = -1;
         for(k = 0; k <= kul; k++) {   
          if(ixx != 0) {
            ixx--;
            jyy++;
            } else {
            ixx = jyy + 1;
            jyy = 0;
            }
/* */
           tt = 0.;
           for(i = 0; i < *npts; i++) {
             gk = 1.;
             if(ixx != 0) gk = pow(xx[i], ixx);
             if(jyy != 0) gk *= pow(yy[i], jyy);
             tt += wwork[i] * gk;
             }
           gg[j][k] = tt / (ggsqt[j] * ggsqt[k]);
           gg[k][j] = gg[j][k];
          } /* EOF look on k */
        } /* EOF j > 0 */
   } /* EOF loop on j */
 } /* EOF noto==1 */

/* COMPUTE THE MATRIX OF COEFFICIENTS, Q, DEFINING THE ORTHOGONAL FUNCTIONS
* IN TERMS OF THE FITTING FUNCTIONS.
*/
      qq[0][0][*noto - 1] = 1.;
      ss[0][0] = 1.;
      aaa[0] = 1.;
      if(*noto == 1) {
         for(k = 1; k < *nterms; k++) {
           akj[k][0] = - gg[k][0];
           }
       } else {
         for(k = 1; k < *nterms; k++) {
           sum = 0.;
           for(j = 0; j <= k; j++) {
             sum += qq[k][j][0] * gg[j][0];
             }
           akj[k][0] = -sum;
           }
       }
/* BOF loop on k */
      for(k = 1; k < *nterms; k++) {
        qq[k][k][*noto - 1] = 1.;
        ss[k][k] = 1.;
        jul = k - 1;
        for(j = 0; j <= jul; j++) {
          if(*noto == 1) {
            tt = akj[k][j];
            } else {
            tt = 0.;
              for(l = j; l <= jul; l++) tt += akj[k][l] * ss[l][j];
            }
          ss[k][j] = tt;
          }
/* */
        for(j = 0; j <= jul; j++) {
          sum = 0.;
          for(l = j; l <= jul; l++) {
                  sum += ss[k][l] * qq[l][j][0];
                  }
          if(*noto == 1) {
            qq[k][j][0] = sum;
            } else {
            qq[k][j][1] = sum + qq[k][j][0];
            }
         }
/* COMPUTE THE VECTOR OF THE SQUARE OF THE NORM OF THE ORTHOGONAL FUNCTIONS
*/
       sum = 0.;
         for(j = 0; j <= k; j++) {
           tt = 0.;
           for(l = 0; l <= k; l++) {
             tt += qq[k][l][*noto - 1] * gg[l][j];
             }
           sum += qq[k][j][*noto - 1] * tt;
           }
         aaa[k] = sum;
         if(k < *nterms) {
            for(j = k + 1; j < *nterms; j++) {
              sum = 0.;
                for(l = 0; l <= k; l++) {
                  if(*noto == 1) {
                     tt = gg[j][l];
                     } else {
                     tt = 0.;
                       for(m = 0; m <= j; m++) tt += qq[j][m][0] * gg[m][l];
                     }
                   sum += qq[k][l][*noto - 1] * tt;
                   }
              akj[j][k] = -sum / aaa[k];
              }
          }
    } /* EOF loop on k */
 
/* COMPUTE THE LEAST SQUARES COEFFICIENTS, A, FOR THE SOLUTION IN TERMS OF
* THE ORTHOGONAL FUNCTIONS.
*/
    for(k = 0; k < *nterms; k++) {
      sum = 0.;
       for(j = 0; j <= k; j++) sum += qq[k][j][*noto - 1] * yg[j];
      aa[k] = sum/aaa[k];
/* DEBUG
printf("DDDD/aa[%d]=%f sum=%f\n", k, aaa[k], sum);
*/
      }
 
/* COMPUTE THE LEAST SQUARES COEFFICIENTS,D, FOR THE SOLUTION IN TERMS OF
* THE FITTING FUNCTIONS.
*/
    for(k = 0; k < *nterms; k++) {
      sum = 0.;
       for(j = k; j < *nterms; j++) sum += qq[j][k][*noto - 1] * aa[j];
      dd[k] = sum / ggsqt[k];
/*
#ifdef DEBUG
      printf("dd[%d]=%f sum=%f ggsqt[k]=%f\n", k, dd[k], sum, ggsqt[k]);
#endif
*/
      }

/* COMPUTE THE STANDARD DEVIATION OF THE RESIDUALS, SDOR.
*/
   *sdor = 0.;
    for(i = 0; i < *npts; i++) {
        sum = patch_poly(&xx[i], &yy[i], dd);
        tt = zz[i] - sum;
        *sdor += tt * tt;
      }
    *sdor = sqrt(*sdor/(flnpts - (double)(*nterms)));
#ifdef DEBUG
  printf("patch_neqsol/Mean standard deviation: %f\n",*sdor);
#endif
return(0);
}
/******************************************************************
*
*     SUBROUTINE REJECT(X,Y,NDIM,D,N,K,TT,M,J)
*
*     WRITTEN BY JONES ET AL., 1967
*     MODIFIED BY W D PENCE, 1980
*
*     REJECT ANY POINT WHOSE RESIDUAL IS GREATER THAN BETA*SIGMA
*     FROM THE POLYNOMIAL DEFINED BY THE COEFFICIENTS D.
*
*     X(NDIM),Y(NDIM) = INDEPENDENT VARIABLE (R*4)
*     Z(NDIM) = THE DEPENDENT VARIABLE (R*4)
*     NDIM = FIRST DIMENSION OF X AND Y  (I*4)
*     D(30) = ARRAY CONTAINING THE POLYNOMIAL COEFFICIENTS (R*8)
*     N = NUMBER OF VALUES TO BE TESTED
*     K = THE NUMBER OF TERMS IN THE POLYNOMIAL
*     TT = THE TEST VALUE USED TO SPECIFY THE REJECTION LEVEL
*     M = THE MODIFIED NUMBER OF VALUES AFTER REJECTION
*     JREJECT = AN INDICATOR WHICH IS ZERO ONLY ON THE LAST CALL
*               ( THIS PREVENTS ANY POINTS FROM BEING REJECTED)
*
* In "patch2_set.for:"
* TEST=2.*SDOR (2-sigma)
*        CALL REJECT1(XX,YY,NDIM,D,NPTS,NTERMS,TEST,NR,1,-1)
*
********************************************************************/
static int patch_reject(double *xx, double *yy, double *zz, int *ndim, double *dd, 
                        int *nn, int *kk, double *tt, int *mm, int *jreject)
{
int indxp[21], indxn[21], npos, nneg;
double avr, uu, vv;
double rr, ss;
register int i, j;
  
#ifdef DEBUG
  printf("REJECT/Test=%f jreject=%d\n", *tt, *jreject);
#endif

  for(i = 0; i < 21; i++) {
     indxn[i] = 0;
     indxp[i] = 0;
     }

  avr = 0.;
  uu = 0.;
  vv = 0.;
  npos = 0;
  nneg = 0;
  *mm = 0;
 
/* BOF loop on i=0,nn */
    for(i = 0; i < *nn; i++) { 
/* Compression of the arrays with the X,Y coordinates: 
*/
      xx[*mm] = xx[i];
      yy[*mm] = yy[i];
      zz[*mm] = zz[i];
      ss = patch_poly(&xx[i], &yy[i], dd);
/*
*     R=RESIDUAL FROM POLYNOMIAL FIT
*/
      rr = zz[i] - ss;
      avr += rr;
      if( rr < 0) {
        nneg++;
        ss = -rr;
/* DO NOT INCREMENT COUNTER M IF RESIDUAL IS GREATER THAN T LIMIT */
        if((ss <= *tt) || (*jreject == 0)) (*mm)++; 
        }
      else if (rr > 0) {
        npos++;
        ss = rr;
        if((ss <= *tt) || (*jreject == 0)) (*mm)++; 
        }
      else
        (*mm)++; 

/* */
      uu += rr * rr;
      vv += rr * rr * rr;

/* Fill indxp (positive residuals) and indnp (negative residuals): */
      if(rr < 0) {
         if(rr + 0.2 < 0) {
           indxn[20]++;
           } else {
           j = (int)(-100. * rr);
           indxn[j]++;
           }
        }
      else if (rr > 0) {
         if(rr - 0.2 > 0) {
           indxp[20]++;
           } else {
           j = (int)(100. * rr);
           indxp[j]++;
           }
        }
      else {
        indxp[0]++;
        }

} /* EOF loop on i=0,nn */

(*mm)--;

return(0);
}
/***********************************************************************
* Function patch_poly(X,Y,D)
*
* Evaluates the polynomial defined by the coefficients dd, at
* the normalized coordinate (x,y).
*
*  z1 = normalized x coordinate  (R*4)
*  z2 = normalized y coordinate  (R*4)
*  dd(30) = array containing the coefficients of the polynomial (R*8)
*
* From a Fortran version written by W D PENCE, Nov. 1980
*
***********************************************************************/
static double ylast_value = -123456789.000;

double patch_poly(double *z1, double *z2, double *dd)
{
double xx, y1, y2, y3, y4, y5, y6;
double c0, c1, c2, c3, c4, c5, c6, c7;
double poly0;
/*
* First, calc 1-d polynomial in x for given y value, if
* not already done
*/
c0 = 0.; c1 = 0.; c2 = 0.;
c3 = 0.; c4 = 0.; c5 = 0.;
c6 = 0.; c7 = 0.;
     xx = *z1;
     y1 = *z2;

     if(y1 != ylast_value){
       y2 = y1 * y1;
       y3 = y2 * y1;
       y4 = y3 * y1;
       y5 = y4 * y1;
       y6 = y5 * y1;
       }
/* (Error found here in dec 2014)... */

       c0 = dd[0] + dd[2] * y1 + dd[5] * y2 + dd[9] * y3 
            + dd[14] * y4 + dd[20] * y5 + dd[27] * y6;
       c1 = dd[1] + dd[4] * y1 + dd[8] * y2 + dd[13] * y3 
            + dd[19] * y4 + dd[26] * y5;
       c2 = dd[3] + dd[7] * y1 + dd[12] * y2 + dd[18] * y3 + dd[25] * y4;
       c3 = dd[6] + dd[11] * y1 + dd[17] * y2 + dd[24] * y3;
       c4 = dd[10] + dd[16] * y1 + dd[23] * y2;
       c5 = dd[15] + dd[22] * y1;
       c6 = dd[21] + dd[29] * y1;
       c7 = dd[28];

/*
* Evaluate polynomial in X
*/
      poly0 = ((((((c7 * xx + c6) * xx + c5) * xx + c4) * xx
                    + c3) * xx + c2) * xx + c1) * xx + c0;

ylast_value = y1;

return(poly0);
}
