/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* Filter routines to enhance the contrast of autocorrelations
* Used by JLP_wxImagePanel
*
* UnsharpMaskingFilter
* HighContrastFilter, VeryHighContrastFilter
*
* JLP
* Version 02-01-2015
---------------------------------------------------------------------*/
#include <stdio.h>
#include <math.h>
#include "jlp_numeric.h"  // jlp_sort.c
#include "jlp_patch_set1.h"
#include "jlp_wx_ipanel_filters.h"

#define BAD_VALUE -1.23456

/* Contained here (and prototypes defined in "jlp_wx_ipanel_filters.h"):
int UnsharpMaskingFilter(double *in_image1, double *out_image2,
                         int nx2, int ny2, int iwidth);
int HighContrastFilter1(double *modsq_centered, double *autoc,
                        double *UnresolvedModsqFrame,
                        double SigmaUnresolvedModsq,
                        const int nx2, const int ny2);
int HighContrastFilter2(double *modsq_centered, double *autoc, int nx2, int ny2);
int MedianProfileFilter(double *modsq_centered, double *autoc,
                        double *UnresolvedModsqFrame,
                        double SigmaUnresolvedModsq,
                        const int nx2, const int ny2);
int ComputeProfile(double *ima0, int nx0, int ny0, double *pro0, int np0,
                   int ioption);
int SubtractProfile(double *ima0, int nx0, int ny0, double *pro0, int np0);
int subtract_back_model(double *autoc_data, double *back_model,
                        double *autoc_flattened, int nx, int ny, int idim);
*/

static double ImageValue(double *image1, int nx1, int ny1,
                                      const int i, const int j);

/***************************************************************************
* Unsharp masking
* JLP version (dec 2014)
* cells should be larger than 3x3
* i.e., half_width should be larger than 1
***************************************************************************/
int UnsharpMaskingFilter(double *in_image1, double *out_image2,
                         int nx2, int ny2, int half_width)
{
double sum, mini0, value, value_orig, mean;
int iw, jw;
register int i, j, ii, jj;

for(i = 0; i < nx2 * ny2; i++) out_image2[i] = in_image1[i];

// Smooth the image:
iw = half_width;
jw = half_width;
for(j = jw; j < ny2 - jw; j++) {
 for(i = iw; i < nx2 - iw; i++) {
   sum = 0.;
   mini0 = 1.e+12;
   for(jj = -jw; jj <= jw; jj++) {
    for(ii = -iw; ii <= iw; ii++) {
//   sum += in_image1[(ii + i) + (jj + j) * nx2];
     value = ImageValue(in_image1, nx2, ny2, ii + i, j + jj);
// Determine the minimum of the edges of the cell
     if((ii == -iw) || (ii == iw) || (jj == -jw) || (jj == jw)) { 
       mini0 = MINI(mini0, value);
       }
     sum += value;
    }
   }
   mean = sum / (double)((2 * iw + 1) * (2 * jw + 1));
   value_orig = ImageValue(in_image1, nx2, ny2, i, j);
// JLP2016: this formula seems magic (!)
   value = (value_orig - 0.8 * mean) / 0.2;
// Handle negative values:
   if(value < 0.1) value = 0.2 * value_orig;
   out_image2[i + j * nx2] = value;
  }
 }

return(0);
}
/**************************************************************************
* Handle the edges by making the image periodic:
***************************************************************************/
static double ImageValue(double *image1, int nx1, int ny1,
                                      const int i, const int j)
{
double value;
value = 0.;
 if((i >= 0 && i < nx1) && (j >= 0 && j < ny1)) {
   value = image1[i + j * nx1]; 
 } else if((i < 0) && (j >= 0 && j < ny1)) {
   value = image1[(nx1 + i) + j * nx1]; 
 } else if((i >= nx1) && (j >= 0 && j < ny1)) {
   value = image1[(nx1 - i) + j * nx1]; 
 } else if((i >= 0 && i < nx1) && (j < 0)) {
   value = image1[i + (ny1 + j) * nx1]; 
 } else if((i >= 0 && i < nx1) && (j >= ny1)) {
   value = image1[i + (ny1 - j) * nx1]; 
 }

return(value);
}
/***************************************************************************
* High contrast (strioscopic) filter, derived from version 2008
* version of 2008 for VCRB
*
***************************************************************************/
int HighContrastFilter1(double *modsq_centered, double *autoc,
                        double *UnresolvedModsqFrame,
                        const double SigmaUnresolvedModsq,
                        const int nx2, const int ny2)
{
double ww, *tmp, mini_value;
int ixc, jxc, nxx2, nyy2;
register int i, j;
// rad2 is forbidden by Windows !
double rad_sq, sigma12, sigma22;
FFTW_COMPLEX *data;

data = new FFTW_COMPLEX[nx2 * ny2];
nxx2 = nx2, nyy2 = ny2;

tmp = new double[nx2 * ny2];

  for(i = 0; i < nx2 * ny2; i++) tmp[i] = modsq_centered[i];

// JLP2011: Wiener deconvolution
// Division by the unresolved modsq when available:
 if(UnresolvedModsqFrame != NULL) {
// Shift center to the lower left corner:
   RECENT_FFT_DOUBLE(tmp, tmp, &nxx2, &nyy2, &nxx2);
   for(i = 0; i < nx2 * ny2; i++) {
// WARNING: origin is not 0,0 here....
// Wiener filter is used for deconvolution:
     ww = MAXI(1.e-09, UnresolvedModsqFrame[i] + SigmaUnresolvedModsq);
     tmp[i] /= sqrt(ww);
     }
   } else {
     ixc = nx2/2;
     jxc = ny2/2;
     sigma12 = SQUARE((double)nx2/16.);
     sigma22 = SQUARE((double)nx2/8.);
     for(j = 0; j < ny2; j++) {
      for(i = 0; i < nx2; i++) {
       rad_sq = SQUARE(i - ixc) + SQUARE(j - jxc);
       tmp[i + j * nx2] /= (20. * exp(-rad_sq/sigma12) + 2. * exp(-rad_sq/sigma22) + 0.2);
      }
     }
// Shift center to the lower left corner:
     RECENT_FFT_DOUBLE(tmp, tmp, &nxx2, &nyy2, &nxx2);
   }

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Compute the autocorrelation (FFT-1 of the power-spectrum):
  for(i = 0; i < nx2 * ny2; i++) {
      c_re(data[i]) = tmp[i];
      c_im(data[i]) = 0;
      }

delete[] tmp;

// JLP99: warning: NY,NX!!!!
   fftw_fast(data, ny2, nx2, -1);
   for(i = 0; i < nx2 * ny2; i++) autoc[i] = c_re(data[i]);

// Shift origin to the lower left corner:
   RECENT_FFT_DOUBLE(autoc, autoc, &nxx2, &nyy2, &nxx2);

// Minimum value computed on the bottom edge:
   mini_value = 0.;
   for(i = 0; i < nx2; i++) mini_value += autoc[i];
   mini_value /= (double)nx2;

// The autocorrelation may have aberrant large values everywhere,
// so I subtract mini_value
// It may have also strong negative values relative to the edges,
// so I correct them (better for further display):
   for(i = 0; i < nx2 * ny2; i++) autoc[i] = MAXI(autoc[i] - mini_value, 0.);

delete data;

return(0);
}
/***************************************************************************
* High contrast (strioscopic) filter, version 2015
* Attempt to zeroing the central frequencies without UnresolvedModsqFrame
*
* INPUT:
*  modsq_centered: power spectrum
*
* OUTPUT:
*  autoc: autocorrelation (derived from a filtered version od modsq_centered)
*
***************************************************************************/
int HighContrastFilter2(double *modsq_centered, double *autoc, int nx2, int ny2)
{
double *tmp_r, *tmp_i, sum_yi_fi, sum_fi_fi, fi, a_coeff, b_coeff;
int ixc, jxc, idirect, iwidth;
register int i, j;
// Something strange with Windows/gcc: rad2 is not allowed as a variable!
/* So I substitute with "rad22" ... */
double mini_value, rad22, sigma1_2, sigma2_2, gaussian_filter, ww;

// Minimum value computed on the bottom edge:
mini_value = 0.;
for(i = 0; i < nx2; i++) mini_value += modsq_centered[i];
mini_value /= (double)nx2;

tmp_r = new double[nx2 * ny2];
tmp_i = new double[nx2 * ny2];

for(i = 0; i < nx2 * ny2; i++) tmp_r[i] = modsq_centered[i];
for(i = 0; i < nx2 * ny2; i++) tmp_i[i] = 0.;

ixc = nx2/2;
jxc = ny2/2;
// 128/16 = 8
sigma1_2 = SQUARE((double)nx2/16.);
// 128/8 =16
sigma2_2 = SQUARE((double)nx2/8.);

// Fit a first Gaussian in the center of the Fourier plane:
sum_yi_fi = 0.;
sum_fi_fi = 0.;
iwidth = nx2/16;
for(j = jxc - iwidth; j < jxc + iwidth; j++) {
  for(i = ixc - iwidth; i < ixc + iwidth; i++) {
    rad22 = SQUARE(i - ixc) + SQUARE(j - jxc);
    fi = exp(-rad22/sigma1_2);
    sum_fi_fi += fi * fi;
    sum_yi_fi += fi * tmp_r[i + j * nx2];
  }
 }
a_coeff = sum_yi_fi / sum_fi_fi;

// Fit a second Gaussian in the center of the Fourier plane:
sum_yi_fi = 0.;
sum_fi_fi = 0.;
iwidth = nx2/8;
for(j = jxc - iwidth; j < jxc + iwidth; j++) {
  for(i = ixc - iwidth; i < ixc + iwidth; i++) {
    rad22 = SQUARE(i - ixc) + SQUARE(j - jxc);
    fi = exp(-rad22/sigma2_2);
    sum_fi_fi += fi * fi;
    sum_yi_fi += fi * tmp_r[i + j * nx2];
  }
 }
b_coeff = sum_yi_fi / sum_fi_fi;

#ifdef DEBUG
printf("HighContrastFilter: a_coeff=%f b_coeff=%f\n", a_coeff, b_coeff);
#endif

for(j = 0; j < ny2; j++) {
  for(i = 0; i < nx2; i++) {
    rad22 = SQUARE(i - ixc) + SQUARE(j - jxc);
// Version similar to 2008:
if(true){
// 128/16 = 8 in HighContrastFilter1, so I take somewhat larger:
  sigma1_2 = SQUARE((double)nx2/20);
// 128/8 = 16 in HighContrastFilter1, so I take somewhat larger:
  sigma2_2 = SQUARE((double)nx2/10.);
  gaussian_filter = 4. * exp(-rad22/sigma1_2) + 1. * exp(-rad22/sigma2_2) + 0.2;
  tmp_r[i + j * nx2] /= gaussian_filter;
} else {
// Version of 2015:
    gaussian_filter = 0.6 * ( a_coeff * exp(-rad22/sigma1_2)
                            + b_coeff * exp(-rad22/sigma2_2));
    ww = tmp_r[i + j * nx2] - gaussian_filter;
    tmp_r[i + j * nx2] = MAXI(0., ww);
   }
 }
}

// Shift center to the lower left corner:
// Defined in "recent_fft.cpp":
//int RECENT_FFT_DOUBLE(double* modsq, double* modsq1, int *nx, int *ny, int *xdim);
 RECENT_FFT_DOUBLE(tmp_r, tmp_r, &nx2, &ny2, &nx2);

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Compute the autocorrelation (FFT-1 of the power-spectrum):

// JLP99: warning: NY,NX!!!!
 idirect = -1;
 fftw_2D_double(tmp_r, tmp_i, ny2, nx2, idirect);
 for(i = 0; i < nx2 * ny2; i++) autoc[i] = tmp_r[i];

// Shift origin to the lower left corner:
 RECENT_FFT_DOUBLE(autoc, autoc, &nx2, &ny2, &nx2);

// Minimum value computed on the bottom edge:
 mini_value = 0.;
 for(i = 0; i < nx2; i++) mini_value += autoc[i];
 mini_value /= (double)nx2;

// The autocorrelation may have aberrant large values everywhere,
// so I subtract mini_value
// It may have also strong negative values relative to the edges,
// so I correct them (better for further display):
 for(i = 0; i < nx2 * ny2; i++) autoc[i] = MAXI(autoc[i] - mini_value, 0.);

delete[] tmp_r;
delete[] tmp_i;

return(0);
}
/***************************************************************************
* Median Profile filter new version (with/without unresolved modsq frame)
* Very high contrast filter
*
* In 2008: UnresolvedModsqFrame!=NULL and ioption = 2 (median profile)
* In 2015: UnresolvedModsqFrame=NULL and ioption = 3 (minimum profile)
*
* INPUT:
* modsq_centered
* UnresolvedModsqFrame: pointer may be NULL if not available/or for another try
*                       not used for version of 2015
* SigmaUnresolvedModsq: sigma of the unresolved mods frame
* version_2008: flag set to true for version of 2008
*
* OUTPUT:
* autoc: filtered autocorrelation
***************************************************************************/
int MedianProfileFilter(double *modsq_centered, double *autoc,
                        double *UnresolvedModsqFrame,
                        const double SigmaUnresolvedModsq,
                        bool version_2008,
                        const int nx2, const int ny2)
{
double *tmp_r, *tmp_i, *pro0;
int np0, ioption;
register int i;

tmp_r = new double[nx2 * ny2];
tmp_i = new double[nx2 * ny2];

for(i = 0; i < nx2 * ny2; i++) tmp_r[i] = modsq_centered[i];
for(i = 0; i < nx2 * ny2; i++) tmp_i[i] = 0.;

// First compute the profile:
// New: 1/4 pixel sampling:
np0 = 4 * (int)(sqrt(SQUARE((double)nx2) + SQUARE((double)ny2)) + 10);
pro0 = new double[np0];

// In 2008: UnresolvedModsqFrame!=NULL and ioption = 2 (median profile)
// In 2015: UnresolvedModsqFrame=NULL and ioption = 3 (minimum profile)
if(UnresolvedModsqFrame == NULL) ioption = 3;
  else ioption = 2;
ComputeProfile(tmp_r, nx2, ny2, pro0, np0, ioption);

// Subtraction of the profile:
SubtractProfile(tmp_r, nx2, ny2, pro0, np0);

// use this power spectrum for high contrast filter:
if(version_2008)
  HighContrastFilter1(tmp_r, autoc, UnresolvedModsqFrame,
                      SigmaUnresolvedModsq, nx2, ny2);
else
  HighContrastFilter2(tmp_r, autoc, nx2, ny2);

delete[] pro0;
delete[] tmp_r;
delete[] tmp_i;
return(0);
}
/***************************************************************************
* Compute mean, median or minimum profile in bins of one pixel width
*
* INPUT:
* pro0: pointer used for the output profile
* np0: number of points to be used to compute the output profile
* ioption: 1=mean 2=pseudo median 3=min
***************************************************************************/
int ComputeProfile(double *ima0, int nx0, int ny0, double *pro0,
                   int np0, int ioption)
{
double *ppro;
int *npro;
// rad2 forbidden by Windows compiler !
double xc, yc, rad22;
int nmax, ipro;
register int i, j;

nmax = 4 * nx0 + ny0;
ppro = new double[np0 * nmax];
npro = new int[np0];
for(i = 0; i < np0; i++) npro[i] = 0;

xc = (double)nx0 / 2.;
yc = (double)ny0 / 2.;
for(j = 0; j < ny0; j++) {
  for(i = 0; i < nx0; i++) {
   rad22 = 1.E-09 + SQUARE(i - xc) + SQUARE(j - yc);
// New: 1/4 pixel sampling:
   ipro = (int)(4. * sqrt(rad22) + 0.5);
   if(ipro < np0) {
     if(npro[ipro] < nmax - 1) {
       ppro[npro[ipro] + ipro * nmax] = ima0[i + j * nx0];
       (npro[ipro])++;
       }
     }
  }
}

switch(ioption) {
// mean profile:
  default:
  case 1:
    for(i = 0; i < np0; i++) {
      pro0[i] = 0.;
      for(j = 0; j < npro[i]; j++) pro0[i] += ppro[j + i * nmax];
      if(npro[i] > 0.) pro0[i] /= (double)(npro[i]);
    }
    break;
  case 2:
// median profile:
    for(i = 0; i < np0; i++) {
/* Sort the array corresponding to bin #i in increasing order: */
// Defined in "jlp_sort.c" (prototypes in jlp_numeric.h):
// int JLP_QSORT_DBLE(double *array, int *nn);
    JLP_QSORT_DBLE(&ppro[i * nmax], &(npro[i]));
// Median corresponds to the middle point
//    pro0[i] = ppro[npro[i] / 2 + i * nmax];
// I take a smaller point:
    pro0[i] = ppro[npro[i] / 3 + i * nmax];
    }
    break;
  case 3:
    for(i = 0; i < np0; i++) {
      pro0[i] = ppro[i * nmax];
      for(j = 1; j < npro[i]; j++) pro0[i] = MINI(pro0[i],ppro[j + i * nmax]);
    }
}

delete[] ppro;
delete[] npro;

return(0);
}
/***************************************************************************
* Subtract a model built with the profile (bins of one pixel width)
* to the input image
*
* INPUT:
* ima0: pointer corresponding to  the input/output image
* pro0: input profile
* np0: number of points of the input profile
***************************************************************************/
int SubtractProfile(double *ima0, int nx0, int ny0, double *pro0, int np0)
{
double *back_model;
double xc, yc, rad22;
int ipro;
register int i, j;

back_model = new double[nx0 * ny0];
for(i = 0; i < nx0 * ny0; i++) back_model[i] = 0.;

xc = (double)nx0 / 2.;
yc = (double)ny0 / 2.;
for(j = 0; j < ny0; j++) {
  for(i = 0; i < nx0; i++) {
   rad22 = SQUARE(i - xc) + SQUARE(j - yc) + 1.E-09;
// New: 1/4 pixel sampling:
   ipro = (int)(4. * sqrt(rad22) + 0.5);
   if(ipro < np0) back_model[i + j * nx0] = pro0[ipro];
  }
}

// int subtract_back_model(double *autoc_data, double *back_model,
//                         double *autoc_flattened, int nx, int ny, int idim)
subtract_back_model(ima0, back_model, ima0, nx0, ny0, nx0);

delete[] back_model;
return(0);
}
/**************************************************************************
* Compute the coefficients to subtract the background model on the frame
* and subtract this model.
*
* Linear regression:
*
* Minimum of Sum ( z - a1 f - a0)^2
* is reached when gradient is nul, i.e., when:
* sum_fz = a1 sum_ff + a0 sum_f
* sum_z  = a1 sum_f + a0 sum_1
*
* INPUT:
* autoc_data
* back_model
*
* OUTPUT:
* autoc_flattened
*
**************************************************************************/
int subtract_back_model(double *autoc_data, double *back_model,
                        double *autoc_flattened, int nx, int ny, int idim)
{
double *tmp;
double wback, wdata, noise_array[1], diam, sigma_sky;
double sum_1, sum_f, sum_z, sum_fz, sum_ff, a0, a1, det;
int ii, jj, ixc, iyc, iside;
register int i, j;
char err_messg[128];

if(autoc_data == NULL || back_model == NULL
   || autoc_flattened == NULL) return(-1);

ixc = nx/2; iyc = ny/2;

tmp = new double[nx * ny];

/* Copy autoc_data to autoc_flattened as a first approximation: */
for(j = 0; j < ny; j++) {
  jj = j * idim;
  for(i = 0; i < nx; i++) {
   autoc_flattened[i + jj] = autoc_data[i + jj];
   tmp[i + jj] = back_model[i + jj];
   }
 }

/* Central pixels are generally bad, so I neutralize them for the fit:
*/
iside = 1;
for(j = -iside; j <= iside; j++) {
  for(i = -iside; i <= iside; i++) {
   ii = (ixc + i) + (iyc + j) * idim;
   tmp[ii] = BAD_VALUE;
   autoc_flattened[ii] = BAD_VALUE;
   }
 }

sum_1 = 0.;
sum_f = 0.;
sum_z = 0.;
sum_fz = 0.;
sum_ff = 0.;
for(j = 0; j < ny; j++) {
  jj = j * idim;
  for(i = 0; i < nx; i++) {
   wback = tmp[i + jj];
   wdata = autoc_flattened[i + jj];
   if(wback != BAD_VALUE) {
     sum_1 += 1.;
     sum_f += wback;
     sum_z += wdata;
     sum_fz += wback * wdata;
     sum_ff += (wback * wback);
     }
  }
 }

/* Resolution with the determinant of the system:
* sum_fz = a1 sum_ff + a0 sum_f
* sum_z  = a1 sum_f + a0 sum_1
*/
det = sum_ff * sum_1 - sum_f * sum_f;
a1 = (sum_1 * sum_fz  - sum_f * sum_z) / det;
a0 = (sum_ff * sum_z  - sum_f * sum_fz) / det;

#ifdef DEBUG
printf("subtract_back_model/ a1=%f a0=%f\n", a1, a0);
#endif

/* Subtract normalized model: */
for(j = 0; j < ny; j++) {
  jj = j * idim;
  for(i = 0; i < nx; i++) {
   ii = i + jj;
   if(tmp[ii] != BAD_VALUE) autoc_flattened[ii] -= (a1 * tmp[ii] + a0);
  }
}

/* In patch2_set_c.c:
int POLY_CIRC_PATCH(double *image1, int nx1, int ny1, int idim1,
                    double xc, double yc, double diam, double diam_factor,
                    double *noise_array,
                    int noise_dim, double sigma_noise, int poly_order,
                    double *sigma_sky, char *err_message)
* Replace central pixel with 3rd polynomial interpolation,
* within a disk of (2*iside+1):
*/
 noise_array[0] = 0.;
/* +2 is too large
*  +1.5 is a bit small
*/

 diam = 2.*iside + 1.8;
 POLY_CIRC_PATCH(autoc_flattened, nx, ny, idim,
            (double)ixc + 0.5, (double)iyc + 0.5, diam, 1.2,
            noise_array, 1, 0., 3,
            &sigma_sky, err_messg);

/* Central pixel is generally bad, so I replace its value
* with an interpolation of its neighbours: */
autoc_flattened[ixc + iyc * idim] = (autoc_flattened[ixc-1 + iyc * idim]
                                + autoc_flattened[ixc+1 + iyc * idim]
                                + autoc_flattened[ixc + (iyc - 1) * idim]
                                + autoc_flattened[ixc + (iyc + 1) * idim])/ 4.;

return(0);
}
