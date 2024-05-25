/***************************************************************
* Set of routines of general use:
* auto_scale1, auto_sky, auto_sky_d
*
* AUTHOR: JLP
* VERSION: 03-06-2017
***************************************************************/
#include <stdio.h>
#include <math.h>
#include "jlp_macros.h"
#include "jlp_auto_scale1.h"

/* Defined here 
int auto_scale_float(float *image1, int nx1, int ny1, int idim,
                     float *lower_itt, float *upper_itt);
int auto_scale_double(double *image1, int nx1, int ny1, int idim,
                double *lower_itt, double *upper_itt);
int auto_scale_double_box(double *image1, int nx1, int ny1, int idim,
                int ix_min, int ix_max, int iy_min, int iy_max,
                double *lower_itt, double *upper_itt);
int auto_scale_double_box(double *image1, int nx1, int ny1, int idim,
                int ix_min, int ix_max, int iy_min, int iy_max,
                double *lower_itt, double *upper_itt);
int auto_sky(double *in_image, int nx, int ny, int idim, double *sky_level);
int auto_sky_d(double *in_image, int nx, int ny, int idim, double *sky_level,
               double *sky_noise);
*/

/*
#define DEBUG
*/

/***************************************************************
 Routine to compute a good scale to display the image 
 (Called by Xdisp1 and plot_3d)

 Two modes:
  if input lower_itt=upper_itt=0: min/max scale
  otherwise: high contrast scale
****************************************************************/
int auto_scale_float(float *image_f1, int nx1, int ny1, int idim1,
                     float *lower_itt, float *upper_itt)
{
int status, i, j;
double *image_d1, d_lower_itt, d_upper_itt;

*lower_itt = 0.;
*upper_itt = 1.;
if((nx1 <= 0) || (ny1 <= 0) || (nx1 > idim1)) return(-1);
 
image_d1 = new double[nx1 * ny1];
for(j = 0; j < ny1; j++) {
 for(i = 0; i < nx1; i++) {
  image_d1[i + j * nx1] = image_f1[i + j * idim1];
  }
 }

status =  auto_scale_double(image_d1, nx1, ny1, nx1, &d_lower_itt, 
                            &d_upper_itt);
if(status == 0) {
  *lower_itt = d_lower_itt;
  *upper_itt = d_upper_itt;
  }

delete[] image_d1;

return(status);
}
/***************************************************************
 Routine to compute a good scale to display the image 
 (Called by Xdisp1 and plot_3d)

 Two modes:
  if input lower_itt=upper_itt=0: min/max scale
  otherwise: high contrast scale
****************************************************************/
int auto_scale_double(double *image1, int nx1, int ny1, int idim,
                double *lower_itt, double *upper_itt)
{
int status;
status =  auto_scale_double_box(image1, nx1, ny1, idim, 0, nx1, 0, ny1,
                               lower_itt, upper_itt);
return(status);
}
/***************************************************************
 Routine to compute a good scale to display the image 
 (Called by Xdisp1 and plot_3d)

 Two modes:
  if input lower_itt=upper_itt=0: min/max scale
  otherwise: high contrast scale
****************************************************************/
int auto_scale_double_box(double *image1, int nx1, int ny1, int idim,
                int ix_min, int ix_max, int iy_min, int iy_max,
                double *lower_itt, double *upper_itt)
{
double sum, sumsq, mean, sigma;
double w1, mini, maxi, mini1, maxi1;
int npts, min_max_scale, jj;
int i, j;

#ifdef DEBUG
 printf("auto_scale_double/ nx1=%d ny1=%d idim=%d\n", nx1, ny1, idim);
 printf("auto_scale_double/ x:%d,%d y=%d,%d \n", ix_min, ix_max, iy_min, iy_max);
#endif

/* Check if output should be min max or high contrasted scale: */
min_max_scale = (*lower_itt == 0.) ? 1 : 0; 

/* Compute min and max: */
 sum = 0.; sumsq = 0.; npts = 0;
 mini = image1[ix_min + iy_min * nx1];
 maxi = mini;
 for(j = iy_min; j < iy_max; j++) {
   jj = j * idim;
   for(i = ix_min; i < ix_max; i++)
    {
    w1 = image1[i + jj]; 
    npts++;
    sum += w1;
    sumsq += w1*w1;
    if(w1 < mini) mini = w1; 
    if(w1 > maxi) maxi = w1;
    }
  }

/* General case: */
*lower_itt = mini;
*upper_itt = maxi;

/* Check if all values are equal: */
if(mini == maxi) 
   {
   fprintf(stderr, "auto_scale_double: all pixels have same value (= %.6G) !\n", mini);
   fprintf(stderr, "auto_scale_double/ Window: x:%d,%d y=%d,%d (npts=%d)\n", 
           ix_min, ix_max, iy_min, iy_max, npts);
   *lower_itt = mini;
   *upper_itt = mini + 1;
   return(0);
   }

/* Otherwise compute mean and sigma: */
#ifdef DEBUG
   printf("npts = %.6G\n",(double)npts);
#endif
 if(npts == 0) npts = 1; 
 mean = sum / (double)npts;
 sumsq /= (double)npts;
 sumsq = sumsq - mean * mean;
 sigma = sqrt((double)sumsq);
#ifdef DEBUG
 printf("auto_scale_double: min=%.6G, max=%.6G, mean_1=%.6G, sigma_1=%.6G\n",
         mini, maxi, mean, sigma);
#endif

/* Return min and max scale if this option has been selected: */
if(min_max_scale) return(0);

/* Do a second loop with 3 sigma rejection: */
 mini1 = mean - 3. * sigma;
 maxi1 = mean + 3. * sigma;
 sum = 0.; sumsq = 0.; npts = 0;
 for(j = iy_min; j < iy_max; j++) {
   jj = j * idim;
   for(i = ix_min; i < ix_max; i++)
    {
    w1 = image1[i + jj]; 
    if(w1 > mini1 && w1 < maxi1)
      {
      npts++;
      sum += w1;
      sumsq += w1*w1;
      }
    }
  }

/* Compute new estimation of mean and sigma: */
if(npts > 0)
 {
 mean = sum / (double)npts;
 sumsq /= (double)npts;
 sumsq = sumsq - mean * mean;
 sigma = sqrt((double)sumsq);
 }

/* Assume that mean and sigma are meaningful: */
if((mean - mini) > 3. * sigma) *lower_itt = mean - 3. * sigma;
if((maxi - mean) > 3. * sigma) *upper_itt = mean + 3. * sigma;

#ifdef DEBUG
 printf("3-sig rejec => min=%.6G, max=%.6G, mean=%.6G, sigma=%.6G\n",
         *lower_itt,*upper_itt,mean,sigma);
#endif

 return(0);
}
/***************************************************************
 Routine to compute a good scale to display the image 
 (Called by jlp_wx_canvas1) 
 double precision version of auto_scale_double

 Two modes:
  if min/max scale
  otherwise: high contrast scale
****************************************************************/
int auto_scale_double_box(double *image1, int nx1, int ny1, int idim,
                int ix_min, int ix_max, int iy_min, int iy_max,
                double *lower_itt, double *upper_itt, int min_max_scale)
{
double sum, sumsq, mean, sigma;
double w1, mini, maxi, mini1, maxi1;
int npts, jj;
int i, j;

#ifdef DEBUG
 printf("auto_scale_double/ ix1,iy1,ix2,iy2=(%d %d) (%d %d) \n", 
        ix_min, iy_min, ix_max, iy_max);
 printf("auto_scale_double/ nx1=%d ny1=%d idim=%d\n", (int)nx1, (int)ny1, (int)idim);
#endif

/* Compute min and max: */
 sum = 0.; sumsq = 0.;
 npts = 0;
 mini = image1[ix_min + iy_min * nx1];
 maxi = mini;
 for(j = iy_min; j < iy_max; j++) {
   jj = j * idim;
   for(i = ix_min; i < ix_max; i++)
    {
    w1 = image1[i + jj]; 
    sum += w1;
    sumsq += w1*w1;
    npts++;
    if(w1 < mini) mini = w1; 
    if(w1 > maxi) maxi = w1; 
    }
  }

/* General case: */
*lower_itt = mini;
*upper_itt = maxi;

/* Check if all values are equal: */
if(mini == maxi) 
   {
   fprintf(stderr, "auto_scale_double: all pixels have same value (= %.6G) !\n", mini);
   *lower_itt = mini;
   *upper_itt = mini + 1;
   return(0);
   }

/* Otherwise compute mean and sigma: */
#ifdef DEBUG
   printf("npts = %.6G\n",(double)npts);
#endif
 mean = sum / (double)npts;
 sumsq /= (double)npts;
 sumsq = sumsq - mean * mean;
 sigma = sqrt((double)sumsq);
#ifdef DEBUG
 printf("auto_scale_double: min=%.6G, max=%.6G, mean_1=%.6G, sigma_1=%.6G\n",
         mini,maxi,mean,sigma);
#endif

/* Return min and max scale if this option has been selected: */
if(min_max_scale) return(0);

/* Do a second loop with 3 sigma rejection: */
 mini1 = mean - 3. * sigma;
 maxi1 = mean + 3. * sigma;
 sum = 0.; sumsq = 0.; npts = 0;
 for(j = iy_min; j < iy_max; j++) {
   jj = j * idim;
   for(i = ix_min; i < ix_max; i++)
    {
    w1 = image1[i + jj]; 
    if(w1 > mini1 && w1 < maxi1)
      {
      npts++;
      sum += w1;
      sumsq += w1*w1;
      }
    }
  }

/* Compute new estimation of mean and sigma: */
if(npts > 0)
 {
 mean = sum / (double)npts;
 sumsq /= (double)npts;
 sumsq = sumsq - mean * mean;
 sigma = sqrt((double)sumsq);
 }

/* Assume that mean and sigma are meaningful: */
if((mean - mini) > 3. * sigma) *lower_itt = mean - 3. * sigma;
if((maxi - mean) > 3. * sigma) *upper_itt = mean + 3. * sigma;
 printf("3-sig rejec => min=%.6G, max=%.6G, mean=%.6G, sigma=%.6G\n",
         *lower_itt,*upper_itt,mean,sigma);

 return(0);
}
/******************************************************************
*
* Compute the minimum mean in the four corners of the image
*******************************************************************/
int auto_sky(double *in_image, int nx, int ny, int idim, double *sky_level)
{
int nn;
int i, j, k;
double sum[4];

/* Compute the size of the areas used to compute the mean in the corners */
nn = MINI(nx, ny);
if(nn >= 60) nn /= 10; 
  else nn = 6;

for(k = 0; k < 4; k++) sum[k] = 0.;

for(j = 0; j < nn; j++)
 {
 for(i = 0; i < nn; i++)
  {
  sum[0] += in_image[i + j*idim];
  sum[1] += in_image[(nx-i-1) + j*idim];
  sum[2] += in_image[(nx-i-1) + (ny-j-1)*idim];
  sum[3] += in_image[i + (ny-j-1)*idim];
  }
 }
for(k = 0; k < 4; k++) sum[k] /= ((double)nn*(double)nn);

/* Select minimum value: */
*sky_level = sum[0];
for(k = 1; k < 4; k++) 
    if(*sky_level > sum[k]) *sky_level = sum[k]; 

printf(" auto_sky/ sky value: %.3f \n",*sky_level);

return(0);
}
/******************************************************************
*
* Compute the minimum mean in the four corners of the image
*******************************************************************/
int auto_sky_d(double *in_image, int nx, int ny, int idim, double *sky_level,
               double *sky_noise)
{
int nn, k0, k1;
int i, j, k;
double sum[4], sumsq[4];

/* Compute the size of the areas used to compute the mean in the corners */
nn = MINI(nx, ny);
nn = MAXI(nn/5, 5); 

for(k = 0; k < 4; k++) {sum[k] = 0.; sumsq[k] = 0.;}

for(j = 0; j < nn; j++)
 {
 for(i = 0; i < nn; i++)
  {
  sum[0] += in_image[i + j*idim];
  sumsq[0] += SQUARE(in_image[i + j*idim]);
  sum[1] += in_image[(nx-i-1) + j*idim];
  sumsq[1] += SQUARE(in_image[(nx-i-1) + j*idim]);
  sum[2] += in_image[(nx-i-1) + (ny-j-1)*idim];
  sumsq[2] += SQUARE(in_image[(nx-i-1) + (ny-j-1)*idim]);
  sum[3] += in_image[i + (ny-j-1)*idim];
  sumsq[3] += SQUARE(in_image[i + (ny-j-1)*idim]);
  }
 }

/* Compute mean and sigma: */
for(k = 0; k < 4; k++) {
  sum[k] /= (double)(nn*nn);
  sumsq[k] /= (double)(nn*nn);
  }

for(k = 0; k < 4; k++) 
   {
   sumsq[k] = sumsq[k] - SQUARE(sum[k]); 
   if(sumsq[k] > 0.) sumsq[k] = sqrt(sumsq[k]);
    else sumsq[k] = 0.;
   }

/* Sky level is the last but one value 
* (when sorted in increasing order): */
*sky_level = sum[0];
k0 = 0;
for(k = 1; k < 4; k++) 
    if(*sky_level > sum[k]) 
       {
       *sky_level = sum[k]; 
       k0 = k;
       }
*sky_level = 1.e12;
for(k = 0; k < 4; k++) 
    if(k != k0 && *sky_level > sum[k]) 
       {
       *sky_level = sum[k]; 
       k1 = k;
       }

*sky_noise = sumsq[k1];

/* DEBUG: 
printf(" k0=%d, k1=%d\n", k0, k1);
for(k = 0; k < 4; k++) 
  printf(" zone #%d: sky=%.3e  noise=%.3e\n", k, sum[k], sumsq[k]);
*/

printf(" auto_sky_d/ sky value: %.3e  (noise: %.3e)\n",
        *sky_level, *sky_noise);

return(0);
}
