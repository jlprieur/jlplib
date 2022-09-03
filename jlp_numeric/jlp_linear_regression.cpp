/*************************************************************************
* compute the linear regression between a cloud of points (xx, yy)
*
* JLP
* Version 27/05/2016
*************************************************************************/
#include <stdio.h>

int jlp_linear_regression(double *yy, double *xx, int npts, double *aa,
                          double *bb)
{
double sum, sumsq, mean_xx, mean_yy, var_xx, var_yy, covar_xx_yy;
int i, status = -1;

*aa = 0.;
*bb = 0.;

if(npts == 0) {
 fprintf(stderr, "jlp_linear_regression/Error: npts=0 !\n");
 return(-1); 
 }

// Computing the mean of xx:
sum = 0.;
for(i = 0; i < npts; i++) sum += xx[i];
mean_xx = sum / (double)npts;

// Computing the mean of yy:
sum = 0.;
for(i = 0; i < npts; i++) sum += yy[i];
mean_yy = sum / (double)npts;

// Computing the variance of xx:
sum = 0.;
sumsq = 0.;
for(i = 0; i < npts; i++) {
  sum += xx[i];
  sumsq += (xx[i] * xx[i]);
  }
var_xx = sumsq / (double)npts - mean_xx * mean_xx;
if(var_xx == 0) {
  fprintf(stderr, "jlp_linear_regression/ mean_x=%f mean_y=%f \n var_x=%f sumsq=%f npts=%d \n",
         mean_xx, mean_yy, var_xx, sumsq, npts);
  return(-1);
}

// Computing the variance of yy:
sum = 0.;
sumsq = 0.;
for(i = 0; i < npts; i++) {
  sum += yy[i];
  sumsq += (yy[i] * yy[i]);
  }
var_yy = sumsq / (double)npts - mean_yy * mean_yy;

// Computing covariance(xx, yy):
sum = 0.;
for(i = 0; i < npts; i++) {
  sum += xx[i] * yy[i];
  }
covar_xx_yy = sum / (double)npts - mean_xx * mean_yy;

if(var_xx != 0.) {
  *aa = covar_xx_yy / var_xx;
  *bb = mean_yy - *aa * mean_xx;
#ifdef DEBUG
  printf("jlp_linear_regression/ mean_x=%f mean_y=%f \n var_x=%f var_y=%f covar_xy=%f a=%f b=%f\n"
         mean_xx, mean_yy, var_xx, var_yy, covar_xx_yy, *aa, *bb);
#endif
  status = 0;
  } else {
  fprintf(stderr, "jlp_linear_regression/Error: var_x = 0 (npts=%d)\n",
          npts);
  fprintf(stderr, "jlp_linear_regression/ mean_x=%f mean_y=%f \n var_x=%f var_y=%f covar_xy=%f \n",
         mean_xx, mean_yy, var_xx, var_yy, covar_xx_yy);
  }

return(status);
}
