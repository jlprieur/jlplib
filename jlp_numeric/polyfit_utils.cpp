/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* polyfit.c 
* Set of routines to fit a polynomial P(x)
*
* JLP 
* Version 07-02-2007
-------------------------------------------------------------------*/
#define DEBUG

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <math.h>
#include "jlp_numeric.h"

/* Defined here: */
static int compute_residuals(double *xx, double *yy, int npts, 
                             double *xc, int poly_order, double *rms_resid);
int POLYFIT(double *xx, double *yy, int *npts, int *poly_order,
            double *xc, double *error_xc, double *rms_resid);
int CALPOLY(double *x, double *y, double *xc, int *poly_order);

#ifdef MAIN_TEST
int main(int argc, char *argv[])
{
double xc[4], error_xc[4], rms_resid;
double xx[200], yy[200];
int i, status, npts = 200, poly_order;

printf(" Program to test polyfit routine\n");
printf(" JLP Version 07-02-07 \n");

/* Simulate data: */
srand(1);

poly_order = 3;
for(i = 0; i < npts; i++) {
 xx[i] = i;
 yy[i] = 0.5 + 2.3 * xx[i] + 4.5 * xx[i] * xx[i] + 6.1 * xx[i] * xx[i] * xx[i];
 yy[i] += 0.1 * ((double)rand() / (double)RAND_MAX - 0.5);
 }

/* Solve problem */
status = POLYFIT(xx, yy, &npts, &poly_order, xc, error_xc,
                 &rms_resid);

return(0);
}
#endif
/*********************************************************************
* Compute the residuals 
*********************************************************************/
static int compute_residuals(double *xx, double *yy, int npts, 
                             double *xc, int poly_order, double *rms_resid)
{
double sum, sumsq, ww, poly_value;
int i;

  sum = 0.;
  sumsq = 0;
  for(i = 0; i < npts; i++) {
   CALPOLY(&xx[i], &poly_value, xc, &poly_order);
   ww = yy[i] - poly_value; 
   sum += ww;
   sumsq += ww * ww;
   }
sum /= (double)npts;
sumsq = sumsq/(double)npts - sum * sum;
sumsq = sqrt(sumsq);
/*
printf("compute_residuals/mean=%f sigma=%f\n", sum, sumsq);
*/
*rms_resid = sumsq;
return(0);
}  
/********************************************************************
* YY = poly(XX),
* or:  yy = AA xc
* or:  yy = xc[0] + xc[1] * xx[i]  + xc[2] * xx[i]^2 .... 
* 
*
* Solve the least-square problem:
*  AA^* AA xc = AA^*  BB
*
* with: 
* BB: vector equal to yy[] array
* AA: operator such that AA:
*  AA xc[i] = poly(x) = xc[0] + xc[1] * xx[i]  + xc[2] * xx[i]^2 .... 
*  xc: vector with polynomial coefficients 
* AA is a matrix whose line #i is formed with:
*     1   xx[i]    xx[i]^2    xx[i]^3 ...
* BB is a vector whose #i th element is yy[i]
*
* INPUT:
* poly_order: Order of the polynomial
* xx, yy : arrays to be fitted by a polynomial (i.e. yy = poly(xx) )
* npts: number of data points (i.e. nber of (xx[i],yy[i]) pairs)
*
* OUTPUT:
* xc : array of the coefficients of the polynomial 
*      (xc[i] is the coeff. of xx^(i-1)
*      (size of xc is ncoeff=poly_order+1) 
* error_xc : standard deviation for the coefficients xc
* rms_resid : mean standard deviation of the fit 
*
********************************************************************/
int POLYFIT(double *xx, double *yy, int *npts, int *poly_order,
            double *xc, double *error_xc, double *rms_resid)
{
double *AA;
int i, k, ifail, status, ncoeff;

ncoeff = *poly_order + 1;

/* Matrix AA for least-squares: */
if((AA = (double *)malloc(*npts * ncoeff * sizeof(double))) == NULL){
 printf("POLYFIT/Fatal error allocating memory, ncoeff=%d npts=%d\n",
         ncoeff, *npts);
 exit(-1);
 }

/* Prepare the matrix AA: */
   for(i = 0; i < *npts; i++) {
     AA[0 + i * ncoeff] = 1;
     for(k = 1; k < ncoeff; k++)
        AA[k + i * ncoeff] = AA[(k-1) + i * ncoeff] * xx[i];
   }

/* Initial guess: */
  for(i = 0; i < ncoeff; i++) xc[i] = 0.;

/* Solve problem
    AA^* AA xc = AA^* yy 
 with conjugate gradients:
*/
  status = JLP_CGRAD(AA, yy, xc, &ncoeff, npts, &ifail);
  if(ifail != 0 || status) {
     printf("From JLP_CGRAD/status=%d ifail=%d\n", status, ifail);
     status = -2;
  } 
  else {
/* Output coefficients: b + ax */
#ifdef DEBUG
   printf("Solution is");
   for(k = 0; k < ncoeff; k++) printf(" xc[%d]=%e", k, xc[k]);
   printf("\n");
#endif
/* Compute residuals */
   compute_residuals(xx, yy, *npts, xc, *poly_order, rms_resid);
#ifdef DEBUG
   printf("Mean rms residuals = %e\n", *rms_resid);
#endif
  }

free(AA);
return(ifail);
}
/************************************************************************
* Compute value y = poly(x)
* with the xc coefficients
*
* INPUT:
* x : value of x, for which the computation is to be made 
* xc: coefficients of the polynomial (size poly_order+1)
* poly_order: order of the polynomial
*
* OUTPUT:
* y = poly(x)
*
************************************************************************/
int CALPOLY(double *x, double *y, double *xc, int *poly_order)
{
double ww;
int k;

*y = xc[0];
ww = *x;
for(k = 1; k <= *poly_order; k++) {
  *y += xc[k] * ww;
  ww *= (*x);
  }
return(0);
}
