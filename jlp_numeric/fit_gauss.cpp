/***************************************************************
* fit_gauss
* To fit a 2-D Gaussian 
* Fits a 2nd order polynomial to Log_intensity(x,y) as a function of
* in x and y coordinates
*
*  Log_Intensity(ix,iy) 
*    f(ix,iy) = a ix**2 + b iy**2 + c iy + d ix + e 
*
* (forces coeff of ix * iy term to be null)
*
*  where:
*  The problem is to find the coefficients (a, b, c, d, e)
*  which minimize the sums:
*   SUM on all the selected disk (ix, iy) of ( Log_intensity - f(xi,yi) ) **2
*   with psi = Log_intensity(ix, iy) and  f(ix,iy) = A * phi
*               / a \
*               | b |
*    with phi = | c |
*               | d | 
*               \ e /
*
* and then solves the normal equation:  A* A phi = A* psi
*
* with A:
*         
*          y0^2 x0^2 y0 x0 1
*          y1^2 x1^2 y1 x1 1
*
*          yi^2 xi^2 yi xi 1
*
*          yn^2 xn^2 yn xn 1
*
*
* JLP 
* 04-10-2006
*****************************************************************/
#include "jlp_numeric.h"

/* Defined in "matrix_set.c": */
int mat_lu_inverse(double *aa, double *aa_inv, int nn);

/*
#define DEBUG
*/
/* JLP2000: This value is very sensitive for stability <= e-4 bad for
* clean_sc_wind.c ... */
#define MIN_VALUE_FOR_LOG 1.e-3

#define NPTS 1000 
/* For KMAXI=2, NCOEFF=6 */
/* For order=KMAXI = 2,    1 x y x^2 xy y^2  */
#define KMAXI 2
#define NCOEFF 6

static int compute_variance(double *aa, double *err_phi, INT4 nx_aa, INT4 ny_aa);
static int compute_ata(double *ata, double *aa, INT4 nx_aa, INT4 ny_aa);
static int compute_sum(double sum[2*KMAXI+1][2*KMAXI+1], INT4 kmax,
                       double *xx, double *yy, INT4 npts);
static int compute_psi(double *psi, double *zz, INT4 npts);
static int compute_aa(double *aa, double *xx1, double *yy1,
                      INT4 nx_aa, INT4 ny_aa);

/*************************************************************
*
* Main program
*
**************************************************************/
#ifdef MAIN_PROG 
main(argc,argv)
int argc;
char *argv[];
{
/* f1 measured intensities */
float xx[NPTS], yy[NPTS], f1[NPTS];
int npts, nx, ny, status, ifail;
float sigx, sigy, xc, yc, rho, wx, wy, errors[5];
int i, j, k;

  printf(" Fit_gauss/JLP version 19/10/98 \n");

/* Fill arrays with data: */
  nx = 8; ny = 8; k = 0;
  printf("Example with: xc=5 yc=3 sigx = 2. sigy = 1.2 rho = 0.35 \n");
   for(j = 0; j < ny; j++)
   {
   for(i = 0; i < nx; i++)
      {
       xx[k] = i;
       yy[k] = j;
       wx = (float)(i - (nx/2+1));
       wy = (float)(j - (ny/2-1));
/* Example with: sigma_x = 2. sigma_y = 1.2 rho = 0.35 */
       f1[k] = 0.2 + 0.35 * exp((double)(- (wx * wx)/ 4. - (wy * wy) / 1.44));
       k++;
      }
   }
   npts = k;

jlp_fit_gauss_flt(xx,yy,f1,&npts,&sigx,&sigy,&xc,&yc,&rho,errors,&ifail);
if(ifail == 0)
    {
    printf("sigx=%.4f+/-%.4f sigy=%.4f+/-%.4f \n",
            sigx, errors[0], sigy, errors[1]);
    printf("xc=%.4f+/-%.4f yc=%.4f+/-%.4f int=%.4f+/-%.4f\n",
           xc, errors[2], yc, errors[3], rho, errors[4]);
    }
else
    printf(" ifail = %d \n",ifail);

/* End of program: */
printf(" Output in fit_gauss.log \n");
exit(0);
}
#endif
/*********************************************************************
*
*********************************************************************/
int jlp_fit_gauss_flt(float *xx0, float *yy0, float *ff0, INT4 *npts,
                      float *sigx0, float *sigy0, float *xc0, float *yc0,
                      float *rho0, float *errors0, INT4 *ifail)
{
double *xx, *yy, *ff, sigx, sigy, xc, yc, rho, *errors;
int i, status;

// Allocate memory and transfert to double arrays:
xx = (double *)malloc(*npts * sizeof(double)); 
yy = (double *)malloc(*npts * sizeof(double)); 
ff = (double *)malloc(*npts * sizeof(double)); 
errors = (double *)malloc(4 * sizeof(double)); 

for(i = 0; i < *npts; i++) {
  xx[i] = xx0[i];
  yy[i] = yy0[i];
  ff[i] = ff0[i];
  }

status = jlp_fit_gauss(xx, yy, ff, npts, &sigx, &sigy, &xc, &yc, &rho, 
                       errors, ifail);

// Transfert to float parameters:
*sigx0 = sigx;
*sigy0 = sigy;
*xc0 = xc;
*yc0 = yc;
*rho0 = rho;
for(i = 0; i < 4; i++) errors0[i] = errors[i];


// Free memory 
free(xx);
free(yy);
free(ff);
free(errors);
return(status);
}
/*****************************************************************
* To fit a Gaussian to f1(xx,yy)
*  rad2 = SQUARE(((float)i - xc) / sigx) + SQUARE (((float)j - yc) / sigy);
*  f(i,j) = rho * exp(-rad2);
*
* Integral of rho exp(- (x - xc)^2 /(2 sigx^2) = rho sigx \sqrt( 2 pi)
*
* Hence integral of rho exp(- (x - xc)^2 /(2 sigx^2) - (y -yc)^2 / 2 sigy^2) 
*                   = rho * 2 * pi * sigx * sigy
*
* 
*
* f1: array of values f1(xx[i],yy[i])
* xx, yy: arrays of coordinates x, y
* npts: number of points
*
* Parameters: 
* sigx, sigy, xc, yc, rho
* errors[4] : errors[sigx,sigy,xc,yc,rho]
*
* ifail = 0 if correct
*         -3: all values are negative or null!
*****************************************************************/
int jlp_fit_gauss(double *xx, double *yy, double *f1, INT4 *npts,
                  double *sigx, double *sigy, double *xc, double *yc,
                  double *rho, double *errors, INT4 *ifail)
{
/* f1 measured intensities */
/* log_f1 log of measured intensities */
double *log_f1, *xx1, *yy1;
double *aa, *psi, phi[NCOEFF], err_phi[NCOEFF];
double sumsq, mean, min, max, xm, ym, xs, ys, npoints;
INT4 kmax, ncoeff, status, npts_positive;
INT4 nx_aa, ny_aa;
double wx, wy;
int i, k;
#ifdef DEBUG
FILE *fp1;
#endif

*ifail = 0;

#ifdef DEBUG
/* Open logfile: */
if ((fp1 = fopen("fit_gauss.log","w")) == NULL)
  {
  printf("fit_gauss/Fatal error, cannot open logfile fit_gauss.log \n");
  exit(-1);
  }
#endif

/* Transfer to double precision arrays and conversion of intensity to Log : */
i = 0;
mean = 0.;
min = 1.E+12;
max = -1.;
for(k = 0; k < *npts; k++)
  {
       if(f1[k] > MIN_VALUE_FOR_LOG) 
         {
          if(min > f1[k]) min = f1[k];
          if(max < f1[k]) max = f1[k];
          mean += f1[k];
          i++;
         }
  }
if(i <= 10) {
  printf("jlp_fit_gauss/Error too few (positive) points for the fit: npts=%d\n", i);
  *ifail = -3; 
  return(-1);
  }

npts_positive = i; 
npoints = (double)(npts_positive);
mean /= npoints;
#ifdef DEBUG
printf(" %d positive pixels, mean =%f min=%f max=%f\n",
        npts_positive,mean,min,max);
#endif
 if((log_f1 = (double *) malloc(npts_positive * sizeof(double))) == NULL ||
    (aa = (double *) malloc(NCOEFF * npts_positive * sizeof(double))) == NULL ||
    (psi = (double *) malloc(npts_positive * sizeof(double))) == NULL ||
    (xx1 = (double *) malloc(npts_positive * sizeof(double))) == NULL ||
    (yy1 = (double *) malloc(npts_positive * sizeof(double))) == NULL)
  {
  printf("jlp_fit_gauss/Error allocating memory for array (npts=%d)\n",
          npts_positive);
  *ifail = -1;
  return(-1);
  }



/* Normalisation to the mean value, otherwise problems... */
i = 0;
min = 1.E+12;
max = -1.E+12;
sumsq = 0.;
xm = 0.; ym = 0.; xs = 0.; ys = 0.;
for(k = 0; k < *npts; k++)
  {
       if(f1[k] > MIN_VALUE_FOR_LOG) 
         {
          log_f1[i] = log((double)f1[k]/mean); 
          if(min > log_f1[i]) min = log_f1[i];
          if(max < log_f1[i]) max = log_f1[i];
          sumsq += log_f1[i]*log_f1[i];
          xx1[i] = xx[k]; 
          xm += xx1[i];
          xs += xx1[i] * xx1[i];
          yy1[i] = yy[k]; 
          ym += yy1[i];
          ys += yy1[i] * yy1[i];
          i++;
         }
  }
sumsq /= npoints;
xm /= npoints;
ym /= npoints;
xs /= npoints;
ys /= npoints;
xs -= xm*xm;
ys -= ym*ym;
#ifdef DEBUG
printf(" Min=%f max=%f sumsq=%f xm(ean)=%.3f ym(ean)=%.3f xs(igma)=%.2f ys(igma)=%.2f\n",
         min,max,sumsq,xm,ym,xs,ys);
#endif

/* For order=KMAXI = 2,    1 x y x^2 y^2  */
kmax = 2; ncoeff = 6;
/* (but force coeff in ix * iy to be null if FIVE_COEFF) */

/* (force coeff in ix * iy to be null) */
nx_aa = 5; ny_aa = npts_positive;

/* Compute matrix A */ 
compute_aa(aa,xx1,yy1,nx_aa,ny_aa);

compute_psi(psi,log_f1,ny_aa);

/* Initial guess : */
for(i = 0; i < ncoeff; i++) phi[i] = 0.;
phi[0] = max -xm*xm/xs -ym*ym/ys;
phi[1] = 2*xm/xs;
phi[2] = 2*ym/ys;
phi[3] = -1./xs;
phi[4] = -1./ys;
#ifdef DEBUG
printf("initial guess: %f %f %f %f %f \n",phi[0],phi[1],phi[2],phi[3],phi[4]);
#endif
#if 1
status = JLP_CGRAD(aa,psi,phi,&nx_aa,&ny_aa,ifail);
#else
ipositiv = 0;
status = JLP_FSTEP(aa,psi,phi,&nx_aa,&ny_aa,&ipositiv);
#endif

/* (since coeff in ix * iy were constrained to be null) */
phi[5] = phi[4]; phi[4] = 0.;

#ifdef DEBUG
  printf("x solution sorted as :  1 x y x2 xy y2\n");
  fprintf(fp1,"x solution sorted as :  1 x y x2 xy y2\n");
  for (i = 0; i < ncoeff; i++)
      {
      if( (i % 3) == 0) {printf("\n"); fprintf(fp1,"\n");}
      printf(" phi[%d] = %10.4g ",i,phi[i]);
      fprintf(fp1," phi[%d] = %10.4g ",i,phi[i]);
      }

   printf("\n\n");
   fprintf(fp1,"\n\n");

/* Display in "c" format: */
   printf(" Log_intensity =%10.4g +%10.4g * xx +%10.4g * yy +%10.4g * xx^2 \n",
            phi[0],phi[1],phi[2],phi[3]); 
   printf("  +%10.4g * xx * yy +%10.4g * yy^2 \n",phi[4],phi[5]); 
   fprintf(fp1," Log_intensity =%10.4g +%10.4g * xx +%10.4g * yy +%10.4g * xx * xx \n",
            phi[0],phi[1],phi[2],phi[3]); 
   fprintf(fp1,"  +%10.4g * xx * yy +%10.4g * yy^2 \n",phi[4],phi[5]); 

   fprintf(fp1,"x, y, Log_Intensity, Computed_Gaussian, residual \n");
#endif

/* Output residuals: */
    sumsq = 0.;
    for(i = 0; i < npts_positive; i++)
        {
        calpoly_0(xx1[i],yy1[i],phi,ncoeff,kmax,&wx);
        wy = wx - log_f1[i];
        sumsq += wy*wy;
#ifdef DEBUG
        fprintf(fp1,"%9.3g %9.3g %10.4g  %10.4g %10.4g\n",
                    xx1[i],yy1[i],log_f1[i],wx,wy);
#endif
        }
#ifdef DEBUG
printf("jlp_fit_gauss/mean error rms: %10.4g\n",sqrt(sumsq/npoints));
#endif

compute_variance(aa,err_phi,nx_aa,ny_aa);

/* (since has forced coeff in ix * iy to be null) */
err_phi[5] = err_phi[4]; err_phi[4] = 0;

/* Conversion to physical parameters: */
/* phi[0] < 0, phi[3] < 0, phi[5] < 0 */
*sigx = -1.0/phi[3];
*sigy = -1.0/phi[5];
*xc = - phi[1] / (2. * phi[3]);
*yc = - phi[2] / (2. * phi[5]);
*rho = exp((double)(phi[0] + (*xc) * (*xc) / (*sigx) + (*yc) * (*yc) / (*sigy)));
*rho *= mean;
 if(*sigx >= 0) *sigx = sqrt((double)(*sigx)); 
 else *sigx = -sqrt((double)(-(*sigx))); 
 if(*sigy >= 0) *sigy = sqrt((double)(*sigy)); 
 else *sigy = -sqrt((double)(-(*sigy))); 

/* errors[sigx,sigy,xc,yc] */
for(i = 0; i < 4; i++) errors[i] = 0.;

/* if a=f(b) :  sigma_a**2 = sigma_b**2 f'(b)**2 */ 
/* Hence if a=1/sqrt(b)  sigma_a**2 = 1/4 * sigma_b**2 / b^{3/2}**2 */
/* Warning: err_phi is a variance=sigma**2 ... (hence do not put a **2...)*/
  if(phi[3] != 0.) 
    {
    errors[0] = 0.25 * err_phi[3] / pow((double)(phi[3]*phi[3]),1.5);  
    errors[0] = sqrt((double)errors[0]);
    }
  if(phi[5] != 0.) 
    {
    errors[1] = 0.25 * err_phi[5] / pow((double)(phi[3]*phi[3]),1.5);  
    errors[1] = sqrt((double)errors[1]);
    }

/* if a=f(b,c) : sigma_a**2 = sigma_b**2 df/db(b)**2 + sigma_c**2 df/dc(c)**2 */ 
/* if a = b/c :  sigma_a**2 = sigma_b**2 / c**2  + sigma_c**2 * b**2 / c**4 */ 
  if(phi[3] != 0.)
  {
  errors[2] = err_phi[1] / (phi[3]*phi[3]) 
             + err_phi[3] * (phi[1]*phi[1]) / pow((double)phi[3],4.);
  errors[2] = sqrt((double)errors[2]);
  }
  if(phi[5] != 0.)
  {
  errors[3] = err_phi[2] / (phi[5]*phi[5]) 
             + err_phi[5] * (phi[2]*phi[2]) / pow((double)phi[5],4.);
  errors[3] = sqrt((double)errors[3]);
  }

#ifdef DEBUG
fclose(fp1);
printf(" Output in fit_gauss.log \n");
#endif

free(log_f1);
free(aa);
free(psi);
free(xx1);
free(yy1);

return(0);
}

/*********************************************************************
* To compute the variance on the coefficients 
* (sigma**2)
*
*********************************************************************/
static int compute_variance(double *aa, double *err_phi, INT4 nx_aa, INT4 ny_aa)
{
double ata[NCOEFF*NCOEFF];
int i;

compute_ata(ata,aa,nx_aa,ny_aa);

#ifdef DEBUG
 printf("compute_error/nx_aa = %d ny_aa = %d \n",nx_aa,ny_aa);
#endif
mat_lu_inverse(ata,ata,nx_aa);

#ifdef DEBUGG
printf(" Inverse matrix: \n");
mat_printf(ata,nx_aa,ny_aa);
#endif

for(i = 0; i < nx_aa; i++) err_phi[i] = ata[i + i * nx_aa];

#ifdef DEBUGG
printf(" Error vector: \n");
mat_printf(err_phi,nx_aa,1);
#endif

return(0);
}
/*********************************************************************
* To compute A* A 
*
*********************************************************************/
static int compute_ata(double *ata, double *aa, INT4 nx_aa, INT4 ny_aa)
{
int i, j, k;

for(j = 0; j < nx_aa; j++)
   {
   for(i = 0; i < nx_aa; i++)
      {
      ata[i + j*nx_aa] = 0.;
          for(k = 0; k < ny_aa; k++)
          {
           ata[i + j*nx_aa] += aa[j + k*nx_aa] * aa[i + k*nx_aa];
          }
      }
   }
return(0);
}
/*********************************************************************
* To compute the sums necessary for the matrix of coeeficients
* (xx,yy) measured coordinates
*
*********************************************************************/
static int compute_sum(double sum[2*KMAXI+1][2*KMAXI+1], INT4 kmax,
                       double *xx, double *yy, INT4 npts)
{
double x[2*KMAXI+1], y[2*KMAXI+1];
int i, j, k;
int kmax_sum;

kmax_sum = kmax * 2;

for(j = 0; j <= kmax_sum; j++)
  for(i = 0; i <= kmax_sum; i++)
       sum[i][j] = 0.;

for(k = 0; k < npts; k++)
  {
/* Examples: 
   1 x y x2 xy y2 x3 x2y xy2 y3
    sum[0][0] += 1 ;
    sum[1][0] += xx[k];
    sum[0][3] += yy[k]*yy[k]*yy[k];
    sum[3][1] += xx[k]*xx[k]*xx[k]*yy[k];
*/

/* First compute successive powers of xx[k] and yy[k]: */
    x[0] = 1.; y[0] = 1.;
    for(i = 1; i <= kmax_sum; i++) x[i] = x[i-1] * xx[k];
    for(j = 1; j <= kmax_sum; j++) y[j] = y[j-1] * yy[k];

/* Then compute the sums: */
    for(j = 0; j <= kmax_sum; j++)
      for(i = 0; i <= kmax_sum; i++)
          sum[i][j] = sum[i][j] + x[i] * y[j];

  }

/* Debug (but INT4...)
    for(j = 0; j <= kmax_sum; j++)
      for(i = 0; i <= kmax_sum; i++)
          printf(" sum[%d][%d]=%f \n",i,j,sum[i][j]);
*/

return(0);
}
/*********************************************************************
* To compute the second member of the normal equation
*
* zz = log_f1
*********************************************************************/
static int compute_psi(double *psi, double *zz, INT4 npts)
{
int j;

for(j = 0; j < npts; j++)
  {
  psi[j] = zz[j];
  }

return(0);
}
/*********************************************************************
* To compute the elements of matrix aa
* Sorted starting with lowest order, with x order changing more
* quickly than y's.
*********************************************************************/
static int compute_aa(double *aa, double *xx1, double *yy1,
                      INT4 nx_aa, INT4 ny_aa)
{
int j;

/* Loop on the lines */
   for(j = 0; j < ny_aa; j ++)
   {
      aa[0 + j * nx_aa] = 1;  
      aa[1 + j * nx_aa] = xx1[j];  
      aa[2 + j * nx_aa] = yy1[j];  
      aa[3 + j * nx_aa] = xx1[j]*xx1[j];  
      aa[4 + j * nx_aa] = yy1[j]*yy1[j];  
    }

return(0);
}
/*****************************************************************
*
* Compute the value given by the solution polynomial
* (to derive the residuals)
* sorted as :  1 x y x2 xy y2 x3 x2y xy2 y3
*****************************************************************/
int calpoly_0(double xx, double yy, double *phi, INT4 ncoeff, INT4 kmax,
              double *value)
{
int i, j, k, m;
double sum, x[KMAXI+1], y[KMAXI+1];

x[0] = 1.;
y[0] = 1.;
for(i = 1; i <= kmax; i++) x[i] = x[i-1] * xx;
for(i = 1; i <= kmax; i++) y[i] = y[i-1] * yy;

sum = 0.;
/* sorted as :  1 x y x2 xy y2 x3 x2y xy2 y3
*/
k = 0;
for(m = 0; m <= kmax && k < ncoeff; m++)
  {
    for(j = 0; j <= m && k < ncoeff; j++)
         {
/* Filling line #n1 */
         i = m - j;
         sum += x[i] * y[j] * phi[k];
         k++;
         }
    }

*value = sum;

return(0);
}
