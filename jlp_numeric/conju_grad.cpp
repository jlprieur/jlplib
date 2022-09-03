/******************************************************
* conju_grad.c
*
* JLP
* Version 05-06-02
******************************************************/
#include "jlp_numeric.h" 

int jlp_eigen_values(double *bb, int nx, double *eigen_min, double *eigen_max);

/*
#define DEBUG 1
*/

#define NITERMAX 40
#define NITER_FSTEP 50000
/* JLP98: warning: e-5 is enough, otherwise, it goes too far... */ 
#define EPSILON 1.e-5 
/* main program to test the routine 
 Generate an array with Gaussian law
 and then find the parameters of the Gaussian 
 */
#ifdef MAIN_PROG
#define NX 2
#define NY 4
/*
 We examine the problem: psi = aa phi
 We suppose that the solution is: phi = (x=1;  y=2)
    1x + 2y = 5.1 
    4x + 5y = 14.5
   -1x + 2y = 2.9 
   -2x + 1y = 0.1
*/


int main(int argc, char *argv[])
{
INT4 nx, ny, ifail;
int status, i, j, ipositiv;
double aa[NX * NY], phi[NX], psi[NY];
float randd, cst;

nx = NX;
ny = NY;

/* JLP_RANDOM_GAUSS(&randd); */

    psi[0] = 5.1; 
    psi[1] = 14.5; 
    psi[2] = 2.9; 
    psi[3] = 0.1; 

#ifdef DEBUG
  for (j = 0; j < ny; j++) printf(" psi[%d] = %12.5e \n",j,psi[j]);
#endif

/* Matrix: */
    aa[0 + 0 * nx] = 1.; aa[1 + 0 * nx] = 2.;
    aa[0 + 1 * nx] = 4.; aa[1 + 1 * nx] = 5.;
    aa[0 + 2 * nx] = -1.; aa[1 + 2 * nx] = 2.;
    aa[0 + 3 * nx] = -2.; aa[1 + 3 * nx] = 1.;
printf(" Solving   aa phi = psi \n");
printf(" with aa: \n");
printf(" 1 2 \n 4 5 \n -1 2 \n -2 1 \n");
printf(" \n phi solution should be around (1,2) \n");

/* First guess for phi */
printf(" Enter initial guess for phi (one value only) := ");
scanf("%f",&cst);
printf("Initial guess: phi=(%f,%f) \n",cst,cst);
for (i = 0; i < nx; i++) phi[i] = cst;

status = JLP_CGRAD(aa,psi,phi,&nx,&ny,&ifail);
printf("JLP_CGRAD/Solution: phi=(%f,%f) \n",phi[0],phi[1]);


/* First guess for phi */
for (i = 0; i < nx; i++) phi[i] = 0.;
phi[0]=1.; phi[1]=2.;

/* Flag set to one if positivity constraint is wanted */
ipositiv=0;
status = JLP_FSTEP(aa,psi,phi,&nx,&ny,&ipositiv);
printf("JLP_FSTEP/Solution: phi=(%f,%f) \n",phi[0],phi[1]);

return(0);
}
#endif
/****************************************************************
* jlp_conjugate_gradients : C interface to JLP_CGRAD
* CONJUGATE GRADIENTS to solve the normal equation of a squares problem
* with conjugate gradients
*
* The basic problem is to solve
* psi = aa phi
* with mean square minimization in E+ (orthogonal to ker(aa) ).
* Thus we have to minimize:                psi = aa phi
* with least squares, i.e. to solve:    aa* psi = aa* aa phi
*
* aa is a continuous operator from E to F
* dim(E) = nx1
* dim(F) = ny1
* thus aa has ny1 lines (i=0,...,nx1 index) and nx1 columns (j=0,...,ny1 index)
*      phi is the solution vector of nx1 components
*      psi is the data vector of ny1 components
*****************************************************************/
int jlp_conjugate_gradients(double *aa, double *psi, double *phi, int nx1, 
                            int ny1)
{
int status;
INT4 ifail;

//int JLP_CGRAD(double *aa, double *psi, double *phi, INT4 *nx1, INT4 *ny1,
//              INT4 *ifail)
  status = JLP_CGRAD(aa, psi, phi, &nx1, &ny1, &ifail);

return(status);
}
/*************************************************************************
* CONJUGATE GRADIENTS
*
* The basic problem is to solve
* psi = aa phi
* with mean square minimization in E+ (orthogonal to ker(aa) ).
* Thus we have to minimize:                psi = aa phi
* with least squares, i.e. to solve:    aa* psi = aa*  aa phi
*
* aa is a continuous operator from E to F
* dim(E) = nx
* dim(F) = ny
* thus aa has ny lines (j index) and nx columns (i index)
*      phi is a vector of nx components
*      psi is a vector of ny components
*
* JLP2016:
* phi should be initialized as a first guess before calling this routine !!!
****************************************************************************/
int JLP_CGRAD(double *aa, double *psi, double *phi, INT4 *nx1, INT4 *ny1,
              INT4 *ifail)
{
int nx, ny;
register int i, j, n_iter;
/*
double phi_n[NX], phi_n1[NX], res_n[NX], res_n1[NX];
double z_n[NX], d_n[NX], d_n1[NX], work[NY], w1[NY], w2[NY];
*/
double *phi_n, *phi_n1, *res_n, *res_n1;
double *z_n, *d_n, *d_n1, *work, *w1, *w2;
double w_n, work0, norm_res_n, norm_res_n1, norm_phi_n1;
double error, gamma_n, gamma_0, norm_res_0;

nx = *nx1; ny = *ny1;

/* Get memory space: */
if((phi_n = (double *)malloc(nx * sizeof(double))) == NULL
|| (phi_n1 = (double *)malloc(nx * sizeof(double))) == NULL
|| (res_n = (double *)malloc(nx * sizeof(double))) == NULL
|| (res_n1 = (double *)malloc(nx * sizeof(double))) == NULL
|| (z_n = (double *)malloc(nx * sizeof(double))) == NULL
|| (d_n = (double *)malloc(nx * sizeof(double))) == NULL
|| (d_n1 = (double *)malloc(nx * sizeof(double))) == NULL
|| (work = (double *)malloc(ny * sizeof(double))) == NULL
|| (w1 = (double *)malloc(ny * sizeof(double))) == NULL
|| (w2 = (double *)malloc(ny * sizeof(double))) == NULL) {
  printf(" Fatal error allocating memory space with: nx=%d ny=%d\n", nx, ny);
  exit(-1);
  }
/**************** FIRST STEP ************************/
/* First step, first guess for phi */
for (i = 0; i < nx; i++) phi_n[i] = phi[i];

/**************** SECOND STEP ************************/
/* Compute the residuals: 
        res_n = aa* ( psi - aa phi_n)
*/
for (j = 0; j < ny; j++)
  {
  work[j] = psi[j];
  for (i = 0; i < nx; i++)
    work[j] -= aa[i + j * nx] * phi_n[i];
}
/*
Now multiplying with aa* (aa^T conjugate)
*/
for (i = 0; i < nx; i++)
  {
  res_n[i] = 0.;
  for (j = 0; j < ny; j++)
    res_n[i] += aa[i + j * nx] * work[j];
}
/* Transfer to d_n: */
  for (i = 0; i < nx; i++) d_n[i] = res_n[i];

norm_res_0 = 0.;
for (i = 0; i < nx; i++)
    norm_res_0 += res_n[i] * res_n[i];

#ifdef DEBUG
  for (i = 0; i < nx; i++) 
      {
      printf(" Initial guess: phi_0[%d] = %12.5e \n",i,phi_n[i]);
      printf(" res_0[%d] = %12.5e \n",i,res_n[i]);
      }
#endif

/**************** MAIN LOOP ************************/

for(n_iter = 0; n_iter < NITERMAX; n_iter++)
{

/**************** THIRD STEP ************************/
/* Compute zn = aa* aa dn
*/
/*
Multiplying with aa: 
*/
for (j = 0; j < ny; j++)
  {
  work[j] = 0.;
  for (i = 0; i < nx; i++)
    work[j] += aa[i + j * nx] * d_n[i];
}

/*
Now multiplying with aa* (aa conjugate)
*/
for (i = 0; i < nx; i++)
  {
  z_n[i] = 0;
  for (j = 0; j < ny; j++)
    z_n[i] += aa[i + j * nx] * work[j];
}

/**************** FOURTH STEP ************************/

/* Compute w_n
*/

/* Computing the squared norm of res_n */
norm_res_n = 0.;
for (i = 0; i < nx; i++)
    norm_res_n += res_n[i] * res_n[i];

/* Computing the scalar product: (d_n | z_n )  */
work0 = 0.;
for (i = 0; i < nx; i++)
    work0 += d_n[i] * z_n[i];

/* Now w_n: */
if(work0 != 0) 
 {
  w_n = norm_res_n / work0;
 }
 else 
 { w_n = 1.E+15; 
   printf("jlp_cgrad/error, work0 is null! (norm_res_n=%f) \n",
   norm_res_n);}

#ifdef DEBUG
  printf("  w_n = %12.5e (...step along conjugate direction)\n", w_n);
#endif

/* Compute phi_n1 (i.e. phi_n+1) */
for (i = 0; i < nx; i++)
    phi_n1[i] = phi_n[i] + w_n * d_n[i];

#ifdef DEBUG
  for (i = 0; i < nx; i++) 
      printf(" New solution: phi_n1[%d] = %12.5e \n",i,phi_n1[i]);
#endif

/* Compute res_n1 (i.e. res_n+1) */
for (i = 0; i < nx; i++)
    res_n1[i] = res_n[i] - w_n * z_n[i];

/******* Testing end of loop ************************/

/* Computing the squared norm of res_n1 */
norm_res_n1 = 0.;
for (i = 0; i < nx; i++)
    norm_res_n1 += res_n1[i] * res_n1[i];

/* Computing the squared norm of phi_n1 */
norm_phi_n1 = 0.;
for (i = 0; i < nx; i++)
    norm_phi_n1 += phi_n1[i] * phi_n1[i];

/* Test */
if(norm_phi_n1 != 0.)
error = sqrt((double)(norm_res_n1 / norm_phi_n1));
else
{ printf("jlp_cgrad/error, norm of phi_n+1 is null ! \n");
  error = 1.E+15;}

#ifdef DEBUG
 if(i == 2)
    printf(" iteration #%d error = %12.5e phi = %12.5e %12.5e \n", 
       n_iter, error, phi_n1[0],phi_n1[1]);
 else
    printf(" iteration #%d error = %12.5e \n", n_iter, error);
#endif

if(error > EPSILON)
{
  gamma_n = norm_res_n1 / norm_res_n;
  gamma_0 = norm_res_n1 / norm_res_0;
#ifdef DEBUG
  printf(" res(n+1)/res(n)=%12.5e  (n+1)/res(0)=%12.5e error=%12.5e\n",
           gamma_n,gamma_0,error); 
#endif
}
else
{
#ifdef DEBUG
printf("Conju_grad/normal exit: iteration= %d error = %12.5e \n",
        n_iter, error);
#endif
*ifail = 0;
break;
}

/* Computing d_n1: */
  for (i = 0; i < nx; i++) d_n1[i] = res_n1[i] + gamma_n * d_n[i];

#ifdef DEBUG
/* Checking if directions are conjugate: */
/* Computing   aa d_n  
   and         aa d_n+1
*/
/*
Multiplying d_n with aa: 
*/
for (j = 0; j < ny; j++)
  {
  w1[j] = 0.;
  for (i = 0; i < nx; i++)
    w1[j] += aa[i + j * nx] * d_n[i];
  }
/*
Multiplying d_n+1 with aa: 
*/
for (j = 0; j < ny; j++)
  {
  w2[j] = 0.;
  for (i = 0; i < nx; i++)
    w2[j] += aa[i + j * nx] * d_n1[i];
  }

/* Computing the scalar product: (aa d_n | aa d_n1 )  */
work0 = 0.;
for (j = 0; j < ny; j++)
    work0 += w1[j] * w2[j];

printf(" (aa d_n | aa d_n+1) = %12.5e (...should be zero)\n",work0);
/* Compare it to (aa d_n): */
work0 = 0.;
for (j = 0; j < ny; j++)
    work0 += w1[j] * w1[j];
printf(" or at least small relative to (aa d_n | aa d_n) = %12.5e\n",work0);

#endif

/* Goto begin of loop (STEP 3)
   and copy from n+1 to n (new become old)*/
  for (i = 0; i < nx; i++) 
   {
   d_n[i] = d_n1[i];
   phi_n[i] = phi_n1[i];
   res_n[i] = res_n1[i];
   }
}

/* End of loop */

if(n_iter >= NITERMAX)
 {
 printf(" jlp_cgrad/ Warning: residuals larger than expected \n");  
 printf(" error = %12.5e \n", error);
 *ifail = 2;
 }

/* Transfer to phi : */
  for (i = 0; i < nx; i++) phi[i] = phi_n1[i];

/* Free memory space: */
free(phi_n);
free(phi_n1);
free(res_n);
free(res_n1);
free(z_n);
free(d_n);
free(d_n1);
free(work);
free(w1);
free(w2);

return(0);
}
/******************************************************
* GRADIENT METHOD WITH FIXED STEP
* The basic problem is to solve
* psi = aa phi
* with mean square minimization in E+ (orthogonal to ker(aa) ).
* Thus we have to minimize:                psi = aa phi
* with least squares, i.e. to solve:    aa* psi = aa*  aa phi
*
* aa is a continuous operator from E to F
* dim(E) = nx
* dim(F) = ny
* thus aa has ny lines (j index) and nx columns (i index)
*      phi is a vector of nx components
*      psi is a vector of ny components
* Flag set to one if positivity constraint is wanted : ipositiv=1;
*******************************************************/
int JLP_FSTEP(double *aa, double *psi, double *phi, INT4 *nx1, INT4 *ny1,
              INT4 *ipositiv1)
{
int nx, ny, ipositiv;
int i, j, k, n_iter, status, isize;
/*
double res_n[NX], work[NY], ata[NX*NX];
*/
double *res_n, *work, *ata;
double norm_res_n, norm_phi_n, step;
double error, ww, eigen_min, eigen_max;

nx = *nx1; ny = *ny1; ipositiv = *ipositiv1;

/* Get memory space: */
isize = nx * sizeof(double);
res_n = (double *)malloc(isize);
isize = ny * sizeof(double);
work = (double *)malloc(isize);
isize = nx * nx * sizeof(double);
ata = (double *)malloc(isize);

/***** PRELIMINARIES: compute ata = aa* aa ************/ 
/*
Term i,j of ata is sum over k of a*(k,j)a(i,k)
     or sum over k of a(j,k)a(i,k)
*/
for (j = 0; j < nx; j++)
  {
  for (i = 0; i < nx; i++)
    {
    ww = 0.;
    for (k = 0; k < ny; k++) ww = ww + aa[j + k * nx] * aa[i + k * nx];
    ata[i + j * nx] = ww;
    }
}

status = jlp_eigen_values(ata,nx,&eigen_min,&eigen_max);

/* Optimum value for step is: (2/(eigen_min+eigen_max))*/
step = 2. / (eigen_min + eigen_max);
printf("jlp_fstep/Optimum value for step is %12.5e (condition number=%12.5e)\n",        step,(eigen_max/eigen_min));

/**************** FIRST STEP ************************/
/* Compute the residuals (for n=0): 
        res_n = aa* ( psi - aa phi_n)

First multiplying with aa:
*/
for (j = 0; j < ny; j++)
  {
  work[j] = psi[j];
  for (i = 0; i < nx; i++)
    work[j] = work[j] - aa[i + j * nx] * phi[i];
}
/*
Now multiplying with aa* (aa conjugate):
*/
for (i = 0; i < nx; i++)
  {
  res_n[i] = 0.;
  for (j = 0; j < ny; j++)
    res_n[i] = res_n[i] + aa[i + j * nx] * work[j];
}

#ifdef DEBUG
  for (i = 0; i < nx; i++) 
      {
      printf(" res_0[%d] = %12.5e \n",i,res_n[i]);
      }
#endif


/**************** MAIN LOOP ************************/

for(n_iter = 0; n_iter < NITER_FSTEP; n_iter++)
{

/* Compute phi_n */
for (i = 0; i < nx; i++)
    phi[i] = phi[i] + step * res_n[i];

/* Apply positivity constraint if needed: */
if(ipositiv == 1)
  {
  for (i = 0; i < nx; i++)
    if(phi[i] < 0.) phi[i] = 0.;
  }

#ifdef DEBUG
  for (i = 0; i < nx; i++) 
      printf(" New solution: phi[%d] = %12.5e \n",i,phi[i]);
#endif

/******* Testing end of loop ************************/

/* Computing the squared norm of res_n */
norm_res_n = 0.;
for (i = 0; i < nx; i++)
    norm_res_n = norm_res_n + res_n[i] * res_n[i];

/* Computing the squared norm of phi_n */
norm_phi_n = 0.;
for (i = 0; i < nx; i++)
    norm_phi_n = norm_phi_n + phi[i] * phi[i];

/* Test */
if(norm_phi_n != 0)
error = sqrt((double)(norm_res_n / norm_phi_n));
else
{ printf("jlp_fstep/error, norm of phi_n is null ! \n");
  error = 1.E+15;}

#ifdef DEBUG
 if(i == 2)
    printf(" iteration #%d error = %12.5e phi = %12.5e %12.5e \n", 
       n_iter, error, phi[0],phi[1]);
 else
    printf(" iteration #%d error = %12.5e \n", n_iter, error);
#endif

if(error < EPSILON)
{
printf("jlp_fstep/Normal exit: error = %12.5e \n", error);
break;
}

/* Compute new residuals: 
       r_n+1 = r_n ( I - step aa* aa)
   i.e., 
       r_n+1 = r_n  - step ata r_n
*/
/* WRONG FOR SOME UNKNOWN REASON: ...
for (k = 0; k < nx; k++)
  {
  ww = 0.;
  for (i = 0; i < nx; i++) ww = ww + ata[i + k * nx] * res_n[i];
  res_n[k] = res_n[k] - step * ww;
}
*/
/* Compute the residuals (for n=0):
        res_n = aa* ( psi - aa phi_n)

First multiplying with aa:
 SO I USE THE FOLLOWING
*/
for (j = 0; j < ny; j++)
  {
  work[j] = psi[j];
  for (i = 0; i < nx; i++)
    work[j] = work[j] - aa[i + j * nx] * phi[i];
}
/*
Now multiplying with aa* (aa conjugate):
*/
for (i = 0; i < nx; i++)
  {
  res_n[i] = 0.;
  for (j = 0; j < ny; j++)
    res_n[i] = res_n[i] + aa[i + j * nx] * work[j];
}
/* end */


/* Goto begin of loop */
}

/* End of loop */

if(n_iter >= NITER_FSTEP)
 {
 printf(" jlp_fstep/ Warning: residuals larger than expected \n");  
 printf(" error = %12.5e \n", error);
 }

/* Ok */
return(0);
}
/***************************************************************
* Compute min and max eigen values of a self-adjoint operator
* (Power method)
*
***************************************************************/
int jlp_eigen_values(double *bb, int nx, double *eigen_min, double *eigen_max)
{
register int i, j;
int isize, n_iter;
double norm, old_norm, ww, *xx, *yy;

/* Get memory space: */
isize = nx * sizeof(double);
xx = (double *)malloc(isize);
yy = (double *)malloc(isize);

/* Initial xx vector: */
for(i = 0; i < nx; i++) xx[i] = 1. / sqrt((double)nx);
old_norm = -1.;

/********************* MAIN LOOP FOR MAX EIGEN VALUE **********************/
for(n_iter = 0; n_iter < 30; n_iter++)
{
/* Compute yy = bb xx : */
norm = 0.;
for(i = 0; i < nx; i++) 
  {
  ww = 0.;
  for(j = 0; j < nx; j++) ww = ww + bb[j + i * nx ] * xx[j];
  norm = norm + ww * ww;
  yy[i] = ww;
  }
norm = sqrt((double)norm);

*eigen_max = norm;

/* Test to exit from the loop: */
if( (norm-old_norm) < 0.001) break;

printf("jlp_eigen_values/ iter#%d estimation of the norm=%12.5e \n",n_iter,
      norm);

/* Normalization: */
for(i = 0; i < nx; i++) xx[i] = yy[i] / norm;
old_norm = norm;
}

printf("jlp_eigen_values/ convergence in %d iterations \n",n_iter);

printf(" Maximum eigen_value is: %12.5e \n",*eigen_max);

/********************* MIN EIGEN VALUE **********************/
/* Same as previously, but with cc = (I - bb/eigen_max) */
/* Initial xx vector: */
for(i = 0; i < nx; i++) xx[i] = 1. / sqrt((double)nx);
old_norm = -1.;

/********************* MAIN LOOP FOR MIN EIGEN VALUE **********************/
for(n_iter = 0; n_iter < 30; n_iter++)
{
/* Compute yy = cc xx : */
norm = 0.;
for(i = 0; i < nx; i++) 
  {
  ww = 0.;
  for(j = 0; j < nx; j++) ww = ww + bb[j + i * nx ] * xx[j];
  ww = xx[i] - ww / *eigen_max;
  norm = norm + ww * ww;
  yy[i] = ww;
  }
norm = sqrt((double)norm);

*eigen_min = *eigen_max * (1. - norm);

/* Test to exit from the loop: */
if( (norm-old_norm) < 0.001) break;

printf("jlp_eigen_values/ iter#%d estimation of the norm=%12.5e \n",n_iter,
      norm);

/* Normalization: */
for(i = 0; i < nx; i++) xx[i] = yy[i] / norm;
old_norm = norm;
}

printf("jlp_eigen_values/ convergence in %d iterations \n",n_iter);

printf(" Minimum eigen_value is: %12.5e \n",*eigen_min);

return(0);
}
