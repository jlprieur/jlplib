/********************************************************************
* Set of routines that performs basic algebra on matrices
*
* lu_decomp(double *aa, int nn, int *indx, double *dd);
* lu_backsub(double *aa, int nn, int *indx, double *bb);
* mat_lu_solve(double *aa, double *xx, double *bb, int nn);
* mat_lu_inverse(double *aa, double *aa_inv, int nn);
* mat_product(double *out, double *in1, double *in2, int ncol1, int nlin1);
* mat_printf(double *aa, int ncol, int nlin);
*
* JLP
* Version 09/02/2007
********************************************************************/
#include <stdio.h>
#include <math.h>
#include <malloc.h>
#include "jlp_numeric.h"

#define TINY 1.0e-20;


/* #define MAIN_PROGRAM */
#ifdef MAIN_PROGRAM 
#define NMAX 4
main()
{
double ll[NMAX*NMAX], uu[NMAX*NMAX], aa[NMAX*NMAX], bb[NMAX], dd;
double aa_inv[NMAX*NMAX];
int nn, indx[NMAX];
register int i, j, k;
nn = 3;
for(j = 0; j < nn*nn; j++) {ll[j] = 0.; uu[j] = 0.; aa[j] = 0.;}

/* Lower matrix: (diagonal to ones ...)*/
#if 0
for(j = 0; j < nn; j++)
     ll[j + j * nn] = 1;
#else
for(j = 0; j < nn; j++)
  for(i = 0; i <= j; i++)
     ll[i + j * nn] = 1 + i - j;
#endif

/* Upper matrix: */
#if 0
for(j = 0; j < nn; j++)
     uu[j + j * nn] = 1;
#else
for(j = 0; j < nn; j++)
  for(i = j; i < nn; i++)
     uu[i + j * nn] = 1 + i + j;
#endif


/* Product: A = L*U */
mat_product(aa,ll,uu,nn,nn);

/* Output: */
printf(" Lower matrix: \n");
mat_printf(ll,nn,nn);
printf(" Upper matrix: \n");
mat_printf(uu,nn,nn);
printf(" Product matrix: \n");
mat_printf(aa,nn,nn);

/* Calling decomposition: */
lu_decomp(aa,nn,indx,&dd);

/* Determinant: */
for(j = 0; j < nn; j++) dd *= aa[j + j*nn];
printf("Determinant = %f \n",dd);

for(j = 0; j < nn; j++) bb[j] = 0.;
bb[1] = 1.;
printf(" Second member: \n");
mat_printf(bb,nn,1);

lu_backsub(aa, nn, indx, bb);
printf(" Solution: \n");
mat_printf(bb,nn,1);

/* Output: */
printf(" Decomposed 'LU' matrix: \n");
mat_printf(aa,nn,nn);

printf(" Index: \n");
for(j = 0; j < nn; j++)
    printf("%d ",indx[j]); 
  printf("\n"); 
/*******************************************************/
/* Check if correct: */
for(j = 0; j < nn; j++)
  for(i = 0; i < j; i++)
     ll[i + j * nn] = aa[i + j *nn];
/* Fill the diagonal with ones: */
for(j = 0; j < nn; j++) ll[j + j * nn] = 1.;

for(j = 0; j < nn; j++)
  for(i = j; i < nn; i++)
     uu[i + j * nn] = aa[i + j *nn];

/* Product: A = L*U */
mat_product(aa,ll,uu,nn,nn);
printf(" computed LU matrix from decomposition: \n");
mat_printf(aa,nn,nn);

/* Inverse of matrix: */
mat_lu_inverse(aa,aa_inv,nn);
printf(" Inverse matrix: \n");
mat_printf(aa_inv,nn,nn);
}
#endif
/****************************************************************************
* LU decomposition (Numerical recipes in C p43)
* Given an n*n matrix aa, this routines replaces it by the LU decomposition
* of a rowwise permutation of itself.
*
* INPUT:
*  aa[i + j * nn]
*  n
* OUTPUT:
*  aa[i + j * nn]
*  indx[nn]:  vector which records the row permutation effected by the partial
*            pivoting. 
*  dd: +/-1 depending on whether the number of row interchanges 
            was even (dd=+1) or odd (dd=-1) 
****************************************************************************/
int lu_decomp(double *aa, int nn, int *indx, double *dd)
{
register int i, j, k;
int jmax;
double big, dum, sum, temp;
double *vv;

vv = (double *) malloc(nn * sizeof(double));
if(!vv) {printf("lu_decomp/Fatal error allocating memory space!\n");
         return(-1);}

/* No row interchanges yet: */
*dd = 1.0;

/* Loop over rows to get the implicit scale information: */
for(j = 0; j < nn; j++)
  {
  big = 0.0;
    for(i = 0; i < nn; i++)
       if((temp = ABS(aa[i + j * nn])) > big) big = temp;
/* Largest element is null: */
  if(big == 0.) 
       {printf("lu_decomp/Fatal error: singular matrix in row %d\n",j);
        return(-2);
       }
/* Save the scaling: */
  vv[j] = 1.0/big;
  } 

/* This is the loop over columns of Crout's method: */
for(i = 0; i < nn; i++)
{
/* Equations: */
/* (12): beta_ij = a_ij - sum_{k=1}^{i-1} alpha_ik beta_kj */
/* (13): alpha_ij = (1/beta_jj) * (a_ij - sum_{k=1}^{i-1} alpha_ik beta_kj) */
/* This is equation (12) except for j=i: */
   for(j = 0; j < i; j++)
    {
    sum = aa[i + j*nn];
     for(k = 0; k < j; k++) sum -= aa[k + j*nn] * aa[i + k*nn];
    aa[i + j*nn] = sum;
    }
/* Initialize for the search for largest pivot element: */
    big = 0.0;
/* This is equation (12) for j=i  and equation (13) for j=i+1, ..., nn: */
   for(j = i; j < nn; j++)
    {
    sum = aa[i + j*nn];
     for(k = 0; k < i; k++) sum -= aa[k + j*nn] * aa[i + k*nn];
    aa[i + j*nn] = sum;
/* Is the figure of merit for the pivot better than the best so far? */
    if((dum = vv[j] * ABS(sum)) >= big)
       {
         big = dum;
         jmax = j;
       }
    }
#define INTERCHANGE
#ifdef INTERCHANGE
/* Do we need to interchange rows? */
    if( i != jmax)
     {
/* Interchange: */
        for(k = 0; k < nn; k++) 
        {
         dum = aa[k + jmax*nn];
         aa[k + jmax*nn] = aa[k + i*nn];
         aa[k + i*nn] = dum;
        }
/* Change the parity of dd: */
        *dd = -(*dd);
/* and interchange the scale factor: */
/* JLP: */
        dum = vv[jmax];
        vv[jmax] = vv[i];
        vv[i] = dum;
     }
     indx[i] = jmax;
#else
     indx[i] = i;
#endif
     if(aa[i + i*nn] == 0.0) aa[i + i*nn] = TINY;
/* Now finally divide by the pivot element: */
    if( i != nn-1)
     {
     dum = 1.0 / aa[i + i*nn];
/* If the pivot element is zero the matrix is singular (at least to the
* precision of the algorithm)
* For some applications on singular matrices it is desirable to substitute
* TINY for zero */
     for(j=i+1; j < nn; j++) aa[i + j*nn] *= dum;
     } 
/* Go back for the next column i in the reduction */
}
free(vv);
return(0);
}
/***********************************************************
* lu_backsub: routine for forward substitution and backsubstitution 
* (Numerical recipes p 44 (lubksb))
*
* Solves the set of linear equations aa * xx = bb
* INPUT:
* aa: actually LU decomposition of initial matrix aa
* indx: permutation vector returned by lu_decomp
* OUTPUT:
* Note that aa, bb, indx, are not modified by this routine and can be left
* in place for successive calls with different right-hand sides bb.
***********************************************************/
int lu_backsub(double *aa, int nn, int *indx, double *bb)
{
register int i, ii, ip, j;
double sum;

ii = -1;

/* When ii is set to a positive value, it will become the index
*  of the first nonvanishing element of bb.
* We now do the forward substitution: */
for(i = 0; i < nn; i++)
  {
  ip = indx[i];
  sum = bb[ip];
  bb[ip] = bb[i];
  if(ii >= 0)
     for(j = ii; j <= i-1; j++) sum -= aa[j + i*nn] * bb[j];
/* A nonzero element was encountered, so from now on 
* we will have to do the sums in the loop above: */ 
  else
     if(sum) ii = i;
  bb[i] = sum;
  }

/* Now we do the backsubstitution: */
for(i = nn-1; i >= 0; i--)
  {
  sum = bb[i];
  for(j = i+1; j < nn; j++) sum -= aa[j + i*nn] * bb[j]; 
  bb[i] = sum / aa[i + i*nn];
  }

return(0);
}
/***********************************************************
* Solve : aa * xx = bb 
* with aa[nn*nn]
* with lu decomposition
***********************************************************/
int mat_lu_solve(double *aa, double *xx, double *bb, int nn)
{
double *aa0, dd;
int *indx;
register int j;

/* Copy to xx, to prevent bb from being erased in lu_backsub */
for(j = 0; j < nn; j++) xx[j] = bb[j];

indx = (int *) malloc(nn * sizeof(int));
aa0 = (double *) malloc(nn * nn * sizeof(double));

if(indx == NULL || aa0 == NULL) 
    {printf("mat_lu_solve/Fatal error allocating memory space \n");
     return(-1);
    }

/* Copy to aa0, to prevent aa from being erased in lu_backsub */
for(j = 0; j < nn*nn; j++) aa0[j] = aa[j];

lu_decomp(aa0, nn, indx, &dd);
lu_backsub(aa0, nn, indx, xx);

free(indx);
free(aa0);
return(0);
}
/***********************************************************
* Compute inverse of matrix aa[nn*nn]
* with lu decomposition
* aa and aa_inv can be the same array (input/output) 
***********************************************************/
int mat_lu_inverse(double *aa, double *aa_inv, int nn)
{
double *bb, *aa0, dd;
int *indx;
register int i, j;

aa0 = (double *) malloc(nn * nn * sizeof(double));
indx = (int *) malloc(nn * sizeof(int));
bb = (double *) malloc(nn * sizeof(double));

if(aa0 == NULL || indx == NULL || bb == NULL) 
    {printf("mat_lu_inverse/Fatal error allocating memory space (nn=%d)\n",nn);
     return(-1);
    }

/* Copy to aa0, to prevent aa from being erased in lu_backsub */
for(j = 0; j < nn*nn; j++) aa0[j] = aa[j];

/* Decompose matrix in LU form */
lu_decomp(aa0, nn, indx, &dd);

/* Find inverse of columns */
for(j = 0; j < nn; j++) 
  {
  for(i = 0; i < nn; i++) bb[i] = 0.; 
  bb[j] = 1.0;
  lu_backsub(aa0, nn, indx, bb);
/* Save bb to column of aa_inv: */
  for(i = 0; i < nn; i++) aa_inv[j + i*nn] = bb[i]; 
  }

free(aa0);
free(indx);
free(bb);
return(0);
}
/***********************************************************
* Product: out = in1 * in2
* in1[ncol1, nlin1]
* in2[ncol2=nlin1, nlin2=ncol1]
***********************************************************/
int mat_product(double *out, double *in1, double *in2, int ncol1, int nlin1)
{
register int i, j, k;

for(j = 0; j < nlin1; j++)
  {
  for(i = 0; i < ncol1; i++)
    {
    out[i + j * ncol1] = 0.;
    for(k = 0; k < ncol1; k++)
     out[i + j * ncol1] += in1[k + j*ncol1] * in2[i + k*nlin1];  
    }
  }
return(0);
}
/***********************************************************
* Print to screen 
***********************************************************/
int mat_printf(double *aa, int ncol, int nlin)
{
register int i, j;

for(j = 0; j < nlin; j++)
  {
  for(i = 0; i < ncol; i++)
    printf("%.5g ",aa[i + j*ncol]); 
  printf("\n"); 
  }  
return(0);
}
