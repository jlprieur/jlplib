/**************************************************************** 
* Set of routines
* To recentre FFT
*
* Contains: 
*  RECENT_FFT(in,out,nx,ny,idim)
*  RECENT_FFT_DOUBLE(in,out,nx,ny,idim)
*  RECENT_FFT_1D_X(in,out,nx,ny,idim)
*  RECENT_FFT_1D_Y(in,out,nx,ny,idim)
*  RECENT_FFT_1D_X_float(in,out,nx,ny,idim)
*  RECENT_FFT_1D_Y_float(in,out,nx,ny,idim)
*  TO_SINGLE(in,out,nx,ny,idim)
*  TO_DOUBLE(in,out,nx,ny,idim)
*
* JLP
* Version 02-12-93
*****************************************************************/
#include "jlp_numeric.h" 

/************************************************************
* To recentre FFT 
* and switch from zero frequency at (0,0) to (nx/2,ny/2) or inversely
* Float version
*
************************************************************/
int RECENT_FFT(float *in, float *out, INT4 *nx, INT4 *ny, INT4 *idim)
{
float w1, w2, w3, w4;
register int i, j;
int nx2, ny2, dim;

dim = *idim;
nx2 = (int)(*nx / 2); ny2 = (int)(*ny / 2);

/* Uses a buffer w1,w2,w3,w4 
   to allow for the same array in input and output: */

/* Starting from:
   w4 w2
   w1 w3
*/
   for(j=0; j < ny2; j++)
      {
      for(i=0; i < nx2; i++)
      {
       w1 = in[i + j * dim]; 
       w2 = in[i + nx2 + (j + ny2) * dim]; 
       w3 = in[i + nx2 + j  * dim]; 
       w4 = in[i + (j + ny2) * dim]; 
/* Then:
   w3 w1
   w2 w4
*/
       out[i + j * dim] = w2; 
       out[i + nx2 + (j + ny2) * dim] = w1; 
       out[i + nx2 + j  * dim] = w4; 
       out[i + (j + ny2) * dim] = w3; 
       }
       }
      
 return(0);
}
/************************************************************
* To recentre FFT 
* and switch from zero frequency at (0,0) to (nx/2,ny/2) or inversely
* Double precision version
*
************************************************************/
int RECENT_FFT_DOUBLE(double *in, double *out, INT4 *nx, INT4 *ny, INT4 *idim)
{
double w1, w2, w3, w4;
register int i, j;
int nx2, ny2, dim;

dim = *idim;
// JLP2016: now handle cases when nx or ny are odd numbers:
nx2 = (int)(*nx / 2); ny2 = (int)(*ny / 2);

/* Uses a buffer w1,w2,w3,w4 
   to allow for the same array in input and output: */

/* Starting from:
   w4 w2
   w1 w3
*/
   for(j=0; j < ny2; j++) {
      for(i=0; i < nx2; i++) {
       w1 = in[i + j * dim]; 
       w2 = in[i + nx2 + (j + ny2) * dim]; 
       w3 = in[i + nx2 + j  * dim]; 
       w4 = in[i + (j + ny2) * dim]; 
/* Then:
   w3 w1
   w2 w4
*/
       out[i + j * dim] = w2; 
       out[i + nx2 + (j + ny2) * dim] = w1; 
       out[i + nx2 + j  * dim] = w4; 
       out[i + (j + ny2) * dim] = w3; 
       }
    }
      
 return(0);
}
/************************************************************
* To recentre FFT for 1D spectra 
* and switch from zero frequency at (x,0) to (x,ny/2) or inversely
*
************************************************************/
int RECENT_FFT_1D_Y(double *in, double *out, INT4 *nx, INT4 *ny, INT4 *idim)
{
float w1, w2;
register int i, j;
int ny2, dim;

dim = *idim;
ny2 = (int)(*ny / 2); 

/* Uses a buffer w1,w2,w3,w4 
   to allow for the same array in input and output: */

/* Starting from:
   w2
   w1
*/
   for(j=0; j < ny2; j++)
     {
       for(i=0; i < *nx; i++)
       {
       w1 = in[i + j * dim]; 
       w2 = in[i + (ny2 + j) * dim]; 
/* Then:
   w1 
   w2
*/
       out[i + j * dim] = w2; 
       out[i + (ny2 + j) * dim] = w1; 
       }
     }
      
 return(0);
}
/************************************************************
* To recentre FFT for 1D spectra 
* and switch from zero frequency at (x,0) to (x,ny/2) or inversely
*
************************************************************/
int RECENT_FFT_1D_Y_FLOAT(float *in, float *out, INT4 *nx, INT4 *ny, INT4 *idim)
{
float w1, w2;
register int i, j;
int ny2, dim;

dim = *idim;
ny2 = (int)(*ny / 2); 

/* Uses a buffer w1,w2,w3,w4 
   to allow for the same array in input and output: */

/* Starting from:
   w2
   w1
*/
   for(j=0; j < ny2; j++)
     {
       for(i=0; i < *nx; i++)
       {
       w1 = in[i + j * dim]; 
       w2 = in[i + (ny2 + j) * dim]; 
/* Then:
   w1 
   w2
*/
       out[i + j * dim] = w2; 
       out[i + (ny2 + j) * dim] = w1; 
       }
     }
      
 return(0);
}
/************************************************************
* To recentre FFT for 1D spectra 
* and switch from zero frequency at (0,y) to (nx/2,y) or inversely
*
************************************************************/
int RECENT_FFT_1D_X_FLOAT(float *in, float *out, INT4 *nx, INT4 *ny, INT4 *idim)
{
float w1, w2;
register int i, j;
int nx2, dim;

dim = *idim;
nx2 = (int)(*nx / 2); 

/* Uses a buffer w1,w2,w3,w4 
   to allow for the same array in input and output: */

/* Starting from:
   w1 w2
*/
   for(j=0; j < *ny; j++)
     {
       for(i=0; i < nx2; i++)
       {
       w1 = in[i + j * dim]; 
       w2 = in[i + nx2 + j * dim]; 
/* Then:
   w2 w1
*/
       out[i + j * dim] = w2; 
       out[i + nx2 + j * dim] = w1; 
       }
     }
      
 return(0);
}
/************************************************************
* To recentre FFT for 1D spectra 
* and switch from zero frequency at (0,y) to (nx/2,y) or inversely
*
************************************************************/
int RECENT_FFT_1D_X(double *in, double *out, INT4 *nx, INT4 *ny, INT4 *idim)
{
float w1, w2;
register int i, j;
int nx2, dim;

dim = *idim;
nx2 = (int)(*nx / 2); 

/* Uses a buffer w1,w2,w3,w4 
   to allow for the same array in input and output: */

/* Starting from:
   w1 w2
*/
   for(j=0; j < *ny; j++)
     {
       for(i=0; i < nx2; i++)
       {
       w1 = in[i + j * dim]; 
       w2 = in[i + nx2 + j * dim]; 
/* Then:
   w2 w1
*/
       out[i + j * dim] = w2; 
       out[i + nx2 + j * dim] = w1; 
       }
     }
      
 return(0);
}
/************************************************************
* To convert double precision array to single precision array 
*
************************************************************/
int TO_SINGLE(double *in, float *out, INT4 *nx, INT4 *ny, INT4 *idim)
{
register int i, j;
 for(j = 0; j < *ny; j++)
   for(i = 0; i < *nx; i++)
     out[i + j * (*idim)] = (float)in[i + j * (*idim)];
return(0);
}
/************************************************************
* To convert single precision array to double precision array 
*
************************************************************/
int TO_DOUBLE(float *in, double *out, INT4 *nx, INT4 *ny, INT4 *idim)
{
register int i, j;
 for(j = 0; j < *ny; j++)
   for(i = 0; i < *nx; i++)
     out[i + j * (*idim)] = (double)in[i + j * (*idim)];
return(0);
}
