/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* Set of C routines to use FFTW1.3 library
* 2D- FFT's for any size
*
*
* JLP
* Version 12/12/2006 (from my BORLAND version)
*--------------------------------------------------------*/
#include <stdio.h>
#include <math.h>
#include "jlp_fftw.h"
#include "jlp_num_rename.h"

/* JLP2001: To be compatible with "matlab" 
* I should not normalize by sqrt(nx*ny): */
/*
#define NORMALIZE_SQRT
*/

/* Content of jlp_fftw.h: 
#include <fftw.h>

int FFTW_1D_Y_FLT(float *re, float *im, INT4 *nx, INT4 *ny, INT4 *idim, 
                   INT4 *direct);
int fftw_1D_Y_float(float *re, float *im, int nx, int ny, int direct);
int FFTW_1D_Y_DBLE(double *re, double *im, INT4 *nx, INT4 *ny, INT4 *idim, 
             INT4 *direct);
int fftw_1D_Y_double(double *re, double *im, int nx, int ny, int direct);
int FFTW_2D_DBLE(double *re, double *im, int *nx, int *ny, int *direct);
int fftw_2D_double(double *re, double *im, int nx, int ny, int direct);
int FFTW_2D_FLT(float *re, float *im, INT4 *nx, INT4 *ny, INT4 *idim, 
                  INT4 *kod);
int fftw_2D_float(float *re, float *im, int nx, int ny, int direct);
int fftw_setup(int nx, int ny);
int FFTW_SETUP(int *nx, int *ny);
int fftw_fast(FFTW_COMPLEX *image, int nx, int ny, int direct);
int fftw_shutdown();
*/

/* Static variables: */
static fftwnd_plan plan_fwd, plan_bkwd;

/*
#define MAIN_TEST
*/

#ifdef MAIN_TEST
main()
{
register int i;
int nx = 9, ny = 8;
double re[128], im[128];
FFTW_COMPLEX* image;
char s[80];

for(i = 0; i < nx * ny; i++)
  {
  re[i] = i;
  im[i] = -i;
  }

fftw_2D_double(re, im, nx, ny, 1);
fftw_2D_double(re, im, nx, ny, -1);

for(i = 0; i < nx * ny; i++)
  {
  printf(" i=%d re=%f im=%f \n", i, re[i], im[i]);
  }

printf(" Now going to fast fft...");
gets(s);

nx = 388; ny = 128;
image = (FFTW_COMPLEX *) malloc(nx * ny * sizeof(FFTW_COMPLEX));
for(i = 0; i < nx * ny; i++)
  {
  c_re(image[i]) = i;
  c_im(image[i]) = -i;
  }
  printf("OK, I go on, with nx=%d ny=%d",nx,ny);

fftw_setup(nx, ny);
fftw_fast(image, nx, ny, 1);
fftw_fast(image, nx, ny, -1);
fftw_shutdown();
for(i = 0; i < 200; i++)
  {
  printf(" i=%d re=%f im=%f \n", i, c_re(image[i]), c_im(image[i]));
  }

gets(s);
free(image);
}
#endif
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* Interface with Fortran routines (with arrays of first dimension = idim)
* kod=1, or 2 direct
* kod=-1, or -2 inverse
* kod=-2, or 2: power spectrum, and phase
*--------------------------------------------------------------*/
int FFTW_2D_FLT(float *re, float *im, INT4 *nx, INT4 *ny, INT4 *idim, 
                  INT4 *kod)
{
int nx1, ny1, direct, status;
if(*idim != *nx)
 {printf("FFTW_2D/Fatal error idim=%d while nx=%d \n", *idim, *nx);
  exit(-1);
 }
if(*kod != 1 && *kod != -1)
 {printf("FFTW_2D/Fatal error kod=%d (option not yet implemented)\n", *kod);
  exit(-1);
 }
nx1 = *nx; ny1 = *ny; direct = *kod;
status = fftw_2D_float(re, im, nx1, ny1, direct);
return(status);
}
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* Interface with Fortran routines
* kod=1, or 2 direct
* kod=-1, or -2 inverse
* kod=-2, or 2: power spectrum, and phase
* --------------------------------------------------------------*/
int FFTW_1D_Y_DBLE(double *re, double *im, INT4 *nx, INT4 *ny, 
                   INT4 *idim, INT4 *kod)
{
int nx1, ny1, direct, status;
if(*idim != *nx)
 {printf("FFTW_1D_Y/Fatal error idim=%d while nx=%d \n", *idim, *nx);
  exit(-1);
 }
if(*kod != 1 && *kod != -1)
 {printf("FFTW_1D_Y/Fatal error kod=%d (option not yet implemented)\n", *kod);
  exit(-1);
 }
nx1 = *nx; ny1 = *ny; direct = *kod;
printf("FFTW_1D_Y/ nx1=%d ny1=%d direct=%d\n",nx1,ny1,direct);
status = fftw_1D_Y_double(re, im, nx1, ny1, direct);
return(status);
}
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* Interface with Fortran routines
* kod=1, or 2 direct
* kod=-1, or -2 inverse
* kod=-2, or 2: power spectrum, and phase
* --------------------------------------------------------------*/
int FFTW_1D_Y_FLT(float *re, float *im, INT4 *nx, INT4 *ny, 
                   INT4 *idim, INT4 *kod)
{
int nx1, ny1, direct, status;

#ifndef TOTO_
double sum;
register int i;
#endif

if(*idim != *nx)
 {printf("FFTW_1D_Y_FLT/Fatal error idim=%d while nx=%d \n", *idim, *nx);
  exit(-1);
 }
if(*kod != 1 && *kod != -1)
 {printf("FFTW_1D_Y_FLT/Fatal error kod=%d (option not yet implemented)\n", *kod);
  exit(-1);
 }
nx1 = *nx; ny1 = *ny; direct = *kod;
#ifdef DEBUG
printf("FFTW_1D_Y_FLT/ nx1=%d ny1=%d direct=%d (fftw)\n",nx1,ny1,direct);
#endif

#ifndef TOTO_
sum =0;
for(i=0; i < ny1; i++) sum += re[nx1/2 + i * nx1];
printf(" Sum of central line = %e and sum/sqrt(ny)=%e \n",
        sum,sum/sqrt((double)ny1));
#endif

status = fftw_1D_Y_float(re, im, nx1, ny1, direct);

/* JLP99: to check that FFT is correct: */
/* Not recentred yet !! */
#ifndef TOTO_
for(i=0; i <= 2; i++)
printf("re,im [ixc,%d]: %e %e \n",i,
        re[nx1/2 + i*nx1],im[nx1/2 + i*nx1]);
#endif

return(status);
}
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* 2D FFT routine for double precision arrays
* direct = 1  : direct or forward
* direct = -1 : reverse or backward
* (FORTRAN version)
* --------------------------------------------------------------*/
int FFTW_2D_DBLE(double *re, double *im, int *nx, int *ny, int *direct)
{
int status;
status = fftw_2D_double(re, im, *nx, *ny, *direct);
return(status);
}
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* 2D FFT routine for double precision arrays
* direct = 1  : direct or forward
* direct = -1 : reverse or backward
*
* --------------------------------------------------------------*/
int fftw_2D_double(double *re, double *im, int nx, int ny, int direct)
{
register int i;
double norm;
int isize;
fftwnd_plan plan;
fftw_direction dir;
FFTW_COMPLEX* in_out;

isize = nx * ny;
in_out = (FFTW_COMPLEX *) malloc(isize * sizeof(FFTW_COMPLEX));

/* Transfer to complex array structure: */
for(i = 0; i < nx * ny; i++)
  {
   c_re(in_out[i]) = re[i];
   c_im(in_out[i]) = im[i];
  }

/* direct = 1  : forward
   direct = -1 : backward
*/
dir = (direct == 1) ? FFTW_FORWARD : FFTW_BACKWARD;
/* Warning: inversion nx-ny here: */
plan = fftw2d_create_plan(ny, nx, dir, 
                              FFTW_ESTIMATE | FFTW_IN_PLACE);
if(plan == NULL)
{printf("FFTW_2D_DBLE: fatal error creating plan\n"); exit(-1);}

/* Compute the FFT: */
   fftwnd(plan, 1, in_out, 1, 0, 0, 0, 0);

/* Transfer back to original arrays (and normalize by sqrt(nx * ny): */
#ifdef NORMALIZE_SQRT 
norm = nx * ny; norm = sqrt(norm);
for(i = 0; i < nx * ny; i++)
  {
  re[i] = c_re(in_out[i]) /norm;
  im[i] = c_im(in_out[i]) /norm;
  }
#else
if(direct == 1)
  {
  for(i = 0; i < nx * ny; i++)
    {
    re[i] = c_re(in_out[i]);
    im[i] = c_im(in_out[i]);
    }
  }
else
  {
  norm = nx * ny;
  for(i = 0; i < nx * ny; i++)
    {
    re[i] = c_re(in_out[i]) /norm;
    im[i] = c_im(in_out[i]) /norm;
    }
  }
#endif

free(in_out);

/* Delete plan: */
  fftwnd_destroy_plan(plan);

return(0);
}
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* 2D FFT routine for single precision arrays (for which idim=nx)
* (well suited to C arrays since nx=idim...)
* direct = 1  : direct or forward
* direct = -1 : reverse or backward
* --------------------------------------------------------------*/
int fftw_2D_float(float *re, float *im, int nx, int ny, int direct)
{
register int i;
double norm;
int isize;
fftwnd_plan plan;
fftw_direction dir;
FFTW_COMPLEX* in_out;

isize = nx * ny;
in_out = (FFTW_COMPLEX *) malloc(isize * sizeof(FFTW_COMPLEX));

/* Transfer to complex array structure: */
for(i = 0; i < nx * ny; i++)
  {
   c_re(in_out[i]) = re[i];
   c_im(in_out[i]) = im[i];
  }

/* direct = 1  : forward
   direct = -1 : backward
*/
dir = (direct == 1) ? FFTW_FORWARD : FFTW_BACKWARD;
/* Warning: inversion nx-ny here: */
plan = fftw2d_create_plan(ny, nx, dir, 
                              FFTW_ESTIMATE | FFTW_IN_PLACE);
if(plan == NULL)
{printf("fftw_2D_float: fatal error creating plan\n"); exit(-1);}

/* Compute the FFT: */
   fftwnd(plan, 1, in_out, 1, 0, 0, 0, 0);

/* Transfer back to original arrays (and normalize by sqrt(nx * ny): */
#ifdef NORMALIZE_SQRT 
norm = nx * ny; norm = sqrt(norm);
for(i = 0; i < nx * ny; i++)
  {
  re[i] = c_re(in_out[i]) /norm;
  im[i] = c_im(in_out[i]) /norm;
  }
#else
if(direct == 1)
  {
  for(i = 0; i < nx * ny; i++)
    {
    re[i] = c_re(in_out[i]);
    im[i] = c_im(in_out[i]);
    }
  }
else
  {
  norm = nx * ny;
  for(i = 0; i < nx * ny; i++)
    {
    re[i] = c_re(in_out[i]) /norm;
    im[i] = c_im(in_out[i]) /norm;
    }
  }
#endif

free(in_out);

/* Delete plan: */
  fftwnd_destroy_plan(plan);

return(0);
}
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* 1D FFT routine for single precision arrays
* along the columns (for spectroscopic mode)
* direct = 1  : direct or forward
* direct = -1 : reverse or backward
*
* --------------------------------------------------------------*/
int fftw_1D_Y_float(float *re, float *im, int nx, int ny, int direct)
{
register int i, j;
double norm;
fftw_plan plan;
fftw_direction dir;
FFTW_COMPLEX* in_out;

in_out = (FFTW_COMPLEX *) malloc(ny * sizeof(FFTW_COMPLEX));
if(in_out == NULL)
{printf("fftw_1D_Y_float: fatal error allocating memory\n"); exit(-1);}


/* direct = 1  : forward
   direct = -1 : backward
*/
dir = (direct == 1) ? FFTW_FORWARD : FFTW_BACKWARD;
plan = fftw_create_plan(ny, dir, FFTW_ESTIMATE | FFTW_IN_PLACE);
if(plan == NULL)
{printf("fftw_1D_Y_float: fatal error creating plan\n"); exit(-1);}

for(i = 0; i < nx; i++)
{
/* Transfer to complex array structure: */
  for(j = 0; j < ny; j++)
  {
   c_re(in_out[j]) = re[i + j * nx];
   c_im(in_out[j]) = im[i + j * nx];
  }

/* Compute the FFT: */
   fftw(plan, 1, in_out, 1, 0, 0, 0, 0);

/* Transfer back to original arrays (and normalize by sqrt(ny): */
#ifdef NORMALIZE_SQRT 
norm = sqrt((double)ny);
for(j = 0; j < ny; j++)
  {
  re[i + j * nx] = c_re(in_out[j]) /norm;
  im[i + j * nx] = c_im(in_out[j]) /norm;
  }
#else
if(direct == 1)
  {
  for(j = 0; j < ny; j++)
    {
    re[i + j * nx] = c_re(in_out[j]);
    im[i + j * nx] = c_im(in_out[j]);
    }
  }
else
  {
  norm = (double)ny;
  for(j = 0; j < ny; j++)
    {
    re[i + j * nx] = c_re(in_out[j]) /norm;
    im[i + j * nx] = c_im(in_out[j]) /norm;
    }
  }
#endif
/* End of loop on i (from 0 to nx-1) */
}

free(in_out);

/* Delete plan: */
  fftw_destroy_plan(plan);

return(0);
}
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* 1D FFT routine for double precision arrays
* along the columns (for spectroscopic mode)
* direct = 1  : direct or forward
* direct = -1 : reverse or backward
*
* --------------------------------------------------------------*/
int fftw_1D_Y_double(double *re, double *im, int nx, int ny, int direct)
{
register int i, j;
double norm;
fftw_plan plan;
fftw_direction dir;
FFTW_COMPLEX* in_out;

in_out = (FFTW_COMPLEX *) malloc(ny * sizeof(FFTW_COMPLEX));

/* direct = 1  : forward
   direct = -1 : backward
*/
dir = (direct == 1) ? FFTW_FORWARD : FFTW_BACKWARD;
plan = fftw_create_plan(ny, dir, FFTW_ESTIMATE | FFTW_IN_PLACE);
if(plan == NULL)
{printf("fftw_1D_Y: fatal error creating plan\n"); exit(-1);}

for(i = 0; i < nx; i++)
{
/* Transfer to complex array structure: */
  for(j = 0; j < ny; j++)
  {
   c_re(in_out[j]) = re[i + j * nx];
   c_im(in_out[j]) = im[i + j * nx];
  }

/* Compute the FFT: */
   fftw(plan, 1, in_out, 1, 0, 0, 0, 0);

/* Transfer back to original arrays (and normalize by sqrt(ny): */
#ifdef NORMALIZE_SQRT 
norm = sqrt((double)ny);
for(j = 0; j < ny; j++)
  {
  re[i + j * nx] = c_re(in_out[j]) /norm;
  im[i + j * nx] = c_im(in_out[j]) /norm;
  }
#else
if(direct == 1)
  {
  for(j = 0; j < ny; j++)
    {
    re[i + j * nx] = c_re(in_out[j]);
    im[i + j * nx] = c_im(in_out[j]);
    }
  }
else
  {
  norm = (double)ny;
  for(j = 0; j < ny; j++)
    {
    re[i + j * nx] = c_re(in_out[j]) /norm;
    im[i + j * nx] = c_im(in_out[j]) /norm;
    }
  }
#endif
/* End of loop on i (from 0 to nx-1) */
}

free(in_out);

/* Delete plan: */
  fftw_destroy_plan(plan);

return(0);
}
/****************************************************************
* Fortran interface to fftw_setup
****************************************************************/
int FFTW_FSETUP(int *nx, int *ny)
{
fftw_setup(*nx, *ny);
return(0);
}
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// fftw_setup, to initialize FFTW routines
// Create forward and backward plans
//
// --------------------------------------------------------------*/
int fftw_setup(int nx, int ny)
{
char fwd_name[60], bkwd_name[60];
FILE *fp_wisd;
int ToOutput;

/********************** With "forward"  ***************************/
#ifdef BORLAND
sprintf(fwd_name,"c:\\tc\\fftw13\\jlp\\fwd_%d.wis",nx);
#else
sprintf(fwd_name,"/d/fftw/fwd_%d%d.wis",nx,ny);
#endif

/* Check if wisdom file already exits: */
printf("fftw_setup/Read wisdom file: %s\n",fwd_name);
if((fp_wisd = fopen(fwd_name,"r")) == NULL) 
  {
  printf("fftw_setup/Failure to open wisdom file: %s\n",fwd_name);
  ToOutput = 1;
  }
else
  {
  ToOutput = 0;
  if(fftw_import_wisdom_from_file(fp_wisd) == FFTW_FAILURE) ToOutput = 1;
  fclose(fp_wisd);
  }

/* Create  "plan" for fftw (speed is wanted for subsequent FFT's) */
/* Warning: inversion nx-ny here: */
plan_fwd = fftw2d_create_plan(ny, nx, FFTW_FORWARD,
                              FFTW_MEASURE | FFTW_IN_PLACE | FFTW_USE_WISDOM);

/* Output wisdom file if needed: */
if(ToOutput)
  {
  printf("fftw_setup/Write wisdom file: %s\n",fwd_name);
  if((fp_wisd = fopen(fwd_name,"w")) != NULL) 
                                       fftw_export_wisdom_to_file(fp_wisd);
  fclose(fp_wisd);
  }

/********************** With "backward"  ***************************/
#ifdef BORLAND
sprintf(bkwd_name,"c:\\tc\\fftw13\\jlp\\bk_%d%d.wis",nx,ny);
#else
sprintf(bkwd_name,"/d/fftw/bk_%d%d.wis",nx,ny);
#endif

/* Check if wisdom file already exits: */
printf("fftw_setup/Read wisdom file: %s\n",bkwd_name);
if((fp_wisd = fopen(bkwd_name,"r")) == NULL) 
  {
  printf("fftw_setup/Failure to open wisdom file: %s\n",bkwd_name);
  ToOutput = 1;
  } 
else
  {
  ToOutput = 0;
  if(fftw_import_wisdom_from_file(fp_wisd) == FFTW_FAILURE) ToOutput = 1;
  fclose(fp_wisd);
  }

/* Create  "plan" for fftw (speed is wanted for subsequent FFT's) */
/* Warning: inversion nx-ny here: */
plan_bkwd = fftw2d_create_plan(ny, nx, FFTW_BACKWARD,
                              FFTW_MEASURE | FFTW_IN_PLACE | FFTW_USE_WISDOM);

/* Output wisdom file if needed: */
if(ToOutput)
  {
  printf("fftw_setup/Write wisdom file: %s\n",bkwd_name);
  if((fp_wisd = fopen(bkwd_name,"w")) != NULL) 
                                        fftw_export_wisdom_to_file(fp_wisd);
  fclose(fp_wisd);
  }

return(0);
}
/*************************************************************
*
**************************************************************/
int FFTW_SHUTDOWN()
{
/* Delete plans: */
  fftwnd_destroy_plan(plan_fwd);
  fftwnd_destroy_plan(plan_bkwd);
return(0);
}
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* 2D FFT routine for double precision fftw_complex arrays
* direction = 1  : forward
* direction = -1 : backward
*  Should be called after fftw_setup and before fftw_shutdown
* --------------------------------------------------------------*/
int fftw_fast(FFTW_COMPLEX *image, int nx, int ny, int dir)
{
register int i;
double norm;

/* Compute the FFT: */
if(dir == 1)
   fftwnd(plan_fwd, 1, image, 1, 0, 0, 0, 0);
else
   fftwnd(plan_bkwd, 1, image, 1, 0, 0, 0, 0);

/* Normalize by sqrt(nx * ny): */
#ifdef NORMALIZE_SQRT 
norm = nx * ny; norm = sqrt(norm);
for(i = 0; i < nx * ny; i++)
  {
  c_re(image[i]) /= norm;
  c_im(image[i]) /= norm;
  }
#else
if(dir == -1)
  {
  norm = (double)(nx * ny);
  for(i = 0; i < nx * ny; i++)
    {
    c_re(image[i]) /= norm;
    c_im(image[i]) /= norm;
    }
  }
#endif

return(0);
}

