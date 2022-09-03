/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* Set of C routines to use FFTW3 library
* 2D- FFT's for any size
*
*
* JLP
* Version 15/01/2015
*--------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>   // exit()
#include <string.h>   // strcpy()
#include <math.h>
#include "jlp_num_rename.h"
#include "jlp_numeric.h"

/* JLP2001: To be compatible with "matlab" 
* I should not normalize by sqrt(nx*ny): */
/*
#define NORMALIZE_SQRT
*/
/*
#define DEBUG
*/

/* Content of jlp_fftw.h: 
#include <fftw3.h>

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
int fftw_setup(char* fftw_directory, int nx, int ny, char *error_message);
int FFTW_SETUP(char* fftw_directory, int *nx, int *ny, char *error_message);
int fftw_fast(fftw_complex *image, int nx, int ny, int direct);
int FFTW_SHUTDOWN();
int fftw_shutdown();
*/

/* Static variables: */
static fftw_plan plan_fwd = 0;
static fftw_plan plan_bkwd = 0;
static fftw_complex* in_array_0 = NULL;
static fftw_complex* out_array_0 = NULL;
static int nx_0 = 0; 
static int ny_0 = 0;

/*
#define MAIN_TEST
*/

#ifdef MAIN_TEST
int main()
{
#define NX 8
#define NY 8
register int i;
int nx = NX, ny = NY;
double re[NX*NY], im[NX*NY];
fftw_complex* image;
char s[80];

printf(" Testing fftw_2D_double ...");

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

printf(" Now testing fftw_fast ...");
gets(s);

image = (fftw_complex *) malloc(nx * ny * sizeof(fftw_complex));
for(i = 0; i < nx * ny; i++)
  {
  image[i][0] = i;
  image[i][1] = -i;
  }
  printf("OK, I go on, with nx=%d ny=%d",nx,ny);

fftw_setup(nx, ny);
fftw_fast(image, nx, ny, 1);
fftw_fast(image, nx, ny, -1);
FFTW_SHUTDOWN();
for(i = 0; i < nx * ny; i++)
  {
  printf(" i=%d re=%f im=%f \n", i, image[i][0], image[i][1]);
  }

gets(s);
free(image);
return(0);
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
 {fprintf(stderr, "FFTW_2D/Fatal error idim=%d while nx=%d \n", *idim, *nx);
  exit(-1);
 }
if(*kod != 1 && *kod != -1)
 {fprintf(stderr, "FFTW_2D/Fatal error kod=%d (option not yet implemented)\n", *kod);
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
 {fprintf(stderr, "FFTW_1D_Y/Fatal error idim=%d while nx=%d \n", *idim, *nx);
  exit(-1);
 }
if(*kod != 1 && *kod != -1)
 {fprintf(stderr, "FFTW_1D_Y/Fatal error kod=%d (option not yet implemented)\n", *kod);
  exit(-1);
 }
nx1 = *nx; ny1 = *ny; direct = *kod;
#ifdef DEBUG
printf("FFTW_1D_Y/ nx1=%d ny1=%d direct=%d\n",nx1,ny1,direct);
#endif
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
 {fprintf(stderr, "FFTW_1D_Y_FLT/Fatal error idim=%d while nx=%d \n", *idim, *nx);
  exit(-1);
 }
if(*kod != 1 && *kod != -1)
 {fprintf(stderr, "FFTW_1D_Y_FLT/Fatal error kod=%d (option not yet implemented)\n", *kod);
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
int isize, dir;
fftw_plan the_plan;
fftw_complex *in_array, *out_array;

isize = nx * ny;
in_array = (fftw_complex *)fftw_malloc(isize * sizeof(fftw_complex));
out_array = (fftw_complex *)fftw_malloc(isize * sizeof(fftw_complex));

/* Transfer to complex array structure: */
for(i = 0; i < nx * ny; i++)
  {
   in_array[i][0] = re[i];
   in_array[i][1] = im[i];
  }

/* direct = 1  : forward
   direct = -1 : backward
*/
dir = (direct > 0) ? FFTW_FORWARD : FFTW_BACKWARD;
/* FFTW2:
// Warning: inversion nx-ny here: 
the_plan = fftw2d_create_plan(ny, nx, dir, FFTW_ESTIMATE);
if(the_plan == NULL)
{fprintf(stderr, "FFTW_2D_DBLE: fatal error creating plan\n"); exit(-1);}

// Compute the FFT: 
   fftwnd(the_plan, 1, in_out, 1, 0, 0, 0, 0);
*/
/* FFTW3: */
the_plan = fftw_plan_dft_2d(ny, nx, in_array, out_array, dir, FFTW_ESTIMATE);

// Compute the FFT: 
fftw_execute(the_plan);

/* Transfer back to original arrays (and normalize by sqrt(nx * ny): */
norm = 1.;
#ifdef NORMALIZE_SQRT 
norm = sqrt((double)nx * ny);
#else
if(direct > 0)
  norm = 1.;
else
/* Problem found here on December 19th: 
  norm = (double)(nx * ny);
 corrected by dividing by 1. 
*/
  norm = 1.;
#endif

#ifdef DEBUG
printf("fftw_2D_double/nx=%d ny=%d direct= %d norm=%f\n", nx, ny, direct, norm);
#endif

for(i = 0; i < nx * ny; i++) {
  re[i] = out_array[i][0] / norm;
  im[i] = out_array[i][1] / norm;
  }

/* Delete plan: */
  fftw_destroy_plan(the_plan);

fftw_free(in_array);
fftw_free(out_array);

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
int isize, dir;
fftw_plan the_plan;
fftw_complex *in_array, *out_array;

isize = nx * ny;
in_array = (fftw_complex *)fftw_malloc(isize * sizeof(fftw_complex));
out_array = (fftw_complex *)fftw_malloc(isize * sizeof(fftw_complex));

/* Transfer to complex array structure: */
for(i = 0; i < nx * ny; i++)
  {
   in_array[i][0] = re[i];
   in_array[i][1] = im[i];
  }

/* direct = 1  : forward
   direct = -1 : backward
*/
dir = (direct == 1) ? FFTW_FORWARD : FFTW_BACKWARD;
/* FFTW2: 
// Warning: inversion nx-ny here: 
the_plan = fftw2d_create_plan(ny, nx, dir, FFTW_ESTIMATE );
if(the_plan == NULL)
{printf("fftw_2D_float: fatal error creating plan\n"); exit(-1);}

// Compute the FFT: 
   fftwnd(the_plan, 1, in_out, 1, 0, 0, 0, 0);
*/
/* FFTW3: */
the_plan = fftw_plan_dft_2d(ny, nx, in_array, out_array, dir, FFTW_ESTIMATE);

// Compute the FFT: 
fftw_execute(the_plan);

/* Transfer back to original arrays (and normalize by sqrt(nx * ny): */
norm = 1.;
#ifdef NORMALIZE_SQRT 
norm = sqrt((double)nx * ny);
#else
if(direct > 0)
  norm = 1.;
else
  norm = (double) (nx * ny);
#endif

for(i = 0; i < nx * ny; i++) {
  re[i] = out_array[i][0] / norm;
  im[i] = out_array[i][1] / norm;
  }

#ifdef DEBUG
printf("fftw_2D_float/nx=%d ny=%d direct= %d norm=%f\n", nx, ny, direct, norm);
#endif

fftw_free(in_array);
fftw_free(out_array);

/* Delete plan: */
  fftw_destroy_plan(the_plan);

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
int dir;
fftw_plan the_plan;
fftw_complex *in_array, *out_array;

in_array = (fftw_complex *)fftw_malloc(ny * sizeof(fftw_complex));
out_array = (fftw_complex *)fftw_malloc(ny * sizeof(fftw_complex));

if(in_array == NULL || out_array == NULL) {
fprintf(stderr, "fftw_1D_Y_float: fatal error allocating memory\n"); 
exit(-1);
}


/* direct = 1  : forward
   direct = -1 : backward
*/
dir = (direct == 1) ? FFTW_FORWARD : FFTW_BACKWARD;
/* FFTW2:
the_plan = fftw_create_plan(ny, dir, FFTW_ESTIMATE);
if(the_plan == NULL)
{printf("fftw_1D_Y_float: fatal error creating the_plan\n"); exit(-1);}
*/

for(i = 0; i < nx; i++)
{
/* Transfer to complex array structure: */
  for(j = 0; j < ny; j++)
  {
   in_array[j][0] = re[i + j * nx];
   in_array[j][1] = im[i + j * nx];
  }

/* FFTW3: */
the_plan = fftw_plan_dft_1d(ny, in_array, out_array, dir, FFTW_ESTIMATE);

/* Compute the FFT: */
fftw_execute(the_plan);

/* Transfer back to original arrays (and normalize by sqrt(ny): */
norm = 1.;
#ifdef NORMALIZE_SQRT 
norm = sqrt((double)ny);
#else
if(direct > 0)
  norm = 1.;
else
  norm = (double)ny;
#endif

 for(j = 0; j < ny; j++) {
   re[i + j * nx] = out_array[j][0] / norm;
   im[i + j * nx] = out_array[j][1] / norm;
   }

/* End of loop on i (from 0 to nx-1) */
}

#ifdef DEBUG
printf("fftw_1D_Y_float/nx=%d ny=%d direct= %d norm=%f\n", nx, ny, direct, norm);
#endif

fftw_free(in_array);
fftw_free(out_array);

/* Delete the_plan: */
  fftw_destroy_plan(the_plan);

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
int dir;
fftw_plan the_plan;
fftw_complex *in_array, *out_array;

in_array = (fftw_complex *)fftw_malloc(ny * sizeof(fftw_complex));
out_array = (fftw_complex *)fftw_malloc(ny * sizeof(fftw_complex));

/* direct = 1  : forward
   direct = -1 : backward
*/
dir = (direct > 0) ? FFTW_FORWARD : FFTW_BACKWARD;
/* FFTW2:
the_plan = fftw_create_plan(ny, dir, FFTW_ESTIMATE);
if(the_plan == NULL)
{printf("fftw_1D_Y: fatal error creating plan\n"); exit(-1);}
*/

for(i = 0; i < nx; i++)
{
/* Transfer to complex array structure: */
  for(j = 0; j < ny; j++)
  {
   in_array[j][0] = re[i + j * nx];
   in_array[j][1] = im[i + j * nx];
  }

/* FFTW3: */
the_plan = fftw_plan_dft_1d(ny, in_array, out_array, dir, FFTW_ESTIMATE);

/* Compute the FFT: */
fftw_execute(the_plan);

/* Transfer back to original arrays (and normalize by sqrt(ny): */
norm = 1.;
#ifdef NORMALIZE_SQRT 
norm = sqrt((double)ny);
#else
if(direct > 0)
  norm = 1.;
else
  norm = (double)ny;
#endif

for(j = 0; j < ny; j++)
  {
  re[i + j * nx] = out_array[j][0] / norm;
  im[i + j * nx] = out_array[j][1] / norm;
  }

/* End of loop on i (from 0 to nx-1) */
}

#ifdef DEBUG
printf("fftw_1D_Y/nx=%d ny=%d direct= %d norm=%f\n", nx, ny, direct, norm);
#endif

fftw_free(in_array);
fftw_free(out_array);

/* Delete plan: */
  fftw_destroy_plan(the_plan);

return(0);
}
/****************************************************************
* Fortran interface to fftw_setup
****************************************************************/
int FFTW_SETUP(char* fftw_directory, int *nx0, int *ny0, char *error_message)
{

fftw_setup(fftw_directory, *nx0, *ny0, error_message);
return(0);
}
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// fftw_setup, to initialize FFTW routines
// Create forward and backward plans
//
// --------------------------------------------------------------*/
int fftw_setup(char* fftw_directory, int nx0, int ny0, char *error_message)
{
char fwd_name[60], bkwd_name[60];
FILE *fp_wisd;
int ToOutput;
register int i;

strcpy(error_message, "");

/* Get memory for static array: */
in_array_0 = (fftw_complex *)fftw_malloc(nx0 * ny0 * sizeof(fftw_complex));
out_array_0 = (fftw_complex *)fftw_malloc(nx0 * ny0 * sizeof(fftw_complex));
nx_0 = nx0;
ny_0 = ny0;

/* Fill with dummy data: */
for(i = 0 ; i < nx0 * ny0; i++) {
  in_array_0[i][0] = 0.;
  in_array_0[i][1] = 0.;
}

/********************** With "forward"  ***************************/
// sprintf(fwd_name,"/d/fftw/fwd_%d%d.wis",nx0,ny0);
sprintf(fwd_name,"%s/fwd_%d%d.wis", fftw_directory, nx0, ny0);

/* Check if wisdom file already there: */
printf("fftw_setup/Read wisdom file: %s\n",fwd_name);
if((fp_wisd = fopen(fwd_name,"r")) == NULL) 
  {
  sprintf(error_message, "fftw_setup/Failure openning wisdom file: %s\n",
         fwd_name);
  ToOutput = 1;
  }
else
  {
  ToOutput = 0;
  if(fftw_import_wisdom_from_file(fp_wisd) != 0) ToOutput = 1;
  fclose(fp_wisd);
  }

/* Create  "plan" for fftw (speed is wanted for subsequent FFT's) */
/* Warning: inversion nx-ny here: */
/* FFTW2:
plan_fwd = fftw2d_create_plan(ny0, nx0, FFTW_FORWARD, FFTW_MEASURE );
*/
/* FFTW3: (FFTW_MEASURE to measure the optimal plan) */
plan_fwd = fftw_plan_dft_2d(ny0, nx0, in_array_0, out_array_0, FFTW_FORWARD, 
                           FFTW_MEASURE);

// Compute the FFT: 
/* Output wisdom file if needed: */
if(ToOutput)
  {
  printf("fftw_setup/Write wisdom file: %s\n",fwd_name);
  if((fp_wisd = fopen(fwd_name,"w")) != NULL) 
                                       fftw_export_wisdom_to_file(fp_wisd);
  fclose(fp_wisd);
  }

/********************** With "backward"  ***************************/
// sprintf(bkwd_name,"/d/fftw/bkwd_%d%d.wis",nx0,ny0);
sprintf(bkwd_name,"%s/bkwd_%d%d.wis", fftw_directory, nx0, ny0);

/* Check if wisdom file already there: */
printf("fftw_setup/Read wisdom file: %s\n",bkwd_name);
if((fp_wisd = fopen(bkwd_name,"r")) == NULL) 
  {
  sprintf(error_message, "fftw_setup/Failure openning wisdom file: %s\n",
         bkwd_name);
  ToOutput = 1;
  } 
else
  {
  ToOutput = 0;
  if(fftw_import_wisdom_from_file(fp_wisd) != 0) ToOutput = 1;
  fclose(fp_wisd);
  }

/* Create  "plan" for fftw (speed is wanted for subsequent FFT's) */
/* Warning: inversion nx-ny here: */
/* FFTW2:
plan_bkwd = fftw2d_create_plan(ny0, nx0, FFTW_BACKWARD, FFTW_MEASURE);
*/

/* FFTW3: (FFTW_MEASURE to measure the optimal plan) */
plan_bkwd = fftw_plan_dft_2d(ny0, nx0, in_array_0, out_array_0, FFTW_BACKWARD, 
                           FFTW_MEASURE);

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
int status;
status = fftw_shutdown();
return(status);
}
/*************************************************************
*
**************************************************************/
int fftw_shutdown()
{
/* Delete plans: */
  fftw_destroy_plan(plan_fwd);
  fftw_destroy_plan(plan_bkwd);

/* Free memory: */
  fftw_free(in_array_0);
  fftw_free(out_array_0);
return(0);
}
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* 2D FFT routine for double precision fftw_complex arrays
* direction = 1  : forward
* direction = -1 : backward
*  Should be called after fftw_setup and before FFTW_SHUTDOWN
* --------------------------------------------------------------*/
int fftw_fast(fftw_complex *image, int nx, int ny, int direct)
{
register int i;
double norm;
char fftw_directory[128], error_message[128];

if(in_array_0 == NULL || out_array_0 == NULL || nx != nx_0 || ny != ny_0){
/* Before 2015: 
 fprintf(stderr, "fftw_fast/Fatal error: memory not previously allocated !\n");
 exit(-1);
* Now:
*/
 strcpy(fftw_directory, ".");
 fftw_setup(fftw_directory, nx, ny, error_message);
 }

/* FFTW3: */
/* Copy to in_array_0: */
for(i = 0; i < nx * ny; i++)
  {
    in_array_0[i][0] = image[i][0];
    in_array_0[i][1] = image[i][1];
  }

/* Compute the FFT: */
if(direct > 0)
    fftw_execute(plan_fwd);
else
    fftw_execute(plan_bkwd);


/* Normalize by sqrt(nx * ny) for direct and inverse FFT: */
norm = 1.;
#ifdef NORMALIZE_SQRT 
norm = sqrt((double)(nx * ny)); 
#else
/* Normalize by (nx * ny) for inverse FFT: */
if(direct > 0)
  norm = 1.;
else
  norm = (double)(nx * ny);
#endif

for(i = 0; i < nx * ny; i++)
  {
    image[i][0] = out_array_0[i][0] / norm;
    image[i][1] = out_array_0[i][1] / norm;
  }

#ifdef DEBUG
printf("fftw_fast/nx=%d ny=%d direct= %d norm=%f\n", nx, ny, direct, norm);
#endif

return(0);
}

