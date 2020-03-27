/**********************************************************************
* jlp_read_ascii_data.cpp 
* to read plot ascii data files 
*
*  JLP
* Version 15-12-2017
**********************************************************************/
#include <stdio.h>
#include <string.h>

#include "jlp_read_ascii_data.h" 

// #define DEBUG
#define MAX_NCOLUMNS 10

static int read_one_col(char *buffer, int icol_x, double *xvalue);

/*************************************************************************
* jlp_ascii_get_ncolumns
* Purpose: determine the number of columns in the input ascii file
*
* INPUT:
*  filename0 : name of the ascii file to be read
*
* OUTPUT:
*  ncols0 : number of columns that have been found in the input file 
*  ndatalines0 : number of uncommented lines in the input file 
*************************************************************************/
int jlp_ascii_get_ncolumns(char *filename0, long *ncols0, long *ndatalines0,
                           long *nlines0)
{
int status = 0, i, imax; 
long iline, idataline; 
double val_x, val_y;
char buffer[120];
FILE *fp_in;

if((fp_in = fopen(filename0,"r")) == NULL) {
  fprintf(stderr, "jlp_ascii_get_columns/Error opening input file >%s< \n", 
          filename0);
  return(-1);
  }

// First count the number of columns in the first uncommented line
imax = 0;
iline = 0;
idataline = 0;
while(!feof(fp_in)) {
  if(fgets(buffer,120,fp_in) == NULL) break;
  iline++;
  if(buffer[0] != '#' && buffer[0] != '%') {
    idataline++;
    if(idataline == 1) { 
      for(i = 1; i <= 10; i++) {
       status = read_one_col(buffer, i, &val_x);
       if(status == 0) if(i > imax) imax = i;
       }
     } // EOF idataline == 1
   } // EOF if buffer... 
}

fclose(fp_in);

*ncols0 = imax;
*nlines0 = iline;
*ndatalines0 = idataline;

if((imax < 0) || (*ndatalines0 == 0)) {
  fprintf(stderr, "jlp_ascii_get_ncolumns/Error: imax=%d ncols=%d ndatalines=%d\n no data found in >%s< \n", 
          imax, *ncols0, *ndatalines0, filename0);
  status = -1;
  } else {
  status = 0;
  }

return(status);
}
/*************************************************************************
* jlp_ascii_read_two_columns
* Purpose: read two columns from the input ascii file
*
* INPUT:
*  filename0 : name of the ascii file to be read
*  icol_x : column number of x data values (in the range [1, MAX_NCOLUMNS])
*  icol_y : column number of y data values (in the range [1, MAX_NCOLUMNS])
*
* OUTPUT:
*  xplot0, yplot0 : x, y arrays that have been read in columns icol_x and icol_y
*  npts0 : number of the (x,y) values that have been read
*************************************************************************/
int jlp_ascii_read_two_columns(char *filename0, int icol_x, int icol_y,
                               double **xplot0, double **yplot0, int *npts0)
{
int status;
double *errorx0, *errory0;

  status = jlp_ascii_read_four_columns(filename0, icol_x, icol_y, 0, 0,
                                       xplot0, yplot0, &errorx0, &errory0, 
                                       npts0);
return(status);
}
/*************************************************************************
* jlp_ascii_read_four_columns
* Purpose: read two columns from the input ascii file
*
* INPUT:
*  filename0 : name of the ascii file to be read
*  icol_x : column number of x data values (in the range [1, MAX_NCOLUMNS])
*  icol_y : column number of y data values (in the range [1, MAX_NCOLUMNS])
*  icol_errorx : column number of x error values (in the range [1, MAX_NCOLUMNS])
*  icol_errory : column number of y error values (in the range [1, MAX_NCOLUMNS])
*
* OUTPUT:
*  xplot0, yplot0 : x, y arrays that have been read in columns icol_x and icol_y
*  errorx0, errory0 : x, y error arrays that have been read in icol_errrorx and icol_errory
*  npts0 : number of the (x,y) values that have been read
*************************************************************************/
int jlp_ascii_read_four_columns(char *filename0, int icol_x, int icol_y,
                                int icol_errorx, int icol_errory,
                                double **xplot0, double **yplot0, 
                                double **errorx0, double **errory0, int *npts0)
{
int status, stat1, stat2, stat3, stat4;
FILE *fp_in;
long ncols0, ndatalines0, nlines0, iline, i;
double val_x, val_y, val_errorx, val_errory;
char buffer[120];

#ifdef DEBUG
  printf("jlp_ascii_read_four_columns/Opening input file >%s< \n",
          filename0);
  printf("icol_x=%d icol_y=%d icol_errorx=%d icol_errory=%d\n", 
          icol_x, icol_y, icol_errorx, icol_errory);
#endif

*npts0 = 0;
*xplot0 = NULL;
*yplot0 = NULL;
*errorx0 = NULL;
*errory0 = NULL;

if((icol_x < 1 || icol_x > MAX_NCOLUMNS)
   || (icol_y < 1 || icol_y > MAX_NCOLUMNS)) {
  fprintf(stderr, "jlp_ascii_read_four_columns/icol_x=%d, icol_y=%d (should be from 1 to max=%d )\n", 
          icol_x, icol_y, MAX_NCOLUMNS);
  return(-2);
  }
if((icol_errorx < 0) && (icol_errorx > MAX_NCOLUMNS) 
  || (icol_errory < 0) && (icol_errory > MAX_NCOLUMNS)) { 
  fprintf(stderr, "jlp_ascii_read_four_columns/icol_errorx=%d, icol_errory=%d (should be from 1 to max=%d )\n", 
          icol_errorx, icol_errory, MAX_NCOLUMNS);
  return(-2);
  }

//********************************************************//
// First go : count the number of data lines
status = jlp_ascii_get_ncolumns(filename0, &ncols0, &ndatalines0,
                                &nlines0);
if(status != 0) {
  fprintf(stderr, "jlp_ascii_read_four_columns/Error: no data found in >%s< status=%d\n", 
          filename0, status);
  return(-1);
  }

if((fp_in = fopen(filename0,"r")) == NULL) {
  fprintf(stderr, "jlp_ascii_read_four_columns/Error opening input file >%s< \n",
          filename0);
  return(-1);
  }

// Allocate memory
 *xplot0 = new double[ndatalines0];
 *yplot0 = new double[ndatalines0];
 *errorx0 = new double[ndatalines0];
 *errory0 = new double[ndatalines0];
 for(i = 0; i < ndatalines0; i++) {
   (*xplot0)[i] = 0.;
   (*yplot0)[i] = 0.;
   (*errorx0)[i] = 0.;
   (*errory0)[i] = 0.;
   }

if((fp_in = fopen(filename0,"r")) == NULL) {
  fprintf(stderr, "jlp_ascii_read_four_columns/Error opening input file >%s< \n",
          filename0);
  return(-1);
  }

//********************************************************//
// Second go : read the data lines

iline = 0;
i = 0;

while(!feof(fp_in)) {

/* Read new line */
  if(fgets(buffer,120,fp_in) == NULL) break;
  iline++;

if(buffer[0] != '#' && buffer[0] != '%') {
  stat1 = read_one_col(buffer, icol_x, &val_x);
  stat2 = read_one_col(buffer, icol_y, &val_y);

  if((stat1 == 0) && (stat2 ==0)) {
     (*xplot0)[i] = val_x;
     (*yplot0)[i] = val_y;
/*
      printf(" Buffer=%s", buffer);
      printf(" i=%d val=%lf val_y=%lf\n", i, val_x, val_y);
*/
    } else {
      fprintf(stderr,"jlp_ascii_read_four_columns/Error at line %d, xvalue=%f yvalue=%f\n%s\n",
              iline, val_x, val_y, buffer);
      break;
    }

  if(icol_errorx != 0) {
      stat3 = read_one_col(buffer, icol_errorx, &val_errorx);
      if(stat3 == 0) {
       (*errorx0)[i] = val_errorx;
      } else {
      fprintf(stderr,"jlp_ascii_read_four_columns/Error at line %d, val_errorx=%f\n%s\n",
              iline, val_errorx, buffer);
      break;
      }
    }
  if(icol_errory != 0) {
      stat3 = read_one_col(buffer, icol_errory, &val_errory);
      if(stat3 == 0) {
       (*errory0)[i] = val_errory;
      } else {
      fprintf(stderr,"jlp_ascii_read_four_columns/Error at line %d, val_errory=%f\n%s\n",
              iline, val_errory, buffer);
      break;
      }
    }

   i++;
  } /* EOF test on buffer[0] */
} /* EOF while(1) loop */

fclose(fp_in);

*npts0 = i;

return(0);
}
/*************************************************************************
* read_one_col
*
* INPUT:
*  buffer : current line to be read
*  icol_x : number of the columns to be read
*
* OUTPUT:
*  xvalue : array of the xvalues actually read 
*************************************************************************/
static int read_one_col(char *buffer, int icol_x, double *xvalue)
{
int ival_x, status = 0;
double val_x;


ival_x = 0;
val_x = 0.;

  switch(icol_x) { 
    case 1:
      ival_x = sscanf(buffer, "%lf", &val_x);
      break;
    case 2:
      ival_x = sscanf(buffer, "%*lf %lf", &val_x);
      break;
    case 3:
      ival_x = sscanf(buffer, "%*lf %*lf %lf", &val_x);
      break;
    case 4:
      ival_x = sscanf(buffer, "%*lf %*lf %*lf %lf", &val_x);
      break;
    case 5:
      ival_x = sscanf(buffer, "%*lf %*lf %*lf %*lf %lf", &val_x);
      break;
    case 6:
      ival_x = sscanf(buffer, "%*lf %*lf %*lf %*lf %*lf %lf", &val_x);
      break;
    case 7:
      ival_x = sscanf(buffer, "%*lf %*lf %*lf %*lf %*lf %*lf %lf", &val_x);
      break;
    case 8:
      ival_x = sscanf(buffer, "%*lf %*lf %*lf %*lf %*lf %*lf %*lf %lf", &val_x);
      break;
    case 9:
      ival_x = sscanf(buffer, "%*lf %*lf %*lf %*lf %*lf %*lf %*lf %*lf %lf", &val_x);
      break;
    case 10:
      ival_x = sscanf(buffer, "%*lf %*lf %*lf %*lf %*lf %*lf %*lf %*lf %*lf %lf", &val_x);
      break;
    default:
      fprintf(stderr,"read_one_col/Error: icol = %d <= 0 or larger than maximum allowed of 10 columns ! \n", icol_x);
      status = -2;
      break;
    }

*xvalue = val_x;

if(ival_x != 1) status = -1;

return(status);
}
