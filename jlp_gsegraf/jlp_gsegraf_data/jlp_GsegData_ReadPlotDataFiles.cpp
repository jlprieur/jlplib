/*******************************************************************************
*
* jlp_GsegData_ReadPlotDataFiles.cpp
*
*
* JLP
* Version 15/05/2017
*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <locale.h>
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_data.h"    // JLP_GsegData class
#include "jlp_fitsio.h"         // for JLP_RDFITS_2D

// #define DEBUG

/**************************************************************************
*
****************************************************************************/
int JLP_GsegData::LoadCurvePlotDataFromFile(char *filename0, 
                                            const int reset_first0,
                                            char *axis_type0)
{
/* Declare variables */
int i, iplot, npts, ndouble0, nchar0, nlinebreaks0;
unsigned int size;
double x, y;
char *string = NULL, ast1, ast2, ichar0;
const char *error_str[] =
   { "Error reading contour-plot data file.",
     "Error reading color-plot data file.",
     "Not enough memory for data.",
     "Not enough memory for line-break data." };
FILE *fptr;

/* JLP 2016 to solve problem of numeric dot vs numeric comma */
/* SHOULD BE CALLED HERE...*/
/* In the "C" locale, a decimal_point is "." */
if (NULL == setlocale(LC_NUMERIC, "C")) {
  fprintf(stderr, "LoadCurvePlotDataFromFile/Error setting numeric dot in setlocale !\n");
  exit(-1);
}

// Increase nplots1 if needed:
if(reset_first0 == 1) {
// Index starts at iplot=1 !
 iplot = 1;
 } else {
 nplots1++;
 iplot = nplots1;
 }

/***************************** First reading : ****************************
*** Get sizes of data files
***************************************************************************/
strcpy(gseg_plotdata1[iplot].filename, filename0);
/* Open data file */
  if ( (fptr = fopen( filename0, "r")) == NULL )
     {
     size = strlen("LoadCurvePlotDataFromFile/Cannot open data file:\n") + strlen(filename0);
     string = new char[size + 1];
     sprintf(string, "%s%s", "LoadCurvePlotDataFromFile/Cannot open data file:\n", filename0);
     JLP_ErrorDialog(string);
     delete[] string;
     if(nplots1 >= 1) nplots1--;
     return(1);
     }

// Set gseg_plot_type = 1 to "points"
gseg_plotdata1[iplot].gseg_plot_type = 1;

strcpy(gseg_plotdata1[iplot].prnt_formats, "\%lf \%lf");

/* Get size of data file */
    if ( strcmp(axis_type0, "linear")   == 0 ||
         strcmp(axis_type0, "semilogx") == 0 ||
         strcmp(axis_type0, "semilogy") == 0 ||
         strcmp(axis_type0, "loglog")   == 0 ||
         strcmp(axis_type0, "polar")    == 0 )
       {

// File format: gseg_plot_type = 1 "points"
       if ( gseg_plotdata1[iplot].gseg_plot_type == 1 )
         {
         i = 0;
         nlinebreaks0 = 0;
         ndouble0 = fscanf(fptr, gseg_plotdata1[iplot].prnt_formats, &x, &y);
         nchar0 = fscanf(fptr, gseg_plotdata1[iplot].prnt_mod_formats,
                                  &ast1, &ast2);
         while(ndouble0 == 2 || (nchar0 == 2 && ast1 == '*' && ast2 == '*') )
            {
#ifdef DEBUG
printf("Read2dFile/Debug: i=%d x=%f y=%f nlinebreaks=%d ast1=%c ast2=%c\n",
       i, x, y, nlinebreaks0, ast1, ast2);
#endif
            if ( ndouble0 == 2 )
               {
               i++;
               ndouble0 = 0;
               }
            else if ( nchar0 == 2 )
               {
               nlinebreaks0++;
               nchar0 = 0;
               }
            ndouble0 = fscanf(fptr, gseg_plotdata1[iplot].prnt_formats, &x, &y);
            nchar0 = fscanf(fptr, gseg_plotdata1[iplot].prnt_mod_formats,
                            &ast1, &ast2);
            }
           npts = i;
           gseg_plotdata1[iplot].nlinebreaks = nlinebreaks0;
           gseg_plotdata1[iplot].npts = npts;

       } else {
        fprintf(stderr, "LoadCurvePlotDataFromFile/bad axis_type : %s not allowed here !\n",
                axis_type0);
        if(nplots1 >= 1) nplots1--;
        return(-1);
       } // EOF test on gseg_plot_type
    }  // EOF case axis_type0...

   fclose(fptr);

/**************************************************************************
*************** Allocate memory
**************************************************************************/
 npts = gseg_plotdata1[iplot].npts;
// Allocate memory for points- and histogram-data arrays
 if(npts > 0) {
    if( (gseg_plotdata1[iplot].xdata = new double[npts]) == NULL ||
        (gseg_plotdata1[iplot].ydata = new double[npts]) == NULL)
     {
     JLP_ErrorDialog(error_str[2]);
     if(nplots1 >= 1) nplots1--;
     return(1);
     }
  }

// Allocate memory for line-break index array
  nlinebreaks0 = gseg_plotdata1[iplot].nlinebreaks;
  if(nlinebreaks0 > 0) {
     if((gseg_plotdata1[iplot].linebreak = new int[nlinebreaks0]) == NULL)
     {
     JLP_ErrorDialog(error_str[3]);
     if(nplots1 >= 1) nplots1--;
      return(1);
     }
   }

/***************************** Second reading : ****************************
****** Read data files
***************************************************************************/
/* Open data file */
  fptr = fopen(gseg_plotdata1[iplot].filename, "r");

/* Read data file */
   if ( strcmp(axis_type0, "linear")   == 0 ||
        strcmp(axis_type0, "semilogx") == 0 ||
        strcmp(axis_type0, "semilogy") == 0 ||
        strcmp(axis_type0, "loglog")   == 0 ||
        strcmp(axis_type0, "polar")    == 0 )
      {
// File format: gseg_plot_type = 1 "points"
      if(gseg_plotdata1[iplot].gseg_plot_type == 1)
         {
// Read the data:
         npts = gseg_plotdata1[iplot].npts;
         ndouble0 = 0;
         nchar0 = 0;
         ichar0 = 0;
         for ( i=1; i<=npts; i++ ) {
            if((ndouble0 = fscanf(fptr, gseg_plotdata1[iplot].prnt_formats,
                              &(gseg_plotdata1[iplot].xdata[i-1]),
                              &(gseg_plotdata1[iplot].ydata[i-1]))) == 2 ||
                (nchar0 = fscanf(fptr, gseg_plotdata1[iplot].prnt_mod_formats,
                                 &ast1, &ast2)) == 2)
               {
               if(ndouble0 != 2 && nchar0 == 2)
                  {
                  ichar0++;
                  gseg_plotdata1[iplot].linebreak[ichar0-1] = i - 1;
                  nchar0 = 0;
                  i--;
                  }
#ifdef DEBUG
               else
printf("Read2dFiles/Debug:  (format=%s) x[%d]=%f y[%d]=%f\n",
        gseg_plotdata1[iplot].prnt_formats, i-1,
        gseg_plotdata1[iplot].xdata[i-1], i-1,
        gseg_plotdata1[iplot].ydata[i-1]);
#endif
               }
           }  // EOF loop on i


/* Modify data for logarithmic axes */
          if ( strcmp(axis_type0, "semilogx") == 0 ||
               strcmp(axis_type0, "loglog")   == 0 )
             for ( i=1; i<=npts; i++ )
                {
                if ( gseg_plotdata1[iplot].xdata[i-1] != 0.0 )
                   gseg_plotdata1[iplot].xdata[i-1] = log10(fabs(gseg_plotdata1[iplot].xdata[i-1]));
                else
                   gseg_plotdata1[iplot].xdata[i-1] = log10(DBLE_MIN_VALUE);
                }  // EOF loop on i

          if ( strcmp(axis_type0, "semilogy") == 0 ||
               strcmp(axis_type0, "loglog")   == 0 )
             for ( i=1; i<=npts; i++ )
                {
                if ( gseg_plotdata1[iplot].ydata[i-1] != 0.0 )
                   gseg_plotdata1[iplot].ydata[i-1] = log10(fabs(gseg_plotdata1[iplot].ydata[i-1]));
                else
                   gseg_plotdata1[iplot].ydata[i-1] = log10(DBLE_MIN_VALUE);
                }  // EOF loop on i

          } // EOF case gseg_plot_type == 1
    } // EOF case axis_type...

   fclose(fptr);

return(0);
}
/**************************************************************************
*
****************************************************************************/
int JLP_GsegData::LoadImagePlotDataFromFitsFile(char *filename0, 
                                                const int reset_first0,
                                                char *axis_type0)
{
/* Declare variables */
int status, iframe, m_ihdu, i, j, iplot, nx0, ny0, nz0;
char comments0[80], errmess0[200];
double *dble_image0;
const char *error_str[] =
   { "Error reading contour-plot data file.",
     "Error reading color-plot data file.",
     "Not enough memory for data."};

/* JLP 2016 to solve problem of numeric dot vs numeric comma */
/* SHOULD BE CALLED HERE...*/
/* In the "C" locale, a decimal_point is "." */
if (NULL == setlocale(LC_NUMERIC, "C")) {
  fprintf(stderr, "LoadImagePlotDataFromFitsFile/Error setting numeric dot in setlocale !\n");
  return(-2);
}

// Read FITS file:

// nx0, ny0, nz0: size of data cube
// dble_image1: array with the data contained in FITS file
// iframe: index of image plane to be loaded from 1 to nz (in case of 3D data)
iframe = 1;
m_ihdu = 1;

// Load a new image or image plane in FITS format
status = JLP_RD_3DXFITS_2D_dble(&dble_image0, &nx0, &ny0, &nz0, iframe, m_ihdu,
                                filename0, comments0, errmess0);
if (status) {
  fprintf(stderr,"Couldn't load image from %s : %s\n", filename0, errmess0); 
  return(-2);
  }

// Increase nplots1 if needed:
// Index starts at iplot=1 !
if(reset_first0 == 1) {
 iplot = 1;
 } else {
 nplots1++;
 iplot = nplots1;
 }

// Copy data to private variables:
 strcpy(gseg_plotdata1[iplot].filename, filename0);
 if((nx0 < 0) || (ny0 < 0)) {
  fprintf(stderr, "LoadImagePlotDataFromFitsFile/Error: nx=%d ny=%d\n",
          nx0, ny0);
  return(-1);
  }

// File format: gseg_plot_type = 4 "color"
  gseg_plotdata1[iplot].gseg_plot_type = 4;
  gseg_plotdata1[iplot].nxcolor = nx0;
  gseg_plotdata1[iplot].nycolor = ny0;

// Free memory:
  if(gseg_plotdata1[iplot].xcolor != NULL ) {
    delete[] gseg_plotdata1[iplot].xcolor;
    gseg_plotdata1[iplot].xcolor = NULL;
    }
  if(gseg_plotdata1[iplot].ycolor != NULL ) {
    delete[] gseg_plotdata1[iplot].ycolor;
    gseg_plotdata1[iplot].ycolor = NULL;
    }
  if(gseg_plotdata1[iplot].zcolor != NULL ) {
    delete[] gseg_plotdata1[iplot].zcolor;
    gseg_plotdata1[iplot].zcolor = NULL;
    }

// Allocate memory for color-plot data arrays
  if((gseg_plotdata1[iplot].xcolor = new double[nx0]) == NULL ||
     (gseg_plotdata1[iplot].ycolor = new double[ny0]) == NULL ||
      (gseg_plotdata1[iplot].zcolor = new double[nx0 * ny0]) == NULL)
    {
    JLP_ErrorDialog(error_str[2]);
    exit(1);
    }

// Transfer to private array:
  for(i = 0; i < nx0; i++) 
     gseg_plotdata1[iplot].xcolor[i] = (double)i / (nx0 - 1);
  for(j = 0; j < ny0; j++) 
     gseg_plotdata1[iplot].ycolor[j] = (double)j / (ny0 - 1);
  for(j = 0; j < ny0; j++) 
  for(i = 0; i < nx0; i++) 
     gseg_plotdata1[iplot].zcolor[i + j * nx0] = dble_image0[i + j * nx0];

// Free memory:
delete[] dble_image0;

return(0);
}
