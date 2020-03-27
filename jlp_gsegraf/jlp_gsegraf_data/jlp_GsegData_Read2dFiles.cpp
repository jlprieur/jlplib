/*******************************************************************************
*
* jlp_GsegData_Read2dFiles.cpp
*
* Contains functions:
*    Read2dFiles
*    sort_compare_double
*
* Reads 2d data files.
*
* Copyright © 2008, 2009, 2010, 2011 Spencer A. Buckner
* http://savannah.gnu.org/projects/gsegrafix
*
* This file is part of GSEGrafix, a scientific and engineering plotting program.
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program.  If not, see <http://www.gnu.org/licenses/>.
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

// #define DEBUG

static int sort_compare_double ( const void *p1, const void *p2 );

/**************************************************************************
*
****************************************************************************/
void JLP_GsegData::Read2dFiles(char *axis_type0)
{
/* Declare variables */
int i, j, iplot, npts, 
    nx, ny, ndouble0, nchar0, ichar0, nlinebreaks0;
unsigned int size;
double x, y;
char fname0[256], *string = NULL, ast1, ast2;
const char *error_str[] =
   { "Error reading contour-plot data file.",
     "Error reading color-plot data file.",
     "Not enough memory for data.",
     "Not enough memory for line-break data." };
FILE *fptr;

// If data was directly input from GSEG_PLOT_DATA structure,
// filenames are not relevant for each iplot, since data is already loaded
// so I exit from here:
 if(gsegdata_from_paramfile == 0) return;

/* JLP 2016 to solve problem of numeric dot vs numeric comma */
/* SHOULD BE CALLED HERE...*/
/* In the "C" locale, a decimal_point is "." */
if (NULL == setlocale(LC_NUMERIC, "C")) {
  fprintf(stderr, " Error setting numeric dot in setlocale !\n");
  exit(-1);
}

/***************************** First reading : ****************************
*** Get sizes of data files 
***************************************************************************/
 for ( iplot=1; iplot<=nplots1; iplot++ )
    {
/* Open data file */
    strcpy(fname0, gseg_plotdata1[iplot].filename);
    if ( (fptr = fopen( fname0, "r")) == NULL )
       {
       size = strlen("Read2dFiles/Cannot open data file:\n") + strlen(fname0);
       string = new char[size + 1];
       sprintf(string, "%s%s", "Read2dFiles/Cannot open data file:\n", fname0);
       JLP_ErrorDialog(string);
       delete[] string;
       exit(1);
        }

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
           }

// File format: gseg_plot_type = 2 "histogram"
         else if ( gseg_plotdata1[iplot].gseg_plot_type == 2 )
            {
            i = 0;
            while ( fscanf(fptr, gseg_plotdata1[iplot].prnt_formats, &x) == 1 )
               i++;
            npts = i;
            gseg_plotdata1[iplot].npts = npts;
            }

// File format: gseg_plot_type = 3 "contour"
         else if ( gseg_plotdata1[iplot].gseg_plot_type == 3 )
            {
// Read first line:
            if ( fscanf(fptr, "%d %d", &nx, &ny) != 2 )
               {
               JLP_ErrorDialog(error_str[0]);
               exit(1);
               }
            gseg_plotdata1[iplot].nxcontour = nx;
            gseg_plotdata1[iplot].nycontour = ny;
#ifdef DEBUG
printf("Read2dFiles/Debug: reading %s\n nxcontour=%d nycontour=%d \n", 
        gseg_plotdata1[iplot].filename, nx, ny);
#endif

            }

// File format: gseg_plot_type = 4 "color"
         else if ( gseg_plotdata1[iplot].gseg_plot_type == 4 )
            {
            if ( fscanf(fptr, "%d %d", &nx, &ny) != 2 )
               {
               JLP_ErrorDialog(error_str[1]);
               exit(1);
               }
            gseg_plotdata1[iplot].nxcolor = nx;
            gseg_plotdata1[iplot].nycolor = ny;
            }
         }

      fclose(fptr);
      } // EOF loop on iplot

/**************************************************************************
*************** Allocate memory
**************************************************************************/
 for(iplot = 1; iplot <= nplots1; iplot++) { 
   npts = gseg_plotdata1[iplot].npts;
// Allocate memory for points- and histogram-data arrays
   if(npts > 0) {
      if( (gseg_plotdata1[iplot].xdata = new double[npts]) == NULL ||
          (gseg_plotdata1[iplot].ydata = new double[npts]) == NULL)
       {
       JLP_ErrorDialog(error_str[2]);
       exit(1);
       }
    }
// Allocate memory for contour-plot data arrays 
    nx = gseg_plotdata1[iplot].nxcontour;
    ny = gseg_plotdata1[iplot].nycontour;
    if((nx > 0) && (ny >0)) {
        if((gseg_plotdata1[iplot].xcontour = new double[nx]) == NULL ||
           (gseg_plotdata1[iplot].ycontour = new double[ny]) == NULL ||
           (gseg_plotdata1[iplot].zcontour = new double[nx * ny]) == NULL)
        {
        JLP_ErrorDialog(error_str[2]);
        exit(1);
        }
     }
// Allocate memory for color-plot data arrays
     nx = gseg_plotdata1[iplot].nxcolor;
     ny = gseg_plotdata1[iplot].nycolor;
     if((nx > 0) && (ny >0)) {
       if((gseg_plotdata1[iplot].xcolor = new double[nx]) == NULL ||
          (gseg_plotdata1[iplot].ycolor = new double[ny]) == NULL ||
          (gseg_plotdata1[iplot].zcolor = new double[nx * ny]) == NULL)
        {
        JLP_ErrorDialog(error_str[2]);
        exit(1);
        }
     }
// Allocate memory for line-break index array
     nlinebreaks0 = gseg_plotdata1[iplot].nlinebreaks;
     if ( nlinebreaks0 > 0 ) {
       if((gseg_plotdata1[iplot].linebreak = new int[nlinebreaks0]) == NULL) 
        {
        JLP_ErrorDialog(error_str[3]);
        exit(1);
        }
      }
 } // EOF loop on iplot

/***************************** Second reading : ****************************
****** Read data files 
***************************************************************************/
 for ( iplot=1; iplot<=nplots1; iplot++ )
    {
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
            }

/* Modify data for logarithmic axes */
          if ( strcmp(axis_type0, "semilogx") == 0 ||
               strcmp(axis_type0, "loglog")   == 0 )
             for ( i=1; i<=npts; i++ )
                {
                if ( gseg_plotdata1[iplot].xdata[i-1] > 0.0 )
                   gseg_plotdata1[iplot].xdata[i-1] = log10(gseg_plotdata1[iplot].xdata[i-1]);
                else
                   gseg_plotdata1[iplot].xdata[i-1] = log10(DBLE_MIN_VALUE);
                }

          if ( strcmp(axis_type0, "semilogy") == 0 ||
               strcmp(axis_type0, "loglog")   == 0 )
             for ( i=1; i<=npts; i++ )
                {
                if ( gseg_plotdata1[iplot].ydata[i-1] > 0.0 )
                   gseg_plotdata1[iplot].ydata[i-1] = log10(gseg_plotdata1[iplot].ydata[i-1]);
                else
                   gseg_plotdata1[iplot].ydata[i-1] = log10(DBLE_MIN_VALUE);
                }

          }

// File format: gseg_plot_type = 2 "histogram"
       else if(gseg_plotdata1[iplot].gseg_plot_type == 2)
          {
// Read first line again but do not use those values:
          if ( fscanf(fptr, "%d", &npts) != 1 ) {
            fprintf(stderr, "Read2dFiles/Error reading the first line the second time\n");
            return;
            }
// Read the data:
          npts = gseg_plotdata1[iplot].npts;
          for ( i=1; i<=npts; i++ )
             {
             fscanf(fptr, gseg_plotdata1[iplot].prnt_formats, 
                    &(gseg_plotdata1[iplot].xdata[i-1]));
             gseg_plotdata1[iplot].ydata[i-1] = 0.0;
             }

          qsort(gseg_plotdata1[iplot].xdata, npts, sizeof(double), 
                sort_compare_double);
          }

// File format: gseg_plot_type = 3 "contour"
       else if(gseg_plotdata1[iplot].gseg_plot_type == 3)
          {
// Read first line again but do not use those values:
          if ( fscanf(fptr, "%d %d", &nx, &ny) != 2 ) {
            fprintf(stderr, "Read2dFiles/Error reading the first line the second time\n");
            return;
            }
// Read the data:
          nx = gseg_plotdata1[iplot].nxcontour;
          ny = gseg_plotdata1[iplot].nycontour;

          for ( i=1; i<=nx; i++ )
             if ( fscanf(fptr, "%lf", &(gseg_plotdata1[iplot].xcontour[i-1])) != 1 )
                {
                JLP_ErrorDialog(error_str[0]);
                exit(1);
                }

          for ( j=1; j<=ny; j++ )
             if ( fscanf(fptr, "%lf", &(gseg_plotdata1[iplot].ycontour[j-1])) != 1 )
                {
                JLP_ErrorDialog(error_str[0]);
                exit(1);
                }

          for ( i=1; i<=nx; i++ )
             for ( j=1; j<=ny; j++ )
                if(fscanf(fptr, "%lf", &(gseg_plotdata1[iplot].zcontour[ny*(i-1) +j-1])) != 1 )
                   {
                   JLP_ErrorDialog(error_str[0]);
                   exit(1);
                   }

#ifdef DEBUG
printf("Read2dFiles/Debug:  OK: all data successfully read\n");
#endif
          }

// File format: gseg_plot_type = 4 "color"
       else if(gseg_plotdata1[iplot].gseg_plot_type == 4)
          {
// Read first line again but do not use those values:
          if ( fscanf(fptr, "%d %d", &nx, &ny) != 2 ) {
            fprintf(stderr, "Read2dFiles/Error reading the first line the second time\n");
            return;
            }
// Read the data:
          nx = gseg_plotdata1[iplot].nxcolor;
          ny = gseg_plotdata1[iplot].nycolor;

          for ( i=1; i<=nx; i++ )
             if ( fscanf(fptr, "%lf", &(gseg_plotdata1[iplot].xcolor[i-1])) != 1 )
                {
                JLP_ErrorDialog(error_str[1]);
                exit(1);
                }

#ifdef DEBUG
printf("Read2dFiles/Debug: xcolor0=%f\n", gseg_plotdata1[iplot].xcolor[0]);
printf("Read2dFiles/Debug: xcolor (maxi)=%f\n", gseg_plotdata1[iplot].xcolor[nx - 1]);
#endif

          for ( j=1; j<=ny; j++ )
             if ( fscanf(fptr, "%lf", &(gseg_plotdata1[iplot].ycolor[j-1])) != 1 )
                {
                JLP_ErrorDialog(error_str[1]);
                exit(1);
                }

#ifdef DEBUG
printf("Read2dFiles/Debug: ycolor0=%f\n", gseg_plotdata1[iplot].ycolor[0]);
printf("Read2dFiles/Debug: ycolor (maxi)=%f\n", gseg_plotdata1[iplot].ycolor[ny - 1]);
#endif

          for ( i=1; i<=nx; i++ )
             for ( j=1; j<=ny; j++ )
                if ( fscanf(fptr, "%lf", &(gseg_plotdata1[iplot].zcolor[ny*(i-1)+j-1])) != 1 )
                   {
                   JLP_ErrorDialog(error_str[1]);
                   exit(1);
                   }
          }
       }

   fclose(fptr);
   }

return;
}
/**************************************************************************
*
**************************************************************************/
static int sort_compare_double ( const void *p1, const void *p2 )
   {
   double key = *(const double *) p1,
          x   = *(const double *) p2;

   if ( key < x )
      return -1;
   else if ( key == x )
      return 0;
   else
      return 1;
   }
