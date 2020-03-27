/*******************************************************************************
*
* jlp_GsegData_Read3dFiles.cpp
*
* Reads 3d data files.
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
*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <locale.h>

#include "jlp_gsegraf.h"
#include "jlp_gseg_data.h"

/*****************************************************************************
*
*****************************************************************************/
void JLP_GsegData::Read3dFiles (void)
{
/* Declare variables */
int i, j, iplot, npts, nx, ny, ndouble0, nchar0, ichar0, nlinebreaks0;
unsigned int size;
double x, y, z;
char fname0[256], *string = NULL, ast1, ast2, ast3;
const char *error_str[] = { "Error reading mesh data file.",
     "Error reading contour data file.",
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
if(setlocale(LC_NUMERIC, "C") == NULL) {
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
   if ( (fptr = fopen(fname0, "r")) == NULL )
     {
     size = strlen("Read3dFiles/Cannot open data file:\n") + strlen(fname0);
     string = new char[size + 32];
     sprintf(string, "%s %s (iplot=%d)", 
             "Read3dFiles/Cannot open data file:\n", fname0, iplot);
     JLP_ErrorDialog(string);
     delete[] string;
     exit(1);
     }

/* Get size of data file */
// File format: gseg_plot_type = 1 "points"
   if ( gseg_plotdata1[iplot].gseg_plot_type == 1 )
     {
     i = 0;
     ndouble0 = 0;
     nlinebreaks0 = 0;
     nchar0 = 0;
     while ( (ndouble0 = fscanf(fptr, gseg_plotdata1[iplot].prnt_formats, 
                                &x, &y, &z)) == 3 ||
             ((nchar0 = fscanf(fptr, gseg_plotdata1[iplot].prnt_mod_formats,
                               &ast1, &ast2, &ast3)) == 3 &&
                ast1 == '*' && ast2 == '*' && ast3 == '*') )
         {
         if ( ndouble0 == 3 )
            {
            i++;
            ndouble0 = 0;
            }
         else if ( nchar0 == 3 )
            {
            nlinebreaks0++;
            nchar0 = 0;
            }
         }
      npts = i;
      gseg_plotdata1[iplot].nlinebreaks = nlinebreaks0;
      gseg_plotdata1[iplot].npts = npts;
      }

// File format: gseg_plot_type = 5 "mesh"
   else if ( gseg_plotdata1[iplot].gseg_plot_type == 5 )
      {
      if ( fscanf(fptr, "%d %d", &nx, &ny) != 2 )
         {
         JLP_ErrorDialog(error_str[0]);
         exit(1);
         }
      gseg_plotdata1[iplot].nxmesh = nx;
      gseg_plotdata1[iplot].nymesh = ny;
      }
// File format: gseg_plot_type = 3 "contour"
   else if(gseg_plotdata1[iplot].gseg_plot_type == 3)
      {
      if ( fscanf(fptr, "%d %d", &nx, &ny) != 2 )
         {
         JLP_ErrorDialog(error_str[1]);
         exit(1);
         }
      gseg_plotdata1[iplot].nxcontour = nx;
      gseg_plotdata1[iplot].nycontour = ny;
      }

// File format: gseg_plot_type = 4 "color"
   else if(gseg_plotdata1[iplot].gseg_plot_type == 4)
      {
      if ( fscanf(fptr, "%d %d", &nx, &ny) != 2 )
         {
         JLP_ErrorDialog(error_str[1]);
         exit(1);
         }
      gseg_plotdata1[iplot].nxcolor = nx;
      gseg_plotdata1[iplot].nycolor = ny;
      }

   fclose(fptr);
   }

/**************************************************************************
*************** Allocate memory
**************************************************************************/
 for(iplot = 1; iplot <= nplots1; iplot++) {
   npts = gseg_plotdata1[iplot].npts;
// Allocate memory for points data arrays
   if(npts > 0) {
      if( (gseg_plotdata1[iplot].xdata = new double[npts]) == NULL ||
          (gseg_plotdata1[iplot].ydata = new double[npts]) == NULL ||
          (gseg_plotdata1[iplot].zdata = new double[npts]) == NULL)
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
/* Allocate memory for mesh-data arrays */
     nx = gseg_plotdata1[iplot].nxmesh;
     ny = gseg_plotdata1[iplot].nymesh;
     if((nx > 0) && (ny >0)) {
       if((gseg_plotdata1[iplot].xmesh = new double[nx]) == NULL ||
          (gseg_plotdata1[iplot].ymesh = new double[ny]) == NULL ||
          (gseg_plotdata1[iplot].zmesh = new double[nx * ny]) == NULL)
        {
        JLP_ErrorDialog(error_str[2]);
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
// File format: gseg_plot_type = 1 "points"
   if(gseg_plotdata1[iplot].gseg_plot_type == 1)
     {
// Read the data:
     npts = gseg_plotdata1[iplot].npts;
     ndouble0 = 0;
     nchar0 = 0;
     ichar0 = 0;
     for ( i=1; i<=npts; i++ )
        if ( (ndouble0 = fscanf(fptr, gseg_plotdata1[iplot].prnt_formats, 
                             &(gseg_plotdata1[iplot].xdata[i-1]),
                             &(gseg_plotdata1[iplot].ydata[i-1]),
                             &(gseg_plotdata1[iplot].zdata[i-1]))) == 3 ||
           (nchar0 = fscanf(fptr, gseg_plotdata1[iplot].prnt_mod_formats,
                            &ast1, &ast2, &ast3)) == 3 )
           {
           if ( ndouble0 != 3 && nchar0 == 3 )
             {
             ichar0++;
             gseg_plotdata1[iplot].linebreak[ichar0-1] = i - 1;
             nchar0 = 0;
             i--;
             }
           }
      }

// File format: gseg_plot_type = 5 "mesh"
   else if ( gseg_plotdata1[iplot].gseg_plot_type == 5 )
      {
// Read first line again but do not use those values:
      if ( fscanf(fptr, "%d %d", &nx, &ny) != 2 ) {
        fprintf(stderr, "Read2dFiles/Error reading the first line the second time\n");
        return;
        }
// Read the data:
      nx = gseg_plotdata1[iplot].nxmesh;
      ny = gseg_plotdata1[iplot].nymesh;

      for ( i=1; i<=nx; i++ )
         if ( fscanf(fptr, "%lf", &(gseg_plotdata1[iplot].xmesh[i-1])) != 1 )
            {
            JLP_ErrorDialog(error_str[0]);
            exit(1);
            }

      for ( j=1; j<=ny; j++ )
         if ( fscanf(fptr, "%lf", &(gseg_plotdata1[iplot].ymesh[j-1])) != 1 )
            {
            JLP_ErrorDialog(error_str[0]);
            exit(1);
            }

      for ( i=1; i<=nx; i++ )
         for ( j=1; j<=ny; j++ )
            if ( fscanf(fptr, "%lf", &(gseg_plotdata1[iplot].zmesh[ny*(i-1)+j-1])) != 1 )
               {
               JLP_ErrorDialog(error_str[0]);
               exit(1);
               }

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
            JLP_ErrorDialog(error_str[1]);
            exit(1);
            }

      for ( j=1; j<=ny; j++ )
         if ( fscanf(fptr, "%lf", &(gseg_plotdata1[iplot].ycontour[j-1])) != 1 )
            {
            JLP_ErrorDialog(error_str[1]);
            exit(1);
            }

      for ( i=1; i<=nx; i++ )
         for ( j=1; j<=ny; j++ )
            if ( fscanf(fptr, "%lf", &(gseg_plotdata1[iplot].zcontour[ny*(i-1)+j-1])) != 1 )
               {
               JLP_ErrorDialog(error_str[1]);
               exit(1);
               }

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

      for ( j=1; j<=ny; j++ )
         if ( fscanf(fptr, "%lf", &(gseg_plotdata1[iplot].ycolor[j-1])) != 1 )
            {
            JLP_ErrorDialog(error_str[1]);
            exit(1);
            }

      for ( i=1; i<=nx; i++ )
         for ( j=1; j<=ny; j++ )
            if ( fscanf(fptr, "%lf", &(gseg_plotdata1[iplot].zcolor[ny*(i-1)+j-1])) != 1 )
              {
              JLP_ErrorDialog(error_str[1]);
              exit(1);
              }

      }

   fclose(fptr);
   }

return;
}
