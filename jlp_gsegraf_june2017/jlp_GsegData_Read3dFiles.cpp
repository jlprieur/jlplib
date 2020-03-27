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
int i, j, iplot, ipoints, imesh, icontour, icolor, npts, npts_total,
    npts_total_xmesh, npts_total_ymesh, npts_total_zmesh,
    npts_total_xcontour, npts_total_ycontour, npts_total_zcontour,
    npts_total_xcolor, npts_total_ycolor, npts_total_zcolor,
    index_filenames, index_formats, index_plot_types,
    index0, nx, ny, xindex_mesh, yindex_mesh, zindex_mesh,
    xindex_contour, yindex_contour, zindex_contour,
    xindex_color, yindex_color, zindex_color,
    ndouble, nchar, ichar;
unsigned int size;
double x, y, z;
char *string = NULL, ast1, ast2, ast3;
const char *error_str[] = { "Error reading mesh data file.",
     "Error reading contour data file.",
     "Not enough memory for data.",
     "Not enough memory for line-break data." };
FILE *fptr;

/* JLP 2016 to solve problem of numeric dot vs numeric comma */
/* SHOULD BE CALLED HERE...*/
/* In the "C" locale, a decimal_point is "." */
if(setlocale(LC_NUMERIC, "C") == NULL) {
  fprintf(stderr, " Error setting numeric dot in setlocale !\n");
  exit(-1);
}

/* Get sizes of data files */
   npts_total = 0;
   npts_total_xmesh = 0;
   npts_total_ymesh = 0;
   npts_total_zmesh = 0;
   npts_total_xcontour = 0;
   npts_total_ycontour = 0;
   npts_total_zcontour = 0;
   npts_total_xcolor = 0;
   npts_total_ycolor = 0;
   npts_total_zcolor = 0;
   index_filenames = 0;
   index_formats = 0;
   index_plot_types = 0;
   ipoints = 0;
   imesh = 0;
   icontour = 0;
   icolor = 0;
   nlinebreaks = 0;
   for ( iplot=1; iplot<=nplots1; iplot++ )
      {
      /* Open data file */
      if ( (fptr = fopen( &filenames[index_filenames], "r")) == NULL )
         {
         size = strlen("Cannot open data file:\n") + strlen(&filenames[index_filenames]);
         string = new char[size + 1];
         sprintf(string, "%s%s", "Cannot open data file:\n", &filenames[index_filenames]);
         JLP_ErrorDialog(string);
         delete[] string;
         exit(1);
         }


      /* Get size of data file */
      if ( strcmp(&plot_types[index_plot_types], "points") == 0 )
         {
         ipoints++;
         i = 0;
         ndouble = 0;
         nchar = 0;
         while ( (ndouble = fscanf(fptr, &formats[index_formats], &x, &y, &z)) == 3 ||
                 ((nchar = fscanf(fptr, &formats_mod[index_formats], &ast1, &ast2, &ast3)) == 3 &&
                   ast1 == '*' && ast2 == '*' && ast3 == '*') )
            {
            if ( ndouble == 3 )
               {
               i++;
               ndouble = 0;
               }
            else if ( nchar == 3 )
               {
               nlinebreaks++;
               nchar = 0;
               }
            }
         index_formats = index_formats + nformats[ipoints-1];
         npts = i;
         ndata[ipoints-1] = npts;
         npts_total = npts_total + npts;
         }

      else if ( strcmp(&plot_types[index_plot_types], "mesh") == 0 )
         {
         imesh++;
         if ( fscanf(fptr, "%d %d", &nx, &ny) != 2 )
            {
            JLP_ErrorDialog(error_str[0]);
            exit(1);
            }
         npts_total_xmesh = npts_total_xmesh + nx;
         npts_total_ymesh = npts_total_ymesh + ny;
         npts_total_zmesh = npts_total_zmesh + nx*ny;
         }

      else if ( strcmp(&plot_types[index_plot_types], "contour") == 0 )
         {
         icontour++;
         if ( fscanf(fptr, "%d %d", &nx, &ny) != 2 )
            {
            JLP_ErrorDialog(error_str[1]);
            exit(1);
            }
         npts_total_xcontour = npts_total_xcontour + nx;
         npts_total_ycontour = npts_total_ycontour + ny;
         npts_total_zcontour = npts_total_zcontour + nx*ny;
         }

      else if ( strcmp(&plot_types[index_plot_types], "color") == 0 )
         {
         icolor++;
         if ( fscanf(fptr, "%d %d", &nx, &ny) != 2 )
            {
            JLP_ErrorDialog(error_str[1]);
            exit(1);
            }
         npts_total_xcolor = npts_total_xcolor + nx;
         npts_total_ycolor = npts_total_ycolor + ny;
         npts_total_zcolor = npts_total_zcolor + nx*ny;
         }


      /* Increment indices */
      fclose(fptr);
      index_filenames = index_filenames + nfilenames[iplot-1];
      index_plot_types = index_plot_types + 10;
      }


   /* Allocate memory for points-data arrays */
   if ( ipoints > 0 )
      if ( (xdata = new double[npts_total]) == NULL ||
           (ydata = new double[npts_total]) == NULL ||
           (zdata = new double[npts_total]) == NULL )
         {
         JLP_ErrorDialog(error_str[2]);
         exit(1);
         }


   /* Allocate memory for mesh-data arrays */
   if ( imesh > 0 )
      if ( (nxmesh = new int[imesh]) == NULL ||
           (nymesh = new int[imesh]) == NULL ||
           (xmesh = new double[npts_total_xmesh]) == NULL ||
           (ymesh = new double[npts_total_ymesh]) == NULL ||
           (zmesh = new double[npts_total_zmesh]) == NULL )
         {
         JLP_ErrorDialog(error_str[2]);
         exit(1);
         }


   /* Allocate memory for contour-data arrays */
   if ( icontour > 0 )
      if ( (nxcontour = new int[icontour]) == NULL ||
           (nycontour = new int[icontour]) == NULL ||
           (xcontour = new double[npts_total_xcontour]) == NULL ||
           (ycontour = new double[npts_total_ycontour]) == NULL ||
           (zcontour = new double[npts_total_zcontour]) == NULL )
         {
         JLP_ErrorDialog(error_str[2]);
         exit(1);
         }


   /* Allocate memory for color-data arrays */
   if ( icolor > 0 )
      if ( (nxcolor = new int[icolor]) == NULL ||
           (nycolor = new int[icolor]) == NULL ||
           (xcolor = new double[npts_total_xcolor]) == NULL ||
           (ycolor = new double[npts_total_ycolor]) == NULL ||
           (zcolor = new double[npts_total_zcolor]) == NULL )
         {
         JLP_ErrorDialog(error_str[2]);
         exit(1);
         }


   /* Allocate memory for line-break index array */
   nlinebreak = NULL;
   if ( nlinebreaks > 0 )
      if ( (nlinebreak = new int[nlinebreaks]) == NULL )
         {
         JLP_ErrorDialog(error_str[3]);
         exit(1);
         }


   /* Read data files */
   index_filenames = 0;
   index_formats = 0;
   index_plot_types = 0;
   index0 = 0;
   xindex_mesh = 0;
   yindex_mesh = 0;
   zindex_mesh = 0;
   xindex_contour = 0;
   yindex_contour = 0;
   zindex_contour = 0;
   xindex_color = 0;
   yindex_color = 0;
   zindex_color = 0;
   ipoints = 0;
   imesh = 0;
   icontour = 0;
   icolor = 0;
   ichar = 0;
   for ( iplot=1; iplot<=nplots1; iplot++ )
      {
      /* Open data file */
      fptr = fopen( &filenames[index_filenames], "r");

      /* Read data file */
      if ( strcmp(&plot_types[index_plot_types], "points") == 0 )
         {
         ipoints++;
         npts = ndata[ipoints-1];
         nchar = 0;
         for ( i=1; i<=npts; i++ )
            if ( (ndouble = fscanf(fptr, &formats[index_formats], 
             &xdata[index0+i-1], &ydata[index0+i-1], &zdata[index0+i-1])) == 3 ||
                 (nchar = fscanf(fptr, &formats_mod[index_formats], &ast1, &ast2, &ast3)) == 3 )
               if ( ndouble != 3 && nchar == 3 )
                  {
                  ichar++;
                  nlinebreak[ichar-1] = index0 + i - 1;
                  nchar = 0;
                  i--;
                  }
         index_formats = index_formats + nformats[ipoints-1];
         index0 = index0 + ndata[ipoints-1];
         }

      else if ( strcmp(&plot_types[index_plot_types], "mesh") == 0 )
         {
         imesh++;
         if ( fscanf(fptr, "%d %d", &nxmesh[imesh-1], &nymesh[imesh-1]) != 2 )
            {
            JLP_ErrorDialog(error_str[0]);
            exit(1);
            }
         nx = nxmesh[imesh-1];
         ny = nymesh[imesh-1];

         for ( i=1; i<=nx; i++ )
            if ( fscanf(fptr, "%lf", &xmesh[xindex_mesh+i-1]) != 1 )
               {
               JLP_ErrorDialog(error_str[0]);
               exit(1);
               }

         for ( j=1; j<=ny; j++ )
            if ( fscanf(fptr, "%lf", &ymesh[yindex_mesh+j-1]) != 1 )
               {
               JLP_ErrorDialog(error_str[0]);
               exit(1);
               }

         for ( i=1; i<=nx; i++ )
            for ( j=1; j<=ny; j++ )
               if ( fscanf(fptr, "%lf", &zmesh[zindex_mesh+ny*(i-1)+j-1]) != 1 )
                  {
                  JLP_ErrorDialog(error_str[0]);
                  exit(1);
                  }

         xindex_mesh = xindex_mesh + nx;
         yindex_mesh = yindex_mesh + ny;
         zindex_mesh = zindex_mesh + nx*ny;
         }

      else if ( strcmp(&plot_types[index_plot_types], "contour") == 0 )
         {
         icontour++;
         if ( fscanf(fptr, "%d %d", &nxcontour[icontour-1], &nycontour[icontour-1]) != 2 )
            {
            JLP_ErrorDialog(error_str[1]);
            exit(1);
            }
         nx = nxcontour[icontour-1];
         ny = nycontour[icontour-1];

         for ( i=1; i<=nx; i++ )
            if ( fscanf(fptr, "%lf", &xcontour[xindex_contour+i-1]) != 1 )
               {
               JLP_ErrorDialog(error_str[1]);
               exit(1);
               }

         for ( j=1; j<=ny; j++ )
            if ( fscanf(fptr, "%lf", &ycontour[yindex_contour+j-1]) != 1 )
               {
               JLP_ErrorDialog(error_str[1]);
               exit(1);
               }

         for ( i=1; i<=nx; i++ )
            for ( j=1; j<=ny; j++ )
               if ( fscanf(fptr, "%lf", &zcontour[zindex_contour+ny*(i-1)+j-1]) != 1 )
                  {
                  JLP_ErrorDialog(error_str[1]);
                  exit(1);
                  }

         xindex_contour = xindex_contour + nx;
         yindex_contour = yindex_contour + ny;
         zindex_contour = zindex_contour + nx*ny;
         }


      else if ( strcmp(&plot_types[index_plot_types], "color") == 0 )
         {
         icolor++;
         if ( fscanf(fptr, "%d %d", &nxcolor[icolor-1], &nycolor[icolor-1]) != 2 )
            {
            JLP_ErrorDialog(error_str[1]);
            exit(1);
            }
         nx = nxcolor[icolor-1];
         ny = nycolor[icolor-1];

         for ( i=1; i<=nx; i++ )
            if ( fscanf(fptr, "%lf", &xcolor[xindex_color+i-1]) != 1 )
               {
               JLP_ErrorDialog(error_str[1]);
               exit(1);
               }

         for ( j=1; j<=ny; j++ )
            if ( fscanf(fptr, "%lf", &ycolor[yindex_color+j-1]) != 1 )
               {
               JLP_ErrorDialog(error_str[1]);
               exit(1);
               }

         for ( i=1; i<=nx; i++ )
            for ( j=1; j<=ny; j++ )
               if ( fscanf(fptr, "%lf", &zcolor[zindex_color+ny*(i-1)+j-1]) != 1 )
                  {
                  JLP_ErrorDialog(error_str[1]);
                  exit(1);
                  }

         xindex_color = xindex_color + nx;
         yindex_color = yindex_color + ny;
         zindex_color = zindex_color + nx*ny;
         }


      /* Increment indices */
      fclose(fptr);
      index_filenames = index_filenames + nfilenames[iplot-1];
      index_plot_types = index_plot_types + 10;
      }

   return;
   }
