/*******************************************************************************
*
* jlp_GsegData_DataMinMax3d.c
*
* Finds minimum and maximum values of 3d-plot data.
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
#include "jlp_gsegraf.h"
#include "jlp_gseg_data.h"

/*****************************************************************************
* Find minimum and maximum values of data files and parameter file
*****************************************************************************/
void JLP_GsegData::DataMinMax3d (void)
{
/* Declare variables */
int i, j, iplot, index_plot_types,
    npts, nx, ny, index, ipoints,
    imesh, xindex_mesh, yindex_mesh, zindex_mesh,
    icontour, xindex_contour, yindex_contour, zindex_contour,
    icolor, xindex_color, yindex_color, zindex_color;
double data_xmin, data_xmax, data_ymin, data_ymax, data_zmin, data_zmax;
char param_filename0[256];

   /* Initialize min-max data */
   data_xmin =  DBLE_MAX_VALUE;
   data_xmax = -DBLE_MAX_VALUE;
   data_ymin =  DBLE_MAX_VALUE;
   data_ymax = -DBLE_MAX_VALUE;
   data_zmin =  DBLE_MAX_VALUE;
   data_zmax = -DBLE_MAX_VALUE;


   /* Find minimum and maximum values of data files */
   index_plot_types = 0;
   index = 0;
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
   for ( iplot=1; iplot<=nplots1; iplot++ )
      {
      if ( strcmp(&plot_types[index_plot_types], "points") == 0 )
         {
         ipoints++;

         npts = ndata[ipoints-1];
         for ( i=1; i<=npts; i++ )
            {
            if ( xdata[index+i-1] < data_xmin )
               data_xmin = xdata[index+i-1];

            if ( xdata[index+i-1] > data_xmax )
               data_xmax = xdata[index+i-1];

            if ( ydata[index+i-1] < data_ymin )
               data_ymin = ydata[index+i-1];

            if ( ydata[index+i-1] > data_ymax )
               data_ymax = ydata[index+i-1];

            if ( zdata[index+i-1] < data_zmin )
               data_zmin = zdata[index+i-1];

            if ( zdata[index+i-1] > data_zmax )
               data_zmax = zdata[index+i-1];
            }

         index = index + ndata[ipoints-1];
         }

      else if ( strcmp(&plot_types[index_plot_types], "mesh") == 0 )
         {
         imesh++;

         nx = nxmesh[imesh-1];
         if ( xmesh[xindex_mesh] < data_xmin )
            data_xmin = xmesh[xindex_mesh];
         if ( xmesh[xindex_mesh+nx-1] > data_xmax )
            data_xmax = xmesh[xindex_mesh+nx-1];

         ny = nymesh[imesh-1];
         if ( ymesh[yindex_mesh] < data_ymin )
            data_ymin = ymesh[yindex_mesh];
         if ( ymesh[yindex_mesh+ny-1] > data_ymax )
            data_ymax = ymesh[yindex_mesh+ny-1];

         for ( i=1; i<=nx; i++ )
            for ( j=1; j<=ny; j++ )
               {
               if ( zmesh[zindex_mesh+ny*(i-1)+j-1] < data_zmin )
                  data_zmin = zmesh[zindex_mesh+ny*(i-1)+j-1];
               if ( zmesh[zindex_mesh+ny*(i-1)+j-1] > data_zmax )
                  data_zmax = zmesh[zindex_mesh+ny*(i-1)+j-1];
               }

         xindex_mesh = xindex_mesh + nx;
         yindex_mesh = yindex_mesh + ny;
         zindex_mesh = zindex_mesh + nx*ny;
         }

      else if ( strcmp(&plot_types[index_plot_types], "contour") == 0 )
         {
         icontour++;

         nx = nxcontour[icontour-1];
         if ( xcontour[xindex_contour] < data_xmin )
            data_xmin = xcontour[xindex_contour];
         if ( xcontour[xindex_contour+nx-1] > data_xmax )
            data_xmax = xcontour[xindex_contour+nx-1];

         ny = nycontour[icontour-1];
         if ( ycontour[yindex_contour] < data_ymin )
            data_ymin = ycontour[yindex_contour];
         if ( ycontour[yindex_contour+ny-1] > data_ymax )
            data_ymax = ycontour[yindex_contour+ny-1];

         for ( i=1; i<=nx; i++ )
            for ( j=1; j<=ny; j++ )
               {
               if ( zcontour[zindex_contour+ny*(i-1)+j-1] < data_zmin )
                  data_zmin = zcontour[zindex_contour+ny*(i-1)+j-1];
               if ( zcontour[zindex_contour+ny*(i-1)+j-1] > data_zmax )
                  data_zmax = zcontour[zindex_contour+ny*(i-1)+j-1];
               }

         xindex_contour = xindex_contour + nx;
         yindex_contour = yindex_contour + ny;
         zindex_contour = zindex_contour + nx*ny;
         }

      else if ( strcmp(&plot_types[index_plot_types], "color") == 0 )
         {
         icolor++;

         nx = nxcolor[icolor-1];
         if ( xcolor[xindex_color] < data_xmin )
            data_xmin = xcolor[xindex_color];
         if ( xcolor[xindex_color+nx-1] > data_xmax )
            data_xmax = xcolor[xindex_color+nx-1];

         ny = nycolor[icolor-1];
         if ( ycolor[yindex_color] < data_ymin )
            data_ymin = ycolor[yindex_color];
         if ( ycolor[yindex_color+ny-1] > data_ymax )
            data_ymax = ycolor[yindex_color+ny-1];

         for ( i=1; i<=nx; i++ )
            for ( j=1; j<=ny; j++ )
               {
               if ( zcolor[zindex_color+ny*(i-1)+j-1] < data_zmin )
                  data_zmin = zcolor[zindex_color+ny*(i-1)+j-1];
               if ( zcolor[zindex_color+ny*(i-1)+j-1] > data_zmax )
                  data_zmax = zcolor[zindex_color+ny*(i-1)+j-1];
               }

         xindex_color = xindex_color + nx;
         yindex_color = yindex_color + ny;
         zindex_color = zindex_color + nx*ny;
         }


      /* Increment indices */
      index_plot_types = index_plot_types + 10;
      }

/* Save data */
  if(nplots1 > 0) {
    p_data_min_max->xmin = data_xmin;
    p_data_min_max->xmax = data_xmax;
    p_data_min_max->ymin = data_ymin;
    p_data_min_max->ymax = data_ymax;
    p_data_min_max->zmin = data_zmin;
    p_data_min_max->zmax = data_zmax;
    }

/* Take other information available in the parameter file into account
* to extend the range of x/y min/max
* stored in p_data_min_max structure
*/
 DataMinMax_ApplyFileSettings();

return;
}
/************************************************************************
* Determine min/max values of x,y,z data from the parameter file
* and use those values
* to extend the previous known values of the range of x/y/z min/max
* stored in fromfile_data_min_max structure
*************************************************************************/
int JLP_GsegData::ReadDataMinMax3dFromFile(char *param_filename0)
{
double data_xmin, data_xmax, data_ymin, data_ymax, data_zmin, data_zmax;
double line_coords[6], symbol_coords[2];
int i, nlines;
char line1[256];
FILE *fptr;
const char *error_str[] = { "Invalid line coordinates." };

// Load previous values:
    data_xmin = fromfile_data_min_max->xmin;
    data_xmax = fromfile_data_min_max->xmax;
    data_ymin = fromfile_data_min_max->ymin;
    data_ymax = fromfile_data_min_max->ymax;
    data_zmin = fromfile_data_min_max->zmin;
    data_zmax = fromfile_data_min_max->zmax;

/* Find minimum and maximum values of line data */
   fptr = fopen(param_filename0, "r");
   nlines = 0;
   while ( fgets(line1, 256, fptr) != NULL )
      {
      /* Get line coordinates */
      if ( strncmp(line1, "line_coords", 11) == 0 )
         {
         nlines++;

         if ( sscanf(line1, "%*s %lf %lf %lf %lf %lf %lf",
                     &line_coords[0], &line_coords[3],
                     &line_coords[1], &line_coords[4],
                     &line_coords[2], &line_coords[5]) != 6 )
            {
            JLP_ErrorDialog(error_str[0]);
            exit(1);
            }

         /* Modify axis minimum and maximum values */
         if ( line_coords[0] < data_xmin )
            data_xmin = line_coords[0];

         if ( line_coords[0] > data_xmax )
            data_xmax = line_coords[0];

         if ( line_coords[3] < data_xmin )
            data_xmin = line_coords[3];

         if ( line_coords[3] > data_xmax )
            data_xmax = line_coords[3];

         if ( line_coords[1] < data_ymin )
            data_ymin = line_coords[1];

         if ( line_coords[1] > data_ymax )
            data_ymax = line_coords[1];

         if ( line_coords[4] < data_ymin )
            data_ymin = line_coords[4];

         if ( line_coords[4] > data_ymax )
            data_ymax = line_coords[4];

         if ( line_coords[2] < data_zmin )
            data_zmin = line_coords[2];

         if ( line_coords[2] > data_zmax )
            data_zmax = line_coords[2];

         if ( line_coords[5] < data_zmin )
            data_zmin = line_coords[5];

         if ( line_coords[5] > data_zmax )
            data_zmax = line_coords[5];
         }

      else if ( strncmp(line1, "#####", 5) == 0 )
         break;
      }
   fclose(fptr);

/* Save data */
   fromfile_data_min_max->xmin = data_xmin;
   fromfile_data_min_max->xmax = data_xmax;
   fromfile_data_min_max->ymin = data_ymin;
   fromfile_data_min_max->ymax = data_ymax;
   fromfile_data_min_max->zmin = data_zmin;
   fromfile_data_min_max->zmax = data_zmax;

return(0);
}
/************************************************************************
* Determine min/max values of x,y symbol data from the parameter file
* and use those values
* to extend the previous known values of the range of x/y min/max
* stored in p_data_min_max structure
*************************************************************************/
int JLP_GsegData::ReadSymbolMinMax3dFromFile(char *param_filename0)
{
double data_xmin, data_xmax, data_ymin, data_ymax, data_zmin, data_zmax;
double symbol_coords[3];
int i, nsymbols;
char line1[256];
FILE *fptr;
const char *error_str[] = { "Invalid symbol coordinates." };

// Load previous values:
    data_xmin = fromfile_data_min_max->xmin;
    data_xmax = fromfile_data_min_max->xmax;
    data_ymin = fromfile_data_min_max->ymin;
    data_ymax = fromfile_data_min_max->ymax;
    data_zmin = fromfile_data_min_max->zmin;
    data_zmax = fromfile_data_min_max->zmax;

/* Find minimum and maximum values of symbol data */
   fptr = fopen(param_filename0, "r");
   nsymbols = 0;
   while ( fgets(line1, 256, fptr) != NULL )
      {
      /* Get symbol coordinates */
      if ( strncmp(line1, "symbol_coords", 13) == 0 )
         {
         nsymbols++;

         if ( sscanf(line1, "%*s %lf %lf %lf", &symbol_coords[0], 
                      &symbol_coords[1], &symbol_coords[2]) != 3 )
            {
            JLP_ErrorDialog(error_str[1]);
            exit(1);
            }

         /* Modify axis minimum and maximum values */
         if ( symbol_coords[0] < data_xmin )
            data_xmin = symbol_coords[0];

         if ( symbol_coords[0] > data_xmax )
            data_xmax = symbol_coords[0];

         if ( symbol_coords[1] < data_ymin )
            data_ymin = symbol_coords[1];

         if ( symbol_coords[1] > data_ymax )
            data_ymax = symbol_coords[1];

         if ( symbol_coords[2] < data_zmin )
            data_zmin = symbol_coords[2];

         if ( symbol_coords[2] > data_zmax )
            data_zmax = symbol_coords[2];
         }

      else if ( strncmp(line1, "#####", 5) == 0 )
         break;
      }
   fclose(fptr);


/* Save data */
   fromfile_data_min_max->xmin = data_xmin;
   fromfile_data_min_max->xmax = data_xmax;
   fromfile_data_min_max->ymin = data_ymin;
   fromfile_data_min_max->ymax = data_ymax;
   fromfile_data_min_max->zmin = data_zmin;
   fromfile_data_min_max->zmax = data_zmax;

return(0);
}
