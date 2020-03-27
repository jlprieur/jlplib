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
* JLP
* Version 16/08/2017
*******************************************************************************/
#include "jlp_gsegraf.h"
#include "jlp_gseg_data.h"
#include "jlp_gseg_plot_data1.h"  // auto_scale_for_image()

/*****************************************************************************
* Find minimum and maximum values of data files and parameter file
*****************************************************************************/
void JLP_GsegData::DataMinMax3d (const int high_contrast)
{
/* Declare variables */
int i, j, iplot, npts, gseg_plot_type0, nx, ny;
double data_xmin, data_xmax, data_ymin, data_ymax, data_zmin, data_zmax, ww;
double lower_itt, upper_itt;

/* Initialize min-max data */
 data_xmin =  DBLE_MAX_VALUE;
 data_xmax = -DBLE_MAX_VALUE;
 data_ymin =  DBLE_MAX_VALUE;
 data_ymax = -DBLE_MAX_VALUE;
 data_zmin =  DBLE_MAX_VALUE;
 data_zmax = -DBLE_MAX_VALUE;
 lower_itt =  data_zmin;
 upper_itt =  data_zmax;

/* Find minimum and maximum values of data files */
 for ( iplot = 1; iplot <= nplots1; iplot++ )
    {
// Get gseg plot type:
    GetGsegPlotType(iplot, &gseg_plot_type0);
/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
// 1="points"
   if (gseg_plot_type0 == 1)
      {
       npts = gseg_plotdata1[iplot].npts;
       for ( i=1; i<=npts; i++ )
          {
          ww = gseg_plotdata1[iplot].xdata[i-1];
          if ( ww < data_xmin ) data_xmin = ww;
          if ( ww > data_xmax ) data_xmax = ww;

          ww = gseg_plotdata1[iplot].ydata[i-1];
          if ( ww < data_ymin ) data_ymin = ww;
          if ( ww > data_ymax ) data_ymax = ww;

          ww = gseg_plotdata1[iplot].zdata[i-1];
          if ( ww < data_zmin ) data_zmin = ww;
          if ( ww > data_zmax ) data_zmax = ww;

          }

       }

/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
// 3="contour"
   else if (gseg_plot_type0 == 3)
      {
      nx = gseg_plotdata1[iplot].nxcontour;
      ny = gseg_plotdata1[iplot].nycontour;

// Assume here that the contours are sorted from smallest to largest values:
      ww = gseg_plotdata1[iplot].xcontour[0];
      if ( ww < data_xmin ) data_xmin = ww;

      ww = gseg_plotdata1[iplot].xcontour[nx - 1];
      if ( ww > data_xmax ) data_xmax = ww;

      ww = gseg_plotdata1[iplot].ycontour[0];
      if ( ww < data_ymin ) data_ymin = ww;

      ww = gseg_plotdata1[iplot].ycontour[ny - 1];
      if ( ww > data_ymax ) data_ymax = ww;

/***
      for ( i=1; i<=nx; i++ )
         for ( j=1; j<=ny; j++ )
            {
            ww = gseg_plotdata1[iplot].zcontour[ny*(i-1)+j-1];
            if ( ww < data_zmin ) data_zmin = ww;
            if ( ww > data_zmax ) data_zmax = ww;
            }
***/
// When images, take other boundaries:
       auto_scale_for_image(&(gseg_plotdata1[iplot].zcontour[0]), nx, ny, ny,
                            high_contrast, &lower_itt, &upper_itt);
       if(lower_itt < data_zmin) data_zmin = lower_itt;
       if(upper_itt > data_zmax) data_zmax = upper_itt;

      }

/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
// 4="color"
   else if (gseg_plot_type0 == 4)
      {
      nx = gseg_plotdata1[iplot].nxcolor;
      ny = gseg_plotdata1[iplot].nycolor;

// Assume here that the color arrays are sorted from smallest to largest values:
      ww = gseg_plotdata1[iplot].xcolor[0];
      if ( ww < data_xmin ) data_xmin = ww;

      ww = gseg_plotdata1[iplot].xcolor[nx - 1];
      if ( ww > data_xmax ) data_xmax = ww;

      ww = gseg_plotdata1[iplot].ycolor[0];
      if ( ww < data_ymin ) data_ymin = ww;

      ww = gseg_plotdata1[iplot].ycolor[ny - 1];
      if ( ww > data_ymax ) data_ymax = ww;

/***
      for ( i=1; i<=nx; i++ )
         for ( j=1; j<=ny; j++ )
            {
            ww = gseg_plotdata1[iplot].zcolor[ny*(i-1)+j-1];
            if ( ww < data_zmin ) data_zmin = ww;
            if ( ww > data_zmax ) data_zmax = ww;
            }
***/
// When images, take other boundaries:
       auto_scale_for_image(&(gseg_plotdata1[iplot].zcolor[0]), nx, ny, ny,
                            high_contrast, &lower_itt, &upper_itt);
       if(lower_itt < data_zmin) data_zmin = lower_itt;
       if(upper_itt > data_zmax) data_zmax = upper_itt;

      }

/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
// 5="mesh"
   else if (gseg_plot_type0 == 5)
      {
      nx = gseg_plotdata1[iplot].nxmesh;
      ny = gseg_plotdata1[iplot].nymesh;

// Assume here that the mesh arrays are sorted from smallest to largest values:
      ww = gseg_plotdata1[iplot].xmesh[0];
      if ( ww < data_xmin ) data_xmin = ww;

      ww = gseg_plotdata1[iplot].xmesh[nx - 1];
      if ( ww > data_xmax ) data_xmax = ww;

      ww = gseg_plotdata1[iplot].ymesh[0];
      if ( ww < data_ymin ) data_ymin = ww;

      ww = gseg_plotdata1[iplot].ymesh[ny - 1];
      if ( ww > data_ymax ) data_ymax = ww;

/***
      for ( i=1; i<=nx; i++ )
         for ( j=1; j<=ny; j++ )
            {
            ww = gseg_plotdata1[iplot].zmesh[ny*(i-1)+j-1];
            if ( ww < data_zmin ) data_zmin = ww;
            if ( ww > data_zmax ) data_zmax = ww;
            }
***/
// When images, take other boundaries:
       auto_scale_for_image(&(gseg_plotdata1[iplot].zmesh[0]), nx, ny, ny,
                            high_contrast, &lower_itt, &upper_itt);
       if(lower_itt < data_zmin) data_zmin = lower_itt;
       if(upper_itt > data_zmax) data_zmax = upper_itt;

      }

  } // EOF loop on iplot

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
double line_coords[6];
int nlines;
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
int nsymbols;
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
