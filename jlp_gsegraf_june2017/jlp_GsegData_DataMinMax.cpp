/*******************************************************************************
*
* jlp_GsegData_DataMinMax.c
*
* Finds minimum and maximum values of 2d-plot data.
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
* Version 20/04/2017
*******************************************************************************/
#include <float.h>
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_data.h"        // JLP_GsegData class

/***************************************************************************
* Find minimum and maximum values of plot data and other useful parameters
*
***************************************************************************/
void JLP_GsegData::DataMinMax (void)
{
/* Declare variables */
int i, j, iplot, npts, index, index_plot_types,
    ipoints, ihist, icontour, icolor,
    n1, n2, nbins, count, index_bin_values,
    nx, ny, xindex_contour, yindex_contour, zindex_contour,
    xindex_color, yindex_color, zindex_color;
double data_xmin, data_xmax, data_ymin, data_ymax, data_zmin, data_zmax; 
double sum, mean, data_min, data_max, q[5]; 
double binwidth, binmin, binmax, bin1, bin2,
       histmin, histmax;
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
 ipoints = 0;
 ihist = 0;
 icontour = 0;
 icolor = 0;
 index_bin_values = 0;
 xindex_contour = 0;
 yindex_contour = 0;
 zindex_contour = 0;
 xindex_color = 0;
 yindex_color = 0;
 zindex_color = 0;
 for ( iplot = 1; iplot <= nplots1; iplot++ )
    {
    if ( strcmp(&plot_types[index_plot_types], "points") == 0 )
       {
       ipoints++;
       npts = ndata[ipoints + ihist -1];
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

          }

       index = index + ndata[ipoints+ihist-1];
       }
    else if ( strcmp(&plot_types[index_plot_types], "histogram") == 0 )
       {
       ihist++;

/* Calculate data mean */
       npts = ndata[ipoints+ihist-1];
       sum = 0.0;
       for ( i=1; i<=npts; i++ )
         sum = sum + xdata[index+i-1];
         mean = sum/npts;

/* Calculate data quartile values */
         data_min = xdata[index];
         data_max = xdata[index+npts-1];
         q[0] = data_min;
         q[4] = data_max;
         for ( i=1; i<=3; i++ )
            {
            j = roundint(i*0.25*npts);
            q[i] = xdata[index+j-1];
            }

/* Estimate optimal histogram bin width */
         if ( bin_widths[ihist-1] <= 0.0 )
            {
/* binwidth = 3.49*std/pow((double) npts, 1.0/3.0); */   /* Scott */
/* Freedman, Diaconis */
            binwidth = 2.0*(q[3] - q[1])/pow((double) npts, 1.0/3.0);   
            bin_widths[ihist-1] = binwidth;
            }
         else
            binwidth = bin_widths[ihist-1];

/* Calculate bin minimum and maximum */
         n1 = ceil(((mean - binwidth/2.0) - data_min)/binwidth);
         n2 = ceil((data_max - (mean + binwidth/2.0))/binwidth);
         nbins = n1 + n2 + 1;
         binmin = (mean - binwidth/2.0) - n1*binwidth;
         binmax = (mean + binwidth/2.0) + n2*binwidth;

/* Find maximum histogram value */
         histmin = 0.0;
         histmax = 0.0;
         for ( i=1; i<=nbins; i++ )
            {
            bin1 = binmin + (i - 1.0)/(nbins - 1.0)*(binmax - binmin - binwidth);
            bin2 = bin1 + binwidth;
            count = 0.0;
            for ( j=1; j<=npts; j++ )
               if ( (xdata[index+j-1] >= bin1)  && (xdata[index+j-1] < bin2) )
                  count = count + 1;

            if ( count > histmax )
               histmax = count;
            }

         if ( strcmp(&bin_values[index_bin_values], "fraction") == 0 )
            histmax = histmax/npts;
         else if ( strcmp(&bin_values[index_bin_values], "percent") == 0 )
            histmax = 100.0*histmax/npts;
         index_bin_values = index_bin_values + 9;

         if ( binmin < data_xmin )
            data_xmin = binmin;

         if ( binmax > data_xmax )
            data_xmax = binmax;

         if ( histmin < data_ymin )
            data_ymin = histmin;

         if ( histmax > data_ymax )
            data_ymax = histmax;

         index = index + ndata[ipoints + ihist -1];
         }

      else if ( strcmp(&plot_types[index_plot_types], "contour") == 0 )
         {
         icontour++;

         nx = nxcontour[icontour-1];
         ny = nycontour[icontour-1];

         if ( xcontour[xindex_contour] < data_xmin )
            data_xmin = xcontour[xindex_contour];

         if ( xcontour[xindex_contour+nx-1] > data_xmax )
            data_xmax = xcontour[xindex_contour+nx-1];

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

      else if ( strcmp(&plot_types[index_plot_types], "color")  == 0 )
         {
         icolor++;

         nx = nxcolor[icolor-1];
         ny = nycolor[icolor-1];

         if ( xcolor[xindex_color] < data_xmin )
            data_xmin = xcolor[xindex_color];

         if ( xcolor[xindex_color+nx-1] > data_xmax )
            data_xmax = xcolor[xindex_color+nx-1];

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
    }

 if(icolor > 0 || icontour > 0)
    {
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
* Determine min/max values of x,y data from the parameter file
* and use those values to extend the range of fromfile_data_min_max
* For 2D plots
*
* INPUT/OUTPUT (in private variables):
*  fromfile_data_min_max structure
*************************************************************************/
int JLP_GsegData::ReadDataMinMaxFromFile(char *param_filename0, 
                                         char *axis_type0)
{
double data_xmin, data_xmax, data_ymin, data_ymax;
double line_coords[4];
int i, nlines;
char line1[256];
FILE *fptr;
const char *error_str[] = { "Invalid line coordinates." };

// Check if 2D/3D:
   if ( strcmp(axis_type0, "3d") == 0 )
    { fprintf(stderr, "ReadDataMinMaxFromFile/Error : 3d setting not allowed here\n");
     exit(-1);
    }

// Load previous min/max values:
    data_xmin = fromfile_data_min_max->xmin; 
    data_xmax = fromfile_data_min_max->xmax; 
    data_ymin = fromfile_data_min_max->ymin; 
    data_ymax = fromfile_data_min_max->ymax; 

/* Find minimum and maximum values of line data */
   fptr = fopen(param_filename0, "r");
   nlines = 0;
   while ( fgets(line1, 256, fptr) != NULL )
      {
      /* Get line coordinates */
      if ( strncmp(line1, "line_coords", 11) == 0 )
         {
         nlines++;

         if ( sscanf(line1, "%*s %lf %lf %lf %lf",
                     &line_coords[0], &line_coords[2],
                     &line_coords[1], &line_coords[3]) != 4 )
            {
            JLP_ErrorDialog(error_str[0]);
            exit(1);
            }

/* Modify line coordinates for logarithmic and polar axes */
         if ( strcmp(axis_type0, "semilogx") == 0 )
            for ( i=1; i<=3; i=i+2 )
               line_coords[i-1] = log10(fabs(line_coords[i-1]));

         else if ( strcmp(axis_type0, "semilogy") == 0 )
            for ( i=2; i<=4; i=i+2 )
               line_coords[i-1] = log10(fabs(line_coords[i-1]));

         else if ( strcmp(axis_type0, "loglog") == 0 )
            for ( i=1; i<=4; i++ )
               line_coords[i-1] = log10(fabs(line_coords[i-1]));

         else if ( strcmp(axis_type0, "polar") == 0 )
            for ( i=1; i<=3; i=i+2 )
               line_coords[i-1] = line_coords[i-1] * DEGTORAD;

/* Modify data minimum and maximum values */
         if ( line_coords[0] < data_xmin )
            data_xmin = line_coords[0];

         if ( line_coords[0] > data_xmax )
            data_xmax = line_coords[0];

         if ( line_coords[2] < data_xmin )
            data_xmin = line_coords[2];

         if ( line_coords[2] > data_xmax )
            data_xmax = line_coords[2];

         if ( line_coords[1] < data_ymin )
            data_ymin = line_coords[1];

         if ( line_coords[1] > data_ymax )
            data_ymax = line_coords[1];

         if ( line_coords[3] < data_ymin )
            data_ymin = line_coords[3];

         if ( line_coords[3] > data_ymax )
            data_ymax = line_coords[3];
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

return(0);
}
/************************************************************************
* Extend the previous known values of the range of x/y min/max
* stored in p_data_min_max structure using the private variables
* 
* INPUT (in private variables):
*  fromfile_data_min_max structure
*************************************************************************/
int JLP_GsegData::DataMinMax_ApplyFileSettings()
{

  if(fromfile_data_min_max->xmin < p_data_min_max->xmin) 
      p_data_min_max->xmin = fromfile_data_min_max->xmin; 
  if(fromfile_data_min_max->xmax > p_data_min_max->xmax) 
      p_data_min_max->xmax = fromfile_data_min_max->xmax; 
  if(fromfile_data_min_max->ymin < p_data_min_max->ymin) 
      p_data_min_max->ymin = fromfile_data_min_max->ymin; 
  if(fromfile_data_min_max->ymax > p_data_min_max->ymax) 
      p_data_min_max->ymax = fromfile_data_min_max->ymax; 
  if(fromfile_data_min_max->zmin < p_data_min_max->zmin) 
      p_data_min_max->zmin = fromfile_data_min_max->zmin; 
  if(fromfile_data_min_max->zmax > p_data_min_max->zmax) 
      p_data_min_max->zmax = fromfile_data_min_max->zmax; 

return(0);
}
/************************************************************************
* Determine min/max values of x,y symbol data from the parameter file
* and use those values to extend the range of fromfile_data_min_max
* For 2D plots
*
* INPUT/OUTPUT (in private variables):
*  fromfile_data_min_max structure
*************************************************************************/
int JLP_GsegData::ReadSymbolMinMaxFromFile(char *param_filename0, 
                                           char *axis_type0)
{
double data_xmin, data_xmax, data_ymin, data_ymax;
double symbol_coords[2];
int i, nsymbols;
char line1[256];
FILE *fptr;
const char *error_str[] = { "Invalid symbol coordinates." };

// Check if 2D/3D:
   if ( strcmp(axis_type0, "3d") == 0 )
    { fprintf(stderr, "ReadDataMinMaxFromFile/Error : 3d setting not allowed here\n");
     exit(-1);
    }

// Load previous values:
    data_xmin = fromfile_data_min_max->xmin;
    data_xmax = fromfile_data_min_max->xmax;
    data_ymin = fromfile_data_min_max->ymin;
    data_ymax = fromfile_data_min_max->ymax;

// Get current value of axis type:
 jlp_gsegraf1->GSEG_GetAxisType(axis_type0);

/* Find minimum and maximum values of symbol data */
   fptr = fopen(param_filename0, "r");
   nsymbols = 0;
   while ( fgets(line1, 256, fptr) != NULL )
      {
      /* Get symbol coordinates */
      if ( strncmp(line1, "symbol_coords", 13) == 0 )
         {
         nsymbols++;

         if ( sscanf(line1, "%*s %lf %lf",
                     &symbol_coords[0], &symbol_coords[1]) != 2 )
            {
            JLP_ErrorDialog(error_str[0]);
            exit(1);
            }

/* Modify symbol coordinates for logarithmic and polar axes */
         if ( strcmp(axis_type0, "semilogx") == 0 )
            symbol_coords[0] = log10(fabs(symbol_coords[0]));

         else if ( strcmp(axis_type0, "semilogy") == 0 )
            symbol_coords[1] = log10(fabs(symbol_coords[1]));

         else if ( strcmp(axis_type0, "loglog") == 0 )
            {
            symbol_coords[0] = log10(fabs(symbol_coords[0]));
            symbol_coords[1] = log10(fabs(symbol_coords[1]));
            }

         else if ( strcmp(axis_type0, "polar") == 0 )
            symbol_coords[0] = symbol_coords[0] * DEGTORAD;

/* Modify data minimum and maximum values */
         if ( symbol_coords[0] < data_xmin )
            data_xmin = symbol_coords[0];

         if ( symbol_coords[0] > data_xmax )
            data_xmax = symbol_coords[0];

         if ( symbol_coords[1] < data_ymin )
            data_ymin = symbol_coords[1];

         if ( symbol_coords[1] > data_ymax )
            data_ymax = symbol_coords[1];
         }

      else if ( strncmp(line1, "#####", 5) == 0 )
         break;
      }
   fclose(fptr);


/* Save new values */
  if(nsymbols > 0) {
    fromfile_data_min_max->xmin = data_xmin;
    fromfile_data_min_max->xmax = data_xmax;
    fromfile_data_min_max->ymin = data_ymin;
    fromfile_data_min_max->ymax = data_ymax;
    }

return(0);
}
/***************************************************************************
* Get minimum/maximum data values
* (called by AxisLimits, AutoScale, AxesEqual)
****************************************************************************/
void JLP_GsegData::GetDataMinMax(double *data_xmin0, double *data_xmax0, 
                                double *data_ymin0, double *data_ymax0, 
                                double *data_zmin0, double *data_zmax0)
{
 *data_xmin0 = p_data_min_max->xmin;
 *data_xmax0 = p_data_min_max->xmax;
 *data_ymin0 = p_data_min_max->ymin;
 *data_ymax0 = p_data_min_max->ymax;
 *data_zmin0 = p_data_min_max->zmin;
 *data_zmax0 = p_data_min_max->zmax;
}

/***************************************************************************
* Set minimum/maximum data values
* (called by JLP_GsegAxes::AxisLimits, JLP_Gsegraf::GSEG_ZoomIn)
****************************************************************************/
void JLP_GsegData::SetDataMinMax(const double data_xmin0, 
                                const double data_xmax0, 
                                const double data_ymin0, 
                                const double data_ymax0, 
                                const double data_zmin0, 
                                const double data_zmax0)
{
// data_min_max_type (may be different from axis_limits when data_xmin > data_xmax ...)
// Should always have data_xmin < data_xmax in data_min_max_type

if(data_xmin0 <= data_xmax0) {
 p_data_min_max->xmin = data_xmin0;
 p_data_min_max->xmax = data_xmax0;
 } else {
 p_data_min_max->xmin = data_xmax0;
 p_data_min_max->xmax = data_xmin0;
 }

if(data_ymin0 <= data_ymax0) {
 p_data_min_max->ymin = data_ymin0;
 p_data_min_max->ymax = data_ymax0;
 } else {
 p_data_min_max->ymin = data_ymax0;
 p_data_min_max->ymax = data_ymin0;
 }

if(data_zmin0 <= data_zmax0) {
 p_data_min_max->zmin = data_zmin0;
 p_data_min_max->zmax = data_zmax0;
 } else {
 p_data_min_max->zmin = data_zmax0;
 p_data_min_max->zmax = data_zmin0;
 }

return;
}
