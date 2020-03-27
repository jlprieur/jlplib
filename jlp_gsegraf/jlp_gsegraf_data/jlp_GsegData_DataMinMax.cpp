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
* Version 16/08/2017
*******************************************************************************/
#include <float.h>
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_data.h"        // JLP_GsegData class
#include "jlp_gseg_plot_data1.h"  // jlp_auto_scale_for_image()

// #define DEBUG

/***************************************************************************
* Find minimum and maximum values of plot data and other useful parameters
*
***************************************************************************/
void JLP_GsegData::DataMinMax(const int high_contrast)
{
/* Declare variables */
int i, j, iplot, npts, gseg_plot_type0, n1, n2, nbins, count, nx, ny;
double data_xmin, data_xmax, data_ymin, data_ymax, data_zmin, data_zmax, ww;
double sum, mean, data_min, data_max, q[5]; 
double binwidth, binmin, binmax, bin1, bin2, histmin, histmax;
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
#ifdef DEBUG
  printf("DataMinMax/nplots1=%d gseg_plot_type[%d]=%d \n", nplots1, iplot,
          gseg_plot_type0);
#endif

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
#ifdef DEBUG
  printf("DataMinMax/npts=%d \n", npts);
#endif

       for ( i=1; i<=npts; i++ )
          {
          ww = gseg_plotdata1[iplot].xdata[i-1];
          if ( ww < data_xmin ) data_xmin = ww;
          if ( ww > data_xmax ) data_xmax = ww;

          ww = gseg_plotdata1[iplot].ydata[i-1];
          if ( ww < data_ymin ) data_ymin = ww;
          if ( ww > data_ymax ) data_ymax = ww;

          }

       }
// 2="histogram"
   else if (gseg_plot_type0 == 2)
       {

/* Calculate data mean */
       npts = gseg_plotdata1[iplot].npts;
       sum = 0.0;
       for ( i=1; i<=npts; i++ )
         sum = sum + gseg_plotdata1[iplot].xdata[i-1];
         mean = sum/npts;

/* Calculate data quartile values */
         data_min = gseg_plotdata1[iplot].xdata[0];
         data_max = gseg_plotdata1[iplot].xdata[npts-1];
         q[0] = data_min;
         q[4] = data_max;
         for ( i=1; i<=3; i++ )
            {
            j = roundint(i*0.25*npts);
            q[i] = gseg_plotdata1[iplot].xdata[j-1];
            }

/* Estimate optimal histogram bin width */
// NB: bin_width initialized at -1 in "jlp_gseg_plot_data1.cpp"
         if ( gseg_plotdata1[iplot].bin_width <= 0.0 )
            {
/* binwidth = 3.49*std/pow((double) npts, 1.0/3.0); */   /* Scott */
/* Freedman, Diaconis */
            binwidth = 2.0*(q[3] - q[1])/pow((double) npts, 1.0/3.0);   
            gseg_plotdata1[iplot].bin_width = binwidth;
            }
         else
            binwidth = gseg_plotdata1[iplot].bin_width;

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
               if ( (gseg_plotdata1[iplot].xdata[j-1] >= bin1)  
                    && (gseg_plotdata1[iplot].xdata[j-1] < bin2) )
                  count = count + 1;

            if ( count > histmax )
               histmax = count;
            }

         if ( strcmp(gseg_plotdata1[iplot].bin_value, "fraction") == 0 )
            histmax = histmax/npts;
         else if ( strcmp(gseg_plotdata1[iplot].bin_value, "percent") == 0 )
            histmax = 100.0*histmax/npts;

         if ( binmin < data_xmin )
            data_xmin = binmin;

         if ( binmax > data_xmax )
            data_xmax = binmax;

         if ( histmin < data_ymin )
            data_ymin = histmin;

         if ( histmax > data_ymax )
            data_ymax = histmax;

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

// Exit from here if error:
      if(nx == 0 || ny == 0) {
       fprintf(stderr, "DataMinMax/Error: nxcontour=%d, nycontour=%d for iplot=%d\n", 
               nx, ny, iplot);
       return;
       }

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

// Exit from here if error:
      if(nx == 0 || ny == 0) {
       fprintf(stderr, "DataMinMax/Error: nxcolor=%d, nycolor=%d for iplot=%d\n", 
               nx, ny, iplot);
       return;
       }

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

#ifdef DEBUG
printf("DataMinMax/Debug1: xmin,max=%f %f  ymin,max=%f %f zmin,max=%f %f\n",
        p_data_min_max->xmin, p_data_min_max->xmax,
        p_data_min_max->ymin, p_data_min_max->ymax,
        p_data_min_max->zmin, p_data_min_max->zmax);
#endif

/* Take other information available in the parameter file into account
* to extend the range of x/y min/max
* stored in p_data_min_max structure
*/
 if(gsegdata_from_paramfile == 1) DataMinMax_ApplyFileSettings();

#ifdef DEBUG
printf("DataMinMax/Debug2: xmin,max=%f %f  ymin,max=%f %f zmin,max=%f %f\n",
        p_data_min_max->xmin, p_data_min_max->xmax,
        p_data_min_max->ymin, p_data_min_max->ymax,
        p_data_min_max->zmin, p_data_min_max->zmax);
#endif

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

// Modify line coordinates for logarithmic and polar axes 
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
int nsymbols;
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
