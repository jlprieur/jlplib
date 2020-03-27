/*******************************************************************************
*
* jlp_GsegAxes_AutoScale.cpp
*
* Calculates "nice" tick-mark labels for linear plot axes.
*
* Labels are constrained to increment as:
*
*       0.1 0.2 0.3 ...   1  2  3 ...   10   20   30 ... etc.
*    or 0.2 0.4 0.6 ...   2  4  6 ...   20   40   60 ... etc.
*    or 0.5 1.0 1.5 ...   5 10 15 ...   50  100  150 ... etc.
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
* Version 27/04/2017
*******************************************************************************/
#include <math.h>
#include "jlp_gseg_axes.h"

/****************************************************************************
*
****************************************************************************/
void JLP_GsegAxes::AutoScale ( int naxes, int nincmax )
{
/* Declare variables */
int iaxis, i, n, nf, n1, n2, f[] = { 1, 2, 5, 10, 20 }, nticks;
double datamin, datamax, incmin, inc, axismin, axismax;
double data_xmin0, data_xmax0, data_ymin0, data_ymax0, data_zmin0, data_zmax0;

// Get data min/max from JLP_Gsegraf object:
   jlp_gsegraf1->GSEG_GetDataMinMax(&data_xmin0, &data_xmax0, &data_ymin0, 
                                    &data_ymax0, &data_zmin0, &data_zmax0);

/* Calculate tick-mark labels for each axis */
   for ( iaxis = 1; iaxis <= naxes; iaxis++ )
      {
      /* Get minimum and maximum values of plot data */
      if ( iaxis == 1 )
         {
         datamin = data_xmin0;
         datamax = data_xmax0;
         }
      else if ( iaxis == 2 )
         {
         datamin = data_ymin0;
         datamax = data_ymax0;
         }
      else if ( iaxis == 3 )
         {
         datamin = data_zmin0;
         datamax = data_zmax0;
         }

/* Calculate minimum tick-mark increment */
      incmin = (datamax - datamin)/nincmax;
// Exit from loop here if range is null:
      if ( incmin <= 0.0 )
         continue;

/* Calculate "nice" tick-mark increment */
      n = roundint(floor(log10(fabs(incmin))));
      if(( 0.5*pow(10.0, (double) n) < incmin) && 
         (incmin <= 1.0*pow(10.0, (double) n) ))
         nf = 0;
      else if (( 1.0*pow(10.0, (double) n) < incmin) && 
               (incmin <= 2.0*pow(10.0, (double) n) ))
         nf = 1;
      else if(( 2.0*pow(10.0, (double) n) < incmin) && 
              (incmin <= 5.0*pow(10.0, (double) n) ))
         nf = 2;
      else
         nf = 3;
      inc = f[nf]*pow(10.0, (double) n);
      n1 = roundint(floor(datamin/inc));
      n2 = roundint(ceil(datamax/inc));
      axismin = n1*inc;
      axismax = n2*inc;

      /* Increase tick-mark increment if too many increments */
      if ( n2 - n1 > nincmax)
         {
         nf = nf + 1;
         inc = f[nf]*pow(10.0, (double) n);
         n1 = roundint(floor(datamin/inc));
         n2 = roundint(ceil(datamax/inc));
         axismin = n1*inc;
         axismax = n2*inc;
         }


      /* Save tick-mark label data */
      nticks = n2 - n1 + 1;
      if ( iaxis == 1 )
         {
         p_ticklabels->nxvalues = nticks;
         for ( i=1; i<=nticks; i++ )
            p_ticklabels->xvalues[i-1] = axismin 
                                  + (i - 1)*((axismax - axismin)/(nticks - 1));
         }
      else if ( iaxis == 2 )
         {
         p_ticklabels->nyvalues = nticks;
         for ( i=1; i<=nticks; i++ )
            p_ticklabels->yvalues[i-1] = axismin 
                                 + (i - 1)*((axismax - axismin)/(nticks - 1));
         }
      else if ( iaxis == 3 )
         {
         p_ticklabels->nzvalues = nticks;
         for ( i=1; i<=nticks; i++ )
            p_ticklabels->zvalues[i-1] = axismin 
                                 + (i - 1)*((axismax - axismin)/(nticks - 1));
         }
      }

#ifdef DEBUG
printf("AutoScale: p_ticklabels->nyvalues = %d (naxes=%d iaxis=%d nticks=%d)\n",
        p_ticklabels->nyvalues, naxes, iaxis, nticks);
#endif
   return;
   }
