/*******************************************************************************
* jlp_Gsegraf_PlotExtraSymbols.cpp
*
* Plots extra symbols from the list contained in JLP_GsegData object.
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
* Version 10/05/2017
*******************************************************************************/
#include <ctype.h>
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"   // JLP_GsegAxes class 
#include "jlp_gseg_data.h"   // JLP_GsegData class 

/**************************************************************************
* Plots extra symbols from the list contained in JLP_GsegData object.
**************************************************************************/
void JLP_Gsegraf::PlotExtraSymbols()
{
/* Declare variables */
int i, ifunc, ncoords, isymb, nsymb, status;
unsigned int symbol_size;
UINT32 symbol_color;
double dev_x1_box, dev_x2_box, dev_y1_box, dev_y2_box,
       xmin, xmax, ymin, ymax, zmin, zmax, rmin, rmax,
       xscale, yscale, zscale, xscalesq, yscalesq, zscalesq,
       rscale, xorigin, yorigin, radius,
       origin[3], Ry[9], Rz[9], Ryz[9], r[3],
       symbol_coords[6] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }, x, y, *ppp;
char *pchar, symbol_char, color_char, buffer[64], axis_type0[64];

// Get plotbox axis type
 jlp_gseg_axes1->GetAxisType(axis_type0);

/* Get box settings */
   jlp_gseg_axes1->GetBoxSettings(&dev_x1_box, &dev_x2_box, &dev_y1_box,
                                  &dev_y2_box, &xmin, &xmax, &ymin, &ymax,
                                  &zmin, &zmax, &rmin, &rmax, &xscale, &yscale,
                                  &zscale, &rscale, &xorigin, &yorigin,
                                  &radius, origin, Ry, Rz, Ryz, &ncoords);

/* Draw symbols */
 nsymb = jlp_gseg_data1->NExtraSymbols();
 for(isymb = 0; isymb < nsymb; isymb++) {

// Get data for plotting line of index isymb
   status = jlp_gseg_data1->GetExtraSymbolData(symbol_coords, &symbol_color,
                                               &symbol_char, &symbol_size, 
                                               isymb);
   if(status) {
     fprintf(stderr, "PlotExtraSymbols/Error getting symbol data for isymb=%d\n",
             isymb);
     return;
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

/* Draw symbol */
         if ( strcmp(axis_type0, "linear")    == 0 ||
              strcmp(axis_type0, "semilogx")  == 0 ||
              strcmp(axis_type0, "semilogy")  == 0 ||
              strcmp(axis_type0, "loglog")    == 0 )
            {
            if ( xmin <= symbol_coords[0] && symbol_coords[0] <= xmax &&
                 ymin <= symbol_coords[1] && symbol_coords[1] <= ymax )
               {
               x = dev_x1_box + (symbol_coords[0] - xmin)*xscale;
               y = dev_y2_box - (symbol_coords[1] - ymin)*yscale;
               }
            else
               continue;
            }

         else if ( strcmp(axis_type0, "polar") == 0 )
            {
            if ( rmin <= symbol_coords[1] && symbol_coords[1] <= rmax )
               {
               x = xorigin + (symbol_coords[1] - rmin)
                              *cos(symbol_coords[0])*rscale;
               y = yorigin - (symbol_coords[1] - rmin)
                              *sin(symbol_coords[0])*rscale;
               }
            else
               continue;
            }

         else if ( strcmp(axis_type0, "3d") == 0 )
            {
            if ( xmin <= symbol_coords[0] && symbol_coords[0] <= xmax &&
                 ymin <= symbol_coords[1] && symbol_coords[1] <= ymax &&
                 zmin <= symbol_coords[2] && symbol_coords[2] <= zmax )
               {
               r[0] = (symbol_coords[0] - xmin)*xscale;
               r[1] = (symbol_coords[1] - ymin)*yscale;
               r[2] = (symbol_coords[2] - zmin)*zscale;

               ppp = multiply_mv(Ryz, r);
               for ( i=1; i<=3; i++, ppp++ )
                  r[i-1] = *ppp;

               x = origin[1] + r[1];
               y = origin[2] - r[2];
               }
            else
               continue;
            }


/* Draw symbols in symbol_string1 ("cCtTsSiIpPhH") */
         if ( (pchar = strchr(symbol_string1, symbol_char)) != NULL )
            {
            ifunc = pchar - symbol_string1;
            if ( isupper(*pchar) == 0 )
               DrawSymbol1(ifunc, x, y, color_rgba1[3], symbol_color, 
                           symbol_size);
            else
               DrawSymbol1(ifunc, x, y, symbol_color, symbol_color, 
                           symbol_size);
            }

         /* Draw symbols in symbol_string2 ("+xra") */
         else if ( (pchar = strchr(symbol_string2, symbol_char)) != NULL )
            {
            ifunc = pchar - symbol_string2;
            DrawSymbol2(ifunc, x, y, symbol_color, symbol_size);
            }

    }  // EOF loop on isymb

return;
}
