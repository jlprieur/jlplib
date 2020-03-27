/*******************************************************************************
*
* jlp_GsegData_ReadExtraSymbols.cpp
*
* Read data needed for plotting extra symbols 
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
* Version 12/05/2017
*******************************************************************************/
#include <ctype.h>
#include <locale.h>
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"   // JLP_GsegAxes class 
#include "jlp_gseg_data.h"   // JLP_GsegData class 

/******************************************************************************
*
*******************************************************************************/
int JLP_GsegData::GetExtraSymbolData(double *symbol_coords, 
                                     UINT32 *symbol_color,
                                     char *symbol_stylechar, 
                                     unsigned int *symbol_size,
                                     int isymb)
{
int i, status = -1;
 if((n_extra_symbols1 > 0) && (isymb >= 0) && (isymb < n_extra_symbols1)) {
    for(i = 0; i < 6; i++) symbol_coords[i] = extra_symbols1[isymb].coords[i];
    *symbol_color = extra_symbols1[isymb].symbol_color;
    *symbol_stylechar = extra_symbols1[isymb].symbol_stylechar;
    *symbol_size = extra_symbols1[isymb].symbol_size;
    status = 0;
   }
return(status);
}

/**************************************************************************
* Read data needed for plotting extra symbols 
**************************************************************************/
void JLP_GsegData::ReadExtraSymbolsFromFile(char *param_filename,
                                           const int ncoords0)

{
/* Declare variables */
int i, nval1, nval2, status;
unsigned int index, symbol_size;
UINT32 symbol_color0, style_color0;
char *pchar, symbol_char0, color_char0, buffer[64], line0[256];
const char *error_str[] =
   { "Invalid or missing symbol coordinates.",
     "Invalid symbol color character.",
     "Invalid or missing symbol or color specification.",
     "Invalid symbol character." };
FILE *fptr;

/* JLP 2016 to solve problem of numeric dot vs numeric comma */
/* SHOULD BE CALLED HERE...*/
/* In the "C" locale, a decimal_point is "." */
if (NULL == setlocale(LC_NUMERIC, "C")) {
  fprintf(stderr, " Error setting numeric dot in setlocale !\n");
  exit(-1);
}

// Check if ncoords is OK
 if((ncoords0 != 2) && (ncoords0 != 3)) {
   fprintf(stderr, "ReadSymbolsFromFile/error: ncoords=%d can't read param_file=%s\n",
   ncoords0, param_filename);
   return;
   }
         
/* Return if no symbols to plot */
  fptr = fopen(param_filename, "r");
  if(fptr == NULL){
    fprintf(stderr, "ReadSymbolsFromFile/Error opening param_file=%s\n",
            param_filename);
    return;
    }
  i = 0;
  while ( fgets(line0, 256, fptr) != NULL ) {
     if ( strncmp(line0, "symbol_coords", 13) == 0 )
        i++;
     }
  fclose(fptr);
  if ( i == 0 ) return;

// Read plot data of symbols 
   fptr = fopen(param_filename, "r");
   i = 0;
   while ( fgets(line0, 256, fptr) != NULL )
      {
// symbol_coords detected
      if ( strncmp(line0, "symbol_coords", 13) == 0 )
         {
         if ( ncoords0 == 2 )
            {
            if ( sscanf(&line0[13], " %lf %lf",
                        &(extra_symbols1[i].coords[0]), 
                        &(extra_symbols1[i].coords[1])) != 2 )
               {
               JLP_ErrorDialog(error_str[0]);
               exit(1);
               }
            }

         else if ( ncoords0 == 3 )
            {
            if ( sscanf(&line0[13], " %lf %lf %lf",
                        &(extra_symbols1[i].coords[0]), 
                        &(extra_symbols1[i].coords[1]), 
                        &(extra_symbols1[i].coords[2])) != 3 )
               {
               JLP_ErrorDialog(error_str[0]);
               exit(1);
               }
            }
// Deafult values:
      extra_symbols1[i].symbol_size = 6;
      extra_symbols1[i].symbol_stylechar = 'C';
      extra_symbols1[i].symbol_color = 0;

// Read next line with symbol type and color 
      if ( fgets(line0, 256, fptr) != NULL ) { 
         if ( strncmp(line0, "symbol_style", 12) == 0 )
            {
            nval1 = sscanf(&line0[12], " %c 0x%x",
                    symbol_char0, symbol_color0);
            if ( nval1 == 2 ) {
               extra_symbols1[i].symbol_stylechar = symbol_char0;
               extra_symbols1[i].symbol_color = symbol_color0;
               }
            nval2 = sscanf(&line0[12], " %c 0X%x",
                    symbol_char0, symbol_color0);
            if ( nval2 == 2 ) {
               extra_symbols1[i].symbol_stylechar = symbol_char0;
               extra_symbols1[i].symbol_color = symbol_color0;
               }
            if ( nval1 == 2 || nval2 == 2) {
               extra_symbols1[i].symbol_stylechar = symbol_char0;
               extra_symbols1[i].symbol_color = symbol_color0;
               sscanf(&line0[12], " %*c %*s %u", 
                      &(extra_symbols1[i].symbol_size));
               }

// color_char0 (compared to  color_string = "kaswrylqbfmogtnpx")
            else if ( sscanf(&line0[12], " %c %c", 
                        &extra_symbols1[i].symbol_stylechar, &color_char0) == 2 )
               {
                status = jlp_gsegraf1->SetStyleColorFromStyleChar(&style_color0,
                                                                  color_char0);

                if ( status != 0 )
                  {
                  JLP_ErrorDialog(error_str[1]);
                  exit(1);
                  }
                else
                  {
                  extra_symbols1[i].symbol_color = style_color0;
                  sscanf(&line0[12], " %*c %*c %u", 
                         &extra_symbols1[i].symbol_size);
                  }
               }

             else
               {
               JLP_ErrorDialog(error_str[2]);
               exit(1);
               }

/* Check symbol type */
            strcpy(buffer, "cCtTsSiIpPhH+xra");
            if ( (pchar = strchr(buffer, extra_symbols1[i].symbol_stylechar)) == NULL )
               {
               JLP_ErrorDialog(error_str[3]);
               exit(1);
               }
           } // EOF detect "symbol_style" 
          } // EOF read next line
      i++;
      } // EOF detection of "symbol_coords"
   else if ( strncmp(line0, "#####", 5) == 0 )
      break;
   } // EOF while

n_extra_symbols1 = i;
fclose(fptr);

return;
}
