/*******************************************************************************
* jlp_Gsegraf_ReadParamFile.cpp
*
* Reads plot-parameter file.
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
* Version 15/04/2017
*******************************************************************************/
#include <ctype.h>
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"      // JLP_GsegAxes class
#include "jlp_gseg_data.h"      // JLP_GsegData class

/******************************************************************************
*
*******************************************************************************/
int JLP_Gsegraf::ReadParamFile(char *param_filename, char **save_filename_0, 
                               int *close_flag_0)
{
int nplots0, ncoords0, status; 
char axis_type0[64];

// Initialize output values to zero:
 *save_filename_0 = NULL;
 *close_flag_0 = 0;

// Read filenames and various other parameters:
 jlp_gseg_data1->ReadFileParamsFromFile(param_filename, &nplots0);
 if(nplots0 == 0) {
   fprintf(stderr, "ReadParamFile/Error in ReadFileParamsFromFile: nplots=0\n");
   return(-1);
   }

// Set data plot types:
 status = jlp_gseg_data1->ReadPlotTypesFromFile(param_filename);
 if(status != 0) return(-1);

// Decode axis type from the parameter file
 jlp_gseg_axes1->ReadAxisTypeFromFile(param_filename);
 jlp_gseg_axes1->GetAxisType(axis_type0);
 ncoords0 = jlp_gseg_axes1->NCoords();

// Read plot styles, extra lines, ellipses, etc
 status = jlp_gseg_data1->ReadComplementoryDataFromFile(param_filename, nplots0,
                                                        ncoords0, axis_type0);
 if(status != 0) return(-1);

// Read grid parameters and other various other parameters:
 jlp_gseg_axes1->ReadAxisLimitsFromFile(param_filename);

// Get background color or background image 
 jlp_gseg_axes1->ReadBackgroundStyleFromFile(param_filename);

// Read fonts characteristics 
 jlp_gseg_axes1->ReadFontsFromFile(param_filename);

// Read color settings (loaded in JLP_Gsegraf private parameters)
 ReadColorSettingsFromFile(param_filename, nplots0, ncoords0, axis_type0,
                           save_filename_0, close_flag_0);

return(0);
}
/******************************************************************************
* Replace the ReadParamFile routine when there is no input parameter file
* Save plot parameters to private variables of JLP_Gsegraf and JLP_GsegAxes
*
* TO BE DONE
*******************************************************************************/
int JLP_Gsegraf::InitPlotWithoutParamFile()
{
return(0);
}
/***********************************************************************
* Read color settings (loaded in JLP_Gsegraf private parameters)
*
************************************************************************/
int JLP_Gsegraf::ReadColorSettingsFromFile(char *param_filename, int nplots0,
                                           int ncoords0, char *axis_type0,
                                           char **save_filename_0,
                                           int *close_flag_0)
{
/* Declare variables */
int i, ch, index_stemflags; 
int istemflag;
unsigned int i1_str, i2_str, size;
char *string;
const char *error_str[] =
      { "Invalid ninterp value.",
        "Invalid or missing contour color.",
        "Invalid or missing mesh color.",
        "Invalid window-size data;\ntwo integers required."};
FILE *fptr;

// Initialize output values to zero:
 *save_filename_0 = NULL;
 *close_flag_0 = 0;

/* Re-open plot-parameter file 
* (and leave it open in this routine until returning) 
*/
 if ( (fptr = fopen(param_filename, "r")) == NULL )
    {
    size = strlen("Cannot open plot-parameter file:\n") 
           + strlen(param_filename);
    string = new char[size + 1];
    sprintf(string, "%s%s", "Cannot open plot-parameter file:\n", 
            param_filename);
    JLP_ErrorDialog(string);
    delete[] string;
    return(-1);
    }

/* Get maximum line length */
 maxline1 = 0;
 i = 0;
 while ( (ch = fgetc(fptr)) != EOF )
    {
    if ( ch != '\n' )
       i++;
    else
       {
       if ( maxline1 < i )
          maxline1 = i;
       i = 0;
       }
    }
 maxline1++;

/****************************************************************************
* Increase maxline1 for use with char *fgets(char *s, int n, FILE *stream).
* fgets reads at most the next n-1 characters into the array s. It is desired
* that fgets read the terminating character so that following uses of fgets
* start with the next line in the file.
****************************************************************************/
 maxline1++;
 if(line1 != NULL) delete[] line1;
 line1 = new char[maxline1];

/* Set default stem flags and stem values */
 for ( i=1; i<=nplots0; i++ )
    {
    strcpy(&stemflags[(i-1)*4], "off");
    stemvalues[i-1] = 0.0;   /* not used */
    }

/* Read remainder of plot-parameter file */
 istemflag = 0;
 index_stemflags = 0;
 rewind(fptr);
 while ( fgets(line1, maxline1, fptr) != NULL ) {

   if ( strncmp(line1, "stems", 5) == 0 )
       {
       istemflag++;
       if ( istemflag <= nplots0 )
          {
          if ( sscanf(&line1[5], "%lf", &stemvalues[istemflag-1]) == 1 )
             {
             strcpy(&stemflags[index_stemflags], "num");
             if ( strcmp(axis_type0, "semilogy") == 0 ||
                  strcmp(axis_type0, "loglog")   == 0 )
                {
                if ( stemvalues[istemflag-1] != 0.0 )
                   stemvalues[istemflag-1] = log10(fabs(stemvalues[istemflag-1]));
                else
                   stemvalues[istemflag-1] = log10(DBLE_MIN_VALUE);
                }
             index_stemflags = index_stemflags + 4;
             }
          else
             {
             size = strlen(&line1[5]);
             if ( (string = gseg_get_string(string_get, &line1[5], &i1_str, 
                                            &i2_str, &size, 0)) != NULL )
                {
                strncpy(&stemflags[index_stemflags], string, 3);
                index_stemflags = index_stemflags + 4;
                }
             }
          }
       }

    else if ( strncmp(line1, "save_close", 10) == 0 )
       {
       size = strlen(&line1[10]);
       if ( (string = gseg_get_string(string_get, &line1[10], &i1_str, 
                                      &i2_str, &size, 0)) != NULL )
          {
          *save_filename_0 = new char[size + 1];
          strcpy(*save_filename_0, string);
          *close_flag_0 = 1;
          }
       }

    else if ( strncmp(line1, "save", 4) == 0 )
       {
       size = strlen(&line1[4]);
       if ( (string = gseg_get_string(string_get, &line1[4], &i1_str, 
                                      &i2_str, &size, 0)) != NULL )
          {
          *save_filename_0 = new char[size + 1];
          strcpy(*save_filename_0, string);
          }
       }

// JLP2016: WARNING: there may be a conflict wit the the window size of the
// calling program !
    else if ( strncmp(line1, "window_size", 11) == 0 )
       {
       if ( sscanf(&line1[11], "%d %d", &window_width1, &window_height1 ) 
            != 2 )
          {
          JLP_ErrorDialog(error_str[3]);
          return(-1);
          }
       }

    else if ( strncmp(line1, "#####", 5) == 0 )
       break;

 }  // EOF while()

 fclose(fptr);

return(0);
}
