/*******************************************************************************
*
* jlp_GsegData_ReadExtraLegends.cpp
*
* Read the legend from the parameter file.
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
* Version 15/05/2017
*******************************************************************************/
#include "jlp_gseg_data.h"    // JLP_GsegData class

/******************************************************************************
*
*******************************************************************************/
int JLP_GsegData::GetLegendData(char **legend_str, char *anchor_text, 
                                double *xlegend, double *ylegend, 
                                double *zlegend, char *legend_coords_flag,
                                int *nlines)
{
int status = -1;

 if((legend_str1 != NULL) && (n_legend_lines1 > 0)) {
   *legend_str = legend_str1;
   strcpy(anchor_text, legend_anchor_text1);
   strcpy(legend_coords_flag, legend_coords_flag1);
   *xlegend = xlegend1;
   *ylegend = ylegend1;
   *zlegend = zlegend1;
   *nlines = n_legend_lines1;
   status = 0;
 }

return(status);
}
/******************************************************************************
*
*******************************************************************************/
void JLP_GsegData::ReadTheLegendFromFile(char *param_filename, char *axis_type0)
{
/* Declare variables */
int i, nlines, index; 
unsigned int i1_str, i2_str, size, nchar;
long int file_position, file_position_1;
char *string, anchor[10];
char line1[256];
char error_str0[256] ;
FILE *fptr;

n_legend_lines1 = 0;
if(legend_str1 != NULL) delete legend_str1;
legend_str1 = NULL;

/* Get number of legend lines */
 nlines = 0;
 fptr = fopen(param_filename, "r");
 // Get the position at the end of this line,
 // that will be position at the beginning of the line to be read
 // (position in bytes from the beginning of the file)
 file_position = ftell(fptr);
 while ( fgets(line1, 256, fptr) != NULL )
    {
    if ( strncmp(line1, "legend_string", 13) == 0 )
       {
       nlines++;
       file_position_1 = file_position;
       while ( fgets(line1, 256, fptr) != NULL )
          {
          if ( strncmp(line1, "legend_string", 13) == 0 )
             nlines++;
          else
             break;
          }
       break;
       }
    else if ( strncmp(line1, "#####", 5) == 0 )
        break;
 // Get the position at the end of this line,
    file_position = ftell(fptr);
    }
 if ( nlines == 0 ) return;

// Get legend-line sizes to determine the total size 
   fseek(fptr, file_position_1, SEEK_SET);
   nchar = 0;
   for ( i=0; i< nlines; i++ )
      {
      fgets(line1, 256, fptr);
      size = strlen(&line1[13]);
      if ( (string = gseg_get_string(string_get, &line1[13], &i1_str, &i2_str, 
                                &size, 1)) != NULL )
         nchar = nchar + size + 1;
      }

// Allocate memory to legend_str1
   legend_str1 = new char[nchar];
   memset(legend_str1, 0, sizeof(legend_str1));

// Goto file_position1, read the legend lines and load it to legend_str1 
   fseek(fptr, file_position_1, SEEK_SET);
   index = 0;
   for ( i=0; i< nlines; i++ )
      {
      fgets(line1, 256, fptr);
      size = strlen(&line1[13]);
      if ( (string = gseg_get_string(string_get, &line1[13], &i1_str, &i2_str, 
                                &size, 1)) != NULL )
         {
         strcpy(&legend_str1[index], string);
         index = index + size + 1;
         legend_str1[index-1] = '\n';
         }
      } // EOF loop on i
   legend_str1[index-1] = '\0';

/* Read the two next lines, in order to get legend coordinates and anchor */
   memset(legend_coords_flag1, 0, sizeof(legend_coords_flag1));
   for ( i=0; i<2; i++ )
      {
      if ( fgets(line1, 256, fptr) != NULL )
         {
         if ( strncmp(line1, "legend_coords_abs", 17) == 0 ||
              strncmp(line1, "legend_coords_rel", 17) == 0 )
            {
            if ( (strcmp(axis_type0, "3d") != 0 &&
                  strncmp(line1, "legend_coords_abs", 17) == 0 &&
                  sscanf(&line1[17], "%lf %lf", &xlegend1, &ylegend1) == 2) ||
                 (strcmp(axis_type0, "3d") == 0 &&
                  strncmp(line1, "legend_coords_abs", 17) == 0 &&
                  sscanf(&line1[17], "%lf %lf %lf", 
                         &xlegend1, &ylegend1, &zlegend1) == 3) ||
                 (strncmp(line1, "legend_coords_rel", 17) == 0 &&
                  sscanf(&line1[17], "%lf %lf", &xlegend1, &ylegend1) == 2) )
               {
               strncpy(legend_coords_flag1, &line1[14], 3);
               }
            else
               {
               sprintf(error_str0, "Invalid or incomplete legend coordinates: >%s<",
                      line1);
               JLP_ErrorDialog(error_str0);
               fclose(fptr);
               exit(1);
               }
            }

         else if ( strncmp(line1, "legend_anchor", 13) == 0 )
            {
            memset(anchor, 0, sizeof(anchor));
            size = strlen(&line1[13]);
            if ( (string = gseg_get_string(string_get, &line1[13], &i1_str, 
                                           &i2_str, &size, 0)) != NULL )
               {
               strncpy(&anchor[0], string, 9);
               }
            else
               {
               sprintf(error_str0, "Invalid legend anchor: >%s<", line1);
               JLP_ErrorDialog(error_str0);
               fclose(fptr);
               exit(1);
               }
            }

         else
            {
            sprintf(error_str0, "Legend coordinates or legend anchor not found: >%s<",
                      line1);
            JLP_ErrorDialog(error_str0);
            fclose(fptr);
            exit(1);
            }
         } // EOF fgets()

      } // EOF loop on i

fclose(fptr);

// Conversion to upper case:
   if ( strcmp(anchor, "center") == 0 )
      {
      strcpy(legend_anchor_text1, "CENTER");
      }
   else if ( strcmp(anchor, "north") == 0 )
      {
      strcpy(legend_anchor_text1, "NORTH");
      }
   else if ( strcmp(anchor, "northeast") == 0 )
      {
      strcpy(legend_anchor_text1, "NORTH_EAST");
      }
   else if ( strcmp(anchor, "east") == 0 )
      {
      strcpy(legend_anchor_text1, "EAST");
      }
   else if ( strcmp(anchor, "southeast") == 0 )
      {
      strcpy(legend_anchor_text1, "SOUTH_EAST");
      }
   else if ( strcmp(anchor, "south") == 0 )
      {
      strcpy(legend_anchor_text1, "SOUTH");
      }
   else if ( strcmp(anchor, "southwest") == 0 )
      {
      strcpy(legend_anchor_text1, "SOUTH_WEST");
      }
   else if ( strcmp(anchor, "west") == 0 )
      {
      strcpy(legend_anchor_text1, "WEST");
      }
// Default is NORTH_WEST:
//   else if ( strcmp(anchor, "northwest") == 0 )
     else
      {
      strcpy(legend_anchor_text1, "NORTH_WEST");
      }

// Save to private variables:
n_legend_lines1 = nlines;

return;
}
