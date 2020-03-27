/*******************************************************************************
* jlp_GsegData_ReadExtraText.cpp
*
* Contains functions:
*    ReadExtraTextFromFile
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
* Version 16/09/2007
*******************************************************************************/
#include "jlp_gseg_data.h"            // JLP_GsegData class

// Length of line1
#define LENGTH1 256

/******************************************************************************
*
* INPUT:
*  itext : index of text item to be retrieved (from 0 to n_text_items1)
*
* OUTPUT:
*  text_str : text string of index=itext
*  anchor_text : anchor text of index=itext (e.g. NORTH, EAST, etc)
*  xtext, ytext, ztext : x, y, z coordinates of text item
*  text_coords_flag : coordinate flag ("abs" or "rel", for absolute or relative)
*
*******************************************************************************/
int JLP_GsegData::GetExtraTextData(char **text_str, char *anchor_text,
                                double *xtext, double *ytext,
                                double *ztext, int *nlines,
                                char *text_coords_flag, const int itext)
{
int status = -1;

 if((itext >= 0) && (n_text_items1 > 0) && (itext < n_text_items1)) {
   *text_str = text_items1[itext].text_str;
   strcpy(anchor_text, text_items1[itext].anchor_text);
   *xtext = text_items1[itext].x_text;
   *ytext = text_items1[itext].y_text;
   *ztext = text_items1[itext].z_text;
   *nlines = text_items1[itext].nlines;
   strcpy(text_coords_flag, text_items1[itext].coords_flag);
   status = 0;
 }

return(status);
}
/******************************************************************************
*
*******************************************************************************/
void JLP_GsegData::ReadExtraTextFromFile(char *param_filename, char *axis_type0)
{
/* Declare variables */
int i, itext, nlines, index;
unsigned int i1_str, i2_str, size, nchar;
double x_text0, y_text0, z_text0;
long int file_position, file_position_1;
char *string, anchor[10], anchor_text0[64], line1[LENGTH1], *text_str0;
char err_msg[256], buffer[2048];
FILE *fptr;

n_text_items1 = 0;
itext = 0;

/* Read text items from file */
 fptr = fopen(param_filename, "r");
 if(fptr == NULL) {
  fprintf(stderr, "ReadExtraTextFromFile/Error opening %s\n", param_filename);
  return;
  }

// Main loop for reading the file and looking for "text_string"
while ( !feof(fptr)) {
    if( fgets(line1, LENGTH1, fptr) == NULL ) break;
// Location of the current file pointer (beginning of reading)
    file_position = ftell(fptr);
// Location of first occurence of text_string
    if ( strncmp(line1, "text_string", 11) == 0 )
       {
       nlines = 0;
       nchar = 0;
       index = 0;
       n_text_items1 = 0;
       file_position_1 = file_position;
// Secondary loop starting from first occurence of text string:
       while ( !feof(fptr))
          {
          size = strlen(&line1[11]);
          if ( (string = gseg_get_string(string_get, &line1[11], &i1_str,
                                         &i2_str, &size, 1)) != NULL ) {
             nchar = nchar + size + 1;
             nlines++;
             strcpy(&buffer[index], string);
             index = index + size + 1;
             buffer[index-1] = '\n';
            } else {
            break;
            }
          if(fgets(line1, LENGTH1, fptr) == NULL ) break;
          if ( strncmp(line1, "text_string", 11) != 0 ) break;
          }
       buffer[index-1] = '\0';
       text_items1[itext].nlines = nlines;

// Allocate memory for text_str:
       if(text_items1[itext].text_str != NULL)
                     delete text_items1[itext].text_str;
       text_items1[itext].text_str = new char[nchar];
// Set that memory array to zero:
       text_str0 = text_items1[itext].text_str;
       memset(text_str0, 0, nchar);
       strcpy(text_str0, buffer);

/* Get text coordinates and anchor */
// Erase memory space:
       memset(text_items1[itext].coords_flag, 0,
              sizeof(text_items1[itext].coords_flag));
       x_text0 = 0;
       y_text0 = 0;
       z_text0 = 0;
       for ( i=1; i<=2; i++ )
         {
             if ( strncmp(line1, "text_coords_abs", 15) == 0 ||
                  strncmp(line1, "text_coords_rel", 15) == 0 )
                {
// 2d and two values if abs
                if ( (strcmp(axis_type0, "3d") != 0 &&
                      strncmp(line1, "text_coords_abs", 15) == 0 &&
                      sscanf(&line1[15], "%lf %lf",
                             &x_text0, &y_text0) == 2) ||
// 3d and three values if abs
                     (strcmp(axis_type0, "3d") == 0 &&
                      strncmp(line1, "text_coords_abs", 15) == 0 &&
                      sscanf(&line1[15], "%lf %lf %lf", &x_text0,
                             &y_text0, &z_text0) == 3) ||
// two values if rel
                     (strncmp(line1, "text_coords_rel", 15) == 0 &&
                      sscanf(&line1[15], "%lf %lf",
                             &x_text0, &y_text0) == 2))
                   {
// Read "text_coords_rel" or "text_coords_abs" and
// write the last 3 characters ("abs" or "rel") to coords_flag
                   strncpy(text_items1[itext].coords_flag, &line1[12], 3);
                   }
                else
                   {
                   sprintf(err_msg, "%s\n%s\n>%s<", line1,
                           "Invalid or incomplete text coordinates for text:",
                           text_str0);
                   JLP_ErrorDialog(err_msg);
                   exit(1);
                   }
                }
             else if ( strncmp(line1, "text_anchor", 11) == 0 )
                {
                memset(anchor, 0, sizeof(anchor));
                size = strlen(&line1[11]);
                if ( (string = gseg_get_string(string_get, &line1[11], &i1_str,
                                               &i2_str, &size, 0)) != NULL )
                   {
                   strncpy(&anchor[0], string, 9);
                   }
                else
                   {
                   sprintf(err_msg, "%s\n%s\n>%s<", line1,
                           "Text anchor not found for text:",
                           text_str0);
                   JLP_ErrorDialog(err_msg);
                   exit(1);
                   }
                } // end of loop on i=1,2

             else
                {
                sprintf(err_msg, "iline1=%d >%s<\n%s\n>%s<", (int)file_position_1, line1,
                        "Text coordinates or text anchor not found for text:",
                        text_str0);
                JLP_ErrorDialog(err_msg);
                exit(1);
                } // end of
// Read next line (for anchor)
           if ( fgets(line1, LENGTH1, fptr) == NULL ) break;
           } // end of loop for

/* Define text anchor */
       if ( strcmp(anchor, "center") == 0 )
          strcpy(anchor_text0, "CENTER");
       else if ( strcmp(anchor, "north") == 0 )
          strcpy(anchor_text0, "NORTH");
       else if ( strcmp(anchor, "northeast") == 0 )
          strcpy(anchor_text0, "NORTH_EAST");
       else if ( strcmp(anchor, "east") == 0 )
          strcpy(anchor_text0, "EAST");
       else if ( strcmp(anchor, "southeast") == 0 )
          strcpy(anchor_text0, "SOUTH_EAST");
       else if ( strcmp(anchor, "south") == 0 )
          strcpy(anchor_text0, "SOUTH");
       else if (strcmp(anchor, "southwest") == 0 )
          strcpy(anchor_text0, "SOUTH_WEST");
       else if ( strcmp(anchor, "west") == 0 )
          strcpy(anchor_text0, "WEST");
       else if ( strcmp(anchor, "northwest") == 0 )
          strcpy(anchor_text0, "NORTH_WEST");
       else
          {
          sprintf(err_msg, "%s\n%s\n>%s<", line1,
                  "Invalid text anchor for text:",
                  anchor);
          JLP_ErrorDialog(err_msg);
          exit(1);
          }
       strcpy(text_items1[itext].anchor_text, anchor_text0);
       text_items1[itext].x_text = x_text0;
       text_items1[itext].y_text = y_text0;
       text_items1[itext].z_text = z_text0;

       itext++;
       }
    else if ( strncmp(line1, "#####", 5) == 0 )
       break;

    } // EOF while

 fclose(fptr);

n_text_items1 = itext;

return;
}
