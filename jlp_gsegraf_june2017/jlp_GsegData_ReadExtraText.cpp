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
* Version 16/05/2007
*******************************************************************************/
#include "jlp_gseg_data.h"            // JLP_GsegData class

/******************************************************************************
*
*******************************************************************************/
int JLP_GsegData::GetExtraTextData(char **text_str, char *anchor_text,
                                double *xtext, double *ytext,
                                double *ztext, char *text_coords_flag,
                                const int itext)
{
int status = -1;

 if((itext >= 0) && (n_text_items1 > 0) && (itext < n_text_items1)) {
   *text_str = text_items1[itext].text_str;
   strcpy(anchor_text, text_items1[itext].anchor_text);
   *xtext = text_items1[itext].x_text;
   *ytext = text_items1[itext].y_text;
   *ztext = text_items1[itext].z_text;
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
int i, itext, nlines, index, raise_to_top;
unsigned int i1_str, i2_str, size, nchar;
double x_text0, y_text0, z_text0;
long int file_position, file_position_1;
char *string, anchor[10], anchor_text0[64], line1[256], *text_str0;
FILE *fptr;

n_text_items1 = 0;
itext = 0;

/* Read text items from file */
 fptr = fopen(param_filename, "r");
 if(fptr == NULL) {
  fprintf(stderr, "ReadExtraTextFromFile/Error opening %s\n", param_filename);
  return;
  }
 file_position = ftell(fptr);

// Main loop for reading the file and looking for "text_string"
 while ( fgets(line1, 256, fptr) != NULL )
    {
    nlines = 0;
// Location of first occurence of text_string
    if ( strncmp(line1, "text_string", 11) == 0 )
       {
       n_text_items1 = 0;
       nlines++;
       file_position_1 = file_position;
// Secondary loop starting from first occurence of text string: 
       while ( fgets(line1, 256, fptr) != NULL )
          {
          if ( strncmp(line1, "text_string", 11) == 0 )
             nlines++;
          else
             break;
          }
       text_items1[itext].nlines = nlines;

/* Get text-line sizes */
       fseek(fptr, file_position_1, SEEK_SET);
       nchar = 0;
       for (i = 1; i <= nlines; i++ )
          {
          fgets(line1, 256, fptr);
          size = strlen(&line1[11]);
          if ( (string = gseg_get_string(string_get, &line1[11], &i1_str, 
                                        &i2_str, &size, 1)) != NULL )
             nchar = nchar + size + 1;
          }

// Allocate memory:
       if(text_items1[itext].text_str != NULL) 
                     delete text_items1[itext].text_str;
       text_items1[itext].text_str = new char[nchar];
       text_str0 = text_items1[itext].text_str;
       memset(text_str0, 0, sizeof(text_str0));

/* Read text lines */
       fseek(fptr, file_position_1, SEEK_SET);
       index = 0;
       for ( i=1; i<= nlines; i++ )
          {
          fgets(line1, 256, fptr);
          size = strlen(&line1[11]);
          if ( (string = gseg_get_string(string_get, &line1[11], &i1_str, 
                                         &i2_str, &size, 1)) != NULL )
             {
             strcpy(&text_str0[index], string);
             index = index + size + 1;
             text_str0[index-1] = '\n';
             }
          }
       text_str0[index-1] = '\0';

/* Get text coordinates and anchor */
       memset(text_items1[itext].coords_flag, 0, 
              sizeof(text_items1[itext].coords_flag));
       x_text0 = 0;
       y_text0 = 0;
       z_text0 = 0;
       for ( i=1; i<=2; i++ )
          if ( fgets(line1, 256, fptr) != NULL )
             {
             if ( strncmp(line1, "text_coords_abs", 15) == 0 ||
                  strncmp(line1, "text_coords_rel", 15) == 0 )
                {
                if ( (strcmp(axis_type0, "3d") != 0 &&
                      strncmp(line1, "text_coords_abs", 15) == 0 &&
                      sscanf(&line1[15], "%lf %lf", 
                             &x_text0, &y_text0) == 2) ||
                     (strcmp(axis_type0, "3d") == 0 &&
                      strncmp(line1, "text_coords_abs", 15) == 0 &&
                      sscanf(&line1[15], "%lf %lf %lf", &x_text0, 
                             &y_text0, &z_text0) == 3) ||
                     (strncmp(line1, "text_coords_rel", 15) == 0 &&
                      sscanf(&line1[15], "%lf %lf", 
                             &x_text0, &y_text0) == 2))
                   strncpy(text_items1[itext].coords_flag, &line1[12], 3);

                else
                   {
                   size = strlen("Invalid or incomplete text coordinates for text:\n") +
                          strlen(text_str0);
                   string = new char[size + 1];
                   sprintf(string, "%s%s",
                           "Invalid or incomplete text coordinates for text:\n",
                           text_str0);
                   JLP_ErrorDialog(string);
                   delete[] string;
                   exit(1);
                   }
                }

             else if ( strncmp(line1, "text_anchor", 11) == 0 )
                {
                memset(anchor, 0, sizeof(anchor));
                size = strlen(&line1[11]);
                if ( (string = gseg_get_string(string_get, &line1[11], &i1_str, 
                                          &i2_str, &size, 0)) != NULL )
                   strncpy(&anchor[0], string, 9);
                else
                   {
                   size = strlen("Text anchor not found for text:\n") +
                          strlen(text_str0);
                   string = new char[size + 1];
                   sprintf(string, "%s%s", "Text anchor not found for text:\n", 
                           text_str0);
                   JLP_ErrorDialog(string);
                   delete[] string;
                   exit(1);
                   }
                }

             else
                {
                size = strlen("Text coordinates or text anchor not found for text:\n") +
                              strlen(text_str0);
                string = new char[size + 1];
                sprintf(string, "%s%s",
                        "Text coordinates or text anchor not found for text:\n",
                        text_str0);
                JLP_ErrorDialog(string);
                delete[] string;
                exit(1);
                }
             }

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
          size = strlen("Invalid text anchor for text:\n") +
                 strlen(text_str0);
          string = new char[size + 1];
          sprintf(string, "%s%s",
                  "Invalid text anchor for text:\n", text_str0);
          JLP_ErrorDialog(string);
          delete[] string;
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

    else
       file_position = ftell(fptr);
    }

 fclose(fptr);

n_text_items1 = itext;

return;
}
