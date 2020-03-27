/*******************************************************************************
* jlp_GsegData_ReadExtraRectangles.cpp
*
* Read the data needed to plot extra rectangles from the parameter file
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
* Version 04/05/2017
*******************************************************************************/
#include <math.h>
#include <locale.h>
#include "jlp_gseg_data.h"         // JLP_GsegData class

/******************************************************************************
* Read the data needed to plot the irect th extra rectangle from the list 
*
*******************************************************************************/
int JLP_GsegData::GetExtraRectangleData(double *x0, double *y0, double *width0,
                                 double *height0, double *angle0,
                                 UINT32 *line_color0, char *line_stylechar0,
                                 unsigned int *line_width0, int irect)
{
int status = -1;
 if((n_extra_rectangles1 > 0) && (irect >= 0) && (irect < n_extra_rectangles1)) {
    *x0 = extra_rectangles1[irect].x0;
    *y0 = extra_rectangles1[irect].y0;
    *width0 = extra_rectangles1[irect].width;
    *height0 = extra_rectangles1[irect].height;
    *line_color0 = extra_rectangles1[irect].line_color;
    *line_stylechar0 = extra_rectangles1[irect].line_stylechar;
    *line_width0 = extra_rectangles1[irect].line_width;
    status = 0;
   }
return(status);
}
/******************************************************************************
* Read the data needed to plot extra rectangles from the parameter file
*
*******************************************************************************/
void JLP_GsegData::ReadExtraRectanglesFromFile(char *param_filename)
{
/* Declare variables */
int nval1, nval2, irect, status;
UINT32 line_color0, style_color0;
double x0, y0, width, height, angle;
char line_char0, color_char0, line0[256];
const char *error_str[] =
    { "Invalid or missing rectangle coordinates.",
      "Invalid rectangle color character.",
      "Invalid or missing rectangle line or color specification.",
      "Invalid rectangle line character." };
FILE *fptr;

n_extra_rectangles1 = 0;

/* JLP 2016 to solve problem of numeric dot vs numeric comma */
/* SHOULD BE CALLED HERE...*/
/* In the "C" locale, a decimal_point is "." */
if (NULL == setlocale(LC_NUMERIC, "C")) {
  fprintf(stderr, " Error setting numeric dot in setlocale !\n");
  exit(-1);
}

/* Return if no rectangles to plot */
   fptr = fopen(param_filename, "r");
   if(fptr == NULL) {
     return;
     }
   irect = 0;
   while ( fgets(line0, 256, fptr) != NULL ) {
      if ( strncmp(line0, "rect_coords", 11) == 0 )
         irect++;
     }
   fclose(fptr);
   if ( irect == 0 ) return;

/* Read the parameters of the rectangles */
   irect = 0;
   fptr = fopen(param_filename, "r");
   while ( fgets(line0, 256, fptr) != NULL )
      {
// Detect "rect_coords"
      if ( strncmp(line0, "rect_coords", 11) == 0 )
         {
         if ( sscanf(&line0[11], "%lf %lf %lf %lf %lf",
                     &x0, &y0, &width, &height, &angle) != 5 )
            {
            JLP_ErrorDialog(error_str[0]);
            exit(1);
            }

// Default values:
        extra_rectangles1[irect].line_width = 1;
        extra_rectangles1[irect].line_stylechar = 'l';
        extra_rectangles1[irect].line_color = 0;

// Read next line:
        if ( fgets(line0, 256, fptr) != NULL ) {
           if ( strncmp(line0, "rect_style", 10) == 0 )
              {
             nval1 = sscanf(&line0[10], " %c 0x%x",
                    line_char0, line_color0);
            if ( nval1 == 2 ) {
               extra_rectangles1[irect].line_stylechar = line_char0;
               extra_rectangles1[irect].line_color = line_color0;
               }
            nval2 = sscanf(&line0[10], " %c 0X%x",
                    line_char0, line_color0);
            if ( nval2 == 2 ) {
               extra_rectangles1[irect].line_stylechar = line_char0;
               extra_rectangles1[irect].line_color = line_color0;
               }
            if ( nval1 == 2 || nval2 == 2) {
               extra_rectangles1[irect].line_stylechar = line_char0;
               extra_rectangles1[irect].line_color = line_color0;
               sscanf(&line0[10], " %*c %*s %u", &(extra_rectangles1[irect].line_width));
               }
// colorchar (compared to  color_string = "kaswrylqbfmogtnpx")
              else if(sscanf(&line0[10], " %c %c",
                         &(extra_rectangles1[irect].line_stylechar), &color_char0) == 2)
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
                  extra_rectangles1[irect].line_color = style_color0;
                  sscanf(&line0[10], " %*c %*c %u",
                         &(extra_rectangles1[irect].line_width));
                  }
                 }

/* Check line character */
              if ( extra_rectangles1[irect].line_stylechar != 'l'
                   && extra_rectangles1[irect].line_stylechar != 'd'
                   && extra_rectangles1[irect].line_stylechar != '.' )
                  {
                  JLP_ErrorDialog(error_str[3]);
                  exit(1);
                  }
           
                 } // EOF reading line containing "rect_style"
               } // EOF reading next line
          irect++;
          } // EOF detection of "rect_coords"
      else if ( strncmp(line0, "#####", 5) == 0 )
         break;
      }  // EOF while

fclose(fptr);

n_extra_rectangles1 = irect;
return;
}
