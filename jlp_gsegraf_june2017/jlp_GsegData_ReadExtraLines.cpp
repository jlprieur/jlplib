/*******************************************************************************
* jlp_GsegData_ReadExtraLinesFromFile.cpp
*
* Read data to plot extra lines.
*
* JLP
* Version 09/05/2017
*******************************************************************************/
#include <math.h>
#include <locale.h>
#include "jlp_gseg_data.h"

/******************************************************************************
*
*******************************************************************************/
int JLP_GsegData::GetExtraLineData(double *line_coords, UINT32 *line_color, 
                            char *line_stylechar, unsigned int *line_width, 
                            int iline)
{
int i, status = -1;
 if((n_extra_lines1 > 0) && (iline >= 0) && (iline < n_extra_lines1)) {
    for(i = 0; i < 6; i++) line_coords[i] = extra_lines1[iline].coords[i]; 
    *line_color = extra_lines1[iline].line_color;
    *line_width = extra_lines1[iline].line_width;
    *line_stylechar = extra_lines1[iline].line_stylechar;
    status = 0;
   } 
return(status);
}
/******************************************************************************
* Read data to plot extra lines.
*
*******************************************************************************/
void JLP_GsegData::ReadExtraLinesFromFile(char *param_filename, 
                                          const int ncoords0)
{
/* Declare variables */
int i, nval1, nval2, status;
unsigned int line_color0;
char *pchar, line_char0, color_char0, line0[256];
UINT32 style_color0;
const char *error_str[] =
      { "Invalid or missing line coordinates.",
        "Invalid line color character.",
        "Invalid or missing line or color specification.",
        "Invalid line character." };
FILE *fptr;

n_extra_lines1 = 0;

/* JLP 2016 to solve problem of numeric dot vs numeric comma */
/* SHOULD BE CALLED HERE...*/
/* In the "C" locale, a decimal_point is "." */
if (NULL == setlocale(LC_NUMERIC, "C")) {
  fprintf(stderr, " Error setting numeric dot in setlocale !\n");
  exit(-1);
}

 /* Return if no lines to plot */
  fptr = fopen(param_filename, "r");
  if(fptr == NULL){
    fprintf(stderr, "ReadLinesFromFile/Error opening param_file=%s\n", 
            param_filename);
    return;
    }
  i = 0;
  while ( fgets(line0, 256, fptr) != NULL )
     if ( strncmp(line0, "line_coords", 11) == 0 )
        i++;
  fclose(fptr);
  if ( i == 0 ) return;

/* Draw lines */
fptr = fopen(param_filename, "r");
i = 0;
while ( fgets(line0, 256, fptr) != NULL )
   {
// Detect "line_coords"
   if ( strncmp(line0, "line_coords", 11) == 0 )
      {
      if ( ncoords0 == 4 )
         {
         if(sscanf(&line0[11], " %lf %lf %lf %lf",
                 &extra_lines1[i].coords[0], &extra_lines1[i].coords[2],
                 &extra_lines1[i].coords[1], &extra_lines1[i].coords[3]) != 4)
            {
            JLP_ErrorDialog(error_str[0]);
            exit(1);
            }
         }

      else if ( ncoords0 == 6 )
         {
         if(sscanf(&line0[11], " %lf %lf %lf %lf %lf %lf",
                 &extra_lines1[i].coords[0], &extra_lines1[i].coords[3],
                 &extra_lines1[i].coords[1], &extra_lines1[i].coords[4],
                 &extra_lines1[i].coords[2], &extra_lines1[i].coords[5]) != 6)
            {
            JLP_ErrorDialog(error_str[0]);
            exit(1);
            }
         }

// Default values:
      extra_lines1[i].line_width = 1;
      extra_lines1[i].line_stylechar = 'l';
      extra_lines1[i].line_color = 0;

// Read next line
      if ( fgets(line0, 256, fptr) != NULL ) {
// Detect "line_style"
         if ( strncmp(line0, "line_style", 10) == 0 )
            {
            nval1 = sscanf(&line0[10], " %c 0x%x", 
                    line_char0, line_color0);
            if ( nval1 == 2 ) {
               extra_lines1[i].line_stylechar = line_char0;
               extra_lines1[i].line_color = line_color0;
               }
            nval2 = sscanf(&line0[10], " %c 0X%x", 
                    line_char0, line_color0);
            if ( nval2 == 2 ) {
               extra_lines1[i].line_stylechar = line_char0;
               extra_lines1[i].line_color = line_color0;
               }
            if ( nval1 == 2 || nval2 == 2) {
               extra_lines1[i].line_stylechar = line_char0;
               extra_lines1[i].line_color = line_color0;
               sscanf(&line0[10], " %*c %*s %u", &(extra_lines1[i].line_width));
               }
// colorchar (compared to  color_string = "kaswrylqbfmogtnpx")
            else if ( sscanf(&line0[10], " %c %c", 
                         &(extra_lines1[i].line_stylechar), &color_char0) == 2 )
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
                  extra_lines1[i].line_color = style_color0;
                  sscanf(line0, "%*s %*c %*c %u", &(extra_lines1[i].line_width));
                  }
               }

            else
               {
               JLP_ErrorDialog(error_str[2]);
               exit(1);
               }

/* Check line type */
            if ((extra_lines1[i].line_stylechar != 'l') 
                 && (extra_lines1[i].line_stylechar != 'd')
                 && (extra_lines1[i].line_stylechar != '.'))
               {
               JLP_ErrorDialog(error_str[3]);
               exit(1);
               }
            } // EOF detect "line_style"
         } // EOF reading next line 

      i++;
      } // EOF detection of "line_coords"

   else if ( strncmp(line0, "#####", 5) == 0 )
      break;
   } // EOF while

fclose(fptr);

n_extra_lines1 = i;
return;
}
