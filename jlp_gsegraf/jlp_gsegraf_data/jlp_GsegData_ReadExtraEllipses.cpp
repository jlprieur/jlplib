/*******************************************************************************
* jlp_GsegData_ReadExtraEllipses.cpp
*
* Read plot data to draw ellipses.
*
* JLP
* Version 12/05/2017
*******************************************************************************/
#include <math.h>
#include <locale.h>
#include "jlp_gseg_data.h"         // JLP_GsegData class

/******************************************************************************
*
*******************************************************************************/
int JLP_GsegData::GetExtraEllipseData(double *x0, double *y0, double *width0, 
                                 double *height0, double *angle0, 
                                 UINT32 *line_color0, char *line_stylechar0, 
                                 unsigned int *line_width0, int iell)
{
int status = -1;
 if((n_extra_ellipses1 > 0) && (iell >= 0) && (iell < n_extra_ellipses1)) {
    *x0 = extra_ellipses1[iell].x0;
    *y0 = extra_ellipses1[iell].y0;
    *width0 = extra_ellipses1[iell].width;
    *height0 = extra_ellipses1[iell].height;
    *line_color0 = extra_ellipses1[iell].line_color;
    *line_stylechar0 = extra_ellipses1[iell].line_stylechar;
    *line_width0 = extra_ellipses1[iell].line_width;
    status = 0;
   }
return(status);
}
/******************************************************************************
*
*******************************************************************************/
void JLP_GsegData::ReadExtraEllipsesFromFile(char *param_filename)
{
/* Declare variables */
int nval1, nval2, iell, status;
UINT32 line_color0, style_color0;
double x0, y0, width, height, angle;
char line_char0, color_char0, line0[256];
const char *error_str[] =
   { "Invalid or missing ellipse coordinates.",
     "Invalid ellipse color character.",
     "Invalid or missing ellipse line or color specification.",
     "Invalid ellipse line character." };
FILE *fptr;

n_extra_ellipses1 = 0;

/* JLP 2016 to solve problem of numeric dot vs numeric comma */
/* SHOULD BE CALLED HERE...*/
/* In the "C" locale, a decimal_point is "." */
if (NULL == setlocale(LC_NUMERIC, "C")) {
  fprintf(stderr, " Error setting numeric dot in setlocale !\n");
  exit(-1);
}

/* Return if no ellipses to plot */
fptr = fopen(param_filename, "r");
if(fptr == NULL) {
 fprintf(stderr, "PlotEllipses/Error opening param_file=%s\n", 
                  param_filename);
 return;
 }
iell = 0;
while ( fgets(line0, 256, fptr) != NULL )
   if ( strncmp(line0, "ellipse_coords", 14) == 0 )
      iell++;
fclose(fptr);
if ( iell == 0 ) return;

/* Load plot data of ellipses */
iell = 0;
  fptr = fopen(param_filename, "r");
  while ( fgets(line0, 256, fptr) != NULL )
     {
// ellipse_coords detected
     if ( strncmp(line0, "ellipse_coords", 14) == 0 )
        {
        if ( sscanf(&line0[14], "%lf %lf %lf %lf %lf",
                    &x0, &y0, &width, &height, &angle) != 5 )
           {
           JLP_ErrorDialog(error_str[0]);
           exit(1);
           }

// Default values:
        extra_ellipses1[iell].line_width = 1;
        extra_ellipses1[iell].line_stylechar = 'l';
        extra_ellipses1[iell].line_color = 0;

// Read next line:
        if ( fgets(line0, 256, fptr) != NULL ) {
// Detection of "ellipse_style"
           if ( strncmp(line0, "ellipse_style", 13) == 0 )
              {
             nval1 = sscanf(&line0[13], " %c 0x%x", &line_char0, &line_color0);
            if ( nval1 == 2 ) {
               extra_ellipses1[iell].line_stylechar = line_char0;
               extra_ellipses1[iell].line_color = line_color0;
               }
            nval2 = sscanf(&line0[13], " %c 0X%x", &line_char0, &line_color0);
            if ( nval2 == 2 ) {
               extra_ellipses1[iell].line_stylechar = line_char0;
               extra_ellipses1[iell].line_color = line_color0;
               } 
            if ( nval1 == 2 || nval2 == 2) {
               extra_ellipses1[iell].line_stylechar = line_char0;
               extra_ellipses1[iell].line_color = line_color0;
               sscanf(&line0[13], " %*c %*s %u", &(extra_ellipses1[iell].line_width));
             }
              else if(sscanf(&line0[13], " %c %c",
                         &(extra_ellipses1[iell].line_stylechar), &color_char0) == 2)
                {
// color_char0 compared to  color_string = "kaswrylqbfmogtnpx":
                status = GetStyleColorFromStyleChar(color_char0, &style_color0);
                if ( status != 0 )
                  {
                  JLP_ErrorDialog(error_str[1]);
                  exit(1);
                  }
                 else
                  {
                  extra_ellipses1[iell].line_color = style_color0;
                  sscanf(&line0[13], " %*c %*c %u", 
                         &(extra_ellipses1[iell].line_width));
                  }
                 }

              else
                 {
                 JLP_ErrorDialog(error_str[2]);
                 exit(1);
                 }

/* Check line character */
              if ( extra_ellipses1[iell].line_stylechar != 'l' 
                   && extra_ellipses1[iell].line_stylechar != 'd' 
                   && extra_ellipses1[iell].line_stylechar != '.' )
                 {
                 JLP_ErrorDialog(error_str[3]);
                 exit(1);
                 }
               } // EOF detection of "ellipse_style" 
              } // EOF read next line
          iell++;
          } // EOF detection of "ellipse_coords"
     else if ( strncmp(line0, "#####", 5) == 0 )
        break;
   } // EOF while

fclose(fptr);

n_extra_ellipses1 = iell;
return;
}
