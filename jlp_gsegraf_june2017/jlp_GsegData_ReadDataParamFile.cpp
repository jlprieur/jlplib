/*******************************************************************************
* jlp_GsegData_ReadDataParamFile.cpp
* JLP_GsegData class
*
* JLP
* Version 19/04/2017
*******************************************************************************/
#include <stdlib.h>         // exit()
#include <ctype.h>          // isdigit()
#include <math.h>
#include "jlp_gseg_data.h"

/***************************************************************************
* Read the file parameters from the parameter file 
*
* OUTPUT:
*  nplots0 : number of files that have been successfully checked for opening 
***************************************************************************/
int JLP_GsegData::ReadFileParamsFromFile(char *param_filename, int *nplots0)
{
int i, iplot, iformat, nformat, nfilenames_total, nformats_total;
int index_filenames, index_formats;
unsigned int i1_str, i2_str, size;
char line1[256], *string;
const char *error_str[] =
      { "Missing file_name data.",
        "Missing file_format data.",
        "Invalid or missing plot_type parameter." };
FILE *fptr;

*nplots0 = 0;

/* Open plot-parameter file */
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

/* Get number of file_name entries */
   iplot = 0;
   while ( fgets(line1, 256, fptr) != NULL )
      {
      if ( strncmp(line1, "file_name", 9) == 0 )
         iplot++;
      else if ( strncmp(line1, "#####", 5) == 0 )
         break;
      }
// Save this value to private variable:
   nplots1 = iplot;
   if ( nplots1 > 0 )
      nfilenames = new int[nplots1];


   /* Get number of file_format entries */
   nformat = 0;
   if ( nplots1 > 0 )
      {
      iformat = 0;
      rewind(fptr);
      while ( fgets(line1, 256, fptr) != NULL )
         {
         if ( strncmp(line1, "file_format", 11) == 0 )
            iformat++;
         else if ( strncmp(line1, "#####", 5) == 0 )
            break;
         }
      nformat = iformat;
      if ( nformat > 0 )
         nformats = new int[nformat];
      }

   /* Get filename sizes */
   nfilenames_total = 0;
   iplot = 0;
   rewind(fptr);
   if ( nplots1 > 0 )
      {
      while ( fgets(line1, 256, fptr) != NULL )
         {
         if ( strncmp(line1, "file_name", 9) == 0 )
            {
            iplot++;
            size = strlen(&line1[9]);
            if ( (string = gseg_get_string(string_get, &line1[9], &i1_str, &i2_str, &size, 0)) != NULL )
               nfilenames[iplot-1] = size + 1;
            else
               {
               JLP_ErrorDialog(error_str[0]);
               return(-1);
               }

            nfilenames_total = nfilenames_total + nfilenames[iplot-1];
            }

         else if ( strncmp(line1, "#####", 5) == 0 )
            break;
         }
      }

   /* Get format sizes */
   nformats_total = 0;
   iformat = 0;
   rewind(fptr);
   if ( nformat > 0 )
      {
      while ( fgets(line1, 256, fptr) != NULL )
         {
         if ( strncmp(line1, "file_format", 11) == 0 )
            {
            iformat++;
            size = strlen(&line1[11]);
            if ( (string = gseg_get_string(string_get, &line1[11], &i1_str, &i2_str, &size, 0)) != NULL )
               nformats[iformat-1] = size + 1;
            else
               {
               JLP_ErrorDialog(error_str[1]);
               return(-1);
               }

            nformats_total = nformats_total + nformats[iformat-1];
            }

         else if ( strncmp(line1, "#####", 5) == 0 )
            break;
         }
      }


/* Allocate memory for plot parameters */
   filenames           = new char[nfilenames_total];
   formats             = new char[nformats_total];
   formats_mod         = new char[nformats_total];
   plot_types          = new char[10*nplots1];
   styleflags          = new int[nplots1];
   stylechar1          = new char[nplots1];
   stylechar2          = new char[nplots1];
   stylesizes          = new UINT32[nplots1];
   stylecolor1         = new UINT32[nplots1];
   stylecolor2         = new UINT32[nplots1];

   memset(filenames,   0, nfilenames_total*sizeof(char));
   memset(formats,     0, nformats_total*sizeof(char));
   memset(formats_mod, 0, nformats_total*sizeof(char));
   memset(plot_types,  0, 10*nplots1*sizeof(char));
   memset(stylechar1,  0, nplots1*sizeof(char));
   memset(stylechar2,  0, nplots1*sizeof(char));

  /* Set default style parameters */
   for ( i = 1; i <= nplots1; i++ )
      {
      styleflags[i-1] = -1;
      stylechar1[i-1] = '*';
      stylechar2[i-1] = '*';
      stylesizes[i-1] = 1;
      }

  /* Read filenames */
   iplot = 0;
   index_filenames = 0;
   rewind(fptr);
   if ( nplots1 > 0 )
      {
      while ( fgets(line1, 256, fptr) != NULL )
         {
         if ( strncmp(line1, "file_name", 9) == 0 )
            {
            iplot++;
            size = strlen(&line1[9]);
            if ( (string = gseg_get_string(string_get, &line1[9], &i1_str, &i2_str, &size, 0)) != NULL )
               {
               strcpy(&filenames[index_filenames], string);
               index_filenames = index_filenames + nfilenames[iplot-1];
               }
            }

         else if ( strncmp(line1, "#####", 5) == 0 )
            break;
         }
      }

   /* Read file formats */
   iformat = 0;
   index_formats = 0;
   rewind(fptr);
   if ( nformat > 0 )
      {
      while ( fgets(line1, 256, fptr) != NULL )
         {
         if ( strncmp(line1, "file_format", 11) == 0 )
            {
            iformat++;
            size = strlen(&line1[11]);
            if ( (string = gseg_get_string(string_get, &line1[11], &i1_str, &i2_str, &size, 0)) != NULL )
               {
               strcpy(&formats[index_formats], string);
               index_formats = index_formats + nformats[iformat-1];
               }
            }

         else if ( strncmp(line1, "#####", 5) == 0 )
            break;
         }
      }

// Save to output variable:
*nplots0 = nplots1;

return(0);
}
/***************************************************************************
* Read the plot types from the parameter file 
***************************************************************************/
int JLP_GsegData::ReadPlotTypesFromFile(char *param_filename)
{
int iplot, index_plot_types;
unsigned int i1_str, i2_str, size;
char line1[256], *string, *pchar, buffer[64];
FILE *fptr;
const char *error_str[] = { "Invalid plot type." };

/* Open plot-parameter file */
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

/* Read plot types */
   index_plot_types = 0;
   if ( nplots1 > 0 )
      {
      while ( fgets(line1, 256, fptr) != NULL )
         {
         if ( strncmp(line1, "plot_type", 9) == 0 )
            {
            size = strlen(&line1[9]);
            if ( (string = gseg_get_string(string_get, &line1[9], 
                                          &i1_str, &i2_str, &size, 0)) != NULL )
               {
               strncpy(&plot_types[index_plot_types], string, 9);
               index_plot_types = index_plot_types + 10;
               }
            }

         else if ( strncmp(line1, "#####", 5) == 0 )
            break;
         }
      }

   index_plot_types = 0;
   for ( iplot=1; iplot<=nplots1; iplot++ )
      {
      if ( strcmp(&plot_types[index_plot_types], "points")    != 0 &&
           strcmp(&plot_types[index_plot_types], "histogram") != 0 &&
           strcmp(&plot_types[index_plot_types], "color")     != 0 &&
           strcmp(&plot_types[index_plot_types], "contour")   != 0 &&
           strcmp(&plot_types[index_plot_types], "mesh")      != 0 )
         {
         JLP_ErrorDialog(error_str[0]);
         return(-1);
         }

      /* Increment index */
      index_plot_types = index_plot_types + 10;
      }

return(0);
}
/***************************************************************************
* Read the plot styles from the parameter file 
***************************************************************************/
int JLP_GsegData::ReadPlotStylesFromFile(char *param_filename, 
                                         char *axis_type0)
{
int iplot, index_plot_types, stylesize0;
double zblack_0, zwhite_0;
unsigned int i1_str, i2_str, size;
char line1[256], *string, *pchar, buffer[64];
UINT32 stylecolor1_0, stylecolor2_0, alphacolor_0;
FILE *fptr;

/* Open plot-parameter file */
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
#ifdef DEBUG
printf("ReadPlotStylesFromFile/ reading %s : axis_type0=%s\n", 
         param_filename, axis_type0);
#endif

/* Read plot styles */
   iplot = 0;
   index_plot_types = 0;
   if ( nplots1 > 0 )
      {
      while ( fgets(line1, 256, fptr) != NULL )
         {
         if ( strncmp(line1, "plot_style", 10) == 0 )
            {
            iplot++;
#ifdef DEBUG
printf("ReadPlotStylesFromFile/plot_style line: \n>%s<\n iplot=%d, plot_type=%s\n", 
        line1, iplot, &plot_types[index_plot_types]);
#endif

            if ( strcmp(&plot_types[index_plot_types], "points")    == 0 ||
                 strcmp(&plot_types[index_plot_types], "histogram") == 0 )
               {
               if ( (sscanf(&line1[10], " %c 0x%x", &stylechar1[iplot-1], 
                            (unsigned int *) &stylecolor2[iplot-1]) == 2 ||
                     sscanf(&line1[10], " %c 0X%x", &stylechar1[iplot-1], 
                            (unsigned int *) &stylecolor2[iplot-1]) == 2) &&
                    isdigit(stylechar1[iplot-1]) == 0 )
                  {
/****************************************************************************
* 2d points and 3d points plot types
* Read symbol character, hexadecimal color, and symbol size
*
* 2d histogram
* Read symbol character and hexadecimal color
****************************************************************************/
                  styleflags[iplot-1] = 4;
// Set outline_colors_rgba et fill_colors_rgba
                  jlp_gsegraf1->SetColorsFromStyleColor(stylecolor2[iplot - 1], 
                                              stylechar1[iplot - 1], iplot -1);

/* Set default symbol size */
                  strcpy(buffer, "cCtTsSiIpPhH+xra");
                  if ( (pchar = strchr(buffer, stylechar1[iplot-1])) != NULL )
                     stylesizes[iplot-1] = 6;
/* Get symbol size if present */
                  if ( sscanf(&line1[10], " %*c %*s %d", &stylesize0) == 1 )
                     stylesizes[iplot-1] = abs(stylesize0);
                  }

               else if(sscanf(&line1[10], " %c %c", &stylechar1[iplot-1], 
                             &stylechar2[iplot-1]) == 2 &&
                    (jlp_gsegraf1->SetColorsFromStyleChars(&stylecolor2_0, 
                      stylechar1[iplot-1], stylechar2[iplot-1], iplot -1)) == 0)
                  {
/****************************************************************************
* 2d points and 3d points plot types
* Read symbol character, color character, and symbol size
*
* 2d histogram
* Read symbol character and color character
****************************************************************************/
                  styleflags[iplot-1] = 2;
/* Set specified color */
                  stylecolor2[iplot-1] = stylecolor2_0;

/* Set default symbol size */
                  strcpy(buffer, "cCtTsSiIpPhH+xra");
                  if ( (pchar = strchr(buffer, stylechar1[iplot-1])) != NULL )
                     stylesizes[iplot-1] = 6;
/* Get symbol size if present */
                  if ( sscanf(&line1[10], " %*c %*c %d", &stylesize0) == 1 )
                     stylesizes[iplot-1] = abs(stylesize0);
                  }
               }

            else if ( strcmp(&plot_types[index_plot_types], "color") == 0 &&
                      strcmp(axis_type0, "linear") == 0 )
               {
               size = strlen(&line1[10]);
               if ( (string = gseg_get_string(string_get, &line1[10], 
                                              &i1_str, &i2_str, &size, 0)) != NULL )
                  {
/************************************************************
* 2d color plot type
* Read "nearest" or "bilinear" string and two decimal numbers
************************************************************/
                  if ( strcmp(string, "nearest") == 0 )
                     {
                     styleflags[iplot-1] = 9;
                     sscanf(&string[size+1], " %lf %lf", 
                            &zblack_0, &zwhite_0);
                     jlp_gsegraf1->SetZBlackWhite(zblack_0, zwhite_0, 
                                                  iplot - 1);
                     }
                  else if ( strcmp(string, "bilinear") == 0 )
                     {
                     styleflags[iplot-1] = 8;
                     sscanf(&string[size+1], " %lf %lf", 
                            &zblack_0, &zwhite_0);
                     jlp_gsegraf1->SetZBlackWhite(zblack_0, zwhite_0, 
                                                  iplot - 1);
                     }
                  }
               }

            else if ( strcmp(&plot_types[index_plot_types], "contour") == 0 &&
                      strcmp(axis_type0, "linear") == 0 )
               {
               size = strlen(&line1[10]);
               if ( (string = gseg_get_string(string_get, &line1[10], &i1_str, 
                                              &i2_str, &size, 0)) != NULL )
                  {
/************************************************************
*
* 2d contour plot type
* Read "auto" string and line width
*
************************************************************/
                  if ( strcmp(string, "auto") == 0 )
                     {
                     styleflags[iplot-1] = 7;
/* Get line width */
                     if ( sscanf(&string[size+1], " %d", &stylesize0) == 1 )
                        stylesizes[iplot-1] = abs(stylesize0);
                     }
                  }

               else if ( sscanf(&line1[10], " 0x%x", 
                          (unsigned int *) &stylecolor1[iplot-1]) == 1 ||
                         sscanf(&line1[10], " 0X%x", 
                          (unsigned int *) &stylecolor1[iplot-1]) == 1 )
                  {
/************************************************************
*
* 2d contour plot type
* Read hexadecimal color and line width
*
************************************************************/
                  styleflags[iplot-1] = 3;
/* get line width */
                  if ( sscanf(&line1[10], " %*s %d", &stylesize0) == 1 )
                     stylesizes[iplot-1] = abs(stylesize0);
                  }

               else if ( sscanf(&line1[10], " %c", &stylechar1[iplot-1]) 
                         == 1 &&
                    (jlp_gsegraf1->SetStyleColorFromStyleChar(&stylecolor1_0, 
                                        stylechar1[iplot-1]) == 0))
                  {
/************************************************************
*
* 2d contour plot type
* Read color character and line width
*
************************************************************/
                  styleflags[iplot-1] = 1;
/* Get specified color */
                  stylecolor1[iplot-1] = stylecolor1_0;
/* Get line width */
                  if ( sscanf(&line1[10], " %*c %d", &stylesize0) == 1 )
                     stylesizes[iplot-1] = abs(stylesize0);
                  }
               }

            else if ( (strcmp(&plot_types[index_plot_types], "contour") == 0 ||
                       strcmp(&plot_types[index_plot_types], "color")   == 0 ||
                       strcmp(&plot_types[index_plot_types], "mesh")    == 0) &&
                      strcmp(axis_type0, "3d") == 0 )
               {
               size = strlen(&line1[10]);
               if ( (strcmp(&plot_types[index_plot_types], "color") == 0 ||
                     strcmp(&plot_types[index_plot_types], "mesh")  == 0) &&
                    (string = gseg_get_string(string_get, &line1[10],
                                          &i1_str, &i2_str, &size, 0)) != NULL )
                  {
/************************************************************
*
* 3d color and 3d mesh plot types
* Read "auto" string and color alpha value
*
************************************************************/
                  if ( strcmp(string, "auto") == 0 )
                     {
                     styleflags[iplot-1] = 7;
                     if ( sscanf(&string[size+1], " 0x%x", 
                                 (unsigned int *) &alphacolor_0) == 1 ||
                          sscanf(&string[size+1], " 0X%x", 
                                 (unsigned int *) &alphacolor_0) == 1 )
                       {
                       jlp_gsegraf1->SetAlphaColor(alphacolor_0, iplot - 1);
                       }
                     }
                  }
               else if ( sscanf(&line1[10], " 0x%x 0x%x", 
                                (unsigned int *) &stylecolor1[iplot-1], 
                                (unsigned int *) &stylecolor2[iplot-1]) == 2 ||
                         sscanf(&line1[10], " 0X%x 0X%x", 
                                (unsigned int *) &stylecolor1[iplot-1], 
                                (unsigned int *) &stylecolor2[iplot-1]) == 2 ||
                         sscanf(&line1[10], " 0x%x 0X%x", 
                                (unsigned int *) &stylecolor1[iplot-1], 
                                (unsigned int *) &stylecolor2[iplot-1]) == 2 ||
                         sscanf(&line1[10], " 0X%x 0x%x", 
                                (unsigned int *) &stylecolor1[iplot-1], 
                                (unsigned int *) &stylecolor2[iplot-1]) == 2 )
                  {
/************************************************************
*
* 3d contour and 3d mesh plot types
* Read two hexadecimal colors
*
************************************************************/
                  styleflags[iplot-1] = 6;
                  }

               else if ( sscanf(&line1[10], " 0x%x %c", 
                                (unsigned int *) &stylecolor1[iplot-1], 
                                &stylechar2[iplot-1]) == 2 ||
                         sscanf(&line1[10], " 0X%x %c", 
                                (unsigned int *) &stylecolor1[iplot-1], 
                                 &stylechar2[iplot-1]) == 2 )
                  {
/************************************************************
*
* 3d contour and 3d mesh plot types
* Read hexadecimal color and color character
*
************************************************************/
                  styleflags[iplot-1] = 5;
                  }

               else if ( sscanf(&line1[10], " %c 0x%x", &stylechar1[iplot-1], 
                                (unsigned int *) &stylecolor2[iplot-1]) == 2 ||
                         sscanf(&line1[10], " %c 0X%x", &stylechar1[iplot-1], 
                                (unsigned int *) &stylecolor2[iplot-1]) == 2 )
                  {
/************************************************************
*
* 3d contour and 3d mesh plot types
* Read color character and hexadecimal color
*
************************************************************/
                  styleflags[iplot-1] = 4;
                  }

               else if ( sscanf(&line1[10], " %c %c", &stylechar1[iplot-1], 
                                &stylechar2[iplot-1]) == 2 )
                  {
/************************************************************
*
* 3d contour and 3d mesh plot types
* Read two color characters
*
************************************************************/
                  styleflags[iplot-1] = 2;
                  }
               }

            /* Increment index */
            index_plot_types = index_plot_types + 10;
#ifdef DEBUG
printf("DEBUG/ styleflags[iplot=%d]=%d stylechar1=>%c< stylecolor2=>%u<\n", 
            iplot, styleflags[iplot-1], stylechar1[iplot-1], 
            stylecolor2[iplot-1]);
#endif
            }

         else if ( strncmp(line1, "#####", 5) == 0 )
            break;
         }
      }

return(0);
}
