/*******************************************************************************
* jlp_GsegAxes_ReadPlotParamFile.cpp
* JLP_GsegAxes class
*
* JLP
* Version 01/04/2017
*******************************************************************************/
#include <stdlib.h>          // exit()
#include <math.h>
#include "jlp_gseg_axes.h"

/***************************************************************************
* Read axis type from the parameter file 
*
***************************************************************************/
int JLP_GsegAxes::ReadAxisType(char *param_filename)
{
unsigned int i1_str, i2_str, size;
char line1[256], *string, error_str0[80];
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

   while ( fgets(line1, 256, fptr) != NULL )
      {
      if ( strncmp(line1, "axis_type", 9) == 0 )
         {
         size = strlen(&line1[9]);
         string = new char[9];
         if((string = gseg_get_string(string_get, &line1[9], &i1_str, &i2_str, 
               &size, 0)) != NULL )
            strncpy(&p_plot_param->axis_type[0], string, 8);
         }

      else if ( strncmp(line1, "#####", 5) == 0 )
         break;
      }
   fclose(fptr);

   if ( strcmp(p_plot_param->axis_type, "linear")   != 0 &&
        strcmp(p_plot_param->axis_type, "semilogx") != 0 &&
        strcmp(p_plot_param->axis_type, "semilogy") != 0 &&
        strcmp(p_plot_param->axis_type, "loglog")   != 0 &&
        strcmp(p_plot_param->axis_type, "polar")    != 0 &&
        strcmp(p_plot_param->axis_type, "3d")       != 0 )
      {
      strcpy(error_str0, "Invalid or missing axis_type parameter.");
      JLP_ErrorDialog(error_str0);
      return(-1);
      }

// Set flags:
SetAxisTypeFlags(p_plot_param->axis_type);

return(0);
}
/***************************************************************************
* Read background color/image style parameters from file 
*
***************************************************************************/
int JLP_GsegAxes::ReadBackgroundStyle(char *param_filename)
{
int index;
unsigned int i1_str, i2_str, size;
char line1[256], *string;
UINT32 canvas_bg_color, canvas_fg_color;
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

 while ( fgets(line1, 256, fptr) != NULL )
    {
    if ( strncmp(line1, "background_color", 16) == 0 )
       {
       size = strlen(&line1[16]);
       if ( (string = gseg_get_string(string_get, &line1[16], &i1_str, &i2_str, 
             &size, 0)) != NULL )
          if ( strcmp(string, "black") == 0 )
             {
             canvas_fg_color = 0xFFFFFFFF;   /* white */
             canvas_bg_color = 0x000000FF;   /* black */
             }
          else 
             {
             canvas_fg_color = 0x000000FF;   /* black */
             canvas_bg_color = 0xFFFFFFFF;   /* white */
             }
         jlp_gsegraf1->SetCanvasColor(canvas_fg_color, canvas_bg_color);
       }

    else if ( strncmp(line1, "background_image", 16) == 0 )
       {
       size = strlen(&line1[16]);

         if ( (string = gseg_get_string(string_get, &line1[16], &i1_str, &i2_str, &size, 0)) != NULL )
            {
            background_image_file = new char[size + 1];
            strcpy(background_image_file, string);
            }
         index = 16 + i2_str + 2;
         size = strlen(&line1[index]);
         if ( (string = gseg_get_string(string_get, &line1[index], &i1_str, &i2_str, &size, 0)) != NULL )
            {
            if ( strcmp(string, "center") == 0 )
               background_image_style = 1;
            else if ( strcmp(string, "fill") == 0 )
               background_image_style = 2;
            else if ( strcmp(string, "scale") == 0 )
               background_image_style = 3;
            else if ( strcmp(string, "zoom") == 0 )
               background_image_style = 4;
            }
         }
      else if ( strncmp(line1, "#####", 5) == 0 )
         break;
      }

fclose(fptr);

return(0);
}
/***************************************************************************
* Read axis limits and various parameters from the parameter file
*
***************************************************************************/
int JLP_GsegAxes::ReadAxisLimits(char *param_filename)
{
int i, j, index; 
unsigned int i1_str, i2_str, size;
double ww;
char line1[256], *string, error_str0[80];
char char0, *pchar;
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

   while ( fgets(line1, 256, fptr) != NULL )
      {
      if ( strncmp(line1, "axis_scale", 10) == 0 )
         {
         size = strlen(&line1[10]);
         if ( (string = gseg_get_string(string_get, &line1[10], &i1_str, 
               &i2_str, &size, 0)) != NULL )
            strncpy(&p_plot_param->axis_scale[0], string, 5);
         }

      else if ( strncmp(line1, "axis_limits", 11) == 0 )
         {
         size = strlen(line1);
         i = 11;
         for ( j=1; j<=6; j++ )
            {
            while ( line1[i] == ' ' || line1[i] == '\t' ) i++;

            if ( sscanf(&line1[i], "%lf", 
                        &p_plot_param->axis_limits[j-1]) == 1 )
               {
               set_axis_limits1[j-1] = 1;
               }
            else if ( sscanf(&line1[i], " %c", &char0 ) == 1 )
               {
               if ( char0 == '#' )
                  break;
               else if ( char0 != '*' )
                  {
                  strcpy(error_str0, "Invalid axis_limits parameter.");
                  JLP_ErrorDialog(error_str0);
                  return(-1);
                  }
               }
            if ( (pchar = strchr(&line1[i], ' ')) == NULL )
               break;

            i = i + pchar - &line1[i];
            }
         }
      else if ( strncmp(line1, "view3d", 6) == 0 )
         {
         if ( sscanf(&line1[6], "%lf %lf", &p_plot_param_3d->phi, &p_plot_param_3d->theta) != 2 )
            {
            strcpy(error_str0, "Invalid view direction angles.");
            JLP_ErrorDialog(error_str0);
            return(-1);
            }
         }

      else if ( strncmp(line1, "minor_ticks", 11) == 0 )
         {
         size = strlen(&line1[11]);
         if ( (string = gseg_get_string(string_get, &line1[11], &i1_str, &i2_str, &size, 0)) != NULL )
            {
            strncpy(&p_plot_param->minor_ticks[0], string, 3);
            if ( strcmp(string, "on") == 0 )
               minor_ticks_flag = 1;
            }
         }

      else if ( strncmp(line1, "grid", 4) == 0 )
         {
         size = strlen(&line1[4]);
         if ( (string = gseg_get_string(string_get, &line1[4], &i1_str, &i2_str, &size, 0)) != NULL &&
              strcmp(string, "off") == 0 )
            {
            strncpy(&p_plot_param->grid[0], string, 3);
            }
         else if((sscanf(&line1[4], " %c 0x%x", &gridchar1, &gridcolor1)
                  == 2) ||
                 (sscanf(&line1[4], " %c 0X%x", &gridchar1, &gridcolor1)
                  == 2 ))
            {
            strcpy(&p_plot_param->grid[0], "on2");
            LoadGridParam(gridchar1, gridchar2, gridcolor1);
            }
// Decodes gridchar1/2 using color_string (i.e., "kaswrylqbfmogtnpx")
// strchr(str0, char0) locates first occurrence of character char0 
// in string str0
         else if ((sscanf(&line1[4], " %c %c", &gridchar1, &gridchar2)
                   == 2) &&
                   ((pchar = strchr(color_string, gridchar2)) != NULL ))
            {
            index = pchar - &color_string[0];   /* get index to color character */
            gridcolor1 = color_rgba[index];      /* set specified color */
            strcpy(&p_plot_param->grid[0], "on1");
            LoadGridParam(gridchar1, gridchar2, gridcolor1);
            }
         else
            {
            strcpy(error_str0, "Invalid grid parameter.");
            JLP_ErrorDialog(error_str0);
            return(-1);
            }
         }

      else if ( strncmp(line1, "xlabel", 6) == 0 )
         {
         size = strlen(&line1[6]);
         if ( (string = gseg_get_string(string_get, &line1[6], &i1_str, &i2_str, &size, 1)) != NULL )
            {
            xlabel1 = new char[size + 1];
            strcpy(xlabel1, string);
            }
         }

      else if ( strncmp(line1, "ylabel", 6) == 0 )
         {
         size = strlen(&line1[6]);
         if ( (string = gseg_get_string(string_get, &line1[6], &i1_str, &i2_str, &size, 1)) != NULL )
            {
            ylabel1 = new char[size + 1];
            strcpy(ylabel1, string);
            }
         }

      else if ( strncmp(line1, "zlabel", 6) == 0 )
         {
         size = strlen(&line1[6]);
         if ( (string = gseg_get_string(string_get, &line1[6], &i1_str, &i2_str, &size, 1)) != NULL )
            {
            zlabel1 = new char[size + 1];
            strcpy(zlabel1, string);
            }
         }

      else if ( strncmp(line1, "title", 5) == 0 )
         {
         size = strlen(&line1[5]);
         if ( (string = gseg_get_string(string_get, &line1[5], &i1_str, &i2_str, &size, 1)) != NULL )
            {
            title1 = new char[size + 1];
            strcpy(title1, string);
            }
         }

      else if ( strncmp(line1, "date_time", 9) == 0 )
         {
         size = strlen(&line1[9]);
         if ( (string = gseg_get_string(string_get, &line1[9], &i1_str, &i2_str, &size, 0)) != NULL )
            strncpy(&p_plot_param->date_time_anchor[0], string, 9);
         }

      else if ( strncmp(line1, "plot_box", 8) == 0 )
         {
         size = strlen(&line1[8]);
         if ( (string = gseg_get_string(string_get, &line1[8], &i1_str, &i2_str, 
               &size, 0)) != NULL )
            strncpy(&p_plot_param->plot_box[0], string, 3);
         }

      else if ( strncmp(line1, "x_tick_marks", 12) == 0 )
         {
         size = strlen(&line1[12]);
         if ( (string = gseg_get_string(string_get, &line1[12], &i1_str, 
               &i2_str, &size, 0)) != NULL )
            strncpy(&p_plot_param->x_tick_marks[0], string, 3);
         }

      else if ( strncmp(line1, "y_tick_marks", 12) == 0 )
         {
         size = strlen(&line1[12]);
         if ( (string = gseg_get_string(string_get, &line1[12], &i1_str, &i2_str, 
               &size, 0)) != NULL )
            strncpy(&p_plot_param->y_tick_marks[0], string, 3);
         }

      else if ( strncmp(line1, "z_tick_marks", 12) == 0 )
         {
         size = strlen(&line1[12]);
         if ( (string = gseg_get_string(string_get, &line1[12], &i1_str, &i2_str, 
               &size, 0)) != NULL )
            strncpy(&p_plot_param->z_tick_marks[0], string, 3);
         }

      else if ( strncmp(line1, "x_tick_labels", 13) == 0 )
         {
         size = strlen(&line1[13]);
         if ( (string = gseg_get_string(string_get, &line1[13], &i1_str, &i2_str, 
               &size, 0)) != NULL )
            strncpy(&p_plot_param->x_tick_labels[0], string, 3);
         }

      else if ( strncmp(line1, "y_tick_labels", 13) == 0 )
         {
         size = strlen(&line1[13]);
         if ( (string = gseg_get_string(string_get, &line1[13], &i1_str, &i2_str, 
               &size, 0)) != NULL )
            strncpy(&p_plot_param->y_tick_labels[0], string, 3);
         }

      else if ( strncmp(line1, "z_tick_labels", 13) == 0 )
         {
         size = strlen(&line1[13]);
         if ( (string = gseg_get_string(string_get, &line1[13], &i1_str, &i2_str, 
               &size, 0)) != NULL )
            strncpy(&p_plot_param->z_tick_labels[0], string, 3);
         }
  } // EOF while()

fclose(fptr);

/* Modify data for logarithmic axes */
   if ( strcmp(p_plot_param->axis_type, "semilogx") == 0 )
      {
      for ( i=1; i<=2; i++ )
         if ( p_plot_param->axis_limits[i-1] == 1 )
            {
            if ( p_plot_param->axis_limits[i-1] <= 0.0 )
               {
               strcpy(error_str0, "Invalid x-axis limit.");
               JLP_ErrorDialog(error_str0);
               return(-1);
               }
            p_plot_param->axis_limits[i-1] =
                    log10(fabs(p_plot_param->axis_limits[i-1]));
            }
      }

   else if ( strcmp(p_plot_param->axis_type, "semilogy") == 0 )
      {
      for ( i=3; i<=4; i++ )
         if ( p_plot_param->axis_limits[i-1] == 1 )
            {
            if ( p_plot_param->axis_limits[i-1] <= 0.0 )
               {
               strcpy(error_str0, "Invalid y-axis limit.");
               JLP_ErrorDialog(error_str0);
               return(-1);
               }
            p_plot_param->axis_limits[i-1] = 
                     log10(fabs(p_plot_param->axis_limits[i-1]));
            }
      }

   else if ( strcmp(p_plot_param->axis_type, "loglog") == 0 )
      {
      for ( i=1; i<=4; i++ )
         if ( p_plot_param->axis_limits[i-1] == 1 )
            {
            if ( p_plot_param->axis_limits[i-1] <= 0.0 )
               {
               strcpy(error_str0, "Invalid axis limit.");
               JLP_ErrorDialog(error_str0);
               return(-1);
               }
            p_plot_param->axis_limits[i-1] = 
                       log10(fabs(p_plot_param->axis_limits[i-1]));
            }
      }

return(0);
}
/***************************************************************************
* Read font parameters from the parameter file
*
***************************************************************************/
int JLP_GsegAxes::ReadFonts(char *param_filename)
{
/* Declare variables */
int status; 
double font_size_title0, font_size_axis_labels0, font_size_tick_labels0;
double font_size_text0, font_size_legend0, font_size_date_time0;
unsigned int i1_str, i2_str, size;
char *string, font_name0[64], line1[256];
const char *error_str[] =
      { "Invalid or missing date-time font size.",
        "Invalid or missing legend font size.",
        "Invalid or missing text font size.",
        "Invalid or missing tick-label font size.",
        "Invalid or missing axis-label font size.",
        "Invalid or missing title font size." };
FILE *fptr;

// Initialize font sizes for JLP_GsegAxes:
  font_size_title0 = 12.;
  font_size_axis_labels0 = 12.;
  font_size_tick_labels0 = 12.;

/* Open plot-parameter file 
(and leave it open in this routine until returning) 
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

   while ( fgets(line1, 256, fptr) != NULL ) {

     if ( strncmp(line1, "font_name", 9) == 0 )
         {
         size = strlen(&line1[9]);
         if ( (string = gseg_get_string(string_get, &line1[9], &i1_str, &i2_str, 
               &size, 0)) != NULL )
            {
            strcpy(font_name0, string);
            }
         }

      else if ( strncmp(line1, "font_size_date_time", 19) == 0 )
         {
         if ( sscanf(&line1[19], "%lf", &font_size_date_time0) != 1 )
            {
            JLP_ErrorDialog(error_str[0]);
            return(-1);
            }
         }

      else if ( strncmp(line1, "font_size_legend", 16) == 0 )
         {
         if ( sscanf(&line1[16], "%lf", &font_size_legend0) != 1 )
            {
            JLP_ErrorDialog(error_str[1]);
            return(-1);
            }
         }

      else if ( strncmp(line1, "font_size_text", 14) == 0 )
         {
         if ( sscanf(&line1[14], "%lf", &font_size_text0) != 1 )
            {
            JLP_ErrorDialog(error_str[2]);
            return(-1);
            }
         }

      else if ( strncmp(line1, "font_size_tick_labels", 21) == 0 )
         {
         if ( sscanf(&line1[21], "%lf", &font_size_tick_labels0) != 1 )
            {
            JLP_ErrorDialog(error_str[3]);
            return(-1);
            }
         }

      else if ( strncmp(line1, "font_size_axis_labels", 21) == 0 )
         {
         if ( sscanf(&line1[21], "%lf", &font_size_axis_labels0) != 1 )
            {
            JLP_ErrorDialog(error_str[4]);
            return(-1);
            }
         }

      else if ( strncmp(line1, "font_size_title", 15) == 0 )
         {
         if ( sscanf(&line1[15], "%lf", &font_size_title0) != 1 )
            {
            JLP_ErrorDialog(error_str[5]);
            return(-1);
            }
         }

      else if ( strncmp(line1, "#####", 5) == 0 )
         break;

   }  // EOF while()

   fclose(fptr);

// Load info to JLP_GsegAxes:
 SetFontParams(font_name0, font_size_title0, font_size_axis_labels0, 
               font_size_tick_labels0, font_size_text0, font_size_legend0,
               font_size_date_time0);

return(0);
}
