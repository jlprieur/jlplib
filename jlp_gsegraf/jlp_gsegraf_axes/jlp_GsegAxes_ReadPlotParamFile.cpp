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
int JLP_GsegAxes::ReadAxisTypeFromFile(char *param_filename)
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
* Set axis type, etc
*
***************************************************************************/
int JLP_GsegAxes::SetAxisParamsFromGsegAxisData(GSEG_AXIS_DATA *gseg_axdata0)
{
char error_str0[80], font_name0[64];
double font_size_title0, font_size_axis_labels0, font_size_tick_labels0;
double font_size_text0, font_size_legend0, font_size_date_time0;
int j;

// Set axis_type :
 strcpy(&p_plot_param->axis_type[0], gseg_axdata0->axis_type);

// Foreground and background colors:
 canvas_fg_color1 = gseg_axdata0->canvas_fg_color;
 canvas_bg_color1 = gseg_axdata0->canvas_bg_color;

/* axis_type: 
* "linear"   (points, histogram, contour, and color plot types)
* "semilogx" (points plot type only)
* "semilogy"  (points plot type only)
* "loglog"  (points plot type only)
* "polar" (points plot type only)
* "3d" (points, contour, color, and mesh plot types)
*/
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

/* background_image_style:
* 1 = "center"
* 2 = "fill"
* 3 = "scale"
* 4 = "zoom"
*/
 background_image_style = gseg_axdata0->background_image_style;
 strcpy(background_image_file, gseg_axdata0->background_image_file);

// axis_scale: "auto" or "equal"
 strcpy(&p_plot_param->axis_scale[0], gseg_axdata0->axis_scale);

// axis_limits:
// if "*", set_axis_limits[i] = 0;
// else set_axis_limits[i] = 1;
 for (j = 1; j <= 6; j++ ) {
   p_plot_param->axis_limits[j-1] = gseg_axdata0->axis_limits[j-1];
   set_axis_limits1[j-1] = gseg_axdata0->set_axis_limits[j-1];
   }

 for (j = 1; j <= 3; j++ ) {
   p_plot_param->reversed_axis[j-1] = gseg_axdata0->reversed_axis[j-1];
   }


/* flag for minor ticks:
* 0 = "off"
* 1 = "on"
*/
 minor_ticks_flag = gseg_axdata0->minor_ticks_flag;

// view3d: phi (azimuth) and theta (elevation) (default: phi=30 deg theta=30deg)
 p_plot_param_3d->phi = gseg_axdata0->phi;
 p_plot_param_3d->theta = gseg_axdata0->theta;

/* grid: 
* "off"
* "on1" if color is directly set with gridcolor1
* "on2" if color is set with the code of gridchar2
*/
 strcpy(&p_plot_param->grid[0], gseg_axdata0->grid);
 gridchar1 = gseg_axdata0->gridchar1;
 gridchar2 = gseg_axdata0->gridchar2;
 gridcolor1 = gseg_axdata0->gridcolor1;
 LoadGridParam(gridchar1, gridchar2, gridcolor1);

// xlabel, ylabel, zlabel, title
// for polar plots: xlabel for angle axis, ylabel for the radial values
 strcpy(pixbuf_xlabel.text, gseg_axdata0->xlabel);
 strcpy(pixbuf_ylabel.text, gseg_axdata0->ylabel);
 strcpy(pixbuf_zlabel.text, gseg_axdata0->zlabel);
 strcpy(pixbuf_title.text, gseg_axdata0->title);

/* date_time:
* "off"
* "north"
* "northeast"
* "southeast"
* "south"
* "southwest"
* "northwest"
* "off"
*/
 strcpy(date_time1, gseg_axdata0->date_time);

// plot_box: "on" or "off"
 strcpy(&p_plot_param->plot_box[0], gseg_axdata0->plot_box);

// x,y,z tick marks: "on" or "off"
 strcpy(&p_plot_param->x_tick_marks[0], gseg_axdata0->x_tick_marks);
 strcpy(&p_plot_param->y_tick_marks[0], gseg_axdata0->y_tick_marks);
 strcpy(&p_plot_param->z_tick_marks[0], gseg_axdata0->z_tick_marks);

// x,y,z tick labels: "on" or "off"
 strcpy(&p_plot_param->x_tick_labels[0], gseg_axdata0->x_tick_labels);
 strcpy(&p_plot_param->y_tick_labels[0], gseg_axdata0->y_tick_labels);
 strcpy(&p_plot_param->z_tick_labels[0], gseg_axdata0->z_tick_labels);

// Fonts:
// 
 strcpy(font_name0, gseg_axdata0->font_name);
 font_size_title0 = gseg_axdata0->font_size_title;
 font_size_axis_labels0 = gseg_axdata0->font_size_axis_labels;
 font_size_tick_labels0 = gseg_axdata0->font_size_tick_labels;
 font_size_text0 = gseg_axdata0->font_size_text;
 font_size_legend0 = gseg_axdata0->font_size_legend;
 font_size_date_time0 = gseg_axdata0->font_size_date_time;

// Load info to JLP_GsegAxes:
 SetFontParams(font_name0, font_size_title0, font_size_axis_labels0, 
               font_size_tick_labels0, font_size_text0, font_size_legend0,
               font_size_date_time0);

 high_contrast_for_z_axis1 = gseg_axdata0->high_contrast_for_z_axis;

// Use font info to set label pixbuffers JLP_GsegAxes:
 SetAxisLabelPixbufs(pixbuf_xlabel.text, pixbuf_ylabel.text,
                     pixbuf_zlabel.text, pixbuf_title.text,
                     font_size_axis_labels1, font_size_title1,
                     pixbuf_xlabel.color);

return(0);
}

/***************************************************************************
* Read background color/image style parameters from file 
*
***************************************************************************/
int JLP_GsegAxes::ReadBackgroundStyleFromFile(char *param_filename)
{
int index;
unsigned int i1_str, i2_str, size;
char line1[256], *string;
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
             &size, 0)) != NULL ) {
          if ( strcmp(string, "black") == 0 )
            {
            canvas_fg_color1 = 0xFFFFFFFF;   /* white */
            canvas_bg_color1 = 0x000000FF;   /* black */
            }
          else 
            {
            canvas_fg_color1 = 0x000000FF;   /* black */
            canvas_bg_color1 = 0xFFFFFFFF;   /* white */
            }
       }
      }
    else if ( strncmp(line1, "background_image", 16) == 0 )
      {
       size = strlen(&line1[16]);

         if ( (string = gseg_get_string(string_get, &line1[16], &i1_str, &i2_str, &size, 0)) != NULL )
            {
            strcpy(background_image_file, string);
            }
         index = 16 + i2_str + 2;
         size = strlen(&line1[index]);
         if ( (string = gseg_get_string(string_get, &line1[index], &i1_str, &i2_str, &size, 0)) != NULL )
            {
/* background_image_style:
* 1 = "center"
* 2 = "fill"
* 3 = "scale"
* 4 = "zoom"
*/
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
* Set the axis limits and various parameters
* to emulate input from the parameter file
* (not useful when creating JLP_Gsegraf from GSEG_AXIS_DATA, ...)
***************************************************************************/
int JLP_GsegAxes::SetPlotParamAxisLimitsAndLabels(double *axis_limits_0, 
                                      int *reversed_axis_0,
                                      char *axis_scale0, char *axis_type0,
                                      char *xlabel0, char *ylabel0,
                                      char *zlabel0, char *title0)
{
int status, j;
char error_str0[64];

// axis_limits0: xmin,xmax,ymin,ymax,zmin,zmax 
  for(j = 0; j < 6; j++) {
   p_plot_param->axis_limits[j] = axis_limits_0[j];
// Will use input value (instead of automatic scale):
   set_axis_limits1[j] = 1;
   }

// axis_scale: "auto" or "equal" 
  strncpy(p_plot_param->axis_scale, axis_scale0, 5);

// axis_type: "linear", "semilogx", etc 
  strcpy(p_plot_param->axis_type, axis_type0);
/* axis_type:
* "linear"   (points, histogram, contour, and color plot types)
* "semilogx" (points plot type only)
* "semilogy"  (points plot type only)
* "loglog"  (points plot type only)
* "polar" (points plot type only)
* "3d" (points, contour, color, and mesh plot types)
*/
 if ( strcmp(p_plot_param->axis_type, "linear")   != 0 &&
      strcmp(p_plot_param->axis_type, "semilogx") != 0 &&
      strcmp(p_plot_param->axis_type, "semilogy") != 0 &&
      strcmp(p_plot_param->axis_type, "loglog")   != 0 &&
      strcmp(p_plot_param->axis_type, "polar")    != 0 &&
      strcmp(p_plot_param->axis_type, "3d")       != 0 )
    {
    strcpy(error_str0, "Invalid axis_type parameter.");
    JLP_ErrorDialog(error_str0);
    return(-1);
    }
// Set flags:
 SetAxisTypeFlags(p_plot_param->axis_type);


// axis_scale: "auto" or "equal" 
  strncpy(p_plot_param->axis_scale, axis_scale0, 5);

// Labels:
   strcpy(pixbuf_xlabel.text, xlabel0);
   strcpy(pixbuf_ylabel.text, ylabel0);
   strcpy(pixbuf_zlabel.text, zlabel0);
   strcpy(pixbuf_title.text, title0);

// Minor ticks
// minor_ticks_flag = 1;

 for (j = 0; j < 3; j++ ) {
   p_plot_param->reversed_axis[j] = reversed_axis_0[j];
   }

return(status);
}
/***************************************************************************
* Read axis limits and various parameters from the parameter file
*
***************************************************************************/
int JLP_GsegAxes::ReadAxisLimitsFromFile(char *param_filename)
{
int i, j, index; 
unsigned int i1_str, i2_str, size;
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
// axis_scale: "auto" or "equal" 
      if ( strncmp(line1, "axis_scale", 10) == 0 )
         {
         size = strlen(&line1[10]);
         if ( (string = gseg_get_string(string_get, &line1[10], &i1_str, 
               &i2_str, &size, 0)) != NULL )
            strncpy(&p_plot_param->axis_scale[0], string, 5);
         }
// axis_limits: "xmin xmax ymin ymax zmin zmax" 
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
            strcpy(pixbuf_xlabel.text, string);
            }
         }

      else if ( strncmp(line1, "ylabel", 6) == 0 )
         {
         size = strlen(&line1[6]);
         if ( (string = gseg_get_string(string_get, &line1[6], &i1_str, &i2_str, &size, 1)) != NULL )
            {
            strcpy(pixbuf_ylabel.text, string);
            }
         }

      else if ( strncmp(line1, "zlabel", 6) == 0 )
         {
         size = strlen(&line1[6]);
         if ( (string = gseg_get_string(string_get, &line1[6], &i1_str, &i2_str, &size, 1)) != NULL )
            {
            strcpy(pixbuf_zlabel.text, string);
            }
         }

      else if ( strncmp(line1, "title", 5) == 0 )
         {
         size = strlen(&line1[5]);
         if ( (string = gseg_get_string(string_get, &line1[5], &i1_str, &i2_str, &size, 1)) != NULL )
            {
            strcpy(pixbuf_title.text, string);
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
int JLP_GsegAxes::ReadFontsFromFile(char *param_filename)
{
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

// Use font info to set label pixbuffers JLP_GsegAxes:
 SetAxisLabelPixbufs(pixbuf_xlabel.text, pixbuf_ylabel.text,
                     pixbuf_zlabel.text, pixbuf_title.text,
                     font_size_axis_labels1, font_size_title1, 
                     pixbuf_xlabel.color);

return(0);
}
