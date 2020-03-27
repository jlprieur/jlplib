/*******************************************************************************
*
* jlp_gsegraf_ChangeAxesLimits.cpp 
*
* Contains:
* int QuestionForChangingAxesRotation(char *prompt0, char *string0);
* int QuestionForChangingAxesLimits(char *prompt0, char *string0);
* int DecodeStringForChangingAxesLimits(char *string, double *xmin, 
*                                       double *xmax,
*                           double *ymin, double *ymax, double *zmin,
*                           double *zmax, char *error_message);
*
* JLP
* Version 06/11/2016
*******************************************************************************/
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "jlp_gseg_axes.h"

/*****************************************************************************
* Create the string that will be displayed for prompting new axes limits
*
* OUTPUT:
* prompt0 : question
* string0 : current values
*****************************************************************************/
int JLP_GsegAxes::QuestionForChangingAxesRotation(char *prompt0, 
                                                      char *string0)
{
double phi, theta;
const char prompt[] = "Enter plot orientation angles:\n"
                      "   azimuth angle (deg) (direction of positive x axis)\n"
                      "   elevation angle (deg) (orientation of x-y plane)\n"
                      "   0 \xE2\x89\xA4 elevation \xE2\x89\xA4 90 degrees";

strcpy(prompt0, prompt);
strcpy(string0, "");

 if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
    {
/* Get view-direction-data string */
      phi   = p_plot_param_3d->phi;
      theta = p_plot_param_3d->theta;
      sprintf(string0, "%g %g", phi, theta);
    }

return(0);
}
/*****************************************************************************
* Create the string that will be displayed for prompting new axes limits 
*
* OUTPUT:
* prompt0 : question
* string0 : current values
*****************************************************************************/
int JLP_GsegAxes::QuestionForChangingAxesLimits(char *prompt0, 
                                                     char *string0)
{
/* Declare variables */
int nxvalues, nyvalues, nzvalues, iplot, nplots, gseg_plot_type0;
int disp_coord_flag, status = -1;;
double xmin, xmax, ymin, ymax, zmin, zmax;
const char prompt[] = "Enter new axis minimum and maximum values:\n"
                      "   2d rectangular plots: xmin xmax ymin ymax\n"
                      "   2d polar plots: rmin rmax\n"
                      "   2d contour and color plots: xmin xmax ymin ymax zmin zmax\n"
                      "   3d plots: xmin xmax ymin ymax zmin zmax";

// Copy question
strcpy(prompt0, prompt);
strcpy(string0, "");

/* Get old axis data */
 nxvalues = p_ticklabels->nxvalues;
 nyvalues = p_ticklabels->nyvalues;
 nzvalues = p_ticklabels->nzvalues;
 xmin = p_ticklabels->xvalues[0];
 xmax = p_ticklabels->xvalues[nxvalues-1];
 ymin = p_ticklabels->yvalues[0];
 ymax = p_ticklabels->yvalues[nyvalues-1];
 zmin = p_ticklabels->zvalues[0];
 zmax = p_ticklabels->zvalues[nzvalues-1];
 xmin = xmin - p_ticklabels->xoffset1;
 xmax = xmax + p_ticklabels->xoffset2;
 ymin = ymin - p_ticklabels->yoffset1;
 ymax = ymax + p_ticklabels->yoffset2;
 zmin = zmin - p_ticklabels->zoffset1;
 zmax = zmax + p_ticklabels->zoffset2;

/* Get axis-data string */
 if (strcmp(p_plot_param->axis_type, "linear") == 0 )
    {
    jlp_gsegraf1->GSEG_get_nplots(&nplots);
    disp_coord_flag = 0;
    for ( iplot=1; iplot<=nplots; iplot++ )
       {
/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
       jlp_gsegraf1->GSEG_get_plot_type(iplot, &gseg_plot_type0);
// 3="contour" or 4="color"
       if(gseg_plot_type0 == 3 || gseg_plot_type0 == 4)
          disp_coord_flag = 1;
       }

    if ( disp_coord_flag == 1 )
       sprintf(string0, "%g %g %g %g %g %g",
                xmin, xmax, ymin, ymax, zmin, zmax);
    else
       sprintf(string0, "%g %g %g %g", xmin, xmax, ymin, ymax);
    status = 0;
    }

 else if ( strcmp(p_plot_param->axis_type, "semilogx") == 0 )
    {
    sprintf(string0, "%g %g %g %g",
            pow(10.0, floor(xmin)), pow(10.0, ceil(xmax)), ymin, ymax);
    status = 0;
    }
 else if ( strcmp(p_plot_param->axis_type, "semilogy") == 0 )
    {
    sprintf(string0, "%g %g %g %g",
            xmin, xmax, pow(10.0, floor(ymin)), pow(10.0, ceil(ymax)));
    status = 0;
    }
 else if ( strcmp(p_plot_param->axis_type, "loglog") == 0 )
    {
    sprintf(string0, "%g %g %g %g",
            pow(10.0, floor(xmin)), pow(10.0, ceil(xmax)),
            pow(10.0, floor(ymin)), pow(10.0, ceil(ymax)));
    status = 0;
    }
 else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
    {
    sprintf(string0, "%g %g", ymin, ymax);
    status = 0;
    }
 else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
    {
    sprintf(string0, "%g %g %g %g %g %g",
            xmin, xmax, ymin, ymax, zmin, zmax);
    status = 0;
    }

return(status);
}
/*****************************************************************************
* Decode the input string and change the axes limits
*
*****************************************************************************/
int JLP_GsegAxes::DecodeStringForChangingAxesLimits(char *string0, 
                                                    double *xmin, double *xmax, 
                                                    double *ymin, double *ymax, 
                                                    double *zmin, double *zmax, 
                                                    char *error_message) 
{
/* Declare variables */
int iplot, nplots, flag, gseg_plot_type0;
const char *error_str[] =
   { "Incorrect number of axis minimum and maximum values;\ntwo expected.",
     "Incorrect number of axis minimum and maximum values;\nfour expected.",
     "Incorrect number of axis minimum and maximum values;\nsix expected.",
     "Invalid x-axis limit.",
     "Invalid y-axis limit.",
     "Invalid axis limit."};

// Set default values:
   *xmin = 0.0,
   *xmax = 1.0,
   *ymin = 0.0,
   *ymax = 1.0,
   *zmin = 0.0,
   *zmax = 1.0;

   if ( string0 == NULL ) {
      strcpy(error_message, "Empty string.");
      return(-2);
      }

   /* Get axes data */
      /* Get new axis limits */
      if ( strcmp(p_plot_param->axis_type, "linear") == 0 )
         {
         jlp_gsegraf1->GSEG_get_nplots(&nplots);
         flag = 0;
         for ( iplot=1; iplot<=nplots; iplot++ )
            {
/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
         jlp_gsegraf1->GSEG_get_plot_type(iplot, &gseg_plot_type0);
// 3="contour" or 4="color"
         if(gseg_plot_type0 == 3 || gseg_plot_type0 == 4)
               flag = 1;
            }

         if ( flag == 0 )
            {
            if(sscanf(string0, "%lf %lf %lf %lf", xmin, xmax, ymin, ymax) != 4)
               {
               strcpy(error_message, error_str[1]);
               return(-1);
               }
            }
         else if ( flag == 1 )
            {
            if ( sscanf(string0, "%lf %lf %lf %lf %lf %lf",
                        xmin, xmax, ymin, ymax, zmin, zmax) != 6 )
               {
               strcpy(error_message, error_str[2]);
               return(-1);
               }
            }
         }

      else if ( strcmp(p_plot_param->axis_type, "semilogx") == 0 )
         {
         if ( sscanf(string0, "%lf %lf %lf %lf", xmin, xmax, ymin, ymax) != 4 )
            {
            strcpy(error_message, error_str[1]);
            return(-1);
            }

         if ( *xmin <= 0.0 || *xmax <= 0.0 )
            {
            strcpy(error_message, error_str[3]);
            return(-1);
            }

         *xmin = log10(fabs(*xmin));
         *xmax = log10(fabs(*xmax));
         }

      else if ( strcmp(p_plot_param->axis_type, "semilogy") == 0 )
         {
         if(sscanf(string0, "%lf %lf %lf %lf", xmin, xmax, ymin, ymax) != 4 )
            {
            strcpy(error_message, error_str[1]);
            return(-1);
            }

         if ( *ymin <= 0.0 || *ymax <= 0.0 )
            {
            strcpy(error_message, error_str[4]);
            return(-1);
            }

         *ymin = log10(fabs(*ymin));
         *ymax = log10(fabs(*ymax));
         }

      else if ( strcmp(p_plot_param->axis_type, "loglog") == 0 )
         {
         if(sscanf(string0, "%lf %lf %lf %lf", xmin, xmax, ymin, ymax) != 4 )
            {
            strcpy(error_message, error_str[1]);
            return(-1);
            }

         if ( *xmin <= 0.0 || *xmax <= 0.0 ||
              *ymin <= 0.0 || *ymax <= 0.0 )
            {
            strcpy(error_message, error_str[5]);
            return(-1);
            }

         *xmin = log10(fabs(*xmin));
         *xmax = log10(fabs(*xmax));
         *ymin = log10(fabs(*ymin));
         *ymax = log10(fabs(*ymax));
         }

      else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
         {
         if ( sscanf(string0, "%lf %lf", ymin, ymax) != 2 )
            {
            strcpy(error_message, error_str[0]);
            return(-1);
            }
         }

      else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
         {
         if ( sscanf(string0, "%lf %lf %lf %lf %lf %lf",
                     xmin, xmax, ymin, ymax, zmin, zmax) != 6 )
            {
            strcpy(error_message, error_str[2]);
            return(-1);
            }
         }

return(0);
}
