/*******************************************************************************
* GSEG_InitializePlot.c
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
* Version 24/07/2017
*******************************************************************************/
#include <stdlib.h>          // exit()
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"    // JLP_GsegAxes class
#include "jlp_gseg_data.h"    // JLP_GsegData class

/******************************************************************************
* Version with a parameter file containing all required parameters
******************************************************************************/
int JLP_Gsegraf::GSEG_InitializePlotWithParamFile(JLP_Gseg *jlp_gseg0, 
                                                  char *parameter_file_0, 
                                                  char **save_filename_0, 
                                                  int *close_flag_0)
{
int status;

/* Save to private variables: */
 jlp_gseg1 = jlp_gseg0;

// Create JLP_GsegAxes object:
 jlp_gseg_axes1 = new JLP_GsegAxes(jlp_gseg1, this);
 
// Create JLP_GsegData object:
 jlp_gseg_data1 = new JLP_GsegData(jlp_gseg1, this);
 
/* Initialize private plot variables (needs jlp_gseg1 !) */
 GSEG_InitializeVariables();

// Read plot-parameter file (and return save_filename_0 and close_flag_0)
// NB: save plot parameters to private variables of JLP_Gsegraf and JLP_GsegAxes
 status = ReadParamFile(parameter_file_0, save_filename_0, close_flag_0);
 if(status) {
  fprintf(stderr, "GSEG_InitializePlot/Error in ReadParamFile : status = %d\n",
          status);
  return(-1);
  }

// Finalize the initialization of Data and Axes parameters 
// and check their validity 
 status = GSEG_InitializePlot_CheckDataAndAxes();

 return(0);
}
/******************************************************************************
* Version with GSEG_PLOT_DATA and GSEG_AXIS_DATA structures 
* containing all the required parameters
******************************************************************************/
int JLP_Gsegraf::GSEG_InitializePlotWithGsegPlotData(JLP_Gseg *jlp_gseg0,
                                                 GSEG_PLOT_DATA *gseg_pltdata0,
                                                 const int nplots0,
                                                 GSEG_AXIS_DATA gseg_axdata0)
{
int status;

/* Save to private variables: */
 jlp_gseg1 = jlp_gseg0;

/* Initialize private plot variables (needs jlp_gseg1 !) */
 GSEG_InitializeVariables();

// Create JLP_GsegAxes object:
 jlp_gseg_axes1 = new JLP_GsegAxes(jlp_gseg1, this, gseg_axdata0);
 
// Create JLP_GsegData object:
 jlp_gseg_data1 = new JLP_GsegData(jlp_gseg1, this, gseg_pltdata0, nplots0);
 
// Save plot parameters to private variables of JLP_Gsegraf and JLP_GsegAxes
 InitPlotWithoutParamFile();

// Finalize the initialization of Data and Axes parameters 
// and check their validity 
 status = GSEG_InitializePlot_CheckDataAndAxes();

 return(0);
}
/******************************************************************************
* Finalize the initialization of Data and Axes parameters 
* and check their validity 
******************************************************************************/
int JLP_Gsegraf::GSEG_InitializePlot_CheckDataAndAxes()
{
int flag_ref0, set_axis_limits0[6], high_contrast0;
char axis_type0[64];

// Get axis type from JLP_GsegAxes object:
 jlp_gseg_axes1->GetAxisType(axis_type0);

/* Check plot-parameter values */
 CheckParamData();

// Update size of plot (from information contained in jlp_gseg1)
 GSEG_InitializePlotSize();

// Get current value of high contrast z scale:
 jlp_gseg_axes1->GetHighContrastForZAxis(&high_contrast0);

/* Analyze plot data */
 if( (jlp_gseg_axes1->flag_2d_rect() == 1 )
    || (jlp_gseg_axes1->flag_polar() == 1 ) )
    {
/* Read the plot data files */
    jlp_gseg_data1->Read2dFiles(axis_type0);

/* Find minimum and maximum values of plot data and other useful parameters */
    jlp_gseg_data1->DataMinMax(high_contrast0);

/* Adjust axes and store those values as reference values */
    flag_ref0 = 1;
    jlp_gseg_axes1->GetAxisLimitsOnOff(set_axis_limits0, 6);
    jlp_gseg_axes1->AxisLimits(flag_ref0, set_axis_limits0);
    }
 else if(jlp_gseg_axes1->flag_3d() == 1 )
    {
/* Read the plot data files */
    jlp_gseg_data1->Read3dFiles();

/* Find minimum and maximum values of plot data and other useful parameters */
    jlp_gseg_data1->DataMinMax3d(high_contrast0);

/* Adjust axes and store those values as reference values */
    flag_ref0 = 1;
    jlp_gseg_axes1->GetAxisLimitsOnOff(set_axis_limits0, 6);
    jlp_gseg_axes1->AxisLimits(flag_ref0, set_axis_limits0);
    }
  else
    {
    fprintf(stderr, 
            "GSEG_InitializePlot/Fatal error: AxisLimits not initialized !\n");
    exit(-1);
    }

return(0);
}
/******************************************************************************
*
******************************************************************************/
int JLP_Gsegraf::GSEG_SetFonts(char *font_name0, double *font_size_date_time0, 
                               double *font_size_legend0,
                               double *font_size_text0, 
                               double *font_size_tick_labels0,
                               double *font_size_axis_labels0, 
                               double *font_size_title0)
{
// Get information about the selected fonts:
jlp_gseg_axes1->GetFontParams(font_name0, font_size_title0,
                              font_size_axis_labels0, font_size_tick_labels0,
                              font_size_text0, font_size_legend0,
                              font_size_date_time0);

// Set fonts for device :
jlp_gseg1->GSEG_SetFontFamily(font_name0, *font_size_date_time0,
                              *font_size_legend0, *font_size_text0,
                              *font_size_tick_labels0, *font_size_axis_labels0,
                              *font_size_title0);

fonts_initialized = 1;

return(0);
}
/******************************************************************************
* Update size of plot (from information contained in jlp_gseg1)
*
******************************************************************************/
int JLP_Gsegraf::GSEG_InitializePlotSize()
{
/* Declare variables */
int xx0, yy0, width0, height0, max_dimension;
double tick_major0, tick_minor0, dash0, space_dash0, space_dot0;

  if(jlp_gseg1 == NULL) return(-1);

// Get current window (device) size
   jlp_gseg1->GSEG_GetWindowLimits(&xx0, &width0, &yy0, &height0);

  window_width1 = width0;
  window_height1 = height0;

/* Calculate tick-mark and dashed-line variables */
   if ( window_width1 >= window_height1 )
      max_dimension = window_width1;
   else
      max_dimension = window_height1;
   tick_major0 = (8.0 / 768.0) * max_dimension;
   tick_minor0 = 0.5 * tick_major0;
   dash0 = tick_major0;
   space_dash0 = 0.5 * dash0;
   space_dot0 = space_dash0 - 1.0;
   GSEG_SetDashParameters(tick_major0, tick_minor0,
                          dash0, space_dash0, space_dot0);


return(0);
}
/***************************************************************************
* LoadCurvePlotDataFromFile
*
* INPUT:
*  axis_type0 : "linear" "semilogx", "semilogy", "loglog", "polar", etc
***************************************************************************/
int JLP_Gsegraf::GSEG_LoadCurvePlotDataFromFile(char *filename0, 
                                                const int reset_first0,
                                                char *axis_type0)
{
int status = -1; 

if(jlp_gseg_axes1 != NULL) {
 jlp_gseg_axes1->GetAxisType(axis_type0);
 if(jlp_gseg_data1 != NULL)
   status = jlp_gseg_data1->LoadCurvePlotDataFromFile(filename0, reset_first0,
                                                      axis_type0);
 }

return(status);
}
/***************************************************************************
* LoadImagePlotDataFromFitsFile
*
* INPUT:
*  axis_type0 : "linear" "semilogx", "semilogy", "loglog", "polar", etc
***************************************************************************/
int JLP_Gsegraf::GSEG_LoadImagePlotDataFromFitsFile(char *filename0,
                                                const int reset_first0,
                                                char *axis_type0)
{
int status = -1; 

if(jlp_gseg_axes1 != NULL) {
 jlp_gseg_axes1->GetAxisType(axis_type0);
 if(jlp_gseg_data1 != NULL)
   status = jlp_gseg_data1->LoadImagePlotDataFromFitsFile(filename0, 
                                                          reset_first0, 
                                                          axis_type0);
 }

return(status);
}
/*****************************************************************************
* Get canvas background color
*****************************************************************************/
void JLP_Gsegraf::Get_canvas_bg_color(UINT32 *canvas_bg_color0)
{
*canvas_bg_color0 = 0x00;
if(jlp_gseg_axes1 != NULL)
  *canvas_bg_color0 = jlp_gseg_axes1->canvas_bg_color();
return;
}
/*****************************************************************************
* Get canvas foreground color
*****************************************************************************/
void JLP_Gsegraf::Get_canvas_fg_color(UINT32 *canvas_fg_color0)
{
*canvas_fg_color0 = 0xFF;
if(jlp_gseg_axes1 != NULL)
  *canvas_fg_color0 = jlp_gseg_axes1->canvas_fg_color();
return;
}
/*****************************************************************************
* Get ticklen nz values (used for estimating the  contour size) 
*****************************************************************************/
int JLP_Gsegraf::Get_nz_values(int *nzvalues)
{
int status = -1;

*nzvalues = 0;
if(jlp_gseg_axes1 != NULL)
 status = jlp_gseg_axes1->GetNzValues(nzvalues);

return(status);
}
/*****************************************************************************
*
*****************************************************************************/
void JLP_Gsegraf::GetColorValues(UINT32 *zoom_fill_color0,
                         UINT32 *canvas_fg_color0,
                         UINT32 *canvas_bg_color0)
{
*canvas_bg_color0 = 0x00;
*canvas_fg_color0 = 0xFF;
*zoom_fill_color0 = 0xFF;

if(jlp_gseg_axes1 != NULL) {
  *canvas_bg_color0 = jlp_gseg_axes1->canvas_bg_color();
  *canvas_fg_color0 = jlp_gseg_axes1->canvas_fg_color();
  *zoom_fill_color0 = jlp_gseg_axes1->zoom_fill_color();
  }

return;
}
/*****************************************************************************
*
*****************************************************************************/
void JLP_Gsegraf::SetCanvasColor(const UINT32 canvas_fg_color0, 
                                 const UINT32 canvas_bg_color0)
{
if(jlp_gseg_axes1 != NULL){
  jlp_gseg_axes1->set_canvas_fg_color(canvas_fg_color0);
  jlp_gseg_axes1->set_canvas_bg_color(canvas_bg_color0);
  }

return;
}
/****************************************************************************
*
* Get_graphic_type:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gegraf_3d_curves
* 7 = gegraf_3d_images
* 8 = gsegraf_polar_curve
*
*****************************************************************************/
int JLP_Gsegraf::Get_graphic_type(int *gdev_graphic_type0)
{
int iplot0, nplots0, gseg_plot_type0, image_plot0;
char axis_type0[64];

// Check if image plot (direct way):
 image_plot0 = 0;
 jlp_gseg_data1->Get_nplots(&nplots0);
 for ( iplot0 = 1; iplot0 <= nplots0; iplot0++ )
   {
   jlp_gseg_data1->GetGsegPlotType(iplot0, &gseg_plot_type0);
/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
   if ((gseg_plot_type0 == 3 ) || (gseg_plot_type0 == 4 ))
     {
      image_plot0 = 1;        
      break;
     }
   }

// Use axis_type to determine gdev_graphic_type
  jlp_gseg_axes1->GetAxisType(axis_type0);

  if ( strcmp(axis_type0, "linear")   == 0 ||
       strcmp(axis_type0, "semilogx") == 0 ||
       strcmp(axis_type0, "semilogy") == 0 ||
       strcmp(axis_type0, "loglog")   == 0 ) {
    *gdev_graphic_type0 = 4;
  } else if ( strcmp(axis_type0, "linear")   == 0){
    if(image_plot0 == 1)
    *gdev_graphic_type0 = 5;
    else
    *gdev_graphic_type0 = 4;
   } else if ( strcmp(axis_type0, "3d") == 0 ) {
    if(image_plot0 == 1)
    *gdev_graphic_type0 = 7;
    else
    *gdev_graphic_type0 = 6;
// Polar plot:
   } else {
    *gdev_graphic_type0 = 8;
   }
 
/** DEBUG
printf("Get_graphic_type/gdev_graphic_type0=%d\n", *gdev_graphic_type0);
**/
return(0);
}
