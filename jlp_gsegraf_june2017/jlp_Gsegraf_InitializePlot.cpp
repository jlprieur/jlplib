/*******************************************************************************
* GSEG_InitializePlot.c
*
* Contains functions:
*    InitializePlot
*
* Initializes plot variables, reads plot-parameter file, checks plot-parameter
* values, calculates various plot variables, and analyzes various plot
* variables.
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
#include <stdlib.h>          // exit()
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"    // JLP_GsegAxes class
#include "jlp_gseg_data.h"    // JLP_GsegData class

/******************************************************************************
*
******************************************************************************/
int JLP_Gsegraf::GSEG_InitializePlot(JLP_Gseg *jlp_gseg0,
                              char *parameter_file_0, char **save_filename_0,
                              int *close_flag_0, char *font_name0,
                              double *font_size_date_time0, 
                              double *font_size_legend0,
                              double *font_size_text0, 
                              double *font_size_tick_labels0,
                              double *font_size_axis_labels0, 
                              double *font_size_title0)
{
int status, flag_ref0, set_axis_limits0[6];
char axis_type0[64];

/* Save to private variables: */
jlp_gseg1 = jlp_gseg0;

// Create JLP_GsegAxes object:
jlp_gseg_axes1 = new JLP_GsegAxes(jlp_gseg1, this);
 
// Create JLP_GsegData object:
jlp_gseg_data1 = new JLP_GsegData(jlp_gseg1, this);
 
/* Initialize plot variables */
 GSEG_InitializeVariables();

// Read plot-parameter file (and return save_filename_0 and close_flag_0)
// NB: save plot parameters to private variables of JLP_Gsegraf and JLP_GsegAxes
 status = ReadParamFile(parameter_file_0, save_filename_0, close_flag_0);
 if(status) return(-1);

// Get axis type from JLP_GsegAxes object:
 jlp_gseg_axes1->GetAxisType(axis_type0);

/* Check plot-parameter values */
 CheckParamData();

// Update size of plot (from information contained in jlp_gseg1)
   GSEG_InitializePlotSize();

   /* Analyze plot data */
   if((jlp_gseg_axes1->flag_2d_rect() == 1 )
      || (jlp_gseg_axes1->flag_polar() == 1 ))
      {
/* Read the plot data files */
      jlp_gseg_data1->Read2dFiles(axis_type0);

/* Find minimum and maximum values of plot data and other useful parameters */
      jlp_gseg_data1->DataMinMax();

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
      jlp_gseg_data1->DataMinMax3d();

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
/****************************************************************************
*
*****************************************************************************/
void JLP_Gsegraf::GSEG_SetPointersFromPlot(char **plot_types0, 
                                           int **styleflags0)
{

*plot_types0 = jlp_gseg_data1->PlotTypesPtr(); 
*styleflags0 = jlp_gseg_data1->StyleFlagsPtr(); 

return;
}
/*****************************************************************************
* Get canvas background color
*****************************************************************************/
void JLP_Gsegraf::Get_canvas_bg_color(UINT32 *canvas_bg_color0)
{
*canvas_bg_color0 = canvas_bg_color1;
return;
}
/*****************************************************************************
* Get canvas foreground color
*****************************************************************************/
void JLP_Gsegraf::Get_canvas_fg_color(UINT32 *canvas_fg_color0)
{
*canvas_fg_color0 = canvas_fg_color1;
return;
}
/*****************************************************************************
*
*****************************************************************************/
void JLP_Gsegraf::GetColorValues(UINT32 *zoom_fill_color_0,
                         UINT32 *canvas_fg_color_0,
                         UINT32 *canvas_bg_color_0)
{

*zoom_fill_color_0 = zoom_fill_color1;
*canvas_fg_color_0 = canvas_fg_color1;
*canvas_bg_color_0 = canvas_bg_color1;

return;
}
/*****************************************************************************
*
*****************************************************************************/
void JLP_Gsegraf::SetCanvasColor(const UINT32 canvas_fg_color_0, 
                                 const UINT32 canvas_bg_color_0)
{
canvas_fg_color1 = canvas_fg_color_0;
canvas_bg_color1 = canvas_bg_color_0;

return;
}
/*****************************************************************************
* Determine the DrawSymbol1 option 
* by decoding stylechar1 
* e.g.,
*   strcpy(symbol_string, "ld.cCtTsSiIpPhH+xra");
*   strcpy(symbol_string1, "cCtTsSiIpPhH");
*
* INPUT:
*  iplt : plot index (from 0 to nplots1)
* OUTPUT:
*  ifunc : DrawSymbol1 option
*****************************************************************************/
int JLP_Gsegraf::SymbolFromStyleChar1(char *symbol_str,
                                      const int iplt, int *ifunc)
{
int status = -1, nplots0;
char *pchar, stylechar1_0;

 *ifunc = 0;
 jlp_gseg_data1->Get_nplots(&nplots0);
 if((iplt < 0) || (iplt >= nplots0)) {
   fprintf(stderr, "SymbolFromStyleChar1/Error iplt=%d\n", iplt);
   return(-1);
   }

// Retrieve stylechar1 of plot 
 stylechar1_0 = jlp_gseg_data1->StyleChar1(iplt);

// Decode it using symbol_str
 pchar = strchr(symbol_str, stylechar1_0);
 if(pchar != NULL) {
   *ifunc = pchar - symbol_str;
   status = 0;
   } else {
   fprintf(stderr, "SymbolFromStyleChar1/Stylechar= >%c< in >%s<\n", 
           stylechar1_0, symbol_str);
   }

return(status);
}
/*****************************************************************************
* Determine the DrawSymbol2 option 
* by decoding stylechar2 
* e.g., in the symbol_string2 ("+xra")
*
* INPUT:
*  iplt : plot index (from 0 to nplots1)
* OUTPUT:
*  ifunc : DrawSymbol2 option
*****************************************************************************/
int JLP_Gsegraf::SymbolFromStyleChar2(char *symbol_str,
                                      const int iplt, int *ifunc)
{
int status = -1, nplots0;
char *pchar, stylechar2_0;

 *ifunc = 0;
 jlp_gseg_data1->Get_nplots(&nplots0);
 if((iplt < 0) || (iplt >= nplots0)) {
   fprintf(stderr, "SymbolFromStyleChar2/Error iplt=%d\n", iplt);
   return(-1);
   }

// Retrieve stylechar of plot 
 stylechar2_0 = jlp_gseg_data1->StyleChar2(iplt);

// Decode it using symbol_str
 pchar = strchr(symbol_str, stylechar2_0);

 if(pchar != NULL) {
   *ifunc = pchar - symbol_str;
   status = 0;
   } else {
   fprintf(stderr, "SymbolFromStyleChar2/Stylechar= >%c< in >%s<\n", 
           stylechar2_0, symbol_str);
   }

return(status);
}
/****************************************************************************
* Set stylecolor2, outline_colors_rgba and fill_colors_rgba
* (called by jlp_GsegData_ReadDataParamFile.cpp)
*****************************************************************************/
int JLP_Gsegraf::SetColorsFromStyleChars(UINT32 *stylecolor2_0, 
                                         char stylechar1_0, char stylechar2_0, 
                                         const int iplt)
{
int status = -1, nplots0, index;
char *pchar, buffer[32];

// Initialization by default to foreground color:
*stylecolor2_0 = canvas_fg_color1; 

 jlp_gseg_data1->Get_nplots(&nplots0);
 if((iplt < 0) || (iplt >= nplots0)) return(-1);

 if((pchar = strchr(color_string, stylechar2_0)) != NULL) {
/* Get index to color character */
    index = pchar - &color_string[0];
/* Get specified color */
    *stylecolor2_0 = color_rgba1[index];
/* Set specified outline color */
    outline_colors_rgba1[iplt] = *stylecolor2_0;
// If stylechar1 is among "ctsiphb", set fill color to background canvas color
    strcpy(buffer, "ctsiphb");
    if ( (pchar = strchr(buffer, stylechar1_0)) != NULL )
        fill_colors_rgba1[iplt] = canvas_bg_color1;
/* Else set specified fill color to stylecolor2_0 */
     else
        fill_colors_rgba1[iplt] = *stylecolor2_0;
   status = 0;
   }

return(status);
}
/****************************************************************************
* Set stylecolor1/2 from stylechar1/2
* (called by jlp_GsegData_ReadDataParamFile.cpp)
* stylechar compared to  color_string = "kaswrylqbfmogtnpx"
*****************************************************************************/
int JLP_Gsegraf::SetStyleColorFromStyleChar(UINT32 *stylecolor_0, 
                                            char stylechar_0)
{
int status = -1, nplots0, index;
char *pchar, buffer[32];

*stylecolor_0 = canvas_fg_color1; 
 jlp_gseg_data1->Get_nplots(&nplots0);

// color_string (= "kaswrylqbfmogtnpx")
 if((pchar = strchr(color_string, stylechar_0)) != NULL) {
/* Get index to color character */
    index = pchar - &color_string[0];
/* Get specified color */
    *stylecolor_0 = color_rgba1[index];
   status = 0;
   }

return(status);
}
/****************************************************************************
* Set outline_colors_rgba and fill_colors_rgba
* (called by jlp_GsegData_ReadDataParamFile.cpp)
*****************************************************************************/
int JLP_Gsegraf::SetColorsFromStyleColor(UINT32 stylecolor2_0, 
                                         char stylechar1_0, const int iplt)
{
int nplots0;
char *pchar, buffer[32];

 jlp_gseg_data1->Get_nplots(&nplots0);
 if((iplt < 0) || (iplt >= nplots0)) return(-1);

/* Set specified outline color */
    outline_colors_rgba1[iplt] = stylecolor2_0;
// If stylechar1 is among "ctsiphb", set fill color to background canvas color
    strcpy(buffer, "ctsiphb");
    if ( (pchar = strchr(buffer, stylechar1_0)) != NULL )
        fill_colors_rgba1[iplt] = canvas_bg_color1;
/* Else set specified fill color to stylecolor2__0 */
     else
        fill_colors_rgba1[iplt] = stylecolor2_0;

return(0);
}
/****************************************************************************
* 2d color plot type
* when input string of parameter file is:
* "nearest zblack zwhite" or "bilinear zblack zwhite"
* where zblack and zwhite are two decimal numbers
*****************************************************************************/
int JLP_Gsegraf::SetZBlackWhite(const double zblack_0, 
                                const double zwhite_0, const int iplt)
{
int nplots0;

 if((iplt < 0) || (iplt >= nplots0)) return(-1);

 zblack[iplt] = zblack_0;
 zwhite[iplt] = zwhite_0;

return(0);
}
/****************************************************************************
*
*****************************************************************************/
int JLP_Gsegraf::SetAlphaColor(const UINT32 alphacolor_0, const int iplt)
{
int nplots0;

 if((iplt < 0) || (iplt >= nplots0)) return(-1);

   alphacolor[iplt] = alphacolor_0;

/* check alpha value */ 
   if(alphacolor[iplt] > 0xFF ) alphacolor[iplt] = 0xFF;

return(0);
}
