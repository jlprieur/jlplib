/*******************************************************************************
* jlp_gsegraf.h
* declaration of JLP_Gseg class
*
* This file was modified from GSEGrafix (gsegrafix-1.0.6, sept. 2011)
* Copyright © 2008, 2009, 2010, 2011 Spencer A. Buckner
* http://savannah.gnu.org/projects/gsegrafix
*
* JLP
* Version 14/12/2016
*******************************************************************************/
#ifndef jlp_gsegraf_h_
#define jlp_gsegraf_h_

#include <stdio.h>
#include <stdlib.h>  // exit()
#include <string.h>

#include "jlp_gseg.h"         // Virtual JLP_Gseg class
#include "jlp_gseg_utils.h"    // Various utility routines

// Definition of some structures used here
#include "jlp_gsegraf_defs.h"

class JLP_GsegAxes;
class JLP_GsegData;

// Definition of the JLP_Gsegraf class
class JLP_Gsegraf {

public:

// Constructors:
 JLP_Gsegraf(JLP_Gseg *jlp_gseg0, char *parameter_file_0, 
             char **save_filename_0, int *close_flag_0);

 JLP_Gsegraf(JLP_Gseg *jlp_gseg0, char *parameter_file_0, 
             char **save_filename_0, int *close_flag_0, char *font_name0,
             double *font_size_date_time0, double *font_size_legend0,
             double *font_size_text0, double *font_size_tick_labels0,
             double *font_size_axis_labels0, double *font_size_title0);

// Definition of function prototypes 

// Accessors to symbols/lines encoding strings:
 char *SymbolString(){return symbol_string;};
 char *SymbolString1(){return symbol_string1;};
 char *SymbolString2(){return symbol_string2;};

#include "jlp_gsegraf_prototypes.h"

private:

 JLP_Gseg  *jlp_gseg1;
 JLP_GsegAxes *jlp_gseg_axes1;
 JLP_GsegData *jlp_gseg_data1;

/* Declare window size (initialized at object creation 
* and updated when the window has changed its size) */
  int window_width1, window_height1;

/* Declare dashed-line variables 
* (initialized at object creation and updated when the window has changed 
*  its size) */
   double dash1, space_dash1, space_dot1;

/* Declare color variables and pointers for colors specified by color characters */
   UINT32 color_rgba1[17], zoom_fill_color1;
   UINT32 *fill_colors_rgba1, *outline_colors_rgba1;

/* Declare color variables for all colors with maximum saturation from blue to green to red */
   int n_color_spectrum_1, n_color_spectrum_2;
   UINT32 color_spectrum_1[1021], color_spectrum_2[769];

/* Declare plot-parameter variables and pointers */
   int maxline1;
   int *ninterp;
   UINT32 canvas_fg_color1, canvas_bg_color1; 
   UINT32 *alphacolor, *meshcolors, *contourcolors;
   char *line1, *string_get; 
   char *stemflags; 
   char *save_filename;
   double *zblack, *zwhite, *stemvalues;

// Labels
   GSEG_EXTRA_LABELS extra_labels1;

/* symbol-specification characters "ldcCtTsSiIpPhH+xra" */
 char symbol_string[128];
/* symbol-specification characters "cCtTsSiIpPhH" */
 char symbol_string1[128];
/* symbol-specification characters "+xra" */
 char symbol_string2[128];
/* color-specification characters "kaswrylqbfmogtnpx" */
 char color_string[128];

};

#endif
