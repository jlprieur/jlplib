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

 JLP_Gsegraf(JLP_Gseg *jlp_gseg0, GSEG_PLOT_DATA *gseg_pltdata0,
             const int nplots0, GSEG_AXIS_DATA gseg_axdata0); 

// Definitions of function prototypes 
#include "jlp_gsegraf_prototypes.h"

private:

 int fonts_initialized;
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

/* Declare color variables for all colors with maximum saturation from blue to green to red */
  int n_color_spectrum_1, n_color_spectrum_2;
  UINT32 color_spectrum_1[1021], color_spectrum_2[769];

/* Declare plot-parameter variables and pointers */
  int maxline1;
  char *line1, *string_get; 
  char *stemflags; 
  char *save_filename;
  double *stemvalues;

// Labels
  GSEG_EXTRA_LABELS extra_labels1;

};

#endif
