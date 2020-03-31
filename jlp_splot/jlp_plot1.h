/************************************************************
* JLP_Plot1 class
*
* JLP
* Version 22/02/2019
************************************************************/
#ifndef _jlp_plot1_def
#define _jlp_plot1_def

#include "jlp_plot1_def.h"  // PLOT1_FILE_... structures

class JLP_Plot1 {
public:

// Contained in "jlp_plot1.cpp":
// Constructor:
  JLP_Plot1(char *plot_file_name);

  void Plot1_Init();

private:
// n_pfile_param_max: size of the lines of the pfiledata0 as read 
// by Plot1_ReadParamFile()...
  int n_datafiles, n_pfile_param_max;
/*
* NB: declarations in the calling program of Plot1_ReadParamFile():
*  PLOT1_SETTINGS pset0[N_PFILE_PARAM_MAX], i.e., pset0[128]
*  PLOT1_FILE_DATA pfiledata0[N_PFILE_PARAM_MAX * N_DATA_FILE_MAX]
*                  i.e., pfiledata0[128 * 64]
*/
  PLOT1_SETTINGS pset0[128];
  PLOT1_FILE_DATA pfiledata0[128 * 64];

};

#endif
