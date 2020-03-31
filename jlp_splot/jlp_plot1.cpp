/************************************************************
* JLP_Plot1 class
*
* JLP
* Version 21/02/2019
************************************************************/
#include <string.h> // strcpy()
#include "jlp_plot1_def.h"  // PLOT1_FILE_DATA structure 
#include "jlp_plot1.h"  // JLP_GDev class
#include "jlp_plot1_utils.h"  // Plot1_ReadParamFile() 

// Constructor:
JLP_Plot1::JLP_Plot1(char *plot_param_fname){

/*
int Plot1_ReadParamFile(char *plot_param_fname0, PLOT1_SETTINGS *pset0,
                         PLOT1_FILE_DATA *pfiledata0, int *n_pfiledata0,
                         int *n_pfile_param_max)
*/
// n_pfile_param_max: size of the lines of the pfiledata0 as read 
// by Plot1_ReadParamFile()...

   Plot1_ReadParamFile(plot_param_fname, pset0, pfiledata0, &n_datafiles,
                       &n_pfile_param_max);

   }

