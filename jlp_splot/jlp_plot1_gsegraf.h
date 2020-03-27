/************************************************************************
* Routines used by the JLP_Plot1 routines (to read JLP plot1 parameter files)
*
* JLP
* Version 19/03/2019
*************************************************************************/
#include "jlp_plot1_def.h"  // PLOT1_FILE_... structures
#include "jlp_gsegraf.h"    // JLP_GSEG_InitializePlot

#ifdef __cplusplus
extern "C" {
#endif

int Plot1_PlotSettingsToGsegAxisData(PLOT1_SETTINGS pset0, 
                                     GSEG_AXIS_DATA *gseg_axdata0);

#ifdef __cplusplus
}
#endif
