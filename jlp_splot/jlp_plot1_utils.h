/************************************************************
* Routines used by the JLP_Plot1 routines (to read JLP plot1 parameter files) 
*
* JLP
* Version 22/02/2019
************************************************************/
#include "jlp_plot1_def.h"  // PLOT1_FILE_... structures

#ifdef __cplusplus
extern "C" {
#endif

int Plot1_ReadParamFile(char *plot_param_fname0, PLOT1_SETTINGS *pset0,
                         PLOT1_FILE_DATA *pfiledata0, int *n_pfiledata0,
                         int *n_pfile_param_max);
int Plot1_ReadPlot1SettingsFromParamFile(char *plot_param_fname0, 
                         PLOT1_SETTINGS *pset0, PLOT1_FILE_PARAM *settings_par0,
                         int n_settings_par0);
int Plot1_ReadPlot1FileDataFromParamFile(char *plot_param_fname0,
                         PLOT1_FILE_DATA *pdat0, int n_data_files,
                         PLOT1_FILE_PARAM *data_fpar0, int n_data_fpar0);
int Plot1_get_n_data_files(char *plot_param_fname0, int *n_data_files);
int Plot1_decode_line_param(char *in_line, PLOT1_FILE_PARAM *pfpar0,
                            int n_pfpar0, int *index);
void Plot1_InitPlot1Settings(PLOT1_SETTINGS *pset0, char *plot_device0);
void Plot1_InitPlot1FileData(PLOT1_FILE_DATA *plt_fdata0, int n_data_files);
void Plot1_InitParamFromPlot1Settings(PLOT1_FILE_PARAM *pfpar0, int *n_pfpar0,
                                      PLOT1_SETTINGS pset0);
void Plot1_InitParamFromPlot1FileData(PLOT1_FILE_PARAM *pfpar0, int *n_pfpar0,
                                      PLOT1_FILE_DATA *pfiledata0,
                                      int n_pfiledata0);
void Plot1_Plot1SettingsFromFileParam(PLOT1_SETTINGS *pset0,
                                      PLOT1_FILE_PARAM *pfpar0, int n_pfpar0,
                                      int index);
void Plot1_Plot1FileDataFromFileParam(PLOT1_FILE_DATA *pfdat0, int n_pfdat0,
                                      PLOT1_FILE_PARAM *pfpar0, int n_pfpar0,
                                      int index);
int Plot1_ReadLatexTableFileToFloat(char *in_latex_fname, int icol_x, 
                                    int icol_y, int icol_xerr, int icol_yerr,
                                    float **x_plot, float **y_plot,
                                    float **xerr_plot, float **yerr_plot,
                                    int *npts, int *error_bars);

#ifdef __cplusplus
}
#endif

