/*****************************************************************************
* jlp_gsegraf.cpp
* JLP_Gsegraf class
*
* JLP
* Version 14/12/2016
*****************************************************************************/

#include "jlp_gsegraf.h" // JLP_Gsegraf

/****************************************************************************
* Constructors:
*****************************************************************************/
JLP_Gsegraf::JLP_Gsegraf(JLP_Gseg *jlp_gseg0, 
                         char *parameter_file_0, char **save_filename_0,
                         int *close_flag_0)
{
char font_name0[64];
double font_size_date_time0, font_size_legend0, font_size_text0;
double font_size_tick_labels0, font_size_axis_labels0, font_size_title0;

strcpy(font_name0, "Sans");
// For safety:
jlp_gseg1 = NULL;

 GSEG_InitializePlot(jlp_gseg0, parameter_file_0, save_filename_0, close_flag_0,
                     font_name0, &font_size_date_time0,
                     &font_size_legend0, &font_size_text0,
                     &font_size_tick_labels0, &font_size_axis_labels0,
                     &font_size_title0);
}
/****************************************************************************
* Constructor:
*****************************************************************************/
JLP_Gsegraf::JLP_Gsegraf(JLP_Gseg *jlp_gseg0, 
                         char *parameter_file_0, char **save_filename_0,
                         int *close_flag_0, char *font_name0,
                         double *font_size_date_time0, 
                         double *font_size_legend0,
                         double *font_size_text0, 
                         double *font_size_tick_labels0,
                         double *font_size_axis_labels0, 
                         double *font_size_title0)
{
// For safety:
jlp_gseg1 = NULL;

 GSEG_InitializePlot(jlp_gseg0, parameter_file_0, save_filename_0, close_flag_0,
                     font_name0, font_size_date_time0,
                     font_size_legend0, font_size_text0,
                     font_size_tick_labels0, font_size_axis_labels0,
                     font_size_title0);
}
