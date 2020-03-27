/************************************************************
* Definitions for JLP_Plot1 class
*
* JLP
* Version 21/02/2019
************************************************************/
#ifndef _jlp_plot1_def_h
#define _jlp_plot1_def_h
// For compilers that support precompilation, includes "wx/wx.h".
#ifdef USE_WXWIDGETS
#include "wx/wxprec.h"
#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif
#endif

#define NFILES_MAX 8

typedef struct {
 char file_name[128];
// latex, ascii_cols 
 char file_format[16];
// L0,Black
 char file_plot_style[16];
 int file_col_x, file_col_y, file_col_xerr, file_col_yerr;
 } PLOT1_FILE_DATA;

typedef struct {
// Type: 0=int 1=float 2=bool 3=wxString 4=char[]
int type;
int ivalue;
float fvalue;
bool bvalue;
#ifdef USE_WXWIDGETS
wxString svalue;
#endif
char cvalue[128];
char keywd[32];
 } PLOT1_FILE_PARAM;

typedef struct {
  char plot_device[32];
// axis_type: linear, etc
  char axis_type[16]; 
// axis_settings: flags(0/1) for jlp_axis, xgrid, ygrid, xlog, ylog
  char axis_settings[64];
// Labels:
  char xlabel[64], ylabel[64], zlabel[64], title[64];
  char axis_limits[64];
  float font_size_expand;
 } PLOT1_SETTINGS;

#endif
