/************************************************************
* Routines used by the JLP_Plot1 routines (to read JLP plot1 parameter files)
*
* JLP
* Version 21/02/2019
************************************************************/
#include <stdio.h> 
#include <stdlib.h> // exit() 
#include <string.h> // strcpy()
#include "jlp_plot1_def.h"    // PLOT1_FILE_DATA structure 
#include "jlp_plot1_utils.h"  // prototypes of the routines defined here 
#include "jlp_read_keywd.h"  // jlp_read_keywd_in_keywd() 

// #define DEBUG 1
#define N_PFILE_PARAM_MAX 128 // max number of parameters in PLOT1_FILE_PARAM array (also max nber of parameters in PLOT1_SETTINGS)
#define N_DATA_FILE_MAX 64 // max number of data files 

/*************************************************************************
* Read the plot parameter file
*
* INPUT:
*  plot_param_fname0: name of the plot parameter file
*
* OUTPUT:
*  pset0: PLOT1_SETTINGS structure array that was read from the plot 
*         parameter file (size: n_psettings_max (= n_pfile_param_max)) 
*  pfiledata0: PLOT1_FILE_DATA structure array read from that was read from the
*              plot parameter file (size: n_pfile_param_max * n_pfiledata0)
*  n_pfiledata0: number of data files in pfiledata0 array
*  n_pfile_param_max: max number of elements per data file in pfiledata0
*
* NB: declarations in the calling program:
*  PLOT1_SETTINGS pset0[N_PFILE_PARAM_MAX], i.e., pset0[128]
*  PLOT1_FILE_DATA pfiledata0[N_PFILE_PARAM_MAX * N_DATA_FILE_MAX]
*                  i.e., pfiledata0[128 * 64]
*************************************************************************/
int Plot1_ReadParamFile(char *plot_param_fname0, PLOT1_SETTINGS *pset0,
                         PLOT1_FILE_DATA *pfiledata0, int *n_pfiledata0,
                         int *n_pfile_param_max)
 {
 int status = -1, n_settings_fpar0 = 0, n_data_fpar0 = 0;
 char plot_device0[128];
 PLOT1_FILE_PARAM data_fpar0[N_PFILE_PARAM_MAX]; 
 PLOT1_FILE_PARAM settings_fpar0[N_PFILE_PARAM_MAX];

#ifdef DEBUG
 printf("Plot1_ReadParamFile/plot_param_fname0 = %s\n", plot_param_fname0);
#endif

 *n_pfile_param_max = N_PFILE_PARAM_MAX;

// Init plot1 settings 
 strcpy(plot_device0, "&square");
 Plot1_InitPlot1Settings(pset0, plot_device0);
 Plot1_InitParamFromPlot1Settings(settings_fpar0, &n_settings_fpar0, *pset0);
 Plot1_ReadPlot1SettingsFromParamFile(plot_param_fname0, pset0, settings_fpar0,
                                      n_settings_fpar0);

 *n_pfiledata0 = 0;
 status = Plot1_get_n_data_files(plot_param_fname0, n_pfiledata0);
#ifdef DEBUG
printf("Plot1_ReadParamFile/number of input data files: %d\n", *n_pfiledata0);
#endif
 if(*n_pfiledata0 >= N_DATA_FILE_MAX ) {
   fprintf(stderr, "Plot1_ReadParamFile/Error: too many data files: n_pfiledata0=%d > max=%d\n",
          *n_pfiledata0, N_DATA_FILE_MAX);
   return(-1);
   }
 else if(*n_pfiledata0 <= 0 ) {
   fprintf(stderr, "Plot1_ReadParamFile/Error: no data file: n_pfiledata0=%d\n",
          *n_pfiledata0);
   return(-1);
 }

 Plot1_InitPlot1FileData(pfiledata0, *n_pfiledata0);
 Plot1_InitParamFromPlot1FileData(data_fpar0, &n_data_fpar0, pfiledata0, 
                                  *n_pfiledata0);

/*
int Plot1_ReadPlot1FileDataFromParamFile(char *plot_param_fname0,
                         PLOT1_FILE_DATA *pdat0, int n_data_files,
                         PLOT1_FILE_PARAM *data_fpar0, int n_data_fpar0)
*/
 Plot1_ReadPlot1FileDataFromParamFile(plot_param_fname0, pfiledata0, 
                                      *n_pfiledata0, data_fpar0,
                                      n_data_fpar0);



 return(status);
 }
/*************************************************************************
* Read the PLOT1_SETTINGS structure from the plot parameter file
* using a PLOT_FILE_PARAM structure containing the keywords used for reading 
*
* INPUT:
*  plot_param_fname0: name of the plot parameter file
*
* INPUT/OUTPUT:
*  settings_fpar0 : PLOT_FILE_PARAM structure containing the keywords 
*                  used for reading 
*  n_settings_fpar0 : number of elements in the settings_fpar0 structure
*
* OUTPUT:
*  pset0: PLOT1_SETTINGS structure  that was read from the input parameter file
*************************************************************************/
int Plot1_ReadPlot1SettingsFromParamFile(char *plot_param_fname0, 
                         PLOT1_SETTINGS *pset0, PLOT1_FILE_PARAM *settings_fpar0,
                         int n_settings_fpar0)
{
int status = -1, iparam;
char buffer[128];
FILE *fp;

// Open the plot parameter file:
 if((fp = fopen(plot_param_fname0, "r")) != NULL) {
   while(!feof(fp)) {
     if(fgets(buffer, 128, fp) != NULL) {
       Plot1_decode_line_param(buffer, settings_fpar0, n_settings_fpar0, 
                               &iparam);
// Copy those values to the PLOT1_SETTINGS structure:
       if(iparam != -1) Plot1_Plot1SettingsFromFileParam(pset0, settings_fpar0, 
                                                  n_settings_fpar0, iparam);
     }
   }
// "file_name"
   fclose(fp);
   status = 0;
 } else {
   fprintf(stderr, "Plot1_ReadPlot1SettingsFromParamFile/Fatal error opening in parameter file: %s\n",
           plot_param_fname0);
 }

return(status);
}
/*************************************************************************
* Read the PLOT1_FILE_DATA structure from the plot parameter file
* using a PLOT_FILE_PARAM structure containing the keywords used for reading
*
* INPUT:
*  plot_param_fname0: name of the plot parameter file
*  n_data_files: number of plot data files
*
* INPUT/OUTPUT:
*  data_fpar0 : PLOT_FILE_PARAM structure array containing the keywords used
*               for reading (PLOT1_FILE_PARAM data_fpar0[N_PFILE_PARAM_MAX])
*  n_data_fpar0 : number of significant elements per line in the n_data_fpar0 
*                structure
*
* OUTPUT:
*  pdat0: PLOT1_FILE_DATA structure array that was read from the input parameter file
*************************************************************************/
int Plot1_ReadPlot1FileDataFromParamFile(char *plot_param_fname0,
                         PLOT1_FILE_DATA *pdat0, int n_data_files,
                         PLOT1_FILE_PARAM *data_fpar0, int n_data_fpar0)
{
int status = -1, ifile, iparam;
char buffer[128], fname_str[64];
FILE *fp;

strcpy(fname_str, "file_name");

// Open the plot parameter file:
 ifile = -1;
 if((fp = fopen(plot_param_fname0, "r")) != NULL) {
   while(!feof(fp)) { 
     if(fgets(buffer, 128, fp) != NULL) {
      if(buffer[0] != '#' && buffer[0] != '%') {
// Look for "file_name" in the buffer and then determine the iparam index:
      if(strstr(buffer, fname_str) != NULL) ifile++;
      if(ifile >= 0 && ifile < n_data_files) {
        Plot1_decode_line_param(buffer, data_fpar0, n_data_fpar0,
                                &iparam);
// Copy this value to the PLOT1_FILE_DATA structure:
        if(iparam != -1) Plot1_Plot1FileDataFromFileParam(pdat0, ifile, 
                                               data_fpar0, n_data_fpar0, iparam);
        }
      }
     }
   }
// "file_name"
   fclose(fp);
   status = 0;
 } else {
   fprintf(stderr, "Plot1_ReadPlot1SettingsFromParamFile/Fatal error opening in parameter file: %s\n",
           plot_param_fname0);
 }

return(status);
}
/***************************************************************************
*
* INPUT:
*  in_line: 
*  *pfpar0: pointer to the PLOT1_FILE_PARAM structure
*  n_pfpar0: number of parameters
* 
* OUTPUT:
* iparam: index of the PLOT1_FILE_PARAM structure parameter that was found in 
*        the input line (-1 if not found)
***************************************************************************/
int Plot1_decode_line_param(char *in_line, PLOT1_FILE_PARAM *pfpar0, 
                            int n_pfpar0, int *iparam)
{
int status, i, type, ivalue;
float fvalue;
bool bvalue;
char cvalue[128], ccvalue[128], keywd[128], *pc;

status = -1;
*iparam = -1;
for(i = 0; i < n_pfpar0 && status != 0; i++) {
  type = pfpar0[i].type;
  strcpy(keywd, pfpar0[i].keywd);
// Type: 0=int 1=float 2=bool 3=wxString 4=char[]
  switch(type) {
    case 0:
      status = jlp_read_keywd_int(in_line, keywd, &ivalue);
      if(status == 0) {
        *iparam = i;
        pfpar0[i].ivalue = ivalue;
#ifdef DEBUG
printf("DEBUG/decode_line i=%d key=%s ivalue=%d \n", i, keywd, ivalue);
#endif
        }
      break;
    case 1:
      status = jlp_read_keywd_float(in_line, keywd, &fvalue);
      if(status == 0) {
        *iparam = i;
        pfpar0[i].fvalue = fvalue;
#ifdef DEBUG
printf("DEBUG/decode_line: i=%d key=%s fvalue=%f \n", i, keywd, fvalue);
#endif
        }
      break;
    case 2:
      status = jlp_read_keywd_char(in_line, keywd, cvalue);
      if(status == 0) {
        if(cvalue[0] == 'n' || cvalue[0] == 'N') bvalue = false;
          else bvalue = true;
        *iparam = i;
        pfpar0[i].bvalue = bvalue;
#ifdef DEBUG
printf("DEBUG/decode_line: i=%d key=%s bvalue=%d \n", i, keywd, bvalue);
#endif
        }
      break;
#ifdef USE_WXWIDGETS
    case 3:
      status = jlp_read_keywd_char(in_line, keywd, cvalue);
      if(status == 0) {
        *iparam = i;
        pfpar0[i].svalue = wxString(cvalue);
#ifdef DEBUG
printf("DEBUG/decode_line: i=%d key=%s cvalue=%s \n", i, keywd, cvalue);
#endif
        }
      break;
#endif
    default:
    case 4:
      status = jlp_read_keywd_char(in_line, keywd, cvalue);
      if(status == 0) {
        *iparam = i;
// Remove " at the beginning and at the end, if present:
        if(cvalue[0] == '"') pc = &cvalue[1]; 
        else pc = &cvalue[0];
        strcpy(ccvalue, pc);
        pc = ccvalue;
        while(*pc && *pc != '"') pc++;
        *pc = '\0';
        strcpy(pfpar0[i].cvalue, ccvalue);
#ifdef DEBUG
printf("DEBUG/decode_line: i=%d key=%s cvalue=%s \n", i, keywd, cvalue);
#endif
        }
      break;
    }
}

return(status);
}
/*************************************************************************
* Read the plot parameter file and determine the number of input data files
*
* INPUT:
*  plot_param_fname0: name of the plot parameter file
*
* OUTPUT:
*  n_data_files: number of input plot data files (ascii, latex, etc)
*************************************************************************/
int Plot1_get_n_data_files(char *plot_param_fname0, int *n_data_files)
{
FILE *fp;
int status = -1, nfiles = 0;
char buffer[128], fname_str[32];

*n_data_files = 0;
strcpy(fname_str, "file_name");

// Open the plot parameter file:
 if((fp = fopen(plot_param_fname0, "r")) != NULL) {
   while(!feof(fp)) {
     if(fgets(buffer, 128, fp) != NULL) {
// Look for "file_name" in the buffer:
// Warning: comments start with % or # !
      if((buffer[0] != '#') && (buffer[0] != '%') &&
        (strstr(buffer, fname_str) != NULL)) {
       nfiles++;
       }
     }
   }
// "file_name"
   fclose(fp);
   status = 0;
 } else {
   fprintf(stderr, "Plot1_get_n_data_files/Fatal error opening in parameter file: %s\n",
           plot_param_fname0);
 } 

*n_data_files = nfiles;
#ifdef DEBUG
printf("Plot1_get_n_data_files/number of input data files: %d\n", *n_data_files);
#endif
return(status);
}
/*************************************************************************
* Initialize the PLOT1_SETTINGS structure
*
* INPUT:
*  plot_device0 : plot device name (e.g. "&square")
*  *pset0: pointer to the PLOT1_SETTINGS structure 
*
**************************************************************************/
void Plot1_InitPlot1Settings(PLOT1_SETTINGS *pset0, char *plot_device0)
{ 
   strcpy(pset0->plot_device, plot_device0);
// Axis type:
   strcpy(pset0->axis_type, "linear");
// axis_settings: flags(0/1) for jlp_axis, xgrid, ygrid, xlog, ylog
   strcpy(pset0->axis_settings, "");
// Labels
   strcpy(pset0->xlabel, "");
   strcpy(pset0->ylabel, "");
   strcpy(pset0->zlabel, "");
   strcpy(pset0->title, "");
// axis_limits: xmin, xmax, ymin, ymax
   strcpy(pset0->axis_limits, "0.,1.,0.,0,1.");
// Font size expand:
   pset0->font_size_expand = 1.2;
   return;
 }
/*************************************************************************
* Initialize the PLOT1_FILE_DATA structure
*
* INPUT:
*  *pset0: pointer to the PLOT1_SETTINGS structure 
*  n_pdata_files: number of plot data files 
*
**************************************************************************/
void Plot1_InitPlot1FileData(PLOT1_FILE_DATA *plt_fdata0, int n_pdata_files)
{ 
int k;

// Initialize the PLOT1_FILE_DATA structure:
  for(k = 0; k < n_pdata_files; k++) {
    strcpy(plt_fdata0[k].file_name, "");
    strcpy(plt_fdata0[k].file_name, "");
    strcpy(plt_fdata0[k].file_format, "ascii_cols");
    strcpy(plt_fdata0[k].file_plot_style, "L0,Black");
    plt_fdata0[k].file_col_x = 1;
    plt_fdata0[k].file_col_y = 2;
    plt_fdata0[k].file_col_xerr = 0;
    plt_fdata0[k].file_col_yerr = 0;
   }
return;
}
/*************************************************************************
* Initialize PLOT1_FILE_PARAM array to allow further reading 
* of the file containing the plot parameters
*
* 0=axis_type : "linear", "xlog", ...
* 1=xlabel
* 2=ylabel
* 3=zlabel
* 4=title
* 5=plot_device: & for gdev, $ for gsegraf
* 6=axis_limits: "-0.2 +0.2 -13 +13"
* 7=axis_settings: "jlp_axes,xgrid,ygrid,xlog,ylog"
* 8=font_size_expand
*
* INPUT:
*  pset0: PLOT1_SETTINGS structure used to set the pfpar[] array
*
* OUTPUT:
*  pfpar0[] : array of PLOT1_FILE_PARAM
*  n_pfpar0 : number of elements in pfpar[] array
**************************************************************************/
void Plot1_InitParamFromPlot1Settings(PLOT1_FILE_PARAM *pfpar0, int *n_pfpar0, 
                                      PLOT1_SETTINGS pset0)
{
int ifdat0, i, ii;

// Type: 0=int 1=float 2=bool 3=wxString 4=char[]

 *n_pfpar0 = 0;

// 0=axis_type : "linear", "xlog", ...
  pfpar0[0].type = 4;
  strcpy(pfpar0[0].cvalue, pset0.axis_type);
  strcpy(pfpar0[0].keywd, "axis_type");

// 1=xlabel 
  pfpar0[1].type = 4;
  strcpy(pfpar0[1].cvalue, pset0.xlabel);
  strcpy(pfpar0[1].keywd, "xlabel");

// 2=ylabel 
  pfpar0[2].type = 4;
  strcpy(pfpar0[2].cvalue, pset0.ylabel);
  strcpy(pfpar0[2].keywd, "ylabel");

// 3=zlabel 
  pfpar0[3].type = 4;
  strcpy(pfpar0[3].cvalue, pset0.ylabel);
  strcpy(pfpar0[3].keywd, "ylabel");

// 4=title 
  pfpar0[4].type = 4;
  strcpy(pfpar0[4].cvalue, pset0.title);
  strcpy(pfpar0[4].keywd, "title");

// 5=plot_device: & for gdev, $ for gsegraf 
  pfpar0[5].type = 4;
  strcpy(pfpar0[5].cvalue, pset0.plot_device);
  strcpy(pfpar0[5].keywd, "plot_device");

// 6=axis_limits: "-0.2 +0.2 -13 +13" 
  pfpar0[6].type = 4;
  strcpy(pfpar0[6].cvalue, pset0.axis_limits);
  strcpy(pfpar0[6].keywd, "axis_limits");

// 7=axis_settings: "jlp_axes,xgrid,ygrid,xlog,ylog"
  pfpar0[7].type = 4;
  strcpy(pfpar0[7].cvalue, pset0.axis_settings);
  strcpy(pfpar0[7].keywd, "axis_settings");

// 8=font_size_expand 
  pfpar0[8].type = 1;
  pfpar0[8].fvalue = pset0.font_size_expand;
  strcpy(pfpar0[8].keywd, "font_size_expand");

 *n_pfpar0 = 9;
return;
}
/*************************************************************************
* Load Plot1 settings from PLOT1_FILE_PARAM array
*
* INPUT:
*  pfpar0[] : array of PLOT1_FILE_PARAM
*  n_pfpar0 : number of elements in pfpar[] array
*  iparam : iparam of the element to be used for copying
*
* OUTPUT:
*  pset0: PLOT1_SETTINGS structure used to set the pfpar[] array
**************************************************************************/
void Plot1_Plot1SettingsFromFileParam(PLOT1_SETTINGS *pset0, 
                                      PLOT1_FILE_PARAM *pfpar0, int n_pfpar0,
                                      int iparam)
{

// Type: 0=int 1=float 2=bool 3=wxString 4=char[]
/*
DEBUG/decode_line: i=0 key=axis_type cvalue="linear" 
DEBUG/decode_line: i=6 key=axis_limits cvalue="-0.2 +0.2 -13 +13" 
DEBUG/decode_line: i=1 key=xlabel cvalue="\Delta \\rho _{(O-C)} (arcsec)"  
DEBUG/decode_line: i=2 key=ylabel cvalue="\Delta \\theta _{(O-C)} (deg.)"  
DEBUG/decode_line: i=5 key=plot_device cvalue="&square" 
*/

/*
* 0=axis_type : "linear", "xlog", ...
* 1=xlabel
* 2=ylabel
* 3=zlabel
* 4=title
* 5=plot_device: & for gdev, $ for gsegraf
* 6=axis_limits: "-0.2 +0.2 -13 +13"
* 7=axis_settings: "jlp_axes,xgrid,ygrid,xlog,ylog"
* 8=font_size_expand
*/

 switch(iparam) {
// 0=axis_type : "linear", "xlog", ...
  case 0:
    strcpy(pset0->axis_type, pfpar0[0].cvalue);
    break;
// 1=xlabel 
  case 1:
    strcpy(pset0->xlabel, pfpar0[1].cvalue);
    break;
// 2=ylabel 
  case 2:
    strcpy(pset0->ylabel, pfpar0[2].cvalue);
    break;
// 3=zlabel 
  case 3:
    strcpy(pset0->zlabel, pfpar0[3].cvalue);
    break;
// 4=title
  case 4:
    strcpy(pset0->title, pfpar0[4].cvalue);
    break;
// 5=plot_device: & for gdev, $ for gsegraf 
  case 5:
    strcpy(pset0->plot_device, pfpar0[5].cvalue);
    break;
// 6=axis_limits: "-0.2 +0.2 -13 +13" 
  case 6:
    strcpy(pset0->axis_limits, pfpar0[6].cvalue);
    break;
// 7=axis_settings: "jlp_axes,xgrid,ygrid,xlog,ylog"
  case 7:
    strcpy(pset0->axis_settings, pfpar0[7].cvalue);
    break;
// 8=font_size_expand 
  case 8:
    pset0->font_size_expand = pfpar0[8].fvalue;
    break;
  }

return;
}
/*************************************************************************
* Initialize PLOT1_FILE_PARAM array to allow further reading 
* of the file containing the plot parameters
*
* INPUT:
*  pfiledata0: array of PLOT1_FILE_DATA structures used to set the pfpar[] array
*  n_pfiledata0: number of data files in pfiledata0 array
*
* OUTPUT:
*  pfpar0[] : array of PLOT1_FILE_PARAM
*  n_pfpar0 : number of elements in pfpar[] array
**************************************************************************/
void Plot1_InitParamFromPlot1FileData(PLOT1_FILE_PARAM *pfpar0, int *n_pfpar0, 
                                      PLOT1_FILE_DATA *pfiledata0, 
                                      int n_pfiledata0)
{
int ifdat0 = 0, k;

 *n_pfpar0 = 0;

 if(n_pfiledata0 < 1) {
   fprintf(stderr, "Plot1_InitParamFromPlot1FileData/Fatal error: n_pfiledata0 is null !");
   exit(-1);
   }

// Type: 0=int 1=float 2=bool 3=wxString 4=char[]

// Use the first elements (k=0) of the PLOT1_FILE_DATA array to initialize 
// the PLOT1_FILE_PARAM structure
  k = 0;

  pfpar0[0].type = 4;
  strcpy(pfpar0[0].cvalue, pfiledata0[k].file_name);
  strcpy(pfpar0[0].keywd, "file_name");

// file_format: "latex" "ascii_cols"
  pfpar0[1].type = 4;
  strcpy(pfpar0[1].cvalue, pfiledata0[k].file_format);
  strcpy(pfpar0[1].keywd, "file_format");

// file_col_x
  pfpar0[2].type = 0;
  pfpar0[2].ivalue = pfiledata0[k].file_col_x;
  strcpy(pfpar0[2].keywd, "file_col_x");

// file_col_y
  pfpar0[3].type = 0;
  pfpar0[3].ivalue = pfiledata0[k].file_col_y;
  strcpy(pfpar0[3].keywd, "file_col_y");

// file_col_xerr
  pfpar0[4].type = 0;
  pfpar0[4].ivalue = pfiledata0[k].file_col_xerr;
  strcpy(pfpar0[4].keywd, "file_col_xerr");

// file_col_yerr
  pfpar0[5].type = 0;
  pfpar0[5].ivalue = pfiledata0[k].file_col_yerr;
  strcpy(pfpar0[5].keywd, "file_col_yerr");

// plot_style: "L0", "92", "92,Black", etc 
  pfpar0[6].type = 4;
  strcpy(pfpar0[6].cvalue, pfiledata0[k].file_plot_style);
  strcpy(pfpar0[6].keywd, "file_plot_style");

// Number of elements in pfpar0[]:
*n_pfpar0 = 7;

return;
}
/*************************************************************************
* Load PLOT1_FILE_DATA values of index=ifile) from PLOT1_FILE_PARAM structure 
*
* INPUT:
*  pfpar0[] : array of PLOT1_FILE_PARAM
*  ifile : index in pfpar[] array
*  iparam : index of the element used for copying
*
* OUTPUT:
*  pset0: PLOT1_SETTINGS structure used to set the pfpar[] array
**************************************************************************/
void Plot1_Plot1FileDataFromFileParam(PLOT1_FILE_DATA *pfiledata0, 
                                      int ifile,
                                      PLOT1_FILE_PARAM *pfpar0, int n_pfpar0,
                                      int iparam)
{

// Type: 0=int 1=float 2=bool 3=wxString 4=char[]
 switch(iparam) {
  case 0:
// file_name 
    strcpy(pfiledata0[ifile].file_name, pfpar0[0].cvalue);
    break;
// file_format: "latex" "ascii_cols"
  case 1:
    strcpy(pfiledata0[ifile].file_format, pfpar0[1].cvalue);
    break;
// file_col_x
  case 2:
    pfiledata0[ifile].file_col_x = pfpar0[2].ivalue;
    break;
  case 3:
    pfiledata0[ifile].file_col_y = pfpar0[3].ivalue;
    break;
  case 4:
    pfiledata0[ifile].file_col_xerr = pfpar0[4].ivalue;
    break;
  case 5:
    pfiledata0[ifile].file_col_yerr = pfpar0[5].ivalue;
    break;
  case 6:
// plot_style: "L0", "92", "92,Black", etc
    strcpy(pfiledata0[ifile].file_plot_style, pfpar0[6].cvalue);
    break;
 }

return;
}
