/******************************************************************************
* JLP_GsegData class
*
* JLP
* Version 27/03/2017
*******************************************************************************/
#ifndef _jlp_gseg_data_h_
#define _jlp_gseg_data_h_

#include "jlp_gsegraf.h"

#define MAX_NSHAPES 64 

// Plot parameters for lines
typedef struct{
 double coords[6];
 UINT32 line_color;
 char line_stylechar;
 unsigned line_width;
} LINE1;

// Plot parameters for ellipses
typedef struct{
 double x0, y0, width, height, angle;
 UINT32 line_color;
 char line_stylechar;
 unsigned line_width;
} ELLIPSE1;

// Plot parameters for rectangles
typedef struct{
 double x0, y0, width, height, angle;
 UINT32 line_color;
 char line_stylechar;
 unsigned line_width;
} RECTANGLE1;

// Plot parameters for symbols
typedef struct{
 double coords[6];
 UINT32 symbol_color;
 char symbol_stylechar;
 unsigned symbol_size;
} SYMBOL1;

// Plot parameters for text items 
typedef struct{
 char *text_str; 
 char anchor_text[64]; 
 char coords_flag[4];
 double x_text, y_text, z_text;
 int nlines;
} TEXT_ITEM1;

class JLP_GsegData {
public:

// in "jlp_GsegData.cpp"
 JLP_GsegData(JLP_Gseg *jlp_gseg0, JLP_Gsegraf *jlp_gsegraf0);
 JLP_GsegData(JLP_Gseg *jlp_gseg0, JLP_Gsegraf *jlp_gsegraf0,
              GSEG_PLOT_DATA *gseg_pltdata0, const int nplots0);

// Desctructor:
 ~JLP_GsegData(){ };

// in "jlp_GsegData.cpp"
 void InitDataParameters();
 void InitializeDataVariables();
 int InitDataArraysAndOptions(const int nplots);
 int InitOptionsFromFile(char *p_param_filename);
 void Get_nplots(int *nplots0);
 int Get_ncontour_plots(int *ncontour_plots0);
 void Get_ncolor_plots(int *ncolor_plots0);
 int GetImageArray(double **array0, int *nx0, int *ny0);

// in "jlp_GsegData_ReadDataParamFile.cpp"
 int SetPlotDataParamsFromGsegPlotData(GSEG_PLOT_DATA *gseg_pltdata0,
                                       const int nplots0);
 int ReadComplementoryDataFromFile(char *param_filename, int nplots0,
                                   int ncoords0, char *axis_type0);
 int ReadFileParamsFromFile(char *p_param_filename, int *nplots0);
 int ReadPlotTypesFromFile(char *p_param_filename);
 int ReadPlotColorSettingsFromFile(char *param_filename);
 int ReadPlotStylesFromFile(char *p_param_filename, char *axis_type0);

// in "jlp_GsegData_CheckParameters.cpp"
 int CheckDataFilenames();
 int CheckHistogramOptions();
 int CheckPlotType(const char *axis_type0);
 int CheckStyleFlag(const char *axis_type0);
 int CheckStyleChar(const char *axis_type0);
 int CheckHistogramData(const char *axis_type0);

// jlp_GsegData_ReadExtraImagePlotSettings.cpp
 int GetExtraImagePlotSettings(char *extra_image_filename0, 
                               char *extra_image_coords_flag0, 
                               char *extra_image_anchor_text0, 
                               double *x_extra_image0, 
                               double *y_extra_image0, double *z_extra_image0);
 int ReadExtraImagePlotSettingsFromFile(char *param_filename, char *axis_type0); 

// jlp_GsegData_ReadPlotDataFiles.cpp
 int LoadCurvePlotDataFromFile(char *filename0, const int reset_first0,
                               char *axis_type0);
 int LoadImagePlotDataFromFitsFile(char *filename0, const int reset_first0,
                               char *axis_type0);

// jlp_GsegData_Read2dFiles.cpp
 void Read2dFiles(char *axis_type0);

// jlp_GsegData_Read3dFiles.cpp
 void Read3dFiles ( void );

// jlp_GsegData_DataMinMax.cpp
 void DataMinMax (const int high_contrast);
 int ReadDataMinMaxFromFile(char *param_filename0, char *axis_type0);
 int ReadSymbolMinMaxFromFile(char *param_filename0, char *axis_type0);
 int DataMinMax_ApplyFileSettings();
 void GetDataMinMax(double *data_xmin0, double *data_xmax0, double *data_ymin0,
                    double *data_ymax0, double *data_zmin0, double *data_zmax0);
 void SetDataMinMax(const double data_xmin0, const double data_xmax0,
                    const double data_ymin0, const double data_ymax0,
                    const double data_zmin0, const double data_zmax0);

// jlp_GsegData_DataMinMax3d.cpp
 void DataMinMax3d (const int high_contrast);
 int ReadDataMinMax3dFromFile(char *param_filename0);
 int ReadSymbolMinMax3dFromFile(char *param_filename0);

// jlp_GsegData_CreateContourLabel.cpp
 int CreateContourLabel(int icontourplot, double x_user, double y_user,
                        double *zcontour_data, const double zmin0,
                        const double zmax0, double *labelled_contour_value);

// jlp_GsegData_ComputeHistogram.cpp
 void ComputeHistogram(int iplot, double **yhist0, int *nbins, 
                       int *npts, double *binmin, double *binmax, 
                       double *binwidth, char *histo_type0);

// jlp_GsegData_ReadExtraLines.cpp
 int GetExtraLineData(double *line_coords, UINT32 *line_color, 
                      char *line_char, unsigned int *line_width, int iline);
 void ReadExtraLinesFromFile(char *param_filename, const int ncoords0);

// jlp_GsegData_ReadExtraEllipses.cpp
 int GetExtraEllipseData(double *x0, double *y0, double *width0, 
                         double *height0, double *angle0, UINT32 *line_color, 
                         char *line_stylechar, unsigned int *line_width, 
                         int iell);
 void ReadExtraEllipsesFromFile(char *param_filename);

// jlp_GsegData_ReadExtraRectangles.cpp
 int GetExtraRectangleData(double *x0, double *y0, double *width0, 
                         double *height0, double *angle0, UINT32 *line_color, 
                         char *line_stylechar, unsigned int *line_width, 
                         int iell);
 void ReadExtraRectanglesFromFile(char *param_filename);

// jlp_GsegData_ReadExtraSymbols.cpp
 int GetExtraSymbolData(double *symbol_coords, UINT32 *symbol_color, 
                        char *symbol_stylechar, unsigned int *symbol_size, 
                        int isymb);
 void ReadExtraSymbolsFromFile(char *param_filename, const int ncoords0);

// jlp_GsegData_ReadTheLegend.cpp
 int GetLegendData(char **legend_str, char *anchor_text, double *xlegend, 
                   double *ylegend, double *zlegend, char *legend_coords_flag,
                   int *nlines);
 void ReadTheLegendFromFile(char *param_filename, char *axis_type0);

// jlp_GsegData_ReadExtraText.cpp
 int GetExtraTextData(char **text_str, char *anchor_text, double *xtext,
                   double *ytext, double *ztext, int *nlines, 
                   char *text_coords_flag, const int itext);
 void ReadExtraTextFromFile(char *param_filename, char *axis_type0);

// Types and styles:
// in "jlp_GsegData_PlotStyles.cpp"
int GetGsegPlotType(const int iplot0, int *gseg_plot_type0);
int GetStyleFlag(const int iplot0, int *styleflag0);
int GetStyleSize(const int iplot0, unsigned int *stylesize0);
int GetStyleChar1(const int iplot0, char *stylechar10);
int GetStyleChar2(const int iplot0, char *stylechar20);
int GetStyleColor1(const int iplot0, UINT32 *stylecolor10);
int GetStyleColor2(const int iplot0, UINT32 *stylecolor20);
int GetOutlineColor(const int iplot0, UINT32 *outlinecolor0);
int GetFillColor(const int iplot0, UINT32 *fillcolor0);
int SetOutlineAndFillColorFromStyleColor(const int iplot0, 
                                          UINT32 stylecolor2_0,
                                          char stylechar1_0);
int SetOutlineAndFillColorFromStyleChars(const int iplot0, 
                                          UINT32 *stylecolor2_0, 
                                          char stylechar1_0, char stylechar2_0);
int GetZBlackWhite(const int iplot, double *zblack_0,
                   double *zwhite_0);
int SetZBlackWhite(const int iplot, const double zblack_0,
                   const double zwhite_0);
int GetAlphaColor(const int iplot, UINT32 *alpha_color0);
int SetAlphaColor(const int iplot, const UINT32 alphacolor_0);
int GetContour3dColor(const int iplot, UINT32 *contour3d_color0);
int GetMeshColor(const int iplot, UINT32 *mesh_color0);
int DrawSymbolOptionFromChar(const char stylechar0, const int isymb,
                             int *ifunc0, int *is_upper_case0);
int DrawSymbolOptionFromStyleChar1(const int iplot, int *ifunc);
int DrawSymbol1OptionFromStyleChar1(const int iplot, int *ifunc);
int DrawSymbol2OptionFromStyleChar1(const int iplot, int *ifunc);
int GetStyleColorFromStyleChar(char stylechar_0, UINT32 *style_color0);
int GetStyleColorFromStyleChar2(const int iplot0, UINT32 *style_color0);


// Accessors:
/*****************************************************************************
* Get number of contour or color plots
*****************************************************************************/

// Contours:
int NContours(const int iplot) {return(gseg_plotdata1[iplot].ncontours); }
int NXContour(const int iplot) {return(gseg_plotdata1[iplot].nxcontour); }
int NYContour(const int iplot) {return(gseg_plotdata1[iplot].nycontour); }
double XContour(const int iplot, const int i) {
    return(gseg_plotdata1[iplot].xcontour[i]); 
    }
double YContour(const int iplot, const int i) {
    return(gseg_plotdata1[iplot].ycontour[i]); 
    }
double ZContour(const int iplot, const int i) {
    return(gseg_plotdata1[iplot].zcontour[i]); 
    }
double* XContourPtr(const int iplot) {
    return(gseg_plotdata1[iplot].xcontour); 
    }
double* YContourPtr(const int iplot) {
    return(gseg_plotdata1[iplot].ycontour); 
    }
double* ZContourPtr(const int iplot) {
    return(gseg_plotdata1[iplot].zcontour); 
    }

// x, y, z color:
int NXColor(const int iplot) {return(gseg_plotdata1[iplot].nxcolor); }
int NYColor(const int iplot) {return(gseg_plotdata1[iplot].nycolor); }
double XColor(const int iplot, const int i) {
    return(gseg_plotdata1[iplot].xcolor[i]); 
    }
double YColor(const int iplot, const int i) {
    return(gseg_plotdata1[iplot].ycolor[i]); 
    }
double ZColor(const int iplot, const int i) {
    return(gseg_plotdata1[iplot].zcolor[i]); 
    }
double* XColorPtr(const int iplot) {
    return(gseg_plotdata1[iplot].xcolor); 
    }
double* YColorPtr(const int iplot) {
    return(gseg_plotdata1[iplot].ycolor); 
    }
double* ZColorPtr(const int iplot) {
    return(gseg_plotdata1[iplot].zcolor); 
    }

// x,y,z data:
int NPts(const int iplot) {return(gseg_plotdata1[iplot].npts); }
double XData(const int iplot, const int i) {
    return(gseg_plotdata1[iplot].xdata[i]); }
double YData(const int iplot, const int i) {
    return(gseg_plotdata1[iplot].ydata[i]); }
double ZData(const int iplot, const int i) {
    return(gseg_plotdata1[iplot].zdata[i]); }
double* XDataPtr(const int iplot, const int i) {
    return(&(gseg_plotdata1[iplot].xdata[i])); }
double* YDataPtr(const int iplot, const int i) {
    return(&(gseg_plotdata1[iplot].ydata[i])); }
double* ZDataPtr(const int iplot, const int i) {
    return(&(gseg_plotdata1[iplot].zdata[i])); }

// x, y, z mesh
int NXMesh(const int iplot) {return(gseg_plotdata1[iplot].nxmesh); }
int NYMesh(const int iplot) {return(gseg_plotdata1[iplot].nymesh); }
double* XMeshPtr(const int iplot) {
    return(gseg_plotdata1[iplot].xmesh); 
    }
double* YMeshPtr(const int iplot) {
    return(gseg_plotdata1[iplot].ymesh); 
    }
double* ZMeshPtr(const int iplot) {
    return(gseg_plotdata1[iplot].zmesh); 
    }

int NInterp(const int iplot) {return(gseg_plotdata1[iplot].ninterp); }

// linebreaks:
int NLinebreaks(const int iplot) {return(gseg_plotdata1[iplot].nlinebreaks); }
int Linebreak(const int iplot, const int i) {
  int ivalue = 0;
  if(i >= 0 && i < gseg_plotdata1[iplot].nlinebreaks) { 
     ivalue = gseg_plotdata1[iplot].linebreak[i]; 
     }
  return(ivalue);
  }

// Extra  data:
int NExtraLines() {return(n_extra_lines1); }
int NExtraEllipses() {return(n_extra_ellipses1); }
int NExtraRectangles() {return(n_extra_rectangles1); }
int NExtraSymbols() {return(n_extra_symbols1); }
int NExtraTextItems() {return(n_text_items1); }
int NLegendLines() {return(n_legend_lines1); }

int color_rgba(const int i, UINT32 *color0){
  int status = -1;
   *color0 = 0;
  if(i > 0 && i < 17) {
   *color0 = color_rgba1[i];
   status = 0;
   }
  return(status);
  };

private:
 JLP_Gseg *jlp_gseg1;
 JLP_Gsegraf *jlp_gsegraf1;
 int gsegdata_from_paramfile;

 data_min_max_type  *p_data_min_max, *fromfile_data_min_max;
 int nplots1;

// Basic data used for plotting:
 GSEG_PLOT_DATA gseg_plotdata1[NPLOTS_MAXI];

// Data for the extra lines:
 LINE1 extra_lines1[MAX_NSHAPES];
 int n_extra_lines1;

// Data for the extra ellipses:
 ELLIPSE1 extra_ellipses1[MAX_NSHAPES];
 int n_extra_ellipses1;

// Data for the extra rectangles:
 RECTANGLE1 extra_rectangles1[MAX_NSHAPES];
 int n_extra_rectangles1;

// Data for the extra symbols:
 SYMBOL1 extra_symbols1[MAX_NSHAPES];
 int n_extra_symbols1;

// Data for the extra text items:
 TEXT_ITEM1 text_items1[MAX_NSHAPES];
 int n_text_items1;

// Data for the legend:
 char *legend_str1, legend_anchor_text1[64], legend_coords_flag1[4];
 double xlegend1, ylegend1, zlegend1;
 int n_legend_lines1;

// Extra image parameters:
 char extra_image_filename1[128], extra_image_anchor_text1[64];
 char extra_image_coords_flag1[4];
 double x_extra_image1, y_extra_image1, z_extra_image1;

// string_get is used for reading lines (see gseg_get_string())
 char *string_get;

// Color variables and pointers for colors specified by color characters
 UINT32 color_rgba1[17]; 

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
