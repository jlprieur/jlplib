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

// Desctructor:
 ~JLP_GsegData(){ };

// in "jlp_GsegData.cpp"
 void InitDataParameters();
 void InitializeDataVariables();
 int InitDataArraysAndOptions(char *p_param_filename, const int nplots);
 void Get_nplots(int *nplots0);
 void Get_ncontours(int *ncontours0);
 int Get_styleflag(int *styleflag0, const int iplot0);
 int Get_plot_type(char *plot_type0, const int iplot0);
 int Get_ncontour_plots(int *ncontour_plots0);
 void Get_ncolor_plots(int *ncolor_plots0);

// in "jlp_GsegData_ReadDataParamFile.cpp"
 int ReadFileParamsFromFile(char *p_param_filename, int *nplots0);
 int ReadPlotTypesFromFile(char *p_param_filename);
 int ReadPlotStylesFromFile(char *p_param_filename, char *axis_type0);

// in "jlp_GsegData_CheckParameters.cpp"
 int CheckDataFilenames();
 int CheckHistogramOptions();
 int CheckPlotTypes(const char *axis_type0);

// jlp_GsegData_ReadImagePlotSettings.cpp
 int GetImagePlotSettings(char *image_filename0, char *image_coords_flag0, 
                          char *image_anchor_text0, double *x_image0, 
                          double *y_image0, double *z_image0);
 int ReadImagePlotSettingsFromFile(char *param_filename, char *axis_type0); 

// jlp_GsegData_Read2dFiles.cpp
 void Read2dFiles(char *axis_type0);

// jlp_GsegData_Read3dFiles.cpp
 void Read3dFiles ( void );

// jlp_GsegData_DataMinMax.cpp
 void DataMinMax ( void );
 int ReadDataMinMaxFromFile(char *param_filename0, char *axis_type0);
 int ReadSymbolMinMaxFromFile(char *param_filename0, char *axis_type0);
 int DataMinMax_ApplyFileSettings();
 void GetDataMinMax(double *data_xmin0, double *data_xmax0, double *data_ymin0,
                    double *data_ymax0, double *data_zmin0, double *data_zmax0);
 void SetDataMinMax(const double data_xmin0, const double data_xmax0,
                    const double data_ymin0, const double data_ymax0,
                    const double data_zmin0, const double data_zmax0);

// jlp_GsegData_DataMinMax3d.cpp
 void DataMinMax3d ( void );
 int ReadDataMinMax3dFromFile(char *param_filename0);
 int ReadSymbolMinMax3dFromFile(char *param_filename0);

// jlp_GsegData_CreateContourLabel.cpp
 int CreateContourLabel(double x_user, double y_user, int icontour_plots,
                        double *zcontour_data, const double zmin0,
                        const double zmax0, double *labelled_contour_value);

// jlp_GsegData_ComputeHistogram.cpp
 void ComputeHistogram(int iplot, int ihist, double **yhist0, int *nbins, 
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
                   double *ytext, double *ztext, char *text_coords_flag,
                   const int itext);
 void ReadExtraTextFromFile(char *param_filename, char *axis_type0);

// Accessors:
/*****************************************************************************
* Get number of contour or color plots
*****************************************************************************/

// ndata:
int NData(const int i) {return(ndata[i]); }

// Contours:
int NContours() {return(ncontours1); }
int NXContour(const int i) {return(nxcontour[i]); }
int NYContour(const int i) {return(nycontour[i]); }
double XContour(const int i) {return(xcontour[i]); }
double YContour(const int i) {return(ycontour[i]); }
double ZContour(const int i) {return(zcontour[i]); }
double* XContourPtr(const int i) {return(&(xcontour[i])); }
double* YContourPtr(const int i) {return(&(ycontour[i])); }
double* ZContourPtr(const int i) {return(&(zcontour[i])); }

// x, y, z color:
int NXColor(const int i) {return(nxcolor[i]); }
int NYColor(const int i) {return(nycolor[i]); }
double XColor(const int i) {return(xcolor[i]); }
double YColor(const int i) {return(ycolor[i]); }
double ZColor(const int i) {return(zcolor[i]); }
double* XColorPtr(const int i) {return(&(xcolor[i])); }
double* YColorPtr(const int i) {return(&(ycolor[i])); }
double* ZColorPtr(const int i) {return(&(zcolor[i])); }

// x,y,z data:
double XData(const int i) {return(xdata[i]); }
double YData(const int i) {return(ydata[i]); }
double ZData(const int i) {return(zdata[i]); }
double* XDataPtr(const int i) {return(&(xdata[i])); }
double* YDataPtr(const int i) {return(&(ydata[i])); }
double* ZDataPtr(const int i) {return(&(zdata[i])); }

// x, y, z mesh
int NXMesh(const int i) {return(nxmesh[i]); }
int NYMesh(const int i) {return(nymesh[i]); }
double* XMeshPtr(const int i) {return(&(xmesh[i])); }
double* YMeshPtr(const int i) {return(&(ymesh[i])); }
double* ZMeshPtr(const int i) {return(&(zmesh[i])); }

// linebreaks:
int NLinebreaks() {return(nlinebreaks); }
int NLinebreak(const int i) {
  int ivalue = 0;
  if(i >= 0 && i < nlinebreaks) ivalue = nlinebreak[i]; 
  return(ivalue);
  }

// Types and styles:
char* PlotTypesPtr() {return(plot_types); }
int PlotTypes(char *plot_types0, const int iplt) {
    int status = -1;
    if(iplt >= 0 || iplt < nplots1) { 
      strcpy(plot_types0, &plot_types[iplt]); 
      status = 0;
      }
    return(status);
    }
int* StyleFlagsPtr() {return(styleflags); }
int StyleFlags(const int iplt) {return(styleflags[iplt]); }
unsigned int StyleSizes(const int iplt) {return(stylesizes[iplt]); }
char StyleChar1(const int iplt) {return(stylechar1[iplt]); }
char StyleChar2(const int iplt) {return(stylechar2[iplt]); }
UINT32 StyleColor1(const int iplt) {return(stylecolor1[iplt]); }
UINT32 StyleColor2(const int iplt) {return(stylecolor2[iplt]); }

// Extra  data:
int NExtraLines() {return(n_extra_lines1); }
int NExtraEllipses() {return(n_extra_ellipses1); }
int NExtraRectangles() {return(n_extra_rectangles1); }
int NExtraSymbols() {return(n_extra_symbols1); }
int NExtraTextItems() {return(n_text_items1); }
int NLegendLines() {return(n_legend_lines1); }

private:
 JLP_Gseg *jlp_gseg1;
 JLP_Gsegraf *jlp_gsegraf1;

 data_min_max_type  *p_data_min_max, *fromfile_data_min_max;
 int nplots1, *styleflags;

// File names and formats 
 char *filenames, *formats, *formats_mod;
 int *nfilenames, *nformats;

// Plot types and styles:
 char *plot_types;
 char *stylechar1, *stylechar2;
 UINT32 *stylecolor1, *stylecolor2;
 unsigned int *stylesizes;

 int ncontours1;
 int *ndata, *nxmesh, *nymesh, *nxcontour, *nycontour;
 int *nxcolor, *nycolor, *nlinebreak, nlinebreaks;
 double *xdata, *ydata, *zdata, *xmesh, *ymesh, *zmesh;
 double *xcontour, *ycontour, *zcontour, *xcolor, *ycolor, *zcolor;

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

// Histograms:
 char *bin_values, *bin_refs;
 double *bin_widths;

// Image parameters:
 char image_filename1[128], image_anchor_text1[64], image_coords_flag1[4];
 double x_image1, y_image1, z_image1;

// string_get is used for reading lines (see gseg_get_string())
 char *string_get;

};

#endif
