/*******************************************************************************
*
* jlp_gseg_prototypes.h
* declaration of the prototypes of the functions used by jlp_gseg
*
* This file was modified from GSEGrafix (gsegrafix-1.0.6, sept. 2011)
* Copyright © 2008, 2009, 2010, 2011 Spencer A. Buckner
* http://savannah.gnu.org/projects/gsegrafix
*
* JLP
* Version 28/10/2016
*******************************************************************************/
#ifndef jlp_gseg_prototypes_h_
#define jlp_gseg_prototypes_h_

// Declaration of some structures used here
#include "jlp_gsegraf_defs.h"

// jlp_Gsegraf_InitializePlot.cpp
int GSEG_InitializePlot(JLP_Gseg *jlp_gseg0,
                        char *parameter_file_0, char **save_filename_0,
                        int *close_flag_0, char *font_name0,
                        double *font_size_date_time0,
                        double *font_size_legend0,
                        double *font_size_text0,
                        double *font_size_tick_labels0,
                        double *font_size_axis_labels0,
                        double *font_size_title0);
int GSEG_InitializePlotSize();
void GSEG_SetPointersFromPlot(char **plot_types0, int **styleflags0);
void Get_param_filename(char *param_filename0);
void Get_canvas_fg_color(UINT32 *canvas_fg_color0);
void Get_canvas_bg_color(UINT32 *canvas_bg_color0);
void GetColorValues(UINT32 *zoom_fill_color_0,
                         UINT32 *canvas_fg_color_0,
                         UINT32 *canvas_bg_color_0);
int SymbolFromStyleChar1(char *symbol_str, const int iplt, int *ifunc);
int SymbolFromStyleChar2(char *symbol_str, const int iplt, int *ifunc);
int SetColorsFromStyleChars(UINT32 *stylecolor2_0, char stylechar1_0, 
                            char stylechar2_0, const int iplt);
int SetStyleColorFromStyleChar(UINT32 *stylecolor_0, char stylechar_0);
int SetColorsFromStyleColor(UINT32 stylecolor2_0, char stylechar1_0, 
                            const int iplt);
int SetZBlackWhite(const double zblack_0, const double zwhite_0, 
                   const int iplt);
int SetAlphaColor(const UINT32 alphacolor_0, const int iplt);

// jlp_Gsegraf_AxesTools.cpp
void GSEG_SetDashParameters(double tick_major0, double tick_minor0,
                            double dash0, double space_dash0,
                            double space_dot0);
void GSEG_GetCurrentPlotSettings(int *flag_2d_0, int *flag_3d_0,
                                 int *flag_2d_rect_0, int *flag_polar_0,
                                 int *flag_linear_0, int *flag_logx_0,
                                 int *flag_logy_0, int *flag_loglog_0,
                                 int *ncontours_0, int *nplots0);
void GSEG_GetBoxSettingsForLinear(double *dev_x1_box0, double *dev_x2_box0,
                                  double *dev_y1_box0, double *dev_y2_box0,
                                  double *xmin_0, double *xmax_0,
                                  double *ymin_0, double *ymax_0,
                                  double *zmin_0, double *zmax_0,
                                  double *xscale_0, double *yscale_0,
                                  char *axis_type0);
int GSEG_AxisLimitsFromTickLabelsForLinear(double *axis_limits0, 
                                           const int nval0);
void GSEG_GetAxisType(char *axis_type0);
void GSEG_SetAxisLabelPixbufs();
void GSEG_SetPlotParamAxisLimits(double *axis_limits_0, const int nlimits);
void GSEG_GetPlotParamAxisLimits(double *axis_limits_0, const int nlimits);
int GSEG_QuestionForChangingAxesRotation(char *prompt0, char *string0);
int GSEG_QuestionForChangingAxesLimits(char *prompt0, char *string0);
int GSEG_DecodeStringForChangingAxesLimits(char *string, double *xmin,
                                       double *xmax, double *ymin, double *ymax,
                                       double *zmin, double *zmax,
                                       char *error_message);
int GSEG_ContourLabel_FromDevToUser(const double x1, const double y1,
                                    double *user_x1, double *user_y1,
                                    int *in_frame);

// jlp_Gsegraf_DataTools.cpp
void GSEG_get_nplots(int *nplots0);
int GSEG_get_ncontour_plots(int *ncontour_plots0);
int GSEG_get_plot_type(char *plot_types0, const int iplt);
int GSEG_get_styleflag(int *styleflag0, const int iplot0);
void GSEG_DataMinMax ( void );
void GSEG_GetDataMinMax(double *data_xmin0, double *data_xmax0, 
                        double *data_ymin0, double *data_ymax0, 
                        double *data_zmin0, double *data_zmax0);
void GSEG_SetDataMinMax(const double data_xmin0, const double data_xmax0,
                        const double data_ymin0, const double data_ymax0,
                        const double data_zmin0, const double data_zmax0);
int GSEG_CreateContourLabel(double xmouse, double ymouse, int icontour_plots,
                            double *zcontour_data,
                            double *labelled_contour_value);

/* jlp_gseg_InitializeVariables.c */
void GSEG_InitializeVariables ( void );

// jlp_gseg_UserToDeviceCoord.cpp
int GSEG_UserFromDeviceCoord(double xmouse, double ymouse,
                              int plot_type_data0,
                              double *x_coords_data,
                              double *y_coords_data,
                              double *z_coords_data);
int GSEG_plot_type_data(int *plot_type_data_value, unsigned int iplot0);

// jlp_gseg_ReDrawPlot.cpp
int GSEG_ReDrawPlot_Scaled(double xmin, double xmax, double ymin, double ymax,
                           double zmin, double zmax, int *disp_coord_flag0);
int GSEG_ReDrawPlot3D_Rotated(double phi, double theta);

// jlp_gseg_ZoomIn.cpp
void GSEG_ZoomIn (double x1_window, double y1_window, double x2_window,
                  double y2_window );

// jlp_gseg_ZoomOut.cpp
void GSEG_ZoomOut ( void );

void GSEG_GetAxisLabelPixbufs ( void );

// jlp_Gsegraf_DrawHistogram
void  DrawHistogram ( int iplot, int ihist );

/**************************************************************************/

int     CheckParamData ( void );

void     ColorPlot3d ( int iplot, int xindex, int yindex, int zindex, int nx, int ny );

void     ContourPlot3d ( int iplot, int icountour, int xindex, int yindex, int zindex, int nx, int ny );

void     DrawContours3d ( int icontour, double *xpoints, double *ypoints, double *zpoints );

void     GSEG_DrawGraph(void);
void     GSEG_AddExtraLabel(char *string0, double x0, double y0,
                            UINT32 fill_color_rgba0, UINT32 canvas_bg_color0, 
                            const char *anchor0, int raise_to_top0);
void     GSEG_DrawExtraLabels(void);
void     GSEG_EraseExtraLabels(void);

// jlp_Gsegraf_PlotImage.cpp
void     PlotImage(void);

// jlp_Gsegraf_DrawTheLegend.cpp
 int DrawTheLegend();
 int DrawTheLegendSymbols(double xanchor, double yanchor, double dx_symbol, 
                          double dy_symbol, double height_legend, int nlines);

// jlp_Gsegraf_DrawExtraText.cpp
 int DrawExtraText(void);

/* DrawLines1.c */
void DrawDashedLine(JLP_CanvasPoints *points, UINT32 fill_color_rgba,
                    unsigned int line_width );
void DrawDottedLine(JLP_CanvasPoints *points, UINT32 fill_color_rgba,
                    unsigned int size );

/* DrawLines.c */
void     DrawLines2d ( int npts, double *x, double *y, double xmin, 
                       double xmax, double ymin, double ymax,
                       double xscale, double yscale, UINT32 color, 
                       unsigned int line_width, char *line_type );
void     DrawLinesPolar ( int npts, double *x, double *y, double xorigin, 
                          double yorigin, double rmin, double rmax, 
                          double rscale, UINT32 color, unsigned int line_width,
                          char *line_type );
void     DrawLines3d ( int npts, double *x, double *y, double *z, 
                       double *origin, double *Ryz, double xmin, double xmax, 
                       double ymin, double ymax, double zmin, double zmax,
                       double xscale, double yscale, double zscale, 
                       UINT32 color, unsigned int line_width, char *line_type );

/* jlp_gseg_DrawSymbols1.c */
void DrawSymbol1(const int isymb1, double x0, double y0,
                 UINT32 fill_color_rgba0, UINT32 outline_color_rgba0,
                 unsigned int size0);
void DrawSymbol2(const int isymb2, double x0, double y0,
                 UINT32 fill_color_rgba0, unsigned int size0);
void GSEG_DrawTriangle(double x, double y, UINT32 fill_color_rgba0,
                       UINT32 outline_color_rgba0, unsigned int size);
void DrawDiamond(double x, double y, UINT32 fill_color_rgba0,
                 UINT32 outline_color_rgba0, unsigned int size );
void DrawPentagon(double x, double y, UINT32 fill_color_rgba,
                  UINT32 outline_color_rgba0, unsigned int size );
void DrawHexagon(double x, double y, UINT32 fill_color_rgba,
                 UINT32 outline_color_rgba0, unsigned int size );
void DrawPlus(double x, double y, UINT32 fill_color_rgba, unsigned int size);
void DrawX ( double x, double y, UINT32 fill_color_rgba, unsigned int size );
void DrawStar(double x, double y, UINT32 fill_color_rgba, unsigned int size);
void DrawAsterisk(double x, double y, UINT32 fill_color_rgba,
                  unsigned int size);

// jlp_Gsegraf_DrawMiscel.cpp
void DrawBar(double x1, double y1, double x2, double y2,
             UINT32 fill_color_rgba0, UINT32 outline_color_rgba0);
void DrawMesh(double x1, double y1, double x2, double y2,
              UINT32 fill_color_rgba0, UINT32 outline_color_rgba0, int flag);
void DrawContour(double x1, double y1, double x2, double y2,
                 UINT32 fill_color_rgba0, UINT32 outline_color_rgba0, int flag);
void DrawColorPlot(double x1, double y1, double x2, double y2);

/* InitializePlot.c */

void GSEG_GetCurrentPlotValues(int *flag_2d_0, int *flag_3d_0, 
                               int *flag_2d_rect_0, int *flag_polar_0, 
                               int *flag_linear_0, int *flag_logx_0,
                               int *flag_logy_0, int *flag_loglog_0,
                               int *ncontours_0,
                               UINT32 *zoom_fill_color_0,
                               UINT32 *canvas_fg_color_0,
                               UINT32 *canvas_bg_color_0);
void GSEG_SetCurrentPlotValues(int flag_2d_0, int flag_3d_0, 
                               int flag_2d_rect_0, int flag_polar_0, 
                               int flag_linear_0, int flag_logx_0,
                               int flag_logy_0, int flag_loglog_0,
                               int ncontours_0,
                               UINT32 zoom_fill_color_0,
                               UINT32 canvas_fg_color_0,
                               UINT32 canvas_bg_color_0);
void SetCanvasColor(const UINT32 canvas_fg_color, const UINT32 canvas_bg_color);

void     MeshPlot3d ( int iplot, int imesh, int xindex, int yindex, int zindex, int nx, int ny );

void     PlotData3d ( void );
void     PlotInterp3d ( int iplot, int imesh,
                        double xmin, double ymin, double zmin, double zmax,
                        double xscale, double yscale, double zscale,
                        double *origin, double *Ryz, UINT32 *fill_color,
                        double *xpoints, double *ypoints, double *zpoints );


void     PlotNormal3d ( int iplot, int imesh,
                        double xmin, double ymin, double zmin, double zmax,
                        double xscale, double yscale, double zscale,
                        double *origin, double *Ryz, UINT32 *fill_color,
                        double *xpoints, double *ypoints, double *zpoints );

/* PlotPoints3d.c */
void   PlotPoints3d ( int iplot, int index, int npts );
void   DrawLineSegments3d ( int iplot, int index, int npts, double *origin, 
                            double *Ryz, double xmin, double xmax, 
                            double ymin, double ymax, double zmin, double zmax,
                            double xscale, double yscale, double zscale, 
                            int linechar );

/* jlp_Gsegraf_PlotExtraRectangles.cpp */
void     PlotExtraRectangles();

/* jlp_Gsegraf_PlotExtraEllipses.cpp */
void     PlotExtraEllipses();

/* jlp_Gsegraf_PlotExtraLines.cpp */
void     PlotExtraLines();

// jlp_Gsegraf_PlotExtraSymbols.cpp
 void PlotExtraSymbols();

/* jlp_Gsegraf_PolarPlot.cpp */
void     PolarPlot();
void     DrawLineSegmentsPolar(int iplot, int index, int npts,
                               double xorigin, double yorigin, double rmin, 
                               double rmax, double rscale, int linechar);
void     DrawDashedCircle ( double xorigin, double yorigin, double radius, 
                            UINT32 fill_color_rgba );
void     DrawDottedCircle ( double xorigin, double yorigin, double radius, 
                            UINT32 fill_color_rgba );

int ReadParamFile (char *param_filename, char **save_filename_0, 
                   int *close_flag_0);

/************************************ 2d ***********************************/
// jlp_Gseggraf_ColorPlot2d.cpp :
 void ColorPlot2d(int iplot, int icolor, int xindex, int yindex,
                  int zindex, int nx, int ny);
 void DrawColorBarFor2dPlot(const double x1_bar0, const double y1_bar0,
                            const int width0, const int height0);

// jlp_Gsegraf_ContourPlot2d.cpp :
  void ContourPlot2d(int iplot, int icontour, int xindex, int yindex,
                     int zindex, int nx, int ny);

// jlp_Gsegraf_PlotData2d.cpp :
  void PlotData2d (void);
  void DrawLineSegments(int iplot, int index, int npts,
                        double xmin, double xmax, double ymin, double ymax,
                        double xscale, double yscale, int linechar);

#endif
