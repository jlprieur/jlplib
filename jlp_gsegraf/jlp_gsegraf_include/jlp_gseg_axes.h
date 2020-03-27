/******************************************************************************
* JLP_GsegAxes class
*
* JLP
* Version 27/03/2017
*******************************************************************************/
#ifndef _jlp_gseg_axes_h_
#define _jlp_gseg_axes_h_

#include "jlp_gsegraf.h"

class JLP_GsegAxes {
public:

// jlp_GsegAxes.cpp
 JLP_GsegAxes(JLP_Gseg *jlp_gseg0, JLP_Gsegraf *jlp_gsegraf0);
 JLP_GsegAxes(JLP_Gseg *jlp_gseg0, JLP_Gsegraf *jlp_gsegraf0, 
              GSEG_AXIS_DATA gseg_axdata0);

// Desctructor:
 ~JLP_GsegAxes(){ };

 void InitAxesParameters();

// jlp_GsegAxes_AutoScale.cpp
 void AutoScale(int naxes, int nincmax);

// jlp_GsegAxes_AxesEqual.cpp
 void AxesEqual(int flag_ref);

// jlp_GsegAxes_AxisLimits.cpp
 int UpdateAxisLimitsAndReversedAxis(double *axis_limits0, 
                                     int *set_axis_limits0);
 int AxisLimits_UpdateDataMinMax(int *set_axis_limits_local);
 void AxisLimits(int flag_ref, int *set_axis_limits_local);

// jlp_GsegAxes_ChangeAxesLimits.cpp
 int QuestionForChangingAxesRotation(char *prompt0, char *string0);
 int QuestionForChangingAxesLimits(char *prompt0, char *string0);
 int DecodeStringForChangingAxesLimits(char *string, double *xmin, 
                                       double *xmax, double *ymin, double *ymax,
                                       double *zmin, double *zmax, 
                                       char *error_message);

// jlp_GsegAxes_DrawAxisLabels.cpp
 void DrawAxisLabels(void);
 int DrawXPoint(double xdata1, double *x);
 int DrawYPoint(double ydata1, double *y);

// jlp_GsegAxes_DrawBackgroundImage.cpp
 void DrawBackgroundImage(void);

// jlp_GsegAxes_DrawColorScale.cpp
 void DrawColorScale(const int color_plot_type, UINT32 *color0, const int nc0);

// jlp_GsegAxes_DrawDateTime.cpp
 void DrawDateTime(void);

// jlp_GsegAxes_DrawGrid.cpp
 void DrawGrid(char *axis_type, double x11_screen, double y11_screen, 
               double x12_screen, double y12_screen, double x21_screen, 
               double y21_screen, int nticks, double *tick_values,
               double offset1, double offset2, int reversed_axis0);
 void LoadGridParam(char gridchar1_0, char gridchar2_0, UINT32 gridcolor_0);
 void GetGridParam(char *gridchar1_0, char *gridchar2_0, UINT32 *gridcolor_0);

// jlp_GsegAxes_DrawGrid2d.cpp
 void DrawGrid2d(void);

// jlp_GsegAxes_DrawGrid3d.cpp
 void DrawGrid3d(void);

// jlp_GsegAxes_DrawGridLog.cpp
 void DrawGridLog(void);

// jlp_GsegAxes_DrawPolarBox.cpp
 void DrawPolarBox_PartI(double xorigin0, double yorigin0, double radius0,
                         double rmin0, double rmax0, double rscale0);
 void DrawPolarBox_PartII(double xorigin0, double yorigin0,
                         double rmin0, double rmax0, double rscale0);

// jlp_GsegAxes_DrawTicklabels.cpp
 double DrawTickLabels(char *axis_type, double x1_screen, double y1_screen,
                       double x2_screen, double y2_screen, int nticks, 
                       double *tick_values, double offset1, double offset2,
                       double xoffset, double yoffset, char *anchor,
                       int reversed_axis0 );

// jlp_GsegAxes_DrawTicklabels2d.cpp
 void DrawTickLabels2d(void);

// jlp_GsegAxes_DrawTicklabelsLog.cpp
 void DrawTickLabelsLog(void);

// jlp_GsegAxes_DrawTickMarks.cpp
 void DrawTickMarks(char *axis_type, int minor_ticks_flag, int center_flag,
                    double x1_screen, double y1_screen, double x2_screen, 
                    double y2_screen, int nticks, double *tick_values, 
                    double offset1, double offset2, double tick_angle,
                    int reversed_axis0);

// jlp_GsegAxes_DrawTickMarks3d.cpp
 void DrawTickMarks3d(void);

// jlp_GsegAxes_Labels3d.cpp
 void DrawLabels3d ( void );

// jlp_GsegAxes_GetWindowCoords.cpp
 void GetWindowCoords(double *plot_coords, double *window_coords);
 int ContourLabel_FromDevToUser(const double x1, const double y1,
                                double *user_x1, double *user_y1,
                                int *in_frame);

// jlp_GsegAxes_Initialize3d.cpp
 void  Initialize3d ( void );

// jlp_GsegAxes_Setup.cpp
 int CheckAxesParameters();
 void InitializeAxesVariables();
 void SetAxisTypeFlags(char *axis_type0);
 void SetPlotParamAxisLimits(double *axis_limits_0, const int nlimits);
 void SetAxisLimitsOnOff(int *set_axis_limits_0, const int nlimits);
 void GetAxisLimitsOnOff(int *set_axis_limits_0, const int nlimits);
 void SetTickOffsetLabels(const double xoffset1_0, const double xoffset2_0, 
                          const double yoffset1_0, const double yoffset2_0,
                          const double zoffset1_0, const double zoffset2_0);
 void SetPlotSettingsToRefValues();
 void Set3DParams(const double phi_0, const double theta_0);
 void SetPlotBoxDataLimitsFromWindowSize(void);
 void SetPlotBoxDataLimits(const double dev_x1_box, const double dev_x2_box,
                           const double dev_y1_box, const double dev_y2_box);
 void SetAxisLabelPixbufs(char *xlabel0, char *ylabel0, char *zlabel0, 
                          char *title0, double font_size_axis_labels0,
                          double font_size_title0, const UINT32 color0);
 void GetFontParams(char *font_name0, double *font_size_title0,
                                 double *font_size_axis_labels0,
                                 double *font_size_tick_labels0,
                                 double *font_size_text0,
                                 double *font_size_legend0,
                                 double *font_size_date_time0);
 void SetFontParams(char *font_name0, double font_size_title0,
                                 double font_size_axis_labels0,
                                 double font_size_tick_labels0,
                                 double font_size_text0,
                                 double font_size_legend0,
                                 double font_size_date_time0);

// jlp_GsegAxes_ReadPlotParameterFile.cpp
 int ReadAxisTypeFromFile(char *p_param_filename);
 int ReadBackgroundStyleFromFile(char *p_param_filename);
 int ReadAxisLimitsFromFile(char *p_param_filename);
 int SetPlotParamAxisLimitsAndLabels(double *axis_limits_0, int *reversed_axis0,
                                     char *axis_scale0,
                                     char *axis_type0, char *xlabel0, 
                                     char *ylabel0, char *zlabel0, 
                                     char *title0);
 int ReadFontsFromFile(char *p_param_filename);
 int SetAxisParamsFromGsegAxisData(GSEG_AXIS_DATA *gseg_axdata0);

// jlp_GsegAxes_GetPlotSettings.cpp
 int NCoords();
 void GetAxisTypeFlags(int *flag_2d_0, int *flag_3d_0, int *flag_2d_rect_0, 
                       int *flag_polar_0, int *flag_linear_0, int *flag_logx_0,
                       int *flag_logy_0, int *flag_loglog_0);
 int GetBoxSettingsForLinear(double *dev_x1_box0, double *dev_x2_box0,
                             double *dev_y1_box0, double *dev_y2_box0,
                             double *xmin_0, double *xmax_0,
                             double *ymin_0, double *ymax_0,
                             double *zmin_0, double *zmax_0,
                             double *xscale_0, double *yscale_0);
 int GetAxisLimitsFromTickLabelsForLinear(double *xmin_0, double *xmax_0,
                                          double *ymin_0, double *ymax_0,
                                          double *zmin_0, double *zmax_0);
 int GetBoxSettingsForPolar(double *xorigin_0, double *yorigin_0, 
                            double *radius_0, double *rmin_0, double *rmax_0, 
                            double *rscale_0);
 int GetBoxSettingsFor3d(double origin_0[3], double Ry_0[9], double Rz_0[9], 
                         double Ryz_0[9], double *xmin_0, double *xmax_0,
                         double *ymin_0, double *ymax_0, double *zmin_0, 
                         double *zmax_0, double *xscale_0, double *yscale_0,
                         double *zscale_0);
 int GetBoxSettings(double *dev_x1_box0, double *dev_x2_box0,
                                  double *dev_y1_box0, double *dev_y2_box0,
                                  double *xmin_0, double *xmax_0,
                                  double *ymin_0, double *ymax_0,
                                  double *zmin_0, double *zmax_0,
                                  double *rmin_0, double *rmax_0,
                                  double *xscale_0, double *yscale_0,
                                  double *zscale_0, double *rscale_0,
                                  double *xorigin_0, double *yorigin_0,
                                  double *radius_0, double origin_0[3],
                                  double Ry_0[9], double Rz_0[9],
                                  double Ryz_0[9], int *ncoords_0);
 int GetNzValues(int *nzvalues);
 void GetPlotParamAxisLimits(double *axis_limits_0, const int nlimits);
 void GetPlotBoxDataLimits(double *dev_x1_box, double *dev_x2_box,
                           double *dev_y1_Box, double *dev_y2_box);
 void GetAxisLabelPixbufs(void);

// Accessors:
 int GetReversedAxis(int *reversed_axis0) {
   int i;
   for(i = 0; i < 3; i++) reversed_axis0[i] = p_plot_param->reversed_axis[i];
   return(0);
   }
 int GetAxisType(char *axis_type0) {
    strcpy(axis_type0, p_plot_param->axis_type); 
    return(0);
    };
 int GetAxisScale(char *axis_scale0) {
    strcpy(axis_scale0, p_plot_param->axis_scale); 
    return(0);
    };
 int GetPlotBox(char *plot_box0) {
    strcpy(plot_box0, p_plot_param->plot_box); 
    return(0);
    };
 int Get3dQuadrant(int *quadrant0) {
    *quadrant0 = p_plot_param_3d->quadrant; 
    return(0);
    };
// phi: view-direction azimuth (deg) from x axis in x-y plane
// theta: view-direction elevation (deg) from x-y plane
 int Get3dViewAngles(double *phi0, double *theta0) {
    *phi0 = p_plot_param_3d->phi; 
    *theta0 = p_plot_param_3d->theta; 
    return(0);
    };
 int GetAxisLength(double *axis_length0) {
    *axis_length0 = p_plot_param_3d->axis_length; 
    return(0);
    };
 int flag_linear() {return(flag_linear_1);};
 int flag_logx() {return(flag_logx_1);};
 int flag_logy() {return(flag_logy_1);};
 int flag_loglog() {return(flag_loglog_1);};
 int flag_polar() {return(flag_polar_1);};
 int flag_2d() {return(flag_2d_1);};
 int flag_3d() {return(flag_3d_1);};
 int flag_2d_rect() {return(flag_2d_rect_1);};
 void SetTickSize(const double tick_minor0, const double tick_major0) 
  {tick_minor_1 = tick_minor0; tick_major_1 = tick_major0;};

UINT32 canvas_fg_color(){return canvas_fg_color1;};
UINT32 canvas_bg_color(){return canvas_bg_color1;};
UINT32 zoom_fill_color(){return zoom_fill_color1;};
void set_canvas_fg_color(const UINT32 canvas_fg_color0){
 canvas_fg_color1 = canvas_fg_color0;
 };
void set_canvas_bg_color(const UINT32 canvas_bg_color0){
 canvas_bg_color1 = canvas_bg_color0;
 };
void set_zoom_fill_color(const UINT32 zoom_fill_color0){
 zoom_fill_color1 = zoom_fill_color0;
 };
 int GetHighContrastForZAxis(int *high_contrast0) {
    *high_contrast0 = high_contrast_for_z_axis1; 
    return(0);
    };

private:
 JLP_Gseg *jlp_gseg1;
 JLP_Gsegraf *jlp_gsegraf1;

/* Declare plot-parameter variables and pointers */
 int set_axis_limits1[6], background_image_style;

/* Declare axis_type flags */
 int flag_linear_1, flag_logx_1, flag_logy_1, flag_loglog_1, flag_polar_1;
 int flag_3d_1, flag_2d_1, flag_2d_rect_1;
 int minor_ticks_flag, high_contrast_for_z_axis1;

 plot_param_type    *p_plot_param;
 plot_param_3d_type *p_plot_param_3d;

 plot_box_data_type *p_plot_box_data;
 ticklabels_type    *p_ticklabels;
 UINT32 gridcolor1;
 char gridchar1, gridchar2; 

// string_get is used for reading lines (see gseg_get_string())
 char background_image_file[128], *string_get;

/* Declare tick-mark variables                 
* (initialized at object creation and updated when the window has changed
*  its size) */
 double tick_major_1, tick_minor_1;

/* Declare tick-label size variables */
 double width_ytick_labels, width_axis1_tick_labels;
 double width_axis2_tick_labels, width_axis3_tick_labels;

/* Declare pixbufs for x, y, z labels and title */
//    JLP_DPixbuf pixbuf_window;
 JLP_TPixbuf pixbuf_xlabel, pixbuf_ylabel, pixbuf_zlabel;
 JLP_TPixbuf pixbuf_title;

// Color variables and pointers for colors specified by color characters
 UINT32 canvas_fg_color1, canvas_bg_color1, zoom_fill_color1;

/* color-specification characters "kaswrylqbfmogtnpx" */
 char color_string[128];
 UINT32 color_rgba[17];

// Fonts
 double font_size_date_time1, font_size_legend1, font_size_text1;
 double font_size_tick_labels1, font_size_axis_labels1, font_size_title1;
 char font_name1[128], date_time1[64];

};

#endif
