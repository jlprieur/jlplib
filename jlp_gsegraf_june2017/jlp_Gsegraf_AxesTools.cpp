/*******************************************************************************
* jlp_Gsegraf_AxesTools.cpp
* JLP_Gsegraf class
*
* Interface with JLP_GsegAxes routines
*
* JLP
* Version 18/04/2017
*******************************************************************************/
#include <stdlib.h>          // exit()
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"            // JLP_Gseg_Axes class
#include "jlp_gseg_data.h"            // JLP_Gseg_Data class

/*****************************************************************************
*
* (called by gsegraf_main/EventHandler())
*****************************************************************************/
void JLP_Gsegraf::GSEG_SetDashParameters(double tick_major0, double tick_minor0,
                                         double dash0, double space_dash0, 
                                         double space_dot0)
{
// Save to private variables:
dash1 = dash0;
space_dash1 = space_dash0;
space_dot1 = space_dot0;

jlp_gseg_axes1->SetTickSize(tick_minor0, tick_major0);

return;
}
/*****************************************************************************
*
* (called by gsegraf_main/CreateMenuBar())
*****************************************************************************/
void JLP_Gsegraf::GSEG_GetCurrentPlotSettings(int *flag_2d_0, int *flag_3d_0,
                                 int *flag_2d_rect_0, int *flag_polar_0,
                                 int *flag_linear_0, int *flag_logx_0,
                                 int *flag_logy_0, int *flag_loglog_0,
                                 int *ncontours_0, int *nplots0)
{

jlp_gseg_data1->Get_ncontours(ncontours_0);

jlp_gseg_data1->Get_nplots(nplots0);

jlp_gseg_axes1->GetAxisTypeFlags(flag_2d_0, flag_3d_0, flag_2d_rect_0,
                                 flag_polar_0, flag_linear_0, flag_logx_0,
                                 flag_logy_0, flag_loglog_0);

return;
}
/*************************************************************************
* Get plot box minimum and maximum values
*  (called by gsegraf_main/CreateMenuBar())
*************************************************************************/
void JLP_Gsegraf::GSEG_GetBoxSettingsForLinear(double *dev_x1_box0, 
                                      double *dev_x2_box0,
                                      double *dev_y1_box0, double *dev_y2_box0,
                                      double *xmin_0, double *xmax_0,
                                      double *ymin_0, double *ymax_0,
                                      double *zmin_0, double *zmax_0,
                                      double *xscale_0, double *yscale_0,
                                      char *axis_type0)
{
jlp_gseg_axes1->GetBoxSettingsForLinear(dev_x1_box0, dev_x2_box0, dev_y1_box0, 
                                        dev_y2_box0, xmin_0, xmax_0, ymin_0, 
                                        ymax_0, zmin_0, zmax_0, xscale_0,
                                        xscale_0);
jlp_gseg_axes1->GetAxisType(axis_type0);
              
return;
}
/*************************************************************************
* Get plot box minimum and maximum values
*  (called by gsegraf_main/CreateMenuBar())
*************************************************************************/
int JLP_Gsegraf::GSEG_AxisLimitsFromTickLabelsForLinear(double *axis_limits0,
                                                         const int nval0)
{
double xmin_0, xmax_0, ymin_0, ymax_0, zmin_0, zmax_0;
int status = -1;

if(nval0 == 6) {
  status = jlp_gseg_axes1->AxisLimitsFromTickLabelsForLinear(&xmin_0, &xmax_0, 
                                                             &ymin_0, &ymax_0, 
                                                             &zmin_0, &zmax_0);
  if(status == 0) {
    axis_limits0[0] = xmin_0;
    axis_limits0[1] = xmax_0;
    axis_limits0[2] = ymin_0;
    axis_limits0[3] = ymax_0;
    axis_limits0[4] = zmin_0;
    axis_limits0[5] = zmax_0;
    }
  }

return(status);
}
/*****************************************************************************
*
* (called by gsegraf_main/CreateMenuBar())
*****************************************************************************/
void JLP_Gsegraf::GSEG_GetAxisType(char *axis_type0)
{

jlp_gseg_axes1->GetAxisType(axis_type0);

return;
}
/*************************************************************************
* Interface with JLP_GsegAxes::SetAxisLabelPixbufs
*  (called by gsegraf_main/CreateMenuBar())
*************************************************************************/
void JLP_Gsegraf::GSEG_SetAxisLabelPixbufs()
{
jlp_gseg_axes1->SetAxisLabelPixbufs();
}
/*************************************************************************
* Interface with JLP_GsegAxes::SetPlotParamAxisLimits
* to emulate settings axis limits from the input parameter file
*************************************************************************/
void JLP_Gsegraf::GSEG_SetPlotParamAxisLimits(double *axis_limits_0, 
                                              const int nlimits0)
{
jlp_gseg_axes1->SetPlotParamAxisLimits(axis_limits_0, nlimits0);
return;
}
/*************************************************************************
* Interface with JLP_GsegAxes::GetPlotParamAxisLimits
* to get the axis limits contained in the input parameter file
*************************************************************************/
void JLP_Gsegraf::GSEG_GetPlotParamAxisLimits(double *axis_limits_0, 
                                              const int nlimits0)
{
jlp_gseg_axes1->GetPlotParamAxisLimits(axis_limits_0, nlimits0);
return;
}
/*************************************************************************
* Interface with JLP_GsegAxes::QuestionForChangingAxesRotation()
*  (called by gsegraf_main/ViewMenu())
*************************************************************************/
int JLP_Gsegraf::GSEG_QuestionForChangingAxesRotation(char *prompt0, 
                                                      char *string0)
{
int status;

status = jlp_gseg_axes1->QuestionForChangingAxesRotation(prompt0, string0);

return(status);
}
/*************************************************************************
* Interface with JLP_GsegAxes::QuestionForChangingAxesLimits()
*  (called by gsegraf_main/ViewMenu())
*************************************************************************/
int JLP_Gsegraf::GSEG_QuestionForChangingAxesLimits(char *prompt0, 
                                                    char *string0)
{
int status;

status = jlp_gseg_axes1->QuestionForChangingAxesLimits(prompt0, string0);

return(status);
}
/*************************************************************************
* Interface with JLP_GsegAxes::QuestionForChangingAxesLimits()
*  (called by gsegraf_main/ViewMenu())
*************************************************************************/
int JLP_Gsegraf::GSEG_DecodeStringForChangingAxesLimits(char *string, 
                                                 double *xmin, double *xmax, 
                                                 double *ymin, double *ymax,
                                                 double *zmin, double *zmax,
                                                 char *error_message)
{
int status;

status = jlp_gseg_axes1->DecodeStringForChangingAxesLimits(string, xmin, xmax,
                                                           ymin, ymax, zmin, 
                                                           zmax, error_message);

return(status);
}
/*************************************************************************
* Interface with JLP_GsegAxes
*************************************************************************/
int JLP_Gsegraf::GSEG_ContourLabel_FromDevToUser(const double dev_x1, 
                                                 const double dev_y1,
                                                 double *user_x1, 
                                                 double *user_y1,
                                                 int *in_frame)
{
int status;

status = jlp_gseg_axes1->ContourLabel_FromDevToUser(dev_x1, dev_y1, user_x1,
                                                    user_y1, in_frame);

return(status);
}
