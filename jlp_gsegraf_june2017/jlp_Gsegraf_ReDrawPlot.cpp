/*******************************************************************************
* jlp_gsegraf_ReDrawPlot.cpp
*
* JLP
* Version 01/04/2017
*******************************************************************************/
#include <stdio.h>
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"      // JLP_GsegAxes class

/************************************************************************
*
************************************************************************/
int JLP_Gsegraf::GSEG_ReDrawPlot_Scaled(double xmin, double xmax, 
                                        double ymin, double ymax,
                                        double zmin, double zmax, 
                                        int *disp_coord_flag0)
{
int i, flag_ref0;
char axis_type0[64], axis_scale0[64];
double axis_limits0[6];
int set_axis_limits0[6];

// Disable coordinate display by default:
*disp_coord_flag0 = 0;

// Clear Screen
   jlp_gseg1->GSEG_Clear();

// Get axis type (e.g. "linear") and scale (e.g. "equal"):
   jlp_gseg_axes1->GetAxisType(axis_type0);
   jlp_gseg_axes1->GetAxisScale(axis_scale0);

// Initialize set_axis_limits to zero by default:
   for ( i=1; i<=6; i++ ) set_axis_limits0[i-1] = 0;

/* Redraw plot */
   if ( strcmp(axis_type0, "linear")   == 0 ||
        strcmp(axis_type0, "semilogx") == 0 ||
        strcmp(axis_type0, "semilogy") == 0 ||
        strcmp(axis_type0, "loglog")   == 0 )
      {
/* Set new axis limits */
      axis_limits0[0] = xmin;
      axis_limits0[1] = xmax;
      axis_limits0[2] = ymin;
      axis_limits0[3] = ymax;
      axis_limits0[4] = zmin;
      axis_limits0[5] = zmax;
      jlp_gseg_axes1->SetPlotParamAxisLimits(axis_limits0, 6);
// Activate axis limits:
      for ( i=1; i<=6; i++ ) set_axis_limits0[i-1] = 1;
      jlp_gseg_axes1->SetAxisLimitsOnOff(set_axis_limits0, 6);
     
/* Draw plot (without storing those values as reference values) */
      flag_ref0 = 0;
      jlp_gseg_axes1->AxisLimits(flag_ref0, set_axis_limits0);
      if ( strcmp(axis_type0, "linear") == 0 &&
           strcmp(axis_scale0, "equal") == 0 )
         jlp_gseg_axes1->AxesEqual(0);

      jlp_gseg_axes1->DrawBackgroundImage();
      jlp_gseg_axes1->DrawGrid2d();
      jlp_gseg_axes1->DrawGridLog();
      PlotData2d();
      if ( strcmp(axis_type0, "linear") == 0 )
         {
         PlotExtraRectangles();
         PlotExtraEllipses();
         }
      PlotExtraLines();
      PlotExtraSymbols();
      jlp_gseg_axes1->DrawTickLabels2d();
      jlp_gseg_axes1->DrawTickLabelsLog();
      jlp_gseg_axes1->DrawAxisLabels();
      DrawTheLegend();
      DrawExtraText();
      PlotImage();
      jlp_gseg_axes1->DrawDateTime();

// Allow coordinate display:
      *disp_coord_flag0 = 1;
      }
   else if ( strcmp(axis_type0, "polar") == 0 )
      {
/* Set new axis limits */
      axis_limits0[2] = ymin;
      axis_limits0[3] = ymax;
      jlp_gseg_axes1->SetPlotParamAxisLimits(axis_limits0, 6);
// Activate axis limits:
      for ( i=3; i<=4; i++ ) axis_limits0[i-1] = 1;
      jlp_gseg_axes1->SetAxisLimitsOnOff(set_axis_limits0, 6);

/* Draw plot (without storing those values as reference values) */
      flag_ref0 = 0;
      jlp_gseg_axes1->AxisLimits(flag_ref0, set_axis_limits0);
      jlp_gseg_axes1->DrawBackgroundImage();
      PolarPlot();
      DrawTheLegend();
      DrawExtraText();
      PlotImage();
      jlp_gseg_axes1->DrawDateTime();
      }

   else if ( strcmp(axis_type0, "3d") == 0 )
      {
/* Set new axis limits */
      axis_limits0[0] = xmin;
      axis_limits0[1] = xmax;
      axis_limits0[2] = ymin;
      axis_limits0[3] = ymax;
      axis_limits0[4] = zmin;
      axis_limits0[5] = zmax;
      jlp_gseg_axes1->SetPlotParamAxisLimits(axis_limits0, 6);
// Activate axis limits:
      for ( i=1; i<=6; i++ ) axis_limits0[i-1] = 1;
      jlp_gseg_axes1->SetAxisLimitsOnOff(set_axis_limits0, 6);

/* Draw plot (without storing those values as reference values) */
      flag_ref0 = 0;
      jlp_gseg_axes1->AxisLimits(flag_ref0, set_axis_limits0);
      jlp_gseg_axes1->Initialize3d();
      jlp_gseg_axes1->DrawGrid3d();
      jlp_gseg_axes1->DrawTickMarks3d();
      jlp_gseg_axes1->DrawLabels3d();
      PlotData3d();
      PlotExtraLines();
      PlotExtraSymbols();
      DrawTheLegend();
      DrawExtraText();
      PlotImage();
      jlp_gseg_axes1->DrawDateTime();
      }

 return(0);
}

/**********************************************************************
*
**********************************************************************/
int JLP_Gsegraf::GSEG_ReDrawPlot3D_Rotated(double phi, double theta)
{

// Clear Screen
   jlp_gseg1->GSEG_Clear();

   jlp_gseg_axes1->Set3DParams(phi, theta);

/* Draw plot */
    jlp_gseg_axes1->Initialize3d();
    jlp_gseg_axes1->DrawGrid3d();
    jlp_gseg_axes1->DrawTickMarks3d();
    jlp_gseg_axes1->DrawLabels3d();
    PlotData3d();
    PlotExtraLines();
    PlotExtraSymbols();
    DrawTheLegend();
    DrawExtraText();
    PlotImage();
    jlp_gseg_axes1->DrawDateTime();

return(0);
}

