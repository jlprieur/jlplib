/*******************************************************************************
* jlp_gsegraf_ZoomIn.cpp
*
* JLP
* Version 01/04/2017
*******************************************************************************/
#include <stdio.h>
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"      // JLP_GsegAxes class
#include "jlp_gseg_data.h"      // JLP_GsegData class

/******************************************************************************
* Causes plot to zoom to indicated rectangle.
*
* INPUT:
*  x1_window, y1_window, ...: device coordinates of zoomed window
*
*******************************************************************************/
void JLP_Gsegraf::GSEG_ZoomIn(double x1_window, double y1_window, 
                              double x2_window, double y2_window)
{
/* Declare variables */
int i, ncoords, flag_ref0, set_axis_limits0[6];
double x1, y1, x2, y2, axis_limits0[6];
double dev_x1_box, dev_x2_box, dev_y1_box, dev_y2_box,
       xmin, xmax, ymin, ymax, zmin, zmax, rmin, rmax,
       xscale, yscale, zscale, rscale,
       xorigin, yorigin, radius, origin[3], Ry[9], Rz[9], Ryz[9];
char axis_type0[64], axis_scale0[64];

// Get axis type and scale (e.g. "equal"):
   jlp_gseg_axes1->GetAxisType(axis_type0);
   jlp_gseg_axes1->GetAxisScale(axis_scale0);

// Get current plot settings
   jlp_gseg_axes1->GetBoxSettings(&dev_x1_box, &dev_x2_box, &dev_y1_box,
                                  &dev_y2_box, &xmin, &xmax, &ymin, &ymax,
                                  &zmin, &zmax, &rmin, &rmax, &xscale, &yscale,
                                  &zscale, &rscale, &xorigin, &yorigin,
                                  &radius, origin, Ry, Rz, Ryz, &ncoords);

/* Calculate new data minimum and maximum values (user coordinates)*/
   x1 = xmin + (x1_window - dev_x1_box)/xscale;
   x2 = xmin + (x2_window - dev_x1_box)/xscale;
   y1 = ymin - (y1_window - dev_y2_box)/yscale;
   y2 = ymin - (y2_window - dev_y2_box)/yscale;

/* Save new data minimum and maximum values to p_data_min_max */
   jlp_gseg_data1->SetDataMinMax(x1, x2, y1, y2, zmin, zmax);

/* Set tick-label offsets to zero */
   jlp_gseg_axes1->SetTickOffsetLabels(0.0, 0.0, 0.0, 0.0, 0.0, 0.0);

/* Turn axis limits off */
   for ( i=1; i<=4; i++ ) {
      set_axis_limits0[i-1] = 0;
      axis_limits0[i-1] = 0.0;
      }
   for ( i=5; i<=6; i++ )
      set_axis_limits0[i-1] = 1;
   jlp_gseg_axes1->SetAxisLimitsOnOff(set_axis_limits0, 6);
   axis_limits0[4] = zmin;
   axis_limits0[5] = zmax;
   jlp_gseg_axes1->SetPlotParamAxisLimits(axis_limits0, 6);

   if ( strcmp(axis_type0, "linear")   == 0 ||
        strcmp(axis_type0, "semilogx") == 0 ||
        strcmp(axis_type0, "semilogy") == 0 ||
        strcmp(axis_type0, "loglog")   == 0 )
      {
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
      }


return;
}
