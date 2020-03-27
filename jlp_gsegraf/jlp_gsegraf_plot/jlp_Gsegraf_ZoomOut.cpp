/*******************************************************************************
* jlp_gsegraf_ZoomOut.cpp
*
* JLP
* Version 07/11/2016
*******************************************************************************/
#include <stdio.h>
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"      // JLP_GsegAxes class

/******************************************************************************
* Causes plot to zoom out to original plot.
* Display full size plot again
*
*******************************************************************************/
void JLP_Gsegraf::GSEG_ZoomOut (void)
{
char axis_type0[64];

// Get axis type and scale (e.g. "equal"):
 jlp_gseg_axes1->GetAxisType(axis_type0);

// Go back to original values (stored as "refrence" values)
 jlp_gseg_axes1->SetPlotSettingsToRefValues();

// Draw original plot
   if ( strcmp(axis_type0, "linear")   == 0 ||
        strcmp(axis_type0, "semilogx") == 0 ||
        strcmp(axis_type0, "semilogy") == 0 ||
        strcmp(axis_type0, "loglog")   == 0 )
      {

/* Draw plot */
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
      PlotExtraImage();
      jlp_gseg_axes1->DrawDateTime();
      }

   else if ( strcmp(axis_type0, "polar") == 0 )
      {
/* Draw plot */
      jlp_gseg_axes1->DrawBackgroundImage();
      PolarPlot();
      DrawTheLegend();
      DrawExtraText();
      PlotExtraImage();
      jlp_gseg_axes1->DrawDateTime();
      }

   else if ( strcmp(axis_type0, "3d") == 0 )
      {

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
      PlotExtraImage();
      jlp_gseg_axes1->DrawDateTime();
      }

   return;
   }
