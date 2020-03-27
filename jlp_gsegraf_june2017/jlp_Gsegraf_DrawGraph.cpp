/*******************************************************************************
* jlp_Gsegraf_DrawGraph.cpp
*
* Draws graph.
*
* Copyright © 2008, 2009, 2010, 2011 Spencer A. Buckner
* http://savannah.gnu.org/projects/gsegrafix
*
* This file is part of GSEGrafix, a scientific and engineering plotting program.
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
*******************************************************************************/
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"    // JLP_GsegAxes class

/***************************************************************************
*
***************************************************************************/
void JLP_Gsegraf::GSEG_DrawGraph (void)
{
char axis_type0[64], axis_scale0[64];

jlp_gseg_axes1->GetAxisType(axis_type0);
jlp_gseg_axes1->GetAxisScale(axis_scale0);

/* Draw graph */
   if ( strcmp(axis_type0, "linear")   == 0 ||
        strcmp(axis_type0, "semilogx") == 0 ||
        strcmp(axis_type0, "semilogy") == 0 ||
        strcmp(axis_type0, "loglog")   == 0 )
      {
/* Compute plot-box coordinates from current window size */
      jlp_gseg_axes1->SetPlotBoxDataLimitsFromWindowSize();

/* Adjust axes */
      if ( strcmp(axis_type0, "linear") == 0 &&
           strcmp(axis_scale0, "equal") == 0 )
         jlp_gseg_axes1->AxesEqual(1);

/* Draw Plot */
      jlp_gseg_axes1->DrawBackgroundImage();
      jlp_gseg_axes1->DrawGrid2d();
      jlp_gseg_axes1->DrawGridLog();
      PlotData2d();
      if(strcmp(axis_type0, "linear") == 0 ) {
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

   else if ( strcmp(axis_type0, "polar") == 0 )
      {
/* Draw Plot */
      jlp_gseg_axes1->DrawBackgroundImage();
      PolarPlot();
      DrawTheLegend();
      DrawExtraText();
      PlotImage();
      jlp_gseg_axes1->DrawDateTime();
      }

   else if ( strcmp(axis_type0, "3d") == 0 )
      {
/* Draw Plot */
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
  
return;
}
/***************************************************************************
* Add extra labels 
***************************************************************************/
void JLP_Gsegraf::GSEG_AddExtraLabel(char *string0, double x0, double y0,
                                     UINT32 fill_color_rgba0,
                                     UINT32 canvas_bg_color0, 
                                     const char *anchor0, 
                                     int raise_to_top0)
{
int i;

if(extra_labels1.nlabels < N_EXTRA_LABELS_MAXI - 1) {
 i = extra_labels1.nlabels;
// Save data to private variable extra_labels1
 extra_labels1.x[i] = x0;
 extra_labels1.y[i] = y0;
 strcpy(&(extra_labels1.string[128 * i]), string0);
 strcpy(&(extra_labels1.anchor[128 * i]), anchor0);
 extra_labels1.canvas_fg_color[i] = fill_color_rgba0; 
 extra_labels1.canvas_bg_color[i] = canvas_bg_color0;
 extra_labels1.raise_to_top[i] = raise_to_top0;
 extra_labels1.nlabels = i + 1;
 } else {
 fprintf(stderr,"GSEG_AddLabel/Error number maximum of labels is %d\n",
         N_EXTRA_LABELS_MAXI);
 }

return;
}
/***************************************************************************
* Draw extra labels if any
***************************************************************************/
void JLP_Gsegraf::GSEG_DrawExtraLabels(void)
{
int i, n_extra_labels0, raise_to_top0;
double x0, y0, x1, y1, x2, y2;
char string0[128], anchor0[128];
UINT32 fill_color_rgba0, canvas_bg_color0;

// Check if pointer is valid
if(jlp_gseg1 == NULL) return;

n_extra_labels0 = extra_labels1.nlabels;

for(i = 0; i < n_extra_labels0; i++) {

// Retrieve data from private variable extra_labels1
 x0 = extra_labels1.x[i];
 y0 = extra_labels1.y[i];
 strcpy(string0, &(extra_labels1.string[128 * i]));
 strcpy(anchor0, &(extra_labels1.anchor[128 * i]));
 fill_color_rgba0 = extra_labels1.canvas_fg_color[i];
 canvas_bg_color0 = extra_labels1.canvas_bg_color[i];
 raise_to_top0 = extra_labels1.raise_to_top[i];

// Draw the label to canvas
 jlp_gseg1->GSEG_DrawLabel(string0, x0, y0, 1, fill_color_rgba0, 
                           canvas_bg_color0, anchor0, raise_to_top0, 
                           &x1, &y1, &x2, &y2);

 }

return;
}
/***************************************************************************
* Erase all extra labels if any
***************************************************************************/
void JLP_Gsegraf::GSEG_EraseExtraLabels(void)
{
 extra_labels1.nlabels = 0;
}
