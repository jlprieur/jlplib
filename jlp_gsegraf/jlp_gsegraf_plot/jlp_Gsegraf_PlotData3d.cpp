/*******************************************************************************
*
* jlp_Gsegraf_PlotData3d.cpp
*
* Plots a two-dimensional projection of three-dimensional data.
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
* JLP
* Version 17/04/2017
*******************************************************************************/
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"     // JLP_GsegAxes class
#include "jlp_gseg_data.h"     // JLP_GsegData class

/***********************************************************************
*
***********************************************************************/
void JLP_Gsegraf::PlotData3d (void)
{
/* Declare variables */
int iplot, nplots0, gseg_plot_type0;;

/* Plot data */
   jlp_gseg_data1->Get_nplots(&nplots0);


   for ( iplot=1; iplot<=nplots0; iplot++ )
      {
/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
      jlp_gseg_data1->GetGsegPlotType(iplot, &gseg_plot_type0);
// 1="points"
      if(gseg_plot_type0 == 1)
         {
/* Plot 3d line data */
         PlotPoints3d(iplot);
         }
// 3="contour"
      else if(gseg_plot_type0 == 3)
         {
/* Plot 3d contour data */
         ContourPlot3d(iplot);
         }
// 4="color"
      else if(gseg_plot_type0 == 4)
         {
/* Plot 3d color data */
         ColorPlot3d(iplot);
         }
// 5="mesh"
      else if(gseg_plot_type0 == 5)
         {
/* Plot 3d mesh data */
         MeshPlot3d(iplot);
         }
      }


   return;
   }
