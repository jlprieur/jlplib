/*******************************************************************************
*
* CheckParamData.c
*
* Checks plot-parameter data read from plot-parameter file.
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
* Version 15/04/2017
*******************************************************************************/
#include <ctype.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"    // JLP_GsegAxes class
#include "jlp_gseg_data.h"    // JLP_GsegData class

/******************************************************************************
*
*******************************************************************************/
int JLP_Gsegraf::CheckParamData(void)
{
int nplots0;
char axis_type0[64];

/* Get number of plots */
 jlp_gseg_data1->Get_nplots(&nplots0);
 if(nplots0 == 0) { 
   fprintf(stderr, "CheckParamData/Error: nplots1 = 0 !\n");
   return(-1);
   }

/* Check axes parameters */
  jlp_gseg_axes1->CheckAxesParameters();

/* Check data filenames */
  jlp_gseg_data1->CheckDataFilenames();

// Get axis type from JLP_GsegAxes object:
  jlp_gseg_axes1->GetAxisType(axis_type0);

/* Check plot_type parameters */
  jlp_gseg_data1->CheckPlotType(axis_type0);

// Check styles:
  jlp_gseg_data1->CheckStyleFlag(axis_type0);
  jlp_gseg_data1->CheckStyleChar(axis_type0);
  jlp_gseg_data1->CheckHistogramData(axis_type0);

return(0);
}
