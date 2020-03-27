/*******************************************************************************
*
* jlp_gsegraf_InitializeVariables.c
*
* Initializes plot variables.
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
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"            // JLP_GsegAxes class
#include "jlp_gseg_data.h"            // JLP_GsegData class

/***************************************************************************
*
****************************************************************************/
void JLP_Gsegraf::GSEG_InitializeVariables (void)
{
/* Declare variables */
int i, x00, y00;

// Size of window (from Gnome interface for instance)
  jlp_gseg1->GSEG_GetWindowLimits(&x00, &window_width1, &y00, &window_height1);

// Initialization of extra labels
  extra_labels1.nlabels = 0;

/* Set pointers to NULL */
   line1                  = NULL;
   string_get            = NULL;

/* Allocate memory for plot parameters */
   stemflags           = new char[4*NPLOTS_MAXI];
   stemvalues          = new double[NPLOTS_MAXI];
   memset(stemflags,   0, 4*NPLOTS_MAXI*sizeof(char));

   /****************************************************************************
   *
   * Calculate all colors with maximum saturation from blue to green to red
   *    used for:
   *       color plots
   *       contour-line colors of 2d contour plots with plot_style = "auto"
   *       upper-surface colors of 3d mesh plots with plot_style = "auto"
   *
   ****************************************************************************/
   n_color_spectrum_1 = 1021;
   color_spectrum_1[0] = 0x0000FFFF;                        /*               0x0000FFFF; index =           0 */
   for ( i=1; i<= 255; i++ )
      color_spectrum_1[i]     = 0x0000FFFF + 0x10000*i;     /* 0x0001FFFF to 0x00FFFFFF; index =   1 to  255 */
   for ( i=1; i<= 255; i++ )
      color_spectrum_1[255+i] = 0x00FFFFFF - 0x100*i;       /* 0x00FFFEFF to 0x00FF00FF; index = 256 to  510 */
   for ( i=1; i<= 255; i++ )
      color_spectrum_1[510+i] = 0x00FF00FF + 0x1000000*i;   /* 0x01FF00FF to 0xFFFF00FF; index = 511 to  765 */
   for ( i=1; i<= 255; i++ )
      color_spectrum_1[765+i] = 0xFFFF00FF - 0x10000*i;     /* 0xFFFE00FF to 0xFF0000FF; index = 766 to 1020 */


   /****************************************************************************
   *
   * Calculate all colors with 75% maximum saturation from blue to green to red
   *    used for lower-surface colors of 3d mesh plots with plot_style = "auto"
   *
   ****************************************************************************/
   n_color_spectrum_2 = 769;
   color_spectrum_2[0] = 0x0000C0FF;                        /*               0x0000C0FF; index =          0 */
   for ( i=1; i<= 192; i++ )
      color_spectrum_2[i]     = 0x0000C0FF + 0x10000*i;     /* 0x0001C0FF to 0x00C0C0FF; index =   1 to 192 */
   for ( i=1; i<= 192; i++ )
      color_spectrum_2[192+i] = 0x00C0C0FF - 0x100*i;       /* 0x00C0BFFF to 0x00C000FF; index = 193 to 384 */
   for ( i=1; i<= 192; i++ )
      color_spectrum_2[384+i] = 0x00C000FF + 0x1000000*i;   /* 0x01C000FF to 0xC0C000FF; index = 385 to 576 */
   for ( i=1; i<= 192; i++ )
      color_spectrum_2[576+i] = 0xC0C000FF - 0x10000*i;     /* 0xC0BF00FF to 0xC00000FF; index = 577 to 768 */

   return;
   }
