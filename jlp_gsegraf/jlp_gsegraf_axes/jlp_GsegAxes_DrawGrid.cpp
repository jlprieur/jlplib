/*******************************************************************************
*
* DrawGrid.c
*
* Draws linear or logarithmic grid lines.
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
#include "jlp_gseg_axes.h"

/****************************************************************************
* Load new values of the grid parameters
****************************************************************************/
void JLP_GsegAxes::LoadGridParam(char gridchar1_0, char gridchar2_0, 
                                 UINT32 gridcolor_0)
{
// Load values to private variables:
gridchar1 = gridchar1_0;
gridchar2 = gridchar2_0;
gridcolor1 = gridcolor_0;
}
/****************************************************************************
* Get the current values of the grid parameters
****************************************************************************/
void JLP_GsegAxes::GetGridParam(char *gridchar1_0, char *gridchar2_0, 
                                 UINT32 *gridcolor_0)
{
// Get values from private variables:
*gridchar1_0 = gridchar1;
*gridchar2_0 = gridchar2;
*gridcolor_0 = gridcolor1;
}
/****************************************************************************
*
****************************************************************************/
void JLP_GsegAxes::DrawGrid(char *axis_type,
                            double x11_screen, double y11_screen, 
                            double x12_screen, double y12_screen,
                            double x21_screen, double y21_screen, 
                            int nticks, double *tick_values,
                            double offset1, double offset2, int reversed_axis0)
{
 /* Declare variables */
 int i, ndecades;
 double xscale, yscale, axis_min, axis_max;
 JLP_CanvasPoints *points;


#ifdef DEBUG
printf("DrawGrid/input points: %f %f %f %f %f %f (nticks=%d)\n",
        x11_screen, y11_screen, x12_screen, y12_screen, x21_screen, y21_screen,
        nticks);
#endif

 /* Draw linear-axis grid lines */
 if ( strcmp(axis_type, "linear") == 0 )
    {
    axis_min = tick_values[0] - offset1;
    axis_max = tick_values[nticks-1] + offset2;
    xscale = (x12_screen - x11_screen)/(axis_max - axis_min);
    yscale = (y12_screen - y11_screen)/(axis_max - axis_min);

    points = jlp_canvas_points_new(2);
    for ( i=1; i<=nticks; i++ )
       {
       points->coords[0] = x11_screen + (tick_values[i-1] - axis_min)*xscale;
       points->coords[1] = y11_screen + (tick_values[i-1] - axis_min)*yscale;
       points->coords[2] = x21_screen + (tick_values[i-1] - axis_min)*xscale;
       points->coords[3] = y21_screen + (tick_values[i-1] - axis_min)*yscale;
       if ( gridchar1 == 'l' )
          jlp_gseg1->GSEG_DrawLine(points, gridcolor1, 1);
       else if ( gridchar1 == 'd' )
          jlp_gsegraf1->DrawDashedLine(points, gridcolor1, 1);
       else if ( gridchar1 == '.' )
          jlp_gsegraf1->DrawDottedLine(points, gridcolor1, 1);
       }
    jlp_canvas_points_free(points);
    }

 /* Draw logarithmic-axis grid lines */
 else if ( strcmp(axis_type, "log") == 0 )
    {
    ndecades = roundint(ceil(tick_values[nticks-1]) - floor(tick_values[0]));
    if ( ndecades <= 10 )
       nticks = ndecades + 1;

    xscale = (x12_screen - x11_screen)/(nticks - 1);
    yscale = (y12_screen - y11_screen)/(nticks - 1);

    points = jlp_canvas_points_new(2);
    for ( i=1; i<=nticks; i++ )
       {
       points->coords[0] = x11_screen + (i - 1)*xscale;
       points->coords[1] = y11_screen + (i - 1)*yscale;
       points->coords[2] = x21_screen + (i - 1)*xscale;
       points->coords[3] = y21_screen + (i - 1)*yscale;
       if ( gridchar1 == 'l' )
          jlp_gseg1->GSEG_DrawLine(points, gridcolor1, 1);
       else if ( gridchar1 == 'd' )
          jlp_gsegraf1->DrawDashedLine(points, gridcolor1, 1);
       else if ( gridchar1 == '.' )
          jlp_gsegraf1->DrawDottedLine(points, gridcolor1, 1);
       }
    jlp_canvas_points_free(points);
    }

return;
}
