/*******************************************************************************
*
* jlp_Gsegraf_DrawContours3d.cpp
*
* Plots 3-dimensional contour data for polygons that are not truncated.
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
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"     // JLP_GsegAxes class
#include "jlp_gseg_data.h"     // JLP_GsegData class

/***********************************************************************
*
***********************************************************************/
void JLP_Gsegraf::DrawContours3d(int iplot, double *xpoints, 
                                 double *ypoints, double *zpoints)
{
/* Declare variables */
int i, j, ic, nc, flag, nzvalues, ncontours0;
unsigned int pen_width0;
double *p, origin[3], Ry[9], Rz[9], Ryz[9];
double xmin, xmax, ymin, ymax, zmin, zmax, xscale, yscale, zscale;
double xtest[8], ytest[8], ztest[8], zmin_polygon, zmax_polygon, contour;
double x1, y1, z1, x2, y2, z2, r1[3], r2[3], length, dx, dy;
UINT32 contour_color0;
JLP_CanvasPoints *points;

 jlp_gseg_data1->GetContour3dColor(iplot, &contour_color0);
 jlp_gseg_data1->GetStyleSize(iplot, &pen_width0);

// Get plot settings 
 jlp_gseg_axes1->GetBoxSettingsFor3d(origin, Ry, Rz, Ryz, &xmin, &xmax, &ymin,
                                     &ymax, &zmin, &zmax, &xscale, &yscale,
                                     &zscale);


/* Define polygon test points */
for ( i=1; i<=4; i++ )
   {
   xtest[i-1] = xpoints[i-1];
   ytest[i-1] = ypoints[i-1];
   ztest[i-1] = zpoints[i-1];
   xtest[i+3] = xpoints[i-1];
   ytest[i+3] = ypoints[i-1];
   ztest[i+3] = zpoints[i-1];
   }

/* Find polygon zmin and zmax */
  zmin_polygon = min(4, zpoints);
  zmax_polygon = max(4, zpoints);

/* Calculate number of contour lines */
  jlp_gseg_axes1->GetNzValues(&nzvalues);
  ncontours0 = jlp_gseg_data1->NContours(iplot);
  if (ncontours0 < 2 ) {
     nc = 2*nzvalues - 1;
   } else {
     nc = ncontours0;
   }


/* Calculate contour-line coordinates */
points = jlp_canvas_points_new(2);
// Loop on all the countours:
for ( ic=1; ic<=nc; ic++ )
   {
   /* Calculate contour value */
   contour = zmin + (ic-1)*(zmax - zmin)/(nc - 1);


   /* Check if all points equal */
   if ( ztest[0] == ztest[1] && ztest[0] == ztest[2] && ztest[0] == ztest[3] )
      continue;


/* Check if contour line goes through polygon */
   if ( zmin_polygon < contour && contour < zmax_polygon )
      {
      /* Find coordinates of intersecting contour line */
      flag = 0;
      for ( i=1; i<=4; i++ )
         {
         /* Case 1: two adjacent sides */
         if ( ztest[i-1] < contour && ztest[i] > contour 
              && ztest[i+1] < contour && ztest[i+2] < contour )
            {
            x1 = interp_rect(ztest[i-1], ztest[i], xtest[i-1], xtest[i], contour);
            y1 = interp_rect(ztest[i-1], ztest[i], ytest[i-1], ytest[i], contour);
            z1 = contour;
            x2 = interp_rect(ztest[i], ztest[i+1], xtest[i], xtest[i+1], contour);
            y2 = interp_rect(ztest[i], ztest[i+1], ytest[i], ytest[i+1], contour);
            z2 = contour;
            flag = 1;
            break;
            }

         else if ( ztest[i-1] > contour && ztest[i] < contour && ztest[i+1] > contour && ztest[i+2] > contour )
            {
            x1 = interp_rect(ztest[i-1], ztest[i], xtest[i-1], xtest[i], contour);
            y1 = interp_rect(ztest[i-1], ztest[i], ytest[i-1], ytest[i], contour);
            z1 = contour;
            x2 = interp_rect(ztest[i], ztest[i+1], xtest[i], xtest[i+1], contour);
            y2 = interp_rect(ztest[i], ztest[i+1], ytest[i], ytest[i+1], contour);
            z2 = contour;
            flag = 1;
            break;
            }


         /* Case 2: two opposite sides */
         else if ( ztest[i-1] < contour && ztest[i] > contour && ztest[i+1] > contour && ztest[i+2] < contour )
            {
            x1 = interp_rect(ztest[i-1], ztest[i], xtest[i-1], xtest[i], contour);
            y1 = interp_rect(ztest[i-1], ztest[i], ytest[i-1], ytest[i], contour);
            z1 = contour;
            x2 = interp_rect(ztest[i+1], ztest[i+2], xtest[i+1], xtest[i+2], contour);
            y2 = interp_rect(ztest[i+1], ztest[i+2], ytest[i+1], ytest[i+2], contour);
            z2 = contour;
            flag = 1;
            break;
            }

         else if ( ztest[i-1] > contour && ztest[i] < contour && ztest[i+1] < contour && ztest[i+2] > contour )
            {
            x1 = interp_rect(ztest[i-1], ztest[i], xtest[i-1], xtest[i], contour);
            y1 = interp_rect(ztest[i-1], ztest[i], ytest[i-1], ytest[i], contour);
            z1 = contour;
            x2 = interp_rect(ztest[i+1], ztest[i+2], xtest[i+1], xtest[i+2], contour);
            y2 = interp_rect(ztest[i+1], ztest[i+2], ytest[i+1], ytest[i+2], contour);
            z2 = contour;
            flag = 1;
            break;
            }


         /* Case 3: both pairs opposite sides */
         else if ( ztest[i-1] < contour && ztest[i] > contour && ztest[i+1] < contour && ztest[i+2] > contour )
            {
            x1 = interp_rect(ztest[i-1], ztest[i], xtest[i-1], xtest[i], contour);
            y1 = interp_rect(ztest[i-1], ztest[i], ytest[i-1], ytest[i], contour);
            z1 = contour;
            x2 = interp_rect(ztest[i+1], ztest[i+2], xtest[i+1], xtest[i+2], contour);
            y2 = interp_rect(ztest[i+1], ztest[i+2], ytest[i+1], ytest[i+2], contour);
            z2 = contour;
            flag = 1;
            break;
            }

         else if ( ztest[i-1] > contour && ztest[i] < contour && ztest[i+1] > contour && ztest[i+2] < contour )
            {
            x1 = interp_rect(ztest[i-1], ztest[i], xtest[i-1], xtest[i], contour);
            y1 = interp_rect(ztest[i-1], ztest[i], ytest[i-1], ytest[i], contour);
            z1 = contour;
            x2 = interp_rect(ztest[i+1], ztest[i+2], xtest[i+1], xtest[i+2], contour);
            y2 = interp_rect(ztest[i+1], ztest[i+2], ytest[i+1], ytest[i+2], contour);
            z2 = contour;
            flag = 1;
            break;
            }


         /* Case 4: one point */
         else if ( ztest[i-1] == contour && ztest[i] > contour && ztest[i+1] < contour && ztest[i+2] < contour )
            {
            x1 = xtest[i-1];
            y1 = ytest[i-1];
            z1 = contour;
            x2 = interp_rect(ztest[i], ztest[i+1], xtest[i], xtest[i+1], contour);
            y2 = interp_rect(ztest[i], ztest[i+1], ytest[i], ytest[i+1], contour);
            z2 = contour;
            flag = 1;
            break;
            }

         else if ( ztest[i-1] == contour && ztest[i] < contour && ztest[i+1] > contour && ztest[i+2] > contour )
            {
            x1 = xtest[i-1];
            y1 = ytest[i-1];
            z1 = contour;
            x2 = interp_rect(ztest[i], ztest[i+1], xtest[i], xtest[i+1], contour);
            y2 = interp_rect(ztest[i], ztest[i+1], ytest[i], ytest[i+1], contour);
            z2 = contour;
            flag = 1;
            break;
            }

         else if ( ztest[i-1] == contour && ztest[i] < contour && ztest[i+1] < contour && ztest[i+2] > contour )
            {
            x1 = xtest[i-1];
               y1 = ytest[i-1];
            z1 = contour;
            x2 = interp_rect(ztest[i+1], ztest[i+2], xtest[i+1], xtest[i+2], contour);
            y2 = interp_rect(ztest[i+1], ztest[i+2], ytest[i+1], ytest[i+2], contour);
            z2 = contour;
            flag = 1;
            break;
            }

         else if ( ztest[i-1] == contour && ztest[i] > contour && ztest[i+1] > contour && ztest[i+2] < contour )
            {
            x1 = xtest[i-1];
            y1 = ytest[i-1];
            z1 = contour;
            x2 = interp_rect(ztest[i+1], ztest[i+2], xtest[i+1], xtest[i+2], contour);
            y2 = interp_rect(ztest[i+1], ztest[i+2], ytest[i+1], ytest[i+2], contour);
            z2 = contour;
            flag = 1;
            break;
            }


         /* Case 5: two adjacent points */
         else if ( ztest[i-1] == contour && ztest[i] == contour && ztest[i+1] != contour && ztest[i+2] != contour )
            {
            x1 = xtest[i-1];
            y1 = ytest[i-1];
            z1 = contour;
            x2 = xtest[i];
            y2 = ytest[i];
            z2 = contour;
            flag = 1;
            break;
            }


         /* Case 6: two opposite points */
         else if ( ztest[i-1] == contour && ztest[i] != contour && ztest[i+1] == contour && ztest[i+2] != contour )
            {
            x1 = xtest[i-1];
            y1 = ytest[i-1];
            z1 = contour;
            x2 = xtest[i+1];
            y2 = ytest[i+1];
            z2 = contour;
            flag = 1;
            break;
            }


         /* Case 7: three points */
         else if ( ztest[i-1] == contour && ztest[i] == contour && ztest[i+1] == contour && ztest[i+2] != contour )
            {
            x1 = xtest[i-1];
            y1 = ytest[i-1];
            z1 = contour;
            x2 = xtest[i+1];
            y2 = ytest[i+1];
            z2 = contour;
            flag = 1;
            break;
            }
         }


/* Check for solution */
      if ( flag == 0 )
         {
         fprintf(stderr, "DrawContours3d/Finding coordinates of intersecting contour line\n");
         fprintf(stderr, "DrawContours3d/error:  flag=0 : no solution\n");
         return;
         }


      /* Calculate contour-line position vectors */
      r1[0] = (x1 - xmin)*xscale;
      r1[1] = (y1 - ymin)*yscale;
      r1[2] = (z1 - zmin)*zscale;

      r2[0] = (x2 - xmin)*xscale;
      r2[1] = (y2 - ymin)*yscale;
      r2[2] = (z2 - zmin)*zscale;


      /* Rotate contour-line position vectors */
      p = multiply_mv(Ryz, &r1[0]);
      for ( j=1; j<=3; j++, p++ )
         r1[j-1] = *p;

      p = multiply_mv(Ryz, &r2[0]);
      for ( j=1; j<=3; j++, p++ )
         r2[j-1] = *p;


      /* Draw contour line */
      x1 = origin[1] + r1[1];
      y1 = origin[2] - r1[2];
      x2 = origin[1] + r2[1];
      y2 = origin[2] - r2[2];
      length = sqrt((x2 - x1)*(x2 - x1) + (y2 - y1)*(y2 - y1));
      dx = (x2 - x1)/length;
      dy = (y2 - y1)/length;
      points->coords[0] = x1 - dx;
      points->coords[1] = y1 - dy;
      points->coords[2] = x2 + dx;
      points->coords[3] = y2 + dy;
// Draw contours:
      jlp_gseg1->GSEG_DrawLine(points, contour_color0, pen_width0);
      }
   }

 jlp_canvas_points_free(points);

return;
}
