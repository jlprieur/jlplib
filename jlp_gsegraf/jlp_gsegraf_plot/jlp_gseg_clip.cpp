/*******************************************************************************
*
* Clip.c
*
* Contains functions:
*    Clip2d
*    ClipPolar
*    Clip3d
*
* Functions prevent data from being plotted outside plot boxes.
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

int Clip2d ( double xmin, double xmax, double ymin, double ymax, double *line_coords )
   {
   /* Declare variables */
   int i;
   double x1, x2, y1, y2;


   /* Copy input coordinates */
   x1 = line_coords[0];
   y1 = line_coords[1];
   x2 = line_coords[2];
   y2 = line_coords[3];


   /* Check coordinates in range */
   if ( xmin <= x1 && x1 <= xmax &&
        ymin <= y1 && y1 <= ymax &&
        xmin <= x2 && x2 <= xmax &&
        ymin <= y2 && y2 <= ymax )
      return 1;

   /* Check coordinates out of range */
   else if ( (x1 < xmin && x2 < xmin) ||
             (x1 > xmax && x2 > xmax) ||
             (y1 < ymin && y2 < ymin) ||
             (y1 > ymax && y2 > ymax) )
      return 0;

   /* Apply clip algorithm */
   else
      {
      for ( i=1; i<=3; i=i+2 )
         {
         if ( line_coords[i-1] < xmin )
            {
            line_coords[i-1] = xmin;
            line_coords[i]   = y1 + (y2 - y1)*(xmin - x1)/(x2 - x1);
            }

         else if ( line_coords[i-1] > xmax )
            {
            line_coords[i-1] = xmax;
            line_coords[i]   = y1 + (y2 - y1)*(xmax - x1)/(x2 - x1);
            }
         }

      for ( i=1; i<=3; i=i+2 )
         {
         if ( line_coords[i] < ymin )
            {
            line_coords[i-1] = x1 + (x2 - x1)*(ymin - y1)/(y2 - y1);
            line_coords[i]   = ymin;
            }

         else if ( line_coords[i] > ymax )
            {
            line_coords[i-1] = x1 + (x2 - x1)*(ymax - y1)/(y2 - y1);
            line_coords[i]   = ymax;
            }
         }

      if ( xmin <= line_coords[0] && line_coords[0] <= xmax &&
           ymin <= line_coords[1] && line_coords[1] <= ymax &&
           xmin <= line_coords[2] && line_coords[2] <= xmax &&
           ymin <= line_coords[3] && line_coords[3] <= ymax )
         return 1;
      else
         return 0;
      }
   }


int ClipPolar ( double rmin, double rmax, double *line_coords )
   {
   /* Declare variables */
   double theta1, theta2, r1, r2, x1, y1, x2, y2,
          cross_prod, a, b, c, A, B, C, cc, CC;


   /* Define variables */
   theta1 = line_coords[0];
   r1     = line_coords[1];
   theta2 = line_coords[2];
   r2     = line_coords[3];
   if ( r1 < rmin )
      {
      r1 = rmin;
      line_coords[1] = rmin;
      }
   if ( r2 < rmin )
      {
      r2 = rmin;
      line_coords[3] = rmin;
      }
   r1 = r1 - rmin;
   r2 = r2 - rmin;
   rmax = rmax - rmin;

   x1 = cos(theta1);
   y1 = sin(theta1);
   x2 = cos(theta2);
   y2 = sin(theta2);
   cross_prod = x1*y2 - x2*y1;


   /* Check if line coordinates in range */
   if ( r1 <= rmax && r2 <= rmax )
      return 1;

   /* Apply clip algorithm */
   else
      {
      /* Check if solution exists */
      C = acos(x1*x2 + y1*y2);
      a = r1;
      b = r2;
      c = sqrt(a*a + b*b - 2.0*a*b*cos(C));
      if ( r1 > rmax && r2 > rmax )
         {
         if ( C == 0.0 )
            return 0;

         A = acos((b*b + c*c - a*a)/(2.0*b*c));
         B = acos((a*a + c*c - b*b)/(2.0*a*c));
         if ( a*sin(B) >= rmax || C <= atan(c/rmax) )
            return 0;

         if ( C == PI )
            {
            line_coords[1] = rmin + rmax;
            line_coords[3] = rmin + rmax;
            return 1;
            }
         }

      /* Calculate new value of theta1 */
      if ( r1 > rmax )
         {
         if ( C == 0.0 || C == PI )
            {
            line_coords[1] = rmin + rmax;
            return 1;
            }

         a = r2;
         b = r1;
         A = acos((b*b + c*c - a*a)/(2.0*b*c));
         B = PI - A - C;

         b = rmax;
         cc = (2.0*a*cos(B) + sqrt(4.0*a*a*cos(B)*cos(B) - 4.0*(a*a - b*b)))/2.0;
         CC = acos((a*a + b*b - cc*cc)/(2.0*a*b));

         if ( cross_prod >= 0 )
            line_coords[0] = theta1 + C - CC;
         else
            line_coords[0] = theta1 - C + CC;
         line_coords[1] = rmin + rmax;
         }

      /* Calculate new value of theta2 */
      if ( r2 > rmax )
         {
         if ( C == 0.0 || C == PI )
            {
            line_coords[3] = rmin + rmax;
            return 1;
            }

         a = r1;
         b = r2;
         A = acos((b*b + c*c - a*a)/(2.0*b*c));
         B = PI - A - C;

         b = rmax;
         cc = (2.0*a*cos(B) + sqrt(4.0*a*a*cos(B)*cos(B) - 4.0*(a*a - b*b)))/2.0;
         CC = acos((a*a + b*b - cc*cc)/(2.0*a*b));

         if ( cross_prod >= 0 )
            line_coords[2] = theta2 - C + CC;
         else
            line_coords[2] = theta2 + C - CC;
         line_coords[3] = rmin + rmax;
         }

      return 1;
      }
   }


int Clip3d ( double xmin, double xmax, double ymin, double ymax, double zmin, double zmax,
             double *line_coords )
   {
   /* Declare variables */
   int i;
   double x1, x2, y1, y2, z1, z2;


   /* Copy input coordinates */
   x1 = line_coords[0];
   y1 = line_coords[1];
   z1 = line_coords[2];
   x2 = line_coords[3];
   y2 = line_coords[4];
   z2 = line_coords[5];


   /* Check coordinates in range */
   if ( xmin <= x1 && x1 <= xmax &&
        ymin <= y1 && y1 <= ymax &&
        zmin <= z1 && z1 <= zmax &&
        xmin <= x2 && x2 <= xmax &&
        ymin <= y2 && y2 <= ymax &&
        zmin <= z2 && z2 <= zmax )
      return 1;

   /* Check coordinates out of range */
   else if ( (x1 < xmin && x2 < xmin) ||
             (x1 > xmax && x2 > xmax) ||
             (y1 < ymin && y2 < ymin) ||
             (y1 > ymax && y2 > ymax) ||
             (z1 < zmin && z2 < zmin) ||
             (z1 > zmax && z2 > zmax) )
      return 0;

   /* Apply clip algorithm */
   else
      {
      for ( i=1; i<=4; i=i+3 )
         {
         if ( line_coords[i-1] < xmin )
            {
            line_coords[i-1] = xmin;
            line_coords[i]   = y1 + (y2 - y1)*(xmin - x1)/(x2 - x1);
            line_coords[i+1] = z1 + (z2 - z1)*(xmin - x1)/(x2 - x1);
            }

         else if ( line_coords[i-1] > xmax )
            {
            line_coords[i-1] = xmax;
            line_coords[i]   = y1 + (y2 - y1)*(xmax - x1)/(x2 - x1);
            line_coords[i+1] = z1 + (z2 - z1)*(xmax - x1)/(x2 - x1);
            }
         }

      for ( i=1; i<=4; i=i+3 )
         {
         if ( line_coords[i] < ymin )
            {
            line_coords[i-1] = x1 + (x2 - x1)*(ymin - y1)/(y2 - y1);
            line_coords[i]   = ymin;
            line_coords[i+1] = z1 + (z2 - z1)*(ymin - y1)/(y2 - y1);
            }

         else if ( line_coords[i] > ymax )
            {
            line_coords[i-1] = x1 + (x2 - x1)*(ymax - y1)/(y2 - y1);
            line_coords[i]   = ymax;
            line_coords[i+1] = z1 + (z2 - z1)*(ymax - y1)/(y2 - y1);
            }
         }

      for ( i=1; i<=4; i=i+3 )
         {
         if ( line_coords[i+1] < zmin )
            {
            line_coords[i-1] = x1 + (x2 - x1)*(zmin - z1)/(z2 - z1);
            line_coords[i]   = y1 + (y2 - y1)*(zmin - z1)/(z2 - z1);
            line_coords[i+1] = zmin;
            }

         else if ( line_coords[i+1] > zmax )
            {
            line_coords[i-1] = x1 + (x2 - x1)*(zmax - z1)/(z2 - z1);
            line_coords[i]   = y1 + (y2 - y1)*(zmax - z1)/(z2 - z1);
            line_coords[i+1] = zmax;
            }
         }

      if ( xmin <= line_coords[0] && line_coords[0] <= xmax &&
           ymin <= line_coords[1] && line_coords[1] <= ymax &&
           zmin <= line_coords[2] && line_coords[2] <= zmax &&
           xmin <= line_coords[3] && line_coords[3] <= xmax &&
           ymin <= line_coords[4] && line_coords[4] <= ymax &&
           zmin <= line_coords[5] && line_coords[5] <= zmax )
         return 1;
      else
         return 0;
      }
   }
