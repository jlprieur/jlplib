/*******************************************************************************
*
* jlp_gseg_utils.cpp 
*
* Miscellaneous functions.
*
* Contains functions:
*    min
*    max
*    roundint
*    dot
*    cross
*    multiply_mv
*    multiply_mm
*    interp1
*    interp2
*    interp_rect
*    interp_color_1
*    interp_color_2
*    find_indices
*    search_compare_ascending
*    search_compare_descending
*    gseg_get_string
*    put_pixel
*
* Rotation matrices:
*
*    Rx = [1           0          0
*          0  cos(theta) sin(theta)
*          0 -sin(theta) cos(theta)]
*
*    Ry = [cos(theta) 0 -sin(theta)
*                   0 1           0
*          sin(theta) 0  cos(theta)]
*
*    Rz = [ cos(theta) sin(theta) 0
*          -sin(theta) cos(theta) 0
*                    0          0 1]
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
#include <stdio.h> 
#include <stdlib.h>          // bsearch()
#include <string.h>          // strcpy()
#include <ctype.h>
#include <math.h>
#include "jlp_gsegraf_defs.h"
#include "jlp_gseg_utils.h"   // Prototypes of functions included here
void JLP_ErrorDialog (const char *message_str);

double min ( int n, double *x )
   {
   int i;
   double xmin;

   xmin = x[0];
   for ( i=2; i<=n; i++ )
      if ( x[i-1] < xmin )
         xmin = x[i-1];

   return xmin;
   }


double max ( int n, double *x )
   {
   int i;
   double xmax;

   xmax = x[0];
   for ( i=2; i<=n; i++ )
      if ( x[i-1] > xmax )
         xmax = x[i-1];

   return xmax;
   }


int roundint ( double x )
   {
   int i;

   if ( x >= 0.0 )
      i = (int) (x + 0.5);
   else
      i = (int) (x - 0.5);

   return i;
   }


double dot ( double *vector1, double *vector2 )
   {
   /* Dot product of two vectors */

   double scalar_out;

   scalar_out = vector1[0]*vector2[0] + vector1[1]*vector2[1] + vector1[2]*vector2[2];

   return scalar_out;
   }


double * cross ( double *vector1, double *vector2 )
   {
   /* Cross product of two vectors */

   static double vector_out[3];

   vector_out[0] = vector1[1]*vector2[2] - vector1[2]*vector2[1];
   vector_out[1] = vector1[2]*vector2[0] - vector1[0]*vector2[2];
   vector_out[2] = vector1[0]*vector2[1] - vector1[1]*vector2[0];

   return &vector_out[0];
   }


double * multiply_mv ( double *matrix, double *vector )
   {
   /* Product of matrix and vector */
   /* Matrix elements labeled: [ 0 1 2; 3 4 5; 6 7 8 ] */

   static double vector_out[3];

   vector_out[0] = matrix[0]*vector[0] + matrix[1]*vector[1] + matrix[2]*vector[2];
   vector_out[1] = matrix[3]*vector[0] + matrix[4]*vector[1] + matrix[5]*vector[2];
   vector_out[2] = matrix[6]*vector[0] + matrix[7]*vector[1] + matrix[8]*vector[2];

   return &vector_out[0];
   }


double * multiply_mm ( double *matrix1, double *matrix2 )
   {
   /* Product of two matrices */
   /* Matrix elements labeled: [ 0 1 2; 3 4 5; 6 7 8 ] */

   static double matrix_out[9];

   matrix_out[0] = matrix1[0]*matrix2[0] + matrix1[1]*matrix2[3] + matrix1[2]*matrix2[6];
   matrix_out[1] = matrix1[0]*matrix2[1] + matrix1[1]*matrix2[4] + matrix1[2]*matrix2[7];
   matrix_out[2] = matrix1[0]*matrix2[2] + matrix1[1]*matrix2[5] + matrix1[2]*matrix2[8];

   matrix_out[3] = matrix1[3]*matrix2[0] + matrix1[4]*matrix2[3] + matrix1[5]*matrix2[6];
   matrix_out[4] = matrix1[3]*matrix2[1] + matrix1[4]*matrix2[4] + matrix1[5]*matrix2[7];
   matrix_out[5] = matrix1[3]*matrix2[2] + matrix1[4]*matrix2[5] + matrix1[5]*matrix2[8];

   matrix_out[6] = matrix1[6]*matrix2[0] + matrix1[7]*matrix2[3] + matrix1[8]*matrix2[6];
   matrix_out[7] = matrix1[6]*matrix2[1] + matrix1[7]*matrix2[4] + matrix1[8]*matrix2[7];
   matrix_out[8] = matrix1[6]*matrix2[2] + matrix1[7]*matrix2[5] + matrix1[8]*matrix2[8];

   return &matrix_out[0];
   }


void interp1 ( int n, int ni, double *x, double *y, double *xi, double *yi )
   {
   /********************************************************************************
   *
   * One-dimensional linear interpolation 
   *
   * Inputs
   *    n:  number of values in x and y vectors
   *    ni: number of values in xi and yi vectors
   *    x:  vector of x values
   *    y:  vector of y values
   *    xi: vector of interpolated values of x
   *
   * Output
   *    yi: vector of interpolated values of y
   *
   * Elements of x must be monotonically increasing or decreasing but need not be
   * equally spaced.
   * Elements of xi must be included in the range of x.
   *
   ********************************************************************************/

   int i, i1, i2;
   double f1, f2;

   for ( i=1; i<=ni; i++ )
      {
      /* Check limits */
      if ( x[0] < x[n-1] )
         {
         /* Elements of x increasing */
         if ( xi[i-1] <= x[0] )
            {
            yi[i-1] = y[0];
            continue;
            }
         else if ( xi[i-1] >= x[n-1] )
            {
            yi[i-1] = y[n-1];
            continue;
            }
         }
      else
         {
         /* Elements of x decreasing */
         if ( xi[i-1] >= x[0] )
            {
            yi[i-1] = y[0];
            continue;
            }
         else if ( xi[i-1] <= x[n-1] )
            {
            yi[i-1] = y[n-1];
            continue;
            }
         }

      /* Find indices of x bracketing xi[i-1] */
      i1 = find_indices(0, n-1, x, xi[i-1]);
      i2 = i1 + 1;

      /* Calculate weighting factors for y[i1] and y[i2] */
      f1 = (x[i2] - xi[i-1])/(x[i2] - x[i1]);
      f2 = 1.0 - f1;

      /* Calculate interpolated value of y */
      yi[i-1] = f1*y[i1] + f2*y[i2];
      }

   return;
   }


void interp2 ( int nx, int ny, int ni, double *x, double *y, double *z, double *xi, double *yi, double *zi )
   {
   /********************************************************************************
   *
   * Two-dimensional linear interpolation 
   *
   * Inputs
   *    nx: number of values in x vector
   *    ny: number of values in y vector
   *    x:  vector of x values
   *    y:  vector of y values
   *    z:  matrix of z values (nx rows, ny columns)
   *    xi: vector of interpolated values of x
   *    yi: vector of interpolated values of y
   *
   * Output
   *    zi: vector of interpolated values of z
   *
   * Elements of x and y must be monotonically increasing or decreasing but need
   * not be equally spaced.
   * Number of elements of xi must be equal to number of elements of yi.
   * Elements of xi and yi must be included in the ranges of x and y, respectively.
   *
   ********************************************************************************/

   int i, i1, i2, j1, j2;
   double z11, z12, z21, z22, dx, dy, dxi, dyi, dxidx, dyidy, one_dxidx, one_dyidy,
          f11, f12, f21, f22;

   for ( i=1; i<=ni; i++ )
      {
      /* Find indices of x bracketing xi */
      if ( x[0] < x[nx-1] )
         {
         /* Elements of x increasing */
         if ( xi[i-1] <= x[0] )
            i1 = 0;
         else if ( xi[i-1] >= x[nx-1] )
            i1 = nx - 2;
         else
            i1 = find_indices(0, nx-1, x, xi[i-1]);
         i2 = i1 + 1;
         }
      else
         {
         /* Elements of x decreasing */
         if ( xi[i-1] >= x[0] )
            i1 = 0;
         else if ( xi[i-1] <= x[nx-1] )
            i1 = nx - 2;
         else
            i1 = find_indices(0, nx-1, x, xi[i-1]);
         i2 = i1 + 1;
         }

      /* Find indices of y bracketing yi */
      if ( y[0] < y[ny-1] )
         {
         /* Elements of y increasing */
         if ( yi[i-1] <= y[0] )
            j1 = 0;
         else if ( yi[i-1] >= y[ny-1] )
            j1 = ny - 2;
         else
            j1 = find_indices(0, ny-1, y, yi[i-1]);
         j2 = j1 + 1;
         }
      else
         {
         /* Elements of y decreasing */
         if ( yi[i-1] >= y[0] )
            j1 = 0;
         else if ( yi[i-1] <= y[ny-1] )
            j1 = ny - 2;
         else
            j1 = find_indices(0, ny-1, y, yi[i-1]);
         j2 = j1 + 1;
         }

      /* Get values of z bracketing solution */
      z11 = z[ny*i1+j1];
      z12 = z[ny*i1+j2];
      z21 = z[ny*i2+j1];
      z22 = z[ny*i2+j2];

      /* Calculate weighting factors for each value of z */
      dxi = x[i2] - xi[i-1];
      dyi = y[j2] - yi[i-1];
      dx  = x[i2] - x[i1];
      dy  = y[j2] - y[j1];
      dxidx = dxi/dx;
      dyidy = dyi/dy;
      one_dxidx = 1.0 - dxidx;
      one_dyidy = 1.0 - dyidy;
      f11 = dxidx*dyidy;
      f12 = dxidx*one_dyidy;
      f21 = one_dxidx*dyidy;
      f22 = one_dxidx*one_dyidy;

      /* Calculate interpolated value of z */
      zi[i-1] = f11*z11 + f12*z12 + f21*z21 + f22*z22;
      }

   return;
   }


double interp_rect ( double x1, double x2, double y1, double y2, double xi )
   {
   /* Interpolate to find where a contour line intersects side of a rectangle */

   double f1, f2, yi;

   f1 = (x2 - xi)/(x2 - x1);
   f2 = 1.0 - f1;
   yi = f1*y1 + f2*y2;

   return yi;
   }


/******************************************************************************
*
* Interpolate colors from opaque blue (0x0000FFFF, fraction = 0.0)
* to opaque red (0xFF0000FF, fraction = 1.0)
*
******************************************************************************/
UINT32 interp_color_spectrum (double fraction0, int n_color_spectrum0,
                              UINT32 *color_spectrum0)
{
int index;
UINT32 color;

 index = roundint(fraction0 * (n_color_spectrum0 - 1));
 color = color_spectrum0[index];

 return color;
}

/*******************************************************************************
*
* Finds indices of values of monotonically increasing or decreasing x array
* that bracket xi.
*
* INPUT
*    index1: lowest index of x array to consider
*    index2: highest index of x array to consider
*    x:      vector of x values
*    xi:     value to be bracketed
*
* OUTPUT
*    i1:     lower index of x array that brackets xi
*
******************************************************************************/
double *pi1, *pi2;
int find_indices ( int index1, int index2, double *x, double xi )
   {
   int npts, i1;
   double *x1;

   /* Check need for search */
   if ( index2 - index1 < 2 )
      return index1;

   /* Do binary search */
   pi1 = &x[index1];
   pi2 = &x[index2];
   npts = index2 - index1 + 1;
/*********
* Calling the C library 
* void *bsearch(const void *key0, const void *base0, size_t nitems0, 
*               size_t size0, int (*compar)(const void *, const void *)) 
* function that searches an array of nitems0 objects, 
* the initial member of which is pointed to by base0, for a member 
* that matches the object pointed to, by key0. 
* The size of each member of the array is specified by size0.
*
* The contents of the array should be in ascending sorted order according 
* to the comparison function referenced by compar.
********/
   if ( x[index1] < x[index2] )
      x1 = (double *)bsearch(&xi, &x[index1], npts, sizeof(double), 
                             search_compare_ascending);
   else
      x1 = (double *)bsearch(&xi, &x[index1], npts, sizeof(double), 
                             search_compare_descending);
   i1 = (pi1 - &x[index1]) + index1;

   return i1;
   }


int search_compare_ascending ( const void *p1, const void *p2 )
   {
   double key = *(const double *) p1,
          x   = *(const double *) p2;

   if ( key < x )
      {
      pi2 = (double *) p2;
      if ( pi2 - pi1 == 1 )
         return 0;
      else
         return -1;
      }
   else if ( key > x )
      {
      pi1 = (double *) p2;
      if ( pi2 - pi1 == 1 )
         return 0;
      else
         return 1;
      }
   else
      {
      /* key = x */
      pi1 = (double *) p2 - 1;
      return 0;
      }
   }


int search_compare_descending ( const void *p1, const void *p2 )
   {
   double key = *(const double *) p1,
          x   = *(const double *) p2;

   if ( key > x )
      {
      pi2 = (double *) p2;
      if ( pi2 - pi1 == 1 )
         return 0;
      else
         return -1;
      }
   else if ( key < x )
      {
      pi1 = (double *) p2;
      if ( pi2 - pi1 == 1 )
         return 0;
      else
         return 1;
      }
   else
      {
      /* key = x */
      pi1 = (double *) p2 - 1;
      return 0;
      }
   }

/************************************************************************
*
************************************************************************/
char *gseg_get_string (char *string_get0, char *string, unsigned int *i1_str, 
                      unsigned int *i2_str, unsigned int *size, int flag )
   {
   /* Declare variables */
   int i, i1, i2, j;
   unsigned int strvalue;
   char *pchar = NULL;
   const char *error_str[] =
      { "Invalid Unicode specification;\n"
        "hexadecimal format is \\xhh;\n"
        "octal format is \\ooo.",
        "Invalid Unicode specification;\n"
        "maximum value is:\n"
        "   FF hexadecimal or\n"
        "   377 octal." };


   /* Get initial string */
   if(string_get0 != NULL) free(string_get0);
   string_get0 = NULL;
   string_get0 = new char[*size + 1];
   memset(string_get0, 0, *size + 1);
   strcpy(string_get0, string);


   /* Get string limits */
   i = 0;
   while ( (string_get0[i] == ' ' || string_get0[i] == '\t') && i < (int) *size )
      i++;

   if ( string_get0[i] == '#' || string_get0[i] != '\"' )
      return NULL;

   i1 = i + 1;
   if ( (pchar = strchr(&string_get0[i1], '\"')) == NULL )
      return NULL;
   *pchar = '\0';
   i2 = i1 + pchar - &string_get0[i1] - 1;


   /************************************************************************
   * Find escape sequences                                                 *
   * Used for text which may contain escape sequences.                     *
   * Not used for text which should not contain escape sequences.          *
   * Not used for file names, which can contain any character except '\0'. *
   ************************************************************************/
   if ( flag == 1 )
      for ( i=i1; i<=i2; i++ )
         {
         /* Find escaped instances of \ */
         if ( string_get0[i] == '\\' && string_get0[i+1] == '\\' )
            {
            for ( j=i; j<=i2; j++ )
               string_get0[j] = string_get0[j+1];
            i2--;
            }

          /* Find newline escape sequences \n */
         else if ( string_get0[i] == '\\' && string_get0[i+1] == 'n' )
            {
            string_get0[i] = '\n';
            for ( j=i+1; j<=i2; j++ )
               string_get0[j] = string_get0[j+1];
            i2--;
            }

         /* Find Unicode hexadecimal or octal escape sequences */
         else if ( string_get0[i] == '\\' && string_get0[i+1] != 'n' )
            {
            /* Check for hexadecimal or octal format */
            if ( string_get0[i+1] == 'x' &&
                 strchr("0123456789abcdefABCDEF", string_get0[i+2]) != NULL &&
                 strchr("0123456789abcdefABCDEF", string_get0[i+3]) != NULL )
               sscanf(&string_get0[i+2], "%2x", &strvalue);

            else if ( strchr("01234567", string_get0[i+1]) != NULL &&
                      strchr("01234567", string_get0[i+2]) != NULL &&
                      strchr("01234567", string_get0[i+3]) != NULL )
               sscanf(&string_get0[i+1], "%3o", &strvalue);

            else
               {
               JLP_ErrorDialog(error_str[0]);
               exit(1);
               }

            /* Check Unicode value */
            if ( strvalue > 255 )
               {
               JLP_ErrorDialog(error_str[1]);
               exit(1);
               }

            /* Modify string */
            string_get0[i] = strvalue;
            for ( j=i+1; j<=i2-3; j++ )
               string_get0[j] = string_get0[j+3];
            i2 = i2 - 3;
            }
         }
   string_get0[i2+1] = '\0';


   /* Calculate return variables */
   *i1_str = (unsigned int) i1;
   *i2_str = (unsigned int) i2;
   *size = (unsigned int) i2 - (unsigned int) i1 + 1;

   return &string_get0[i1];
   }


char * get_string_old ( char *string, unsigned int *size )
   {
   /* Declare variables */
   int i, i1, i2, j;
   unsigned int strvalue;
   char *pchar = NULL;
   const char *error_str[] =
      { "Invalid Unicode specification;\n"
        "hexadecimal format is \\xhh;\n"
        "octal format is \\ooo.",
        "Invalid Unicode specification;\n"
        "maximum value is:\n"
        "   FF hexadecimal or\n"
        "   377 octal." };


   /* Get string limits */
   i = 0;
   while ( (string[i] == ' ' || string[i] == '\t') && i < (int) *size )
      i++;

   if ( string[i] == '#' || string[i] != '\"' )
      return NULL;

   i1 = i + 1;
   if ( (pchar = strchr(&string[i1], '\"')) == NULL )
      return NULL;
   *pchar = '\0';
   i2 = i1 + pchar - &string[i1] - 1;


   /* Find escape sequences */
   for ( i=i1; i<=i2; i++ )
      {
      /* Find escaped instances of \ */
      if ( string[i] == '\\' && string[i+1] == '\\' )
         {
         for ( j=i; j<=i2; j++ )
            string[j] = string[j+1];
         i2--;
         }

       /* Find newline escape sequences \n */
      else if ( string[i] == '\\' && string[i+1] == 'n' )
         {
         string[i] = '\n';
         for ( j=i+1; j<=i2; j++ )
            string[j] = string[j+1];
         i2--;
         }

      /* Find Unicode hexadecimal or octal escape sequences */
      else if ( string[i] == '\\' && string[i+1] != 'n' )
         {
         /* Check for hexadecimal or octal format */
         if ( string[i+1] == 'x' &&
              strchr("0123456789abcdefABCDEF", string[i+2]) != NULL &&
              strchr("0123456789abcdefABCDEF", string[i+3]) != NULL )
            sscanf(&string[i+2], "%2x", &strvalue);

         else if ( strchr("01234567", string[i+1]) != NULL &&
                   strchr("01234567", string[i+2]) != NULL &&
                   strchr("01234567", string[i+3]) != NULL )
            sscanf(&string[i+1], "%3o", &strvalue);

         else
            {
            JLP_ErrorDialog(error_str[0]);
            exit(1);
            }

         /* Check Unicode value */
         if ( strvalue > 255 )
            {
            JLP_ErrorDialog(error_str[1]);
            exit(1);
            }

         /* Modify string */
         string[i] = strvalue;
         for ( j=i+1; j<=i2-3; j++ )
            string[j] = string[j+3];
         i2 = i2 - 3;
         }
      }
   string[i2+1] = '\0';


   /* Calculate string length */
   *size = (unsigned int) i2 - (unsigned int) i1 + 1;

   return &string[i1];
   }
