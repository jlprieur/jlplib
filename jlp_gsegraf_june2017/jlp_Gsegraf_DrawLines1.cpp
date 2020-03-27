/*******************************************************************************
*
* DrawLines1.cpp
*
* Contains functions:
*    DrawDashedLine
*    DrawDottedLine
*
*******************************************************************************/
#include <math.h>
#include "jlp_gsegraf.h"

/**************************************************************************
*
**************************************************************************/
void JLP_Gsegraf::DrawDashedLine(JLP_CanvasPoints *points, UINT32 fill_color_rgba, 
                    unsigned int line_width )
{
/* Declare variables */
int i, j, j1, j2, k, k1, k2, ipts, npoints, nlines;
double *x, *y, *length, length_total, inc, fspace, factor;
double xinterp[2], yinterp[2], length_interp[2];
JLP_CanvasPoints *pts;

/* Check linewidth */
 if ( line_width == 0 ) return;

/* Calculate line length */
 npoints = points->num_points;
 x = new double[npoints];
 y = new double[npoints];
 length = new double[npoints];
 x[0] = points->coords[0];
 y[0] = points->coords[1];
 length[0] = 0.0;
 for(i = 1; i < npoints; i++) {
    x[i] = points->coords[2*i];
    y[i] = points->coords[2*i+1];
    length[i] = length[i-1] +
                sqrt((x[i] - x[i-1])*(x[i] - x[i-1]) +
                     (y[i] - y[i-1])*(y[i] - y[i-1]));
    }
 length_total = length[npoints-1];

 /* Draw short lines */
 if(length_total < 2.0 ) {
    delete[] x;
    delete[] y;
    delete[] length;
      return;
      }
   else if ( length_total <= dash1 )
      {
      jlp_gseg1->GSEG_DrawLine(points, fill_color_rgba, line_width);
      delete[] x;
      delete[] y;
      delete[] length;
      return;
      }


   /* Recalculate dash and space lengths */
   inc = dash1 + space_dash1;
   fspace = space_dash1 / inc;
   if ( sqrt((x[npoints-1] - x[0])*(x[npoints-1] - x[0]) +
             (y[npoints-1] - y[0])*(y[npoints-1] - y[0])) >= space_dash1 )
      {
      /* Line ends with dash */
      nlines = roundint((length_total + space_dash1)/inc);
      inc = length_total/(nlines - fspace);
      factor = length_total + fspace*inc;
      }
   else
      {
      /* Line ends with space */
      nlines = roundint(length_total/inc);
      inc = length_total/nlines;
      factor = length_total;
      }


   /* Interpolate and draw dashed line */
   j1 = 2;
   j2 = npoints - 1;
   for ( i=1; i<=nlines; i++ )
      {
      /* Find coordinates for dash beginning and end */
      length_interp[0] = (((double) i - 1.0)/nlines)*factor;
      length_interp[1] = (((double) i - fspace)/nlines)*factor;

      interp1(npoints, 2, length, x, &length_interp[0], &xinterp[0]);
      interp1(npoints, 2, length, y, &length_interp[0], &yinterp[0]);

      /* Find data points contained in dash */
      k1 = j1;
      k2 = j2;
      for ( j=j1; j<=j2; j++ )
         {
         if ( length[j-1] <= length_interp[0] )
            k1 = j + 1;
         if ( length[j-1] >= length_interp[1] )
            {
            k2 = j - 1;
            j1 = j;
            break;
            }
         }

      /* Calculate points-structure components for dash */
      pts = jlp_canvas_points_new(k2 - k1 + 3);
      ipts = 0;
      pts->coords[0] = xinterp[0];
      pts->coords[1] = yinterp[0];
      for ( k=k1; k<=k2; k++ )
         {
         ipts++;
         pts->coords[2*ipts]   = x[k-1];
         pts->coords[2*ipts+1] = y[k-1];
         }
      ipts++;
      pts->coords[2*ipts]   = xinterp[1];
      pts->coords[2*ipts+1] = yinterp[1];

      /* Draw dash */
      jlp_gseg1->GSEG_DrawLine(pts, fill_color_rgba, line_width);
      jlp_canvas_points_free(pts);
      }

   delete[] x;
   delete[] y;
   delete[] length;
   }

/**************************************************************************
*
**************************************************************************/
void JLP_Gsegraf::DrawDottedLine(JLP_CanvasPoints *points, UINT32 fill_color_rgba, 
                    unsigned int size )
{
/* Declare variables */
int i, npoints, ndots;
double *x, *y, *length, length_total, inc, inc_calc; 
double length_interp, xinterp, yinterp;

/* Check size */
 if(size == 0) return;

/* Calculate line length */
  npoints = points->num_points;
  x = new double[npoints];
  y = new double[npoints];
  length = new double[npoints];
  x[0] = points->coords[0];
  y[0] = points->coords[1];
  length[0] = 0.0;
  for(i = 1; i < npoints; i++ ) {
     x[i] = points->coords[2*i];
     y[i] = points->coords[2*i+1];
     length[i] = length[i-1] +
                 sqrt((x[i] - x[i-1])*(x[i] - x[i-1]) +
                      (y[i] - y[i-1])*(y[i] - y[i-1]));
     }
  length_total = length[npoints-1];

/* Check line length */
  inc = space_dot1 + size;
  if(length_total < inc ) {
     delete[] x;
     delete[] y;
     delete[] length;
     return;
     }

/* Calculate number of dots */
  ndots = roundint(length_total/inc + 1.0);
  inc_calc = length_total/(ndots - 1);

/* Interpolate and draw dots */
  for(i = 1; i <= ndots; i++ ) {
     length_interp = length[0] + (i - 1)*inc_calc;
     interp1(npoints, 1, length, x, &length_interp, &xinterp);
     interp1(npoints, 1, length, y, &length_interp, &yinterp);
     jlp_gseg1->GSEG_DrawCircle(xinterp, yinterp, size, fill_color_rgba, 
                     fill_color_rgba, 1);
     }

  delete[] x;
  delete[] y;
  delete[] length;

}
