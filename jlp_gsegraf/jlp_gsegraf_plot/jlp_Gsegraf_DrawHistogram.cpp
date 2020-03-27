/******************************************************************************
*
* jlp_Gseghraf_DrawHistogram.c
*
* Calculate and draw histogram of input data.
*
* JLP
* Version 15/04/2017
******************************************************************************/
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gsegraf_defs.h"
#include "jlp_gseg_axes.h"           // JLP_GsegAxes class
#include "jlp_gseg_data.h"           // JLP_GsegData class

/*******************************************************************
*
* INPUT:
*  iplot : plot index (from 1 to number of plots)
*
**********************************************************************/
void JLP_Gsegraf::DrawHistogram ( int iplot )
{
/* Declare variables */
int i, npts, nbins;
unsigned style_size0;
double dev_x1_box0, dev_y1_box0, dev_x2_box0, dev_y2_box0,
       xmin0, xmax0, ymin0, ymax0, zmin0, zmax0, xscale0, yscale0,
       *yhist, x1_bar, y1_bar, x2_bar, y2_bar,
       x1, y1, x2, y2, xline, binmin, binmax, binwidth;
char histo_type0[64], style_char10;;
UINT32 outline_color_rgba0, fill_color_rgba0, canvas_bg_color0;
JLP_CanvasPoints *points;

 canvas_bg_color0 = jlp_gseg_axes1->canvas_bg_color();

// Get style parameters of plot:
 jlp_gseg_data1->GetStyleChar1(iplot, &style_char10);
 jlp_gseg_data1->GetStyleSize(iplot, &style_size0);
 jlp_gseg_data1->GetOutlineColor(iplot, &outline_color_rgba0);
 jlp_gseg_data1->GetFillColor(iplot, &fill_color_rgba0);

// Comput the histogram and return the current settings of iplot
jlp_gseg_data1->ComputeHistogram(iplot, &yhist, &nbins, &npts,
                                 &binmin, &binmax, &binwidth, histo_type0);

/* Get axis minimum and maximum values */
jlp_gseg_axes1->GetBoxSettingsForLinear(&dev_x1_box0, &dev_x2_box0, 
                                      &dev_y1_box0, &dev_y2_box0, &xmin0, 
                                      &xmax0, &ymin0, &ymax0, &zmin0, 
                                      &zmax0, &xscale0, &yscale0);

/* Plot histogram */
points = jlp_canvas_points_new(2);
for ( i=1; i<=nbins; i++ )
   {
   y1_bar = 0.0;
   if ( strcmp(histo_type0, "number") == 0 )
      y2_bar = yhist[i-1];
   else if ( strcmp(histo_type0, "fraction") == 0 )
      y2_bar = yhist[i-1]/npts;
   else if ( strcmp(histo_type0, "percent") == 0 )
      y2_bar = 100.0*yhist[i-1]/npts;

   if ( y1_bar >= ymax0 || y2_bar <= ymin0 )
      continue;
   if ( y1_bar < ymin0 )
       y1_bar = ymin0;
   if ( y2_bar > ymax0 )
       y2_bar = ymax0;

   y1 = dev_y2_box0 - (y1_bar - ymin0)*yscale0;
   y2 = dev_y2_box0 - (y2_bar - ymin0)*yscale0;

// Style 'b' or 'B' with bars 
   if ( style_char10 == 'b' || style_char10 == 'B' )
      {
/* Draw bars */
      x1_bar = binmin + (i - 1.0)/(nbins - 1.0)*(binmax - binwidth - binmin);
      x2_bar = x1_bar + binwidth;

      if ( x1_bar >= xmax0 || x2_bar <= xmin0 )
         continue;
      if ( x1_bar < xmin0 )
          x1_bar = xmin0;
      if ( x2_bar > xmax0 )
          x2_bar = xmax0;

      x1 = dev_x1_box0 + (x1_bar - xmin0)*xscale0;
      x2 = dev_x1_box0 + (x2_bar - xmin0)*xscale0;

      DrawBar(x1+1.0, y1, x2-1.0, y2, fill_color_rgba0, 
              outline_color_rgba0);
      DrawBar(x1, y1+1.0, x2, y2-1.0, 0xFFFFFF00, canvas_bg_color0);
      }
// Style 'l' with lines 
   else if ( style_char10 == 'l' )
      {
/* Draw lines */
      xline = binmin + (i - 1.0)/(nbins - 1.0)*(binmax - binwidth - binmin) 
              + binwidth/2.0;
      if ( xmin0 < xline && xline < xmax0 )
         {
         points->coords[0] = dev_x1_box0 + (xline - xmin0)*xscale0;
         points->coords[1] = y1;
         points->coords[2] = points->coords[0];
         points->coords[3] = y2;
         jlp_gseg1->GSEG_DrawLine(points, fill_color_rgba0, style_size0);
         }
      }
   }

jlp_canvas_points_free(points);
delete[] yhist;

return;
}
