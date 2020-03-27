/**************************************************************************
* "jlp_gdev_gsegraf.cpp"
*
* Definition of the members of the JLP_GDev class
* linked to gsegraf routines
*
* JLP
* Version 07/03/2019
**************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>   // isdigit()
#include <math.h>

#include "jlp_gdev.h"
#include "jlp_macros.h"    // MINI, MAXI, etc
#include "jlp_splot_idv.h"   // JLP_GET_PLOT_DATA, etc 
#include "jlp_gseg_plot_data1.h"    // jlp_gseg_init_plot_data()

/* Contained here
*/
static int convert_xydata_for_plot(GSEG_PLOT_DATA *gseg_pltdata0,
                                   double *xplot0, double *yplot0,
                                   int npts0, char *axis_type0);

/*************************************************************************
* Update Jgc0 and Mgc0 structures from Gsegraf axis data settings
*
* INPUT:
*  gseg_axdata0: GSEG_AXIS_DATA structure
*************************************************************************/
int JLP_GDev::GDev_UpdateJgcFromGsegAxisData(GSEG_AXIS_DATA gseg_axdata0)
{
int i;

   strcpy(Jgc0.box_xlabel, gseg_axdata0.xlabel);
   strcpy(Jgc0.box_ylabel, gseg_axdata0.ylabel);
   strcpy(Jgc0.box_zlabel, gseg_axdata0.zlabel);
   strcpy(Jgc0.box_title, gseg_axdata0.title);
   for(i = 0; i < 6; i++) Jgc0.axis_limits[i] = gseg_axdata0.axis_limits[i]; 

// Correspondance from Jgc0 to Mgc0 parameters
FromJgc0ToMgc0();

return(0);
}
/*************************************************************************
* Update Jgc0 and Mgc0 structures from Gsegraf settings
*
* INPUT:
*   dev_x0, dev_y0 : location of the left top corner
*   dev_width0, dev_height0 : width and height of the device box
*************************************************************************/
int JLP_GDev::GDev_UpdateJgcFromGsegSettings(JLP_Gsegraf *jlp_gsegraf0,
                                        int dev_x0, int dev_y0, 
                                        int dev_width0, int dev_height0)
{
double xmin_0, xmax_0, ymin_0, ymax_0, zmin_0, zmax_0;
double dev_x1_box0, dev_x2_box0, dev_y1_box0, dev_y2_box0;
double xscale_0, yscale_0;
char axis_type0[64];

// This object should have been created by open_device
 if(jlp_gsegraf0 == NULL) {
   fprintf(stderr, "InitializeGdevPlotWithParamFile/Fatal error, jlp_gsegraf0 is NULL\n");
   fprintf(stderr, "(should have been created by calling routine !)\n");
   return(-1);
  }

// Jgc0.dev_width, Jgc0.dev_height: window size in device coordinates
// (for unscrolled plots)
Jgc0.dev_width = dev_width0;
Jgc0.dev_height = dev_height0;

// Get box limits:
jlp_gsegraf0->GSEG_GetBoxSettingsForLinear(&dev_x1_box0, &dev_x2_box0,
                                           &dev_y1_box0, &dev_y2_box0,
                                           &xmin_0, &xmax_0, &ymin_0, &ymax_0,
                                           &zmin_0, &zmax_0,
                                           &xscale_0, &yscale_0, axis_type0);

// Axis limits in user coordinates
SetBoxLimits(xmin_0, xmax_0, ymin_0, ymax_0, zmin_0, zmax_0);
Jgc0.axis_limits[0] = xmin_0;
Jgc0.axis_limits[1] = xmax_0;
Jgc0.axis_limits[2] = ymin_0;
Jgc0.axis_limits[3] = ymax_0;
Jgc0.axis_limits[4] = zmin_0;
Jgc0.axis_limits[5] = zmax_0;

// Location and size of the box axis in MGO coordinates:
Jgc0.offx = (int)((dev_x1_box0 + dev_x0)
                     * (double)SCREEN_SIZE / (double)dev_width0);
Jgc0.axlen = (int)((dev_x2_box0 - dev_x1_box0)
              * (double)SCREEN_SIZE / (double)dev_width0);
// Warning origin is on the top left corner for Gseg !
Jgc0.offy = (int)((dev_y0 + dev_height0 - dev_y2_box0)
                     * (double)SCREEN_SIZE / (double)dev_height0);
Jgc0.aylen = (int)((dev_y2_box0 - dev_y1_box0)
              * (double)SCREEN_SIZE / (double)dev_height0);

/* DEBUG:
*/
printf("GDev_UpdateJgcFromGsegSettings/ axislimits: %f %f %f %f %f %f\n",
       Jgc0.axis_limits[0], Jgc0.axis_limits[1], Jgc0.axis_limits[2],
       Jgc0.axis_limits[3], Jgc0.axis_limits[4], Jgc0.axis_limits[5]);
printf("GDev_UpdateJgcFromGsegSettings/ dev_x0=%d dev_x1_box0/x2 = %f %f dev_y0=%d dev_y1_box0/y2 = %f %f\n",
       dev_x0, dev_x1_box0, dev_x2_box0, dev_y0, dev_y1_box0, dev_y2_box0);
printf("GDev_UpdateJgcFromGsegSettings/ offx/axlen = %d %d offy/aylen = %d %d\n",
        Jgc0.offx, Jgc0.axlen, Jgc0.offy, Jgc0.aylen);

// Correspondance from Jgc0 to Mgc0 parameters
FromJgc0ToMgc0();

return(0);
}

/*************************************************************************
* Load curve to gseg_pltdata0 object
* using private arrays: npts_1, xplot_1, yplot_1, zplot_1, nmax_1, etc,
* NB: for gsegraph, iplot index starts at iplot=1 !
* 
* INPUT:
*  iplot: gsegraph index, or (index-1) for private arrays xplot_1, yplot_1... 
*  gdev_graphic_type: gdev_graphic_type (1=jlp_splot_curves, ...)
*  gseg_plot_type0: gseg plot type (1="points", 2="histogram", etc) 
*  axis_type0: axis type ("linear", "loglog", etc) 
*  pen_width: width of pen (default is 1)
*
* OUTPUT:
*  gseg_pltdata0 : GSEG_PLOT_DATA structure
*
*************************************************************************/
int JLP_GDev::LoadXYPlot1ToGsegPlot(GSEG_PLOT_DATA *gseg_pltdata0,
                                    const int iplot, int gdev_graphic_type0,
                                    int gseg_plot_type0,
                                    char *axis_type0, const int pen_width0)
{
int i, nn, isize;
char pcolor0[32], nchar0[4], buffer[128];

// Load selected options:
/* axis_type:
* "linear"   (points, histogram, contour, and color plot types)
* "semilogx" (points plot type only)
* "semilogy"  (points plot type only)
* "loglog"  (points plot type only)
* "polar" (points plot type only)
* "3d" (points, contour, color, and mesh plot types)
*/

/* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
*/
// Initialize gseg_pltdata0 according to gdev_graphic_type0:
  jlp_gseg_init_plot_data(gdev_graphic_type0, gseg_pltdata0);

/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*/
  gseg_pltdata0->gseg_plot_type = gseg_plot_type0;
// NB: gseg_plot_type0="points" should be associated with styleflag = 2 or 4
  gseg_pltdata0->style_flag = 2;

// Pen width (default is 1):
  gseg_pltdata0->style_size = pen_width0;

   nn = npts_1[iplot-1];

// Character from symbol_string "ld.cCtTsSiIpPhH+xra"
/*
         l         solid line
         d         dashed line
         .         dotted line
         c         circle, unfilled
         C         circle, filled
         t         triangle, unfilled
         T         triangle, filled
         s         square, unfilled
         S         square, filled
         i         diamond, unfilled
         I         diamond, filled
         p         pentagon, unfilled
         P         pentagon, filled
         h         hexagon, unfilled
         H         hexagon, filled
         +         plus sign
         x         multiplication sign
         r         star, 5 points
         a         asterisk, 6 points
*/
// L0, L1, ...
/* 
*   isize between 1 and 9  if called by NEWPLOT2 (multiplied internally by 20)
*   isymb: type of symbol:
*     0: histogram
*     1: a tiny point
*     2: white triangle (base down)
*     3: black triangle (base down)
*     4: cross +
*     5: cross X
*     6: open square
*     7: filled square
*     8: open circle with a diameter (20 * size)
*     9: filled circle with a diameter (20 * isize)
*     12: white triangle (base up)
*     13: black triangle (base up)
*/
// NB: kcurve starts at one for gsegraf, and at 0 for splot_gdev
    strncpy(nchar0, &nchar_1[4 * (iplot-1)], 4);
    gseg_pltdata0->style_char1 = 'l';
    if((nchar0[0] == 'L') || (nchar0[0] == 'l')) {
       if(nchar0[1] == '1') gseg_pltdata0->style_char1 = 'd';
       else if(nchar0[1] == '2') gseg_pltdata0->style_char1 = '.';
       else gseg_pltdata0->style_char1 = 'l';
       } else {
        if(nchar0[0] == '2') 
          gseg_pltdata0->style_char1 = 't';
        else if(nchar0[0] == '3') 
          gseg_pltdata0->style_char1 = 'T';
        else if(nchar0[0] == '4') 
          gseg_pltdata0->style_char1 = '+';
        else if(nchar0[0] == '5') 
          gseg_pltdata0->style_char1 = 'x';
        else if(nchar0[0] == '6') 
          gseg_pltdata0->style_char1 = 's';
        else if(nchar0[0] == '7') 
          gseg_pltdata0->style_char1 = 'S';
        else if(nchar0[0] == '8') 
          gseg_pltdata0->style_char1 = 'c';
        else if(nchar0[0] == '9') 
          gseg_pltdata0->style_char1 = 'C';
        else 
          gseg_pltdata0->style_char1 = 'a';

// Symbol size (default is 1, but it is too small
       gseg_pltdata0->style_size = 4;
       if(isdigit(nchar0[1]) == true) {
// Conversion to int:
        buffer[0] = nchar0[1]; 
        buffer[1] = '\0'; 
        isize = atoi(buffer); 
        if(isize >= 1 && isize <= 9) gseg_pltdata0->style_size *= isize;
        }
       }

// Character from color_string "kaswrylqbfmogtnpx"
       gseg_pltdata0->style_char2 = 'k';
/*
         k               black                      000000
         a               gray    (gray50)           808080
         s               silver  (gray75)           C0C0C0
         w               white                      FFFFFF
         r               red                        FF0000
         y               yellow                     FFFF00
         l               lime    (green)            00FF00
         q               aqua    (cyan)             00FFFF
         b               blue                       0000FF
         f               fuchsia (magenta)          FF00FF
         m               maroon  (dark red)         800000
         o               olive   (dark yellow)      808000
         g               green   (dark green)       008000
         t               teal    (dark cyan)        008080
         n               navy    (dark blue)        000080
         p               purple  (dark magenta)     800080
*/
    gseg_pltdata0->style_char2 = 'k';
// NB: alpha = FF for opaque, 00 for transparent:
    gseg_pltdata0->outline_color_rgba = 0x000000FF;
// NB: kcurve starts at one for gsegraf, and at 0 for splot_gdev
    strncpy(pcolor0, &pcolor_1[32 * (iplot - 1)], 32);
    if((strcmp(pcolor0, "Black") == 0)
        || (strcmp(pcolor0, "black") == 0)
        || (strcmp(pcolor0, "RGB_0_0_0") == 0)) {
       gseg_pltdata0->style_char2 = 'k';
       gseg_pltdata0->outline_color_rgba = 0x000000FF;
       }
// Gray (silver or gray75)
    else if((strcmp(pcolor0, "Gray") == 0)
        || (strcmp(pcolor0, "gray") == 0)
        || (strcmp(pcolor0, "Grey") == 0)
        || (strcmp(pcolor0, "grey") == 0)
        || (strcmp(pcolor0, "RGB_200_200_200") == 0)) {
       gseg_pltdata0->style_char2 = 's';
       gseg_pltdata0->outline_color_rgba = 0xC0C0C0FF;
       }
    else if((strcmp(pcolor0, "White") == 0)
        || (strcmp(pcolor0, "white") == 0)
        || (strcmp(pcolor0, "RGB_255_255_255") == 0)) {
       gseg_pltdata0->style_char2 = 'w';
       gseg_pltdata0->outline_color_rgba = 0xFFFFFFFF;
       }
    else if((strcmp(pcolor0, "Red") == 0)
        || (strcmp(pcolor0, "red") == 0)
        || (strcmp(pcolor0, "RGB_255_0_0") == 0)) {
       gseg_pltdata0->style_char2 = 'r';
       gseg_pltdata0->outline_color_rgba = 0xFF0000FF;
       }
// Green/Dark green
    else if((strcmp(pcolor0, "Green") == 0)
        || (strcmp(pcolor0, "green") == 0)
        || (strcmp(pcolor0, "RGB_0_255_0") == 0)) {
       gseg_pltdata0->style_char2 = 'g';
// 0x008000FF is much too dark !
       gseg_pltdata0->outline_color_rgba = 0x00D000FF;
       }
    else if((strcmp(pcolor0, "Blue") == 0)
        || (strcmp(pcolor0, "blue") == 0)
        || (strcmp(pcolor0, "RGB_0_0_255") == 0)) {
       gseg_pltdata0->style_char2 = 'b';
       gseg_pltdata0->outline_color_rgba = 0x0000FFFF;
       }
    else if((strcmp(pcolor0, "Yellow") == 0)
        || (strcmp(pcolor0, "yellow") == 0)
        || (strcmp(pcolor0, "RGB_255_255_0") == 0)) {
       gseg_pltdata0->style_char2 = 'y';
       gseg_pltdata0->outline_color_rgba = 0xFFFF00FF;
       }
    gseg_pltdata0->fill_color_rgba = gseg_pltdata0->outline_color_rgba;

#ifdef DEBUG
   printf("LoadXYPlot1ToGsegPlot nchar0=%s pcolor0=%s kcurve=%d\n",
           nchar0, pcolor0, iplot); 
   printf("LoadXYPlot1ToGsegPlot style_char1=%c style_char2=%c style_size=%d outline=%d fill=%d\n", 
          gseg_pltdata0->style_char1, gseg_pltdata0->style_char2,
          gseg_pltdata0->style_size, gseg_pltdata0->outline_color_rgba, 
          gseg_pltdata0->fill_color_rgba);
#endif

    gseg_pltdata0->npts = nn;  
    gseg_pltdata0->xdata = new double[nn];
    gseg_pltdata0->ydata = new double[nn];
    gseg_pltdata0->zdata = new double[nn];
    for(i = 0; i < nn; i++) gseg_pltdata0->zdata[i] = 0.;

// Log conversion if needed:
// (using private parameters xplot_1, yplot_1, nmaxis_1, etc)
// NB: kcurve starts at one for gsegraf, and at 0 for splot_gdev
   convert_xydata_for_plot(gseg_pltdata0, &(xplot_1[(iplot -1) * nmaxi_1]),
                           &(yplot_1[(iplot -1) * nmaxi_1]), nn, axis_type0);

return(0);
}
/**************************************************************************
*
**************************************************************************/
static int convert_xydata_for_plot(GSEG_PLOT_DATA *gseg_pltdata0,
                                   double *xplot0, double *yplot0,
                                   int npts0, char *axis_type0)
{
int i;

/* Modify data for logarithmic axes */
    if ( strcmp(axis_type0, "semilogx") == 0 ||
         strcmp(axis_type0, "loglog")   == 0 ) {
        for(i = 0; i < npts0; i++) {
           if(xplot0[i] > 0.0)
             gseg_pltdata0->xdata[i] = log10(xplot0[i]);
           else
             gseg_pltdata0->xdata[i] = log10(DBLE_MIN_VALUE);
         }
     } else {
      for(i = 0; i < npts0; i++) {
        gseg_pltdata0->xdata[i] =  xplot0[i];
        }
     }

/* Modify data for logarithmic axes */
    if ( strcmp(axis_type0, "semilogy") == 0 ||
         strcmp(axis_type0, "loglog")   == 0 ) {
        for(i = 0; i < npts0; i++) {
           if(yplot0[i] > 0.0)
             gseg_pltdata0->ydata[i] = log10(yplot0[i]);
           else
             gseg_pltdata0->ydata[i] = log10(DBLE_MIN_VALUE);
         }
     } else {
      for(i = 0; i < npts0; i++) {
        gseg_pltdata0->ydata[i] =  yplot0[i];
        }
     }

return(0);
}
