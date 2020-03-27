/*************************************************************************
* jlp_gseg_plot_data.cpp
*
* JLP
* Version 16/08/2017
**************************************************************************/
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "jlp_gseg_plot_data1.h"

/*************************************************************************
* Initialize GSEG_PLOT_DATA structure
**************************************************************************/
int jlp_gseg_init_plot_data(int gdev_graphic_type0, GSEG_PLOT_DATA *gspdata0)
{

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
/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*/
  switch(gdev_graphic_type0) {
// 1 = jlp_splot_curves
// 2 = jlp_splot_images
// 3 = wx_scrolled/jlp_splot_images
// 4 = gsegraf_2d_curves
    default:
    case 1:
    case 2:
    case 3:
    case 4:
// gseg_plot_type= 1 ("points")
      gspdata0->gseg_plot_type = 1;
// gseg_plot_type0="points"
// should be associated with styleflag = 2 or 4
      gspdata0->style_flag = 2;
      break;
// 5 = gsegraf_2d_images
    case 5:
// gseg_plot_type= 4 ("color")
      gspdata0->gseg_plot_type = 4;
// gseg_plot_type0="color"
// should be associated with styleflag = 2 ??
      gspdata0->style_flag = 2;
      break;
// 6 = gsegraf_3d_curves
    case 6:
// gseg_plot_type= 1 ("points")
      gspdata0->gseg_plot_type = 1;
// gseg_plot_type0="points"
// should be associated with styleflag = 2 or 4
      gspdata0->style_flag = 2;
      break;
// 7 = gsegraf_3d_images
    case 7:
// gseg_plot_type= 4 ("color")
      gspdata0->gseg_plot_type = 4;
// gseg_plot_type0="color"
// should be associated with styleflag = 2 ??
      gspdata0->style_flag = 2;
      break;
// 8 = gsegraf_polar_curve
    case 8:
// gseg_plot_type= 1 ("points")
      gspdata0->gseg_plot_type = 1;
// gseg_plot_type0="points"
// should be associated with styleflag = 2 or 4
      gspdata0->style_flag = 2;
      break;
  }

 strcpy(gspdata0->filename, "");
 strcpy(gspdata0->prnt_formats, "%lf %lf");
 strcpy(gspdata0->prnt_mod_formats, "");

// points- and histogram- data:
 gspdata0->npts = 0;
 gspdata0->xdata = NULL;
 gspdata0->ydata = NULL;
 gspdata0->zdata = NULL;
 gspdata0->nlinebreaks = 0;
 gspdata0->linebreak = NULL;

// 2d or 3d contours:
 gspdata0->ncontours = 0;
 gspdata0->nxcontour = 0;
 gspdata0->nycontour = 0;
 gspdata0->xcontour = NULL;
 gspdata0->ycontour = NULL;
 gspdata0->zcontour = NULL;

// 2d or 3d color plots:
 gspdata0->nxcolor = 0;
 gspdata0->nycolor = 0;
 gspdata0->xcolor = NULL;
 gspdata0->ycolor = NULL;
 gspdata0->zcolor = NULL;

// 3d mesh plots:
 gspdata0->nxmesh = 0;
 gspdata0->nymesh = 0;
 gspdata0->xmesh = NULL;
 gspdata0->ymesh = NULL;
 gspdata0->zmesh = NULL;

// Histogram:
 strcpy(gspdata0->bin_value, "percent");
 strcpy(gspdata0->bin_ref, "mean");
 gspdata0->bin_width = -1.0;

/* Set default 3d-mesh colors and 3d-contour colors */
 gspdata0->mesh_color = 0x000000FF;   // opaque black
 gspdata0->contour3d_color = 0x000000FF;   // opaque black

/* Set default style parameters */
 gspdata0->style_color1 = 0x000000FF;   // opaque black
 gspdata0->style_color2 =  0x000000FF;   // opaque black
 gspdata0->alpha_color = 0xFF;
 gspdata0->zblack = 0.;
 gspdata0->zwhite = DBLE_MAX_VALUE;
// Character from symbol_string "ld.cCtTsSiIpPhH+xra"
 gspdata0->style_char1 = 'l';
// Character from color_string "kaswrylqbfmogtnpx"
 gspdata0->style_char2 = 'k';
// Pen width:
 gspdata0->style_size = 1;
// Set default number of interpolation intervals (3d color plots)
 gspdata0->ninterp = 20;
return(0);
}
/*************************************************************************
* Free memory of GSEG_PLOT_DATA structure
**************************************************************************/
int jlp_gseg_free_plot_data(GSEG_PLOT_DATA *gspdata0)
{
int dummy_graphic_dev = 4; // 4 = gsegraf_2d_curves

// points- and histogram- data:
 if(gspdata0->xdata != NULL) delete[] gspdata0->xdata;
 if(gspdata0->ydata != NULL) delete[] gspdata0->ydata;
 if(gspdata0->zdata != NULL) delete[] gspdata0->zdata;
 if(gspdata0->linebreak != NULL) delete[] gspdata0->linebreak;

// 2d or 3d contours:
 if(gspdata0->xcontour != NULL) delete[] gspdata0->xcontour;
 if(gspdata0->ycontour != NULL) delete[] gspdata0->ycontour;
 if(gspdata0->zcontour != NULL) delete[] gspdata0->zcontour;

// 2d or 3d color plots:
 if(gspdata0->xcolor != NULL) delete[] gspdata0->xcolor;
 if(gspdata0->ycolor != NULL) delete[] gspdata0->ycolor;
 if(gspdata0->zcolor != NULL) delete[] gspdata0->zcolor;

// 3d mesh plots:
 if(gspdata0->xmesh != NULL) delete[] gspdata0->xmesh;
 if(gspdata0->ymesh != NULL) delete[] gspdata0->ymesh;
 if(gspdata0->zmesh != NULL) delete[] gspdata0->zmesh;

// Set pointers to NULL:
 jlp_gseg_init_plot_data(dummy_graphic_dev, gspdata0);
return(0);
}
/*************************************************************************
* Copy GSEG_PLOT_DATA structures
*
* INPUT:
*  gspdata1
*
* OUTPUT:
*  gspdata0
**************************************************************************/
int jlp_gseg_copy_plot_data(GSEG_PLOT_DATA *gspdata0, GSEG_PLOT_DATA gspdata1)
{
int i, nn;

// Free memory of output structure:
 jlp_gseg_free_plot_data(gspdata0);

 gspdata0->gseg_plot_type = gspdata1.gseg_plot_type;
 strcpy(gspdata0->filename, gspdata1.filename);
 strcpy(gspdata0->prnt_formats, gspdata1.prnt_formats);
 strcpy(gspdata0->prnt_mod_formats, gspdata1.prnt_mod_formats);

// points- and histogram- data:
 gspdata0->npts = gspdata1.npts;
 if(gspdata0->npts > 0) {
   gspdata0->xdata = new double[gspdata0->npts];
   gspdata0->ydata = new double[gspdata0->npts];
   gspdata0->zdata = new double[gspdata0->npts];
   for(i = 0; i < gspdata0->npts; i++) {
     gspdata0->xdata[i] = gspdata1.xdata[i];
     gspdata0->ydata[i] = gspdata1.ydata[i];
     gspdata0->zdata[i] = gspdata1.zdata[i];
     }
 }
 gspdata0->nlinebreaks = gspdata1.nlinebreaks;
 if(gspdata0->nlinebreaks > 0) {
   gspdata0->linebreak = new int[gspdata0->nlinebreaks];
   for(i = 0; i < gspdata0->nlinebreaks; i++) {
     gspdata0->linebreak[i] = gspdata1.linebreak[i];
     }
  }

// 2d or 3d contours:
 gspdata0->ncontours = gspdata1.ncontours;
// size of xcontour is nxcontour
 gspdata0->nxcontour = gspdata1.nxcontour;
// size of ycontour is nycontour
 gspdata0->nycontour = gspdata1.nycontour;
// size of zcontour is nxcontour * nycontour
 if(gspdata0->nxcontour > 0) {
   gspdata0->xcontour = new double[gspdata0->nxcontour];
   for(i = 0; i < gspdata0->nxcontour; i++) {
     gspdata0->xcontour[i] = gspdata1.xcontour[i];
     }
   }
 if(gspdata0->nycontour > 0) {
   gspdata0->ycontour = new double[gspdata0->nycontour];
   for(i = 0; i < gspdata0->nycontour; i++) {
     gspdata0->ycontour[i] = gspdata1.ycontour[i];
     }
   }
 nn = gspdata0->nxcontour * gspdata0->nycontour;
 if(nn > 0) {
   gspdata0->zcontour = new double[nn];
   for(i = 0; i < nn; i++) {
     gspdata0->zcontour[i] = gspdata1.zcontour[i];
     }
   }

// 2d or 3d color plots:
 gspdata0->nxcolor = gspdata1.nxcolor;
 gspdata0->nycolor = gspdata1.nycolor;
 if(gspdata0->nxcolor > 0) {
   gspdata0->xcolor = new double[gspdata0->nxcolor];
   for(i = 0; i < gspdata0->nxcolor; i++) {
     gspdata0->xcolor[i] = gspdata1.xcolor[i];
     }
   }
 if(gspdata0->nycolor > 0) {
   gspdata0->ycolor = new double[gspdata0->nycolor];
   for(i = 0; i < gspdata0->nycolor; i++) {
     gspdata0->ycolor[i] = gspdata1.ycolor[i];
     }
   }
// size of zcolor is nxcolor * nycolor
 nn = gspdata0->nxcolor * gspdata0->nycolor;
 if(nn > 0) {
   gspdata0->zcolor = new double[nn];
   for(i = 0; i < nn; i++) {
     gspdata0->zcolor[i] = gspdata1.zcolor[i];
     }
   }

// 3d mesh plots:
 gspdata0->nxmesh = gspdata1.nxmesh;
 gspdata0->nymesh = gspdata1.nymesh;
// size of zmesh is nxmesh * nymesh
 if(gspdata0->nxmesh > 0) {
   gspdata0->xmesh = new double[gspdata0->nxmesh];
   for(i = 0; i < gspdata0->nxmesh; i++) {
     gspdata0->xmesh[i] = gspdata1.xmesh[i];
     }
   }
 if(gspdata0->nymesh > 0) {
   gspdata0->ymesh = new double[gspdata0->nymesh];
   for(i = 0; i < gspdata0->nymesh; i++) {
     gspdata0->ymesh[i] = gspdata1.ymesh[i];
     }
   }
// size of zmesh is nxmesh * nymesh
 nn = gspdata0->nxmesh * gspdata0->nymesh;
 if(nn > 0) {
   gspdata0->zmesh = new double[nn];
   for(i = 0; i < nn; i++) {
     gspdata0->zmesh[i] = gspdata1.zmesh[i];
     }
   }

// Histogram:
 strcpy(gspdata0->bin_value, gspdata1.bin_value);
 strcpy(gspdata0->bin_ref, gspdata1.bin_ref);
 gspdata0->bin_width = gspdata1.bin_width;

/* Set 3d-mesh and 3d contour colors */
 gspdata0->mesh_color = gspdata1.mesh_color;
 gspdata0->style_color1 = gspdata1.mesh_color;
// gseg_plot_type: 5="mesh"
 if(gspdata0->gseg_plot_type == 5)
   gspdata0->contour3d_color = gspdata1.mesh_color;

/* Set style parameters */
 gspdata0->style_color1 = gspdata1.style_color1;
 gspdata0->style_color2 = gspdata1.style_color2;
 gspdata0->alpha_color = gspdata1.alpha_color;
 gspdata0->outline_color_rgba = gspdata1.outline_color_rgba;
 gspdata0->fill_color_rgba = gspdata1.fill_color_rgba;
 gspdata0->zblack = gspdata1.zblack;
 gspdata0->zwhite = gspdata1.zwhite;
// Character from symbol_string "ld.cCtTsSiIpPhH+xra"
 gspdata0->style_char1 = gspdata1.style_char1;
// Character from color_string "kaswrylqbfmogtnpx"
 gspdata0->style_char2 = gspdata1.style_char2;
 gspdata0->style_flag = gspdata1.style_flag;
 gspdata0->style_size = gspdata1.style_size;
// Number of interpolation intervals:
 gspdata0->ninterp = gspdata1.ninterp;

return(0);
}
/***************************************************************
 Routine to compute a good scale to display the image
 (Called by Xdisp1 and plot_3d)

 Two modes:
  if input lower_itt=upper_itt=0: min/max scale
  otherwise: high contrast scale
****************************************************************/
int auto_scale_for_image(double *image1, int nx1, int ny1, int idim,
                         const int high_contrast, double *lower_itt,
                         double *upper_itt)
{
int status;
status =  auto_scale_for_box(image1, nx1, ny1, idim, 0, nx1, 0, ny1,
                             high_contrast, lower_itt, upper_itt);
return(status);
}
/***************************************************************
 Routine to compute a good scale to display the image
 (Called by Xdisp1 and plot_3d)

 Two modes:
  if input lower_itt=upper_itt=0: min/max scale
  otherwise: high contrast scale
****************************************************************/
int auto_scale_for_box(double *image1, int nx1, int ny1, int idim,
                       const int ix_min, const int ix_max,
                       const int iy_min, const int iy_max,
                       const int high_contrast,
                       double *lower_itt, double *upper_itt)
{
double sum, sumsq, mean, sigma;
double w1, mini, maxi, mini1, maxi1;
int npts, min_max_scale, jj;
register int i, j;

#ifdef DEBUG
 printf("auto_scale_double/ nx1=%d ny1=%d idim=%d\n", nx1, ny1, idim);
 printf("auto_scale_double/ x:%d,%d y=%d,%d \n", ix_min, ix_max, iy_min, iy_max);
#endif

/* Check if output should be min max or high contrasted scale: */
min_max_scale = (high_contrast == 0) ? 1 : 0;

/* Compute min and max: */
 sum = 0.; sumsq = 0.; npts = 0;
 mini = image1[ix_min + iy_min * nx1];
 maxi = mini;
 for(j = iy_min; j < iy_max; j++) {
   jj = j * idim;
   for(i = ix_min; i < ix_max; i++)
    {
    w1 = image1[i + jj];
    npts++;
    sum += w1;
    sumsq += w1*w1;
    if(w1 < mini) mini = w1;
    if(w1 > maxi) maxi = w1;
    }
  }

/* General case: */
*lower_itt = mini;
*upper_itt = maxi;

/* Check if all values are equal: */
if(mini == maxi)
   {
   fprintf(stderr, "auto_scale_double: all pixels have same value (= %.6G) !\n", mini);
   fprintf(stderr, "auto_scale_double/ Window: x:%d,%d y=%d,%d (npts=%d)\n",
           ix_min, ix_max, iy_min, iy_max, npts);
   *lower_itt = mini;
   *upper_itt = mini + 1;
   return(0);
   }

/* Otherwise compute mean and sigma: */
#ifdef DEBUG
   printf("npts = %.6G\n",(double)npts);
#endif
 if(npts == 0) npts = 1;
 mean = sum / (double)npts;
 sumsq /= (double)npts;
 sumsq = sumsq - mean * mean;
 sigma = sqrt((double)sumsq);
#ifdef DEBUG
 printf("auto_scale_double: min=%.6G, max=%.6G, mean_1=%.6G, sigma_1=%.6G\n",
         mini, maxi, mean, sigma);
#endif

/* Return min and max scale if this option has been selected: */
if(min_max_scale) return(0);

/* Do a second loop with 3 sigma rejection: */
 mini1 = mean - 3. * sigma;
 maxi1 = mean + 3. * sigma;
 sum = 0.; sumsq = 0.; npts = 0;
 for(j = iy_min; j < iy_max; j++) {
   jj = j * idim;
   for(i = ix_min; i < ix_max; i++)
    {
    w1 = image1[i + jj];
    if(w1 > mini1 && w1 < maxi1)
      {
      npts++;
      sum += w1;
      sumsq += w1*w1;
      }
    }
  }

/* Compute new estimation of mean and sigma: */
if(npts > 0)
 {
 mean = sum / (double)npts;
 sumsq /= (double)npts;
 sumsq = sumsq - mean * mean;
 sigma = sqrt((double)sumsq);
 }

/* Assume that mean and sigma are meaningful: */
if((mean - mini) > 3. * sigma) *lower_itt = mean - 3. * sigma;
if((maxi - mean) > 3. * sigma) *upper_itt = mean + 3. * sigma;

#ifdef DEBUG
 printf("3-sig rejec => min=%.6G, max=%.6G, mean=%.6G, sigma=%.6G\n",
         *lower_itt,*upper_itt,mean,sigma);
#endif

 return(0);
}
