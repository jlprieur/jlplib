/*******************************************************************************
* jlp_GsegAxes_GetPlotSettings.cpp
* JLP_GsegAxes class
*
* JLP
* Version 01/04/2017
*******************************************************************************/
#include <stdlib.h>          // exit()
#include <math.h>
#include "jlp_gseg_axes.h"

/*****************************************************************************
*
*****************************************************************************/
void JLP_GsegAxes::GetAxisTypeFlags(int *flag_2d_0, int *flag_3d_0,
                                    int *flag_2d_rect_0, int *flag_polar_0,
                                    int *flag_linear_0, int *flag_logx_0,
                                    int *flag_logy_0, int *flag_loglog_0)
{

*flag_2d_0 = flag_2d_1;
*flag_3d_0 = flag_3d_1;
*flag_2d_rect_0 = flag_2d_rect_1;
*flag_polar_0 = flag_polar_1;
*flag_linear_0 = flag_linear_1;
*flag_logx_0 = flag_logx_1;
*flag_logy_0 = flag_logy_1;
*flag_loglog_0 = flag_loglog_1;

return;
}
/*************************************************************************
* Get plot box minimum and maximum values for linear scale of a 2d plot 
*
* OUTPUT:
* dev_x1_box0, dev_x2_box0, dev_y1_box0, dev_y2_box0 : device coord. of the
*                                                      four corners of the box
* xmin_0, xmax_0, ymin_0, ymax_0 : user coords of the axes limits as indicated 
*                                  by the tick labels
* xscale, yscale : (device coords range) / (user coords range) ratio
*************************************************************************/
int JLP_GsegAxes::GetBoxSettingsForLinear(double *dev_x1_box0, 
                                          double *dev_x2_box0, 
                                          double *dev_y1_box0, 
                                          double *dev_y2_box0,
                                          double *xmin_0, double *xmax_0, 
                                          double *ymin_0, double *ymax_0,
                                          double *zmin_0, double *zmax_0,
                                          double *xscale_0, double *yscale_0)
{

// Get device coords of the rectangular box:
 *dev_x1_box0 = p_plot_box_data->xmin;
 *dev_x2_box0 = p_plot_box_data->xmax;
 *dev_y1_box0 = p_plot_box_data->ymin;
 *dev_y2_box0 = p_plot_box_data->ymax;

 GetAxisLimitsFromTickLabelsForLinear(xmin_0, xmax_0, ymin_0, ymax_0,
                                   zmin_0, zmax_0);

// Compute the axis scale : (device coords range) / (user coords range) ratio
 *xscale_0 = ((*dev_x2_box0) - (*dev_x1_box0)) / (*xmax_0 - *xmin_0);
 *yscale_0 = ((*dev_y2_box0) - (*dev_y1_box0)) / (*ymax_0 - *ymin_0);

return(0);
}
/*************************************************************************
* Get plot box minimum and maximum values for linear scale
*
* OUTPUT:
* xmin_0, xmax_0, ymin_0, ymax_0 : user coords of the axes limits as indicated 
*                                  by the tick labels
*************************************************************************/
int JLP_GsegAxes::GetAxisLimitsFromTickLabelsForLinear(double *xmin_0, 
                                          double *xmax_0,
                                          double *ymin_0, double *ymax_0,
                                          double *zmin_0, double *zmax_0)
{
int nxvalues, nyvalues, nzvalues;

/* Get axis minimum and maximum values */
 nxvalues = p_ticklabels->nxvalues;
 nyvalues = p_ticklabels->nyvalues;
 nzvalues = p_ticklabels->nzvalues;
 *xmin_0 = p_ticklabels->xvalues[0];
 *xmax_0 = p_ticklabels->xvalues[nxvalues-1];
 *ymin_0 = p_ticklabels->yvalues[0];
 *ymax_0 = p_ticklabels->yvalues[nyvalues-1];
 *zmin_0 = p_ticklabels->zvalues[0];
 *zmax_0 = p_ticklabels->zvalues[nzvalues-1];

// Take the offset values of the tick labels into account:
 *xmin_0 = *xmin_0 - p_ticklabels->xoffset1;
 *xmax_0 = *xmax_0 + p_ticklabels->xoffset2;
 *ymin_0 = *ymin_0 - p_ticklabels->yoffset1;
 *ymax_0 = *ymax_0 + p_ticklabels->yoffset2;
 *zmin_0 = *zmin_0 - p_ticklabels->zoffset1;
 *zmax_0 = *zmax_0 + p_ticklabels->zoffset2;

return(0);
}
/*************************************************************************
* Get plot box minimum and maximum values for polar plot 
*
* OUTPUT:
* xorigin_0, yorigin_0, radius_0 : device coord. of the center and radius
*                                  of the circular box
* rmin_0, rmax_0 : user coords of the inner/outer radius limits as indicated 
*                  by the tick labels
* rscale_0 : (device coords range) / (user coords range) ratio
*************************************************************************/
int JLP_GsegAxes::GetBoxSettingsForPolar(double *xorigin_0, double *yorigin_0,
                                         double *radius_0, double *rmin_0, 
                                         double *rmax_0, double *rscale_0)
{
int x0, y0, window_width0, window_height0, nyvalues;

// Get size of window (from Gnome interface for instance)
 jlp_gseg1->GSEG_GetWindowLimits(&x0, &window_width0, &y0, &window_height0);

// Get device coords of the circular box:
  *xorigin_0 = 0.375 * window_width0;
  *yorigin_0 = 0.500 * window_height0;
  if(window_width0 >= window_height0 )
    *radius_0 = 0.375 * window_height0;
  else
    *radius_0 = 0.375 * window_width0;

/* Get minimum and maximum radius values */
  nyvalues = p_ticklabels->nyvalues;
  *rmin_0 = p_ticklabels->yvalues[0];
  *rmax_0 = p_ticklabels->yvalues[nyvalues-1];

// Take the offset values of the tick labels into account:
  *rmin_0 = *rmin_0 - p_ticklabels->yoffset1;
  *rmax_0 = *rmax_0 + p_ticklabels->yoffset2;

// Compute the axis scale : (device coords range) / (user coords range) ratio
  *rscale_0 = *radius_0 / (*rmax_0 - *rmin_0);

return(0);
}
/*************************************************************************
*
*************************************************************************/
int JLP_GsegAxes::GetNzValues(int *nzvalues)
{
  *nzvalues = p_ticklabels->nzvalues;
return(0);
}
/*************************************************************************
* Get plot box minimum and maximum values for 3d plot 
*************************************************************************/
int JLP_GsegAxes::GetBoxSettingsFor3d(double origin_0[3], double Ry_0[9],
                                      double Rz_0[9], double Ryz_0[9],
                                      double *xmin_0, double *xmax_0, 
                                      double *ymin_0, double *ymax_0,
                                      double *zmin_0, double *zmax_0,
                                      double *xscale_0, double *yscale_0,
                                      double *zscale_0)
{
int i, nxvalues, nyvalues, nzvalues;
double xscalesq, yscalesq, zscalesq; 
double phi0, theta0, phi0_rad, theta0_rad, axis_length;
double *ppp;

/* Get minimum and maximum axis values */
  nxvalues = p_ticklabels->nxvalues;
  nyvalues = p_ticklabels->nyvalues;
  nzvalues = p_ticklabels->nzvalues;
  *xmin_0 = p_ticklabels->xvalues[0];
  *xmax_0 = p_ticklabels->xvalues[nxvalues-1];
  *ymin_0 = p_ticklabels->yvalues[0];
  *ymax_0 = p_ticklabels->yvalues[nyvalues-1];
  *zmin_0 = p_ticklabels->zvalues[0];
  *zmax_0 = p_ticklabels->zvalues[nzvalues-1];

// Take the offset values of the tick labels into account:
  *xmin_0 = *xmin_0 - p_ticklabels->xoffset1;
  *xmax_0 = *xmax_0 + p_ticklabels->xoffset2;
  *ymin_0 = *ymin_0 - p_ticklabels->yoffset1;
  *ymax_0 = *ymax_0 + p_ticklabels->yoffset2;
  *zmin_0 = *zmin_0 - p_ticklabels->zoffset1;
  *zmax_0 = *zmax_0 + p_ticklabels->zoffset2;

/* Get view angles (in degrees) */
  phi0 = p_plot_param_3d->phi;
  theta0 = p_plot_param_3d->theta;

/* Get origin */
  if ( phi0 >= 0.0 && phi0 < 90.0 )
     {
     origin_0[0] = p_plot_param_3d->origin[0];
     origin_0[1] = p_plot_param_3d->origin[1];
     origin_0[2] = p_plot_param_3d->origin[2];
     }

  else if ( phi0 >= 90.0 && phi0 < 180.0 )
     {
     origin_0[0] = p_plot_param_3d->origin[0] + p_plot_param_3d->axis2[0];
     origin_0[1] = p_plot_param_3d->origin[1] + p_plot_param_3d->axis2[1];
     origin_0[2] = p_plot_param_3d->origin[2] - p_plot_param_3d->axis2[2];
     }

  else if ( phi0 >= 180.0 && phi0 < 270.0 )
     {
     origin_0[0] = p_plot_param_3d->origin[0] + p_plot_param_3d->axis1[0]
                 + p_plot_param_3d->axis2[0];
     origin_0[1] = p_plot_param_3d->origin[1] + p_plot_param_3d->axis1[1]
                 + p_plot_param_3d->axis2[1];
     origin_0[2] = p_plot_param_3d->origin[2] - p_plot_param_3d->axis1[2]
                 - p_plot_param_3d->axis2[2];
     }

  else if ( phi0 >= 270.0 && phi0 < 360.0 )
     {
     origin_0[0] = p_plot_param_3d->origin[0] + p_plot_param_3d->axis1[0];
     origin_0[1] = p_plot_param_3d->origin[1] + p_plot_param_3d->axis1[1];
     origin_0[2] = p_plot_param_3d->origin[2] - p_plot_param_3d->axis1[2];
     }

/* Calculate rotation matrices */
  phi0_rad = phi0 * DEGTORAD;
  theta0_rad = theta0 * DEGTORAD;

  Ry_0[0] = cos(-theta0_rad);
  Ry_0[1] = 0.0;
  Ry_0[2] = -sin(-theta0_rad);
  Ry_0[3] = 0.0;
  Ry_0[4] = 1.0;
  Ry_0[5] = 0.0;
  Ry_0[6] = sin(-theta0_rad);
  Ry_0[7] = 0.0;
  Ry_0[8] = cos(-theta0_rad);

  Rz_0[0] = cos(phi0_rad);
  Rz_0[1] = sin(phi0_rad);
  Rz_0[2] = 0.0;
  Rz_0[3] = -sin(phi0_rad);
  Rz_0[4] = cos(phi0_rad);
  Rz_0[5] = 0.0;
  Rz_0[6] = 0.0;
  Rz_0[7] = 0.0;
  Rz_0[8] = 1.0;

  ppp = multiply_mm(Ry_0, Rz_0);
  for ( i=1; i <= 9; i++, ppp++ )
     Ryz_0[i-1] = *ppp;

/* Get axis length */
  axis_length = p_plot_param_3d->axis_length;

// Compute the axis scale factors: 
// defined as scale = (device coords range) / (user coords range) ratio
  *xscale_0 = axis_length / (*xmax_0 - *xmin_0);
  *yscale_0 = axis_length / (*ymax_0 - *ymin_0);
  *zscale_0 = axis_length / (*zmax_0 - *zmin_0);
  xscalesq = *xscale_0 * *xscale_0;
  yscalesq = *yscale_0 * *yscale_0;
  zscalesq = *zscale_0 * *zscale_0;

return(0);
}
/*************************************************************************
* Get plot box minimum and maximum values 
*
* OUTPUT:
* ncoords_0 : number of coordinates (3 for 3D, 2 otherwise)
*************************************************************************/
int JLP_GsegAxes::GetBoxSettings(double *dev_x1_box0, double *dev_x2_box0, 
                                  double *dev_y1_box0, double *dev_y2_box0,
                                  double *xmin_0, double *xmax_0, 
                                  double *ymin_0, double *ymax_0,
                                  double *zmin_0, double *zmax_0,
                                  double *rmin_0, double *rmax_0,
                                  double *xscale_0, double *yscale_0,
                                  double *zscale_0, double *rscale_0,
                                  double *xorigin_0, double *yorigin_0,
                                  double *radius_0, double origin_0[3],
                                  double Ry_0[9], double Rz_0[9], 
                                  double Ryz_0[9], int *ncoords_0)
{
int i, nxvalues, nyvalues, nzvalues, status = 0;
int x0, y0, window_width0, window_height0;
double xscalesq, yscalesq, zscalesq, quadrant;
double *ppp, phi0, theta0, phi0_rad, theta0_rad, axis_length;

/******************* Get minimum and maximum axis values *******************/

 if(strcmp(p_plot_param->axis_type, "linear") == 0 ) 
   { 
   nxvalues = p_ticklabels->nxvalues;
   nyvalues = p_ticklabels->nyvalues;
   nzvalues = p_ticklabels->nzvalues;
   *xmin_0 = p_ticklabels->xvalues[0];
   *xmax_0 = p_ticklabels->xvalues[nxvalues-1];
   *xmin_0 = *xmin_0 - p_ticklabels->xoffset1;
   *xmax_0 = *xmax_0 + p_ticklabels->xoffset2;
   *ymin_0 = p_ticklabels->yvalues[0];
   *ymax_0 = p_ticklabels->yvalues[nyvalues-1];
   *ymin_0 = *ymin_0 - p_ticklabels->yoffset1;
   *ymax_0 = *ymax_0 + p_ticklabels->yoffset2;
   *zmin_0 = p_ticklabels->zvalues[0];
   *zmax_0 = p_ticklabels->zvalues[nzvalues-1];
   *zmin_0 = *zmin_0 - p_ticklabels->zoffset1;
   *zmax_0 = *zmax_0 + p_ticklabels->zoffset2;
   }
 else if ( strcmp(p_plot_param->axis_type, "semilogx") == 0 )
   {
   nxvalues = p_ticklabels->nxvalues;
   *xmin_0 = floor(p_ticklabels->xvalues[0]);
   *xmax_0 = ceil(p_ticklabels->xvalues[nxvalues - 1]);
   nxvalues = roundint(*xmax_0 - *xmin_0 + 1.0);
   nyvalues = p_ticklabels->nyvalues;
   *ymin_0 = p_ticklabels->yvalues[0];
   *ymax_0 = p_ticklabels->yvalues[nyvalues-1];
   *ymin_0 = *ymin_0 - p_ticklabels->yoffset1;
   *ymax_0 = *ymax_0 + p_ticklabels->yoffset2;
   }
 else if ( strcmp(p_plot_param->axis_type, "semilogy") == 0 ) 
   {
   nxvalues = p_ticklabels->nxvalues;
   *xmin_0 = p_ticklabels->xvalues[0];
   *xmax_0 = p_ticklabels->xvalues[nxvalues-1];
   *xmin_0 = *xmin_0 - p_ticklabels->xoffset1;
   *xmax_0 = *xmax_0 + p_ticklabels->xoffset2;
   nyvalues = p_ticklabels->nyvalues;
   *ymin_0 = floor(p_ticklabels->yvalues[0]);
   *ymax_0 = ceil(p_ticklabels->yvalues[nyvalues-1]);
   nyvalues = roundint(*ymax_0 - *ymin_0 + 1.0);
   }
 else if ( strcmp(p_plot_param->axis_type, "loglog") == 0 )
   {
   nxvalues = p_ticklabels->nxvalues;
   *xmin_0 = floor(p_ticklabels->xvalues[0]);
   *xmax_0 = ceil(p_ticklabels->xvalues[nxvalues-1]);
   nxvalues = roundint(*xmax_0 - *xmin_0 + 1.0);
   nyvalues = p_ticklabels->nyvalues;
   *ymin_0 = floor(p_ticklabels->yvalues[0]);
   *ymax_0 = ceil(p_ticklabels->yvalues[nyvalues-1]);
   nyvalues = roundint(*ymax_0 - *ymin_0 + 1.0);
   }
 else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
   {
   nyvalues = p_ticklabels->nyvalues;
   *rmin_0 = p_ticklabels->yvalues[0];
   *rmax_0 = p_ticklabels->yvalues[nyvalues-1];
   *rmin_0 = *rmin_0 - p_ticklabels->yoffset1;
   *rmax_0 = *rmax_0 + p_ticklabels->yoffset2;
   }
 else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
   {
   nxvalues = p_ticklabels->nxvalues;
   nyvalues = p_ticklabels->nyvalues;
   nzvalues = p_ticklabels->nzvalues;
   *xmin_0 = p_ticklabels->xvalues[0];
   *xmax_0 = p_ticklabels->xvalues[nxvalues-1];
   *ymin_0 = p_ticklabels->yvalues[0];
   *ymax_0 = p_ticklabels->yvalues[nyvalues-1];
   *zmin_0 = p_ticklabels->zvalues[0];
   *zmax_0 = p_ticklabels->zvalues[nzvalues-1];
   *xmin_0 = *xmin_0 - p_ticklabels->xoffset1;
   *xmax_0 = *xmax_0 + p_ticklabels->xoffset2;
   *ymin_0 = *ymin_0 - p_ticklabels->yoffset1;
   *ymax_0 = *ymax_0 + p_ticklabels->yoffset2;
   *zmin_0 = *zmin_0 - p_ticklabels->zoffset1;
   *zmax_0 = *zmax_0 + p_ticklabels->zoffset2;
   }

// Box corners (also used for 3d (see ColorPlot3d)
 *dev_x1_box0 = p_plot_box_data->xmin;
 *dev_x2_box0 = p_plot_box_data->xmax;
 *dev_y1_box0 = p_plot_box_data->ymin;
 *dev_y2_box0 = p_plot_box_data->ymax;

/****************** Calculate axis scale factors ******************/
if ( strcmp(p_plot_param->axis_type, "linear")    == 0 ||
      strcmp(p_plot_param->axis_type, "semilogx")  == 0 ||
      strcmp(p_plot_param->axis_type, "semilogy")  == 0 ||
      strcmp(p_plot_param->axis_type, "loglog")    == 0 )
    {
    *xscale_0 = (*dev_x2_box0 - *dev_x1_box0) / (*xmax_0 - *xmin_0);
    *yscale_0 = (*dev_y2_box0 - *dev_y1_box0) / (*ymax_0 - *ymin_0);
    xscalesq = *xscale_0 * *xscale_0;
    yscalesq = *yscale_0 * *yscale_0;
    }
 else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
    {
// Get size of window (from Gnome interface for instance)
    jlp_gseg1->GSEG_GetWindowLimits(&x0, &window_width0, &y0, &window_height0);

    *xorigin_0 = 0.375 * window_width0;
    *yorigin_0 = 0.500 * window_height0;
    if ( window_width0 >= window_height0 )
       *radius_0 = 0.375 * window_height0;
    else
       *radius_0 = 0.375 * window_width0;
    *rscale_0 = *radius_0 / (*rmax_0 - *rmin_0);
    }
 else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
    {
/* Get view angles */
    phi0 = p_plot_param_3d->phi;
    theta0 = p_plot_param_3d->theta;

    phi0 = fmod(phi0, 360.0);
    if ( phi0 < 0.0 ) phi0 = phi0 + 360.0;

    if ( 0.0 <= phi0 && phi0 < 90.0 )
      quadrant = 1;
    else if ( 90.0 <= phi0 && phi0 < 180.0 )
      quadrant = 2;
    else if ( 180.0 <= phi0 && phi0 < 270.0 )
      quadrant = 3;
    else
      quadrant = 4;
    p_plot_param_3d->quadrant = quadrant;

/* Get origin */
     if (quadrant == 1)
       {
       origin_0[0] = p_plot_param_3d->origin[0];
       origin_0[1] = p_plot_param_3d->origin[1];
       origin_0[2] = p_plot_param_3d->origin[2];
       }
    else if ( quadrant == 2 )
       {
       origin_0[0] = p_plot_param_3d->origin[0] + p_plot_param_3d->axis2[0];
       origin_0[1] = p_plot_param_3d->origin[1] + p_plot_param_3d->axis2[1];
       origin_0[2] = p_plot_param_3d->origin[2] - p_plot_param_3d->axis2[2];
       }
    else if ( quadrant == 3 )
       {
       origin_0[0] = p_plot_param_3d->origin[0] + p_plot_param_3d->axis1[0] 
                   + p_plot_param_3d->axis2[0];
       origin_0[1] = p_plot_param_3d->origin[1] + p_plot_param_3d->axis1[1] 
                   + p_plot_param_3d->axis2[1];
       origin_0[2] = p_plot_param_3d->origin[2] - p_plot_param_3d->axis1[2] 
                   - p_plot_param_3d->axis2[2];
       }
    else if ( quadrant == 4)
       {
       origin_0[0] = p_plot_param_3d->origin[0] + p_plot_param_3d->axis1[0];
       origin_0[1] = p_plot_param_3d->origin[1] + p_plot_param_3d->axis1[1];
       origin_0[2] = p_plot_param_3d->origin[2] - p_plot_param_3d->axis1[2];
       }
/* ZZZ : NOT IMPLEMENTED YET : Adjust view azimuth-angle as in Initialize3d ??? 
 if ( quadrant == 2 )
    phi0 = phi0 - 90.0;
 else if ( quadrant == 3 )
    phi0 = phi0 - 180.0;
 else if ( quadrant == 4 )
    phi0 = phi0 - 270.0;
*/

/* Calculate rotation matrices */
    phi0_rad = phi0 * DEGTORAD;
    theta0_rad = theta0 * DEGTORAD;

    Ry_0[0] = cos(-theta0_rad);
    Ry_0[1] = 0.0;
    Ry_0[2] = -sin(-theta0_rad);
    Ry_0[3] = 0.0;
    Ry_0[4] = 1.0;
    Ry_0[5] = 0.0;
    Ry_0[6] = sin(-theta0_rad);
    Ry_0[7] = 0.0;
    Ry_0[8] = cos(-theta0_rad);

    Rz_0[0] = cos(phi0_rad);
    Rz_0[1] = sin(phi0_rad);
    Rz_0[2] = 0.0;
    Rz_0[3] = -sin(phi0_rad);
    Rz_0[4] = cos(phi0_rad);
    Rz_0[5] = 0.0;
    Rz_0[6] = 0.0;
    Rz_0[7] = 0.0;
    Rz_0[8] = 1.0;

    ppp = multiply_mm(Ry_0, Rz_0);
    for ( i=1; i<=9; i++, ppp++ )
       Ryz_0[i-1] = *ppp;

/* Get axis length */
    axis_length = p_plot_param_3d->axis_length;

/* Calculate axis scale factors */
    *xscale_0 = axis_length / (*xmax_0 - *xmin_0);
    *yscale_0 = axis_length / (*ymax_0 - *ymin_0);
    *zscale_0 = axis_length / (*zmax_0 - *zmin_0);
    xscalesq = *xscale_0 * *xscale_0;
    yscalesq = *yscale_0 * *yscale_0;
    zscalesq = *zscale_0 * *zscale_0;
    }

// Specify number of coordinates of position vector
  *ncoords_0 = NCoords();

return(status);
}
/*****************************************************************************
*
*****************************************************************************/
int JLP_GsegAxes::NCoords()
{
int ncoords0 = 2;

// Specify number of coordinates of position vector
 if(strcmp(p_plot_param->axis_type, "linear")   == 0 ||
    strcmp(p_plot_param->axis_type, "semilogx") == 0 ||
    strcmp(p_plot_param->axis_type, "semilogy") == 0 ||
    strcmp(p_plot_param->axis_type, "loglog")   == 0 ||
    strcmp(p_plot_param->axis_type, "polar")    == 0)
    ncoords0 = 2;
 else if(strcmp(p_plot_param->axis_type, "3d") == 0)
    ncoords0 = 3;

return(ncoords0);
}
/*****************************************************************************
* Get the axis limits contained in the parameter file (if any)
*
*****************************************************************************/
void JLP_GsegAxes::GetPlotParamAxisLimits(double *axis_limits_0, 
                                          const int nlimits)
{
int i;
 if(nlimits != 6) {
  fprintf(stderr, "GetAxisLimits/Error: nlimits should be equal to 6 !\n");
  exit(-1);
  }
 for(i = 0; i < 6; i++) axis_limits_0[i] = p_plot_param->axis_limits[i];

 return; 
}
/*****************************************************************************
* Get the current values of the device coordinates of the plot box four corners
*
*****************************************************************************/
void JLP_GsegAxes::GetPlotBoxDataLimits(double *dev_x1_box, double *dev_x2_box,
                                        double *dev_y1_box, double *dev_y2_box)
{
 *dev_x1_box = p_plot_box_data->xmin;
 *dev_x2_box = p_plot_box_data->xmax;
 *dev_y1_box = p_plot_box_data->ymin;
 *dev_y2_box = p_plot_box_data->ymax;
}
