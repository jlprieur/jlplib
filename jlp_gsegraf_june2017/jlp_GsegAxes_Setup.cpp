/******************************************************************************
* jlp_Gsegraf_Setup.cpp
* JLP_GsegAxes class
*
* JLP
* Version 01/04/2017
******************************************************************************/
#include <stdlib.h>          // exit()
#include <math.h>
#include <time.h>
#include "jlp_gseg_axes.h"

/***************************************************************************
* Check axis type 
*
***************************************************************************/
int JLP_GsegAxes::CheckAxesParameters()
{
int status = 0;
char error_str0[80], *pchar;

/* Check axis_type parameter */
   if ( strcmp(p_plot_param->axis_type, "linear")   != 0 &&
        strcmp(p_plot_param->axis_type, "semilogx") != 0 &&
        strcmp(p_plot_param->axis_type, "semilogy") != 0 &&
        strcmp(p_plot_param->axis_type, "loglog")   != 0 &&
        strcmp(p_plot_param->axis_type, "polar")    != 0 &&
        strcmp(p_plot_param->axis_type, "3d")       != 0 )
      {
      strcpy(error_str0, "Invalid or missing axis_type parameter.");
      JLP_ErrorDialog(error_str0);
      status = -1;
      }

/* Check axis_scale parameter */
   if ( strcmp(p_plot_param->axis_scale, "auto")  != 0 &&
        strcmp(p_plot_param->axis_scale, "equal") != 0 )
      {
      strcpy(error_str0, "Invalid axis_scale parameter.");
      JLP_ErrorDialog(error_str0);
      status = -1;
      }


/* Check grid parameters */
   if ( strcmp(p_plot_param->grid, "on1") != 0 &&
        strcmp(p_plot_param->grid, "on2") != 0 &&
        strcmp(p_plot_param->grid, "off") != 0 )
      {
      strcpy(error_str0, "Invalid grid parameter.");
      JLP_ErrorDialog(error_str0);
      status = -1;
      }

// Decodes grid color using color_string (i.e., "kaswrylqbfmogtnpx")
   if ( strcmp(p_plot_param->grid, "on1") == 0 )
      {
      if ( (gridchar1 != 'l' && gridchar1 != 'd' && gridchar1 != '.') ||
           (pchar = strchr(color_string, gridchar2)) == NULL )
         {
         strcpy(error_str0, "Invalid grid parameter.");
         JLP_ErrorDialog(error_str0);
         status = -1;
         }
      }
   else if ( strcmp(p_plot_param->grid, "on2") == 0 )
      {
      if ( gridchar1 != 'l' && gridchar1 != 'd' )
         {
         strcpy(error_str0, "Invalid grid parameter.");
         JLP_ErrorDialog(error_str0);
         status = -1;
         }
      }
   /* Check view-direction angles for 3d plots */
   if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
      if ( p_plot_param_3d->theta <  0.0 ||
           p_plot_param_3d->theta > 90.0 )
         {
         strcpy(error_str0, "View-direction elevation angle out of range.");
         JLP_ErrorDialog(error_str0);
         status = -1;
         }

   /* Check plot_box parameter */
   if ( strcmp(p_plot_param->plot_box, "on")  != 0 &&
        strcmp(p_plot_param->plot_box, "off") != 0 )
      {
      strcpy(error_str0, "Invalid plot_box parameter.");
      JLP_ErrorDialog(error_str0);
      status = -1;
      }


   /* Check x_tick_marks parameter */
   if ( strcmp(p_plot_param->x_tick_marks, "on")  != 0 &&
        strcmp(p_plot_param->x_tick_marks, "off") != 0 )
      {
      strcpy(error_str0, "Invalid x_tick_marks parameter.");
      JLP_ErrorDialog(error_str0);
      status = -1;
      }


   /* Check y_tick_marks parameter */
   if ( strcmp(p_plot_param->y_tick_marks, "on")  != 0 &&
        strcmp(p_plot_param->y_tick_marks, "off") != 0 )
      {
      strcpy(error_str0, "Invalid y_tick_marks parameter.");
      JLP_ErrorDialog(error_str0);
      status = -1;
      }


   /* Check z_tick_marks parameter */
   if ( strcmp(p_plot_param->z_tick_marks, "on")  != 0 &&
        strcmp(p_plot_param->z_tick_marks, "off") != 0 )
      {
      strcpy(error_str0, "Invalid z_tick_marks parameter.");
      JLP_ErrorDialog(error_str0);
      status = -1;
      }

   /* Check x_tick_labels parameter */
   if ( strcmp(p_plot_param->x_tick_labels, "on")  != 0 &&
        strcmp(p_plot_param->x_tick_labels, "off") != 0 )
      {
      strcpy(error_str0, "Invalid x_tick_labels parameter.");
      JLP_ErrorDialog(error_str0);
      exit(1);
      }


   /* Check y_tick_labels parameter */
   if ( strcmp(p_plot_param->y_tick_labels, "on")  != 0 &&
        strcmp(p_plot_param->y_tick_labels, "off") != 0 )
      {
      strcpy(error_str0, "Invalid y_tick_labels parameter.");
      JLP_ErrorDialog(error_str0);
      status = -1;
      }


   /* Check z_tick_labels parameter */
   if ( strcmp(p_plot_param->z_tick_labels, "on")  != 0 &&
        strcmp(p_plot_param->z_tick_labels, "off") != 0 )
      {
      strcpy(error_str0, "Invalid z_tick_labels parameter.");
      JLP_ErrorDialog(error_str0);
      status = -1;
      }

   /* Check date_time parameter */
   if ( strcmp(p_plot_param->date_time_anchor, "off")       != 0 &&
        strcmp(p_plot_param->date_time_anchor, "north")     != 0 &&
        strcmp(p_plot_param->date_time_anchor, "northeast") != 0 &&
        strcmp(p_plot_param->date_time_anchor, "southeast") != 0 &&
        strcmp(p_plot_param->date_time_anchor, "south")     != 0 &&
        strcmp(p_plot_param->date_time_anchor, "southwest") != 0 &&
        strcmp(p_plot_param->date_time_anchor, "northwest") != 0 )
      {
      strcpy(error_str0, "Invalid date_time parameter.");
      JLP_ErrorDialog(error_str0);
      status = -1;
      }


if(status == -1) exit(1);

return(status);
}
/***************************************************************************
* Set axis_type flags
*
***************************************************************************/
void JLP_GsegAxes::SetAxisTypeFlags(char *axis_type0)
{
   strcpy(p_plot_param->axis_type, axis_type0);
   flag_linear_1 = 0;
   flag_logx_1 = 0;
   flag_logy_1 = 0;
   flag_loglog_1 = 0;
   flag_polar_1 = 0;
   flag_3d_1 = 0;
   flag_2d_1 = 0;
   flag_2d_rect_1 = 0;
   if ( strcmp(axis_type0, "linear") == 0 )
      flag_linear_1 = 1;
   else if ( strcmp(axis_type0, "semilogx") == 0 )
      flag_logx_1 = 1;
   else if ( strcmp(axis_type0, "semilogy") == 0 )
      flag_logy_1 = 1;
   else if ( strcmp(axis_type0, "loglog") == 0 )
      flag_loglog_1 = 1;
   else if ( strcmp(axis_type0, "polar") == 0 )
      flag_polar_1 = 1;
   else if ( strcmp(axis_type0, "3d") == 0 )
      flag_3d_1 = 1;

   if ( flag_linear_1 == 1 ||
        flag_logx_1   == 1 ||
        flag_logy_1   == 1 ||
        flag_loglog_1 == 1 ||
        flag_polar_1  == 1 )
      flag_2d_1 = 1;

   if ( flag_linear_1 == 1 ||
        flag_logx_1   == 1 ||
        flag_logy_1   == 1 ||
        flag_loglog_1 == 1 )
      flag_2d_rect_1 = 1;
return;
}
/*****************************************************************************
* Set the axis limits in the same way as if it was read from the parameter file
*
*****************************************************************************/
void JLP_GsegAxes::SetPlotParamAxisLimits(double *axis_limits_0, 
                                          const int nlimits)
{
int i;
 if(nlimits != 6) {
  fprintf(stderr, "SetAxisLimits/Error: nlimits should be equal to 6 !\n");
  exit(-1);
  }
 for(i = 0; i < 6; i++) p_plot_param->axis_limits[i] = axis_limits_0[i];

 return; 
}
/*****************************************************************************
* Turn axis limits on/off 
* if set_axis_limits=0, axis limits is turned off
* if set_axis_limits=1, axis limits is turned on
*****************************************************************************/
void JLP_GsegAxes::SetAxisLimitsOnOff(int *set_axis_limits_0, 
                                          const int nlimits)
{
int i;
 if(nlimits != 6) {
  fprintf(stderr, "SetAxisLimitsOnOff/Error: nlimits should be equal to 6 !\n");
  exit(-1);
  }
 for(i = 0; i < 6; i++) set_axis_limits1[i] = set_axis_limits_0[i];

 return; 
}
/*****************************************************************************
* Get axis limits on/off flags 
*****************************************************************************/
void JLP_GsegAxes::GetAxisLimitsOnOff(int *set_axis_limits_0, 
                                          const int nlimits)
{
int i;
 if(nlimits != 6) {
  fprintf(stderr, "SetAxisLimitsOnOff/Error: nlimits should be equal to 6 !\n");
  exit(-1);
  }
 for(i = 0; i < 6; i++) set_axis_limits_0[i] = set_axis_limits1[i];

 return; 
}
/*****************************************************************************
*
*****************************************************************************/
void JLP_GsegAxes::SetTickOffsetLabels(const double xoffset1_0, 
                                       const double xoffset2_0, 
                                       const double yoffset1_0, 
                                       const double yoffset2_0,
                                       const double zoffset1_0, 
                                       const double zoffset2_0)
{
 p_ticklabels->xoffset1 = xoffset1_0; 
 p_ticklabels->xoffset2 = xoffset2_0;
 p_ticklabels->yoffset1 = yoffset1_0; 
 p_ticklabels->yoffset2 = yoffset2_0; 
 p_ticklabels->zoffset1 = zoffset1_0; 
 p_ticklabels->zoffset2 = zoffset2_0; 
 
 return; 
}
/*****************************************************************************
* Set plot settings back to their original values (stored as "refrence" values)
*
*****************************************************************************/
void JLP_GsegAxes::SetPlotSettingsToRefValues()
{
int i, nx, ny, nz;

   if ( strcmp(p_plot_param->axis_type, "linear")   == 0 ||
        strcmp(p_plot_param->axis_type, "semilogx") == 0 ||
        strcmp(p_plot_param->axis_type, "semilogy") == 0 ||
        strcmp(p_plot_param->axis_type, "loglog")   == 0 )
      {
      /* Get tick-mark reference values */
      nx = p_ticklabels->nxvalues_ref;
      ny = p_ticklabels->nyvalues_ref;
      nz = p_ticklabels->nzvalues_ref;
      p_ticklabels->nxvalues = nx;
      p_ticklabels->nyvalues = ny;
      p_ticklabels->nzvalues = nz;
      for ( i=1; i<=nx; i++ )
         p_ticklabels->xvalues[i-1] = p_ticklabels->xvalues_ref[i-1];
      for ( i=1; i<=ny; i++ )
         p_ticklabels->yvalues[i-1] = p_ticklabels->yvalues_ref[i-1];
      for ( i=1; i<=nz; i++ )
         p_ticklabels->zvalues[i-1] = p_ticklabels->zvalues_ref[i-1];
      p_ticklabels->xoffset1 = p_ticklabels->xoffset1_ref;
      p_ticklabels->xoffset2 = p_ticklabels->xoffset2_ref;
      p_ticklabels->yoffset1 = p_ticklabels->yoffset1_ref;
      p_ticklabels->yoffset2 = p_ticklabels->yoffset2_ref;
      p_ticklabels->zoffset1 = p_ticklabels->zoffset1_ref;
      p_ticklabels->zoffset2 = p_ticklabels->zoffset2_ref;
      }
   else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
      {
      /* Get tick-mark reference values */
      ny = p_ticklabels->nyvalues_ref;
      p_ticklabels->nyvalues = ny;
      for ( i=1; i<=ny; i++ )
         p_ticklabels->yvalues[i-1] = p_ticklabels->yvalues_ref[i-1];
      p_ticklabels->yoffset1 = p_ticklabels->yoffset1_ref;
      p_ticklabels->yoffset2 = p_ticklabels->yoffset2_ref;
      }
  else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
      {
      /* Get tick-mark reference values */
      nx = p_ticklabels->nxvalues_ref;
      ny = p_ticklabels->nyvalues_ref;
      nz = p_ticklabels->nzvalues_ref;
      p_ticklabels->nxvalues = nx;
      p_ticklabels->nyvalues = ny;
      p_ticklabels->nzvalues = nz;
      for ( i=1; i<=nx; i++ )
         p_ticklabels->xvalues[i-1] = p_ticklabels->xvalues_ref[i-1];
      for ( i=1; i<=ny; i++ )
         p_ticklabels->yvalues[i-1] = p_ticklabels->yvalues_ref[i-1];
      for ( i=1; i<=nz; i++ )
         p_ticklabels->zvalues[i-1] = p_ticklabels->zvalues_ref[i-1];
      p_ticklabels->xoffset1 = p_ticklabels->xoffset1_ref;
      p_ticklabels->xoffset2 = p_ticklabels->xoffset2_ref;
      p_ticklabels->yoffset1 = p_ticklabels->yoffset1_ref;
      p_ticklabels->yoffset2 = p_ticklabels->yoffset2_ref;
      p_ticklabels->zoffset1 = p_ticklabels->zoffset1_ref;
      p_ticklabels->zoffset2 = p_ticklabels->zoffset2_ref;

      /* Get view-angle reference values */
      p_plot_param_3d->phi   = p_plot_param_3d->phi_ref;
      p_plot_param_3d->theta = p_plot_param_3d->theta_ref;
   }

return;
}
/*****************************************************************************
*
*****************************************************************************/
void JLP_GsegAxes::Set3DParams(const double phi_0, const double theta_0)
{
 p_plot_param_3d->phi = phi_0;
 p_plot_param_3d->theta = theta_0;

 return; 
}
/*****************************************************************************
* Compute plot-box device coordinates from current window size,
* and set them to the coordinates of the plot box four corners 
*
* INPUT:
*  p_plot_param : number of plots, etc 
*  plot_types : "color", "contour", etc
*
* OUTPUT:
*  p_plot_box_data : xmin, xmax, ymin, ymax
*****************************************************************************/
void JLP_GsegAxes::SetPlotBoxDataLimitsFromWindowSize(void)
{
/* Declare variables */
int iplot, nplots0, flag, *styleflags0;
int x0, y0, window_width0, window_height0;
char *plot_types0;

// Size of window (from Gnome interface for instance)
 jlp_gseg1->GSEG_GetWindowLimits(&x0, &window_width0, &y0, &window_height0);

/* Specify plot box screen coordinates */
  jlp_gsegraf1->GSEG_get_nplots(&nplots0);
  jlp_gsegraf1->GSEG_SetPointersFromPlot(&plot_types0, &styleflags0);
  flag = 0;
  if ( strcmp(p_plot_param->axis_type, "linear") == 0 )
     for ( iplot = 1; iplot <= nplots0; iplot++ )
        if ( strcmp(&plot_types0[(iplot-1)*10], "color") == 0 ||
             (strcmp(&plot_types0[(iplot-1)*10], "contour") == 0 &&
              styleflags0[iplot-1] == 7) )
           flag = 1;

/* Plot types: points, contour, and histogram */
  if ( flag == 0 )
     {
     p_plot_box_data->xmin = 0.15625 * window_width0;
     p_plot_box_data->xmax = 0.90625 * window_width0;
     p_plot_box_data->ymin = 0.09375 * window_height0;
     p_plot_box_data->ymax = 0.84375 * window_height0;
     }

/* Plot types: points, contour, histogram, and color */
  else if ( flag == 1 )
     {
     p_plot_box_data->xmin = 0.18750 * window_width0;
     p_plot_box_data->xmax = 0.75000 * window_width0;
     p_plot_box_data->ymin = 0.09375 * window_height0;
     p_plot_box_data->ymax = 0.84375 * window_height0;
     }

return;
}
/*****************************************************************************
* Set the device coordinates of the plot box four corners
*
*****************************************************************************/
void JLP_GsegAxes::SetPlotBoxDataLimits(const double dev_x1_box, 
                                        const double dev_x2_box,
                                        const double dev_y1_box, 
                                        const double dev_y2_box)
{
 p_plot_box_data->xmin = dev_x1_box;
 p_plot_box_data->xmax = dev_x2_box;
 p_plot_box_data->ymin = dev_y1_box;
 p_plot_box_data->ymax = dev_y2_box;
}
/*****************************************************************************
* Set pixbufs for xlabel, ylabel, zlabel, and title
*****************************************************************************/
void JLP_GsegAxes::SetAxisLabelPixbufs()
{
if(pixbuf_xlabel.text != NULL) delete[] pixbuf_xlabel.text;
pixbuf_xlabel.text = NULL;

if(pixbuf_ylabel.text != NULL) delete[] pixbuf_ylabel.text;
pixbuf_ylabel.text = NULL;

if(pixbuf_zlabel.text != NULL) delete[] pixbuf_zlabel.text;
pixbuf_zlabel.text = NULL;

if(pixbuf_title.text != NULL) delete[] pixbuf_title.text;
pixbuf_title.text = NULL;

if(pixbuf_xlabel.font_name != NULL) delete[] pixbuf_xlabel.font_name;
pixbuf_xlabel.font_name = NULL;

if(pixbuf_ylabel.font_name != NULL) delete[] pixbuf_ylabel.font_name;
pixbuf_ylabel.font_name = NULL;

if(pixbuf_zlabel.font_name != NULL) delete[] pixbuf_zlabel.font_name;
pixbuf_zlabel.font_name = NULL;

if(pixbuf_title.font_name != NULL) delete[] pixbuf_title.font_name;
pixbuf_title.font_name = NULL;

if(xlabel1 != NULL) {
 pixbuf_xlabel.text = new char[128];
 strcpy(pixbuf_xlabel.text, xlabel1);
 pixbuf_xlabel.font_name = new char[128];
 strcpy(pixbuf_xlabel.font_name, "font_axis_labels");
 pixbuf_xlabel.font_size = font_size_axis_labels1;
 }

if(ylabel1 != NULL) {
 pixbuf_ylabel.text = new char[128];
 strcpy(pixbuf_ylabel.text, ylabel1);
 pixbuf_ylabel.font_name = new char[128];
 strcpy(pixbuf_ylabel.font_name, "font_axis_labels");
 pixbuf_ylabel.font_size = font_size_axis_labels1;
 }

if(zlabel1 != NULL) {
 pixbuf_zlabel.text = new char[128];
 strcpy(pixbuf_zlabel.text, zlabel1);
 pixbuf_zlabel.font_name = new char[128];
 strcpy(pixbuf_zlabel.font_name, "font_axis_labels");
 pixbuf_zlabel.font_size = font_size_axis_labels1;
 }

if(title1 != NULL) {
 pixbuf_title.text = new char[128];
 strcpy(pixbuf_title.text, title1);
 pixbuf_title.font_name = new char[128];
 strcpy(pixbuf_title.font_name, "font_axis_title");
 pixbuf_title.font_size = font_size_title1;
 }

return;
}
/*****************************************************************************
*
*****************************************************************************/
void JLP_GsegAxes::GetFontParams(char *font_name0, double *font_size_title0,
                                 double *font_size_axis_labels0, 
                                 double *font_size_tick_labels0,
                                 double *font_size_text0, 
                                 double *font_size_legend0,
                                 double *font_size_date_time0)
{
// Get values from private variables:
strcpy(font_name0, font_name1);
*font_size_title0 = font_size_title1;
*font_size_axis_labels0 = font_size_axis_labels1;
*font_size_tick_labels0 = font_size_tick_labels1;
*font_size_text0 = font_size_text1;
*font_size_legend0 = font_size_legend1;
*font_size_date_time0 = font_size_date_time1;

return;
}
/*****************************************************************************
*
*****************************************************************************/
void JLP_GsegAxes::SetFontParams(char *font_name0, double font_size_title0,
                                 double font_size_axis_labels0, 
                                 double font_size_tick_labels0,
                                 double font_size_text0, 
                                 double font_size_legend0,
                                 double font_size_date_time0)
{
// Save values to private variables:
strcpy(font_name1, font_name0);
font_size_title1 = font_size_title0;
font_size_axis_labels1 = font_size_axis_labels0;
font_size_tick_labels1 = font_size_tick_labels0;
font_size_text1 = font_size_text0;
font_size_legend1 = font_size_legend0;
font_size_date_time1 = font_size_date_time0;

return;
}
