/*******************************************************************************
*
* jlp_Gsegraf_DataTools.cpp
* JLP_Gsegraf class
*
* Interface with JLP_GsegData routines
*
*******************************************************************************/
#include <stdlib.h>          // exit()
#include <math.h>
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"      // JLP_GsegAxes class
#include "jlp_gseg_data.h"      // JLP_GsegData class

/*****************************************************************************
*
*****************************************************************************/
void JLP_Gsegraf::GSEG_get_nplots(int *nplots0)
{
*nplots0 = 0;
if(jlp_gseg_data1 != NULL) jlp_gseg_data1->Get_nplots(nplots0);

return;
}
/*****************************************************************************
*
*****************************************************************************/
int JLP_Gsegraf::GSEG_get_ncontour_plots(int *ncontour_plots0)
{
int status = -1;

*ncontour_plots0 = 0;
if(jlp_gseg_data1 != NULL) 
             status = jlp_gseg_data1->Get_ncontour_plots(ncontour_plots0);

return(status);
}
/*****************************************************************************
*
*****************************************************************************/
int JLP_Gsegraf::GSEG_get_plot_type(const int iplot0, int *gseg_plot_type0)
{
int status = -1;

*gseg_plot_type0 = 0;
if(jlp_gseg_data1 != NULL) { 
    status = jlp_gseg_data1->GetGsegPlotType(iplot0, gseg_plot_type0);
   }

return(status);
}
/*****************************************************************************
*
*****************************************************************************/
int JLP_Gsegraf::GSEG_get_style_flag(const int iplot0, int *style_flag0)
{
int status = -1;

*style_flag0 = 0;
if(jlp_gseg_data1 != NULL) 
    status = jlp_gseg_data1->GetStyleFlag(iplot0, style_flag0);

return(status);
}
/*****************************************************************************
*
*****************************************************************************/
void JLP_Gsegraf::GSEG_DataMinMax(const int high_contrast)
{

if(jlp_gseg_data1 != NULL) jlp_gseg_data1->DataMinMax(high_contrast);

return;
}
/*****************************************************************************
*
*****************************************************************************/
void JLP_Gsegraf::GSEG_GetDataMinMax(double *data_xmin0, double *data_xmax0, 
                                     double *data_ymin0, double *data_ymax0, 
                                     double *data_zmin0, double *data_zmax0)
{
if(jlp_gseg_data1 != NULL) jlp_gseg_data1->GetDataMinMax(data_xmin0, 
                                             data_xmax0, data_ymin0, data_ymax0,
                                             data_zmin0, data_zmax0);
}
/*****************************************************************************
*
*****************************************************************************/
void JLP_Gsegraf::GSEG_SetDataMinMax(const double data_xmin0, 
                                     const double data_xmax0,
                                     const double data_ymin0, 
                                     const double data_ymax0,
                                     const double data_zmin0, 
                                     const double data_zmax0)
{
if(jlp_gseg_data1 != NULL) jlp_gseg_data1->SetDataMinMax(data_xmin0, 
                                             data_xmax0, data_ymin0, data_ymax0,
                                             data_zmin0, data_zmax0);
}
/*****************************************************************************
*
* INPUT:
*  xmouse, ymouse: device coordinates of the location of the mouse
*  icontourplot : number of contour plot (from 0 to ncontour_plots - 1)
*
* OUTPUT:
*  zcontour_data : z value (intensity in user coordinates)
*  labelled_contour_value : value of the nearest contour
*****************************************************************************/
int JLP_Gsegraf::GSEG_CreateContourLabel(double xmouse, double ymouse,
                                         int icontourplot,
                                         double *zcontour_data,
                                         double *labelled_contour_value)
{
double dev_x1_box, dev_x2_box, dev_y1_box, dev_y2_box; 
double xmin, xmax, ymin, ymax, zmin, zmax, xscale, yscale;
double x_user, y_user;
int in_frame, status = -1;

// Get user coordinates 
 jlp_gseg_axes1->ContourLabel_FromDevToUser(xmouse, ymouse, &x_user, &y_user, 
                                            &in_frame);
 if(in_frame != 1) return(-1);

/* Get plot box settings */
 jlp_gseg_axes1->GetBoxSettingsForLinear(&dev_x1_box, &dev_x2_box, &dev_y1_box, 
                                         &dev_y2_box, &xmin, &xmax, &ymin,
                                         &ymax, &zmin, &zmax, &xscale, &yscale);


 if(jlp_gseg_data1 != NULL) status = jlp_gseg_data1->CreateContourLabel(
                                                     icontourplot, x_user, 
                                                     y_user, zcontour_data, 
                                                     zmin, zmax, 
                                                     labelled_contour_value);
return(status);
}
/*****************************************************************************
*
*****************************************************************************/
int JLP_Gsegraf::GSEG_GetImageArray(double **array0, int *nx0, int *ny0)
{
int status = -1;

if(jlp_gseg_data1 != NULL) { 
    status = jlp_gseg_data1->GetImageArray(array0, nx0, ny0);
   }

return(status);
}
