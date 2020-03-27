/************************************************************************
* Routines used by the JLP_Plot1 routines (to read JLP plot1 parameter files)
*
* JLP
* Version 19/03/2019
*************************************************************************/
#include "jlp_plot1_def.h" // PLOT1_FILE_... structures
#include "jlp_plot1_gsegraf.h"     // prototypes of the jlp_plot1 routines defined here 
#include "jlp_gseg_plot_data1.h"    // jlp_gseg_init_plot_data()
#include "jlp_gseg_axis_data1.h"    // jlp_gseg_init_axis_data()

/************************************************************************
* Load the axis settings of pset0 (from the parameter file) to gseg_axdata0   
*
* INPUT:
*  pset0 : PLOT1_SETTINGS structure with setting from JLP plot1 parameter file
*
* OUTPUT:
*  gseg_axdata0 : GSEG_AXIS_DATA structure
*************************************************************************/
int Plot1_PlotSettingsToGsegAxisData(PLOT1_SETTINGS pset0, 
                                     GSEG_AXIS_DATA *gseg_axdata0)
{
int xgrid, ygrid, dummy_gdev_graphic_type0, nval, i; 
double ax0[6];

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

// Minimum configuration:
 dummy_gdev_graphic_type0 = 4;
 jlp_gseg_init_axis_data(dummy_gdev_graphic_type0, gseg_axdata0);

// Load additional parameters contained in pset0:

 strcpy(gseg_axdata0->xlabel, pset0.xlabel);
 strcpy(gseg_axdata0->ylabel, pset0.ylabel);
 strcpy(gseg_axdata0->zlabel, pset0.zlabel);
 strcpy(gseg_axdata0->title, pset0.title);

/*** Further transfer from pset0 to gseg_axdata, TOBEDONE:
 if(equal_scale0 == 1)
    strcpy(gseg_axdata0->axis_scale, "equal");
 else
    strcpy(gseg_axdata0->axis_scale, "auto");
//
// Reversed axes:
 gseg_axdata0->reversed_axis[0] = x_rev0;
 gseg_axdata0->reversed_axis[1] = y_rev0;
 gseg_axdata0->reversed_axis[2] = z_rev0;
***/

// Grid:
// axis_settings: flags(0/1) for jlp_axis, xgrid, ygrid, xlog, ylog
 if(pset0.axis_settings[1] == '1') xgrid = 1;
  else xgrid = 0;
 if(pset0.axis_settings[2] == '1') ygrid = 1;
  else ygrid = 0;
 if(xgrid == 1 || ygrid == 1) {
    strcpy(gseg_axdata0->grid, "on1");
    gseg_axdata0->gridchar1 = 'l';
    gseg_axdata0->gridchar2 = 's';
    gseg_axdata0->gridcolor1 = 0x0F0F00FF;
  } else {
    strcpy(gseg_axdata0->grid, "off");
  }
 nval = sscanf(pset0.axis_limits, "%lf %lf %lf %lf", &ax0[0], &ax0[1], 
        &ax0[2], &ax0[3]);
 for(i = 0; i < nval; i++) {
   gseg_axdata0->axis_limits[i] = ax0[i];
   gseg_axdata0->set_axis_limits[i] = 1;
   }

return(0);
}
