/**************************************************************************
* "jlp_gdev_curves.cpp"
*
* Definition of the members of the JLP_GDev class
* to load and plot curves
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

/*************************************************************
* Load plot the settings that will be used by PlotDisplay() 
* (generally called by external routines)
* INPUT:
* iplan: same scale in X and Y if iplan=1
* x1,x2,y1,y2 : x and y box boundaries (user coordinates)
*               x1=x2=0 if automatic x scale
*               y1=y2=0 if automatic y scale
*************************************************************/
int JLP_GDev::Curves_LoadPlotSettings(const char *xlabel, const char *ylabel,
                                      const char *title,
                                      const char *pen_colour, 
                                      const char *pen_default_colour, 
                                      const char *backgd_colour, 
                                      const int xgrid_is_wanted, 
                                      const int ygrid_is_wanted, 
                                      const int jlp_axes_are_wanted,
                                      const int iplan, 
                                      const double x1, const double x2,
                                      const double y1, const double y2)
{
// Labels:
strcpy(Jgc0.box_xlabel, xlabel);
strcpy(Jgc0.box_ylabel, ylabel);
strcpy(Jgc0.box_title, title);

strcpy(Jgc0.pen_colour, pen_colour);
strcpy(Jgc0.pen_default_colour, pen_default_colour);
strcpy(Jgc0.backgd_colour, backgd_colour);

// iplan: same scale in X and Y if iplan=1
Jgc0.box_plan = iplan;

// Background grid flag:
Jgc0.box_xgrid = xgrid_is_wanted;
Jgc0.box_ygrid = ygrid_is_wanted;

// JLp axes flag:
Jgc0.box_type = jlp_axes_are_wanted;

// Set the box with box_xmin, box_xmax, box_ymin, box_ymax
if((x1 == x2) || (y1 == y2)) 
  Curves_BoxLimitsAuto();
else
  SetBoxLimits(x1, x2, y1, y2, 0., 1.); 

// Update the parameters used by the popup menu
Update_GDProcSettings_from_JGC();

return(0);
}
/*************************************************************
* Plot the curves to current device 
*************************************************************/
int JLP_GDev::PlotAllCurves_splot()
{
char filename[128], comments[128];
int k, hardcopy_device, npts, error_bars0 = 0, full_caption0 = 0, idv0;
char axis_color[40], xlabel0[64], ylabel0[64], title0[128];
double box_xmin0, box_xmax0, box_ymin0, box_ymax0;

/** DEBUG: 
* WARNING: there is a huge number of calls here when enlarging the window !!!
printf("DEBUG/JLP_GDev::PlotAllCurves_splot() : ncurves_1=%d\n", ncurves_1);
**/
// Exit if no curve has been entered yet: 
if(ncurves_1 == 0) return(1);

// Update the private JGC parameters with the current values of the popup menu
Update_JGC_from_GDProcSettings();

// Now plot the curves, (first get the idv number):
idv0 = Jgc0.dev_idv;

if(idv0 == -1) {
  fprintf(stderr, "JLP_GDev::PlotAllCurves/Error: wrong idv number, idev0 = %d\n", idv0);
  return(-1);
 }

// Erase idv0 device first:
  jlp_erase(idv0);

strcpy(filename, "");
strcpy(comments,"Test comments");
strcpy(xlabel0, Jgc0.box_xlabel);
strcpy(ylabel0, Jgc0.box_ylabel);
strcpy(title0, Jgc0.box_title);
strcpy(axis_color, Jgc0.pen_default_colour);
box_xmin0 = Jgc0.axis_limits[0];
box_xmax0 = Jgc0.axis_limits[1];
box_ymin0 = Jgc0.axis_limits[2];
box_ymax0 = Jgc0.axis_limits[3];
jlp_box(box_xmin0, box_xmax0, box_ymin0, box_ymax0, xlabel0, ylabel0, title0,
        Jgc0.box_ticks_in, 1, full_caption0, Jgc0.box_type, 
        Jgc0.box_xgrid, Jgc0.box_ygrid, 
        Jgc0.xaxis_type, Jgc0.yaxis_type, Jgc0.expand, axis_color);

hardcopy_device = (Jgdev_dev_type(idv0) == 2) ? 1 : 0;
if(hardcopy_device && full_caption0)
       jlp_comments_for_curves(filename, comments, Jgc0.expand, idv0);

// Draw in loop with jlp_curve
 for(k = 0; k < ncurves_1; k++) {
    if(npts_1[k] > 0) {
      npts = npts_1[k];
      error_bars0 = 1;
      draw_curve(&xplot_1[nmaxi_1 * k], &yplot_1[nmaxi_1 * k],
                &errorx_1[nmaxi_1 * k], &errory_1[nmaxi_1 * k],
                npts, &nchar_1[4*k], &pcolor_1[32*k], error_bars0);
     }
  }

 JLP_GFLUSH(&idv0);

return(0);
}
/************************************************************************
* Update the position of the vertical cursor
* INPUT:
*  x_position_value: x position_value in user coordinates 
************************************************************************/
void JLP_GDev::UpdateCursor(const double x_position_value)
{
double x1;
float xmindata0, xmaxdata0, ymindata0, ymaxdata0;
double ymin_data0, ymax_data0;
int offx0, offy0, axlen0, aylen0, iplan0, idv0;

if(ncurves_1 > 0) {
  PlotAllCurves_splot();
// Draw a blue vertical line:
  setrgbcolor(0., 0., 1.);
// Read the values of offx0, offy0, axlen0, aylen0 :
  idv0 = Jgc0.dev_idv;
  JLP_GET_PLOT_PARAM(&offx0, &offy0, &axlen0, &aylen0, &xmindata0, &xmaxdata0,
                     &ymindata0, &ymaxdata0, &iplan0, &idv0);

  x1 = x_position_value; 
  ymin_data0 = ymindata0;
  ymax_data0 = ymaxdata0;
  gdev_line1(x1, ymin_data0, x1, ymax_data0, 0, 0);

// Go back to black color:
  setrgbcolor(0., 0., 0.);
 } 

return;
}
