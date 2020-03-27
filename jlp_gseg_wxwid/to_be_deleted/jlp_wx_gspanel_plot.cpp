/******************************************************************************
* Name:       jlp_wx_plot JLP_wxGsegPanel class
* Purpose:     displaying plots 
*
* Author:      JLP 
* Version:     14/11/2016
******************************************************************************/
#include "jlp_wx_gscanvas.h"
#include "jlp_splot.h"        // newplot210
#include "jlp_plotlib.h"      // JLP_SET_PLOT_PARAM 
#include "jlp_wx_gspanel.h"    // JLP_wxGsegPanel class 

/***************************************************************************
* Update cursor position 
* (called by JLP_wxVideoPanel when frame number has changed) 
*
* INPUT:
*  x_position: frame number
***************************************************************************/
void JLP_wxGsegPanel::UpdateCursor(const int x_position)
{
double x_value;
if(initialized == 1234) {
// Conversion from frame number to wavelength value:
  x_value = (x_position - 1) * wavel_step1 + wavel_start1;
  jlp_wxgseg_canvas1->UpdateCursor(x_value);
  }
}
/***************************************************************************
* ClearDrawingDisplay 
* (called by external routines)
***************************************************************************/
void JLP_wxGsegPanel::ClearDrawingDisplay() 
{

if(initialized == 1234) jlp_wxgseg_canvas1->ClearDrawingDisplay();

}
/***************************************************************************
* PlotToDrawingDisplay 
* (called by external routines)
***************************************************************************/
void JLP_wxGsegPanel::PlotToDrawingDisplay() 
{

if(initialized == 1234) jlp_wxgseg_canvas1->PlotToDrawingDisplay();

}
/***************************************************************************
* LoadPlotData
* Load data to xplot and yplot arrays
* (called by external routines)
*
* INPUT: 
*   reset_first: flag used to reset all private arrays
*                (if 1: erase all old data; if 0: add this data to old data)
*
***************************************************************************/
int JLP_wxGsegPanel::LoadPlotData(double *xplot, double *yplot, 
                                     const int npts, const char *nchar_type, 
                                     const char *pcolor, 
                                     const char *plot_fname, 
                                     const int reset_first)
{
int status = -1;

if(initialized == 1234) {
  status = jlp_wxgseg_canvas1->LoadPlotData(xplot, yplot, npts, nchar_type, pcolor, 
                                       plot_fname, reset_first);
  }

return(status);
}
/************************************************************************
* Setup plot configuration
* (called by external routines)
*
* Allocate memory if needed
************************************************************************/
void JLP_wxGsegPanel::InitPlotData(const int nmaxi, const int ncurves_maxi, 
                                      const int nout_maxi, 
                                      const double wavel_start,
                                      const double wavel_step)
{

if(initialized == 1234) {
  jlp_wxgseg_canvas1->InitPlotData(nmaxi, ncurves_maxi, nout_maxi);
// Save to private parameters:
  wavel_start1 = wavel_start; 
  wavel_step1 = wavel_step; 
  }

}
/*************************************************************
* Load plot the settings that will be used by PlotDisplay() 
* (generally called by external routines)
*************************************************************/
int JLP_wxGsegPanel::LoadPlotSettings(const char *xlabel, const char *ylabel,
                                         const char *title, 
                                         const int xgrid_is_wanted, 
                                         const int ygrid_is_wanted, 
                                         const int jlp_axes_are_wanted,
                                         const int iplan, const double x1,
                                         const double x2, const double y1,
                                         const double y2)
{
int status = -1;

if(initialized == 1234) {
  status = jlp_wxgseg_canvas1->LoadPlotSettings(xlabel, ylabel, title, 
                                           xgrid_is_wanted, ygrid_is_wanted,
                                           jlp_axes_are_wanted, iplan, 
                                           x1, x2, y1, y2);
  }

return(status);
}
