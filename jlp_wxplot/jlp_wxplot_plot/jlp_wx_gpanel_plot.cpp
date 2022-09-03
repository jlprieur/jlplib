/******************************************************************************
* Name:       jlp_wx_plot JLP_wxGraphicPanel class
* Purpose:     displaying plots 
*
* Author:      JLP 
* Version:     24/07/2015
******************************************************************************/
#include "jlp_gdev_wxwid.h"
#include "jlp_wx_gpanel.h"    // JLP_wxGraphicPanel class 

/***************************************************************************
* Update cursor position (for curves) 
* (called by JLP_wxVideoPanel when frame number has changed) 
*
* INPUT:
*  x_position: frame number
***************************************************************************/
void JLP_wxGraphicPanel::wxGP_UpdateCursor(const int x_position)
{
double x_value;
if(initialized == 1234) {
// Conversion from frame number to wavelength value:
  x_value = (x_position - 1) * wavel_step1 + wavel_start1;
  Drawing_wxgdev->UpdateCursor(x_value);
  }
}
#ifdef ZZZ
/***************************************************************************
* PlotToDrawingDisplay (for curves) 
* (called by external routines)
***************************************************************************/
void JLP_wxGraphicPanel::wxGP_PlotToDrawingDisplay() 
{

if(initialized == 1234) Drawing_wxgdev->PlotToDrawingDisplay();

}
/***************************************************************************
* LoadPlotLabel
* (called by external routines)
*
* INPUT: 
*  xstart, ystart: user coordinates of the label to be plotted
***************************************************************************/
int JLP_wxGraphicPanel::wxGP_PlotLabel1(const char *str, double xstart, 
                                        double ystart, double angle,
                                        double expand, int draw_it)
{
int status = -1, backup_to_file = 0;

if(initialized == 1234) {
  status = Drawing_wxgdev->gdev_label1(str, xstart, ystart, angle,
                                       expand, draw_it, backup_to_file);
  }

return(status);
}
/***************************************************************************
* LoadPlotMgoLabel
* (called by external routines)
*
* INPUT: 
*  ixstart, iystart: mgo coordinates of the label to be plotted
***************************************************************************/
int JLP_wxGraphicPanel::wxGP_PlotMgoLabel(const char *str, int ixstart, 
                                          int iystart, double angle,
                                          double expand, int draw_it)
{
int status = -1, backup_to_file = 0;

if(initialized == 1234) {
  status = Drawing_wxgdev->gdev_label(str, ixstart, iystart, angle,
                                      expand, draw_it, backup_to_file);
  }

return(status);
}
/***************************************************************************
* LoadPlotMgoLine 
* (called by external routines)
*
* INPUT: 
*  xplot1, yplot1, xplot2, yplot2: mgo coordinates of the points to be plotted
***************************************************************************/
int JLP_wxGraphicPanel::wxGP_PlotMgoLine(int xplot1, int yplot1, 
                                         int xplot2, int yplot2, 
                                         int lwidth, char *pcolor)                  
{
int status = -1, backup_to_file = 0;
char old_pcolor[64];

if(initialized == 1234) {
  Drawing_wxgdev->GetCurrentPColor(old_pcolor);
  Drawing_wxgdev->SetPColor(pcolor);
  status = Drawing_wxgdev->gdev_line(xplot1, yplot1, xplot2, yplot2,
                                      lwidth, backup_to_file);
  Drawing_wxgdev->SetPColor(old_pcolor);
  }

return(status);
}
/***************************************************************************
* LoadPlotLine1 
* (called by external routines)
*
* INPUT: 
*  xplot1, yplot1, xplot2, yplot2: user coordinates of the points to be plotted
***************************************************************************/
int JLP_wxGraphicPanel::wxGP_PlotLine1(double xplot1, double yplot1, 
                                       double xplot2, double yplot2, 
                                       int lwidth, char *pcolor)                  
{
int status = -1, backup_to_file = 0;
char old_pcolor[64];

if(initialized == 1234) {
  Drawing_wxgdev->GetCurrentPColor(old_pcolor);
  Drawing_wxgdev->SetPColor(pcolor);
  status = Drawing_wxgdev->gdev_line1(xplot1, yplot1, xplot2, yplot2,
                                      lwidth, backup_to_file);
  Drawing_wxgdev->SetPColor(old_pcolor);
  }

return(status);
}
#endif
