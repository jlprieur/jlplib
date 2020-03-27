/****************************************************************************
* Name: jlp_wx_gpanel.h
* 
* JLP
* Version 17/01/2013
****************************************************************************/
#ifndef _jlp_wx_gpanel_ 
#define _jlp_wx_gpanel_

#include "time.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif

#include "wx/tglbtn.h"
#include "wx/statusbr.h"
#include "wx/imaglist.h"
#include "wx/cshelp.h"

#if wxUSE_TOOLTIPS
    #include "wx/tooltip.h"
#endif

#include "wx/progdlg.h"

// My files:
#include "jlp_gdev_wxwid.h"       // JLP_GDev_wxWID 

class JLP_wxVideoPanel;
class JLP_wxLogbook;

//----------------------------------------------------------------------
// class definitions
//----------------------------------------------------------------------
class JLP_wxGraphicPanel: public wxPanel
{
public:

  JLP_wxGraphicPanel(wxFrame *frame, const int my_wxID, 
                     JLP_wxLogbook *logbook, 
                     int x, int y, int width, int height);
  JLP_wxGraphicPanel(wxFrame *frame, const int my_wxID, 
                     JLP_wxLogbook *logbook, 
                     JLP_wxVideoPanel *jlp_video_panel0, 
                     int x, int y, int width, int height);
  virtual ~JLP_wxGraphicPanel();
  int wxGP_WriteToLogbook(wxString str1, bool save_to_file0);
  int wxGP_LoadDbleImage1(double *dble_image0, int nx0, int ny0);

// jlp_wx_gpanel_menu.cpp
  int wxGP_SelectAndLoadCurveFromFile(wxString &input_filename0,
                                      wxString &err_msg);
  int wxGP_SelectAndLoadFitsImage(wxString &input_filename0,
                                  wxString &err_msg);
  int wxGP_SelectAndLoadGsegParamFile(wxString &input_filename0,
                                      wxString &err_msg);
  int wxGP_SelectAndLoadGdevParamFile(wxString &input_filename0,
                                      wxString &err_msg);
  int wxGP_InitializeGsegImagePlotWithPrivateData(int gdev_graphic_type0, 
                                                 int contours_option0);
  void wxGP_SaveGraphicToFile();
  void wxGP_SaveGraphicToPostscriptFile();
  void wxGP_ViewChangeAxisLimits();
  int wxGP_ViewAxisRotate();
  int wxGP_Set_ViewLabelContours();
  int wxGP_Set_ViewDisplayCoordinates();

// Handling events:
  void OnResize(wxSizeEvent &event);
  void JLP_ResizeGraphicPanel();
  void OnZoomButton(wxCommandEvent &event);
  void OnMoveButton(wxCommandEvent &event);
  void OnShowVideoPlane(wxCommandEvent &event);

// Add Panel for plot
  void Setup_DrawingPanel(const int ID0);

// In "jlp_wx_gpanel_plot.cpp": 
  void wxGP_UpdateCursor(const int x_position);
  void wxGP_PlotToDrawingDisplay();
  void wxGP_ClearDrawingDisplay();
  int wxGP_LoadPlotDataToPrivateParameters0(double *xplot1, double *yplot1, 
                        const int npts1, 
                        const char *nchar_type, const char *pcolor, 
                        const char *plot_fname, const int reset_first); 
  int wxGP_LoadPlotDataToPrivateParameters(double *xplot1, double *yplot1, 
                        double *errorx1, double *errory1,  const int npts1,
                        const char *nchar_type, const char *pcolor, 
                        const char *plot_fname, const int reset_first); 
  int wxGP_PlotMgoLabel(const char *s, int ixstart, int iystart, double angle,
                        double expand, int draw_it);
  int wxGP_PlotLabel1(const char *s, double xstart, double ystart, double angle,
                      double expand, int draw_it);
  int wxGP_PlotMgoLine(int xplot1, int yplot1, int xplot2, 
                       int yplot2, int lwidth, char *pcolor); 
  int wxGP_PlotLine1(double xplot1, double yplot1, double xplot2, 
                     double yplot2, int lwidth, char *pcolor); 
  int wxGP_LoadPlotSettings(const char *xlabel, const char *ylabel,
                       const char *title, const int xgrid_is_wanted,
                       const int ygrid_is_wanted,
                       const int jlp_axes_are_wanted,
                       const int iplan, const double x1,
                       const double x2, const double y1, const double y2);

  void wxGP_LoadNewWavelengthCalib(double wavel_start, double wavel_step) {
   wavel_start1 = wavel_start;
   wavel_step1 = wavel_step;
   }
  void wxGP_ReadWavelengthCalib(double *wavel_start, double *wavel_step) {
   *wavel_start = wavel_start1;
   *wavel_step = wavel_step1;
   }
/***** No longer used:
  int PstCopyOfDisplay(char *fname) {
   int status = -1;
   if(initialized == 1234) {
     status = Drawing_wxgdev->PstCopyOfDisplay(fname);
     }
   return(status);
   }
********/

private:
  int initialized;

// Main parameters:
  JLP_wxLogbook *jlp_logbook;
  JLP_wxVideoPanel *jlp_video_panel;
  wxPanel *main_panel;
  int Drawing_idev;
  int m_width, m_height;
  double wavel_start1, wavel_step1;

// Drawing Panel:
  JLP_GDev_wxWID *Drawing_wxgdev;

  wxStaticText *m_StaticCoord, *m_StaticHelp;
  wxBitmapButton *m_ZoomInButton, *m_ZoomOutButton;
  wxBitmapButton *m_MoveRightButton, *m_MoveLeftButton;
  wxBitmapButton *m_VideoPreviousButton, *m_VideoNextButton;
  wxBitmapButton *m_VideoPreviousFastButton, *m_VideoNextFastButton;

  DECLARE_EVENT_TABLE()
};

#endif
