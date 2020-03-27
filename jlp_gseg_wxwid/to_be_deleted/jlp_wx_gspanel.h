/****************************************************************************
* Name: jlp_wx_gspanel.h
* 
* JLP
* Version 17/10/2016
****************************************************************************/
#ifndef _jlp_wx_gspanel_ 
#define _jlp_wx_gspanel_

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
#include "jlp_gsegraf.h"          // JLP_Gsegraf class
#include "jlp_gseg_wxwid.h"       // JLP_Gseg_Wxwid class
#include "jlp_gdev_wxwid.h"       // JLP_GDev_wxWID 

class JLP_wxVideoPanel;
class JLP_wxLogbook;

//----------------------------------------------------------------------
// class definitions
//----------------------------------------------------------------------
class JLP_wxGsegPanel: public wxPanel
{
public:

  JLP_wxGsegPanel(wxFrame *frame, const int my_wxID, 
                     JLP_wxLogbook *logbook, 
                     int x, int y, int width, int height);
  JLP_wxGsegPanel(wxFrame *frame, const int my_wxID, 
                     JLP_wxLogbook *logbook, 
                     JLP_wxVideoPanel *jlp_video_panel0, 
                     int x, int y, int width, int height);
  virtual ~JLP_wxGsegPanel();

// jlp_wx_gspanel_menu.cpp
  int SelectAndLoadInputDataFile(wxString &input_filename);
  int SelectAndLoadInputParamFile(wxString &input_filename);
  void SaveGraphicToFile();
  void SaveGraphicToPostscriptFile();
  void SaveBackupPostscriptToFile();
  void ViewChangeAxisLimits();
  void ViewAxisRotate();
  void Set_ViewLabelContours();

// Handling events:
  void OnResize(wxSizeEvent &event);
  void JLP_ResizeGraphicPanel();
  void OnZoomButton(wxCommandEvent &event);
  void OnMoveButton(wxCommandEvent &event);
  void OnShowVideoPlane(wxCommandEvent &event);

// Add Panel for plot
  void Setup_DrawingPanel(const int ID0);

// In "jlp_wx_gspanel_plot.cpp": 
  void UpdateCursor(const int x_position);
  void PlotToDrawingDisplay();
  void ClearDrawingDisplay();
  int LoadPlotData(double *xplot1, double *yplot1, const int npts1,
                   const char *nchar_type, const char *pcolor, 
                   const char *plot_fname, const int reset_first); 
  int LoadPlotSettings(const char *xlabel, const char *ylabel,
                       const char *title, const int xgrid_is_wanted,
                       const int ygrid_is_wanted,
                       const int jlp_axes_are_wanted,
                       const int iplan, const double x1,
                       const double x2, const double y1, const double y2);
  void InitPlotData(const int nmaxi1, const int ncurves_maxi1, 
                    const int nout_maxi1, const double wavel_start,
                    const double wavel_step);
  void LoadNewWavelengthCalib(double wavel_start, double wavel_step) {
   wavel_start1 = wavel_start;
   wavel_step1 = wavel_step;
   }
  void ReadWavelengthCalib(double *wavel_start, double *wavel_step) {
   *wavel_start = wavel_start1;
   *wavel_step = wavel_step1;
   }
  int PstCopyOfDisplay(char *fname) {
   int status = -1;
   if(initialized == 1234) {
     status = jlp_wxgseg_canvas1->PstCopyOfDisplay(fname);
     }
   return(status);
   }

private:
// Main parameters:
  JLP_wxLogbook *jlp_logbook;
  JLP_wxVideoPanel *jlp_video_panel;
  wxPanel *main_panel;
  int initialized;
  int Drawing_idev;
  int m_width, m_height;
  double wavel_start1, wavel_step1;

// Drawing Panel:
  JLP_GDev_wxWID *jlp_wxgseg_canvas1;

  wxStaticText *m_StaticCoord, *m_StaticHelp;
  wxBitmapButton *m_ZoomInButton, *m_ZoomOutButton;
  wxBitmapButton *m_MoveRightButton, *m_MoveLeftButton;
  wxBitmapButton *m_VideoPreviousButton, *m_VideoNextButton;
  wxBitmapButton *m_VideoPreviousFastButton, *m_VideoNextFastButton;

  DECLARE_EVENT_TABLE()
};

#endif
