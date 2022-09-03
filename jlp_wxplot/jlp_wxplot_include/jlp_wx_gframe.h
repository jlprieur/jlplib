/******************************************************************************
* jlp_wx_gframe.h
*
* To plot graphics on a separate popup frame 
*
* Author:   JLP 
* Version:  26/02/2016
******************************************************************************/
#ifndef jlp_wx_gframe_h    // sentry 
#define jlp_wx_gframe_h

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif

#include "wx/file.h"
#include "wx/textfile.h"
#include "wx/filename.h"
#include "wx/mstream.h"
#include "wx/wfstream.h"
#include "wx/quantize.h"

#include "jlp_wx_gpanel.h"    // JLP_wxGraphicPanel class
#include "jlp_wx_video_panel.h"    // JLP_wxVideoPanel class

/********************************************************************
* Class JLP_wxGraphicFrame
*********************************************************************/

class JLP_wxGraphicFrame: public wxFrame
{
public:

// Constructor:
  JLP_wxGraphicFrame(JLP_wxVideoPanel *jlp_video_panel0,
                     const wxString title0, const wxString xlabel0,
                     const wxString ylabel0,
                     const int width0, const int height0);
  JLP_wxGraphicFrame(const wxString title0, const wxString xlabel0,
                     const wxString ylabel0,
                     const int width0, const int height0);

// Destructor: 
  ~JLP_wxGraphicFrame(){MyFreeMemory();};

  void PanelSetup();
  int LoadNewPlot(double *xplot0, double *yplot0, double *errorx0,
                  double *errory0, int nplot0, const int reset_first); 
  void MyFreeMemory();

// Interface with JLP_wxGraphicPanel:
  int wxGF_LoadPlotDataToPrivateParameters0(double *xplot0, double *yplot0, 
                   const int npts0, const char *nchar_type, const char *pcolor, 
                   const char *plot_fname, const int reset_first);
  int wxGF_LoadPlotDataToPrivateParameters(double *xplot0, double *yplot0, 
                   double *errorx0, double *errory0, const int npts0, 
                   const char *nchar_type, const char *pcolor, 
                   const char *plot_fname, const int reset_first);
  int wxGF_LoadPlotSettings(const char *xlabel, const char *ylabel,
                       const char *title, const char *pen_colour, 
                       const char *pen_default_colour,
                       const char *backgd_colour,
                       const int xgrid_is_wanted, const int ygrid_is_wanted,
                       const int jlp_axes_are_wanted, const int iplan,
                       const double x1, const double x2,
                       const double y1, const double y2);
  void wxGF_ClearDrawingDisplay();
  void wxGF_PlotToDrawingDisplay();

  void LoadNewWavelengthCalib(double wavel_start, double wavel_step) {
   if((initialized == 1234) && (m_GraphicPanel != NULL)) {
     m_GraphicPanel->wxGP_LoadNewWavelengthCalib(wavel_start, wavel_step);
     }
   }


private:
  int initialized;
  double wavel_start1, wavel_step1;
  wxString title1, xlabel1, ylabel1;
  JLP_wxGraphicPanel *m_GraphicPanel;
  JLP_wxVideoPanel *m_VideoPanel;

  DECLARE_EVENT_TABLE()
};

#endif               // EOF sentry
