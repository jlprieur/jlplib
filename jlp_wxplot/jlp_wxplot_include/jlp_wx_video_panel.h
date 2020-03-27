/****************************************************************************
* Name: jlp_wx_video_panel.h
* 
* JLP
* Version 08/03/2016
****************************************************************************/
#ifndef _jlp_wx_video_panel__ 
#define _jlp_wx_video_panel__ 

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif

#include "wx/tglbtn.h"
#include "wx/bookctrl.h"
#include "wx/imaglist.h"
#include "wx/cshelp.h"

#if wxUSE_TOOLTIPS
    #include "wx/tooltip.h"
#endif

class JLP_wxGraphicPanel; 
class JLP_wxGraphicFrame; 
class JLP_wxImagePanel; 
class JLP_GDev_wxWID; 
class JLP_wxGraphicPanel;

//----------------------------------------------------------------------
// class definitions
//----------------------------------------------------------------------

class JLP_wxVideoPanel: public wxPanel
{
public:
// In "jlp_wx_video_panel.cpp":

 JLP_wxVideoPanel(wxFrame *panel_frame,
                   const int width0, const int height0);
 ~JLP_wxVideoPanel(){MyFreeMemory();};

 void MyFreeMemory();
 void DisplayPanel_Setup(int width0, int height0);
 int LoadFITSImage(const int iframe);
 void DisplayFrame_NewChoice();
 void DisplayNextFrame();
 void DisplayPreviousFrame();
 void DisplayFastNextFrame();
 void DisplayFastPreviousFrame();
 void DisplayFrame(const int iframe);
 void UpdateCursorInSpectrumPanel();
 int LoadImage(double *dble_image0, int nx0, int ny0);
 void ConnectSpectrumToVideoPanel(JLP_wxGraphicPanel *spectrum_panel);

// In "jlp_wx_video_onclick.cpp":
 void OnVideoNextFrame(wxCommandEvent& event);
 void OnVideoPreviousFrame(wxCommandEvent& event);
 void OnVideoGotoFrame(wxCommandEvent& event);
 void OnVideoSetDelay(wxCommandEvent& event);
 void OnVideoPlay(wxCommandEvent& event);
 void OnVideoStop(wxCommandEvent& event);
 void OnVideoTimer(wxTimerEvent& event);
 void EnableStopVideo(bool flag);

// In "jlp_wx_video_rw_fits.cpp":
 int LoadNewFITSCube(wxString full_filename0, const int ihdu0,
                     const int nx0, const int ny0, const int nz0);
 int ReadFITSImage_1(const int iframe, double **dble_image0);

private:
  int nx1, ny1, nz1, initialized;

  wxString full_filename1;
 
  JLP_wxImagePanel *m_ImagePanel;
  JLP_GDev_wxWID *m_image_gdev;
  JLP_wxGraphicPanel *spectrum_GraphicPanel;
  wxCheckBox *ImagePanelCheckBox;

  int m_iframe, m_ihdu;

  wxTimer *m_video_timer;
  int m_video_ms_delay;

  wxStaticText *m_StaticFrameNber, *m_StaticCoord, *m_StaticHelp;
  wxBitmapButton *m_NextFrameButton, *m_PreviousFrameButton;
  wxBitmapButton *m_PlayButton, *m_StopButton, *m_GotoFrameButton;
  wxBitmapButton *m_SetDelayButton, *m_GotoFirstButton, *m_GotoMiddleButton;

  DECLARE_EVENT_TABLE()
};

// In "spea_display_rw_fits.cpp" :
int OpenNewFITSImage_1(wxString &full_filename0, const int ihdu,
                       int *nx0, int *ny0, int *nz0);

#endif
