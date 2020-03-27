/****************************************************************************
* Name: jlp_wx_video_panel.cpp
*
* JLP
* Version 12/04/2016
****************************************************************************/
#include "jlp_wx_video_panel_id.h"
#include "jlp_wx_video_panel.h"
#include "jlp_wx_gpanel.h"        // JLP_wxGraphicPanel
#include "jlp_wx_ipanel.h"          // JLP_wxImagePanel class
#include "jlp_bitmaps.h"           // JLP bitmaps

BEGIN_EVENT_TABLE(JLP_wxVideoPanel, wxPanel)
  EVT_TIMER(ID_VIDEO_TIMER, JLP_wxVideoPanel::OnVideoTimer)

// Video
EVT_BUTTON(ID_VIDEO_NEXT_FRAME, JLP_wxVideoPanel::OnVideoNextFrame)
EVT_BUTTON(ID_VIDEO_PREVIOUS_FRAME, JLP_wxVideoPanel::OnVideoPreviousFrame)
EVT_BUTTON(ID_VIDEO_GOTO_FIRST, JLP_wxVideoPanel::OnVideoGotoFrame)
EVT_BUTTON(ID_VIDEO_GOTO_MIDDLE, JLP_wxVideoPanel::OnVideoGotoFrame)
EVT_BUTTON(ID_VIDEO_GOTO_FRAME, JLP_wxVideoPanel::OnVideoGotoFrame)
EVT_BUTTON(ID_VIDEO_DELAY, JLP_wxVideoPanel::OnVideoSetDelay)
EVT_BUTTON(ID_VIDEO_PLAY, JLP_wxVideoPanel::OnVideoPlay)
EVT_BUTTON(ID_VIDEO_STOP, JLP_wxVideoPanel::OnVideoStop)

END_EVENT_TABLE()

/******************************************************************************
* Constructor from JLP_wxVideoPanel and wxFrame:
******************************************************************************/
JLP_wxVideoPanel::JLP_wxVideoPanel(wxFrame *panel_frame, 
                                   const int width0, const int height0)
                                   : wxPanel( panel_frame )
{
 initialized = 0;

// JLP_wxImagePanel();
 m_ImagePanel = NULL;
 m_image_gdev = NULL;
 spectrum_GraphicPanel = NULL;

 m_iframe = 0;
 nz1 = 0;
 m_ihdu = 1;
 full_filename1 = wxT("");

 DisplayPanel_Setup(width0, height0);

// Timer for the video:
 m_video_timer = new wxTimer(this, ID_VIDEO_TIMER);

// Set delay to obtain a rate of 10 images/second:
 m_video_ms_delay = 100;

 initialized = 1234;

}
/********************************************************************
* Connect spectrum graphic panel to this video panel
* to allow cursor update in graphic panel 
********************************************************************/
void JLP_wxVideoPanel::ConnectSpectrumToVideoPanel(JLP_wxGraphicPanel *spectrum_panel)
{
 spectrum_GraphicPanel = spectrum_panel;
}
/********************************************************************
* Shutdown
********************************************************************/
void JLP_wxVideoPanel::MyFreeMemory()
{  
return;
}
/**********************************************************************
* DisplayPanel setup 
* Create DisplayPanel with image panel
*
***********************************************************************/
void JLP_wxVideoPanel::DisplayPanel_Setup(const int width0, const int height0)
{
wxBoxSizer *vbox_sizer, *hbox_sizer1, *vbox_sizer1;
int width1, height1;
int fits_in_window = 1;

 width1 = width0 - 40;
 height1 = height0 - 40;

 vbox_sizer = new wxBoxSizer( wxVERTICAL );
 hbox_sizer1 = new wxBoxSizer( wxHORIZONTAL );
 vbox_sizer1 = new wxBoxSizer( wxVERTICAL );

 m_StaticFrameNber = new wxStaticText(this, -1,
                                 wxT("2340"), wxPoint(-1,-1),
                                 wxSize(80,20));
 m_StaticCoord = new wxStaticText(this, -1,
                                 wxT("x=12.3 y=87.4 54.34"), wxPoint(-1,-1),
                                 wxSize(140,20));
 m_StaticHelp = new wxStaticText(this, -1,
                    wxT("Processing mode = 0 : click and drag to select box"));

// Scrollwindow for image display:
  m_ImagePanel = new JLP_wxImagePanel((wxFrame *)this, m_StaticCoord,
                                        m_StaticHelp, 20, 20, width1, height1,
                                       fits_in_window);
  m_image_gdev = m_ImagePanel->Image_gdev();

// 1: vertically strechable
// wxEXPAND | xALL: horizontally strechable with borders all around
  vbox_sizer1->Add(m_ImagePanel, 1, wxEXPAND | wxALL);
  hbox_sizer1->Add(vbox_sizer1, 1, wxEXPAND);

// 1 : vertically strechable
// wxEXPAND : horizontally strechable 
  vbox_sizer->Add(hbox_sizer1, 1, wxEXPAND);

// Create a few buttons that are horizontally unstretchable,
// with an all-around border with a width of 10 and implicit top alignment
 wxBoxSizer *button_sizer = new wxBoxSizer( wxHORIZONTAL );

// Bitmap colors: 1=yellow 2=red 3= blue

// Goto next frame:
 m_NextFrameButton = new wxBitmapButton(this, ID_VIDEO_NEXT_FRAME,
                                       wxBITMAP(move_right1));
 button_sizer->Add( m_NextFrameButton, 0, wxLEFT, 10);

// Goto middle frame:
 m_GotoMiddleButton = new wxBitmapButton(this, ID_VIDEO_GOTO_MIDDLE,
                                       wxBITMAP(move_left_right1));
 button_sizer->Add( m_GotoMiddleButton, 0, wxLEFT, 10);

// Goto frame:
 m_GotoFrameButton = new wxBitmapButton(this, ID_VIDEO_GOTO_FRAME,
                                       wxBITMAP(move_right_prompt1));
 button_sizer->Add( m_GotoFrameButton, 0, wxLEFT, 10);

// Goto previous frame:
 m_PreviousFrameButton = new wxBitmapButton(this, ID_VIDEO_PREVIOUS_FRAME,
                                       wxBITMAP(move_left1));
 button_sizer->Add( m_PreviousFrameButton, 0, wxLEFT, 10);

// Goto first frame:
 m_GotoFirstButton = new wxBitmapButton(this, ID_VIDEO_GOTO_FIRST,
                                       wxBITMAP(move_left_start1));
 button_sizer->Add( m_GotoFirstButton, 0, wxLEFT, 10);


// Set delay 
 m_SetDelayButton = new wxBitmapButton(this, ID_VIDEO_DELAY,
                                       wxBITMAP(clock3));
 button_sizer->Add( m_SetDelayButton, 0, wxLEFT, 10);

// Play continuously:
 m_PlayButton = new wxBitmapButton(this, ID_VIDEO_PLAY,
                                       wxBITMAP(play_start3));
 button_sizer->Add( m_PlayButton, 0, wxLEFT, 10);

// Stop:
 m_StopButton = new wxBitmapButton(this, ID_VIDEO_STOP,
                                       wxBITMAP(play_stop3));
 button_sizer->Add( m_StopButton, 0, wxLEFT, 10);

// Static text with the coordinates
 button_sizer->Add( m_StaticFrameNber, 0, wxLEFT | wxALIGN_CENTER_VERTICAL, 10);

// Static text with the coordinates
 button_sizer->Add( m_StaticCoord, 0, wxLEFT | wxALIGN_CENTER_VERTICAL, 10);

// Help static tex on the next line:
 button_sizer->Add( m_StaticHelp, 0, wxLEFT | wxALIGN_CENTER_VERTICAL, 20);

// Create a sizer with no border and centered horizontally
 vbox_sizer->Add(button_sizer, 0, wxALIGN_CENTER | wxALL, 10);

// To set the minimum size that fits this setup:
 SetSizerAndFit(vbox_sizer);      // use the sizer for layout

return;
}
// ***************** Video menu ******************************
/*
  menuVideo->Append( ID_VIDEO_NEXT_FRAME, _T("Next frame"),
                       wxT("Display next plane in 3D data cube"));
  menuVideo->Append( ID_VIDEO_PREVIOUS_FRAME, _T("Previous frame"),
                       wxT("Display previous plane in 3D data cube"));
  menuVideo->Append( ID_VIDEO_GOTO_FIRST, _T("Go to first frame"),
                       wxT("Go to the first image"));
  menuVideo->Append( ID_VIDEO_GOTO_ZERO, _T("Go to last frame"),
                       wxT("Go to the last image"));
  menuVideo->Append( ID_VIDEO_GOTO_FRAME, _T("Go to frame #"),
                       wxT("Go to a given plane in 3D data cube"));
  menuVideo->Append( ID_VIDEO_DELAY, _T("Set video delay"),
                       wxT("Define the delay between two frames"));
  menuVideo->Append( ID_VIDEO_PLAY, _T("Play"),
                       wxT("Start video playing"));
  menuVideo->Append( ID_VIDEO_STOP, _T("Stop"),
                       wxT("Stop video playing"));
  menu_bar->Append(menuVideo, _T("Video"));
*/
/*************************************************************************
* Goto a given frame
*************************************************************************/
void JLP_wxVideoPanel::DisplayFrame_NewChoice()
{
wxString s_values, s_question;
long iframe;

s_question.Printf(wxT("Enter frame number (from 1 to %d):"), nz1);

s_values.Printf(wxT("%d"), m_iframe);

// Prompt for a new value of iframe with a dialog box:
wxString result = wxGetTextFromUser(s_question, _T("Load new image plane"),
                                   s_values, NULL);
if(!result.IsEmpty()){
  if(result.ToLong(&iframe) == true) {
    if(iframe > 0 && iframe <= nz1) {
      DisplayFrame(iframe);
      } else {
      wxLogError(_T("Error/Bad value for iframe"));
      }
  }
}

return;
}
/******************************************************************
* Update menu Stop and Play buttons
*  if true: ("stop=enabled" and "play=disabled")
*  if false: ("stop=disabled" and "play=enabled")
*
*******************************************************************/
void JLP_wxVideoPanel::EnableStopVideo(bool flag)
{
/****
if(menuVideo == NULL) return;

 if(menuVideo->FindItem(ID_VIDEO_STOP) != NULL)
      menuVideo->Enable(ID_VIDEO_STOP, flag);

 if(menuVideo->FindItem(ID_VIDEO_PLAY) != NULL)
       menuVideo->Enable(ID_VIDEO_PLAY, !flag);
***/
return;
}
/******************************************************************
* Load and display a single plane of a 3D FITS cube
*
* INPUT:
* change_filename: true if a new filename is required
*                  false if the old filename is to be used (in case of 3D data)
* iframe: index of image plane to be loaded from 1 to nz (in case of 3D data)
* ihdu: HDU number in FITS file
*******************************************************************/
int JLP_wxVideoPanel::LoadFITSImage(const int iframe)
{
double *dble_image0;
wxString str1;
int status;

 m_iframe = iframe;
 status = ReadFITSImage_1(iframe, &dble_image0);

// Display Image:
 if(status == 0) {
   status = m_ImagePanel->wxIP_LoadImage(dble_image0, nx1, ny1);
  }

// Free memory:
delete[] dble_image0;
return(status);
}
/*************************************************************************
* Display the next frame
*************************************************************************/
void JLP_wxVideoPanel::DisplayNextFrame()
{
// m_iframe from 1 to nz1:
if(m_iframe < nz1) {
  m_iframe++;
  DisplayFrame(m_iframe);
  } 
return;
}
/*************************************************************************
* Display the istep th frame after the current value
*************************************************************************/
void JLP_wxVideoPanel::DisplayFastNextFrame()
{
int istep;
istep = MAXI(2, nz1 / 20);

// m_iframe from 1 to nz1:
if(m_iframe < nz1 - istep) {
  m_iframe += istep;
  DisplayFrame(m_iframe);
  } else {
  m_iframe = nz1;
  DisplayFrame(m_iframe);
  } 
return;
}
/*************************************************************************
* Display the previous frame
*************************************************************************/
void JLP_wxVideoPanel::DisplayPreviousFrame()
{
if(m_iframe > 0) {
  m_iframe--;
  DisplayFrame(m_iframe);
  } 
return;
}
/*************************************************************************
* Display the istep th frame before the current value
*************************************************************************/
void JLP_wxVideoPanel::DisplayFastPreviousFrame()
{
int istep;
istep = MAXI(2, nz1 / 20);
if(m_iframe > istep) {
  m_iframe -= istep;
  DisplayFrame(m_iframe);
  } else {
  m_iframe = 1;
  DisplayFrame(m_iframe);
  } 
return;
}
/*************************************************************************
* Display a given frame
*************************************************************************/
void JLP_wxVideoPanel::DisplayFrame(const int iframe)
{
wxString buffer;

if((iframe >= 1) && (iframe <= nz1)) {
 m_iframe = iframe;
 buffer.Printf(wxT("#%d/%d"), m_iframe, nz1);
 m_StaticFrameNber->SetLabel(buffer);
 LoadFITSImage(m_iframe);
// Update cursor in spectrum graphic panel
 UpdateCursorInSpectrumPanel();
 }

return;
}
/*************************************************************************
* Update cursor in spectrum GraphicPanel 
*************************************************************************/
void JLP_wxVideoPanel::UpdateCursorInSpectrumPanel()
{
double x_pos;

 if((m_iframe < 1) || (m_iframe > nz1)) return; 

// x_pos in user coordinates (I assume here that X axis is the frame number)
 x_pos = (double)m_iframe;
 if(spectrum_GraphicPanel != NULL) spectrum_GraphicPanel->wxGP_UpdateCursor(x_pos);

return;
}
/*************************************************************************
* Display a double array 
*************************************************************************/
int JLP_wxVideoPanel::LoadImage(double *dble_image0, int nx0, int ny0)
{
int status;
 status = m_ImagePanel->wxIP_LoadImage(dble_image0, nx0, ny0);

return(status);
}
