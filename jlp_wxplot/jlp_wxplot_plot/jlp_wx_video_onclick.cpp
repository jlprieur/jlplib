/******************************************************************************
* Name:        JLP_wxVideoPanel class (same file for Gdpisco and FileSpeck2)
* Purpose:     video mode: displaying a plane of a 3D FITS file
*
* Author:      JLP 
* Version:     07/04/2015
******************************************************************************/
#include "jlp_wx_video_panel_id.h"   // ID_VIDEO_TIMER
#include "jlp_wx_video_panel.h"
#include "jlp_wx_gframe.h"  // JLP_wxGraphicFrame class

/* Prototypes of routines contained here and defined in "gdp_frame.h":
void OnVideoNextFrame(wxCommandEvent& event);
void OnVideoPreviousFrame(wxCommandEvent& event);
void OnVideoGotoFrame(wxCommandEvent& event);

void OnVideoSetDelay(wxCommandEvent& WXUNUSED(event));
void OnVideoPlay(wxCommandEvent& WXUNUSED(event));
void OnVideoTimer(wxTimerEvent &WXUNUSED(event));
void OnVideoStop(wxCommandEvent& event);
*/ 

/*************************************************************************
* Display the next frame
*************************************************************************/
void JLP_wxVideoPanel::OnVideoNextFrame(wxCommandEvent& event)
{
  DisplayNextFrame(); 

return;
}
/*************************************************************************
* Display the previous frame
*************************************************************************/
void JLP_wxVideoPanel::OnVideoPreviousFrame(wxCommandEvent& event)
{
  DisplayPreviousFrame(); 

return;
}
/*************************************************************************
* Goto a given frame
*************************************************************************/
void JLP_wxVideoPanel::OnVideoGotoFrame(wxCommandEvent& event)
{
switch(event.GetId()) {
  case ID_VIDEO_GOTO_FIRST:
    DisplayFrame(1); 
    break;
  case ID_VIDEO_GOTO_MIDDLE:
    DisplayFrame(nz1/2); 
    break;
  case ID_VIDEO_GOTO_FRAME:
  default:
    DisplayFrame_NewChoice();
    break;
}

return;
}
/***********************************************************************
* Select the delay between two frames for video display (for 3D data cubes)
***********************************************************************/
void JLP_wxVideoPanel::OnVideoSetDelay(wxCommandEvent& WXUNUSED(event))
{
wxString s_values, s_question;
long delay0;

s_question = wxT("Enter delay in millisec (range 0, 10000): ");

s_values.Printf(wxT("%d"), m_video_ms_delay);

// Prompt for a new value of iframe with a dialog box:
wxString result = wxGetTextFromUser(s_question, _T("Delay between two frames"),
                                   s_values, NULL);
if (!result.IsEmpty()){
  if(result.ToLong(&delay0) == true) {
    if(delay0 > 0 && delay0 <= 10000) {
      m_video_ms_delay = delay0;
      } else {
      wxLogError(_T("Error/Bad value for delay (ms)"));
      }
  }
}

return;
}
/***********************************************************************
* Start displaying continuously (for 3D data cubes)
***********************************************************************/
void JLP_wxVideoPanel::OnVideoPlay(wxCommandEvent& WXUNUSED(event))
{

if(m_video_timer->IsRunning()) {
  wxLogError(wxT("Error: video already running!"));
  return;
  }

// Start the timer:
m_video_timer->Start(m_video_ms_delay, wxTIMER_CONTINUOUS);

// To direct timer events to this class:
m_video_timer->SetOwner(this, ID_VIDEO_TIMER);

// Update menu buttons (true -> "stop=enabled" and "play=disabled") 
EnableStopVideo(true);

return;
}
/****************************************************************
* Handle timer events, when "video play" button has been pressed 
* (called every m_video_ms_delay milliseconds )
*****************************************************************/
void JLP_wxVideoPanel::OnVideoTimer(wxTimerEvent &WXUNUSED(event))
{
int status = 0;

if(m_iframe < nz1) {
// Display next image
  m_iframe++;
  DisplayFrame(m_iframe); 
  if(status) {
    m_video_timer->Stop(); 
    fprintf(stderr, "OnVideoTimer/error loading frame #%d (%s)\n", 
            m_iframe, (const char *)full_filename1.mb_str());
// Update menu buttons (false -> "stop=disabled" and "play=enabled") 
    EnableStopVideo(false);
    }
  } else {
  m_video_timer->Stop(); 
// Update menu buttons (false -> "stop=disabled" and "play=enabled") 
  EnableStopVideo(false);
  } 

return;
}
/****************************************************************
* To stop video 
*****************************************************************/
void JLP_wxVideoPanel::OnVideoStop(wxCommandEvent& event)
{
 
if(!m_video_timer->IsRunning()) {
  wxLogError(wxT("Error: video already stopped!"));
  } else {
  m_video_timer->Stop();
// Update menu buttons (false -> "stop=disabled" and "play=enabled") 
  EnableStopVideo(false);
  }

return;
}
