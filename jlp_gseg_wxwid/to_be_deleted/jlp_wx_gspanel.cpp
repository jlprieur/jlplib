/****************************************************************************
* Name: jlp_wx_gspanel.cpp
* 
* JLP
* Version 06/11/2016
***************************************************************************/
#include <stdio.h>
// jlp_splot :
#include "jlp_gdev_idv.h"
#include "jlp_splot.h"
#include "jlp_plotlib.h"
// jlp_wxplot :
#include "jlp_gdev_wxwid.h"
#include "jlp_wxlogbook.h"         // JLP_wxLogbook
#include "jlp_wx_video_panel.h"    // JLP_wxVideoPanel

// jlp_gseg_wxwid
#include "jlp_wx_gspanel.h"
// ZZ  #include "jlp_bitmaps.h"           // JLP bitmaps 

// mBookCtrl
enum{
  ID_DRAW_CGDEV = 2850,
  ID_DRAW_ZOOM_IN,
  ID_DRAW_ZOOM_OUT,
  ID_DRAW_MOVE_RIGHT,
  ID_DRAW_MOVE_LEFT,
  ID_DRAW_VIDEO_PREVIOUS,
  ID_DRAW_VIDEO_NEXT,
  ID_DRAW_VIDEO_PREVIOUS_FAST,
  ID_DRAW_VIDEO_NEXT_FAST
};

BEGIN_EVENT_TABLE(JLP_wxGsegPanel, wxPanel)
EVT_BUTTON  (ID_DRAW_ZOOM_IN, JLP_wxGsegPanel::OnZoomButton)
EVT_BUTTON  (ID_DRAW_ZOOM_OUT, JLP_wxGsegPanel::OnZoomButton)
EVT_BUTTON  (ID_DRAW_MOVE_LEFT, JLP_wxGsegPanel::OnMoveButton)
EVT_BUTTON  (ID_DRAW_MOVE_RIGHT, JLP_wxGsegPanel::OnMoveButton)
EVT_BUTTON  (ID_DRAW_VIDEO_PREVIOUS, JLP_wxGsegPanel::OnShowVideoPlane)
EVT_BUTTON  (ID_DRAW_VIDEO_NEXT, JLP_wxGsegPanel::OnShowVideoPlane)
EVT_BUTTON  (ID_DRAW_VIDEO_PREVIOUS_FAST, JLP_wxGsegPanel::OnShowVideoPlane)
EVT_BUTTON  (ID_DRAW_VIDEO_NEXT_FAST, JLP_wxGsegPanel::OnShowVideoPlane)
// catch size events
EVT_SIZE (JLP_wxGsegPanel::OnResize)
END_EVENT_TABLE()


// ============================================================================
// implementation
// ============================================================================


/*******************************************************************************
* Constructor as a subwindow from wxFrame:
*******************************************************************************/
JLP_wxGsegPanel::JLP_wxGsegPanel( wxFrame *frame, const int my_wxID,
                                        JLP_wxLogbook *logbook,
                                        JLP_wxVideoPanel *jlp_video_panel0, 
                                        int x, int y, int iwidth, int iheight)
                                        : wxPanel( frame, my_wxID, 
                                        wxPoint(x, y), wxSize(iwidth, iheight))
{
wxString input_filename;

// Transform coma into point for numbers:
setlocale(LC_NUMERIC, "C");

initialized = 0;
m_width = iwidth;
m_height = iheight;
wavel_start1 = 0.;
wavel_step1 = 1.;

jlp_logbook = logbook;
jlp_video_panel = jlp_video_panel0;

// Setup drawing panel
Setup_DrawingPanel(my_wxID);

initialized = 1234;

return;
}
/*******************************************************************************
* Constructor as a subwindow from wxFrame:
*******************************************************************************/
JLP_wxGsegPanel::JLP_wxGsegPanel( wxFrame *frame, const int my_wxID,
                                        JLP_wxLogbook *logbook,
                                        int x, int y, int iwidth, int iheight)
                                        : wxPanel( frame, my_wxID, 
                                        wxPoint(x, y), wxSize(iwidth, iheight))
{
wxString input_filename;

// Transform coma into point for numbers:
setlocale(LC_NUMERIC, "C");

initialized = 0;
m_width = iwidth;
m_height = iheight;
wavel_start1 = 0.;
wavel_step1 = 1.;

jlp_logbook = logbook;
jlp_video_panel = NULL;

// Setup drawing panel
Setup_DrawingPanel(my_wxID);

initialized = 1234;

return;
}
//*********************************************************************
// Setup drawing panel
//*********************************************************************
void JLP_wxGsegPanel::Setup_DrawingPanel(const int ID0)
{
int status;
wxString buffer;

 wxBoxSizer *topsizer = new wxBoxSizer( wxVERTICAL );
 
 m_StaticHelp = new wxStaticText(this, -1, 
                    wxT("Processing mode = 0 : click and drag to select box"));
 m_StaticCoord = new wxStaticText(this, -1, 
                                  wxT("x=12.3456 y=12.3456"), wxPoint(-1,-1), 
                                  wxSize(140,20));

// New JLP_wxGseg_Canvas object (for display on the screen)
// Start at wxpoint(0,0) from top left corner:
 jlp_wxgseg_canvas1 = new JLP_wxGseg_Canvas(this, ID0, wxPoint(0, 0), 
                                     wxSize(m_width, m_height - 60),
                                     m_StaticCoord, m_StaticHelp);
 topsizer->Add(jlp_wxgseg_canvas1, 1, wxEXPAND);

// Get idev number:
 status = jlp_wxgseg_canvas1->Get_idev(Drawing_idev);

 if(status) {
   fprintf(stderr, "Setup_DrawingPanel/Fatal error: Drawing_idev = %d\n",
           Drawing_idev);
   buffer.Printf(wxT("Setup_DrawingPanel/Fatal error: Drawing_idev = %d"),
           Drawing_idev);
   wxMessageBox(buffer, wxT("JLP_wxGsegPanel"), wxOK | wxICON_ERROR);
   exit(-1);
 }

// Create two buttons that are horizontally unstretchable,
// with an all-around border with a width of 10 and implicit top alignment
 wxBoxSizer *button_sizer = new wxBoxSizer( wxHORIZONTAL );

// Bitmap colors: 1=yellow 2=red 3= blue
 m_ZoomInButton = new wxBitmapButton(this, ID_DRAW_ZOOM_IN,
                                     wxBITMAP(zoom_plus3));
 button_sizer->Add( m_ZoomInButton, 0);

 m_ZoomOutButton = new wxBitmapButton(this, ID_DRAW_ZOOM_OUT,
                                      wxBITMAP(zoom_minus3));
 button_sizer->Add( m_ZoomOutButton, 0, wxLEFT, 10);

 m_MoveRightButton = new wxBitmapButton(this, ID_DRAW_MOVE_RIGHT,
                                       wxBITMAP(move_right3));
 button_sizer->Add( m_MoveRightButton, 0, wxLEFT, 10);

 m_MoveLeftButton = new wxBitmapButton(this, ID_DRAW_MOVE_LEFT,
                                       wxBITMAP(move_left3));
 button_sizer->Add( m_MoveLeftButton, 0, wxLEFT, 10);

// Yellow buttons (like for "jlp_wx_video_panel.cpp")
 if(jlp_video_panel != NULL) {
   m_VideoNextFastButton = new wxBitmapButton(this, ID_DRAW_VIDEO_NEXT_FAST,
                                           wxBITMAP(move_right_fast1));
   button_sizer->Add( m_VideoNextFastButton, 0, wxLEFT, 10);
   m_VideoNextButton = new wxBitmapButton(this, ID_DRAW_VIDEO_NEXT,
                                           wxBITMAP(move_right1));
   button_sizer->Add( m_VideoNextButton, 0, wxLEFT, 10);
   m_VideoPreviousButton = new wxBitmapButton(this, ID_DRAW_VIDEO_PREVIOUS,
                                              wxBITMAP(move_left1));
   button_sizer->Add( m_VideoPreviousButton, 0, wxLEFT, 10);
   m_VideoPreviousFastButton = new wxBitmapButton(this, ID_DRAW_VIDEO_PREVIOUS_FAST,
                                              wxBITMAP(move_left_fast1));
   button_sizer->Add( m_VideoPreviousFastButton, 0, wxLEFT, 10);
   }

// Static text with the coordinates
 button_sizer->Add( m_StaticCoord, 0, wxLEFT | wxALIGN_CENTER_VERTICAL, 10);

// Help static tex on the next line:
 button_sizer->Add( m_StaticHelp, 0, wxLEFT | wxALIGN_CENTER_VERTICAL, 20);

// Create a sizer with no border and centered horizontally
 topsizer->Add(button_sizer, 0, wxALIGN_CENTER | wxALL, 10);

// To set the minimum size that fits this setup:
 SetSizerAndFit(topsizer);      // use the sizer for layout

return;
}
/************************************************************************
* Destructor 
*************************************************************************/
JLP_wxGsegPanel::~JLP_wxGsegPanel()
{
// Free Memory:
delete jlp_wxgseg_canvas1;
Close();
}
/************************************************************************
* To enlarge the window at the maximum size: 
* JLP 2015: resize events should be handled here 
* (attempts to make it at the jlp_cgdev class level have failed !)
* (=> It should be done using the instanciation not the class !!)
************************************************************************/
void JLP_wxGsegPanel::OnResize(wxSizeEvent &event )
{
 JLP_ResizeGraphicPanel();

// Skip this event (to avoid infinite processing of the same event):
 event.Skip();

return;
}
/************************************************************************
* To enlarge the window at the maximum size: 
* JLP 2015: resize events should be handled here 
* (attempts to make it at the jlp_cgdev class level have failed !)
* (=> It should be done using the instanciation not the class !!)
*
* Routine defined here outside of resize events to be called manually
************************************************************************/
void JLP_wxGsegPanel::JLP_ResizeGraphicPanel()
{
int width0, height0;

if(initialized != 1234) return;

// Both GetSize() or GetClientSize() give a bad size (too large)
GetClientSize(&width0, &height0);
/*
printf("JLP_wxGsegPanel::GetClientSize/width,height = %d,%d\n", 
        width0, height0);
*/

// Step 1: resize the drawing panel with the large size and update the drawings
 jlp_wxgseg_canvas1->SetNewBitmapSize(width0, height0);

jlp_wxgseg_canvas1->Get_ClientSize(&width0, &height0);
/*
printf("JLP_wxGsegPanel/From Drawing_cgdex::GetClientSize/width,height = %d,%d\n", 
        width0, height0);
*/

// Step 2: resize the drawing panel with the smaller size and update the drawings
 jlp_wxgseg_canvas1->SetNewBitmapSize(width0, height0);
// JLP2016: it is necessary to refresh the drawing window to make it fill 
// the whole allocated panel space: 
 jlp_wxgseg_canvas1->Refresh();

return;
}
/*********************************************************************
* ZoomIn/ZoomOut button
*********************************************************************/
void JLP_wxGsegPanel::OnZoomButton(wxCommandEvent &event)
{
bool zoom_in;

if(initialized != 1234) return;

switch(event.GetId()) {
 default:
 case ID_DRAW_ZOOM_IN: 
   zoom_in = true;
   break;
 case ID_DRAW_ZOOM_OUT: 
   zoom_in = false;
   break;
 }

jlp_wxgseg_canvas1->BoxLimitsZoom(zoom_in);
jlp_wxgseg_canvas1->RedrawToBackupDC(8001);

// Update cursor:
if(jlp_video_panel != NULL) jlp_video_panel->UpdateCursorInSpectrumPanel();

return;
}
/*********************************************************************
* Move right/left button
*********************************************************************/
void JLP_wxGsegPanel::OnMoveButton(wxCommandEvent &event)
{
bool move_to_right;

if(initialized != 1234) return;

switch(event.GetId()) {
 default:
 case ID_DRAW_MOVE_RIGHT: 
   move_to_right = true;
   break;
 case ID_DRAW_MOVE_LEFT: 
   move_to_right = false;
   break;
 }

jlp_wxgseg_canvas1->BoxLimitsMove(move_to_right);
jlp_wxgseg_canvas1->RedrawToBackupDC(8002);

// Update cursor:
if(jlp_video_panel != NULL) jlp_video_panel->UpdateCursorInSpectrumPanel();

return;
}
/*********************************************************************
* Show Video Plane button
*********************************************************************/
void JLP_wxGsegPanel::OnShowVideoPlane(wxCommandEvent &event)
{

if((initialized != 1234) || (jlp_video_panel == NULL)) return;

switch(event.GetId()) {
 default:
 case ID_DRAW_VIDEO_PREVIOUS: 
   jlp_video_panel->DisplayPreviousFrame();
   break;
 case ID_DRAW_VIDEO_PREVIOUS_FAST: 
   jlp_video_panel->DisplayFastPreviousFrame();
   break;
 case ID_DRAW_VIDEO_NEXT: 
   jlp_video_panel->DisplayNextFrame();
   break;
 case ID_DRAW_VIDEO_NEXT_FAST: 
   jlp_video_panel->DisplayFastNextFrame();
   break;
 }

return;
}
