/****************************************************************************
* Name: jlp_wx_gpanel.cpp
* to display curves/images with jlp_splot software
* (possibility of zoom/unzoom and left/right motion on a single curve
*  and left/right motion in a 3D FITS cube frame) 
* NB: may assume a link with a JLP_wxVideoPanel
* 
* JLP
* Version 06/05/2013
***************************************************************************/
#include <stdio.h>
// wxplot files:
#include "jlp_gdev_idv.h"
#include "jlp_gdev_wxwid.h"
// jlp_wx_gpanel files:
#include "jlp_wx_gpanel.h"
#include "jlp_wxlogbook.h"         // JLP_wxLogbook
#include "jlp_wx_video_panel.h"    // JLP_wxVideoPanel
#include "jlp_bitmaps.h"           // JLP bitmaps 

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
  ID_DRAW_VIDEO_NEXT_FAST,
  ID_DRAW_SAVE_AS
};

BEGIN_EVENT_TABLE(JLP_wxGraphicPanel, wxPanel)
EVT_BUTTON  (ID_DRAW_ZOOM_IN, JLP_wxGraphicPanel::OnZoomButton)
EVT_BUTTON  (ID_DRAW_ZOOM_OUT, JLP_wxGraphicPanel::OnZoomButton)
EVT_BUTTON  (ID_DRAW_MOVE_LEFT, JLP_wxGraphicPanel::OnMoveButton)
EVT_BUTTON  (ID_DRAW_MOVE_RIGHT, JLP_wxGraphicPanel::OnMoveButton)
EVT_BUTTON  (ID_DRAW_VIDEO_PREVIOUS, JLP_wxGraphicPanel::OnShowVideoPlane)
EVT_BUTTON  (ID_DRAW_VIDEO_NEXT, JLP_wxGraphicPanel::OnShowVideoPlane)
EVT_BUTTON  (ID_DRAW_VIDEO_PREVIOUS_FAST, JLP_wxGraphicPanel::OnShowVideoPlane)
EVT_BUTTON  (ID_DRAW_VIDEO_NEXT_FAST, JLP_wxGraphicPanel::OnShowVideoPlane)
EVT_BUTTON  (ID_DRAW_SAVE_AS, JLP_wxGraphicPanel::OnSaveAsButton)
// catch size events
EVT_SIZE (JLP_wxGraphicPanel::OnResize)
END_EVENT_TABLE()


// ============================================================================
// implementation
// ============================================================================


/*******************************************************************************
* Constructor as a subwindow from wxFrame:
*******************************************************************************/
JLP_wxGraphicPanel::JLP_wxGraphicPanel( wxFrame *frame, const int my_wxID,
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
Drawing_wxgdev = NULL;
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
JLP_wxGraphicPanel::JLP_wxGraphicPanel( wxFrame *frame, const int my_wxID,
                                        JLP_wxLogbook *logbook,
                                        int x, int y, int iwidth, int iheight)
                                        : wxPanel( frame, my_wxID, 
                                        wxPoint(x, y), wxSize(iwidth, iheight))
{
wxString input_filename;

// Transform coma into point for numbers:
setlocale(LC_NUMERIC, "C");

initialized = 0;
Drawing_wxgdev = NULL;
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
void JLP_wxGraphicPanel::Setup_DrawingPanel(const int ID0)
{
int status;
wxString buffer;

// Set background to white by default:
 SetBackgroundColour(wxColour(* wxWHITE));

 wxBoxSizer *topsizer = new wxBoxSizer( wxVERTICAL );
 
 m_StaticHelp = new wxStaticText(this, -1, 
                    wxT("Processing mode = 0 : click and drag to select box"));
 m_StaticCoord = new wxStaticText(this, -1, 
                                 wxT("x=12.3456 y=12.3456"), wxPoint(-1,-1), 
                                 wxSize(180,20));

// New JLP_GDev_wxWID object (for display on the screen)
// Start at wxpoint(0,0) from top left corner:
 Drawing_wxgdev = new JLP_GDev_wxWID(this, ID0, wxPoint(0, 0), 
                                     wxSize(m_width, m_height - 60),
                                     m_StaticCoord, m_StaticHelp);
 topsizer->Add(Drawing_wxgdev, 1, wxEXPAND);

// Get idev number:
 status = Drawing_wxgdev->Get_idev(Drawing_idev);

 if(status) {
   fprintf(stderr, "Setup_DrawingPanel/Fatal error: Drawing_idev = %d\n",
           Drawing_idev);
   buffer.Printf(wxT("Setup_DrawingPanel/Fatal error: Drawing_idev = %d"),
           Drawing_idev);
   wxMessageBox(buffer, wxT("JLP_wxGraphicPanel"), wxOK | wxICON_ERROR);
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

 m_SaveAsButton = new wxBitmapButton(this, ID_DRAW_SAVE_AS,
                                       wxBITMAP(save));
 button_sizer->Add( m_SaveAsButton, 0, wxLEFT, 20);

// Yellow buttons (like for "jlp_wx_video_panel.cpp")
 if(jlp_video_panel != NULL) {
   m_VideoNextFastButton = new wxBitmapButton(this, ID_DRAW_VIDEO_NEXT_FAST,
                                           wxBITMAP(move_right_fast1));
   button_sizer->Add( m_VideoNextFastButton, 0, wxLEFT, 20);
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

// Create a sizer with no border: 
 topsizer->Add(button_sizer, 0, wxALL, 20);

// To set the minimum size that fits this setup:
 SetSizerAndFit(topsizer);      // use the sizer for layout

return;
}
/************************************************************************
* Destructor 
*************************************************************************/
JLP_wxGraphicPanel::~JLP_wxGraphicPanel()
{
// Free Memory:
if(Drawing_wxgdev != NULL) delete Drawing_wxgdev;

Close();
}
/************************************************************************
* To enlarge the window at the maximum size: 
* JLP 2015: resize events should be handled here 
* (attempts to make it at the jlp_gdev class level have failed !)
* (=> It should be done using the instanciation not the class !!)
************************************************************************/
void JLP_wxGraphicPanel::OnResize(wxSizeEvent &event )
{

if(initialized == 1234) JLP_ResizeGraphicPanel();

// Skip this event (to avoid infinite processing of the same event):
 event.Skip();

return;
}
/************************************************************************
* To enlarge the window at the maximum size: 
* JLP 2015: resize events should be handled here 
* (attempts to make it at the jlp_gdev class level have failed !)
* (=> It should be done using the instanciation not the class !!)
*
* Routine defined here outside of resize events to be called manually
************************************************************************/
void JLP_wxGraphicPanel::JLP_ResizeGraphicPanel()
{
int width0, height0;

if(initialized != 1234) return;

// Both GetSize() or GetClientSize() give a bad size (too large)
GetClientSize(&width0, &height0);
/*
printf("JLP_wxGraphicPanel::GetClientSize/width,height = %d,%d\n", 
        width0, height0);
*/

// Correct this size and update plot parameters
 Drawing_wxgdev->ResizePlot1(width0, height0);

return;
}
/*********************************************************************
* SaveAs button
* Output curve as an ASCII file 
*********************************************************************/
void JLP_wxGraphicPanel::OnSaveAsButton(wxCommandEvent &event)
{
wxString save_filename, str0;
char ascii_filename[128];
int i, npts0, icurve, status;
double *xplot0, *yplot0, *errorx0, *errory0;
FILE *fp_out;
bool save_to_file0;

if(initialized != 1234) return;

   wxFileDialog dialog(NULL, _T("save to ASCII file"), wxEmptyString,
                       wxEmptyString, _T("files (*.txt)|*.txt"),
                       wxFD_SAVE|wxFD_OVERWRITE_PROMPT);
   if (dialog.ShowModal() != wxID_OK) return;

   save_filename = dialog.GetPath();

   if ( save_filename.empty() ) return;
   strcpy(ascii_filename, (const char *)save_filename.mb_str());

if((fp_out = fopen(ascii_filename,"w")) != NULL) {
// GetCurveData
// In "jlp_splot/jlp_gdev_curves_process.cpp"
    icurve = 0;
    status = Drawing_wxgdev->GetCurveData(&xplot0, &yplot0, &errorx0, &errory0,
                                          &npts0, icurve);
    for(i = 0; i < npts0; i++) {
     fprintf(fp_out, "%f %f\n", xplot0[i], yplot0[i]);
     }
    fclose(fp_out);
// Display/Save string to logbook
    str0 = _T("Curve #0 saved to ") + save_filename + _T("\n");
    save_to_file0 = false;
    wxGP_WriteToLogbook(str0, save_to_file0);
   } else {
// Display/Save string to logbook
    str0 = _T("Error saving curve #0 to ") + save_filename + _T("\n");
    save_to_file0 = false;
    wxGP_WriteToLogbook(str0, save_to_file0);
   }

return;
}
/*********************************************************************
* ZoomIn/ZoomOut button
*********************************************************************/
void JLP_wxGraphicPanel::OnZoomButton(wxCommandEvent &event)
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

Drawing_wxgdev->Curves_BoxLimitsZoom(zoom_in);
Drawing_wxgdev->RedrawToBackupDC(1302);

// Update cursor:
if(jlp_video_panel != NULL) jlp_video_panel->UpdateCursorInSpectrumPanel();

return;
}
/*********************************************************************
* Move right/left button
*********************************************************************/
void JLP_wxGraphicPanel::OnMoveButton(wxCommandEvent &event)
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

Drawing_wxgdev->Curves_BoxLimitsMove(move_to_right);
Drawing_wxgdev->RedrawToBackupDC(1303);

// Update cursor:
if(jlp_video_panel != NULL) jlp_video_panel->UpdateCursorInSpectrumPanel();

return;
}
/*********************************************************************
* Show Video Plane button
*********************************************************************/
void JLP_wxGraphicPanel::OnShowVideoPlane(wxCommandEvent &event)
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
/**********************************************************************
* Display/Save string to logbook
***********************************************************************/
int JLP_wxGraphicPanel::wxGP_WriteToLogbook(wxString str1, bool save_to_file0)
{
int status = -1;

if(initialized == 1234 && jlp_logbook != NULL) {
 status = jlp_logbook->WriteToLogbook(str1, save_to_file0);
 }

return(status);
}
/************************************************************************
* Load a new double precision image array 
************************************************************************/
int JLP_wxGraphicPanel::wxGP_LoadDbleImage1(double *dble_image0, int nx0, 
                                            int ny0)
{
int status = -1;

if(initialized == 1234 && Drawing_wxgdev != NULL) {
 status = Drawing_wxgdev->LoadDbleImage1(dble_image0, nx0, ny0);
 }

return(0);
}
