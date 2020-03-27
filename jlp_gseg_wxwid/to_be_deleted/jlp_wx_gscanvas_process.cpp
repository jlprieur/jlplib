/******************************************************************************
* Name:        jlp_cgdev_wxwid_process.cpp
*              JLP_wxGseg_Canvas class
*
* Purpose:     simple curve processing
* NB: Handling JLP_wxGseg_Canvas menu and events in "jlp_cgdev_wxwid_menu.cpp"
*
* Author:      JLP
* Version:     06/02/2016
******************************************************************************/
#include "jlp_wx_gscanvas.h"
#include "jlp_cgdev_wxwid_id.h"       // ID_FILTER_...
#include "jlp_wx_patch_dlg.h"
#include "jlp_patch_set1.h"
#include "jlp_auto_scale1.h"           // For auto_scale3 ...
#include "jlp_wx_gsproc1.h"          // For JLP_gsProc1
#include "jlp_wx_curve_labels.h"      // JLP_wxImageLabels class
#include "jlp_wx_ipanel.h"  // Filters

/*
  void OnMotion(wxMouseEvent& event);
  void OnLeftDown(wxMouseEvent& event);
  void HandleDraggingMotion(wxMouseEvent& event);
  int SetNewRequiredPoints(int npts_are_required, int limits_box_type);
  int CheckBoxLimits(double *x1_1, double *y1_1, double *x1_2, double *y1_2);
  int GetUserCoordFromCursor(double *x1, double *y1, int *in_frame);
  int LeftUp_GetLastData(int *nn, double *x1, double *y1);
  int LeftDown_GetLastData(int *nn, double *x1, double *y1);
*/

/*************************************************************
* OnMotion: when the user moves the mouse on JLP_wxImageCanvas
*
*
* Handle the cross-hair or arrow cursor
* that displays the coordinates and the curve intensity
* in real time as the user moves the mouse
*************************************************************/
void JLP_wxGseg_Canvas::OnMotion(wxMouseEvent& event)
{
wxString str_coordinates;
long ix, iy;
double x1, y1;
int in_frame;
wxDC *dc1;

  event.GetPosition(&ix, &iy);
  if(jlp_gsegraf0 == NULL) return;

  jlp_gsegraf0->FromDeviceToUserCoord(ix, iy, &x1, &y1, &in_frame);

// x1, y1 are the user coordinates
  if(in_frame) {
   str_coordinates.Printf(wxT("x=%.5g y=%.5g"), x1, y1);

 } else {
   str_coordinates = wxT("");
 }
// Display coordinates on the screen
  DisplayCoordinates(str_coordinates);

// event.Moving() is true if motion without any button pressed
if(event.Moving()) {

// ****************** CrossHair Cursor ***********************************
  if(backup_dc->IsOk() &&
     ((cgdev_settings1.cursor_type.Cmp(wxT("CrossHair")) == 0)
     || (cgdev_settings1.cursor_type.Cmp(wxT("CrossHair1")) == 0)) ) {

// Warning: should not use wxPaintDC outside of Paint events,
// so use wxClientDC:
     dc1 = new wxClientDC(this);

// Update the screen with backup_dc (hence erase previous overlay drawings):
     cgdev_render(dc1);

// Draw crosshair on top of curve:
//   dc1->SetPen( *wxWHITE_PEN );
     dc1->SetPen(cgdev_settings1.pen_colour);
     dc1->CrossHair(ix, iy);

// Wait 3 ms to avoid flickering (for Windows):
//     ::wxMilliSleep(3);
// (useless...)
     delete dc1;
     }

// event.Dragging() is true if motion with button pressed
  } else if(event.Dragging()) {
  HandleDraggingMotion(event);
  }

// Pass on event for other purposes (dragging for instance):
  event.Skip(true);

return;
}
/*************************************************************
* OnLeftDown: process a wxEVT_LEFT_DOWN event (see "jlp_wx_canvas1_menu.cpp")
*
* The handler of this event should normally call event.Skip() to allow
* the default processing to take place as otherwise the window under mouse
* wouldn't get the focus.
*************************************************************/
void JLP_wxGseg_Canvas::OnLeftDown(wxMouseEvent& event)
{
long ix, iy;
double x1, y1;
int in_frame;

event.GetPosition(&ix, &iy);

jlp_gsegraf0->FromDeviceToUserCoord(ix, iy, &x1, &y1, &in_frame);

// Otherwise display the curve values:
if(in_frame) {
  if(s_nLeftDown < MAX_CURSOR_NPOINTS - 1) {
// Check if the user has not clicked twice by error:
   if(s_nLeftDown > 0) {
    if(x1 != s_LeftDown_x[s_nLeftDown-1] &&
       y1 != s_LeftDown_y[s_nLeftDown-1]){
    s_LeftDown_x[s_nLeftDown] = x1;
    s_LeftDown_y[s_nLeftDown] = y1;
    s_nLeftDown++;
     } // Case of new point different from previous
// Case when no previous point has been entered
     } else {
       s_LeftDown_x[s_nLeftDown] = x1;
       s_LeftDown_y[s_nLeftDown] = y1;
       s_nLeftDown++;
     }
   } else {
    fprintf(stderr, "OnLeftDown/Error: too many points entered (max=%d)\n",
            s_nLeftDown);
   }
}

// To pass on this event (necessary when the focus is obtained by
// clicking on the mouse...)
  event.Skip();

return;
}
/*************************************************************
* OnLeftUp: process a wxEVT_LEFT_UP event (see "jlp_wx_canvas1_menu.cpp")
*
*************************************************************/
void JLP_wxGseg_Canvas::OnLeftUp(wxMouseEvent& event)
{
long ix, iy;
double x1, y1;
int ii, in_frame;

 if(s_nLeftDown == 0) {
// Return since nothing else to be done:
     event.Skip();
     return;
   }

  event.GetPosition(&ix, &iy);

  jlp_gsegraf0->FromDeviceToUserCoord(ix, iy, &x1, &y1, &in_frame);

// If the user has released the mouse button on the same
// point when 2, 4 or 6 points are needed,
// cancel the last left-down point
 if((nPointsAreRequired == 2) || (nPointsAreRequired == 4) ||
    (nPointsAreRequired == 6) || (nPointsAreRequired == 8)) {
    ii = s_nLeftDown -1;
     if (x1 == s_LeftDown_x[ii]  && y1 == s_LeftDown_y[ii]) {
      s_nLeftDown--;
      return;
     }
  }

// If second point is different from first
// (in the case when two points are required)
// or other case go ahead:
  if(s_nLeftUp < MAX_CURSOR_NPOINTS - 1) {
    s_LeftUp_x[s_nLeftUp] = x1;
    s_LeftUp_y[s_nLeftUp] = y1;
    s_nLeftUp++;
    } else {
    fprintf(stderr, "OnLeftUp/Error: too many points entered (max=%d)\n",
            s_nLeftUp);
    }

// Handle case when nPointsRequired == s_nLeftDown:
 if((nPointsAreRequired == 1) || (nPointsAreRequired == 2 * s_nLeftDown)){
    active_gsproc->DataProcessing(s_LeftDown_x, s_LeftDown_y, s_nLeftDown,
                                  s_LeftUp_x, s_LeftUp_y, s_nLeftUp, m_label1);
// Refresh screen (necessary to see new labels or erase the boxes):
    RedrawToBackupDC(); 
// Reset counters for new calculation:
    s_nLeftUp = 0;
    s_nLeftDown = 0;
 }

// To pass on this event (necessary when the focus is obtained by
// clicking on the mouse...)
  event.Skip();

return;
}
/*************************************************************
* HangleDraggingMotion: when the user moves the mouse on the canvas
*             with a button pressed.
*
*************************************************************/
void JLP_wxGseg_Canvas::HandleDraggingMotion(wxMouseEvent& event)
{
wxString str1;
long ix0, iy0;
int in_frame, ix_start, iy_start;
double  x1_start, y1_start;
int radius0, width0, height0, nn, status;
wxDC *dc1;
 dc1 = new wxClientDC(this);

// event.Moving() is true if motion without button pressed
// event.Dragging() is true if motion with button pressed
// Handle only the case of mouse motion with left button pressed:
if(event.LeftIsDown()) {

// Get the user coordinates of the previous entered point
status = LeftDown_GetLastData(&nn, &x1_start, &y1_start);

if(!status) {
// Compute the device coordinates of that previous entered point
  jlp_gsegraf0->FromUserToDeviceCoord(x1_start, y1_start, 
                                      &ix_start, &iy_start, &in_frame);
 if(in_frame) {

// Get current coordinates:
   event.GetPosition(&ix0, &iy0);

// Update the screen with backup_dc (hence erase previous overlay drawings):
     cgdev_render(dc1);

// Draw new drawing:
     dc1->SetPen(cgdev_settings1.pen_colour);
// x, y, width, height:
     dc1->SetBrush(*wxTRANSPARENT_BRUSH);
// Limits box type: 0=none 1=line, 2=rectangle, 3=circle, 4=ellipse, 5=ring
   if(LimitsBoxType == 1) {
     dc1->DrawLine( ix_start, iy_start, ix0, iy0);
     } else if(LimitsBoxType == 3) {
     radius0 = (int)sqrt((double)SQUARE(ix0 - ix_start)
                        + (double)SQUARE(iy0 - iy_start));
     dc1->DrawCircle( ix_start, iy_start, radius0);
// Ellipse (in rectangular box)
     } else if(LimitsBoxType == 4) {
     width0 = (int)(ix0 - ix_start);
     height0 = (int)(iy0 - iy_start);
     dc1->DrawEllipse(ix_start, iy_start, width0, height0);
// Ring: draw two circles 
     } else if(LimitsBoxType == 5) {
     width0 = (int)ABS(ix0 - ix_start);
     height0 = (int)ABS(iy0 - iy_start);
     dc1->DrawCircle( ix_start, iy_start, width0);
     dc1->DrawCircle( ix_start, iy_start, height0);
// Rectangle 
     } else if(LimitsBoxType != 0) {
     dc1->DrawRectangle( ix_start, iy_start, ix0 - ix_start, iy0 - iy_start);
     }
   } else {
   fprintf(stderr, "HandleDraggingMotion/Sorry data point is out of frame\n");
   }
 }

} // End of dragging with left button pressed

delete dc1;

// Passing on this event
event.Skip();

return;
}
/***********************************************************************
* Set interactive internal processing mode (selected in the popup menu)
*
* 0=BoxLimits:
* Set the thresholds by computing the min/max values
* in a rectangular box selected by the user
*
* 1=Statistics in a rectangular box selected by the user

* 2=Astrometry: to compute the position of a pattern within a circle
* that is interactively selected by the user
*
* 3=Photometry: to compute the flux of a pattern within a circle
* that is interactively selected by the user
*
* 4=Add label
*
* 8=Slice: to compute the slice along a line 
* that is interactively selected by the user
***********************************************************************/
int JLP_wxGseg_Canvas::SetInternalProcessingMode(int processing_mode)
{

// Configure active processing mode to internal processing mode:
active_gsproc = m_gsproc1;

if(processing_mode < -1 || processing_mode > MAX_PROCESSING_NBER) processing_mode = -1;

/* InteractiveProcessingMode :
* -1=None 0=BoxLimits 1=Statistics, 2=Astrometry 3=Photometry
* 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
* 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
* 13=Add a ring 14=Remove a shape
*/

cgdev_settings1.InternalProcessingMode = processing_mode;

// Output: LimitsBoxType, nPointsAreRequired
m_gsproc1->SetNewProcessingMode(cgdev_settings1.InternalProcessingMode, 
                                 &LimitsBoxType, &nPointsAreRequired);

// Reset point counters:
s_nLeftDown = 0;
s_nLeftUp = 0;

// Update help text on top of screen:
RedrawToBackupDC();

return(0);
}
/***********************************************************************
* Set interactive processing mode from external routine
* (called by Gdpisco, GdPanel)
*
* Input:
* LimitsBoxType, nPointsAreRequired
***********************************************************************/
int JLP_wxGseg_Canvas::SetExternalProcessingMode(JLP_wx_gsProc *external_gsproc,
                               int limits_box_type, int npts_required)
{

// Configure active processing mode to internal processing mode:
active_gsproc = external_gsproc;
LimitsBoxType = limits_box_type;
nPointsAreRequired = npts_required;

// Disable m_gsproc1 internal interactive mode:
cgdev_settings1.InternalProcessingMode = -1;

// Reset point counters:
s_nLeftDown = 0;
s_nLeftUp = 0;

// Refresh screen to update help text on top of screen
RedrawToBackupDC();

return(0);
}
/*****************************************************************
* Limits box type: 1=line, 2=rectangle, 3=circle, 4=ellipse, 5=ring
*****************************************************************/
int JLP_wxGseg_Canvas::SetNewRequiredPoints(int npts_are_required,
                                          int limits_box_type)
{
// Only one, two or six points allowed here yet:
if(npts_are_required !=1 && npts_are_required != 2 && npts_are_required != 6) {
  fprintf(stderr, "SetNewRequiredPoints/Error: npts_are_required=%d\n",
          npts_are_required);
  return(-1);
  }

 nPointsAreRequired = npts_are_required;
 LimitsBoxType = limits_box_type;
return(0);
}
/***********************************************************************
* Get user coordinates of the point chosen by the user when
* pressing the left button of the mouse
*
* (Not used)
***********************************************************************/
int JLP_wxGseg_Canvas::GetUserCoordFromCursor(double *x1, double *y1, 
                                            int *in_frame)
{
// max_waiting_time in milli-seconds (5 minutes):
unsigned long max_waiting_time = 300000;
// Waiting_time in milli-seconds for each iteration:
unsigned long wait_value = 100;
unsigned long i;
int status = -1;
int n_old, n_new;

LeftUp_GetLastData(&n_old, x1, y1);

for(i = 0; i < max_waiting_time/wait_value; i++) {
  wxMilliSleep(wait_value);
  LeftUp_GetLastData(&n_new, x1, y1);
  if(n_new > n_old) {
// TBD
//    *in_frame = (*x1 < 0 || *x1 >= nx1 || *y1 < 0 || *y1 >= ny1) ? 0 : 1;
    *in_frame = 1;
    status = 0;
    break;
    }
  }

return(status);
}
/***********************************************************************
* Get nn, ix, iy of the last point entered by the user when releasing
* the left button of the mouse
*
* OUTPUT:
* x1, y1: user coordinates of the last entered point
* nn: number of points entered by the user
***********************************************************************/
int JLP_wxGseg_Canvas::LeftUp_GetLastData(int *nn, double *x1, double *y1)
{
int status = -1;

// Initializing the output variables:
*nn = 0; *x1 = 0.; *y1 = 0.;

// Get the coordinates of the previous entered point
  if(s_nLeftUp > 0) {
    *nn = s_nLeftUp;
    *x1 = s_LeftUp_x[s_nLeftUp - 1];
    *y1 = s_LeftUp_y[s_nLeftUp - 1];
    status = 0;
    }

return (status);
}
/***********************************************************************
* Get nn, ix, iy of the last point entered by the user when pressing on
* the left button of the mouse
*
* OUTPUT:
* x1, y1: user coordinates of the last entered point
* nn: number of points entered by the user
***********************************************************************/
int JLP_wxGseg_Canvas::LeftDown_GetLastData(int *nn, double *x1, double *y1)
{
int status = -1;

// Initializing the output variables:
*nn = 0; *x1 = 0.; *y1 = 0.;

// Get the coordinates of the previous entered point
  if(s_nLeftDown > 0) {
     *nn = s_nLeftDown;
     *x1 = s_LeftDown_x[s_nLeftDown - 1];
     *y1 = s_LeftDown_y[s_nLeftDown - 1];
     status = 0;
     }

return (status);
}
