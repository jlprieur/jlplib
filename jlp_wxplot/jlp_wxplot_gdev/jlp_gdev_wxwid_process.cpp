/******************************************************************************
* Name:        jlp_gdev_wxwid_process.cpp
*              JLP_GDev_wxWID class
*
* Purpose:     simple image processing
* NB: Handling JLP_GDev_wxWID menu and events in "jlp_gdev_wxwid_menu.cpp"
*
* Author:      JLP
* Version:     26/12/2015
******************************************************************************/
#include "jlp_gdev_wxwid.h"
#include "jlp_wx_patch_dlg.h"
#include "jlp_patch_set1.h"
#include "jlp_wx_gdproc1.h"          // For JLP_GDProc1
#include "jlp_wx_scrolled1.h"        // For JLP_wxScrolled1
#include "jlp_wxgdev_labels.h"       // JLP_wxGDevLabels class
#include "jlp_wx_ipanel.h"           // Filters

/*
  void OnFilterUnsharp1(wxCommandEvent& event);
  void OnFilterUnsharp2(wxCommandEvent& event);
  void OnFilterUnsharp3(wxCommandEvent& event);
  void OnFilterHighContrast(wxCommandEvent& event);
  void OnFilterVeryHighContrast(wxCommandEvent& event);
  void OnFilterNone(wxCommandEvent& event);

  void OnMotion(wxMouseEvent& event);
  void OnLeftDown(wxMouseEvent& event);
  void HandleDraggingMotion(wxMouseEvent& event);
  int Process_TwoPoints(double x1, double y1, double x2, double y2);
  int Process_SixPoints(double *x_down, double *y_down, double *x_up,
                        double *y_up);
  int SetNewRequiredPoints(int npts_are_required, int limits_box_type);
  int StartTwoOrSixPointsAcquisition();
  int CheckBoxLimits(double *x1, double *y1, double *x2, double *y2);
  int UserCoordFromMouse(long idev_x0, long idev_y0,
                         wxString *str_coordinates0, int *in_frame);
  void DisplayCoordinatesToScreen( wxDC *dc0, wxString str_coordinates0);
*/

/*
#define DEBUG
*/

/*************************************************************
* OnMotion: when the user moves the mouse on JLP_wxImageCanvas
*
* Handle the cross-hair or arrow cursor
* that displays the coordinates and the image intensity
* in real time as the user moves the mouse
*************************************************************/
void JLP_GDev_wxWID::OnMotion(wxMouseEvent& event)
{
wxString str_coordinates;
long idev_x1, idev_y1;
int in_frame, scroll_x, scroll_y, width1, height1;
wxDC *dc1, *scroll_dc0;

// Warning: should not use wxPaintDC outside of Paint events,
// so use wxClientDC:
  dc1 = new wxClientDC(this);
  if(m_scrolled_window != NULL)
    scroll_dc0 = new wxClientDC(m_scrolled_window);
  else
    scroll_dc0 = NULL;

/* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
*/

event.GetPosition(&idev_x1, &idev_y1);

// Convert device coordinates and create an output string for displaying
// the user coordinates (and corresponding value if relevant)
 UserCoordFromMouse(idev_x1, idev_y1, &str_coordinates, &in_frame);

/********************* OUT OF FRAME ************************************/
// Display an empty string as coordinates to the screen
if(in_frame == 0) {
 str_coordinates = wxT("");
 DisplayCoordinatesToScreen(dc1, str_coordinates);
 delete dc1;
 if(scroll_dc0 != NULL) delete scroll_dc0;
// Pass on the event for other purposes:
 event.Skip(true);
 return;
 }

/********************* IN FRAME ************************************/

// event.Moving() is true if motion without any button pressed
if(event.Moving()) {

// ****************** CrossHair Cursor ***********************************
  if(backup_dc->IsOk() &&
     ((wxgdev_settings1.cursor_type.Cmp(wxT("CrossHair")) == 0)
     || (wxgdev_settings1.cursor_type.Cmp(wxT("CrossHair1")) == 0)) ) {

/* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
*/
// Get origin in case of scrolling window:
   if(m_scrolled_window != NULL) {
      m_scrolled_window ->CalcUnscrolledPosition(0, 0, &scroll_x, &scroll_y);
// For curves:
     } else {
     scroll_x = 0;
     scroll_y = 0;
     }
// For scrolled images:
   if(Jgc0.gdev_graphic_type == 3) {
// Update the screen with backup_dc (hence erase previous overlay drawings):
     m_scrolled_window ->DoPrepareDC(*scroll_dc0);
     wxGDev_RenderForScrolledImages(scroll_dc0);
     } else {
// Update the screen with backup_dc (hence erase previous overlay drawings):
     dc1->GetSize(&width1, &height1);
     wxGDev_RenderForCurves(dc1, width1, height1);
     }

// Draw crosshair on top of image:
// dc1->SetPen( *wxWHITE_PEN );
   dc1->SetPen(wxgdev_settings1.pen_colour);
   dc1->CrossHair(idev_x1 + scroll_x, idev_y1 + scroll_y);

// Wait 3 ms to avoid flickering (for Windows):
//     ::wxMilliSleep(3);
// (useless...)
   }

// Display coordinates on the screen
  DisplayCoordinatesToScreen(dc1, str_coordinates);

// event.Dragging() is true if motion with button pressed
  } else if(event.Dragging()) {
  HandleDraggingMotion(event, str_coordinates);
  }

// Pass on event for other purposes:
event.Skip(true);

delete dc1;
if(scroll_dc0 != NULL) delete scroll_dc0;
return;
}
/*************************************************************
* Soft refresh...
* (better than Refresh() when scrolled window is used...)
*************************************************************/
void JLP_GDev_wxWID::wxGdev_Refresh()
{
Refresh();

return;
}
/*************************************************************
* OnRightUp: process a wxEVT_RIGHT_UP event (see "jlp_wx_canvas1_menu.cpp")
*
*************************************************************/
void JLP_GDev_wxWID::OnRightUp(wxMouseEvent& event)
{
  ShowPopupMenu();
}
/*************************************************************
* OnLeftDown: process a wxEVT_LEFT_DOWN event (see "jlp_wx_canvas1_menu.cpp")
*
* The handler of this event should normally call event.Skip() to allow
* the default processing to take place as otherwise the window under mouse
* wouldn't get the focus.
*************************************************************/
void JLP_GDev_wxWID::OnLeftDown(wxMouseEvent& event)
{
double dev_x0, dev_y0;
long idev_x0, idev_y0;

// Input of mouse location in (long int) device coordinates:
 event.GetPosition(&idev_x0, &idev_y0);
 dev_x0 = (double)idev_x0;
 dev_y0 = (double)idev_y0;

// Call Mouse_AddLeftDownPoint with (double) dev coordinates
 Mouse_AddLeftDownPoint(dev_x0, dev_y0);

// To pass on this event (necessary when the focus is obtained by
// clicking on the mouse...)
 event.Skip();

return;
}
/*************************************************************
* OnLeftUp: process a wxEVT_LEFT_UP event (see "jlp_wx_canvas1_menu.cpp")
*
*************************************************************/
void JLP_GDev_wxWID::OnLeftUp(wxMouseEvent& event)
{
long idev_x1, idev_y1;
double dev_x1, dev_y1, dev_x2, dev_y2;
int n_left_down, status;
int nPointsAreRequired0, LimitsBoxType0, ProcessingMode0;

if(initialized != 1234) return;

// Check if some left-down points have been already entered,
// and retrieve the user coord. of the last one
 status = Mouse_GetLastLeftDownPoint(&dev_x2, &dev_y2, &n_left_down);

/* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
*/
// For scrolled images:
 if(((c_image1 == NULL) && (Jgc0.gdev_graphic_type == 3))
    || (n_left_down <= 0) || (status != 0) || (active_gdproc == NULL)) {
// Return since nothing else to be done:
     event.Skip();
     return;
   }

// Input of mouse location in (long) device coordinates:
 event.GetPosition(&idev_x1, &idev_y1);
 dev_x1 = (double)idev_x1;
 dev_y1 = (double)idev_y1;

// Limits box type: 1=line, 2=rectangle, 3=circle
 active_gdproc->GetActiveProcessingMode(&ProcessingMode0, &LimitsBoxType0,
                                        &nPointsAreRequired0);

// Save the coordinates to logbook when processing mode is set to 19 :
 if(ProcessingMode0 == 19) {
// Add left up point, even if equal to left down point in this case:
    Mouse_AddLeftUpPoint(dev_x1, dev_y1);
    active_gdproc->DataProcessing(LeftDown_dev_x_1, LeftDown_dev_y_1,
                                  npts_LeftDown_1, LeftUp_dev_x_1,
                                  LeftUp_dev_y_1, npts_LeftUp_1, m_label1);
// Reset counters for new calculation:
    Mouse_EraseLeftDownPoints();
    Mouse_EraseLeftUpPoints();
// Return since nothing else to be done:
    event.Skip();
    return;
   }

// If the user has released the mouse button on the same
// point when 2, 4 or 6 points are needed,
// cancel the last left-down point
 if((nPointsAreRequired0 == 2) || (nPointsAreRequired0 == 4) ||
    (nPointsAreRequired0 == 6) || (nPointsAreRequired0 == 8)) {
   if((dev_x1 == dev_x2) && (dev_y1 == dev_y2)){
      Mouse_RemoveLastLeftDownPoint();
      event.Skip();
      return;
     }
  }

// If second point is different from first
// (in the case when two points are required)
// or other case go ahead:
 Mouse_AddLeftUpPoint(dev_x1, dev_y1);

// Handle case when nPointsRequired0 == n_left_down:
 if((nPointsAreRequired0 == 1) || (nPointsAreRequired0 == 2 * n_left_down)){
   active_gdproc->DataProcessing(LeftDown_dev_x_1, LeftDown_dev_y_1,
                                 npts_LeftDown_1,
                                 LeftUp_dev_x_1, LeftUp_dev_y_1, npts_LeftUp_1,
                                 m_label1);
// Redraw to screen (necessary to see new labels or erase the boxes):
   RedrawToBackupDC(5001);

// Reset counters for new calculation:
   Mouse_EraseLeftDownPoints();
   Mouse_EraseLeftUpPoints();
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
void JLP_GDev_wxWID::HandleDraggingMotion(wxMouseEvent& event,
                                          wxString str_coordinates)
{
wxString str1, str_empty = wxT(" ");
long idev_x0, idev_y0;
int scroll_x, scroll_y, nn;
double dev_x0, dev_y0, dev_x1_start, dev_y1_start;
int radius0, width0, height0, status, width1, height1;
int nPointsAreRequired0, LimitsBoxType0, ProcessingMode0;
wxDC *dc1, *scroll_dc0, *drawing_dc0;

// event.Moving() is true if motion without button pressed
// event.Dragging() is true if motion with button pressed
// Handle only the case of mouse motion with left button pressed:
if(event.LeftIsDown() == false) {event.Skip(); return;}

// Get the dev coordinates of the previous entered point
 status = Mouse_GetLastLeftDownPoint(&dev_x1_start, &dev_y1_start, &nn);
 if(status != 0) return;

// Get active JLP_GdProc
 if(active_gdproc == NULL) return;

// Limits box type: 1=line, 2=rectangle, 3=circle
 active_gdproc->GetActiveProcessingMode(&ProcessingMode0, &LimitsBoxType0,
                                        &nPointsAreRequired0);

// Input of mouse location in device coordinates:
 event.GetPosition(&idev_x0, &idev_y0);
 dev_x0 = (double)idev_x0;
 dev_y0 = (double)idev_y0;

/* DEBUG
printf("dragging: dev_x1/y1_start = %d %d (nn=%d) status=%d dev_x/y0 : %d %d\n",
       (int)dev_x1_start, (int)dev_y1_start, nn, status,
       (int)dev_x0, (int)dev_y0);
*/

 dc1 = new wxClientDC(this);
 if(m_scrolled_window != NULL) {
    scroll_dc0 = new wxClientDC(m_scrolled_window);
    drawing_dc0 = scroll_dc0;
  } else {
    scroll_dc0 = NULL;
    drawing_dc0 = dc1;
  }

  scroll_x = 0;
  scroll_y = 0;

// For scrolled images:
 if(Jgc0.gdev_graphic_type == 3) {
   if(m_scrolled_window != NULL) {
      m_scrolled_window->CalcUnscrolledPosition(0, 0, &scroll_x, &scroll_y );
      }
// Update the screen with backup_dc (hence erase previous overlay drawings):
      m_scrolled_window->DoPrepareDC(*scroll_dc0);
     wxGDev_RenderForScrolledImages(scroll_dc0);
     } else {
// Update the screen with backup_dc (hence erase previous overlay drawings):
     dc1->GetSize(&width1, &height1);
     wxGDev_RenderForCurves(dc1, width1, height1);
     }

// Erase 2nd column of StatusBar with old coordinates (icol=0 or 1):
 if(m_StatusBar != NULL) {
   m_StatusBar->SetStatusText(str_empty, 1);
 } else {
// Write coordinates (and help text) on top of the window
// or on StaticText labels
  if(Jgc0.gdev_graphic_type == 3) {
    DisplayCoordinatesForImages(scroll_dc0, str_coordinates);
   } else {
    DisplayCoordinatesForCurves(str_coordinates);
   }
 }

// Draw new drawing:
  drawing_dc0->SetPen(wxgdev_settings1.pen_colour);
// x, y, width, height:
  drawing_dc0->SetBrush(*wxTRANSPARENT_BRUSH);
// Limits box type: 0=none 1=line, 2=rectangle, 3=circle, 4=ellipse, 5=ring
  switch (LimitsBoxType0) {
    case 1:
      drawing_dc0->DrawLine(dev_x1_start + scroll_x, dev_y1_start + scroll_y,
                        dev_x0 + scroll_x, dev_y0 + scroll_y);
      break;
// 3 : Circle
    case 3:
      radius0 = (int)sqrt((double)SQUARE(dev_x0 - dev_x1_start)
                         + (double)SQUARE(dev_y0 - dev_y1_start));
      drawing_dc0->DrawCircle(dev_x1_start + scroll_x, dev_y1_start + scroll_y,
                      radius0);
      break;
// 4 : Ellipse (in rectangular box)
    case 4:
      width0 = (int)(dev_x0 - dev_x1_start);
      height0 = (int)(dev_y0 - dev_y1_start);
      drawing_dc0->DrawEllipse(dev_x1_start + scroll_x, dev_y1_start + scroll_y,
                       width0, height0);
      break;
// 5 : Ring: draw two circles
    case 5:
      width0 = (int)ABS(dev_x0 - dev_x1_start);
      height0 = (int)ABS(dev_y0 - dev_y1_start);
      drawing_dc0->DrawCircle(dev_x1_start + scroll_x, dev_y1_start + scroll_y,
                      width0);
      drawing_dc0->DrawCircle(dev_x1_start + scroll_x, dev_y1_start + scroll_y,
                      height0);
      break;
// 0 or other : Rectangle
    default:
    case 0:
    case 2:
    drawing_dc0->DrawRectangle(dev_x1_start + scroll_x, dev_y1_start + scroll_y,
                       dev_x0 - dev_x1_start, dev_y0 - dev_y1_start);
     break;
  }

delete dc1;
if(scroll_dc0 != NULL) delete scroll_dc0;

// Passing on this event
event.Skip();

return;
}
/***********************************************************************
* Convert device coordinates to user coordinates
* and creates the corresponding string for further display
***********************************************************************/
int JLP_GDev_wxWID::UserCoordFromMouse(long idev_x0, long idev_y0,
                                       wxString *str_coordinates0,
                                       int *in_frame)
{
int status, ix1, iy1;
double x1_user, y1_user, value1;

 *str_coordinates0 = wxT("");
 *in_frame = 0;

// DEBUG:
//printf("UserCoordFromMouse/graphic_type=%d\n", Jgc0.gdev_graphic_type);

/* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
*/
  if(c_image1 != NULL && Jgc0.gdev_graphic_type == 3) {
// Display coordinates and values for images
     ConvDevToUser(idev_x0, idev_y0, &x1_user, &y1_user, in_frame);
     if(*in_frame != 1) return(-1);
// x1, y1 are the user coordinates
     ix1 = NINT(x1_user);
     iy1 = NINT(y1_user);
     value1 = c_image1->GetValue(ix1, iy1);
/* DEBUG:
     print("Device coord: x=%d y=%d User coord: x1=%.1f y1=%.1f Value=%.4g\n",
            ix1, iy1, x1_user, y1_user, (double)value1);
*/

// Long format (needed when files have a small dynamical range !
     str_coordinates0->Printf(wxT("%.1f %.1f %.4g"), x1_user, y1_user, value1);
/* Short format:
     str_coordinates0->Printf(wxT("%.1f %.1f %.3g "), x1_user, y1_user, value1);
*/
/* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
*/
   } else if((Jgc0.gdev_graphic_type > 3) && (jlp_gsegraf1 != NULL)) {

// The image plot number was loaded in gseg_iplot_coords1
   if (Jgc0.gdev_graphic_type == 4 || Jgc0.gdev_graphic_type == 5) {
   status = jlp_gsegraf1->GSEG_UserFromDeviceCoord(idev_x0, idev_y0,
                                                   gseg_iplot_coords1,
                                                   &x1_user, &y1_user, &value1);
   } else {
   status = -1;
   }
   if(status == 0) {
     if(value1 == -1234.0) {
      str_coordinates0->Printf(wxT("x=%.5g y=%.5g"), x1_user, y1_user);
    } else if((x1_user != -1234.0) || (y1_user != -1234.0)){
     str_coordinates0->Printf(wxT("%.1f %.1f %.4g"), x1_user, y1_user, value1);
    }
    *in_frame = 1;
// If status==-1 (case of 3d plots for instance, fill the coordinates
// with zeroes) set in_frame to zero, to prevent interactive boxes to be used.
    } else {
     *in_frame = 0;
     str_coordinates0->Printf(wxT(""));
    }
   } else {
// Display x, y, values for jlp_splot curves
    ConvDevToUser(idev_x0, idev_y0, &x1_user, &y1_user, in_frame);
    if(*in_frame != 1) return(-1);
    str_coordinates0->Printf(wxT("x=%.5g y=%.5g"), x1_user, y1_user);
   }

return(0);
}
/***********************************************************************
*
***********************************************************************/
void JLP_GDev_wxWID::DisplayCoordinatesToScreen(wxDC *dc0,
                                                wxString str_coordinates0)
{
// Display coordinates on the screen
 if(m_StatusBar != NULL) {
// New version: write coordinates on the Status Bar (2nd column)
// Write to StatusBar (icol=0 or 1):
  m_StatusBar->SetStatusText(str_coordinates0, 1);
// Write coordinates (and help text) on top of the window or on StaticText label
  } else {
/* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
*/
    if(Jgc0.gdev_graphic_type == 3) {
      DisplayCoordinatesForImages(dc0, str_coordinates0);
     } else {
      DisplayCoordinatesForCurves(str_coordinates0);
     }
  }

return;
}
/***********************************************************************
* Set interactive internal processing mode (selected in the popup menu)
*
* 0=Automatic thresholds:
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
* 8=Slice: to compute the slice along a line
* that is interactively selected by the user
***********************************************************************/
int JLP_GDev_wxWID::SetInternalProcessingMode(int processing_mode)
{
if(m_gdproc1 == NULL) return(-1);

// Configure active processing mode to internal processing mode:
active_gdproc = m_gdproc1;

if(processing_mode < -1 || processing_mode > 20) {
  fprintf(stderr, "SetInternalProcessingMode/Error invalid value: pmode=%d\n",
           processing_mode);
  processing_mode = -1;
  }
#ifdef DEBUG
printf("SetInternalProcessingMode: pmode=%d\n", processing_mode);
#endif

// InternalProcessingMode
// -1=None 0=Automatic thresholds 1=Statistics, 2=Astrometry 3=Photometry
// 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
// 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
// 13=Add a ring 14=Remove a shape 15=Rotate a shape 16=Magnify a shape
// 17=Move a shape 18=ZoomIn 19=DisplayCoordinates 20=LabelContours

wxgdev_settings1.InternalProcessingMode = processing_mode;

// Input: InternalProcessingMode
m_gdproc1->SetNewProcessingMode(wxgdev_settings1.InternalProcessingMode);

// Reset point counters:
Mouse_EraseLeftDownPoints();
Mouse_EraseLeftUpPoints();

// Update help text on top of screen:
wxGdev_Refresh();

return(0);
}
/***********************************************************************
* Set interactive processing mode from external routine
* (called by Gdpisco, GdPanel)
*
* Input:
*  external_gproc : JLP_wx_GDProc class
***********************************************************************/
int JLP_GDev_wxWID::SetExternalProcessingMode(JLP_wx_GDProc *external_gdproc)
{

// Configure active processing mode to internal processing mode:
active_gdproc = external_gdproc;

// Disable m_gdproc1 internal interactive mode:
wxgdev_settings1.InternalProcessingMode = -1;
m_gdproc1->SetNewProcessingMode(wxgdev_settings1.InternalProcessingMode);

// Reset point counters:
Mouse_EraseLeftDownPoints();
Mouse_EraseLeftUpPoints();

// Refresh screen to update help text on top of screen
RedrawToBackupDC(5003);

return(0);
}
/***********************************************************************
*
***********************************************************************/
int JLP_GDev_wxWID::GetActiveExternalProcessingHelpText(wxString *str0,
                                                        int *ix0, int *iy0)
{
int status = -1;
// Location of help text for images:
  *ix0 = 152;
  *iy0 = 0;
  *str0 = wxT("");
  if(active_gdproc != NULL) {
   *str0 = active_gdproc->Get_HelpText();
   status = 0;
   }

return(status);
}
