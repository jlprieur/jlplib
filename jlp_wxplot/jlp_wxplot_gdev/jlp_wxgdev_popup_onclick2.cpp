/******************************************************************************
* jlp_wxgdev_onclick2.cpp
* JLP_wxGDev_Popup class
* Purpose:     Functions used by the popup menu when displaying 
*              a curve/image with wxwidgets
*
* Author:      JLP
* Version:     12/02/2017
******************************************************************************/
#include <stdio.h>
#include <stdlib.h>

#include "jlp_wxgdev_popup.h"
#include "jlp_wxgdev_popup_id.h"      // ID_INFO, ....
/****
#include "jlp_wxgdev_labels.h"
#include "jlp_wx_cursor.h"
#include "jlp_itt1.h"
#include "jlp_wxplot_frame.h"  // JLP_wxPlot_Frame
***/

/************************************************************************
* Display the current (double precision) image as a 3D plot 
* in a separate window
*
************************************************************************/
void JLP_wxGDev_Popup::OnDisplayDbleImage3D(wxCommandEvent &WXUNUSED(event) )
{
 if(initialized == 1234) 
    jlp_gdev_wxwid1->DisplayDbleImage3D();
}
/************************************************************************
* Display the current (double precision) image as a countour plot
* in a separate window
*
************************************************************************/
void JLP_wxGDev_Popup::OnDisplayDbleImageContours(wxCommandEvent 
                                                &WXUNUSED(event) )
{
 if(initialized == 1234) 
    jlp_gdev_wxwid1->DisplayDbleImageContours();
}
/************************************************************************
* Output curve as a JPEG, PNG, etc. file
************************************************************************/
void JLP_wxGDev_Popup::OnSave(wxCommandEvent &WXUNUSED(event) )
{
 if(initialized == 1234) 
    jlp_gdev_wxwid1->SaveGraphicToFile();
}
/************************************************************************
* Save the current bitmap to postscript file
*
************************************************************************/
void JLP_wxGDev_Popup::OnSaveToPostscript(wxCommandEvent &WXUNUSED(event) )
{
 if(initialized == 1234) 
    jlp_gdev_wxwid1->SaveGraphicToPostscriptFile();
}
/************************************************************************
* Change axis limits (for Gsegraf plots) 
************************************************************************/
void JLP_wxGDev_Popup::OnViewChangeAxisLimits(wxCommandEvent& WXUNUSED(event))
{
 if(initialized == 1234) 
    jlp_gdev_wxwid1->ViewChangeAxisLimits();
}
/************************************************************************
* Change axis projection of 3D plots (for Gsegraf plots)
************************************************************************/
void JLP_wxGDev_Popup::OnViewAxisRotate(wxCommandEvent& WXUNUSED(event))
{
int status;
 
if(initialized != 1234) return;

  status = jlp_gdev_wxwid1->ViewAxisRotate();
  if(status == -5) {
   wxMessageBox(wxT("Axis rotation is not available in this mode (only for 3D plots!)"),
                wxT("JLP_wxGDev_Popup::OnViewAxisRotate"), wxICON_ERROR);
   }

return;
}
/**************************************************************************
* OnSelectFilter
*
* 0=NONE
* 1=soft unsharp (UNSH1) 2=medium unsharp (UNSH2) 3=hard unsharp (UNSH3)
* 4=high contrast1 (VHC1) 5=high contrast2 (VHC2) 6=high contrast3 (VHC3)
* 7=high contrast4 (VHC4)
**************************************************************************/
void JLP_wxGDev_Popup::OnSelectFilter(wxCommandEvent& event)
{
wxGDev_SETTINGS wxgdev_settings0;
int filter0;

if(initialized != 1234) return;

// Get all current GDev settings (pen color, pen width, background color, etc):
 jlp_gdev_wxwid1->GDevGet_wxGDevSettings(&wxgdev_settings0);

 switch(event.GetId()){
   default:
   case ID_FILTER_0:
    filter0 = 0;
    break;
   case ID_FILTER_1:
    filter0 = 1;
    break;
   case ID_FILTER_2:
    filter0 = 2;
    break;
   case ID_FILTER_3:
    filter0 = 3;
    break;
   case ID_FILTER_4:
    filter0 = 4;
    break;
   case ID_FILTER_5:
    filter0 = 5;
    break;
   case ID_FILTER_6:
    filter0 = 6;
    break;
   case ID_FILTER_7:
    filter0 = 7;
    break;
  }

// Store input value to wxgdev SETTINGS:
  wxgdev_settings0.filter = filter0;

// Update new settings
 jlp_gdev_wxwid1->GDevLoad_wxGDevSettings(wxgdev_settings0);

// Update settings of JLP_GDev_wxWID:
 jlp_gdev_wxwid1->GDevUpdatePopupMenu_SelectFilter(filter0);

return;
}
/**********************************************************************
* Display a shape on the image
* The user will then position this label by
* clicking on the mouse (see OnLeftDown in "jlp_image_canvas.cpp")
**********************************************************************/
void JLP_wxGDev_Popup::OnAddShape(wxCommandEvent& event)
{
wxGDev_SETTINGS wxgdev_settings0;

if(initialized != 1234) return;

// Get all current GDev settings (pen color, pen width, background color, etc):
 jlp_gdev_wxwid1->GDevGet_wxGDevSettings(&wxgdev_settings0);

// InternalProcessingMode
// -1=None 0=Automatic thresholds 1=Statistics, 2=Astrometry 3=Photometry
// 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
// 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
// 13=Add a ring 14=Remove a shape 15=Rotate a shape 16=Magnify a shape
 switch(event.GetId()) {
   case ID_ADD_LINE:
    wxgdev_settings0.InternalProcessingMode = 9;
    break;
   case ID_ADD_RECTANGLE:
    wxgdev_settings0.InternalProcessingMode = 10;
    break;
   case ID_ADD_CIRCLE:
    wxgdev_settings0.InternalProcessingMode = 11;
    break;
   case ID_ADD_ELLIPSE:
    wxgdev_settings0.InternalProcessingMode = 12;
    break;
   case ID_ADD_RING:
    wxgdev_settings0.InternalProcessingMode = 13;
    break;
  }

// Update settings of JLP_GDev_wxWID:
 jlp_gdev_wxwid1->GDevLoad_wxGDevSettings(wxgdev_settings0);

// Update popup menu and apply new processing mode to jlp_gdev_wxwid1
 UpdatePopupMenu_InternalProcessingMode();

return;
}
/**********************************************************************
* Change/remove a shape from the image (after selecting it by clicking on it)
**********************************************************************/
void JLP_wxGDev_Popup::OnChangeShape(wxCommandEvent& event)
{
wxGDev_SETTINGS wxgdev_settings0;

if(initialized != 1234) return;

// Get all current GDev settings (pen color, pen width, background color, etc):
 jlp_gdev_wxwid1->GDevGet_wxGDevSettings(&wxgdev_settings0);

/* InternalProcessingMode
* -1=None 0=Automatic thresholds 1=Statistics, 2=Astrometry 3=Photometry
* 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
* 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
* 13=Add a ring 14=Remove a shape 15=Rotate a shape 16=Magnify a shape
* 17=Move a shape
*/
 switch(event.GetId()) {
// Remove a shape
  case ID_REM_SHAPE:
    wxgdev_settings0.InternalProcessingMode = 14;
    break;
// Rotate a shape
  case ID_ROTATE_SHAPE:
    wxgdev_settings0.InternalProcessingMode = 15;
    break;
// Magnify a shape
  case ID_MAGNIFY_SHAPE:
    wxgdev_settings0.InternalProcessingMode = 16;
    break;
// Move a shape
  case ID_MOVE_SHAPE:
    wxgdev_settings0.InternalProcessingMode = 17;
    break;
// Remove the last shape
  case ID_CANCEL_SHAPE:
    jlp_gdev_wxwid1->CancelLastShape();
    break;
// Remove all the shapes
  case ID_REM_ALL_SHAPES:
     jlp_gdev_wxwid1->EraseAllShapes();
    break;
  }

// Update settings of JLP_wxGDev_Popup:
 jlp_gdev_wxwid1->GDevLoad_wxGDevSettings(wxgdev_settings0);

// Update popup menu and apply new processing mode to jlp_gdev_wxwid1
 UpdatePopupMenu_InternalProcessingMode();
}
/**********************************************************************
* Display a label on the image
* The user will then position this label by
* clicking on the mouse (see OnLeftDown in "jlp_image_canvas.cpp")
**********************************************************************/
// Image labels:
void JLP_wxGDev_Popup::OnAddLabel(wxCommandEvent& WXUNUSED(event))
{
 if(initialized == 1234) 
   jlp_gdev_wxwid1->GDevAddLabel();
}
/**********************************************************************
* Remove the last entered label from the image
**********************************************************************/
void JLP_wxGDev_Popup::OnRemoveLabel(wxCommandEvent& WXUNUSED(event))
{
wxGDev_SETTINGS wxgdev_settings0;

if(initialized != 1234) return;

// Get all current GDev settings (pen color, pen width, background color, etc):
 jlp_gdev_wxwid1->GDevGet_wxGDevSettings(&wxgdev_settings0);

// InternalProcessingMode
// -1=None 0=Automatic thresholds 1=Statistics, 2=Astrometry 3=Photometry
// 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
// 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
// 13=Add a ring 14=Remove a shape
 wxgdev_settings0.InternalProcessingMode = 5;

// Update settings of JLP_GDev_wxWID:
 jlp_gdev_wxwid1->GDevLoad_wxGDevSettings(wxgdev_settings0);

// Update popup menu and apply new processing mode to jlp_gdev_wxwid1
 UpdatePopupMenu_InternalProcessingMode();
}
/**********************************************************************
* Display a scale bar on the image
**********************************************************************/
void JLP_wxGDev_Popup::OnAddScaleBar(wxCommandEvent& WXUNUSED(event))
{
 if(initialized == 1234) 
   jlp_gdev_wxwid1->GDevAddScaleBar();
}
/**********************************************************************
* Display North-East labels on the image
**********************************************************************/
void JLP_wxGDev_Popup::OnAddNorthEastLabel(wxCommandEvent& WXUNUSED(event))
{
 if(initialized == 1234) 
   jlp_gdev_wxwid1->GDevAddNorthEastLabel();
}
/**********************************************************************
* Remove the scale bar from the image
**********************************************************************/
void JLP_wxGDev_Popup::OnRemoveScaleBar(wxCommandEvent& WXUNUSED(event))
{
 if(initialized == 1234) 
    jlp_gdev_wxwid1->RemoveScaleBar();
}
/**********************************************************************
* Remove the North-East label from the image
**********************************************************************/
void JLP_wxGDev_Popup::OnRemoveNorthEastLabel(wxCommandEvent& WXUNUSED(event))
{
 if(initialized == 1234) 
    jlp_gdev_wxwid1->RemoveNorthEastLabel();
}
/**********************************************************************
* Handle label contours popup menu item 
* (Toggle processing mode to -1 if button selected twice)
*
* Display labels on a contour of the image
* The user position those labels by
* clicking on the mouse at the wanted location 
*
**********************************************************************/
void JLP_wxGDev_Popup::OnSetLabelContours(wxCommandEvent& WXUNUSED(event))
{
 if(initialized == 1234) 
    jlp_gdev_wxwid1->GDevSetLabelContours();
}
