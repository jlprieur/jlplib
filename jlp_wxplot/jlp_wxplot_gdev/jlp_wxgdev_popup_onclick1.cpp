/******************************************************************************
* jlp_wxgdev_onclick.cpp
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

/************************************************************************
* Set the canvas cursor to Cross Hair, Cross Hair with a cross, etc.
*
* (Radio-Buttons)
************************************************************************/
void JLP_wxGDev_Popup::OnSetCursor(wxCommandEvent& event)
{

if(initialized != 1234) return;

  switch (event.GetId())
  {
// Set the canvas cursor to Cross Hair
   case ID_CURSOR_CROSSHAIR:
     wxgdev_settings1.cursor_type = wxT("CrossHair");
     break;
// Set the canvas cursor to Cross Hair with a cross in the center
// (necessary when crosshair is not well refreshed)
   case ID_CURSOR_CROSSHAIR1:
     wxgdev_settings1.cursor_type = wxT("CrossHair1");
     break;
// Set the canvas cursor to down arrow (with bitmap)
   case ID_CURSOR_DOWN_ARROW:
     wxgdev_settings1.cursor_type = wxT("DownArrow");
     break;
// Set the canvas cursor to Arrow
   case ID_CURSOR_ARROW:
     wxgdev_settings1.cursor_type = wxT("Arrow");
     break;
// Set the canvas cursor to BigCross
   case ID_CURSOR_BIG_CROSS:
     wxgdev_settings1.cursor_type = wxT("BigCross");
     break;
// Set the canvas cursor to Cross
   default:
   case ID_CURSOR_CROSS:
     wxgdev_settings1.cursor_type = wxT("Cross");
     break;
  }

// Update the popup menu and settings of JLP_GDev_wxWID
UpdatePopupMenu_Cursor();

return;
}
/************************************************************************
* Set the canvas pen color to black, white, etc
************************************************************************/
void JLP_wxGDev_Popup::OnSetPenColour(wxCommandEvent& event)
{
int update_display = 1;

if(initialized != 1234) return;

  switch (event.GetId())
  {
   default:
   case ID_BLACK_PEN_COLOUR:
     wxgdev_settings1.pen_default_colour = *wxBLACK;
     break;
   case ID_RED_PEN_COLOUR:
     wxgdev_settings1.pen_default_colour = *wxRED;
     break;
   case ID_GREEN_PEN_COLOUR:
     wxgdev_settings1.pen_default_colour = *wxGREEN;
     break;
   case ID_BLUE_PEN_COLOUR:
     wxgdev_settings1.pen_default_colour = *wxBLUE;
     break;
   case ID_WHITE_PEN_COLOUR:
     wxgdev_settings1.pen_default_colour = *wxWHITE;
     break;
  }

 wxgdev_settings1.pen_colour = wxgdev_settings1.pen_default_colour;

// Update the popup menu and settings of JLP_GDev_wxWID
 UpdatePopupMenu_PenColour(update_display);

return;
}
/************************************************************************
* Set the canvas background color to black, white, etc
************************************************************************/
void JLP_wxGDev_Popup::OnSetBackgdColour(wxCommandEvent& event)
{
int update_display = 1;

if(initialized != 1234) return;

  switch (event.GetId())
  {
   case ID_BLACK_BACK_COLOUR:
     wxgdev_settings1.backgd_colour = *wxBLACK;
     break;
   case ID_YELLOW_BACK_COLOUR:
     wxgdev_settings1.backgd_colour = *wxYELLOW;
     break;
   case ID_GREY_BACK_COLOUR:
     wxgdev_settings1.backgd_colour = *wxLIGHT_GREY;
     break;
   case ID_WHITE_BACK_COLOUR:
     wxgdev_settings1.backgd_colour = *wxWHITE;
     break;
  }

// Update the popup menu and settings of JLP_GDev_wxWID
 UpdatePopupMenu_BackgroundColour(update_display);

return;
}
/************************************************************************
* Set the box type and other box options
*
* (Radio-Buttons)
************************************************************************/
void JLP_wxGDev_Popup::OnSetBoxType(wxCommandEvent& event)
{
int update_display = 1;

if(initialized != 1234) return;

  switch (event.GetId())
  {
// MGO axes:
   default:
   case ID_BOX_TYPE0:
     wxgdev_settings1.box_type = 0;
     break;
// JLP axes:
   case ID_BOX_TYPE1:
     wxgdev_settings1.box_type = 1;
     break;
// GSEG axes:
   case ID_BOX_TYPE2:
     wxgdev_settings1.box_type = 2;
     break;
// Toggle ticks_in:
   case ID_BOX_TICKS_IN:
     wxgdev_settings1.ticks_in = 1 - wxgdev_settings1.ticks_in;
     break;
// Toggle x grid:
   case ID_BOX_XGRID:
     wxgdev_settings1.xgrid = 1 - wxgdev_settings1.xgrid;
     break;
// Toggle y grid:
   case ID_BOX_YGRID:
     wxgdev_settings1.ygrid = 1 - wxgdev_settings1.ygrid;
     break;
  }

// Update the popup menu and settings of JLP_GDev_wxWID
UpdatePopupMenu_BoxType(update_display);

return;
}
/************************************************************************
* Prompt the user for new Box Limits
*
* ID_BOX_LIMITS_AUTO,
* ID_BOX_LIMITS_MANUAL,
* ID_BOX_LIMITS_WITH_BOX,
************************************************************************/
void JLP_wxGDev_Popup::OnChangeBoxLimits(wxCommandEvent& event)
{

if(initialized != 1234) return;

// Possible problem avoided here!
// (i.e. conflict between "Statistics" and "FromBox")
// This event can be generated by PopupMenuEraseCheckBoxes():
// so I found a wayout to prevent loosing processing selection:
  if(software_event1){
   software_event1 = 0;
   return;
   }

// Stop interactive ITT threshold selection if no box selection:
 if((wxgdev_settings1.InternalProcessingMode == 0)
       && (event.GetId() != ID_BOX_LIMITS_WITH_BOX)) {
    wxgdev_settings1.InternalProcessingMode = -1;
    }

switch (event.GetId())
  {
  default:
  case ID_BOX_LIMITS_WITH_BOX:
// ProcessingMode=0 for interactive selection of box limits
    wxgdev_settings1.InternalProcessingMode = 0;
  case ID_BOX_LIMITS_AUTO:
    jlp_gdev_wxwid1->Curves_BoxLimitsAuto();
    break;
  case ID_BOX_LIMITS_MANUAL:
    if(gdev_graphic_type1 <= 3)
      jlp_gdev_wxwid1->BoxLimitsPrompt();
    else
      jlp_gdev_wxwid1->ViewChangeAxisLimits();
    break;
  }

// Update the popup menu and settings of JLP_GDev_wxWID
UpdatePopupMenu(wxgdev_settings1);

// Draw the graph again:
jlp_gdev_wxwid1->RedrawToBackupDC(4004);

return;
}
/************************************************************************
* Change LUT
* lut_type: 'l'=log rainbow1 'r'=rainbow2 's'=saw 'g'=gray 'c'=curves
*           'p'=pisco
*
* NB: wxWidgets automatically toggles the flag value when the item is clicked
************************************************************************/
void JLP_wxGDev_Popup::OnChangeLUT(wxCommandEvent& event)
{
int update_display = 1;

if(initialized != 1234) return;

  switch (event.GetId())
  {
// Rainbow1:
   case ID_LUT_RAIN1:
     strcpy(wxgdev_settings1.lut_type,"log rainbow1");
     break;
// Rainbow2:
   case ID_LUT_RAIN2:
     strcpy(wxgdev_settings1.lut_type,"rainbow2");
     break;
// Saw:
   case ID_LUT_SAW:
     strcpy(wxgdev_settings1.lut_type,"saw");
     break;
// Gray:
   case ID_LUT_GRAY:
     strcpy(wxgdev_settings1.lut_type,"gray");
     break;
// For curves:
   case ID_LUT_CUR:
     strcpy(wxgdev_settings1.lut_type,"curves");
     break;
// Pisco-like:
   default:
   case ID_LUT_PISCO:
     strcpy(wxgdev_settings1.lut_type,"pisco");
     break;
// Reverse LUT:
   case ID_LUT_REV:
     wxgdev_settings1.lut_reversed = 1 - wxgdev_settings1.lut_reversed;
     break;
   }

// Apply changes to JLP_GDev_wxWID object:
jlp_gdev_wxwid1->ApplyLUTSettings(wxgdev_settings1, update_display);

return;
}
/************************************************************************
* Change Zoom factor
*
* NB: wxWidgets automatically toggles the flag value when the item is clicked
************************************************************************/
void JLP_wxGDev_Popup::OnChangeZoom(wxCommandEvent& event)
{
int update_display = 1;

if(initialized != 1234) return;

  switch (event.GetId())
  {
// zoom_fact = -2:
   case ID_ZOOM_C2:
     wxgdev_settings1.zoom = -2;
     break;
// zoom_fact = -3:
   case ID_ZOOM_C3:
     wxgdev_settings1.zoom = -3;
     break;
// zoom_fact = -4:
   case ID_ZOOM_C4:
     wxgdev_settings1.zoom = -4;
     break;
// zoom_fact = -5:
   case ID_ZOOM_C5:
     wxgdev_settings1.zoom = -5;
     break;
// zoom_fact = -6:
   case ID_ZOOM_C6:
     wxgdev_settings1.zoom = -6;
     break;
// zoom_fact = -7:
   case ID_ZOOM_C7:
     wxgdev_settings1.zoom = -7;
     break;
// zoom_fact = -8:
   case ID_ZOOM_C8:
     wxgdev_settings1.zoom = -8;
     break;
// zoom_fact = 1:
   case ID_ZOOM1:
     wxgdev_settings1.zoom = 1;
     break;
// zoom_fact = 2:
   case ID_ZOOM2:
     wxgdev_settings1.zoom = 2;
     break;
// zoom_fact = 3:
   case ID_ZOOM3:
     wxgdev_settings1.zoom = 3;
     break;
// zoom_fact = 4:
   case ID_ZOOM4:
     wxgdev_settings1.zoom = 4;
     break;
// zoom_fact = 5:
   case ID_ZOOM5:
     wxgdev_settings1.zoom = 5;
     break;
// zoom_fact = 6:
   case ID_ZOOM6:
     wxgdev_settings1.zoom = 6;
     break;
// zoom_fact = 8:
   case ID_ZOOM8:
     wxgdev_settings1.zoom = 8;
     break;
// zoom_fact = 10:
   case ID_ZOOM10:
     wxgdev_settings1.zoom = 10;
     break;
// zoom_fact = 15:
   case ID_ZOOM15:
     wxgdev_settings1.zoom = 15;
     break;
// zoom_fact = 20:
   case ID_ZOOM20:
     wxgdev_settings1.zoom = 20;
     break;
// zoom_fact = 40:
   case ID_ZOOM40:
     wxgdev_settings1.zoom = 40;
     break;
  }

// Apply changes to JLP_GDev_wxWID object:
jlp_gdev_wxwid1->ApplyZoomSettings(wxgdev_settings1, update_display);

// Update the popup menu (after applying the zoom factor):
 UpdatePopupMenu_Zoom();

return;
}
/************************************************************************
* Change the type of the ITT (Intensity Transfer Table used for the display)
*
* ITT_is_linear:
*         "1=Lin" (Linear scale)
*         "0=Log" (Log scale)
*
* This table is filled with the indices k to be used for LUT conversion
* of a double precision image
************************************************************************/
void JLP_wxGDev_Popup::OnChangeITT_is_linear(wxCommandEvent& event)
{
int update_display = 1;

if(initialized != 1234) return;

  switch (event.GetId())
  {
// Switch to linear scale:
   case ID_ITT_LIN:
     jlp_gdev_wxwid1->GDevSetITT_linear(1);
     break;
// Switch to logarithmic scale
   case ID_ITT_LOG:
     jlp_gdev_wxwid1->GDevSetITT_linear(0);
     break;
  }

// Update the popup menu:
 UpdatePopupMenu_ITT(update_display);

 return;
}
/************************************************************************
* Change the threshold input mode of the ITT (Intensity Transfer Table
* used for the display)
*
* ITT_thresh: Threshold determination
* "FromBox" "DirectInput" "MinMax" "Median" "HC" (high contrast)
* or "VHC" (very high contrast)
************************************************************************/
void JLP_wxGDev_Popup::OnChangeITT_thresholds(wxCommandEvent& event)
{
int update_display = 1;

if(initialized != 1234) return;

// Possible problem avoided here!
// (i.e. conflict between "Statistics" and "FromBox")
// This event can be generated by PopupMenuEraseCheckBoxes():
// so I found a wayout to prevent loosing processing selection:
  if(software_event1){
   software_event1 = 0;
   return;
   }

// Stop interactive ITT threshold selection if no box selection:
 if((wxgdev_settings1.InternalProcessingMode == 0)
       && (event.GetId() != ID_THR_BOX)) {
    wxgdev_settings1.InternalProcessingMode = -1;
// Update settings of JLP_GDev_wxWID:
    jlp_gdev_wxwid1->ApplyInternalProcessingModeSettings(wxgdev_settings1);
    }

// Then select required option:
  switch (event.GetId())
  {
// Change thresholds with a box selection
// with Low_Threshold and Up_Threshold computed in the box chosen by the user
   case ID_THR_BOX:
// ProcessingMode=0 when automatic thresholds are activated
     wxgdev_settings1.InternalProcessingMode = 0;
// Update settings of JLP_GDev_wxWID:
     jlp_gdev_wxwid1->ApplyInternalProcessingModeSettings(wxgdev_settings1);
// Do not set here to "FromBox"
// (it is done when the user has selected the box)
     break;
// Change thresholds manually
// with new Low_Threshold and Up_Threshold chosen by the user
   case ID_THR_DIRECT:
   case ID_THR_DIRECT1:
     jlp_gdev_wxwid1->PromptForNewThresholds();
// Do not set here to "DirectInput"
// (it is done in PrompForNewThresholds)
     break;
// Change thresholds to min-max ITT: dummy values of the thresholds here
// the good thresholds will be computed later:
// with Low_Threshold = min_value of dble_image0
// and Up_Threshold = max_value of dble_image0
   case ID_THR_AUTO_MINMAX:
     jlp_gdev_wxwid1->GDevSet_ITT_Thresh(wxT("MinMax"), 0., 0., 0., 0., 0., 0.);
     break;
// Change to Median ITT, with dummy threshold values
   case ID_THR_AUTO_MEDIAN:
     jlp_gdev_wxwid1->GDevSet_ITT_Thresh(wxT("Median"), 0., 0., 0., 0., 0., 0.);
     break;
// Change to High Contrast ITT, with dummy threshold values
   case ID_THR_AUTO_HC:
     jlp_gdev_wxwid1->GDevSet_ITT_Thresh(wxT("HC"), 0., 0., 0., 0., 0., 0.);
     break;
// Change to Very High Contrast ITT, with dummy threshold values
   case ID_THR_AUTO_VHC:
     jlp_gdev_wxwid1->GDevSet_ITT_Thresh(wxT("VHC"), 0., 0., 0., 0., 0., 0.);
     break;
 }

// Update the popup menu:
 UpdatePopupMenu_ITT(update_display);

return;
}
/************************************************************************
* Statistics: to display statistics about the displayed image
*   (within a box selected by the user)
* Astrometry: to compute the position of a pattern within a circle
*   (circle interactively selected by the user)
* Photometry: to compute the flux within a circle of the displayed image
*   (circle interactively selected by the user)
* Slice: to make a slice on the displayed image
* (line interactively selected by the user)
* GetCoordinates: to input a list of coordinates to the logbook
************************************************************************/
void JLP_wxGDev_Popup::OnSetInteractiveProcessing(wxCommandEvent& event)
{
// InternalProcessingMode
// -1=None 0=Automatic thresholds 1=Statistics, 2=Astrometry 3=Photometry
// 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
// 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
// 13=Add a ring 14=Remove a shape
  switch(event.GetId()) {
   case ID_STATISTICS:
// Toggle processing mode if button pressed twice :
     if(wxgdev_settings1.InternalProcessingMode == 1)
       wxgdev_settings1.InternalProcessingMode = -1;
     else
       wxgdev_settings1.InternalProcessingMode = 1;
     break;
   case ID_ASTROMETRY:
// Toggle processing mode if button pressed twice :
     if(wxgdev_settings1.InternalProcessingMode == 2)
       wxgdev_settings1.InternalProcessingMode = -1;
     else
       wxgdev_settings1.InternalProcessingMode = 2;
     break;
   case ID_PHOTOMETRY:
// Toggle processing mode if button pressed twice :
     if(wxgdev_settings1.InternalProcessingMode == 3)
       wxgdev_settings1.InternalProcessingMode = -1;
     else
       wxgdev_settings1.InternalProcessingMode = 3;
     break;
   case ID_SLICE:
// Toggle processing mode if button pressed twice :
     if(wxgdev_settings1.InternalProcessingMode == 8)
       wxgdev_settings1.InternalProcessingMode = -1;
     else
       wxgdev_settings1.InternalProcessingMode = 8;
     break;
   case ID_GET_COORDINATES:
// Toggle processing mode if button pressed twice :
     if(wxgdev_settings1.InternalProcessingMode == 19)
       wxgdev_settings1.InternalProcessingMode = -1;
     else
       wxgdev_settings1.InternalProcessingMode = 19;
     break;
  }

// Update settings of JLP_GDev_wxWID:
 jlp_gdev_wxwid1->ApplyInternalProcessingModeSettings(wxgdev_settings1);

return;
}
/************************************************************************
* InfoImage: to display information about current image
************************************************************************/
void JLP_wxGDev_Popup::InfoImage(wxCommandEvent& WXUNUSED(event))
{
 jlp_gdev_wxwid1->GDevDisplayInfoImage();
}
/************************************************************************
* InfoCurve: to display information about plotted curve(s)
************************************************************************/
void JLP_wxGDev_Popup::OnInfoCurve(wxCommandEvent& WXUNUSED(event))
{
 jlp_gdev_wxwid1->GDevDisplayInfoCurve();
}
/************************************************************************
* Copy to clipboard
************************************************************************/
void JLP_wxGDev_Popup::OnCopy(wxCommandEvent& WXUNUSED(event))
{
 jlp_gdev_wxwid1->GDevCopyToClipboard();
}
/************************************************************************
* Get image from clipboard
************************************************************************/
void JLP_wxGDev_Popup::OnPaste(wxCommandEvent& WXUNUSED(event))
{
 jlp_gdev_wxwid1->GDevPasteFromClipboard();
}
