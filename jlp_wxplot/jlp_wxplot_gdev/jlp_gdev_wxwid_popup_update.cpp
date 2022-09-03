/******************************************************************************
* jlp_gdev_wxwid_popup_update.cpp
* JLP_GDev_wxWID class
* Purpose:     Functions called by the popup menu
*
* Author:      JLP
* Version:     12/09/2017
******************************************************************************/
#include <stdio.h>
#include <stdlib.h>

#include "jlp_gdev_wxwid.h"
#include "jlp_wxgdev_labels.h"
#include "jlp_wxgdev_shapes.h"
#include "jlp_wx_cursor.h"
#include "jlp_itt1.h"

/**************************************************************************
*
* INPUT
* cursor_type: CrossHair1, CrossHair, DownArrow, Arrow, BigCross
*
**************************************************************************/
void JLP_GDev_wxWID::ApplyCursorSettings(wxGDev_SETTINGS wxgdev_settings0)
{
wxCursor my_cursor;
wxString cursor_type0; // Output from jlp_SetCursor: not used

// Refresh screen (necessary if crosshair cursor was set,
// to erase the crosshair)
wxGdev_Refresh();

// Copy wxGDev_SETTINGS to private settings (i.e., wxgdev_seetings1):
 GDevLoad_wxGDevSettings(wxgdev_settings0);

/* Cursor types: Arrow, DownArrow, Cross, CrossHair, CrossHair1
*/
 if(wxgdev_settings1.cursor_type.Cmp(wxT("CrossHair1")) == 0) {
   jlp_SetCrossHairCursor1(my_cursor, cursor_type0);
 } else if (wxgdev_settings1.cursor_type.Cmp(wxT("CrossHair")) == 0) {
   jlp_SetCrossHairCursor(my_cursor, cursor_type0);
 } else if (!wxgdev_settings1.cursor_type.Cmp(wxT("DownArrow"))) {
   jlp_SetDownArrowCursor(my_cursor, wxgdev_settings1.pen_colour, cursor_type0);
 } else if (!wxgdev_settings1.cursor_type.Cmp(wxT("Arrow"))) {
   jlp_SetArrowCursor(my_cursor, cursor_type0);
 } else if (!wxgdev_settings1.cursor_type.Cmp(wxT("BigCross"))) {
   jlp_SetCrossCursor(my_cursor, wxgdev_settings1.pen_colour,
                      cursor_type0, 64);
// Default (=Cross 32)
 } else {
   jlp_SetCrossCursor(my_cursor, wxgdev_settings1.pen_colour, cursor_type0, 32);
 }

// Update cursor:
 SetCursor(my_cursor);
 wxGdev_Refresh();

return;
}
/**************************************************************************
* Update LUT from c_image1
* and store it to wxgdev_settings1.lut_type and wxgdev_settings1.lut_reversed
*
* lut_type: 'l'=log rainbow1 'r'=rainbow2 's'=saw 'g'=gray 'c'=curves
*           'p'=pisco
* ID for LUT: RAIN1, RAIN2, SAW, GRAY, CUR, PISCO, REV
*
**************************************************************************/
void JLP_GDev_wxWID::ApplyLUTSettings(wxGDev_SETTINGS wxgdev_settings0,
                                      const int update_display)
{
 if(c_image1 == NULL || initialized != 1234) return;

// Copy wxGDev_SETTINGS:
 GDevLoad_wxGDevSettings(wxgdev_settings0);

if(update_display == 1) {
// Apply current settings (LUT, ITT, etc) to c_image1
  ApplyCurrentSettingsToCImage1();
  RedrawToBackupDC(4006);
  }

return;
}
/**************************************************************************
* Update gsegraf choice according to the values of
* wxgdev_settings1.gseg_phi
* wxgdev_settings1.gseg_theta, ...
*
**************************************************************************/
void JLP_GDev_wxWID::ApplyGsegrafSettings(wxGDev_SETTINGS wxgdev_settings0,
                                          const int update_display)
{

if(initialized != 1234) return;

// Copy wxGDev_SETTINGS:
 GDevLoad_wxGDevSettings(wxgdev_settings0);

// Apply those changes if needed
  if(update_display == 1) {
      RedrawToBackupDC(4007);
   }

return;
}
/**************************************************************************
* Update ITT choice from value obtained from c_image1
* and store it to wxgdev_settings1.itt_type
*
* ITT_thresh: Threshold determination
* "FromBox" "DirectInput" "MinMax" "Median" "HC" (high contrast)
* or "VHC" (very high contrast)
*
**************************************************************************/
void JLP_GDev_wxWID::ApplyITTSettings(wxGDev_SETTINGS wxgdev_settings0,
                                      const int update_display)
{
wxString itt_mode_str;
double lower_itt, upper_itt;
int ix1, ix2, iy1, iy2, status;

 if(c_image1 == NULL || initialized != 1234) return;

// Copy wxGDev_SETTINGS:
 GDevLoad_wxGDevSettings(wxgdev_settings0);

  itt_mode_str = wxgdev_settings1.itt_type;
  lower_itt = wxgdev_settings1.low_itt_thresh;
  upper_itt = wxgdev_settings1.up_itt_thresh;
  ix1 = wxgdev_settings1.itt_x1_box;
  iy1 = wxgdev_settings1.itt_y1_box;
  ix2 = wxgdev_settings1.itt_x2_box;
  iy2 = wxgdev_settings1.itt_y2_box;

  if(c_image1 != NULL) {
    status = c_image1->SetITT_thresh(itt_mode_str, lower_itt, upper_itt,
                                     ix1, iy1, ix2, iy2);
    }

if(status == 0 && update_display == 1) {
// Apply current settings (LUT, ITT, etc) to c_image1
  ApplyCurrentSettingsToCImage1();
  RedrawToBackupDC(4008);
  }
return;
}
/**************************************************************************
* Pen colour
*
**************************************************************************/
void JLP_GDev_wxWID::ApplyPenColourSettings(wxGDev_SETTINGS wxgdev_settings0,
                                      UINT32 canvas_fg_color0,
                                      const int update_display)
{
UINT32 canvas_bg_color0;

if(initialized != 1234) return;

// Copy wxGDev_SETTINGS:
 GDevLoad_wxGDevSettings(wxgdev_settings0);

// For jlp_splot/curves and jlp_splot/images:
  if((Jgc0.gdev_graphic_type >= 1) && (Jgc0.gdev_graphic_type <= 3)) {
// Apply changes
       SetPenColour_dc(wxgdev_settings1.pen_colour);
    } else {
// gsegraf:
// Foreground and background colors: canvas_fg_color, canvas_bg_color
      if(jlp_gsegraf1 != NULL) {
      jlp_gsegraf1->Get_canvas_bg_color(&canvas_bg_color0);
      jlp_gsegraf1->SetCanvasColor(canvas_fg_color0, canvas_bg_color0);
      }
    }

// Apply those changes if needed
  if(update_display == 1) {
      RedrawToBackupDC(4001);
   }

return;
}
/**************************************************************************
* Background colour (for curves)
*
**************************************************************************/
void JLP_GDev_wxWID::ApplyBackgroundColourSettings(wxGDev_SETTINGS wxgdev_settings0,
                                                   UINT32 canvas_bg_color0,
                                                   const int update_display)
{
UINT32 canvas_fg_color0;

if(initialized != 1234) return;

// Copy settings to wxGDev_SETTINGS (i.e. to wxgdev_settings1):
 GDevLoad_wxGDevSettings(wxgdev_settings0);

// Make this change to all plots (even for gsegraf):
    SetBackgdColour_dc(wxgdev_settings1.backgd_colour);

// For gsegraf, change also the background color (for the legend for instance):
// Foreground and background colors: canvas_fg_color, canvas_bg_color
    if(jlp_gsegraf1 != NULL) {
      jlp_gsegraf1->Get_canvas_fg_color(&canvas_fg_color0);
      jlp_gsegraf1->SetCanvasColor(canvas_fg_color0, canvas_bg_color0);
    }

// Apply all changes
  if(update_display == 1) {
      RedrawToBackupDC(4002);
  }

return;
}
/**************************************************************************
* Box type and various box options
*
**************************************************************************/
void JLP_GDev_wxWID::ApplyBoxTypeSettings(wxGDev_SETTINGS wxgdev_settings0,
                                          const int update_display)
{
if(initialized != 1234) return;

// Copy wxGDev_SETTINGS:
 GDevLoad_wxGDevSettings(wxgdev_settings0);

// Apply changes (box_type, box_xgrid, etc...)
 Update_JGC_from_GDProcSettings();

// Apply those changes if needed:
 if(update_display == 1) {
  RedrawToBackupDC(4003);
  }

return;
}
/**************************************************************************
* Update InternalProcessingMode
*
*
* InternalProcessingMode
* -1=None 0=Automatic thresholds 1=Statistics, 2=Astrometry 3=Photometry
* 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
* 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
* 13=Add a ring 14=Remove a shape 15=Rotate a shape 16=Magnify a shape
* 17=Move a shape 18=ZoomIn 19=DisplayCoordinates 20=LabelContours
*
**************************************************************************/
void JLP_GDev_wxWID::ApplyInternalProcessingModeSettings(wxGDev_SETTINGS wxgdev_settings0)
{
if(initialized != 1234) return;

// Copy wxGDev_SETTINGS:
 GDevLoad_wxGDevSettings(wxgdev_settings0);

// Apply settings:
 SetInternalProcessingMode(wxgdev_settings1.InternalProcessingMode);

return;
}
/************************************************************************
* Prompt the user for new ITT thresholds
*
* OUTPUT:
* Low_Threshold,Up_Threshold chosen by the user
************************************************************************/
void JLP_GDev_wxWID::PromptForNewThresholds()
{
wxString s_values, s_question, itt_type0;
double wlow = 0., wup = 0.;
int x1_box0, y1_box0, x2_box0, y2_box0;

if(c_image1 != NULL) {
  c_image1->GetITT_Thresh(&itt_type0, &wlow, &wup,
                    &x1_box0, &y1_box0, &x2_box0, &y2_box0);
  wxgdev_settings1.low_itt_thresh = wlow;
  wxgdev_settings1.up_itt_thresh = wup;
  }

s_question.Printf(
wxT("Current thresholds are: %.4g %.4g \n Enter new values (CR to cancel):"),
                  wxgdev_settings1.low_itt_thresh,
                  wxgdev_settings1.up_itt_thresh);

s_values.Printf(wxT("%.4g %.4g"), wxgdev_settings1.low_itt_thresh,
                  wxgdev_settings1.up_itt_thresh);

wxString result = wxGetTextFromUser(s_question, _T("New thresholds:"),
                                   s_values, NULL);
if (!result.IsEmpty()){
  if(sscanf(result.char_str(), "%lf %lf", &wlow, &wup) == 2) {
     GDevSet_ITT_Thresh(wxT("DirectInput"), wlow, wup, 0., 0., 0., 0.);
     }
  }

return;
}
/************************************************************************
* Set new ITT thresholds
* Thresholds are only used for "DirectInput"
*
* Called by jlp_igdev_proc1.cpp and PromptForNewThresholds()
*
* INPUT:
* itt_mode_str : selection mode (e.g. "DirectInput" or "FromBox")
* lower_itt, upper_itt: thresholds chosen by the user
*
************************************************************************/
int JLP_GDev_wxWID::GDevSet_ITT_Thresh(wxString itt_mode_str,
                                       const double lower_itt,
                                       const double upper_itt,
                                       const int ix1, const int iy1,
                                       const int ix2, const int iy2)
{
int status = -1;

  wxgdev_settings1.itt_type = itt_mode_str;
  wxgdev_settings1.low_itt_thresh = lower_itt;
  wxgdev_settings1.up_itt_thresh = upper_itt;
  wxgdev_settings1.itt_x1_box = ix1;
  wxgdev_settings1.itt_y1_box = iy1;
  wxgdev_settings1.itt_x2_box = ix2;
  wxgdev_settings1.itt_y2_box = iy2;

  if(c_image1 != NULL) {
    status = c_image1->SetITT_thresh(itt_mode_str, lower_itt, upper_itt,
                                     ix1, iy1, ix2, iy2);
    }

return(status);
}
