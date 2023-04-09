/******************************************************************************
* jlp_wxgdev_popup_update.cpp
* JLP_wxGDev_Popup class
* Purpose:     Popup menu for displaying a curve with wxwidgets
*
* Author:      JLP
* Version:     12/02/2017
******************************************************************************/
#include <stdio.h>
#include <stdlib.h>

#include "jlp_wxgdev_popup.h"
#include "jlp_wxgdev_popup_id.h"      // ID_INFO, ....
/***
#include "jlp_wxgdev_labels.h"
#include "jlp_wxgdev_shapes.h"
#include "jlp_wx_cursor.h"
#include "jlp_itt1.h"
**/

/**************************************************************************
*
* INPUT
* cursor_type: CrossHair1, CrossHair, DownArrow, Arrow, BigCross
*
**************************************************************************/
void JLP_wxGDev_Popup::UpdatePopupMenu_Cursor()
{
wxCursor my_cursor;
wxString cursor_type0; // Output from jlp_SetCursor: not used
wxGDev_SETTINGS wxgdev_settings0;

if(initialized != 1234) return;

// Get all current GDev settings (pen color, pen width, background color, etc):
 jlp_gdev_wxwid1->GDevGet_wxGDevSettings(&wxgdev_settings0);

/* Cursor types: Arrow, DownArrow, Cross, CrossHair, CrossHair1
*/
 if(wxgdev_settings0.cursor_type.Cmp(wxT("CrossHair1")) == 0) {
   menuCursor->Check(ID_CURSOR_CROSSHAIR1, true);
 } else if (wxgdev_settings0.cursor_type.Cmp(wxT("CrossHair")) == 0) {
   menuCursor->Check(ID_CURSOR_CROSSHAIR, true);
 } else if (!wxgdev_settings0.cursor_type.Cmp(wxT("DownArrow"))) {
   menuCursor->Check(ID_CURSOR_DOWN_ARROW, true);
 } else if (!wxgdev_settings0.cursor_type.Cmp(wxT("Arrow"))) {
   menuCursor->Check(ID_CURSOR_ARROW, true);
 } else if (!wxgdev_settings0.cursor_type.Cmp(wxT("BigCross"))) {
   menuCursor->Check(ID_CURSOR_BIG_CROSS, true);
// Default (=Cross 32)
 } else {
   menuCursor->Check(ID_CURSOR_CROSS, true);
 }

jlp_gdev_wxwid1->ApplyCursorSettings(wxgdev_settings0);

return;
}
/**************************************************************************
* Update LUT from c_image1
*
* lut_type: 'l'=log rainbow1 'r'=rainbow2 's'=saw 'g'=gray 'c'=curves
*           'p'=pisco
* ID for LUT: RAIN1, RAIN2, SAW, GRAY, CUR, PISCO, REV
*
**************************************************************************/
void JLP_wxGDev_Popup::UpdatePopupMenu_LUT(const int update_display)
{
int status, lut_reversed0;
char lut_type0[64];
wxGDev_SETTINGS wxgdev_settings0;

 if(NULL || initialized != 1234) return;

// Get all current GDev settings (pen color, pen width, background color, etc):
 jlp_gdev_wxwid1->GDevGet_wxGDevSettings(&wxgdev_settings0);

// Inquire current LUT settings
 status = jlp_gdev_wxwid1->GetLUT_Settings(lut_type0, &lut_reversed0);
 if(status != 0) return;

// Save them to local variables:
 strcpy(wxgdev_settings0.lut_type, lut_type0);
 wxgdev_settings0.lut_reversed = lut_reversed0;

 switch (wxgdev_settings0.lut_type[0]) {
   case 'l':
    menuLUT->Check(ID_LUT_RAIN1, true);
    break;
   case 'r':
    menuLUT->Check(ID_LUT_RAIN2, true);
    break;
   case 's':
    menuLUT->Check(ID_LUT_SAW, true);
    break;
   case 'g':
    menuLUT->Check(ID_LUT_GRAY, true);
    break;
   case 'c':
    menuLUT->Check(ID_LUT_CUR, true);
    break;
   default:
   case 'p':
    menuLUT->Check(ID_LUT_PISCO, true);
    break;
    break;
   }

// Reversed LUT:
 if(wxgdev_settings0.lut_reversed == 0) {
   menuLUT->Check(ID_LUT_REV, false);
   } else {
   menuLUT->Check(ID_LUT_REV, true);
   }

// Update settings of JLP_GDev_wxWID:
jlp_gdev_wxwid1->ApplyLUTSettings(wxgdev_settings0, update_display);

return;
}
/**************************************************************************
* Update zoom choice from gamma1,gamma_d obtained from c_image1
* and store it to wxgdev_settings0.zoom
*
**************************************************************************/
void JLP_wxGDev_Popup::UpdatePopupMenu_Zoom()
{
int status, gamma1, gamma_d;
wxGDev_SETTINGS wxgdev_settings0;

 if(initialized != 1234) return;

// Get all current GDev settings (pen color, pen width, background color, etc):
 jlp_gdev_wxwid1->GDevGet_wxGDevSettings(&wxgdev_settings0);

// Inquire current gamma values
 status = jlp_gdev_wxwid1->GetZoomGammaValues(&gamma1, &gamma_d);
 if(status != 0) return;

  if(gamma_d > 1)
    wxgdev_settings0.zoom = gamma_d;
  else if(gamma1 > 1)
    wxgdev_settings0.zoom = -gamma1;
  else
    wxgdev_settings0.zoom = 1;

/************* It is better to leave all items accessible ....
if(wxgdev_settings0.zoom >= 1) {
  menuZoom->Enable(ID_ZOOM40, true);
  menuZoom->Enable(ID_ZOOM20, true);
  menuZoom->Enable(ID_ZOOM15, true);
  menuZoom->Enable(ID_ZOOM10, true);
  menuZoom->Enable(ID_ZOOM8, true);
  menuZoom->Enable(ID_ZOOM6, true);
  menuZoom->Enable(ID_ZOOM5, true);
  menuZoom->Enable(ID_ZOOM4, true);
  menuZoom->Enable(ID_ZOOM3, true);
  menuZoom->Enable(ID_ZOOM2, true);
  menuZoom->Enable(ID_ZOOM1, true);

  menuZoom->Enable(ID_ZOOM_C5, false);
  menuZoom->Enable(ID_ZOOM_C4, false);
  menuZoom->Enable(ID_ZOOM_C3, false);
  menuZoom->Enable(ID_ZOOM_C2, false);
  menuZoom->Enable(ID_ZOOM_C1, false);
} else {
  menuZoom->Enable(ID_ZOOM40, false);
  menuZoom->Enable(ID_ZOOM20, false);
  menuZoom->Enable(ID_ZOOM15, false);
  menuZoom->Enable(ID_ZOOM10, false);
  menuZoom->Enable(ID_ZOOM8, false);
  menuZoom->Enable(ID_ZOOM6, false);
  menuZoom->Enable(ID_ZOOM5, false);
  menuZoom->Enable(ID_ZOOM4, false);
  menuZoom->Enable(ID_ZOOM3, false);
  menuZoom->Enable(ID_ZOOM2, false);
  menuZoom->Enable(ID_ZOOM1, false);

  menuZoom->Enable(ID_ZOOM_C5, true);
  menuZoom->Enable(ID_ZOOM_C4, true);
  menuZoom->Enable(ID_ZOOM_C3, true);
  menuZoom->Enable(ID_ZOOM_C2, true);
  menuZoom->Enable(ID_ZOOM_C1, true);
}
*/

  switch (wxgdev_settings0.zoom) {
    case 40:
      menuZoom->Check(ID_ZOOM40, true);
      break;
    case 20:
      menuZoom->Check(ID_ZOOM20, true);
      break;
    case 15:
      menuZoom->Check(ID_ZOOM15, true);
      break;
    case 10:
      menuZoom->Check(ID_ZOOM10, true);
      break;
    case 8:
      menuZoom->Check(ID_ZOOM8, true);
      break;
    case 6:
      menuZoom->Check(ID_ZOOM6, true);
      break;
    case 5:
      menuZoom->Check(ID_ZOOM5, true);
      break;
    case 4:
      menuZoom->Check(ID_ZOOM4, true);
      break;
    case 3:
      menuZoom->Check(ID_ZOOM3, true);
      break;
    case 2:
      menuZoom->Check(ID_ZOOM2, true);
      break;
    default:
    case 1:
      menuZoom->Check(ID_ZOOM1, true);
      break;
    case -2:
      menuZoom->Check(ID_ZOOM_C2, true);
      break;
    case -3:
      menuZoom->Check(ID_ZOOM_C3, true);
      break;
    case -4:
      menuZoom->Check(ID_ZOOM_C4, true);
      break;
    case -5:
      menuZoom->Check(ID_ZOOM_C5, true);
      break;
    case -6:
      menuZoom->Check(ID_ZOOM_C6, true);
      break;
    case -7:
      menuZoom->Check(ID_ZOOM_C7, true);
      break;
    case -8:
      menuZoom->Check(ID_ZOOM_C8, true);
      break;
  }

// Update settings of JLP_GDev_wxWID:
jlp_gdev_wxwid1->GDevLoad_wxGDevSettings(wxgdev_settings0);

return;
}
/**************************************************************************
* Update filter choice according to the value of wxgdev_settings0.filter
*
**************************************************************************/
void JLP_wxGDev_Popup::UpdatePopupMenu_Filter()
{
wxGDev_SETTINGS wxgdev_settings0;

if(initialized != 1234 || menuFilter == NULL) return;

// Get all current GDev settings (pen color, pen width, background color, etc):
 jlp_gdev_wxwid1->GDevGet_wxGDevSettings(&wxgdev_settings0);

// First uncheck all options
/* NOT needed
  menuFilter->Check(ID_FILTER_0, false);
  menuFilter->Check(ID_FILTER_1, false);
  menuFilter->Check(ID_FILTER_2, false);
  menuFilter->Check(ID_FILTER_3, false);
  menuFilter->Check(ID_FILTER_4, false);
  menuFilter->Check(ID_FILTER_5, false);
  menuFilter->Check(ID_FILTER_6, false);
  menuFilter->Check(ID_FILTER_7, false);
  menuFilter->Check(ID_FILTER_8, false);
*/

// Then select good one:
  switch (wxgdev_settings0.filter) {
    default:
    case 0:
      if(menuFilter->FindItem(ID_FILTER_0) != NULL)
        menuFilter->Check(ID_FILTER_0, true);
      break;
    case 1:
      if(menuFilter->FindItem(ID_FILTER_1) != NULL)
        menuFilter->Check(ID_FILTER_1, true);
      break;
    case 2:
      if(menuFilter->FindItem(ID_FILTER_2) != NULL)
        menuFilter->Check(ID_FILTER_2, true);
      break;
    case 3:
      if(menuFilter->FindItem(ID_FILTER_3) != NULL)
        menuFilter->Check(ID_FILTER_3, true);
      break;
    case 4:
      if(menuFilter->FindItem(ID_FILTER_4) != NULL)
        menuFilter->Check(ID_FILTER_4, true);
      break;
    case 5:
      if(menuFilter->FindItem(ID_FILTER_5) != NULL)
        menuFilter->Check(ID_FILTER_5, true);
      break;
    case 6:
      if(menuFilter->FindItem(ID_FILTER_6) != NULL)
        menuFilter->Check(ID_FILTER_6, true);
      break;
    case 7:
      if(menuFilter->FindItem(ID_FILTER_7) != NULL)
        menuFilter->Check(ID_FILTER_7, true);
      break;
    case 8:
      if(menuFilter->FindItem(ID_FILTER_8) != NULL)
        menuFilter->Check(ID_FILTER_8, true);
      break;
  }

// Update settings of JLP_GDev_wxWID:
jlp_gdev_wxwid1->GDevLoad_wxGDevSettings(wxgdev_settings0);

return;
}
/**************************************************************************
* Update gsegraf choice according to the values of
* wxgdev_settings0.gseg_phi
* wxgdev_settings0.gseg_theta, ...
*
**************************************************************************/
void JLP_wxGDev_Popup::UpdatePopupMenu_Gsegraf(const int update_display)
{
wxGDev_SETTINGS wxgdev_settings0;

if(initialized != 1234 || menuGsegraf == NULL) return;

// Get all current GDev settings (pen color, pen width, background color, etc):
 jlp_gdev_wxwid1->GDevGet_wxGDevSettings(&wxgdev_settings0);

// Update settings of JLP_GDev_wxWID:
jlp_gdev_wxwid1->ApplyGsegrafSettings(wxgdev_settings0, update_display);

return;
}
/**************************************************************************
* Update ITT choice from value obtained from c_image1
* and store it to wxgdev_settings0.itt_type
*
* ITT_thresh: Threshold determination
* "FromBox" "DirectInput" "MinMax" "Median" "HC" (high contrast)
* or "VHC" (very high contrast)
*
**************************************************************************/
void JLP_wxGDev_Popup::UpdatePopupMenu_ITT(const int update_display)
{
wxGDev_SETTINGS wxgdev_settings0;
double low_threshold0, upper_threshold0;
int status, itt_is_linear0, box_x10, box_x20, box_y10, box_y20;
wxString itt_type0;
int wc2, wc3;

 if(initialized != 1234) return;

// Get all current GDev settings (pen color, pen width, background color, etc):
 jlp_gdev_wxwid1->GDevGet_wxGDevSettings(&wxgdev_settings0);

// Inquire current ITT thresholds
 status = jlp_gdev_wxwid1->GDevGet_ITT_Thresh(&itt_type0, &itt_is_linear0,
                                             &low_threshold0, &upper_threshold0,
                                             &box_x10, &box_y10, &box_x20, &box_y20);
 if(status != 0) return;

// Save them to private variables:
 wxgdev_settings0.itt_type = itt_type0;
 wxgdev_settings0.itt_is_linear = itt_is_linear0;
 wxgdev_settings0.low_itt_thresh = low_threshold0;
 wxgdev_settings0.up_itt_thresh = upper_threshold0;

 wc2 = (int)wxgdev_settings0.itt_type[0];
 wc3 = (int)wxgdev_settings0.itt_type[1];
 if(wxgdev_settings0.InternalProcessingMode == 0) wc2 = (int)'I';

// To avoid loose of processing selection (statistics, photometry, etc.)
 software_event1 = 1;
 switch (wc2) {
   default:
   case (int)('I'):
     if(menuITT2->FindItem(ID_THR_BOX) != NULL)
       menuITT2->Check(ID_THR_BOX, true);
     break;
   case (int)('D'):
     if(menuITT2->FindItem(ID_THR_DIRECT) != NULL)
       menuITT2->Check(ID_THR_DIRECT, true);
     break;
   case (int)('H'):
     if(menuITT2->FindItem(ID_THR_AUTO_HC) != NULL)
       menuITT2->Check(ID_THR_AUTO_HC, true);
     break;
// MinMax or Median:
   case (int)('M'):
     if(wc3 == (int)('e')) {
      if(menuITT2->FindItem(ID_THR_AUTO_MEDIAN) != NULL)
        menuITT2->Check(ID_THR_AUTO_MEDIAN, true);
      } else {
      if(menuITT2->FindItem(ID_THR_AUTO_MINMAX) != NULL)
        menuITT2->Check(ID_THR_AUTO_MINMAX, true);
      }
     break;
   case (int)('V'):
     if(menuITT2->FindItem(ID_THR_AUTO_VHC) != NULL)
       menuITT2->Check(ID_THR_AUTO_VHC, true);
     break;
  }
 software_event1 = 0;

// Update settings of JLP_GDev_wxWID:
jlp_gdev_wxwid1->ApplyITTSettings(wxgdev_settings0, update_display);

return;
}
/**************************************************************************
* Pen colour
*
**************************************************************************/
void JLP_wxGDev_Popup::UpdatePopupMenu_PenColour(const int update_display)
{
wxGDev_SETTINGS wxgdev_settings0;
UINT32 canvas_fg_color0;

if(initialized != 1234 || menuPen == NULL) return;

// Get all current GDev settings (pen color, pen width, background color, etc):
 jlp_gdev_wxwid1->GDevGet_wxGDevSettings(&wxgdev_settings0);

// Pen colour:
wxgdev_settings0.pen_colour = wxgdev_settings0.pen_default_colour;
 if(wxgdev_settings0.pen_colour == *wxWHITE){
   canvas_fg_color0 = 0xFFFFFFFF;
   if(menuPen->FindItem(ID_WHITE_PEN_COLOUR) != NULL)
      menuPen->Check(ID_WHITE_PEN_COLOUR, true);
 } else if(wxgdev_settings0.pen_colour == *wxRED){
// NB: DF instead of FF for a darker color...
   canvas_fg_color0 = 0xDF0000FF;
   if(menuPen->FindItem(ID_RED_PEN_COLOUR) != NULL)
      menuPen->Check(ID_RED_PEN_COLOUR, true);
 } else if(wxgdev_settings0.pen_colour == *wxGREEN){
// NB: DF instead of FF for a darker color...
   canvas_fg_color0 = 0x00DF00FF;
   if(menuPen->FindItem(ID_GREEN_PEN_COLOUR) != NULL)
      menuPen->Check(ID_GREEN_PEN_COLOUR, true);
 } else if(wxgdev_settings0.pen_colour == *wxBLUE){
// NB: DF instead of FF for a darker color...
   canvas_fg_color0 = 0x0000DFFF;
   if(menuPen->FindItem(ID_BLUE_PEN_COLOUR) != NULL)
      menuPen->Check(ID_BLUE_PEN_COLOUR, true);
// Default is black:
 } else {
   canvas_fg_color0 = 0x000000FF;
   if(menuPen->FindItem(ID_BLACK_PEN_COLOUR) != NULL)
      menuPen->Check(ID_BLACK_PEN_COLOUR, true);
   wxgdev_settings0.pen_colour = *wxBLACK;
 }

// Update cursor (since there may be some possible change
// of the coloured cursor)
 UpdatePopupMenu_Cursor();

// Update settings of JLP_GDev_wxWID:
 jlp_gdev_wxwid1->ApplyPenColourSettings(wxgdev_settings0, canvas_fg_color0, 
                                         update_display);

return;
}
/**************************************************************************
* Background colour (for curves)
*
**************************************************************************/
void JLP_wxGDev_Popup::UpdatePopupMenu_BackgroundColour(const int update_display)
{
wxGDev_SETTINGS wxgdev_settings0;
UINT32 canvas_bg_color0;

if(initialized != 1234 || menuBackgd == NULL) return;

// Get all current GDev settings (pen color, pen width, background color, etc):
 jlp_gdev_wxwid1->GDevGet_wxGDevSettings(&wxgdev_settings0);

// Background colour:
 if(wxgdev_settings0.backgd_colour == *wxBLACK){
   canvas_bg_color0 = 0x000000FF;
   if(menuBackgd->FindItem(ID_BLACK_BACK_COLOUR) != NULL)
      menuBackgd->Check(ID_BLACK_BACK_COLOUR, true);
 } else if(wxgdev_settings0.backgd_colour == *wxYELLOW){
   canvas_bg_color0 = 0xFFFF00FF;
   if(menuBackgd->FindItem(ID_YELLOW_BACK_COLOUR) != NULL)
      menuBackgd->Check(ID_YELLOW_BACK_COLOUR, true);
 } else if(wxgdev_settings0.backgd_colour == *wxLIGHT_GREY){
   canvas_bg_color0 = 0xDDDDDDFF;
   if(menuBackgd->FindItem(ID_GREY_BACK_COLOUR) != NULL)
      menuBackgd->Check(ID_GREY_BACK_COLOUR, true);
// Default is white:
 } else {
   canvas_bg_color0 = 0xFFFFFFFF;
   if(menuBackgd->FindItem(ID_WHITE_BACK_COLOUR) != NULL)
      menuBackgd->Check(ID_WHITE_BACK_COLOUR, true);
   wxgdev_settings0.backgd_colour = *wxWHITE;
 }

// Update settings of JLP_GDev_wxWID:
 jlp_gdev_wxwid1->ApplyBackgroundColourSettings(wxgdev_settings0,
                                                canvas_bg_color0,
                                                update_display);

return;
}
/**************************************************************************
* Box type and various box options
*
**************************************************************************/
void JLP_wxGDev_Popup::UpdatePopupMenu_BoxType(const int update_display)
{
wxGDev_SETTINGS wxgdev_settings0;

if((initialized != 1234) || (menuBackgd == NULL)
   || (menuBoxType == NULL) || (menuBoxLimits == NULL)) return;

// Get all current GDev settings (pen color, pen width, background color, etc):
 jlp_gdev_wxwid1->GDevGet_wxGDevSettings(&wxgdev_settings0);

 if(menuBoxLimits->FindItem(ID_BOX_LIMITS_WITH_BOX) != NULL) {
    if(wxgdev_settings0.InternalProcessingMode == 0)
      menuBoxLimits->Check(ID_BOX_LIMITS_WITH_BOX, true);
    else
      menuBoxLimits->Check(ID_BOX_LIMITS_WITH_BOX, false);
    }

// Box type:
 switch(wxgdev_settings0.box_type){
  default:
  case 0:
   if(menuBoxType->FindItem(ID_BOX_TYPE0) != NULL)
        menuBoxType->Check(ID_BOX_TYPE0, true);
    break;
  case 1:
   if(menuBoxType->FindItem(ID_BOX_TYPE1) != NULL)
        menuBoxType->Check(ID_BOX_TYPE1, true);
    break;
  case 2:
   if(menuBoxType->FindItem(ID_BOX_TYPE2) != NULL)
        menuBoxType->Check(ID_BOX_TYPE2, true);
    break;
 }

// Ticks in
 if(menuBoxType->FindItem(ID_BOX_TICKS_IN) != NULL) {
   if(wxgdev_settings0.ticks_in == 1)
      menuBoxType->Check(ID_BOX_TICKS_IN, true);
   else
      menuBoxType->Check(ID_BOX_TICKS_IN, false);
  }

// X grid
 if(menuBoxType->FindItem(ID_BOX_XGRID) != NULL) {
   if(wxgdev_settings0.xgrid == 1)
      menuBoxType->Check(ID_BOX_XGRID, true);
   else
      menuBoxType->Check(ID_BOX_XGRID, false);
  }

// Y grid
 if(menuBoxType->FindItem(ID_BOX_YGRID) != NULL) {
   if(wxgdev_settings0.ygrid == 1)
      menuBoxType->Check(ID_BOX_YGRID, true);
   else
      menuBoxType->Check(ID_BOX_YGRID, false);
  }

// Apply those changes (box_type, box_xgrid, etc...)
// Update settings of JLP_GDev_wxWID:
 jlp_gdev_wxwid1->ApplyBoxTypeSettings(wxgdev_settings0, update_display);

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
void JLP_wxGDev_Popup::UpdatePopupMenu_InternalProcessingMode()
{
wxGDev_SETTINGS wxgdev_settings0;
int pmode;

if(initialized != 1234) return;

// Get all current GDev settings (pen color, pen width, background color, etc):
 jlp_gdev_wxwid1->GDevGet_wxGDevSettings(&wxgdev_settings0);

// Update current value from JLP_GDev_WXWID object:
   jlp_gdev_wxwid1->GetInternalProcessingMode(&pmode);
   wxgdev_settings0.InternalProcessingMode = pmode;

// First set process options to false:
  if(menuProcess->FindItem(ID_STATISTICS) != NULL)
    menuProcess->Check(ID_STATISTICS, false);
  if(menuProcess->FindItem(ID_ASTROMETRY) != NULL)
    menuProcess->Check(ID_ASTROMETRY, false);
  if(menuProcess->FindItem(ID_PHOTOMETRY) != NULL)
    menuProcess->Check(ID_PHOTOMETRY, false);
  if(menuProcess->FindItem(ID_LABEL_CONTOURS) != NULL)
     menuProcess->Check(ID_LABEL_CONTOURS, false);
  if(menuProcess->FindItem(ID_GET_COORDINATES) != NULL)
    menuProcess->Check(ID_GET_COORDINATES, false);
// Slices for jlp_splot/images only
   if((gdev_graphic_type1 == 2) || (gdev_graphic_type1 == 3)) {
     if(menuProcess->FindItem(ID_SLICE) != NULL)
          menuProcess->Check(ID_SLICE, false);
     }

// Uncheck Box threshold option and set it to MinMax
  if(wxgdev_settings0.InternalProcessingMode != 0) {
// Problem here!
// May generate an event to ChangeITT_threshold, so I create a wayout:
      software_event1 = 1;
// For jlp_splot/images:
   if((gdev_graphic_type1 == 2) || (gdev_graphic_type1 == 3)) {
        if(menuITT2->FindItem(ID_THR_BOX) != NULL)
          menuITT2->Check(ID_THR_BOX, false);
        if(menuITT2->FindItem(ID_THR_AUTO_MINMAX) != NULL)
          menuITT2->Check(ID_THR_AUTO_MINMAX, true);
// For curves:
       } else if (menuBoxLimits != NULL) {
        if(menuBoxLimits->FindItem(ID_BOX_LIMITS_WITH_BOX) != NULL)
          menuBoxLimits->Check(ID_BOX_LIMITS_WITH_BOX, false);
       }
      software_event1 = 0;
      }

// Then select the good one:
  switch(wxgdev_settings0.InternalProcessingMode){
    case 0:
// For jlp_splot/images: ITT selection with rectangular box
   if((gdev_graphic_type1 == 2) || (gdev_graphic_type1 == 3)) {
         if(menuITT2->FindItem(ID_THR_BOX) != NULL)
            menuITT2->Check(ID_THR_BOX, true);
       } else {
// For curves: box limits selection with rectangular box
         if(menuBoxLimits->FindItem(ID_BOX_LIMITS_WITH_BOX) != NULL)
           menuBoxLimits->Check(ID_BOX_LIMITS_WITH_BOX, true);
       }
      break;
    case 1:
      if(menuProcess->FindItem(ID_STATISTICS) != NULL)
        menuProcess->Check(ID_STATISTICS, true);
      break;
    case 2:
      if(menuProcess->FindItem(ID_ASTROMETRY) != NULL)
         menuProcess->Check(ID_ASTROMETRY, true);
      break;
    case 3:
      if(menuProcess->FindItem(ID_PHOTOMETRY) != NULL)
        menuProcess->Check(ID_PHOTOMETRY, true);
      break;
// Slices are allowed for jlp_splot/images only:
    case 8:
      if((gdev_graphic_type1 == 2) || (gdev_graphic_type1 == 4)) {
        if(menuProcess->FindItem(ID_SLICE) != NULL)
             menuProcess->Check(ID_SLICE, true);
        }
// Get coordinates
    case 19:
      if(menuProcess->FindItem(ID_GET_COORDINATES) != NULL)
        menuProcess->Check(ID_GET_COORDINATES, true);
      break;
// Label contours
    case 20:
      if(menuProcess->FindItem(ID_LABEL_CONTOURS) != NULL)
        menuProcess->Check(ID_LABEL_CONTOURS, true);
      break;
  }

// Apply settings to JLP_GDev_wxWID:
 jlp_gdev_wxwid1->ApplyInternalProcessingModeSettings(wxgdev_settings0);

return;
}

/****************************************************************************
* Update the popup menu items according to the Canvas parameters,
*
* Needed before showing the popup menu since :
All menus must be created on the heap because all menus attached
to a menubar or to another menu will be deleted by their parent
when it is deleted.
The only exception to this rule are the popup menus
(i.e. menus used with wxWindow::PopupMenu()) as wxWidgets
does not destroy them to allow reusing the same menu more than once.
But the exception applies only to the menus themselves and not
to any submenus of popup menus which are still destroyed by wxWidgets
as usual and so must be heap-allocated.
****************************************************************************/
void JLP_wxGDev_Popup::UpdatePopupMenu(wxGDev_SETTINGS wxgdev_settings0,
                                       int update_display)
{
double low_threshold0, upper_threshold0;
int status, itt_is_linear0, box_x10, box_y10, box_x20, box_y20;
wxString itt_type0;
wxString str2;
bool has_shapes, has_labels, has_scale_bar, has_north_east;

if(initialized != 1234 || PopupMenu1 == NULL) return;

// Load input settings (pen color, pen width, background color, etc):
 jlp_gdev_wxwid1->GDevLoad_wxGDevSettings(wxgdev_settings0);

// Erase all check boxes:
 PopupMenuEraseProcessingCheckBoxes();

// Pen colour:
 UpdatePopupMenu_PenColour(update_display);

// Background colour for curves only:
   if((gdev_graphic_type1 == 1) || (gdev_graphic_type1 == 4)) {
    UpdatePopupMenu_BackgroundColour(update_display);
    }

// Cursor:
 UpdatePopupMenu_Cursor();

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
// For curves only : box type
 if((gdev_graphic_type1 == 1) || (gdev_graphic_type1 == 4)) {
  UpdatePopupMenu_BoxType(update_display);
// Gsegraf
  UpdatePopupMenu_Gsegraf(update_display);
// Processing mode
  UpdatePopupMenu_InternalProcessingMode();
  return;
  }

// LUT:
 UpdatePopupMenu_LUT(update_display);

 status = jlp_gdev_wxwid1->GDevGet_ITT_Thresh(&itt_type0, &itt_is_linear0,
                                             &low_threshold0, &upper_threshold0,
                                             &box_x10, &box_y10, &box_x20, &box_y20);

// Default function is selection of thresholds within a rectangular box:
 if(status == 0) {
// Save them to private variables:
  wxgdev_settings0.itt_type = itt_type0;
  wxgdev_settings0.itt_is_linear = itt_is_linear0;
  wxgdev_settings0.low_itt_thresh = low_threshold0;
  wxgdev_settings0.up_itt_thresh = upper_threshold0;
  if(wxgdev_settings0.itt_is_linear == 1) {
    menuITT1->Check(ID_ITT_LIN, true);
   } else {
    menuITT1->Check(ID_ITT_LOG, true);
   }
 }

// Zoom:
 UpdatePopupMenu_Zoom();

// Filters
 UpdatePopupMenu_Filter();

// Gsegraf
 UpdatePopupMenu_Gsegraf(update_display);

// Internal ProcesingMode (interactive selection of boxes with the mouse)
// SHOULD BE CALLED BEFORE UpdatePopupMenu_ITT() !!!
 UpdatePopupMenu_InternalProcessingMode();

// ITT
 UpdatePopupMenu_ITT(update_display);

// Enable/Disable label menus:
  jlp_gdev_wxwid1->GetImageLabelOptions(&has_labels, &has_scale_bar,
                                        &has_north_east, &has_shapes);
// For jlp_splot/images:
  if(menuLabel != NULL) {
    menuLabel->Enable(ID_REM_LABEL, has_labels);
    if(menuLabel->FindItem(ID_ADD_SCALE) != NULL)
      menuLabel->Enable(ID_ADD_SCALE, !has_scale_bar);
    if(menuLabel->FindItem(ID_REM_SCALE) != NULL)
      menuLabel->Enable(ID_REM_SCALE, has_scale_bar);
    if(menuLabel->FindItem(ID_ADD_NORTH_EAST) != NULL)
      menuLabel->Enable(ID_ADD_NORTH_EAST, !has_north_east);
    if(menuLabel->FindItem(ID_REM_NORTH_EAST) != NULL)
      menuLabel->Enable(ID_REM_NORTH_EAST, has_north_east);
    }
// Shapes
  if(menuShape != NULL) {
   if(menuShape->FindItem(ID_REM_SHAPE) != NULL)
     menuShape->Enable(ID_REM_SHAPE, has_shapes);
   if(menuShape->FindItem(ID_CANCEL_SHAPE) != NULL)
     menuShape->Enable(ID_CANCEL_SHAPE, has_shapes);
   if(menuShape->FindItem(ID_ROTATE_SHAPE) != NULL)
     menuShape->Enable(ID_ROTATE_SHAPE, has_shapes);
   if(menuShape->FindItem(ID_MAGNIFY_SHAPE) != NULL)
     menuShape->Enable(ID_MAGNIFY_SHAPE, has_shapes);
   if(menuShape->FindItem(ID_MOVE_SHAPE) != NULL)
     menuShape->Enable(ID_MOVE_SHAPE, has_shapes);
   }

// Update settings of JLP_GDev_wxWID:
jlp_gdev_wxwid1->ApplyITTSettings(wxgdev_settings0, update_display);

return;
}
/************************************************************************
* Erase all check boxes in popup menu
************************************************************************/
void JLP_wxGDev_Popup::PopupMenuEraseProcessingCheckBoxes()
{
// Problem here!
// May generate an event to ChangeITT_threshold, so I create a wayout:
  software_event1 = 1;
// For jlp_splot/images:
  if((gdev_graphic_type1 == 2) || (gdev_graphic_type1 == 3)) {
    if(menuITT2->FindItem(ID_THR_BOX) != NULL)
         menuITT2->Check(ID_THR_BOX, false);
    if(menuITT2->FindItem(ID_THR_AUTO_MINMAX) != NULL)
         menuITT2->Check(ID_THR_AUTO_MINMAX, true);
// For curves:
    } else if(menuBoxLimits != NULL) {
    if(menuBoxLimits->FindItem(ID_BOX_LIMITS_WITH_BOX) != NULL)
      menuBoxLimits->Check(ID_BOX_LIMITS_WITH_BOX, false);
    }
  software_event1 = 0;

  if(menuProcess->FindItem(ID_STATISTICS) != NULL)
    menuProcess->Check(ID_STATISTICS, false);
  if(menuProcess->FindItem(ID_ASTROMETRY) != NULL)
    menuProcess->Check(ID_ASTROMETRY, false);
  if(menuProcess->FindItem(ID_PHOTOMETRY) != NULL)
    menuProcess->Check(ID_PHOTOMETRY, false);
  if(menuProcess->FindItem(ID_GET_COORDINATES) != NULL)
    menuProcess->Check(ID_GET_COORDINATES, false);
// For jlp_splot/images:
  if((gdev_graphic_type1 == 2) || (gdev_graphic_type1 == 3)) {
     if(menuProcess->FindItem(ID_SLICE) != NULL)
          menuProcess->Check(ID_SLICE, false);
    }

return;
}
