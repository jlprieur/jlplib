/******************************************************************************
* jlp_cgdev_wxwid_menu.cpp
* JLP_wxGseg_Canvas class
* Purpose:     Popup menu for displaying a curve with wxwidgets
*
* void JLP_wxGseg_Canvas::UpdatePopupMenu()
* void JLP_wxGseg_Canvas::BoxLimits_Auto()
*
* Author:      JLP 
* Version:     06/02/2016 
******************************************************************************/
#include "jlp_cgdev_wxwid_id.h"
#include "jlp_wx_gscanvas.h"
#include "jlp_wx_cursor.h"   // jlp_SetCrossHairCursor1, etc

/*************************************************************
* Create the popup menu
*************************************************************/
void JLP_wxGseg_Canvas::CreatePopupMenu()
{

  PopupMenu1 = new wxMenu;

// ***************** Curve menu **********************************

  menuCurve = new wxMenu;
  menuCurve->Append( ID_INFO_CURVE, _T("Info. about curve(s)"));
  menuCurve->AppendSeparator();
// ***************** Clipboard submenu ******************************
  menuCurve->Append(wxID_COPY, _T("&Copy curve\tCtrl-C"));
  menuCurve->AppendSeparator();

  menuCurve->Append( ID_SAVE_TO_PST, _T("Save to postscript"),
                    _T("Save graphic to postscript file"));

  menuCurve->Append( ID_SAVE, _T("&Save as ..."),
                    _T("Save to bmp, jpg, png, pcx, pnm, tiff, xpm, ico, cur"));

  PopupMenu1->Append(Menu_Popup_Curve, _T("&Curve"), menuCurve);

// ***************** Setup menu **********************************
  menuSetup = new wxMenu;

  PopupMenu1->Append(Menu_Popup_Curve, _T("&Setup"), menuSetup);
// ***************** Colour pen submenu ******************************
  menuPen = new wxMenu;
  menuPen->Append( ID_BLACK_PEN_COLOUR, _T("Black Pen"), _T("Pen colour"), 
                      wxITEM_RADIO);
  menuPen->Append( ID_RED_PEN_COLOUR, _T("Red Pen"), _T("Pen colour"), 
                      wxITEM_RADIO);
  menuPen->Append( ID_GREEN_PEN_COLOUR, _T("Green Pen"), _T("Pen colour"), 
                      wxITEM_RADIO);
  menuPen->Append( ID_BLUE_PEN_COLOUR, _T("Blue Pen"), _T("Pen colour"), 
                      wxITEM_RADIO);
  menuPen->Append( ID_WHITE_PEN_COLOUR, _T("White Pen"), _T("Pen colour"), 
                      wxITEM_RADIO);
  menuSetup->AppendSubMenu(menuPen, _T("Pen"), _T("Select Pen properties"));
// ***************** Colour background submenu ******************************
  menuBackgd = new wxMenu;
  menuBackgd->Append( ID_BLACK_BACK_COLOUR, _T("Black Background"), 
                   _T("Background colour"), wxITEM_RADIO);
  menuBackgd->Append( ID_YELLOW_BACK_COLOUR, _T("Yellow Background"), 
                   _T("Background colour"), wxITEM_RADIO);
  menuBackgd->Append( ID_GREY_BACK_COLOUR, _T("Grey Background"), 
                   _T("Background colour"), wxITEM_RADIO);
  menuBackgd->Append( ID_WHITE_BACK_COLOUR, _T("White Background"), 
                   _T("Background colour"), wxITEM_RADIO);
  menuSetup->AppendSubMenu(menuBackgd, _T("Background"), 
                          _T("Select Background properties"));
// ***************** Cursor submenu ******************************
  menuCursor = new wxMenu;
  menuCursor->Append( ID_CURSOR_CROSS, _T("Cross"), _T("Cursor shape"),
                      wxITEM_RADIO);
  menuCursor->Append( ID_CURSOR_BIG_CROSS, _T("BigCross"), _T("Cursor shape"),
                      wxITEM_RADIO);
  menuCursor->Append( ID_CURSOR_ARROW, _T("Arrow"), _T("Cursor shape"), 
                      wxITEM_RADIO);
  menuCursor->Append( ID_CURSOR_DOWN_ARROW, _T("DownArrow"), 
                      _T("Cursor shape"), wxITEM_RADIO);
  menuCursor->Append( ID_CURSOR_CROSSHAIR, _T("CrossHair"), _T("Cursor shape"),
                      wxITEM_RADIO);
  menuCursor->Append( ID_CURSOR_CROSSHAIR1, _T("CrossHair with a cross"),
                      _T("Cursor shape"), wxITEM_RADIO);

  menuSetup->AppendSubMenu(menuCursor, _T("Cursor"), _T("Select the cursor type"));
// ***************** Box type submenu ******************************
  menuBoxType = new wxMenu;
  menuBoxType->Append(ID_BOX_TYPE0, _T("BoxType: big steps"), 
                      _T("Mongo type"), wxITEM_RADIO);
  menuBoxType->Append( ID_BOX_TYPE1, _T("BoxType: small steps"), 
                      _T("SciSoft type"), wxITEM_RADIO);
  menuBoxType->Append( ID_BOX_TYPE2, _T("BoxType: round-off limits"), 
                      _T("Gsegrafix type"), wxITEM_RADIO);
  menuBoxType->Append( ID_BOX_TICKS_IN, _T("Axis ticks in"), 
                      _T("With axis tiks in"), wxITEM_CHECK);
  menuBoxType->Append( ID_BOX_XGRID, _T("X grid"), 
                      _T("With a grid on the X axis"), wxITEM_CHECK);
  menuBoxType->Append( ID_BOX_YGRID, _T("Y grid"), 
                      _T("With a grid on the Y axis"), wxITEM_CHECK);

  PopupMenu1->Append(Menu_Popup_Curve, _T("BoxType"), 
                     menuBoxType);

// ***************** Box limits menu **********************************
  menuBoxLimits = new wxMenu;
  menuBoxLimits->Append(ID_BOX_LIMITS_WITH_BOX, 
                        _T("BoxLimits/Zoom: with box"),
                        _T("Interactive selection of a rectangular box"),
                        wxITEM_RADIO);
  menuBoxLimits->Append(ID_BOX_LIMITS_AUTO, _T("BoxLimits: auto"),
                        _T("Best setup for full plot"),
                        wxITEM_RADIO);
  menuBoxLimits->Append(ID_BOX_LIMITS_MANUAL, _T("BoxLimits: manual"),
                        _T("Input of your choice"),
                        wxITEM_RADIO);
  menuBoxLimits->Append(ID_BOX_LIMITS_MANUAL1, 
                        _T("BoxLimits: manual (new values)"),
                        _T("Input of your choice"),
                        wxITEM_RADIO);

  PopupMenu1->Append(Menu_Popup_Curve, _T("BoxLimits"), 
                     menuBoxLimits);

// ***************** Label menu ******************************
  menuLabel = new wxMenu;
  menuLabel->Append(ID_ADD_LABEL, _T("Add a label"), 
                      wxT("Label positioned with the mouse"),
                      wxITEM_CHECK);
  menuLabel->Append(ID_REM_LABEL, _T("Remove a label"), 
                      wxT("Remove the last label (if any)"),
                      wxITEM_CHECK);
  menuLabel->Append(ID_IDLE_LABEL, _T("Idle labelling"), 
                      wxT("No labelling"),
                      wxITEM_CHECK);
  PopupMenu1->Append(Menu_Popup_Label, _T("Labels"), menuLabel);

// ***************** Curve Processing menu ******************************
  menuProcess = new wxMenu;
  menuProcess->AppendCheckItem( ID_STATISTICS, _T("Statistics in a box"),
                      wxT("Rectangular box selected by the user"));
  menuProcess->AppendCheckItem( ID_PHOTOMETRY, _T("Photometry in a box"),
                      wxT("Rectangle selected by the user"));
  menuProcess->AppendCheckItem( ID_ASTROMETRY, _T("Astrometry in a box"),
                      wxT("Rectangle selected by the user"));
  PopupMenu1->Append(wxID_ANY, _T("Processing"), menuProcess);

return;
}
/**************************************************************************
*
* INPUT
* cursor_type: CrossHair1, CrossHair, DownArrow, Arrow, BigCross
*
**************************************************************************/
void JLP_wxGseg_Canvas::UpdatePopupMenu_Cursor()
{
wxString str1, str2;
wxCursor my_cursor;

// Refresh screen (necessary if crosshair cursor was set,
// to erase the crosshair)
 Refresh();

/* Cursor types: Arrow, DownArrow, Cross, CrossHair, CrossHair1
*/
 if(!cgdev_settings1.cursor_type.Cmp(wxT("CrossHair1"))) {
   if(menuCursor->FindItem(ID_CURSOR_CROSSHAIR1) != NULL) 
        menuCursor->Check(ID_CURSOR_CROSSHAIR1, true);
   jlp_SetCrossHairCursor1(my_cursor, cgdev_settings1.cursor_type);
 } else if (!cgdev_settings1.cursor_type.Cmp(wxT("CrossHair"))) { 
   if(menuCursor->FindItem(ID_CURSOR_CROSSHAIR) != NULL) 
        menuCursor->Check(ID_CURSOR_CROSSHAIR, true);
   jlp_SetCrossHairCursor(my_cursor, cgdev_settings1.cursor_type);
 } else if (!cgdev_settings1.cursor_type.Cmp(wxT("DownArrow"))) { 
   if(menuCursor->FindItem(ID_CURSOR_DOWN_ARROW) != NULL) 
        menuCursor->Check(ID_CURSOR_DOWN_ARROW, true);
   jlp_SetDownArrowCursor(my_cursor, cgdev_settings1.pen_colour, 
                          cgdev_settings1.cursor_type);
 } else if (!cgdev_settings1.cursor_type.Cmp(wxT("Arrow"))) { 
   if(menuCursor->FindItem(ID_CURSOR_ARROW) != NULL) 
        menuCursor->Check(ID_CURSOR_ARROW, true);
   jlp_SetArrowCursor(my_cursor, cgdev_settings1.cursor_type);
 } else if (!cgdev_settings1.cursor_type.Cmp(wxT("BigCross"))) { 
   if(menuCursor->FindItem(ID_CURSOR_BIG_CROSS) != NULL) 
        menuCursor->Check(ID_CURSOR_BIG_CROSS, true);
   jlp_SetCrossCursor(my_cursor, cgdev_settings1.pen_colour, 
                      cgdev_settings1.cursor_type, 64);
// Default (=Cross 32)
 } else { 
   if(menuCursor->FindItem(ID_CURSOR_CROSS) != NULL) 
        menuCursor->Check(ID_CURSOR_CROSS, true);
   jlp_SetCrossCursor(my_cursor, cgdev_settings1.pen_colour, 
                      cgdev_settings1.cursor_type, 32);
 }

// Update cursor:
 SetCursor(my_cursor);
 Refresh();

return;
}
/**************************************************************************
* Pen colour
*
**************************************************************************/
void JLP_wxGseg_Canvas::UpdatePopupMenu_PenColour()
{

// Pen colour:
cgdev_settings1.pen_colour = cgdev_settings1.pen_default_colour;
 if(cgdev_settings1.pen_colour == *wxWHITE){
 if(menuPen->FindItem(ID_WHITE_PEN_COLOUR) != NULL)
      menuPen->Check(ID_WHITE_PEN_COLOUR, true);
 } else if(cgdev_settings1.pen_colour == *wxRED){
 if(menuPen->FindItem(ID_RED_PEN_COLOUR) != NULL)
      menuPen->Check(ID_RED_PEN_COLOUR, true);
 } else if(cgdev_settings1.pen_colour == *wxGREEN){
 if(menuPen->FindItem(ID_GREEN_PEN_COLOUR) != NULL)
      menuPen->Check(ID_GREEN_PEN_COLOUR, true);
 } else if(cgdev_settings1.pen_colour == *wxBLUE){
 if(menuPen->FindItem(ID_BLUE_PEN_COLOUR) != NULL)
      menuPen->Check(ID_BLUE_PEN_COLOUR, true);
// Default is black:
 } else {
 if(menuPen->FindItem(ID_BLACK_PEN_COLOUR) != NULL)
      menuPen->Check(ID_BLACK_PEN_COLOUR, true);
 cgdev_settings1.pen_colour = *wxBLACK;
 }

// Apply changes
 SetPenColour_dc(cgdev_settings1.pen_colour);

// Change the colour of the labels if needed:
 RedrawToBackupDC();

// Update cursor (since there may be some possible change 
// of the coloured cursor)
  UpdatePopupMenu_Cursor();

return;
}
/**************************************************************************
* Background colour
*
**************************************************************************/
void JLP_wxGseg_Canvas::UpdatePopupMenu_BackgroundColour()
{

// Background colour:
 if(cgdev_settings1.backgd_colour == *wxBLACK){
 if(menuBackgd->FindItem(ID_BLACK_BACK_COLOUR) != NULL)
      menuBackgd->Check(ID_BLACK_BACK_COLOUR, true);
 } else if(cgdev_settings1.backgd_colour == *wxYELLOW){
 if(menuBackgd->FindItem(ID_YELLOW_BACK_COLOUR) != NULL)
      menuBackgd->Check(ID_YELLOW_BACK_COLOUR, true);
 } else if(cgdev_settings1.backgd_colour == *wxLIGHT_GREY){
 if(menuBackgd->FindItem(ID_GREY_BACK_COLOUR) != NULL)
      menuBackgd->Check(ID_GREY_BACK_COLOUR, true);
// Default is white:
 } else {
 if(menuBackgd->FindItem(ID_WHITE_BACK_COLOUR) != NULL)
      menuBackgd->Check(ID_WHITE_BACK_COLOUR, true);
 cgdev_settings1.backgd_colour = *wxWHITE;
 }

// Apply changes
 SetBackgdColour_dc(cgdev_settings1.backgd_colour);

// Change the colour of the labels if needed:
 RedrawToBackupDC();

return;
}
/**************************************************************************
* Box type and various box options 
*
**************************************************************************/
void JLP_wxGseg_Canvas::UpdatePopupMenu_BoxType()
{
if(initialized != 1234) return;

// Background colour:
 switch(cgdev_settings1.box_type){
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
   if(cgdev_settings1.ticks_in == 1)
      menuBoxType->Check(ID_BOX_TICKS_IN, true);
   else
      menuBoxType->Check(ID_BOX_TICKS_IN, false);
  }

// X grid 
 if(menuBoxType->FindItem(ID_BOX_XGRID) != NULL) {
   if(cgdev_settings1.xgrid == 1)
      menuBoxType->Check(ID_BOX_XGRID, true);
   else
      menuBoxType->Check(ID_BOX_XGRID, false);
  }

// Y grid 
 if(menuBoxType->FindItem(ID_BOX_YGRID) != NULL) {
   if(cgdev_settings1.ygrid == 1)
      menuBoxType->Check(ID_BOX_YGRID, true);
   else
      menuBoxType->Check(ID_BOX_YGRID, false);
  }

// Apply changes
 RedrawToBackupDC();

return;
}
/****************************************************************************
* Update the popup menu items according to the wxPanel parameters,
****************************************************************************/
void JLP_wxGseg_Canvas::UpdatePopupMenu()
{
wxString str1, str2;

if(PopupMenu1 == NULL) return;

// Erase all check boxes:
 PopupMenuEraseProcessingCheckBoxes();

// Internal ProcessingMode (including the interactive selection 
// of the rectangular boxes (used for statictics, box limits, etc) 
// with the mouse)
// SHOULD BE CALLED BEFORE UpdatePopupMenu_ITT() !!!
 UpdatePopupMenu_InternalProcessingMode();

// Pen colour
 UpdatePopupMenu_PenColour();

// Background colour
 UpdatePopupMenu_BackgroundColour();

// Cursor 
// (after pen colour since there may be possible change of the coloured cursor)
 UpdatePopupMenu_Cursor();

// Box type
 UpdatePopupMenu_BoxType();

return;
}
/**************************************************************************
* Update InternalProcessingMode 
* Update popup menu and apply settings
*
* InternalProcessingMode
* -1=None 0=BoxLimits 1=Statistics, 2=Astrometry 3=Photometry
* 4=Add a label 5=Remove a label 
*
**************************************************************************/
void JLP_wxGseg_Canvas::UpdatePopupMenu_InternalProcessingMode()
{

// First set process options to false:
  menuProcess->Check(ID_STATISTICS, false);
  menuProcess->Check(ID_ASTROMETRY, false);
  menuProcess->Check(ID_PHOTOMETRY, false);
  menuLabel->Check(ID_ADD_LABEL, false);
  menuLabel->Check(ID_REM_LABEL, false);

// Idle to true
  menuLabel->Check(ID_IDLE_LABEL, true);

// Uncheck Box threshold option and set it to MinMax
  if(cgdev_settings1.InternalProcessingMode != 0) {
// Problem here!
// May generate an event to ChangeITT_threshold, so I create a wayout:
      software_event1 = 1;
      menuBoxLimits->Check(ID_BOX_LIMITS_WITH_BOX, false);
      menuBoxLimits->Check(ID_BOX_LIMITS_AUTO, true);
      software_event1 = 0;
      }

// Then select the good one:
  switch(cgdev_settings1.InternalProcessingMode){
    case -1:
      menuLabel->Check(ID_IDLE_LABEL, true);
      break;
    case 1:
      menuProcess->Check(ID_STATISTICS, true);
      break;
    case 2:
      menuProcess->Check(ID_ASTROMETRY, true);
      break;
    case 3:
      menuProcess->Check(ID_PHOTOMETRY, true);
      break;
    case 4:
      menuLabel->Check(ID_ADD_LABEL, true);
      menuLabel->Check(ID_IDLE_LABEL, false);
      break;
    case 5:
      menuLabel->Check(ID_REM_LABEL, true);
      menuLabel->Check(ID_IDLE_LABEL, false);
      break;
// 18=ZoomIn
    case 18:
      menuBoxLimits->Check(ID_BOX_LIMITS_WITH_BOX, true);
      break;
  }

// Apply settings:
 SetInternalProcessingMode(cgdev_settings1.InternalProcessingMode);

return;
}
/************************************************************************
* Erase all check boxes in popup menu
************************************************************************/
void JLP_wxGseg_Canvas::PopupMenuEraseProcessingCheckBoxes()
{
// Problem here!
// May generate an event to ChangeITT_threshold, so I create a wayout:
  software_event1 = 1;
  menuBoxLimits->Check(ID_BOX_LIMITS_WITH_BOX, false);
  menuBoxLimits->Check(ID_BOX_LIMITS_AUTO, true);
  software_event1 = 0;

  menuProcess->Check(ID_STATISTICS, false);
  menuProcess->Check(ID_ASTROMETRY, false);
  menuProcess->Check(ID_PHOTOMETRY, false);

return;
}
/************************************************************************
* Automatic determination of the new Box Limits 
************************************************************************/
void JLP_wxGseg_Canvas::BoxLimitsAuto()
{
double xmin, xmax, ymin, ymax;

// Redraw plot with initial settings if empty box
//   Callback_ZoomIn(0, 0, 0, 0);

#ifdef OLD_VERSION
// Exit if no curve entered yet:
  if(ncurves1 == 0 || initialized != 1234) return;

  ComputeXYMinMaxForCurves(xmin, xmax, ymin, ymax);

// The calling program may have chosen some of the boundaries
// by calling LoadPlotSettings() without 0,0,0,0 for x1,x2,y1,y2
  if(x1_1 != x2_1) {
    xmin = x1_1;
    xmax = x2_1; 
    }
  if(y1_1 != y2_1) {
    ymin = y1_1;
    ymax = y2_1; 
    }

  SetBoxLimits(xmin, xmax, ymin, ymax);

// Set axes to JLP axes (pb...):
// cgdev_settings1.box_type = 1;
// Set axes to MGO axes (works better):
//  cgdev_settings1.box_type = 0;

#endif
return;
}
/************************************************************************
* INPUT:
* zoom_in: if true, multiply the x range by two 
*          if false (i.e. zoom_out) divide the x range by two 
************************************************************************/
void JLP_wxGseg_Canvas::BoxLimitsZoom(const bool zoom_in)
{
double xmin, xmax, ymin, ymax;
double xmin0, xmax0, ymin0, ymax0;
double xc, xrange;

// Exit if no curve entered yet:
  if(ncurves1 == 0 || initialized != 1234) return;

  xmin = Jgc0.xminuser;
  xmax = Jgc0.xmaxuser;

// Range:
  xrange = xmax - xmin;

// Coordinates of the center:
  xc = (xmin + xmax) / 2.;

  if(zoom_in) {
// Divide the x range by two:
     xmin = xc - xrange / 4.;
     xmax = xmin + xrange / 2.;
   } else {
// Double the x range
     xmin = xc - xrange;
     xmax = xmin + 2. * xrange;
   }

// Compute the maximum limits for the X and Y axes: 
  ComputeXYMinMaxForCurves(xmin0, xmax0, ymin0, ymax0);
  xmin = MAXI(xmin0, xmin);
  xmax = MINI(xmax0, xmax);

// Compute the limits for the Y axis: 
  ComputeYMinMaxForCurves(xmin, xmax, ymin, ymax);

  SetBoxLimits(xmin, xmax, ymin, ymax);

// Set axes to JLP axes (pb...):
// cgdev_settings1.box_type = 1;
// Set axes to MGO axes (works better):
//  cgdev_settings1.box_type = 0;

return;
}
/************************************************************************
* Move to the right (by half width of current box limits)
************************************************************************/
void JLP_wxGseg_Canvas::BoxLimitsMove(const bool move_to_right)
{
double xmin, xmax, ymin, ymax;
double xmin0, xmax0, ymin0, ymax0;
double xc, xrange;

// Exit if no curve entered yet:
  if(ncurves1 == 0 || initialized != 1234) return;

  xmin = Jgc0.xminuser;
  xmax = Jgc0.xmaxuser;

// Range:
  xrange = xmax - xmin;

// Coordinates of the center:
  xc = (xmin + xmax) / 2.;

  if(move_to_right) { 
     xmin = xmin + xrange / 4.;
   } else {
     xmin = xmin - xrange / 4.;
   }
   xmax = xmin + xrange;

// Compute the maximum limits for the X and Y axes: 
  ComputeXYMinMaxForCurves(xmin0, xmax0, ymin0, ymax0);

// Return if left/right limit has been reached:
  if((xmin  <= xmin0) || (xmax  >= xmax0)) return;

// Compute the limits for the Y axis: 
  ComputeYMinMaxForCurves(xmin, xmax, ymin, ymax);

  SetBoxLimits(xmin, xmax, ymin, ymax);

// Set axes to JLP axes (pb...):
// cgdev_settings1.box_type = 1;
// Set axes to MGO axes (works better):
//  cgdev_settings1.box_type = 0;

return;
}
/************************************************************************
* Pen colour
************************************************************************/
void JLP_wxGseg_Canvas::SetPenColour_dc(const wxColour wxC)
{
// For curves:
  backup_dc->SetPen(wxPen(wxC));
// For Text:
  backup_dc->SetTextForeground(wxC);

return;
}
/************************************************************************
* Background colour
************************************************************************/
void JLP_wxGseg_Canvas::SetBackgdColour_dc(const wxColour wxC)
{
// For curves:
  backup_dc->SetBackground(wxBrush(wxC));
// For Text:
  backup_dc->SetTextBackground(wxC);

return;
}
