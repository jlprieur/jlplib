/******************************************************************************
* jlp_wxgdev_popup_create.cpp
* JLP_wxGDev_Popup class
* Purpose:     Popup menu for displaying a curve with wxwidgets
*
* Author:      JLP
* Version:     12/03/2020
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
***/

/*
#define DEBUG
*/

/*************************************************************
* Delete the previous popup menu
*
*************************************************************/
void JLP_wxGDev_Popup::DeletePopupMenu()
{
  if(PopupMenu1 != NULL) {
    if(menuInfo != NULL) delete menuInfo;
    menuInfo = NULL;
/** WARNING: SUBMENUS SHOULD NOT BE DELETED SEPARATELY ...
// menuCursor: submenu of menuSetup: should be deleted first ?
    if(menuCursor != NULL) delete menuCursor;
    menuCursor = NULL; 
// menuPen: submenu of menuSetup: should be deleted first ?
    if(menuPen != NULL) delete menuPen;
    menuPen = NULL;
// menuBackgd: submenu of menuSetup: should be deleted first ?
    if(menuBackgd != NULL) delete menuBackgd;
    menuBackgd = NULL; 
// menuLUT: submenu of menuSetup: should be deleted first ?
    if(menuLUT != NULL) delete menuLUT;
    menuLUT = NULL;
// menuITT1: submenu of menuSetup: should be deleted first ?
    if(menuITT1 != NULL) delete menuITT1;
    menuITT1 = NULL; 
// menuITT2: submenu of menuSetup: should be deleted first ?
    if(menuITT2 != NULL) delete menuITT2;
    menuITT2 = NULL; 
// menuZoom: submenu of menuSetup: should be deleted first ?
    if(menuZoom != NULL) delete menuZoom;
    menuZoom = NULL;
***/
// menuSetup: should also delete all submenus...
    if(menuSetup != NULL) delete menuSetup;
    menuSetup = NULL;

    if(menuGsegraf != NULL) delete menuGsegraf;
    menuGsegraf = NULL;
    if(menuFilter != NULL) delete menuFilter;
    menuFilter = NULL;
    if(menuProcess != NULL) delete menuProcess;
    menuProcess = NULL;
    if(menuLabel != NULL) delete menuLabel;
    menuLabel = NULL;
    if(menuShape != NULL) delete menuShape;
    menuShape = NULL;
    if(menuBoxType != NULL) delete menuBoxType;
    menuBoxType = NULL;
    if(menuBoxLimits != NULL) delete menuBoxLimits;
    menuBoxLimits = NULL;
// PopupMenu1: should be deleted the last
// problem here ...
//    delete PopupMenu1;
    PopupMenu1 = NULL;
  }

return;
}
/************************************************************************
* Create the popup menu corresponding to gdev_graphic_type1
*
************************************************************************/
void JLP_wxGDev_Popup::CreatePopupMenu()
{
bool display_image;

  if(PopupMenu1 != NULL) DeletePopupMenu();
  PopupMenu1 = new wxMenu;

#if 0
  PopupMenu1.AppendCheckItem(ID_MENU_ToBeChecked, _T("To be &checked"));
  PopupMenu1.Append(ID_MENU_ToBeGreyed, _T("To be &greyed"),
                _T("This menu item should be initially greyed out"));
  PopupMenu1.AppendSeparator();

  PopupMenu1.Delete(ID_MENU_ToBeDeleted);
  PopupMenu1.Check(ID_MENU_ToBeChecked, true);
  PopupMenu1.Enable(ID_MENU_ToBeGreyed, false);
#endif

#ifdef DEBUG
printf("CreatePopupMenu/ gdev_graphic_type=%d\n", gdev_graphic_type1);
#endif

// ***************** Info menu **********************************
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
  if((gdev_graphic_type1 == 2) || (gdev_graphic_type1 == 3)
     || (gdev_graphic_type1 == 5) || (gdev_graphic_type1 == 7)) {
     display_image = true;
    } else {
     display_image = false;
    }

  menuInfo = new wxMenu;
   if(display_image == true) {
    menuInfo->Append( ID_INFO_IMAGE, _T("Info. about image"));
    if((gdev_graphic_type1 == 2) || (gdev_graphic_type1 == 3)
       || (gdev_graphic_type1 == 5)) {
    menuInfo->Append( ID_DISPLAY_IMAGE_3D, _T("Display the image in 3D"),
                      _T("Plot to a separate frame"));
    menuInfo->Append( ID_DISPLAY_IMAGE_CONTOURS, 
                      _T("Display the image contours"),
                      _T("Plot to a separate frame"));
      }  
    } else {
    menuInfo->Append( ID_INFO_CURVE, _T("Info. about curve(s)"));
    }
  menuInfo->AppendSeparator();
// ***************** Clipboard submenu ******************************
  if(display_image == true) {
    menuInfo->Append(wxID_COPY, _T("Copy image\tCtrl-C"));
    menuInfo->Append(wxID_PASTE, _T("Paste image\tCtrl-V"));
    } else {
    menuInfo->Append(wxID_COPY, _T("&Copy curve\tCtrl-C"));
    }

  menuInfo->AppendSeparator();
// ***************** Save submenu ******************************
  menuInfo->Append(ID_SAVE_TO_PST, _T("Save to postscript"),
                    _T("Save graphic to postscript file"));
  menuInfo->Append(ID_SAVE, _T("&Save as ..."),
                    _T("Save to bmp, jpg, png, pcx, pnm, tiff, xpm, ico, cur"));

  if(display_image == true) {
   PopupMenu1->Append(wxID_ANY, _T("&Image"), menuInfo);
   } else {
   PopupMenu1->Append(wxID_ANY, _T("&Curve"), menuInfo);
   }


// ***************** Setup menu **********************************
  menuSetup = new wxMenu;

// ***************** Colour pen submenu ******************************
  menuPen = new wxMenu;
  menuPen->Append(ID_BLACK_PEN_COLOUR, _T("Black Pen"), _T("Pen colour"),
                   wxITEM_RADIO);
  menuPen->Append(ID_RED_PEN_COLOUR, _T("Red Pen"), _T("Pen colour"),
                   wxITEM_RADIO);
  menuPen->Append(ID_GREEN_PEN_COLOUR, _T("Green Pen"), _T("Pen colour"),
                   wxITEM_RADIO);
  menuPen->Append(ID_BLUE_PEN_COLOUR, _T("Blue Pen"), _T("Pen colour"),
                   wxITEM_RADIO);
  menuPen->Append(ID_WHITE_PEN_COLOUR, _T("White Pen"), _T("Pen colour"),
                   wxITEM_RADIO);
  menuSetup->AppendSubMenu(menuPen, _T("Pen"), _T("Select Pen properties"));

// Colour background (for curves only)
  CreateColourBackgroundMenu();

// ***************** Cursor submenu ******************************
  menuCursor = new wxMenu;
  menuCursor->Append(ID_CURSOR_CROSS, _T("Cross"), _T("Cursor shape"),
                      wxITEM_RADIO);
  menuCursor->Append(ID_CURSOR_BIG_CROSS, _T("BigCross"), _T("Cursor shape"),
                      wxITEM_RADIO);
  menuCursor->Append(ID_CURSOR_ARROW, _T("Arrow"), _T("Cursor shape"),
                      wxITEM_RADIO);
  menuCursor->Append(ID_CURSOR_DOWN_ARROW, _T("DownArrow"),
                      _T("Cursor shape"), wxITEM_RADIO);
  menuCursor->Append(ID_CURSOR_CROSSHAIR, _T("CrossHair"), _T("Cursor shape"),
                      wxITEM_RADIO);
  menuCursor->Append(ID_CURSOR_CROSSHAIR1, _T("CrossHair with a cross"),
                      _T("Cursor shape"), wxITEM_RADIO);

  menuSetup->AppendSubMenu(menuCursor, _T("Cursor"), _T("Select the cursor type"));

// LUT, ITT, discrete zoom (for jlp_splot/images only)
  CreateImageMenuForSplot();

  PopupMenu1->Append(wxID_ANY, _T("&Setup"), menuSetup);

// Filter menu (for jlp_splot/images only)
  CreateImageMenuFilterForSplot();

  CreateSetupMenuForCurves();

// Gsegraf menu 
  CreateGsegrafMenu();

// ***************** Label menu ******************************
  menuLabel = new wxMenu;
  menuLabel->Append(ID_ADD_LABEL, _T("Add a label"),
                      wxT("Label positioned with the mouse"));
  menuLabel->Append(ID_REM_LABEL, _T("Remove a label"),
                      wxT("Remove a label by clicking on the mouse"));
// Scale and orientation for jlp_splot/images only
   if(display_image == true) {
   menuLabel->Append(ID_ADD_SCALE, _T("Add the scale"),
                      wxT("Scale positioned with the mouse"));
   menuLabel->Append(ID_REM_SCALE, _T("Remove the scale"),
                      wxT("Remove the scale (if already entered)"));
   menuLabel->Append(ID_ADD_NORTH_EAST,
                      wxT("Add the North-East label"),
                      wxT("North-East label positioned with the mouse"));
   menuLabel->Append(ID_REM_NORTH_EAST,
                      wxT("Remove the North-East label"),
                      wxT("Remove the North-East label (if already entered)"));
   }
// Not available yet for gsegraf:
  if(gdev_graphic_type1 <= 3) {
   PopupMenu1->Append(wxID_ANY, _T("Labels"), menuLabel);
   }

// ***************** Shape menu ******************************
  CreateShapeMenu();

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

// ***************** Image/Curve processing menu ******************************
  menuProcess = new wxMenu;
  if(gdev_graphic_type1 <= 3) {
   menuProcess->AppendCheckItem( ID_STATISTICS, _T("Statistics in a box"),
                      wxT("Rectangular box selected by the user"));
   }
// Photometry in a circle, for jlp_splot/images only:
  if(gdev_graphic_type1 == 3) {
   menuProcess->AppendCheckItem( ID_PHOTOMETRY, _T("Photometry in a circle"),
                      wxT("Circle selected by the user"));
   menuProcess->AppendCheckItem( ID_ASTROMETRY, _T("Astrometry in a circle"),
                      wxT("Circle selected by the user"));
   menuProcess->AppendCheckItem( ID_SLICE, _T("Slice along a line"),
                      wxT("Line selected by the user"));
// Photometry in a box, for jlp_splot/curves only:
   } else if(gdev_graphic_type1 == 1) {
   menuProcess->AppendCheckItem( ID_PHOTOMETRY, _T("Photometry in a box"),
                      wxT("Rectangle selected by the user"));
   menuProcess->AppendCheckItem( ID_ASTROMETRY, _T("Astrometry in a box"),
                      wxT("Rectangle selected by the user"));
   } else if(gdev_graphic_type1 == 5) {
   menuProcess->AppendCheckItem(ID_LABEL_CONTOURS, wxT("Label contours"),
                      wxT("Interactive labelling of contours (if appropriate)"));
   }
  menuProcess->AppendCheckItem( ID_GET_COORDINATES, 
                             _T("Get list of coordinates"),
                             wxT("Interactive input of a list of coordinates"));
  PopupMenu1->Append(wxID_ANY, _T("Processing"), menuProcess);

return;
}
/****************************************************************************
* LUT, ITT, discrete zoom, and filter menu
* For jlp_splot images
****************************************************************************/
void JLP_wxGDev_Popup::CreateImageMenuForSplot()
{
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
   if((gdev_graphic_type1 != 2) && (gdev_graphic_type1 != 3)) return;

// ***************** LUT submenu ******************************
/* lut_type: 'l'=log rainbow1 'r'=rainbow2 's'=saw 'g'=gray 'c'=curves
*           'p'=pisco
* RAIN1, RAIN2, SAW, GRAY, CUR, PISCO, REV
*/
  menuLUT = new wxMenu;
  menuLUT->Append(ID_LUT_RAIN1, _T("Rainbow1"), wxT("Colour Look Up Table"),
                  wxITEM_RADIO);
  menuLUT->Append(ID_LUT_RAIN2, _T("Rainbow2"), wxT("Colour Look Up Table"),
                  wxITEM_RADIO);
  menuLUT->Append(ID_LUT_PISCO, _T("Pisco_like"), wxT("Colour Look Up Table"),
                  wxITEM_RADIO);
  menuLUT->Append(ID_LUT_CUR, _T("For Curves"), wxT("Colour Look Up Table"),
                  wxITEM_RADIO);
  menuLUT->Append(ID_LUT_SAW, _T("Gray Saw"), wxT("B&W Look Up Table"),
                  wxITEM_RADIO);
  menuLUT->Append(ID_LUT_GRAY, _T("Gray Linear"), wxT("B&W Look Up Table"),
                  wxITEM_RADIO);
  menuLUT->AppendSeparator();
  menuLUT->Append(ID_LUT_REV, _T("Reversed LUT"), wxT("Inverse colors"),
                  wxITEM_CHECK);
  menuSetup->AppendSubMenu(menuLUT, _T("LUT"),
                           wxT("To select a Look Up Table"));
   
// ***************** ITT submenu ******************************
/*
* ITT_1:
*         "Lin" (Linear scale)
* LIN, LOG
*/
  menuITT1 = new wxMenu;
  menuITT1->Append( ID_ITT_LIN, _T("Linear"), _T("Linear scale"),
                  wxITEM_RADIO);
  menuITT1->Append( ID_ITT_LOG, _T("Logarithmic"), _T("Log. scale"),
                  wxITEM_RADIO);
    menuSetup->AppendSubMenu(menuITT1, _T("ITT type"),
                             _T("Type of Intensity Transfer Table"));

/*  "Log" (Linear scale)
*   "Direct" (Thresholds entered by the user)
*   "Box" (Automatic: from a rectangular box)
*   "MinMax" (Automatic scale: Min Max)
*   "Median" (Automatic scale: Median)
*   "HC" (Automatic scale:  High contrast)
* BOX, DIRECT, DIRECT1, AUTO_MINMAX, AUTO_HC, AUTO_VHC, etc
*
*/
  menuITT2 = new wxMenu;
  menuITT2->Append( ID_THR_DIRECT, _T("With fixed thresholds"),
                  _T("Direct input of thresholds"), wxITEM_RADIO);
  menuITT2->Append( ID_THR_DIRECT1, _T("With new fixed thresholds"),
                  _T("Direct input of thresholds"), wxITEM_RADIO);
  menuITT2->Append( ID_THR_BOX, _T("Automatic: from box"),
                  _T("Interactive selection from box"), wxITEM_RADIO);
  menuITT2->Append( ID_THR_AUTO_MINMAX, _T("Automatic: min/max"),
                  _T("Automatic computation"), wxITEM_RADIO);
  menuITT2->Append( ID_THR_AUTO_MEDIAN, _T("Automatic: median"),
                  _T("Automatic computation with the median"), wxITEM_RADIO);
  menuITT2->Append( ID_THR_AUTO_HC, _T("Automatic: high contrast"),
                  _T("Automatic computation"), wxITEM_RADIO);
  menuITT2->Append( ID_THR_AUTO_VHC, _T("Automatic: very high contrast"),
                  _T("Automatic computation"), wxITEM_RADIO);

  menuSetup->AppendSubMenu(menuITT2, _T("ITT thresholds"),
                           _T("Thresholds of the Intensity Transfer Table"));

// No longer used since I fix the maximum size of the image now...
//  menuSetup->Append( ID_FULL, _T("&Full screen on/off"));

// ***************** Discrete Zoom ****************************
// Discrete zoom for jlp_splot/images only
  menuZoom = new wxMenu;
  menuZoom->Append( ID_ZOOM_C8, _T("/8"), _T("Reduction: /8"),
                    wxITEM_RADIO);
  menuZoom->Append( ID_ZOOM_C7, _T("/7"), _T("Reduction: /7"),
                    wxITEM_RADIO);
  menuZoom->Append( ID_ZOOM_C6, _T("/6"), _T("Reduction: /6"),
                    wxITEM_RADIO);
  menuZoom->Append( ID_ZOOM_C5, _T("/5"), _T("Reduction: /5"),
                    wxITEM_RADIO);
  menuZoom->Append( ID_ZOOM_C4, _T("/4"), _T("Reduction: /4"),
                    wxITEM_RADIO);
  menuZoom->Append( ID_ZOOM_C3, _T("/3"), _T("Reduction: /3"),
                    wxITEM_RADIO);
  menuZoom->Append( ID_ZOOM_C2, _T("/2"), _T("Reduction: /2"),
                    wxITEM_RADIO);
  menuZoom->Append( ID_ZOOM1, _T("1x"), _T("Magnification: 1x"),
                    wxITEM_RADIO);
  menuZoom->Append( ID_ZOOM2, _T("2x"), _T("Magnification: 2x"),
                    wxITEM_RADIO);
  menuZoom->Append( ID_ZOOM3, _T("3x"), _T("Magnification: 3x"),
                    wxITEM_RADIO);
  menuZoom->Append( ID_ZOOM4, _T("4x"), _T("Magnification: 4x"),
                    wxITEM_RADIO);
  menuZoom->Append( ID_ZOOM5, _T("5x"), _T("Magnification: 5x"),
                    wxITEM_RADIO);
  menuZoom->Append( ID_ZOOM6, _T("6x"), _T("Magnification: 6x"),
                    wxITEM_RADIO);
  menuZoom->Append( ID_ZOOM8, _T("8x"), _T("Magnification: 8x"),
                    wxITEM_RADIO);
  menuZoom->Append( ID_ZOOM10, _T("10x"), _T("Magnification: 10x"),
                    wxITEM_RADIO);
  menuZoom->Append( ID_ZOOM15, _T("15x"), _T("Magnification: 15x"),
                    wxITEM_RADIO);
  menuZoom->Append( ID_ZOOM20, _T("20x"), _T("Magnification: 20x"),
                    wxITEM_RADIO);
  menuZoom->Append( ID_ZOOM40, _T("40x"), _T("Magnification: 40x"),
                    wxITEM_RADIO);
  menuSetup->AppendSubMenu(menuZoom, _T("Zoom"), 
                           _T("To change the magnification"));

return;
}
/***********************************************************************
* Gsegraf menu
***********************************************************************/
void JLP_wxGDev_Popup::CreateGsegrafMenu()
{
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
   if(gdev_graphic_type1 < 4) return;

 menuGsegraf = new wxMenu;
 menuGsegraf->Append(ID_GSEG_AXIS_LIMITS,
                      wxT("Change axis limits"),
                      wxT("Display and change the axis limits"));
 menuGsegraf->Append(ID_GSEG_AXIS_ROTATE,
                      wxT("Axis rotation (if 3d mode)"),
                      wxT("Change the projection angles of the 3d plot"));
 PopupMenu1->Append(wxID_ANY, _T("Gsegraf"), menuGsegraf);
return;
}
/***********************************************************************
* LUT, ITT, discrete zoom, and filter menu
* For jlp_splot images
***********************************************************************/
void JLP_wxGDev_Popup::CreateImageMenuFilterForSplot()
{
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
   if((gdev_graphic_type1 != 2) && (gdev_graphic_type1 != 3)) return;

// ***************** Filter menu ******************************
// 0=NONE
// 1=soft unsharp (UNSH1) 2=medium unsharp (UNSH2) 3=hard unsharp (UNSH3)
  menuFilter = new wxMenu;
  menuFilter->Append( ID_FILTER_0, _T("None"),
                       wxT("No filter"), wxITEM_RADIO);
  menuFilter->Append( ID_FILTER_1, _T("Soft unsharp masking"),
                       wxT("With big square box"), wxITEM_RADIO);
  menuFilter->Append( ID_FILTER_2, _T("Medium unsharp masking"),
                       wxT("With medium sized square box"), wxITEM_RADIO);
  menuFilter->Append( ID_FILTER_3, _T("Hard unsharp masking"),
                       wxT("With small square box"), wxITEM_RADIO);
  menuFilter->Append( ID_FILTER_4, _T("Very hard unsharp masking"),
                       wxT("With very small square box"), wxITEM_RADIO);
/***
* For popup menu (without unresolved modsq):
* 5 = VHC1: high contrast version 2008 without unresolved modsq
* 6 = VHC2: high contrast version 2015 without unresolved modsq
* 7 = VHC3: median profile version 2008 without unresolved modsq
* 8 = VHC4: median profile version 2015 without unresolved modsq
****/
  menuFilter->Append( ID_FILTER_5, _T("High contrast for autoc. (VHC1)"),
                      wxT("High contrast without unres. modsq: version 2008"),
                      wxITEM_RADIO);
  menuFilter->Append( ID_FILTER_6, _T("High contrast for autoc. (VHC2)"),
                      wxT("High contrast without unres. modsq: version 2015"), 
                      wxITEM_RADIO);
  menuFilter->Append( ID_FILTER_7, _T("High contrast for autoc. (VHC3)"),
                      wxT("Median profile without unres. modsq: version 2008"),
                      wxITEM_RADIO);
  menuFilter->Append( ID_FILTER_8, _T("High contrast for autoc. (VHC4)"),
                      wxT("Median profile without unres. modsq: version 2015"),
                      wxITEM_RADIO);

 PopupMenu1->Append(wxID_ANY, _T("Filter"), menuFilter);

return;
}
/***********************************************************************
* Shape menu (for jlp_splot images) 
*
***********************************************************************/
void JLP_wxGDev_Popup::CreateShapeMenu()
{
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
  if((gdev_graphic_type1 != 2) && (gdev_graphic_type1 != 3)) return;

  menuShape = new wxMenu;
  menuShape->Append(ID_ADD_LINE, _T("Add a line"),
                    wxT("Line selected with the mouse"));
  menuShape->Append(ID_ADD_RECTANGLE, _T("Add a rectangle"),
                    wxT("Rectangle selected with the mouse"));
  menuShape->Append(ID_ADD_CIRCLE, _T("Add a circle"),
                    wxT("Circle selected with the mouse"));
  menuShape->Append(ID_ADD_ELLIPSE, _T("Add an ellipse"),
                    wxT("Ellipse selected with the mouse"));
  menuShape->Append(ID_ADD_RING, _T("Add a ring"),
                    wxT("Ring selected with the mouse"));
  menuShape->Append(ID_CANCEL_SHAPE, _T("Cancel the last shape"),
                    wxT("Remove the last entered shape"));
  menuShape->Append(ID_REM_SHAPE, _T("Remove a shape"),
                    wxT("Remove a shape by clicking on it"));
  menuShape->Append(ID_ROTATE_SHAPE, _T("Rotate a shape"),
                    wxT("Rotate a shape with the mouse"));
  menuShape->Append(ID_MAGNIFY_SHAPE, _T("Reduce/enlarge a shape"),
                    wxT("Change the size of a shape with the mouse"));
  menuShape->Append(ID_MOVE_SHAPE, _T("Move a shape"),
                    wxT("Move a shape with the mouse"));
  menuShape->Append(ID_REM_ALL_SHAPES, _T("Remove all shapes"),
                    wxT("Remove all the shapes"));

  PopupMenu1->Append(wxID_ANY, _T("Shapes"), menuShape);

return;
}
/****************************************************************************
* Background color (for curves only)
*****************************************************************************/
void JLP_wxGDev_Popup::CreateColourBackgroundMenu()
{
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
  if((gdev_graphic_type1 == 2) || (gdev_graphic_type1 == 3)
     || (gdev_graphic_type1 == 5) || (gdev_graphic_type1 == 7)) return;

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
   
return;
}
/*************************************************************
* Setup menu for curves 
* Box type and box limits, for curves/or gsegraf only
*
*************************************************************/
void JLP_wxGDev_Popup::CreateSetupMenuForCurves()
{
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
// Not available gsegraf 2D image/curves for now (to be done later...)
  if((gdev_graphic_type1 == 2) || (gdev_graphic_type1 == 3)
     || (gdev_graphic_type1 > 3)) return;

// ***************** Box type submenu ******************************
  menuBoxType = new wxMenu;
  menuBoxType->Append(ID_BOX_TYPE0, _T("BoxType: big steps"),
                      _T("Mongo type"), wxITEM_RADIO);
  menuBoxType->Append(ID_BOX_TYPE1, _T("BoxType: small steps"),
                      _T("SciSoft type"), wxITEM_RADIO);
  menuBoxType->Append(ID_BOX_TYPE2, _T("BoxType: round-off limits"),
                      _T("Gsegrafix type"), wxITEM_RADIO);
  menuBoxType->Append(ID_BOX_TICKS_IN, _T("Axis ticks in"),
                      _T("With axis tiks in"), wxITEM_CHECK);
  menuBoxType->Append(ID_BOX_XGRID, _T("X grid"),
                      _T("With a grid on the X axis"), wxITEM_CHECK);
  menuBoxType->Append(ID_BOX_YGRID, _T("Y grid"),
                      _T("With a grid on the Y axis"), wxITEM_CHECK);

// For jlp_splot curves only:
  if(gdev_graphic_type1 == 1) {
    PopupMenu1->Append(Menu_Popup_Curve, _T("BoxType"),
                       menuBoxType);
    }

// ***************** Box limits menu **********************************
  menuBoxLimits = new wxMenu;
  menuBoxLimits->AppendCheckItem(ID_BOX_LIMITS_WITH_BOX,
                        _T("BoxLimits: with box"),
                        _T("Interactive selection of a rectangular box"));
  menuBoxLimits->Append(ID_BOX_LIMITS_AUTO, _T("BoxLimits: auto"),
                        _T("Best setup for full plot"));
  menuBoxLimits->Append(ID_BOX_LIMITS_MANUAL, _T("BoxLimits: manual"),
                        _T("Input of your choice"));

  if((gdev_graphic_type1 == 1) || (gdev_graphic_type1 == 4)
     || (gdev_graphic_type1 == 5)) {
    PopupMenu1->Append(Menu_Popup_Curve, _T("BoxLimits"),
                       menuBoxLimits);
    }

return;
}
