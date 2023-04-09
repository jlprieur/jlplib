/******************************************************************************
* jlp_wxgdev_popup_events.cpp
* JLP_wxGDev_Popup class
* Purpose:     Events used by the popup menu when displaying 
*              a curve/image with wxwidgets 
*
* Author:      JLP
* Version:     12/02/2017
******************************************************************************/
#include <stdio.h>
#include <stdlib.h>

#include "jlp_wxgdev_popup.h"
#include "jlp_wxgdev_popup_id.h"      // ID_INFO, ....


 BEGIN_EVENT_TABLE(JLP_wxGDev_Popup, wxWindow)
  EVT_MENU (ID_INFO_IMAGE,  JLP_wxGDev_Popup::InfoImage)
  EVT_MENU (ID_DISPLAY_IMAGE_3D,  JLP_wxGDev_Popup::OnDisplayDbleImage3D)
  EVT_MENU (ID_DISPLAY_IMAGE_CONTOURS,  JLP_wxGDev_Popup::OnDisplayDbleImageContours)
  EVT_MENU (ID_INFO_CURVE,  JLP_wxGDev_Popup::OnInfoCurve)
  EVT_MENU (ID_SAVE,  JLP_wxGDev_Popup::OnSave)
  EVT_MENU (ID_SAVE_TO_PST,  JLP_wxGDev_Popup::OnSaveToPostscript)
  EVT_MENU(wxID_COPY, JLP_wxGDev_Popup::OnCopy)
  EVT_MENU(wxID_PASTE, JLP_wxGDev_Popup::OnPaste)

// Processing:
  EVT_MENU (ID_STATISTICS,  JLP_wxGDev_Popup::OnSetInteractiveProcessing)
  EVT_MENU (ID_PHOTOMETRY,  JLP_wxGDev_Popup::OnSetInteractiveProcessing)
  EVT_MENU (ID_ASTROMETRY,  JLP_wxGDev_Popup::OnSetInteractiveProcessing)
  EVT_MENU (ID_SLICE,  JLP_wxGDev_Popup::OnSetInteractiveProcessing)
  EVT_MENU (ID_GET_COORDINATES,  JLP_wxGDev_Popup::OnSetInteractiveProcessing)

// Labels:
  EVT_MENU (ID_ADD_LABEL,  JLP_wxGDev_Popup::OnAddLabel)
  EVT_MENU (ID_ADD_SCALE,  JLP_wxGDev_Popup::OnAddScaleBar)
  EVT_MENU (ID_ADD_NORTH_EAST,  JLP_wxGDev_Popup::OnAddNorthEastLabel)
  EVT_MENU (ID_REM_LABEL,  JLP_wxGDev_Popup::OnRemoveLabel)
  EVT_MENU (ID_REM_SCALE,  JLP_wxGDev_Popup::OnRemoveScaleBar)
  EVT_MENU (ID_REM_NORTH_EAST,  JLP_wxGDev_Popup::OnRemoveNorthEastLabel)
  EVT_MENU (ID_LABEL_CONTOURS,  JLP_wxGDev_Popup::OnSetLabelContours)

// Shapes:
  EVT_MENU (ID_ADD_LINE,    JLP_wxGDev_Popup::OnAddShape)
  EVT_MENU (ID_ADD_RECTANGLE,    JLP_wxGDev_Popup::OnAddShape)
  EVT_MENU (ID_ADD_CIRCLE,  JLP_wxGDev_Popup::OnAddShape)
  EVT_MENU (ID_ADD_ELLIPSE, JLP_wxGDev_Popup::OnAddShape)
  EVT_MENU (ID_ADD_RING,    JLP_wxGDev_Popup::OnAddShape)
  EVT_MENU (ID_REM_SHAPE,   JLP_wxGDev_Popup::OnChangeShape)
  EVT_MENU (ID_MOVE_SHAPE,   JLP_wxGDev_Popup::OnChangeShape)
  EVT_MENU (ID_CANCEL_SHAPE,   JLP_wxGDev_Popup::OnChangeShape)
  EVT_MENU (ID_ROTATE_SHAPE,   JLP_wxGDev_Popup::OnChangeShape)
  EVT_MENU (ID_MAGNIFY_SHAPE,   JLP_wxGDev_Popup::OnChangeShape)
  EVT_MENU (ID_REM_ALL_SHAPES,   JLP_wxGDev_Popup::OnChangeShape)

// Setup: cursor
  EVT_MENU(ID_CURSOR_CROSS, JLP_wxGDev_Popup::OnSetCursor)
  EVT_MENU(ID_CURSOR_BIG_CROSS, JLP_wxGDev_Popup::OnSetCursor)
  EVT_MENU(ID_CURSOR_CROSSHAIR, JLP_wxGDev_Popup::OnSetCursor)
  EVT_MENU(ID_CURSOR_CROSSHAIR1, JLP_wxGDev_Popup::OnSetCursor)
  EVT_MENU(ID_CURSOR_ARROW, JLP_wxGDev_Popup::OnSetCursor)
  EVT_MENU(ID_CURSOR_DOWN_ARROW, JLP_wxGDev_Popup::OnSetCursor)

// Setup: pen colour
  EVT_MENU(ID_BLACK_PEN_COLOUR, JLP_wxGDev_Popup::OnSetPenColour)
  EVT_MENU(ID_RED_PEN_COLOUR, JLP_wxGDev_Popup::OnSetPenColour)
  EVT_MENU(ID_GREEN_PEN_COLOUR, JLP_wxGDev_Popup::OnSetPenColour)
  EVT_MENU(ID_BLUE_PEN_COLOUR, JLP_wxGDev_Popup::OnSetPenColour)
  EVT_MENU(ID_WHITE_PEN_COLOUR, JLP_wxGDev_Popup::OnSetPenColour)

// Setup: background colour
  EVT_MENU(ID_BLACK_BACK_COLOUR, JLP_wxGDev_Popup::OnSetBackgdColour)
  EVT_MENU(ID_GREY_BACK_COLOUR, JLP_wxGDev_Popup::OnSetBackgdColour)
  EVT_MENU(ID_YELLOW_BACK_COLOUR, JLP_wxGDev_Popup::OnSetBackgdColour)
  EVT_MENU(ID_WHITE_BACK_COLOUR, JLP_wxGDev_Popup::OnSetBackgdColour)
  EVT_MENU(ID_BOX_TYPE0, JLP_wxGDev_Popup::OnSetBoxType)
  EVT_MENU(ID_BOX_TYPE1, JLP_wxGDev_Popup::OnSetBoxType)
  EVT_MENU(ID_BOX_TYPE2, JLP_wxGDev_Popup::OnSetBoxType)
  EVT_MENU(ID_BOX_TICKS_IN, JLP_wxGDev_Popup::OnSetBoxType)
  EVT_MENU(ID_BOX_XGRID, JLP_wxGDev_Popup::OnSetBoxType)
  EVT_MENU(ID_BOX_YGRID, JLP_wxGDev_Popup::OnSetBoxType)

// Box limits;
  EVT_MENU(ID_BOX_LIMITS_MANUAL, JLP_wxGDev_Popup::OnChangeBoxLimits)
  EVT_MENU(ID_BOX_LIMITS_AUTO, JLP_wxGDev_Popup::OnChangeBoxLimits)
  EVT_MENU(ID_BOX_LIMITS_WITH_BOX, JLP_wxGDev_Popup::OnChangeBoxLimits)
  EVT_MENU(ID_BOX_LIMITS_ZOOM_IN, JLP_wxGDev_Popup::OnChangeBoxLimits)
  EVT_MENU(ID_BOX_LIMITS_ZOOM_OUT, JLP_wxGDev_Popup::OnChangeBoxLimits)

// Setup: zoom
  EVT_MENU(ID_ZOOM_C2, JLP_wxGDev_Popup::OnChangeZoom)
  EVT_MENU(ID_ZOOM_C3, JLP_wxGDev_Popup::OnChangeZoom)
  EVT_MENU(ID_ZOOM_C4, JLP_wxGDev_Popup::OnChangeZoom)
  EVT_MENU(ID_ZOOM_C5, JLP_wxGDev_Popup::OnChangeZoom)
  EVT_MENU(ID_ZOOM_C6, JLP_wxGDev_Popup::OnChangeZoom)
  EVT_MENU(ID_ZOOM_C7, JLP_wxGDev_Popup::OnChangeZoom)
  EVT_MENU(ID_ZOOM_C8, JLP_wxGDev_Popup::OnChangeZoom)
  EVT_MENU_RANGE(ID_ZOOM1, ID_ZOOM40, JLP_wxGDev_Popup::OnChangeZoom)

/* OLD (before using EVT_MENU_RANGE:
  EVT_MENU(ID_ZOOM2, JLP_wxGDev_Popup::OnChangeZoom)
  EVT_MENU(ID_ZOOM3, JLP_wxGDev_Popup::OnChangeZoom)
  EVT_MENU(ID_ZOOM4, JLP_wxGDev_Popup::OnChangeZoom)
  EVT_MENU(ID_ZOOM5, JLP_wxGDev_Popup::OnChangeZoom)
  EVT_MENU(ID_ZOOM6, JLP_wxGDev_Popup::OnChangeZoom)
  EVT_MENU(ID_ZOOM8, JLP_wxGDev_Popup::OnChangeZoom)
  EVT_MENU(ID_ZOOM10, JLP_wxGDev_Popup::OnChangeZoom)
  EVT_MENU(ID_ZOOM15, JLP_wxGDev_Popup::OnChangeZoom)
  EVT_MENU(ID_ZOOM20, JLP_wxGDev_Popup::OnChangeZoom)
  EVT_MENU(ID_ZOOM40, JLP_wxGDev_Popup::OnChangeZoom)
*/

/* lut_type: 'l'=log rainbow1 'r'=rainbow2 's'=saw 'g'=gray 'c'=curves
*           'p'=pisco
* RAIN1, RAIN2, SAW, GRAY, CUR, PISCO, REV
*/
  EVT_MENU(ID_LUT_RAIN1, JLP_wxGDev_Popup::OnChangeLUT)
  EVT_MENU(ID_LUT_RAIN2, JLP_wxGDev_Popup::OnChangeLUT)
  EVT_MENU(ID_LUT_SAW, JLP_wxGDev_Popup::OnChangeLUT)
  EVT_MENU(ID_LUT_GRAY, JLP_wxGDev_Popup::OnChangeLUT)
  EVT_MENU(ID_LUT_CUR, JLP_wxGDev_Popup::OnChangeLUT)
  EVT_MENU(ID_LUT_PISCO, JLP_wxGDev_Popup::OnChangeLUT)
  EVT_MENU(ID_LUT_REV, JLP_wxGDev_Popup::OnChangeLUT)
/*
* ITT_1:
*         "Lin" (Linear scale)
*         "Log" (Linear scale)
* LIN, LOG
*/
   EVT_MENU(ID_ITT_LIN, JLP_wxGDev_Popup::OnChangeITT_is_linear)
   EVT_MENU(ID_ITT_LOG, JLP_wxGDev_Popup::OnChangeITT_is_linear)

/* ITT thresholds:
*         "Direct" (Thresholds entered by the user)
*         "Box" (Automatic: from a rectangular box)
*         "MinMax" (Automatic scale: Min Max)
*         "Median" (Automatic scale: Median)
*         "HC" (Automatic scale:  High contrast)
* BOX, DIRECT, AUTO_MINMAX, AUTO_HC, ...
*/
   EVT_MENU(ID_THR_BOX, JLP_wxGDev_Popup::OnChangeITT_thresholds)
   EVT_MENU(ID_THR_DIRECT, JLP_wxGDev_Popup::OnChangeITT_thresholds)
   EVT_MENU(ID_THR_DIRECT1, JLP_wxGDev_Popup::OnChangeITT_thresholds)
   EVT_MENU(ID_THR_AUTO_MINMAX, JLP_wxGDev_Popup::OnChangeITT_thresholds)
   EVT_MENU(ID_THR_AUTO_MEDIAN, JLP_wxGDev_Popup::OnChangeITT_thresholds)
   EVT_MENU(ID_THR_AUTO_HC, JLP_wxGDev_Popup::OnChangeITT_thresholds)
   EVT_MENU(ID_THR_AUTO_VHC, JLP_wxGDev_Popup::OnChangeITT_thresholds)

// Filter
   EVT_MENU (ID_FILTER_0, JLP_wxGDev_Popup::OnSelectFilter)
   EVT_MENU (ID_FILTER_1, JLP_wxGDev_Popup::OnSelectFilter)
   EVT_MENU (ID_FILTER_2, JLP_wxGDev_Popup::OnSelectFilter)
   EVT_MENU (ID_FILTER_3, JLP_wxGDev_Popup::OnSelectFilter)
   EVT_MENU (ID_FILTER_4, JLP_wxGDev_Popup::OnSelectFilter)
   EVT_MENU (ID_FILTER_5, JLP_wxGDev_Popup::OnSelectFilter)
   EVT_MENU (ID_FILTER_6, JLP_wxGDev_Popup::OnSelectFilter)
   EVT_MENU (ID_FILTER_7, JLP_wxGDev_Popup::OnSelectFilter)
   EVT_MENU (ID_FILTER_8, JLP_wxGDev_Popup::OnSelectFilter)

// Gsegraf
   EVT_MENU (ID_GSEG_AXIS_ROTATE, JLP_wxGDev_Popup::OnViewAxisRotate)
   EVT_MENU (ID_GSEG_AXIS_LIMITS, JLP_wxGDev_Popup::OnViewChangeAxisLimits)

END_EVENT_TABLE()

/**
****************** OLD SETTINGS ******************************************
   win0->Connect(wxEVT_SCROLL_THUMBTRACK, 
                 wxScrollWinEventHandler(JLP_wxGDev_Popup::OnScrollWindow));
// Mouse events:
   win0->Connect(wxEVT_MOTION,
                 wxMouseEventHandler(JLP_wxGDev_Popup::OnMotion), NULL, this);
   win0->Connect(wxEVT_LEFT_DOWN,
                 wxMouseEventHandler(JLP_wxGDev_Popup::OnLeftDown), NULL, this);
   win0->Connect(wxEVT_LEFT_UP,
                 wxMouseEventHandler(JLP_wxGDev_Popup::OnLeftUp), NULL, this);

// catch paint events
   this->Connect(wxEVT_PAINT,
                 wxPaintEventHandler(JLP_wxGDev_Popup::OnPaint));

// Menu: Save/copy
   win0->Connect(wxID_COPY, wxEVT_COMMAND_MENU_SELECTED,
                 wxCommandEventHandler(JLP_wxGDev_Popup::OnCopy));
   win0->Connect(wxID_PASTE, wxEVT_COMMAND_MENU_SELECTED,
                 wxCommandEventHandler(JLP_wxGDev_Popup::OnPaste));

// Menu: Processing
   win0->Connect(ID_STATISTICS, wxEVT_COMMAND_MENU_SELECTED,
             wxCommandEventHandler(JLP_wxGDev_Popup::OnSetInteractiveProcessing));
   win0->Connect(ID_PHOTOMETRY, wxEVT_COMMAND_MENU_SELECTED,
             wxCommandEventHandler(JLP_wxGDev_Popup::OnSetInteractiveProcessing));
   win0->Connect(ID_ASTROMETRY, wxEVT_COMMAND_MENU_SELECTED,
             wxCommandEventHandler(JLP_wxGDev_Popup::OnSetInteractiveProcessing));
   win0->Connect(ID_SLICE, wxEVT_COMMAND_MENU_SELECTED,
             wxCommandEventHandler(JLP_wxGDev_Popup::OnSetInteractiveProcessing));
   win0->Connect(ID_GET_COORDINATES, wxEVT_COMMAND_MENU_SELECTED,
             wxCommandEventHandler(JLP_wxGDev_Popup::OnSetInteractiveProcessing));

// Menu: Labels 
   win0->Connect(ID_ADD_LABEL, wxEVT_COMMAND_MENU_SELECTED,
             wxCommandEventHandler(JLP_wxGDev_Popup::OnAddLabel));
   win0->Connect(ID_REM_LABEL, wxEVT_COMMAND_MENU_SELECTED,
             wxCommandEventHandler(JLP_wxGDev_Popup::OnRemoveLabel));
   win0->Connect(ID_LABEL_CONTOURS, wxEVT_COMMAND_MENU_SELECTED,
             wxCommandEventHandler(JLP_wxGDev_Popup::OnSetLabelContour));

// Menu: Setup/cursor
   win0->Connect(ID_CURSOR_CROSS, wxEVT_MENU,
             wxCommandEventHandler(JLP_wxGDev_Popup::OnSetCursor));
   win0->Connect(ID_CURSOR_BIG_CROSS, wxEVT_COMMAND_MENU_SELECTED,
             wxCommandEventHandler(JLP_wxGDev_Popup::OnSetCursor));
   win0->Connect(ID_CURSOR_CROSSHAIR, wxEVT_COMMAND_MENU_SELECTED,
             wxCommandEventHandler(JLP_wxGDev_Popup::OnSetCursor));
   win0->Connect(ID_CURSOR_CROSSHAIR1, wxEVT_COMMAND_MENU_SELECTED,
             wxCommandEventHandler(JLP_wxGDev_Popup::OnSetCursor));
   win0->Connect(ID_CURSOR_ARROW, wxEVT_COMMAND_MENU_SELECTED,
             wxCommandEventHandler(JLP_wxGDev_Popup::OnSetCursor));
   win0->Connect(ID_CURSOR_DOWN_ARROW, wxEVT_COMMAND_MENU_SELECTED,
             wxCommandEventHandler(JLP_wxGDev_Popup::OnSetCursor));

*/
