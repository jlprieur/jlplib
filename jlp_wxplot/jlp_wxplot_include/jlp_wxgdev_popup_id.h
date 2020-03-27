/******************************************************************************
* jlp_wxgdev_popup_id.h
* JLP_GDev_wxWID class
* Purpose: Popup menu for displaying a curve with wxwidgets
*
* Author:  JLP
* Version: 12/09/2017
******************************************************************************/
#ifndef _jlp_wxgdev_popup_id_h
#define _jlp_wxgdev_popup_id_h

enum
{
    Menu_Help_About = wxID_ABOUT,
    Menu_Popup_Curve = 7000,
    Menu_Popup_Setup,
    Menu_Popup_Label,
    Menu_Popup_Process,
    ID_INFO_CURVE,
    ID_INFO_IMAGE,
    ID_SAVE_TO_PST,
    ID_SAVE,

// Box type:
    ID_BOX_TYPE0,
    ID_BOX_TYPE1,
    ID_BOX_TYPE2,
    ID_BOX_TICKS_IN,
    ID_BOX_XGRID,
    ID_BOX_YGRID,

// Box limits:
    ID_BOX_LIMITS_AUTO,
    ID_BOX_LIMITS_MANUAL,
    ID_BOX_LIMITS_WITH_BOX,
    ID_BOX_LIMITS_ZOOM_IN,
    ID_BOX_LIMITS_ZOOM_OUT,

// Processing:
    ID_STATISTICS,
    ID_PHOTOMETRY,
    ID_ASTROMETRY,
    ID_SLICE,
    ID_LABEL_CONTOURS,
    ID_GET_COORDINATES,

// ITT: LIN, LOG
    ID_ITT_LIN,
    ID_ITT_LOG,

// ITT thresholds: AUTO_MINMAX, AUTO_HC, AUTO_VHC, BOX
    ID_THR_DIRECT,
    ID_THR_DIRECT1,
    ID_THR_BOX,
    ID_THR_AUTO_MINMAX,
    ID_THR_AUTO_MEDIAN,
    ID_THR_AUTO_HC,
    ID_THR_AUTO_VHC,

// Labels:
    ID_IDLE_LABEL,
    ID_ADD_LABEL,
    ID_ADD_SCALE,
    ID_ADD_NORTH_EAST,
    ID_REM_LABEL,
    ID_REM_SCALE,
    ID_REM_NORTH_EAST,

// Shapes:
    ID_IDLE_SHAPE,
    ID_ADD_LINE,
    ID_ADD_RECTANGLE,
    ID_ADD_CIRCLE,
    ID_ADD_ELLIPSE,
    ID_ADD_RING,
    ID_CANCEL_SHAPE,
    ID_REM_SHAPE,
    ID_REM_ALL_SHAPES,
    ID_ROTATE_SHAPE,
    ID_MAGNIFY_SHAPE,
    ID_MOVE_SHAPE,

// LUT: RAIN1, RAIN2, SAW, GRAY, CUR, PISCO, REV
    ID_LUT_RAIN1,
    ID_LUT_RAIN2,
    ID_LUT_SAW,
    ID_LUT_GRAY,
    ID_LUT_CUR,
    ID_LUT_PISCO,
    ID_LUT_REV,

// Cursor: 
    ID_CURSOR_CROSSHAIR,
    ID_CURSOR_CROSSHAIR1,
    ID_CURSOR_ARROW,
    ID_CURSOR_CROSS,
    ID_CURSOR_BIG_CROSS,
    ID_CURSOR_DOWN_ARROW,

// Pen color:
    ID_BLACK_PEN_COLOUR,
    ID_RED_PEN_COLOUR,
    ID_GREEN_PEN_COLOUR,
    ID_BLUE_PEN_COLOUR,
    ID_WHITE_PEN_COLOUR,
    ID_BLACK_BACK_COLOUR,
    ID_YELLOW_BACK_COLOUR,
    ID_GREY_BACK_COLOUR,
    ID_WHITE_BACK_COLOUR,

// ZOOM:
    ID_ZOOM1,
    ID_ZOOM2,
    ID_ZOOM3,
    ID_ZOOM4,
    ID_ZOOM5,
    ID_ZOOM6,
    ID_ZOOM8,
    ID_ZOOM10,
    ID_ZOOM15,
    ID_ZOOM20,
    ID_ZOOM40,
    ID_ZOOM_C2,
    ID_ZOOM_C3,
    ID_ZOOM_C4,
    ID_ZOOM_C5,
    ID_ZOOM_C6,
    ID_ZOOM_C7,
    ID_ZOOM_C8,

// Gsegraf:
    ID_GSEG_AXIS_LIMITS,
    ID_GSEG_AXIS_ROTATE,
    ID_DISPLAY_IMAGE_3D,
    ID_DISPLAY_IMAGE_CONTOURS,

// Filters:
    ID_FILTER_0,
    ID_FILTER_1,
    ID_FILTER_2,
    ID_FILTER_3,
    ID_FILTER_4,
    ID_FILTER_5,
    ID_FILTER_6,
    ID_FILTER_7,
    ID_FILTER_8,
    ID_FILTER_9,
    ID_FILTER_10,
    ID_FILTER_11,
    ID_FILTER_12,
    ID_FILTER_13,
    ID_FILTER_14

};

#endif
