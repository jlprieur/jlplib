/******************************************************************************
* jlp_cgdev_wxwid_id.h 
* JLP_wxGseg_Canvas class
* Purpose:     Popup menu for displaying a curve with wxwidgets
*
* Author:      JLP 
* Version:     06/05/2016
******************************************************************************/
#ifndef _jlp_cgdev_wxwid_id
#define _jlp_cgdev_wxwid_id
enum
{
    Menu_Popup_Curve = 7000,
    Menu_Popup_Setup,
    Menu_Popup_Label,
    Menu_Popup_Process,
    ID_INFO_CURVE,
    ID_SAVE_TO_PST,
    ID_SAVE,

// Cursor:
    ID_CURSOR_CROSSHAIR,
    ID_CURSOR_CROSSHAIR1,
    ID_CURSOR_ARROW,
    ID_CURSOR_CROSS,
    ID_CURSOR_BIG_CROSS,
    ID_CURSOR_DOWN_ARROW,

// Pen colour:
    ID_BLACK_PEN_COLOUR,
    ID_RED_PEN_COLOUR,
    ID_GREEN_PEN_COLOUR,
    ID_BLUE_PEN_COLOUR,
    ID_WHITE_PEN_COLOUR,
    ID_BLACK_BACK_COLOUR,
    ID_YELLOW_BACK_COLOUR,
    ID_GREY_BACK_COLOUR,
    ID_WHITE_BACK_COLOUR,

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
    ID_BOX_LIMITS_MANUAL1,
    ID_BOX_LIMITS_WITH_BOX,
    ID_BOX_LIMITS_ZOOM_IN,
    ID_BOX_LIMITS_ZOOM_OUT,

// Processing:
    ID_STATISTICS,
    ID_PHOTOMETRY,
    ID_ASTROMETRY,

// Labels:
    ID_IDLE_LABEL,
    ID_ADD_LABEL,
    ID_REM_LABEL

};
#endif
