/**************************************************************************
* jlp_wxgdev_popup.cpp
* JLP_wxGdev_Popup class (for Curves/Images)
*
* JLP
* Version 12/09/2017
**************************************************************************/
#include "jlp_wxgdev_popup.h"       // JLP_wxGdev_popup class

#include "jlp_wxgdev_popup_events.h"  // To include at least one the events

/*************************************************************************
* Constructor
*************************************************************************/
JLP_wxGDev_Popup::JLP_wxGDev_Popup(JLP_GDev_wxWID *jlp_gdev_wxwid0,
                                   int gdev_graphic_type0)
{
int update_display;

// Settings:
  wxGDev_SETTINGS wxgdev_settings0;

// Save input parameters to private variables
 jlp_gdev_wxwid1 = jlp_gdev_wxwid0;

 gdev_graphic_type1 = gdev_graphic_type0;

 initialized = 0;
 Main_Init();

// Create popup menu:
 CreatePopupMenu();

// Load settings (pen color, pen width, background color, etc):
 jlp_gdev_wxwid1->GDevGet_wxGDevSettings(&wxgdev_settings0);

// Configure this menu with those settings:
 update_display = 1; // Change display here to synchronize it with settings
 UpdatePopupMenu(wxgdev_settings0, update_display);

 initialized = 1234;

return;
}
/*************************************************************************
* 
*************************************************************************/
void JLP_wxGDev_Popup::Main_Init()
{
// Popup menu:
    PopupMenu1 = NULL;
    menuInfo = NULL;
    menuSetup = NULL;
    menuCursor = NULL;
    menuLUT = NULL;
    menuITT1 = NULL;
    menuITT2 = NULL;
    menuFilter = NULL;
    menuProcess = NULL;
    menuPen = NULL;
    menuZoom = NULL;
    menuLabel = NULL;
    menuShape = NULL;
    menuBackgd = NULL;
    menuBoxType = NULL;
    menuBoxLimits = NULL;
    menuGsegraf = NULL;

    software_event1 = 0;
}
