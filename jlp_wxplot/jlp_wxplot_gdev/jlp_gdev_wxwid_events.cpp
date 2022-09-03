/******************************************************************************
* jlp_gdev_wxwid_events.cpp
* JLP_GDev_wxWID class
* Purpose:     Events used by the popup menu when displaying 
*              a curve/image with wxwidgets 
*
* Author:      JLP
* Version:     12/02/2017
******************************************************************************/
#include <stdio.h>
#include <stdlib.h>

#include "jlp_gdev_wxwid.h"
#include "jlp_wx_gpanel.h"          // JLp_wx_GPanel
#include "jlp_wxgdev_popup.h"       // JLP_wxGDev_Popup class
#include "jlp_wxgdev_popup_id.h"    // ID_CURSOR... 
#include "jlp_wxgdev_labels.h"
#include "jlp_wx_cursor.h"
#include "jlp_itt1.h"

/**************************************************************************
* JLP_GDev_wxWID class (derived from wxPanel)
**************************************************************************/
// JLP_GDev_wxWID

BEGIN_EVENT_TABLE(JLP_GDev_wxWID, wxPanel)
// Check in wx/event.h for the full list:
/* no longer used
  EVT_LEAVE_WINDOW(JLP_GDev_wxWID::OnLeaveWindow)
*/
// catch mouse events
  EVT_MOTION(JLP_GDev_wxWID::OnMotion)
  EVT_LEFT_DOWN(JLP_GDev_wxWID::OnLeftDown)
  EVT_LEFT_UP(JLP_GDev_wxWID::OnLeftUp)
  EVT_RIGHT_UP(JLP_GDev_wxWID::OnRightUp)

// catch paint events
  EVT_PAINT(JLP_GDev_wxWID::OnPaint)

// catch size events
  EVT_SIZE (JLP_GDev_wxWID::OnResize)

// Setup: cursor
/* Test version
  EVT_MENU(ID_CURSOR_CROSS, JLP_GDev_wxWID::OnPopupClick) 
  EVT_MENU(ID_CURSOR_BIG_CROSS, JLP_GDev_wxWID::OnPopupClick) 
  EVT_MENU(ID_CURSOR_CROSSHAIR, JLP_GDev_wxWID::OnPopupClick) 
  EVT_MENU(ID_CURSOR_CROSSHAIR1, JLP_GDev_wxWID::OnPopupClick)
  EVT_MENU(ID_CURSOR_ARROW, JLP_GDev_wxWID::OnPopupClick) 
  EVT_MENU(ID_CURSOR_DOWN_ARROW, JLP_GDev_wxWID::OnPopupClick)
*/

END_EVENT_TABLE()

/*************************************************************
* Display the popup menu
*************************************************************/
void JLP_GDev_wxWID::ShowPopupMenu()
{
int update_display;
/* Not used since not working...
int width1, height1;
wxDC *dc1 = new wxClientDC(this);
*/

wxMenu *popmenu0;

if(m_popup_menu1 == NULL) return;

// Make menu items checked correctly, according to image parameters:
  update_display = 0; // Do not change display here (since should be already synchronized when creating Popup object) 
  m_popup_menu1->UpdatePopupMenu(wxgdev_settings1, update_display);

  popmenu0 = m_popup_menu1->GetPopupMenu1();

// Activate event handler of m_popup_menu1:
// and set it as the event handler of this window:
  SetEventHandler(m_popup_menu1);

/* Not used since not working...
// For curves, update screen to avoid grey window:
  if(Jgc0.gdev_graphic_type != 3) {
    dc1->GetSize(&width1, &height1);
    wxGDev_RenderForCurves(dc1, width1, height1);
    }
*/

// Display the popup menu:
// It is recommended to not explicitly specify coordinates
// when calling PopupMenu in response to mouse click, 
// because some of the ports (namely, wxGTK) can do a better job
// of positioning the menu in that case
// (hence should not call  PopupMenu(PopupMenu1, pos.x, pos.y);)
  PopupMenu(popmenu0);

// Go back to the previous status with the events handled by the current window
  SetEventHandler(this);

// Send a paint event to erase the underlying box that would appear as black
  wxGdev_Refresh();

return;
}
/****************************************************************************
*
****************************************************************************/
/* Test version : no longer used
void JLP_GDev_wxWID::OnPopupClick(wxCommandEvent &evt)
 {
printf("DEBUG: popup event received: OK ?\n");
   switch(evt.GetId()) {
      case ID_CURSOR_CROSS:
      case ID_CURSOR_BIG_CROSS:
      case ID_CURSOR_CROSSHAIR:
      case ID_CURSOR_ARROW:
      case ID_CURSOR_DOWN_ARROW:
         m_popup_menu1->OnSetCursor(evt);
         break;
      case ID_ADD_LABEL:
         m_popup_menu1->OnSetCursor(evt);
         break;
   }
}
*/
