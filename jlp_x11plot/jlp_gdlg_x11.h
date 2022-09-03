/*************************************************************************
* jlp_gdlg_x11.h
* JLP_GDlg_X11 Class: routine to create a popup window 
*  in order to obtain the values of some parameters
*
* C++ version with classes
*
* JLP
* Version 16/11/2006 
**************************************************************************/
#ifdef JLP_USE_X11           /* New flag to disable X11 if necessary */

#ifndef __jlp_gdlg_x11_h                /* sentry */
#define __jlp_gdlg_x11_h

#include <stdio.h>                 // stderr
#include <string.h>                // strcpy()

#include "jlp_gdlg.h"

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/cursorfont.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>            /* For Color Maps */
#include <X11/keysym.h>           /* For key symbols */
#include <X11/keysymdef.h>        /* For key symbols */

// Definition of JLP_GDlg_X11 class:
class JLP_GDlg_X11 : public JLP_GDlg {

public:
  
/*******************************************************************
* Constructor:
*******************************************************************/
JLP_GDlg_X11(const char *title, const int small_size) 
{

// Initialize common parameters:
 JLP_GDlg::Setup();

// Open new window with logbook:
 window_is_open = 0;
// WARNING: window_is_open is set to one in OpenDialogBox
 OpenDialogBox(title, small_size);

return;
}

/*******************************************************************
* Destructor:
* (Should be declared as virtual!)
* (WARNING: only called if declared as JLP_GDlg_X11,
* otherwise call destructor of JLP_GDlg !)
*******************************************************************/
virtual ~JLP_GDlg_X11() {
   return;
}

// Routine to close the window:
int CloseWindow() {
  if(!window_is_open) {
    fprintf(stderr, "CloseWindow/Error: already closed!\n");
// Return 0, otherwise may not declare window closed ... */
    return(0);
  }

   XFreeFont(Dialog_display, Dialog_Font);
   XFreeGC(Dialog_display, Dialog_gc);
   XCloseDisplay(Dialog_display);
   window_is_open = 0;
   return(0);
};

/* Contained in jlp_dlg_x11.cpp: */

int OpenDialogBox(const char *title, const int small_size);

int CreateOKCancel(int y_ul);

int WaitForOK(char svalues[][80], int *is_checked, int nvalues, int& is_OK);

int CreateWidgets(char label0[][80], int *widget_type0, int *radio_group0,
                  char *val_type0, char svalue0[][80], int *is_checked0, 
                  int nwidgets0);

int CreateStaticLabel(char *label0, int x_ul, int y_ul, int iJWid,
                      int height0, int &n_created_lines);
int CreateCheckButton(JLP_WIDGET &JWid0, int x_ul, int y_ul);
int CreateRadioButton(JLP_WIDGET &JWid0, int x_ul, int y_ul);
int CreateEditButton(JLP_WIDGET &JWid0, int x_ul, int y_ul);

/* Accessors: */
int FromJWidgetToWindow_ID(int JWid_id, Window *win) {
    if(JWid_id >= 0 && JWid_id < 100) { 
      *win = Widgets_win_id[JWid_id]; 
      return(0);
      } else {
      *win = 0;
      return(-1);
      }
}
int FromWindowToJWidget_ID(Window win, int *JWid_id) {
    register int i;
    *JWid_id = -1;
    for(i = 0; i < 100; i++) {
      if(win == Widgets_win_id[i]) {
         *JWid_id = i; 
         return(0);
        }
      }
    return(-1);
    }

private:

int DrawDialogBox(int active_Edit);
int DrawTextCursor(int active_Edit);
int DrawBlinkTextCursor(int active_Edit);
int HandleEventOnEditButton(XEvent& event, JLP_WIDGET &JWid0);
int RemoveRightFromString(JLP_WIDGET &JWid0);
int RemoveLeftFromString(JLP_WIDGET &JWid0);
int AddToString(JLP_WIDGET &JWid0, char cc);

// Private variables:

Window Dialog_win, OK_Button, Cancel_Button;
Window Widgets_win_id[100];
GC Dialog_gc;
Colormap Dialog_cmap;
Display *Dialog_display;
XFontStruct *Dialog_Font;

};

#endif    /* __jlp_gdlg_x11_h sentry */

#endif    /* End of JLP_USE_X11 */
