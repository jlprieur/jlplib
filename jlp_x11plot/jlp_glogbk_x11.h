/*************************************************************************
* jlp_logbk_x11.h
* JLP_GLogbk_X11 Class: routine to create a popup window 
*  in order to display the logbook (with the values of some parameters)
*
* C++ version with classes
*
* JLP
* Version 16/11/2008 
**************************************************************************/
#ifdef JLP_USE_X11   /* New flag to disable X11 if necessary */

#ifndef __jlp_glogbk_x11_h                     /* sentry */
#define __jlp_glogbk_x11_h

#include <stdio.h>
#include <math.h>
#include <ctype.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/cursorfont.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>            /* For Color Maps */
#include <X11/keysym.h>            /* For key symbols */
#include <X11/keysymdef.h>            /* For key symbols */

#include "jlp_glogbk.h"

// Definition of JLP_GLogbk_X11 class:
class JLP_GLogbk_X11 : public JLP_GLogbk {

public:
  
/*******************************************************************
* Constructor:
*******************************************************************/
JLP_GLogbk_X11(const char *title) : JLP_GLogbk(title) {
int status;
 Setup();

// Open new window with logbook:
 window_is_open = 0;
// WARNING: window_is_open is set to one in OpenLogbookBox
 status = OpenLogbookBox(title);

 return;
};

/*******************************************************************
* Destructor:
* (Should be declared as virtual!)
* (WARNING: only called if declared as JLP_GLogbk_X11,
* otherwise call destructor of JLP_GLogbk !)
*******************************************************************/
virtual ~JLP_GLogbk_X11() {
   return;
}
// Routine to close the window:
int CloseWindow() {
  if(!window_is_open) {
    fprintf(stderr, "CloseWindow/Error: already closed!\n");
// Return 0, otherwise may not declare window closed ... */
    return(0);
  }

   XFreeFont(Logbook_display, Logbook_Font);
   XFreeGC(Logbook_display, Logbook_gc);
   XCloseDisplay(Logbook_display);
   window_is_open = 0;
   return(0);
};

// Defined in jlp_glogbk_x11.cpp:
int WaitForClose(int *process_data);

protected:

// Defined in jlp_glogbk_x11.cpp:
  int OpenLogbookBox(const char *title);
  int DrawLogbookBox();
  int CreateCloseButton(int x_ul, int y_ul, int d_width, int d_height);
  int CreateProcessButton(int x_ul, int y_ul, int d_width, int d_height);

private:

// Private variables:
Window Logbook_win, CloseButton, ProcessButton;
GC Logbook_gc;
Colormap Logbook_cmap;
Display *Logbook_display;
XFontStruct *Logbook_Font;
};

#endif    /* __jlp_glogbk_x11_h sentry */

#endif  /* End of JLP_USE_X11 */
