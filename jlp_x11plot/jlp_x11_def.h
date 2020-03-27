/**************************************************************************
* definitions of structures used by jlp_igdev.cpp and jlp_cgdev.cpp
*
* JLP
* Version 11/01/2007
**************************************************************************/
#if JLP_USE_X11 /* New flag to disable X11 if necessary */

#ifndef __jlp_X11_def_h                     /* sentry */
#define __jlp_X11_def_h

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

#define NBUT_MAX 20               // Maximum number of buttons
#define POPUP_NITEMS_MAX 20       // Maximum number of items in popup menu

/* XBUTTON structure (for X11 display only) */
typedef struct {
Window win;              // Window ID
char label[40];          // label
int x, y;                // origin (upper-left corner) in device coord.
int height, width;       // size in device coord.
int subwin_is_created;   // flag set to one when subwindow is created 
int subwin_is_mapped;    // flag set to one when subwindow is mapped
Window subwin;           // Sub-Window ID
char subwin_label[POPUP_NITEMS_MAX][40];   // Sub-Window labels
int subwin_x, subwin_y;  // origin (upper-left corner) in device coord.
int subwin_height, subwin_width;  // size of individual item box (device coord.)
int subwin_nitems;       // Number of menu items in subwindow
int is_active;           // Set to one when button is active
} XBUTTON;

/* XSTATIC button structure (for X11 display only) */
typedef struct {
Window win;              // Window ID
char label[100];          // label
int x, y;                // origin (upper-left corner) in device coord.
int height, width;       // size in device coord.
int is_created;          // Flag to test whether this button is created
} XSTATIC;

/* XCONTEXT structure (for X11 display only) */
typedef struct {
Display *display;
Window win;               // Window ID
XFontStruct *fonts;       // Fonts
GC gc;                    // Graphic context to draw on win and backing (GCcopy)
GC erasegc;               // Graphic context to erase win (GCclear)
GC cursorgc;              // Graphic context for a big crosshair cursor (GCxor) 
unsigned int gc_background;
unsigned int gc_foreground;
XSegment *xvec;
Cursor default_cursor;
Pixmap backing;
Colormap cmap;
int screen;
int nvec;
int color_code, max_colors;
int max_nvec;
int nbuttons;             // Number of (child) buttons in the window
XBUTTON But[NBUT_MAX];    // Button information
XSTATIC StatusBar;        // StatusBar
XSTATIC CursorPosition;   // Cursor position 
GC Button_graygc;         // Graphic context for buttons (gray background) 
GC Button_whitegc;        // Graphic context for buttons (white background) 
} XCONTEXT;

#endif // EOF sentry
#endif // EOF JLP_USE_X11
