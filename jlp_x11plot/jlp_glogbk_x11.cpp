/*************************************************************************
* jlp_logbook_x11.cpp
* JLP_GLogbk_X11 Class: routine to create a popup window
*  in order to display the logbook (with the values of some parameters)
*
* C++ version with classes
*
* JLP
* Version 16/11/2006 
**************************************************************************/
#ifdef JLP_USE_X11            /* To disable X11 if necessary */

#include "jlp_macros.h"               // For definitions of macros: MAXI
#include "jlp_glogbk.h"
#include "jlp_glogbk_x11.h"
#include "jlp_x11_utils.h"
#include "time.h"

/*
#define DEBUG
*/

/***************************************************************************
* Open a box to contain the logbook 
*
***************************************************************************/
int JLP_GLogbk_X11::OpenLogbookBox(const char *title) 
{
char *display_name=NULL;
int x_ul, y_ul, border_width, d_width, d_height;
XSetWindowAttributes attributes;
XWindowAttributes win_attributes;
XGCValues gcvalues;
XSizeHints size_hints;
register int i;

if(window_is_open) { 
  fprintf(stderr,"OpenLogbookBox/Error: already opened!\n");
// Return 0, otherwise may declare window closed ... */
  return(0);
  }

/* Open display and return display_name which is the value of
   environment variable DISPLAY : */
    if((Logbook_display = XOpenDisplay(display_name)) == NULL)
     {
      fprintf(stderr,
              "OpenLogbookBox/Error: cannot connect to XServer %s\n", 
               XDisplayName(display_name));
      return(-1);
     }
  Logbook_screen = DefaultScreen(Logbook_display);
  Logbook_cmap = DefaultColormap(Logbook_display,Logbook_screen);

// Get information about the screen (width and height):
  XGetWindowAttributes(Logbook_display, 
                       RootWindow(Logbook_display,Logbook_screen),
                       &win_attributes);
// Will display 40 lines of 80 characters: 
  JLP_LoadFonts(Logbook_display, &Logbook_Font);
// Cf User manual p 153:
  Font_width = Logbook_Font->max_bounds.rbearing - 
               Logbook_Font->min_bounds.lbearing;
  Font_height = Logbook_Font->max_bounds.ascent - 
                Logbook_Font->min_bounds.descent;
#ifdef DEBUG
 printf(" Font height=%d width=%d\n", Font_width, Font_height);
#endif
  Logbook_width = 80 * Font_width;
  Logbook_height = 45 * Font_width; 
  x_ul = win_attributes.width / 2 - Logbook_width / 2;
  y_ul = win_attributes.height / 2 - Logbook_height / 2;
  Black =  BlackPixel(Logbook_display,Logbook_screen);
  White =  WhitePixel(Logbook_display,Logbook_screen);

#ifdef DEBUG
printf("Width = %d, height = %d\n", Logbook_width, Logbook_height);
#endif

// Warning: should call this routine only when Logbook_screen 
// and Logbook_cmap are initialized!
// Load Gray array to be used for drawing buttons:
// 0,0,0 is black, 255,255,255 is white:
  for(i = 0; i < 256; i++)
          JLP_Xcolor_pixel(Logbook_display, Logbook_cmap, Gray[i], i, i, i);

// Need to use CreateWindow to set it as INputOutput
// and enable it to have children (needed for buttons)
 border_width = 2; 
 attributes.background_pixel = Gray[210];
 attributes.border_pixel = White; 

 Logbook_win = XCreateWindow(Logbook_display, 
                            RootWindow(Logbook_display, Logbook_screen),
                            x_ul, y_ul, Logbook_width, Logbook_height, 
                            border_width, CopyFromParent, InputOutput, 
                            CopyFromParent, CWBorderPixel|CWBackPixel, 
                            &attributes); 

// Tunable size, but set the title:
 size_hints.flags = PPosition|PSize;
 size_hints.x = x_ul;
 size_hints.y = y_ul;
 size_hints.width = Logbook_width;
 size_hints.height = Logbook_height;
 XSetStandardProperties(Logbook_display,Logbook_win,title,title,None,
                        NULL,0,&size_hints);

// Events to be monitored: 
 XSelectInput(Logbook_display,Logbook_win,
              ExposureMask|ButtonPressMask|KeyPressMask);


// Create graphic context to write in black: 
 gcvalues.font = Logbook_Font->fid;
 gcvalues.function = GXcopy;
 gcvalues.foreground = Black;
 Logbook_gc = XCreateGC(Logbook_display, Logbook_win, 
                       GCForeground|GCFunction|GCFont, &gcvalues);
// Set line width to 1 (to draw buttons):
 XSetLineAttributes(Logbook_display, Logbook_gc, 1,
                    LineSolid, CapButt, JoinBevel);

// Initialize window_is_open to one, otherwise pb in CreateCloseButton
 window_is_open = 1;
 d_width = MAXI(30, Logbook_width/6);
 d_height = MAXI(20, Logbook_height/10);
 CreateCloseButton(0, 0, d_width, d_height);
 CreateProcessButton(d_width+3, 0, d_width, d_height);

/*********************************************************************/
// Map the subwindows and window:
 XMapSubwindows(Logbook_display,Logbook_win);
 XMapWindow(Logbook_display,Logbook_win);

/* Force child processes to disinherit the TCP file descriptor.
* This helps the shell command (creating new xterm) forked and
* exec'ed from the menu to work properly.  */
/* in winman.c, ... here no effect
 if ((fcntl(ConnectionNumber(Logbook_display), F_SETFD, 1)) == -1)
        fprintf(stderr, "Child cannot disinherit TCP fd");
*/

return(0);
}
/***************************************************************************
* Create Close button (to "clear" the screen when the logfile is no 
* longer wanted by the user) 
*
* INPUT:
* x_ul, y_ul: upper-left corner in device coordinates of line 
*             containing the "Close" button 
***************************************************************************/
int JLP_GLogbk_X11::CreateCloseButton(int x_ul, int y_ul, int d_width, 
                                   int d_height)
{
int border_width; 
XSetWindowAttributes attributes;

if(!window_is_open) { 
  fprintf(stderr,"CreateCloseButton/Error: window was not opened!\n");
  return(-1);
  }

 border_width = 2; 
 CloseButton_width = d_width; 
 CloseButton_height = d_height; 
// Location of string inside the button:
 CloseButton_x = CloseButton_width / 3;
 CloseButton_y = (int)((double)CloseButton_height * 0.7);
// Background of LogbookBox was set to Gray[210], so I take a smaller
// value (= darker) 
 attributes.background_pixel = Gray[150];
 attributes.border_pixel = Black; 
 border_width = 1; 
 CloseButton = XCreateWindow(Logbook_display, Logbook_win, x_ul, y_ul,
                             CloseButton_width, CloseButton_height, 
                             border_width,
                             CopyFromParent, InputOutput, CopyFromParent,
                             CWBorderPixel|CWBackPixel, &attributes);

// Events to be monitored: 
 XSelectInput(Logbook_display,CloseButton,ExposureMask|ButtonPressMask);

return(0);
}
/***************************************************************************
* Create Process button (to "process" the data contained in Xdisp1.log
* for astrometry) 
*
* INPUT:
* x_ul, y_ul: upper-left corner in device coordinates of line 
*             containing the "Close" button 
***************************************************************************/
int JLP_GLogbk_X11::CreateProcessButton(int x_ul, int y_ul, int d_width,
                                     int d_height)
{
int border_width; 
XSetWindowAttributes attributes;

if(!window_is_open) { 
  fprintf(stderr,"CreateProcessButton/Error: window was not opened!\n");
  return(-1);
  }

 border_width = 2; 
 ProcessButton_width = d_width; 
 ProcessButton_height = d_height; 
// Location of string inside the button:
 ProcessButton_x = ProcessButton_width / 3;
 ProcessButton_y = (int)((double)ProcessButton_height * 0.7);
// Background of LogbookBox was set to Gray[210], so I take a smaller
// value (= darker) 
 attributes.background_pixel = Gray[150];
 attributes.border_pixel = Black; 
 border_width = 1; 
 ProcessButton = XCreateWindow(Logbook_display, Logbook_win, x_ul, y_ul,
                             ProcessButton_width, ProcessButton_height, 
                             border_width,
                             CopyFromParent, InputOutput, CopyFromParent,
                             CWBorderPixel|CWBackPixel, &attributes);

// Events to be monitored: 
 XSelectInput(Logbook_display,ProcessButton,ExposureMask|ButtonPressMask);

return(0);
}
/**************************************************************
* Handle events 
* and close display when user has clicked on "Close" with the mouse
* or has typed "Enter" on the keyboard
*
* OUTPUT:
* process_data: set to one if data has to be processed
***************************************************************/
int JLP_GLogbk_X11::WaitForClose(int *process_data)
{
int to_be_closed = 0;
XEvent event;
XComposeStatus compose;
KeySym key_sym;
char buffer[20];

*process_data = 0;

if(!window_is_open) { 
  fprintf(stderr,"WaitForClose/Error: window was not opened!\n");
  return(-1);
  }


while(!to_be_closed) {

// Wait for next event and return "event" when one is found:
  XNextEvent(Logbook_display, &event);
      switch (event.type) {
       case Expose:
         DrawLogbookBox();
/* Flushes graphic to screen; */
         XFlush(Logbook_display);
         break;
// Keyboard entry:
// (after having pressed with the mouse on this button since I require
//  a sucessful on test active_Edit that I activate with the mouse only)
// The mouse can also be anywhere when entering the characters.
       case KeyPress:
// buffer_size = 20:
         XLookupString((XKeyEvent *)&event, buffer, 20, &key_sym, &compose);
         if(key_sym == XK_Return) to_be_closed = 1; 
         break;
// Button was pressed: check if it was on OK or Cancel:
       case ButtonPress:
         if(event.xbutton.window == CloseButton) to_be_closed = 1;
         else if(event.xbutton.window == ProcessButton) {
            *process_data = 1; 
            to_be_closed = 1;
            }
         break;
        default:
         break;
      }  // EOF switch on event.type

 }    /* end while */

return(0);
}
/**************************************************************
* Draw all items in the LogbookBox 
***************************************************************/
int JLP_GLogbk_X11::DrawLogbookBox()
{
register int i;
int ix, iy, iy_step;

if(!window_is_open) { 
  fprintf(stderr,"DrawLogbookBox/Error: window was not opened!\n");
  return(-1);
  }

  XClearWindow(Logbook_display, Logbook_win);

// Draw static labels of all widgets:
  iy_step = (int)(Font_height * 1.1); 
  ix = 0; 
  iy = CloseButton_height + iy_step;
  for(i = 0; i < nLines; i++) {
    XDrawString(Logbook_display, Logbook_win, Logbook_gc, 
                ix,  iy, Logbook_label[i], strlen(Logbook_label[i]));
    iy += iy_step;
    }

// Draw Close and Process buttons:
   XDrawString(Logbook_display, CloseButton, Logbook_gc, 
               CloseButton_x,  CloseButton_y, 
               CloseButton_label, strlen(CloseButton_label));
   XDrawString(Logbook_display, ProcessButton, Logbook_gc, 
               ProcessButton_x,  ProcessButton_y, 
               ProcessButton_label, strlen(ProcessButton_label));

return(0);
}
#endif /* End of JLP_USE_X11 */
