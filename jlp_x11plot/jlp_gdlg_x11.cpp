/*************************************************************************
* jlp_dlg_x11.cpp
* JLP_Dialog Class: routine to create a popup window
*  in order to obtain the values of some parameters
*
* C++ version with classes
*
* JLP
* Version 16/11/2006 
**************************************************************************/
#ifdef JLP_USE_X11   /* New flag to disable X11 if necessary */

#include "jlp_macros.h"               // For definitions of macros: MAXI
#include "jlp_gdlg_x11.h"
#include "jlp_x11_utils.h"
#include "time.h"

/*
#define DEBUG
*/

/***************************************************************************
* Open a dialog box 
*
***************************************************************************/
int JLP_GDlg_X11::OpenDialogBox(const char *title, const int small_size) 
{
char *display_name=NULL;
int x_ul, y_ul, border_width; 
XSetWindowAttributes attributes;
XWindowAttributes win_attributes;
XGCValues gcvalues;
XSizeHints size_hints;
register int i;

if(window_is_open) {
  fprintf(stderr,"OpenDialogBox/Error: window already opened!\n");
  return(0);
  }


/* Open display and return display_name which is the value of
   environment variable DISPLAY : */
    if((Dialog_display = XOpenDisplay(display_name)) == NULL)
     {
      fprintf(stderr, "OpenDialogBox/Fatal error: cannot connect to XServer %s\n", 
              XDisplayName(display_name));
      return(-1);
     }
  Dialog_screen = DefaultScreen(Dialog_display);
  Dialog_cmap = DefaultColormap(Dialog_display,Dialog_screen);

// Get information about the screen (width and height):
  XGetWindowAttributes(Dialog_display, 
                       RootWindow(Dialog_display,Dialog_screen),
                       &win_attributes);
  if(small_size) {
    Dialog_width = win_attributes.width / 2;
    Dialog_height = win_attributes.height / 3;
  } else {
    Dialog_width = win_attributes.width / 2;
    Dialog_height = (win_attributes.height * 4) / 5;
  }
  x_ul = win_attributes.width / 2 - Dialog_width / 2;
  y_ul = win_attributes.height / 2 - Dialog_height / 2;
  Black =  BlackPixel(Dialog_display,Dialog_screen);
  White =  WhitePixel(Dialog_display,Dialog_screen);

#ifdef DEBUG
printf("Width = %d, height = %d\n", Dialog_width, Dialog_height);
#endif

// Warning: should call this routine only when Dialog_screen 
// and Dialog_cmap are initialized!
// Load Gray array to be used for drawing buttons:
// 0,0,0 is black, 255,255,255 is white:
  for(i = 0; i < 256; i++) 
          JLP_Xcolor_pixel(Dialog_display, Dialog_cmap, Gray[i], i, i, i);

// Need to use CreateWindow to set it as INputOutput
// and enable it to have children (needed for buttons)
 border_width = 2; 
 attributes.background_pixel = Gray[210];
 attributes.border_pixel = White; 

 Dialog_win = XCreateWindow(Dialog_display, 
                            RootWindow(Dialog_display, Dialog_screen),
                            x_ul, y_ul, Dialog_width, Dialog_height, 
                            border_width, CopyFromParent, InputOutput, 
                            CopyFromParent, CWBorderPixel|CWBackPixel, 
                            &attributes); 

// Forces fixed size and set the title:
 size_hints.flags = PPosition|PSize|PMinSize|PMaxSize;
 size_hints.x = x_ul;
 size_hints.y = y_ul;
 size_hints.width = Dialog_width;
 size_hints.min_width = Dialog_width;
 size_hints.max_width = Dialog_width;
 size_hints.height = Dialog_height;
 size_hints.min_height = Dialog_height;
 size_hints.max_height = Dialog_height;
 XSetStandardProperties(Dialog_display,Dialog_win,title,title,None,
                        NULL,0,&size_hints);

// Events to be monitored: 
// I add KeyPressMask to be able to input characters in the
// "active" EditButton when the mouse is elsewhere
 XSelectInput(Dialog_display,Dialog_win,
              ExposureMask|ButtonPressMask|KeyPressMask);


// Create graphic context to write in black: 
 JLP_LoadFonts(Dialog_display, &Dialog_Font);
 gcvalues.font = Dialog_Font->fid;
 gcvalues.function = GXcopy;
 gcvalues.foreground = Black;
 Dialog_gc = XCreateGC(Dialog_display, Dialog_win, 
                       GCForeground|GCFunction|GCFont, &gcvalues);
// Set line width to 1 (to draw buttons):
 XSetLineAttributes(Dialog_display, Dialog_gc, 1,
                    LineSolid, CapButt, JoinBevel);


 window_is_open = 1;
return(0);
}
/***************************************************************************
* Create OK and Cancel buttons 
*
* INPUT:
* y_ul: upper-left corner in device coordinates of line 
*       containing OK and Cancel buttons 
***************************************************************************/
int JLP_GDlg_X11::CreateOKCancel(int y_ul) 
{
int d_width, d_height, border_width; 
int OK_x_ul, OK_y_ul, Cancel_x_ul, Cancel_y_ul;
XSetWindowAttributes attributes;

if(!window_is_open) {
  fprintf(stderr,"CreateOKCancel/Error: window was not opened!\n");
  return(-1);
  }

// Need to use CreateWindow to set it as INputOutput
// and enable it to have children (needed for buttons)
 border_width = 2; 
 d_width = MAXI(30, Dialog_width/6);
 d_height = 30;
 OK_x_ul = (int)((double)Dialog_width * 0.1);
 OK_y_ul = y_ul;
 OK_x = d_width / 3;
 OK_y = (int)((double)d_height * 0.7);
// Background of DialogBox was set to Gray[210], so I take a smaller
// value (= darker) 
 attributes.background_pixel = Gray[150];
 attributes.border_pixel = Black; 
 border_width = 1; 
 OK_Button = XCreateWindow(Dialog_display, Dialog_win, OK_x_ul, OK_y_ul,
                           d_width, d_height, border_width,
                           CopyFromParent, InputOutput,
                           CopyFromParent,
                           CWBorderPixel|CWBackPixel, &attributes);

 Cancel_x_ul = (int)((double)Dialog_width * 0.6);
 Cancel_y_ul = OK_y_ul; 
 Cancel_x = d_width / 8;
 Cancel_y = OK_y; 
 Cancel_Button = XCreateWindow(Dialog_display, Dialog_win, Cancel_x_ul, 
                               Cancel_y_ul, d_width, d_height, border_width,
                               CopyFromParent, InputOutput, CopyFromParent,
                               CWBorderPixel|CWBackPixel, &attributes);

// Events to be monitored: 
 XSelectInput(Dialog_display,OK_Button,ExposureMask|ButtonPressMask);
 XSelectInput(Dialog_display,Cancel_Button,ExposureMask|ButtonPressMask);

return(0);
}
/**************************************************************
* Handle events 
* and close display when user has pressed on OK or Cancel
*
* OUTPUT:
* is_OK: 1 if "OK" or 0 if "Cancel"
***************************************************************/
int JLP_GDlg_X11::WaitForOK(char svalues[][80], int *is_checked, int nvalues, 
                         int& is_OK)
{
int active_Edit;
XEvent event;
XComposeStatus compose;
KeySym key_sym;
Window jwin;
char buffer[20];
int button_was_pressed;
int irad, idata, status; 
register int i, j, k;

// Initialize output value to zero:
 is_OK = 0;

if(!window_is_open) {
  fprintf(stderr,"WaitForOK/Error: window was not opened!\n");
  return(-1);
  }

// Handle events:
button_was_pressed = 0;
active_Edit = 0;
while(!button_was_pressed) {

// Wait for next event and return "event" when one is found:
  XNextEvent(Dialog_display, &event);
      switch (event.type) {
       case Expose:
         DrawDialogBox(active_Edit);
         break;
// Keyboard entry:
// (after having pressed with the mouse on this button since I require
//  a sucessful on test active_Edit that I activate with the mouse only)
// The mouse can also be anywhere when entering the characters.
       case KeyPress:
           if(active_Edit) { 
             j = JEdit_iJWid[active_Edit-1];
             status = HandleEventOnEditButton(event, JWid[j]);
             if(!status) {
               DrawDialogBox(active_Edit);
             } else {
               button_was_pressed = 1;
               is_OK = 1;
               }
// If Carriage Return, assume that is is OK:
           } else { 
// buffer_size = 20:
             XLookupString((XKeyEvent *)&event, buffer, 20, &key_sym, &compose);
             if(key_sym == XK_Return){ 
               button_was_pressed = 1;
               is_OK = 1;
             }
           }
         break;
// Button was pressed: check if it was on OK or Cancel:
       case ButtonPress:
         active_Edit = 0;
         for(i = 0; i < nJEdit; i++) {
           j = JEdit_iJWid[i];
           status = FromJWidgetToWindow_ID(JWid[j].JWid_id, &jwin); 
           if(status) {
             fprintf(stderr," Fatal error in FromJWidgetToWindow_ID\n");
             exit(-1);
             }
           if(jwin == event.xbutton.window) {
             active_Edit = i + 1;
             break;
             }
          }
         if(event.xbutton.window == OK_Button) {
            button_was_pressed = 1;
            is_OK = 1;
            }
         else if(event.xbutton.window == Cancel_Button) {
            button_was_pressed = 1;
            is_OK = 0;
            }
         else { 
             for(i = 0; i < nJWid; i++) { 
             status = FromJWidgetToWindow_ID(JWid[i].JWid_id, &jwin); 
             if(status) {
               fprintf(stderr," Fatal error in FromJWidgetToWindow_ID\n");
               exit(-1);
               }
                if(jwin == event.xbutton.window){
                  switch (JWid[i].widget_type) {
// CheckButton:
                    case 2:
                      if(JWid[i].is_checked) 
                        JWid[i].is_checked = 0;
                      else 
                        JWid[i].is_checked = 1;
                      break;
// RadioButton:
                    case 3:  
                      JWid[i].is_checked = 1;
                      irad = JWid[i].radio_group;
// Erase the values of all other items of this radio group:
                      for(k = 0; k < JRadio[irad].nmembers; k++) {
                         j = JRadio[irad].iJWid[k]; 
                         if(j != i) JWid[j].is_checked = 0;
                         }
                      break;
                    } // EOF switch on widget_type
                  }  // EOF if test  
                }   // EOF loop on i
           } // EOF if test on event.xbutton.window (else case) 
// Redraw screen with new values:
          DrawDialogBox(active_Edit);
         break;
        default:
         break;
      }  // EOF switch on event.type

 }    /* end while */


// Transfer current values to output arrays:
if(is_OK) {
  for(i = 0; i < nJWid; i++) {
    idata = JWid[i].idata;
      if(idata >= 0) {
      is_checked[idata] = JWid[i].is_checked;
      strcpy(svalues[idata], JWid[i].svalue);
      }
    }
 }

CloseWindow();

return(0);
}
/**************************************************************
* Draw all items in the DialogBox 
***************************************************************/
int JLP_GDlg_X11::DrawDialogBox(int active_Edit)
{
register int i;
Window iwin;
int x_ul, y_ul, status;

if(!window_is_open) {
  fprintf(stderr,"DrawDialogBox/Error: window was not opened!\n");
  return(-1);
  }

// Draw label:
/*
   XDrawString(Dialog_display, Dialog_win, Dialog_gc, Label_x,  
               Label_y, Label, strlen(Label));
*/

// Draw static labels of all widgets:
  for(i = 0; i < nJWid; i++) {
    XDrawString(Dialog_display, Dialog_win, Dialog_gc, 
                JWid[i].x_label,  JWid[i].y_label, 
                JWid[i].label, strlen(JWid[i].label));
    status = FromJWidgetToWindow_ID(JWid[i].JWid_id, &iwin); 
      if(status) {
         fprintf(stderr," Fatal error in FromJWidgetToWindow_ID\n");
         exit(-1);
      }
    switch (JWid[i].widget_type) {
// Static label:
       case 1:
       default:
          break;
// CheckButton:
       case 2:
          XClearWindow(Dialog_display, iwin);
          if(JWid[i].is_checked){
              XDrawLine(Dialog_display, iwin, Dialog_gc,
                        0, 0, JWid[i].win_width, JWid[i].win_height);
              XDrawLine(Dialog_display, iwin, Dialog_gc,
                        0, JWid[i].win_height, JWid[i].win_width, 0);
           }
          break;
// RadioButton:
       case 3:
          XClearWindow(Dialog_display, iwin);

/* Draw the big circle: */
          XDrawArc(Dialog_display, iwin, Dialog_gc,
                   0, 0, JWid[i].win_width, JWid[i].win_height,
                   0,(int)(360*64));

/* If checked draw a small circle, too: */
          if(JWid[i].is_checked){
             x_ul = (int)((double)JWid[i].win_width * (0.5 - 0.15));
             y_ul = (int)((double)JWid[i].win_height * (0.5 - 0.15));
             XFillArc(Dialog_display, iwin, Dialog_gc,
                      x_ul, y_ul,
                      JWid[i].win_width/2, JWid[i].win_height/2,
                      0,(int)(360*64));
           }
          break;
// EditButton:
       case 4:
         XSetWindowBackground(Dialog_display, iwin, Gray[230]);
// Set a brighter background if active Edit Button:
           if(active_Edit) {
           if(i == JEdit_iJWid[active_Edit - 1])
            XSetWindowBackground(Dialog_display, iwin, Gray[250]);
           }
         XClearWindow(Dialog_display, iwin);
         XDrawString(Dialog_display, iwin, Dialog_gc, 0,
                     (int)((double)JWid[i].win_height * 0.8), 
                     JWid[i].svalue, strlen(JWid[i].svalue));
// Draw text cursor "|" to see the location of icur:
         if(active_Edit) DrawTextCursor(active_Edit);
          break;
       } // EOF switch on widget_type
    }   // EOF loop on i

// Draw OK and Cancel Buttons:
   XDrawString(Dialog_display, OK_Button, Dialog_gc, OK_x,  OK_y, 
               OK_label, strlen(OK_label));
   XDrawString(Dialog_display, Cancel_Button, Dialog_gc, Cancel_x,
               Cancel_y, Cancel_label, strlen(Cancel_label));
return(0);
}
/************************************************************************
* Create all widgets
*
*************************************************************************/
int JLP_GDlg_X11::CreateWidgets(char label0[][80], int *widget_type0, 
                           int *radio_group0, char *val_type0, 
                           char svalue0[][80], int *is_checked0, 
                           int nwidgets0)
{
register int i;
int status, x_ul, y_ul, height0=10; 
int nlines, n_created_lines, OnlyEvaluation; 

if(!window_is_open) {
  fprintf(stderr,"CreateWidgets/Error: window was not opened!\n");
  return(-1);
  }

/*********************************************************************/
// First iteration: determine the number of lines to be created
/*********************************************************************/
// Initalize Radio Buttons and other parameters:
  for(i = 0; i < NRADIO_MAX; i++) JRadio[i].nmembers = -1;
  x_ul = y_ul = 0;
  OnlyEvaluation = 1;
  nJWid = 0;
  nlines = 0;
  for(i = 0; i < nwidgets0; i++) {
#ifdef DEBUG
    printf("CreateWidgets/Evaluating widget #%d label=%s\n", i, label0[i]);
#endif
    if(widget_type0[i] == 3)
       status = AddRadioButton(label0[i], widget_type0[i], radio_group0[i], 
                               val_type0[i], svalue0[i], is_checked0[i], 
                               x_ul, y_ul, i, height0, n_created_lines, 
                               OnlyEvaluation);
    else
       status = AddWidget(label0[i], widget_type0[i], radio_group0[i], 
                          val_type0[i], svalue0[i], is_checked0[i], 
                          x_ul, y_ul, i, height0, n_created_lines, 
                          OnlyEvaluation);
    if(status) {
      fprintf(stderr, "CreateWidgets/Fatal error creating widget #%d\n", i);
      exit(-1);
      } 
    nlines += n_created_lines;
    }


/*********************************************************************/
// Second iteration: create the widgets
/*********************************************************************/
// Initalize Radio Buttons and other parameters:
 for(i = 0; i < NRADIO_MAX; i++) JRadio[i].nmembers = -1;
 OnlyEvaluation = 0;
 nJWid = 0;
 height0 = (int)((double)Dialog_height / (double)(nlines + 4));

// Position of the first item:
 y_ul = 2 * height0;
 x_ul = Dialog_width / 8;

  for(i = 0; i < nwidgets0; i++) {
#ifdef DEBUG
    printf("CreateWidgets/Adding widget #%d label=%s\n", i, label0[i]);
#endif
    if(widget_type0[i] == 3)
       status = AddRadioButton(label0[i], widget_type0[i], radio_group0[i], 
                               val_type0[i], svalue0[i], is_checked0[i], 
                               x_ul, y_ul, i, height0, n_created_lines, 
                               OnlyEvaluation);
    else
       status = AddWidget(label0[i], widget_type0[i], radio_group0[i], 
                          val_type0[i], svalue0[i], is_checked0[i], 
                          x_ul, y_ul, i, height0, n_created_lines, 
                          OnlyEvaluation);
    if(status) {
      fprintf(stderr, "CreateWidgets/Fatal error creating widget #%d\n", i);
      exit(-1);
      } 
    y_ul += (height0 * n_created_lines);
    }

/*********************************************************************/
// Check if radio buttons are coherent and initialize text cursor positions: 
  FinalizeWidgets();

/*********************************************************************/
// Create OK and Cancel buttons at the bottom:
  CreateOKCancel(y_ul);

/*********************************************************************/
// Map the subwindows and window:
 XMapSubwindows(Dialog_display,Dialog_win);
 XMapWindow(Dialog_display,Dialog_win);

/* Force child processes to disinherit the TCP file descriptor.
* This helps the shell command (creating new xterm) forked and
* exec'ed from the menu to work properly.  */
/* in winman.c, ... here no effect 
 if ((fcntl(ConnectionNumber(Dialog_display), F_SETFD, 1)) == -1)
        fprintf(stderr, "CreateWidgets: child cannot disinherit TCP fd");
*/

return(status);
}
/*************************************************************************
* Create a Static label
*
* OUPUT:
* n_created_lines: number of created lines
*************************************************************************/
int JLP_GDlg_X11::CreateStaticLabel(char *label0, int x_ul, int y_ul, int iJWid, 
                               int height0, int &n_created_lines)
{
int i, i0, j, status = -1;

n_created_lines = 0;

if(!window_is_open) {
  fprintf(stderr,"CreateStaticLabel/Error: window was not opened!\n");
  return(-1);
  }

// Copy label up to the first '\0' or first '\n' encountered:
i0 = 0;
j = iJWid;
while(i0 < 79 && label0[i0] && j < NWIDGET_MAX) { 
// Status is set to 0 when at least one label is created:
  status = 0;
  for(i = 0; i < 79 && i0 < 79 && label0[i0] && label0[i0] != '\n'; i++) { 
     JWid[j].label[i] = label0[i0++];
     }
  if(label0[i0] == '\n' && i0 < 79) i0++;
  JWid[j].label[i] = '\0';
  JWid[j].x_label = x_ul;
  JWid[j].y_label = y_ul + height0 * n_created_lines;
  JWid[j].idata = -1;
  JWid[j].widget_type = 1;
  n_created_lines++;
  j++;
  }
return(status);
}
/*************************************************************************
* Create a CheckButton 
*
*************************************************************************/
int JLP_GDlg_X11::CreateCheckButton(JLP_WIDGET &JWid0, int x_ul, int y_ul)
{
int border_width, d_width, d_height;
XSetWindowAttributes attributes;

if(!window_is_open) {
  fprintf(stderr,"CreateCheckButton/Error: window was not opened!\n");
  return(-1);
  }

// Need to use CreateWindow to set it as INputOutput
// and enable it to have children (needed for buttons)
 border_width = 1;
 d_width = 10;
 d_height = 10;
// Background of DialogBox was set to Gray[210], so I do the same here:
 attributes.background_pixel = Gray[210];
 attributes.border_pixel = Black;
 border_width = 1;
 Widgets_win_id[JWid0.JWid_id] = XCreateWindow(Dialog_display, Dialog_win, 
                                   x_ul, y_ul - d_height, d_width, d_height, 
                                   border_width, CopyFromParent, InputOutput, 
                                   CopyFromParent, CWBorderPixel|CWBackPixel, 
                                   &attributes);

// Take a smaller value (better for drawing afterwards):
 JWid0.win_width = (int)((double)d_width * 0.98);
 JWid0.win_height = (int)((double)d_height * 0.98);

// Location of static label (bottom-left):
  JWid0.x_label = x_ul + d_width + 10;
  JWid0.y_label = y_ul;

// Events to be monitored:
 XSelectInput(Dialog_display,Widgets_win_id[JWid0.JWid_id],
              ExposureMask|ButtonPressMask);

return(0);
}
/*************************************************************************
* Create a RadioButton 
*
*************************************************************************/
int JLP_GDlg_X11::CreateRadioButton(JLP_WIDGET &JWid0, int x_ul, int y_ul)
{
int border_width, d_width, d_height;
XSetWindowAttributes attributes;

if(!window_is_open) {
  fprintf(stderr,"CreateRadioButton/Error: window was not opened!\n");
  return(-1);
  }

// Need to use CreateWindow to set it as INputOutput
// and enable it to have children (needed for buttons)
 border_width = 1;
 d_width = 10;
 d_height = 10;
// Background of DialogBox was set to Gray[210], so I do the same here:
 attributes.background_pixel = Gray[210];
 attributes.border_pixel = Black;
 border_width = 0;
 Widgets_win_id[JWid0.JWid_id] = XCreateWindow(Dialog_display, Dialog_win,
                                   x_ul, y_ul - d_height, d_width, d_height,
                                   border_width, CopyFromParent, InputOutput,
                                   CopyFromParent, CWBorderPixel|CWBackPixel,
                                   &attributes);

// Take a smaller value (better for drawing afterwards):
 JWid0.win_width = (int)((double)d_width * 0.98);
 JWid0.win_height = (int)((double)d_height * 0.98);

// Location of static label (bottom-left):
 JWid0.x_label = x_ul + d_width + 10;
 JWid0.y_label = y_ul;

// Events to be monitored:
 XSelectInput(Dialog_display,Widgets_win_id[JWid0.JWid_id],
              ExposureMask|ButtonPressMask);

return(0);
}
/*************************************************************************
* Create an EditButton 
*
*************************************************************************/
int JLP_GDlg_X11::CreateEditButton(JLP_WIDGET &JWid0, int x_ul, int y_ul)
{
int border_width, d_x, d_y, d_width, d_height, font_size;
XSetWindowAttributes attributes;
Cursor mycursor;                       /* cursor for Edit Button */

if(!window_is_open) {
  fprintf(stderr,"CreateEditButton/Error: window was not opened!\n");
  return(-1);
  }

// Location of static label (bottom-left):
 JWid0.x_label = x_ul;
 JWid0.y_label = y_ul;

// Need to use CreateWindow to set it as INputOutput
// and enable it to have children (needed for buttons)
 border_width = 0;
 font_size = XTextWidth(Dialog_Font, "x", 1);
 if(JWid0.val_type == 'c') d_width = 20 * font_size;
  else d_width = 10 * font_size;
 d_height = 2 * font_size;
 d_x = x_ul + XTextWidth(Dialog_Font, JWid0.label, strlen(JWid0.label)) + 2;
 d_y = y_ul - (int)((double)d_height * 0.8);

// Background of DialogBox was set to Gray[210], I take something brighter:
 attributes.background_pixel = Gray[230];
 attributes.border_pixel = Black;
 Widgets_win_id[JWid0.JWid_id] = XCreateWindow(Dialog_display, Dialog_win, 
                                  d_x, d_y, d_width, d_height, border_width,
                                  CopyFromParent, InputOutput, CopyFromParent,
                                  CWBorderPixel|CWBackPixel, &attributes);

// Cursor for Edit Button: 
// See Xlib Reference Manual p 641 and "X11/cursorfont.h"
 mycursor = XCreateFontCursor(Dialog_display, XC_xterm);
 XDefineCursor(Dialog_display, Widgets_win_id[JWid0.JWid_id], mycursor);

 JWid0.win_width = d_width;
 JWid0.win_height = d_height;

// Events to be monitored:
 XSelectInput(Dialog_display, Widgets_win_id[JWid0.JWid_id],
              ExposureMask|ButtonPressMask|KeyPressMask);

return(0);
}
/*****************************************************************
* Add one character to a string
* at location svalue_icur
*****************************************************************/
int JLP_GDlg_X11::AddToString(JLP_WIDGET& JWid0, char cc)
{
int len, icur, status = -1;
register int i;

// Input:
len = strlen(JWid0.svalue);
icur = JWid0.svalue_icur;

icur = MINI(icur, len); 
icur = MAXI(icur, 0); 

if(JWid0.svalue_len < JWid0.string_size) {
// Shift all values to the right, starting from end (i.e. '\0'):
    for(i = len; i >= icur; i--) JWid0.svalue[i] = JWid0.svalue[i-1]; 
    len++; JWid0.svalue[len] = '\0';
    JWid0.svalue[icur] = cc;
    icur++;
  }

// Output:
JWid0.svalue_len = len;
JWid0.svalue_icur = icur;

return(status);
}
/*****************************************************************
* Remove one character from a string (before icur)
*
* INPUT/OUTPUT:
* svalue: character string
* icur: position of cursor
*
* OUTPUT:
* svalue_len
*****************************************************************/
int JLP_GDlg_X11::RemoveLeftFromString(JLP_WIDGET &JWid0)
{
int status = -1, len, icur;
register int i;

// Input:
len = strlen(JWid0.svalue);
icur = JWid0.svalue_icur;

icur = MINI(icur, len); 
icur = MAXI(icur, 0); 

if(icur == len) {
  len--; icur--;
  JWid0.svalue[len] = '\0';
} else if(len > 0 && icur > 0) {
// Shift all characters to the left from cursor position:
    for(i = icur -  1; i <= len; i++) JWid0.svalue[i] = JWid0.svalue[i+1]; 
    len--; icur--;
  }

// Output:
JWid0.svalue_len = len;
JWid0.svalue_icur = icur;

return(status);
}
/*****************************************************************
* Remove one character from a string (after icur)
*
* INPUT/OUTPUT:
* svalue: character string
* icur: position of cursor
*
* OUTPUT:
* svalue_len
*****************************************************************/
int JLP_GDlg_X11::RemoveRightFromString(JLP_WIDGET &JWid0)
{
int status = -1, len, icur;
register int i;

// Input:
len = strlen(JWid0.svalue);
icur = JWid0.svalue_icur;

icur = MINI(icur, len); 
icur = MAXI(icur, 0); 

if(len > 0 && icur <= len) {
    for(i = icur; i <= len; i++) JWid0.svalue[i] = JWid0.svalue[i+1]; 
    len--;
  }

// Output:
JWid0.svalue_len = len;
JWid0.svalue_icur = icur;

return(status);
}
/*********************************************************************
* Handle event on Edit Button
*
* OUTPUT:
*   1 if "Return" has been entered
*   0 otherwise 
*********************************************************************/
int JLP_GDlg_X11::HandleEventOnEditButton(XEvent& event, JLP_WIDGET &JWid0)
{
int nchar;
const int buffer_size = 20;
XComposeStatus compose;
KeySym key_sym;
char buffer[buffer_size];

if(!window_is_open) {
  fprintf(stderr,"HandleEventOnEditButton/Error: window was not opened!\n");
  return(-1);
  }

  nchar = XLookupString((XKeyEvent *)&event, buffer, buffer_size,
                         &key_sym, &compose);

// See Xlib Reference Manual, p625 and include file "X11/keysymdef.h":
  switch (key_sym) {
// CR, "Return" or "Enter": */
   case XK_Return: 
     return(1);
     break;
// Right Arrow
    case XK_Right:
// Up to len:
     if(JWid0.svalue_icur < JWid0.svalue_len) JWid0.svalue_icur++;
     else XBell(Dialog_display, 0);
     break;
// Left Arrow
    case XK_Left:
// Down to 0:
     if(JWid0.svalue_icur > 0) JWid0.svalue_icur--;
     else XBell(Dialog_display, 0);
     break;
// Backspace Key (= delete to left):
    case XK_BackSpace:
     if(JWid0.svalue_icur > 0) RemoveLeftFromString(JWid0);
     else XBell(Dialog_display, 0);
     break;
// Delete Key (= delete to right) "Suppr" key:
    case XK_Delete:
     if(JWid0.svalue_icur < JWid0.svalue_len) RemoveRightFromString(JWid0);
     else XBell(Dialog_display, 0);
     break;
// Not found in JLP Keyboard (2006):
    case XK_Clear:
     printf("OK: (new) Clear Key pressed\n");
     JWid0.svalue[0] = '\0';
     JWid0.svalue_len = 0;
     JWid0.svalue_icur = 0;
     break;
// Not found in JLP Keyboard (2006):
    case XK_Begin:
     printf("OK: (new) Begin Key pressed\n");
     JWid0.svalue_icur = 0;
     break;
    case XK_Home:
     JWid0.svalue_icur = 0;
     break;
    case XK_End:
     JWid0.svalue_icur = JWid0.svalue_len;
     break;
    default:
     break;
    }
  if(nchar) {
     if(IsGoodValue(*buffer, JWid0.val_type))
        AddToString(JWid0,*buffer);
/* DEBUGG:
     else
      printf("HandleEventOnEditButton/Bad value (buffer=%c type=%c)\n", *buffer, JWid0.val_type);
*/
    }
return(0);
}
/*************************************************************************
*
* INPUT:
* active_Edit: (index + 1) of widget JWid[] corresponding to the active 
*              Edit Button
*************************************************************************/
int JLP_GDlg_X11::DrawTextCursor(int active_Edit)
{
int iJW, iJEdit, ix, iy, width0, height0, len, status;
char buffer[80];
Window jwin;

if(!window_is_open) {
  fprintf(stderr,"DrawTextCursor/Error: window was not opened!\n");
  return(-1);
  }

iJEdit = active_Edit - 1;
if(iJEdit < 0 || iJEdit > nJEdit) {
   fprintf(stderr,"DrawBlinkCursor/Fatal error: iJEdit=%d (nJEdit=%d)\n", 
           iJEdit, nJEdit);
   exit(-1);
   }

iJW = JEdit_iJWid[iJEdit];
if(iJW < 0 || iJW > nJWid) {
   fprintf(stderr,"DrawBlinkCursor/Fatal error: iJWid = %d\n", iJW);
   exit(-1);
   }

width0 = 1;
height0 = (int)((double)JWid[iJW].win_height * 0.8);

// Compute location of text cursor:
 len = JWid[iJW].svalue_icur;
 strncpy(buffer, JWid[iJW].svalue, len);
 buffer[len] = '\0';
 ix = XTextWidth(Dialog_Font, "buffer", len);
 iy = (int)((double)JWid[iJW].win_height * 0.1);

// Draw text cursor:
   status = FromJWidgetToWindow_ID(JWid[iJW].JWid_id, &jwin);
   if(status){
    fprintf(stderr, "Fatal error in FromJWidgetToWindow_ID\n");
    exit(-1);
    }
    XDrawLine(Dialog_display, jwin, Dialog_gc,
              ix, iy, ix, iy + height0);

return(0);
}
/*************************************************************************
* Test in 2006: does not work since it would require XCheckEvent
* instead of XNextEvent, but it is unstable...
*
* INPUT:
* active_Edit: (index + 1) of widget JWid[] corresponding to the active 
*              Edit Button
*************************************************************************/
int JLP_GDlg_X11::DrawBlinkTextCursor(int active_Edit)
{
int iJW, iJEdit, ix, iy, width0, height0, len; 
int blink_period, status;
Window iwin;
char buffer[80];
time_t lt;
struct tm *ptr;

if(!window_is_open) {
  fprintf(stderr,"DrawBlinkTextCursor/Error: window was not opened!\n");
  return(-1);
  }

iJEdit = active_Edit - 1;
if(iJEdit < 0 || iJEdit > nJEdit) {
   fprintf(stderr,"DrawBlinkCursor/Fatal error: iJEdit=%d (nJEdit=%d)\n", 
           iJEdit, nJEdit);
   exit(-1);
   }

iJW = JEdit_iJWid[iJEdit];
if(iJW < 0 || iJW > nJWid) {
   fprintf(stderr,"DrawBlinkCursor/Fatal error: iJWid = %d\n", iJW);
   exit(-1);
   }

width0 = 1;
height0 = (int)((double)JWid[iJW].win_height * 0.8);

// Compute location of text cursor:
 len = JWid[iJW].svalue_icur;
 strncpy(buffer, JWid[iJW].svalue, len);
 buffer[len] = '\0';
 ix = XTextWidth(Dialog_Font, "buffer", len);
 iy = (int)((double)JWid[iJW].win_height * 0.1);

 status = FromJWidgetToWindow_ID(JWid[iJW].JWid_id, &iwin); 
   if(status) {
      fprintf(stderr," Fatal error in FromJWidgetToWindow_ID\n");
      exit(-1);
   }
// Draw Cursor or erase it according to time:
 lt = time(NULL);
 ptr = localtime(&lt);
// Blink once per second:
 blink_period = 2;
 if((ptr->tm_sec % blink_period) == 0) {
    XSetForeground(Dialog_display, Dialog_gc, Gray[250]);
    XDrawLine(Dialog_display, iwin, Dialog_gc,
              ix, iy, ix, iy + height0);
    XSetForeground(Dialog_display, Dialog_gc, Black);
    }
 else
    XDrawLine(Dialog_display, iwin, Dialog_gc,
              ix, iy, ix, iy + height0);

return (0);
}
#endif /* End of JLP_USE_X11 */
