/*************************************************************************
* "jlp_igdev_x11_menu.cpp
* JLP_iGDev_X11 Class: menu routines
*
* X - window driver for JLP_PLOT (X11) 
* C++ version with classes
*
* JLP
* Version 19/10/2006 
**************************************************************************/
#if JLP_USE_X11 /* New flag to disable X11 if necessary */
#include "jlp_macros.h"
#include "jlp_splot.h"
#include "jlp_igdev_x11.h"
#include "jlp_x11_utils.h"

/*
#define DEBUG
*/

/*********************************************************************
* jlp_setup_menu
*
* INPUT:
*   items is an array with labels of "menu_slen" characters
*   menu_slen: length of labels 
*   menu_nitems: number of menu items 
*   menu_nsub: number of menu sub-items 
*   vertical: 1 if vertical menu (on the right side), 
              0 otherwise (on the top)
*
**********************************************************************/
int JLP_iGDev_X11::setup_menu(char *items, int nitems, int menu_nsub, int menu_slen,
                        int vertical)
{
int ix, iy, width, height;

if(vertical) {
 ix = (int)((double)Jgc0.offx * 1.4 + (double)Jgc0.axlen);
// Small offset in the bottom to avoid problems with border (mgo coord.):
 iy = 200;

 width = (int)(0.92 * (SCREEN_SIZE - ix));

// Leave some space on top for StatusBar and CursorPosition:
 height = (int)((double)(SCREEN_SIZE) * 0.95) - iy;
 } else {
 ix = 200;
 height = (int)((double)(SCREEN_SIZE) * 0.06);
 iy = SCREEN_SIZE - height;
 width = (int)((double)(SCREEN_SIZE) * 0.95);
 }

 CreateMenuButtons(items, nitems, menu_nsub, menu_slen, ix, iy, width, height,
                   vertical);

return(0);
}
/***************************************************************************
*
* INPUT:
* ix0, iy0: coordinates of origin of menu (bottom-left in mgo coordinates)
* nitems: number of items
***************************************************************************/
int JLP_iGDev_X11::CreateMenuButtons(char *items, const int nitems, 
                               const int menu_nsub, const int menu_slen,
                               const int ix0, const int iy0, const int width, 
                               const int height, const int vertical)
{
register int i, j;
int width0, step0, height0, ix, iy; 
int d_width, d_height, subwin_nitems;
double d_x, d_y;

if(nitems > NBUT_MAX) {
   fprintf(stderr,"CreateMenuButtons/Fatal error: too many buttons (%d > NBUT_MAX=%d)\n", nitems, NBUT_MAX);
   exit(-1);
   }

if(vertical) {
// width0, height0: x and y size (in mgo units)
 width0 = width;
// Maximum height of button is 1/10th of window height:
 step0 = MINI(3000, height / nitems);
 height0 = (int)(0.9 * (double)step0);
 } else {
// Maximum width of button is 1/10th of window width:
 width0 = MINI(6000, width / nitems);
 height0 = height;
 }

 ix = ix0; iy = iy0;
 d_width = (int)(width0 * Jgc0.g_dx);
 d_height = (int)(height0 * Jgc0.g_dy);
 Xgc0.nbuttons = 0;

// Create all buttons 
if(vertical) {
 for(i = 0; i < nitems; i++) { 
// Location of upper-left corner in mgo coordinates:
    iy = iy0 + (i + 1) * step0 - (step0 - height0)/2;
// d_x, d_y: upper-left corner in device coordinates:
    conv_mgo_to_dev(ix,iy,d_x,d_y);
    CreateButton(Xgc0.But[i], &(items[i*menu_nsub*menu_slen]), 
                 NINT(d_x), NINT(d_y), d_width, d_height); 
    Xgc0.nbuttons++;
  }    
 } else {
// Computing ix, iy, the location of the upper-left corner in mgo coordinates
// and converting to d_x, d_y (device coordinates)
 iy = iy0 + height0;
 for(i = 0; i < nitems; i++) { 
    ix = ix0 + i * width0;
    conv_mgo_to_dev(ix,iy,d_x,d_y);
    CreateButton(Xgc0.But[i], &(items[i*menu_nsub*menu_slen]), 
                 NINT(d_x), NINT(d_y), d_width, d_height); 
    Xgc0.nbuttons++;
  }    
 }

// User manual Xlib Vol 1 p 479 (and p 385)
// Subwindows of Xgc0.win won't appear until parent (Xgc0.win) is mapped:
XMapSubwindows(Xgc0.display, Xgc0.win);
XMapWindow(Xgc0.display, Xgc0.win);

// Create all popup windows for buttons 
// Larger width is needed for "miscellaneous" button:
 if(vertical) d_width = 190;

  for(i = 0; i < nitems; i++) { 

// Count the number of sub-items:
     for(j = 1; j < menu_nsub; j++)
        if(items[i*menu_nsub*menu_slen + menu_slen*j] == '\0') break; 
     subwin_nitems = j - 1;

// Create the corresponding popup window:
     if(subwin_nitems > 0)
        CreatePopupMenu(Xgc0.But[i], &(items[(i*menu_nsub + 1)*menu_slen]), 
                        menu_slen, subwin_nitems, d_width, d_height, vertical); 
   }    

return(0);
}
/***************************************************************************
* Create graphic contexts used for menu buttons
*
***************************************************************************/
int JLP_iGDev_X11::create_menu_gc()
{
int line_width;
unsigned long int myblack, mywhite;
XGCValues white_gcvalues, gray_gcvalues;
unsigned long int mygray;

myblack =  BlackPixel(Xgc0.display,Xgc0.screen);
mywhite =  WhitePixel(Xgc0.display,Xgc0.screen);

// Create graphic context for buttons (useable for all buttons
// since they have the same depth and same screen (cf. Vol 1 p 109))

// For buttons, I chose gray background and white foreground: 
 JLP_Xcolor_pixel(Xgc0.display, Xgc0.cmap, mygray, 80, 80, 80);
 gray_gcvalues.foreground = mywhite; 
 gray_gcvalues.background = mygray; 
 gray_gcvalues.function = GXcopy;
 gray_gcvalues.font = Xgc0.fonts->fid;
 Xgc0.Button_graygc = XCreateGC(Xgc0.display, Xgc0.win,
                            GCForeground|GCBackground|GCFunction|GCFont,
                            &gray_gcvalues);

 white_gcvalues.foreground = myblack; 
 white_gcvalues.background = mywhite; 
 white_gcvalues.function = GXcopy;
 white_gcvalues.font = Xgc0.fonts->fid;
 Xgc0.Button_whitegc = XCreateGC(Xgc0.display, Xgc0.win,
                                 GCForeground|GCBackground|GCFunction|GCFont,
                                 &white_gcvalues);
// To have thick lines for separators:
 line_width = 3;
 XSetLineAttributes(Xgc0.display,Xgc0.Button_whitegc,line_width,
                      LineSolid,CapButt,JoinBevel);

return(0);
}
/***************************************************************************
* Create a popup window attached to a button for the menu
*
* INPUT:
* d_width, d_height: box width and box height for a single item 
*                      (device coordinates) 
***************************************************************************/
int JLP_iGDev_X11::CreatePopupMenu(XBUTTON& MyBut, const char *items, 
                             const int menu_slen, const int nitems,
                             const int d_width, const int d_height,
                             const int vertical) 
{
register int i;
int d_x, d_y, border_width = 2, jj; 
// unsigned long int myblack =  BlackPixel(Xgc0.display,Xgc0.screen);
unsigned long int mywhite =  WhitePixel(Xgc0.display,Xgc0.screen);
XSetWindowAttributes attributes;
unsigned long int mygray;
Cursor mycursor;                       /* graphics cursor for Popup windows */

 JLP_Xcolor_pixel(Xgc0.display, Xgc0.cmap, mygray, 80, 80, 80);
 attributes.background_pixel = mygray;
 attributes.border_pixel = mywhite; 

// Location of the upper-left corner: 
if(vertical) {
// Put them on the left of the Buttons:
// Should not be superimposed on the Button to avoid conflicts!
// jj is a parameter to tune this offset:
    jj = 4;
    d_x = MyBut.x - d_width - jj;
    d_y = MyBut.y;
    d_x = MINI(Jgc0.devwidth - d_width - jj, d_x);
    d_x = MAXI(d_x, jj);
    d_y = MINI(Jgc0.devheight - d_height * nitems - jj, d_y);
    d_y = MAXI(d_y, jj);
} else {
    jj = 4;
    d_x = MyBut.x;
    d_y = MyBut.y + jj;
}
    MyBut.subwin = XCreateWindow(Xgc0.display, Xgc0.win, d_x, d_y,
                                       d_width, d_height * nitems, 
                                       border_width,
                                       CopyFromParent, InputOutput,
                                       CopyFromParent, 
                                       CWBorderPixel|CWBackPixel, &attributes); 
// "hand" cursor for popup window (same as for buttons):
    mycursor = XCreateFontCursor(Xgc0.display, XC_hand2);
    XDefineCursor(Xgc0.display, MyBut.subwin, mycursor);

// Events to be monitored: 
    XSelectInput(Xgc0.display,MyBut.subwin,ExposureMask|ButtonPressMask);

    MyBut.subwin_height = d_height;
    MyBut.subwin_width = d_width;
    MyBut.subwin_x = d_x;
    MyBut.subwin_y = d_y;
// Load labels:
    MyBut.subwin_nitems = nitems;
    for(i = 0; i < nitems; i++)
        sprintf(MyBut.subwin_label[i],&(items[menu_slen * i]));
    MyBut.subwin_is_created = 1;
    MyBut.subwin_is_mapped = 0;

return(0);
}
/***************************************************************************
* Create a button for the menu
*
* INPUT:
* x_ul, y_ul: coordinates of upper-left corner of the button 
*               (device coordinates)
* d_width, d_height: width and height (device coordinates) 
***************************************************************************/
int JLP_iGDev_X11::CreateButton(XBUTTON &MyBut, const char *label, const int x_ul, 
                          const int y_ul, const int d_width, const int d_height) 
{
int border_width = 2;
// unsigned long int myblack =  BlackPixel(Xgc0.display,Xgc0.screen);
unsigned long int mywhite =  WhitePixel(Xgc0.display,Xgc0.screen);
XSetWindowAttributes attributes;
unsigned long int mygray;
Cursor mycursor;                       /* graphics cursor for buttons */

    strcpy(MyBut.label, label);

    JLP_Xcolor_pixel(Xgc0.display, Xgc0.cmap, mygray, 80, 80, 80);
    attributes.background_pixel = mygray;
    attributes.border_pixel = mywhite; 
    MyBut.win = XCreateWindow(Xgc0.display, Xgc0.win, x_ul, y_ul,
                                    d_width, d_height, border_width,
                                    CopyFromParent, InputOutput,
                                    CopyFromParent, 
                                    CWBorderPixel|CWBackPixel, &attributes); 
// "hand" cursor for buttons:
    mycursor = XCreateFontCursor(Xgc0.display, XC_hand2);
    XDefineCursor(Xgc0.display, MyBut.win, mycursor);

// Events to be monitored: 
    XSelectInput(Xgc0.display,MyBut.win,
                 ExposureMask|ButtonPressMask|ButtonReleaseMask|
                 EnterWindowMask|LeaveWindowMask);

// Origin of button (upper-left):
    MyBut.x = x_ul;
    MyBut.y = y_ul;
    MyBut.is_active = 0;
/*
printf(" CreateMenuButtons: x_ul=%d y_ul=%d d_width0=%d d_height=%d\n", 
          x_ul, y_ul, d_width, d_height);
printf(" But.x=%d But.y=%d \n", MyBut.x, MyBut.y); 
*/
// Size of button:
    MyBut.height = d_height;
    MyBut.width = d_width;

// Initialization of subwindow parameters:
    MyBut.subwin_is_created = 0;
    MyBut.subwin_is_mapped = 0;
    MyBut.subwin_nitems = 0;
    MyBut.subwin = 0;

return(0);
}
/***************************************************************************
* Draw label on button child window 
* with the graphic context MyGC
*
***************************************************************************/
int JLP_iGDev_X11::DrawButton(XBUTTON& MyBut)
{
int slen, x, y;
unsigned long int mygray;

  slen = strlen(MyBut.label);
  x = (int)(MyBut.width * 0.05);
// Warning y increasing to the bottom:
  y = (int)(MyBut.height * 0.8);

if(MyBut.is_active) {
// Change the background value: 
  XSetWindowBackground(Xgc0.display, MyBut.win, Xgc0.gc_foreground); 
  XClearWindow(Xgc0.display, MyBut.win);
  XDrawString(Xgc0.display, MyBut.win, Xgc0.Button_whitegc, x,  
              y, MyBut.label, slen);
  }
else
  {
// Change the background value: 
  JLP_Xcolor_pixel(Xgc0.display, Xgc0.cmap, mygray, 80, 80, 80);
  XSetWindowBackground(Xgc0.display, MyBut.win, mygray); 

// Clearing repaints the background: 
  XClearWindow(Xgc0.display, MyBut.win);
  XDrawString(Xgc0.display, MyBut.win, Xgc0.Button_graygc, x,  
              y, MyBut.label, slen);
  }

return(0);
}
/***************************************************************************
*
***************************************************************************/
int JLP_iGDev_X11::DrawPopupMenu(XBUTTON& MyBut)
{
int slen, x1, y1, x2, y2;
register int i;

/**************************************************************
* Example of lightgray background: ...

unsigned long light_gray;
 JLP_Xcolor_pixel(Xgc0.display, Xgc0.cmap, light_gray, 120, 120, 120);

 XSetForeground(Xgc0.display, Xgc0.gc, 
                BlackPixel(Xgc0.display,Xgc0.screen));

// To have a background different from parent:
XSetWindowBackground(Xgc0.display, MyBut.subwin,light_gray); 

// Clearing repaints the background: 
XClearWindow(Xgc0.display, MyBut.subwin);
***************************************************************/

// Draw popup menus of Buttons
  for(i = 0; i < MyBut.subwin_nitems; i++) {

// Warning y increasing to the bottom:
    x1 = (int)(MyBut.subwin_width * 0.05);
    y1 = (int)(MyBut.subwin_height * ((double)i + 0.8));
    slen = strlen(MyBut.subwin_label[i]);
    XDrawString(Xgc0.display, MyBut.subwin, Xgc0.Button_graygc, x1,  
                y1, MyBut.subwin_label[i], slen);

// Draw a straight line to separate the items:
   x2 = MyBut.subwin_width;  
   y2 = (int)(MyBut.subwin_height * ((double)i + 1.0));
   XDrawLine(Xgc0.display, MyBut.subwin, Xgc0.Button_graygc,
             0, y2, x2, y2);
  }

return(0);
}
/***************************************************************
*
***************************************************************/
int JLP_iGDev_X11::select_menu(int& select, int& subselect)
{
register int i, j;
int x1, y1;
XEvent event;

 if(!Xgc0.nbuttons) {
    select = 0;
    subselect = 0;
    return(-1);
    }

/* Desactivate all buttons, and paint them back as inactive: */
for(i = 0; i < Xgc0.nbuttons; i++) {
   Xgc0.But[i].is_active = 0;
   DrawButton(Xgc0.But[i]);
   }

while(1) {
  XNextEvent(Xgc0.display, &event);
// Redraw image if popup window has hidden part of it:
     if(event.xbutton.window == Xgc0.win) {
      if(event.type == Expose) redraw(); 
     } else {
/**********************************************************************
* Events on button child windows: 
**********************************************************************/
// Loop on all buttons:
      for(i = 0; i < Xgc0.nbuttons; i++) {

// Handle menu popup windows:
         if(event.xbutton.window == Xgc0.But[i].subwin
            && Xgc0.But[i].subwin_is_mapped) {
           switch(event.type) {
             case ButtonPress:
// Coordinates of point:
               x1 = event.xkey.x;
               y1 = event.xkey.y;
               j = y1 / Xgc0.But[i].subwin_height;
               select = i;
               subselect = j;
// Draw Button as an active mode:
               Xgc0.But[i].is_active = 1;
               DrawButton(Xgc0.But[i]);
// Unmap subwindow and exit:
               Xgc0.But[i].subwin_is_mapped = 0;
               XUnmapWindow(Xgc0.display, Xgc0.But[i].subwin);
// Not visible, so I refresh the screen (redraw and call XFlush):
               JLP_iGDev_X11::gflush();
// Redraw underlying image since popup window may have hidden part of it:
               redraw();
               return(0);
               break;
             case Expose: 
               DrawPopupMenu(Xgc0.But[i]);
               break;
             default:
               break;
             }
// Handle menu buttons:
         } else if(event.xbutton.window == Xgc0.But[i].win) {
           switch(event.type) {
             case ButtonPress:
               select = i;
// Return if there is no popup window attached to this button
               if(!Xgc0.But[i].subwin_is_created) {
// Draw Button as an active mode:
                  Xgc0.But[i].is_active = 1;
                  DrawButton(Xgc0.But[i]);
                  return(0);
                  }
// Do not break here!
// because ButtonPress should be equivalent to EnterNotify
// in order to create Popup menus if not yet done (useful
// when selecting menu directly from "cursor" mode):
             case EnterNotify:
// Close all other subwindows if necessary:
               for(j = 0; j < Xgc0.nbuttons; j++) {
                 if(j != i && Xgc0.But[j].subwin_is_mapped) {
                    XUnmapWindow(Xgc0.display, Xgc0.But[j].subwin);
                    Xgc0.But[j].subwin_is_mapped = 0;
                   }
                 }
// Open subwindow attached to this button:
               if(Xgc0.But[i].subwin_is_created 
                  && !Xgc0.But[i].subwin_is_mapped) {
                   Xgc0.But[i].subwin_is_mapped = 1;
                   XMapWindow(Xgc0.display, Xgc0.But[i].subwin);
                   DrawPopupMenu(Xgc0.But[i]);
                  } // EOF event on subwin
               break;
             case LeaveNotify:
               break;
/* Case expose */
             case Expose:
               DrawButton(Xgc0.But[i]);
               break;
             default: 
               break;
              }  // End of switch 
            }  // End of event on button #i
         }  // End of loop on all buttons
     } // End of event on button windows
 }    /* end while */
return(-1);
}
/**********************************************************************
* Handle a single event on button child windows
* Check if button was pressed
* (called by JLP_iGDev_X11::cursor(), JLP_iGDev_X11::winlimits(), JLP_iGDev_X11::get_circles())
*
* OUTPUT:
*  select: button number that was pressed
*  status = 1  if button has been pressed
*           0  otherwise
**********************************************************************/
int JLP_iGDev_X11::button_was_pressed(XEvent& event, int& select)
{
register int i;

select = -1;

// Loop on all buttons:
      for(i = 0; i < Xgc0.nbuttons; i++) {
         if(event.xbutton.window == Xgc0.But[i].win) {
           switch(event.type) {
/* Case expose */
             case Expose:
               DrawButton(Xgc0.But[i]);
               break;
             case ButtonPress:
/*
               printf("OK: Button #%d (%s) has been pressed!\n",
                       i, Xgc0.But[i].label);
*/
               select = i;
               return(1);
               break;
             default:
               break;
              }  // End of switch
            }  // End of event on button #i
         }  // End of loop on all buttons

return(0);
}
#endif   /* End of JLP_USE_X11 */
