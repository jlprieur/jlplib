/*************************************************************************
* "jlp_gdev_x11_static.cpp
* JLP_GDev_X11 Class:  routines to define Static buttons and Check buttons
*
* X - window driver for JLP_PLOT (X11) 
* C++ version with classes
*
* JLP
* Version 14/11/2006 
**************************************************************************/
#ifdef JLP_USE_X11              /* New flag to disable X11 if necessary */

#include "jlp_macros.h"
#include "jlp_gdev_x11.h"
#include "jlp_x11_utils.h"

/*
#define DEBUG
*/

/***************************************************************************
* Create a static button (used for StatusBar, CursorPosition, etc) 
*
* INPUT:
* ix, iy: lower-left corner in mgo coordinates
* width, height: size in mgo coordinates
***************************************************************************/
int JLP_GDev_X11::CreateStaticButton(XSTATIC &MyStat, const char *label, 
                                const int ix, const int iy, 
                                const int width, const int height) 
{
int border_width = 1;
// unsigned long int myblack =  BlackPixel(Xgc0.display,Xgc0.screen);
unsigned long int mywhite =  WhitePixel(Xgc0.display,Xgc0.screen);
XSetWindowAttributes attributes;
unsigned long int mygrey;
int x_ul, y_ul, d_width, d_height;
double d_x, d_y;
double g_dx, g_dy;

  g_dx = (double)Jgc0.dev_width / (double)SCREEN_SIZE;
  g_dy = (double)Jgc0.dev_height / (double)SCREEN_SIZE;

  conv_mgo_to_dev(ix, iy, &d_x, &d_y);
  x_ul = NINT(d_x);
  y_ul = NINT(d_y);
/*
* x_ul, y_ul: coordinates of upper-left corner of the button 
*               (device coordinates)
* d_width, d_height: width and height (device coordinates) 
*/
  d_width = (int)((double)width * g_dx);
  d_height = (int)((double)height * g_dy);
  y_ul = MAXI(0, y_ul - d_height);

  strcpy(MyStat.label, label);

  JLP_Xcolor_pixel(Xgc0.display, Xgc0.cmap, mygrey, 40, 40, 40);
  attributes.background_pixel = mygrey;
  attributes.border_pixel = mywhite; 
  MyStat.win = XCreateWindow(Xgc0.display, Xgc0.win, x_ul, y_ul,
                             d_width, d_height, border_width,
                             CopyFromParent, InputOutput,
                             CopyFromParent, 
                             CWBorderPixel|CWBackPixel, &attributes); 

// Events to be monitored: 
  XSelectInput(Xgc0.display,MyStat.win,ExposureMask);

// Origin of button (upper-left) on the main window:
  MyStat.x = x_ul;
  MyStat.y = y_ul;

// Size of button:
  MyStat.height = d_height;
  MyStat.width = d_width;

  MyStat.is_created = 1;

// Makes it visible:
  XMapWindow(Xgc0.display, Xgc0.StatusBar.win);

return(0);
}
/***************************************************************************
* To erase everything in status bar
***************************************************************************/
int JLP_GDev_X11::EraseStatusBar()
{
int status = -1;
  if(Xgc0.StatusBar.is_created) {
     XClearWindow(Xgc0.display, Xgc0.StatusBar.win);
     strcpy(Xgc0.StatusBar.label," ");
     status = 0;
    }

return(status);
}
/***************************************************************************
* To erase everything in CursorPosition 
***************************************************************************/
int JLP_GDev_X11::EraseCursorPosition()
{
int status = -1;
  if(Xgc0.CursorPosition.is_created) {
     XClearWindow(Xgc0.display, Xgc0.CursorPosition.win);
     strcpy(Xgc0.CursorPosition.label," ");
     status = 0;
    }

return(status);
}
/***************************************************************************
* To draw a label on the status bar
***************************************************************************/
int JLP_GDev_X11::DrawToStatusBar(char *label)
{
int status = -1;

  if(Xgc0.StatusBar.is_created) {

// Save label for further display:
    if(strlen(label) > 99) label[99] = '\0';
    strcpy(Xgc0.StatusBar.label,label);

// Call DrawStaticButton
    status = DrawStaticButton(Xgc0.StatusBar);
    JLP_GDev_X11::gflush();
  }

return(status);
}
/***************************************************************************
* To draw label on CursorPosition button
***************************************************************************/
int JLP_GDev_X11::DrawToCursorPosition(char *label)
{
int status = -1;

  if(Xgc0.CursorPosition.is_created) {

// Save label for further display:
    if(strlen(label) > 99) label[99] = '\0';
    strcpy(Xgc0.CursorPosition.label,label);

// Call DrawStaticButton
    status = DrawStaticButton(Xgc0.CursorPosition);
  }

return(status);
}
/***************************************************************************
* Update static button 
***************************************************************************/
int JLP_GDev_X11::DrawStaticButton(XSTATIC &MyStat)
{
int status = -1, x_ul, y_ul;
  if(Xgc0.StatusBar.is_created) {
    XClearWindow(Xgc0.display, MyStat.win);
// Small offset:
    x_ul = 10;
    y_ul = (int)((double)MyStat.height * 0.8); 
    XDrawString(Xgc0.display, MyStat.win, Xgc0.gc, x_ul, y_ul,
                MyStat.label, strlen(MyStat.label));
    status = 0;
    }

return(status);
}
#endif  /* End of JLP_USE_X11 */
