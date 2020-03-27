/*************************************************************************
* jlp_x11_utils.h
* Set of programs useful for both JLP_GDev_X11 and JLP_GDlg_X11 Classes 
*
* JLP
* Version 17/11/2006 
**************************************************************************/
#ifdef JLP_USE_X11           /* New flag to disable X11 if necessary */

#ifndef __jlp_x11_utils_h              // BOF sentry
#define __jlp_x11_utils_h
#include <stdio.h>
#include <stdlib.h>                    // For "exit()"
#include <math.h>
#include <ctype.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/cursorfont.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>            /* For Color Maps */

int JLP_LoadFonts(Display *display, XFontStruct **font_struct);
int JLP_Xcolor_pixel(Display *display, Colormap& cmap,
                     unsigned long int &pixel, int r, int g, int b);
int JLP_Xcolor(Display *display, Colormap& cmap, XColor &xcolor,
               int r, int g, int b);
#endif   /* EOF sentry */

#endif   /* End of JLP_USE_X11 */
