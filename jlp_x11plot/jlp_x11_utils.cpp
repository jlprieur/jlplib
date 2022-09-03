/*************************************************************************
* jlp_x11_utils.cpp
* Set of programs useful for the JLP_X11 and JLP_Dlg_X11 Classes 
*
* JLP
* Version 17/11/2006 
**************************************************************************/
#if JLP_USE_X11   /* New flag to disable X11 if necessary */

#include "jlp_x11_utils.h"

/*************************************************************
* Load fonts
* (You can check which fonts are available with the program "xfontsel") 
*
* OUTPUT:
* **font_struct
************************************************************/
int JLP_LoadFonts(Display *display, XFontStruct **font_struct)
{
/* Fonts:  look for courier-18, if not present use simply 9x15*/
  *font_struct = XLoadQueryFont(display,"-*-courier-medium-r-*-*-14-*");
    if(*font_struct == NULL) {
     printf("LoadFonts/Error loading fonts: courier-medium 14 not found\n");
     *font_struct = XLoadQueryFont(display,"9x15");
     if(*font_struct == NULL) {
#ifdef DEBUG
       printf("LoadFonts/Error loading fonts: courier and 9x15 not found\n");
#endif
       *font_struct = XLoadQueryFont(display,"fixed");
        if(*font_struct == NULL) {
          printf("setup/Fatal error loading fonts: 'fixed' not found\n");
          exit(-1);
          }
       }
   } 

return(0);
}
/**************************************************************
* Get value of the cell in the colormap corresponding
* to (r,g,b) value
*
* INPUT:
* r,g,b between 0 and 255
*
* OUTPUT:
*  xcolor.pixel: full XColor structure of xcolor is initialized
***************************************************************/
int JLP_Xcolor_pixel(Display *display, Colormap& cmap,
                     unsigned long int &pixel, int r, int g, int b)
{
int status = 0;
XColor xcolor;

pixel = 0;
  status =  JLP_Xcolor(display, cmap, xcolor, r, g, b);

  if(!status) pixel = xcolor.pixel;

return(status);
}
/**************************************************************
* Get value of the cell in the colormap corresponding
* to (r,g,b) value
*
* INPUT:
* r,g,b between 0 and 255
*
* OUTPUT:
*  xcolor.pixel: full XColor structure of xcolor is initialized
***************************************************************/
int JLP_Xcolor(Display *display, Colormap& cmap, XColor &xcolor, 
               int r, int g, int b)
{
int status = 0;
int white_pixel;

   white_pixel = WhitePixel(display,DefaultScreen(display));
   xcolor.red = (white_pixel * r) / 255;
   xcolor.green = (white_pixel * g) / 255;
   xcolor.blue = (white_pixel * b) / 255;
   xcolor.flags = DoRed | DoGreen | DoBlue;

/* XAllocColor returns the index of the (readonly) colorcell that contains the
*   RGB value closest to the one required from the ASCII color data base
* (loaded in xcolor.pixel)
*/
   if (XAllocColor(display, cmap, &xcolor) == 0) {
      fprintf(stderr," JLP_Xcolor_pixel/Error: can't find color (%d,%d,%d)\n",
              r, g, b);
      status = -1;
    }

return(status);
}

#endif  /* End of JLP_USE_X11 */
