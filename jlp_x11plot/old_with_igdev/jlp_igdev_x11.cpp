/*************************************************************************
* JLP_iGDev_X11 Class
* X - window driver for JLP_PLOT (X11) 
* C++ version with classes
*
* For LUT:
*   mylut_x11[]: array of X11 addresses of allocated read/write color cells
*   r[], g[], b[]: colors of corresponding cells
*
* JLP
* Version 11/01/2007 
**************************************************************************/
#if JLP_USE_X11 /* New flag to disable X11 if necessary */

#include "jlp_macros.h"
#include "jlp_splot.h"
#include "jlp_x11_def.h"
#include "jlp_igdev_x11.h"
#include "jlp_x11_utils.h"

#define icon_bitmap_width 20
#define icon_bitmap_height 20
/* Cf User Manual p 60 */
static char icon_bitmap_bits[] = {
0x60, 0x00, 0x01, 0xb0, 0x00, 0x07, 0x0c, 0x03, 0x00, 0x04, 0x04, 0x00,
0xc2, 0x18, 0x00, 0x03, 0x30, 0x00, 0x01, 0x60, 0x00, 0xf1, 0xdf, 0x00,
0xc1, 0xf0, 0x01, 0x82, 0x01, 0x00, 0x02, 0x03, 0x00, 0x02, 0x0c, 0x00,
0x02, 0x38, 0x00, 0x04, 0x60, 0x00, 0x04, 0xe0, 0x00, 0x04, 0x38, 0x00,
0x84, 0x06, 0x00, 0x14, 0x14, 0x00, 0x0c, 0x34, 0x00, 0x00, 0x00, 0x00};

/*
#define DEBUG
*/

#ifndef PI 
#define PI 3.1415926536
#endif

#define MAX_POLYGON 20		/* maximum corners for PTYPE 3 points */
/* JLP 91 200 --> 2000 */
#define START_NVEC 2000		/* starting value for max_nvec */
#define FONT_SIZE   12.         /* Size of fonts */

/***************************************************************
 int screen1 : screen number (usually 0) 
 int nvec: number of vectors to be drawn 
 int max_nvec: max number of vectors allocated 
 Cursor cursor: graphics cursor to be used
****************************************************************/

/*************************************************************
* For images
* Setup window parameters according to the device name
*
* INPUT:
* plotdev
* title
* nx1, ny1: size of input image
*
* OUTPUT:
* offx, offy, axlen, aylen
* dev_width, dev_height : size of window in device coordinates
* TeX_flag
* devtype1 = 1 if X11, 2 if postscript, ...
************************************************************************/
int JLP_iGDev_X11::setup_device_for_image(const char *plotdev, const char *title,
                                    const int nx1, const int ny1, int& offx,
                                    int& offy, int& axlen, int& aylen,
                                    int& dev_width, int& dev_height, 
                                    int& TeX_flag, int& devtype, 
                                    int& landscape, int idev1) 
{
int status = 0;

  offx = 3500; offy = 3500; axlen = 26000; aylen = 26000;
  dev_width = 300; dev_height = 300; 

// Not yet possible to rotate the image with X11:
  landscape = 0;

// Device type (for X11 devtype=1)
  devtype = 1;

switch(plotdev[1])
    {
/* Display of an image with X11: ("XDISPLAY of xdisplay")*/
    case 'D':
    case 'd':
// Test on nx1, ny1:
      if((nx1 < 0 || nx1 > 2000) || (ny1 < 0 || ny1 > 2000))
        { fprintf(stderr, 
                  "setup_device_for_image/Fatal error: nx = %d ny = %d\n",
                   nx1, ny1);
          exit(-1);
        }
      if(nx1 > 100) {
        dev_width  = 60+nx1+150;
        dev_height = 120+ny1+40;
        offx = (int)((double)(SCREEN_SIZE*60)/(double)(dev_width));
        offy = (int)((double)(SCREEN_SIZE*120)/(double)(dev_height));
      } else {
        dev_width  = 30+nx1+75;
        dev_height = 60+ny1+20;
        offx = (int)((double)(SCREEN_SIZE*30)/(double)(dev_width));
        offy = (int)((double)(SCREEN_SIZE*60)/(double)(dev_height));
        }
      axlen = (int)( (double) SCREEN_SIZE * nx1/(double)dev_width);
      aylen = (int)( (double) SCREEN_SIZE * ny1/(double)dev_height);
      break;
    default:
      devtype = 0;
      printf("setup_device_for_image/Error: unknown X11 graphic device\n (%s is unknown for images)\n",
             plotdev);
      status = -1;
      break;
    }

// TeX flag
  TeX_flag = 0;

/* A trick to select TeX interactively: if upper case, TeX selected */
 if(plotdev[0] == 'X') TeX_flag = 1;

// Maximum level for LUT values :
// 65535 seems good with my pc (August 2013)
  MaxColorLevelForLUT = 65535;

return(status);
}
/*************************************************************
* Open the X11 device
*
* INPUT:
* title: string to be displayed on top of the window
* dev_width1, dev_height1: size of window in device coordinates
* landscape: flag set to one if plot is rotated by 90 degrees
*            (Not used here)
*
* OUTPUT:
* Jgc0.devwidth, Jgc0.devheight: window size in device coordinates
************************************************************/
int JLP_iGDev_X11::open(const char* title, int dev_width1, int dev_height1,
                  const int landscape)
{
//char *display_name=NULL;
char display_name[40];
unsigned long int mywhite, myblack;
int border_width;
Pixmap icon_pixmap;
XSetWindowAttributes attributes;
XSizeHints size_hints;
register int i;

if(italk > 1) {
  printf(" JLP/X11 graphic environment Version 11-01-2007 \n");
  printf(" JLP_iGDev_X11::setup/WARNING: This program assumes %d bits per pixel of video board (%d colors)\n", 
        BIT_PER_VIDEO_PIXEL, (int)pow(2.,(double)BIT_PER_VIDEO_PIXEL));
}

/* Open display and return display_name which is the value of 
   environment variable DISPLAY : */
    *display_name = '\0';
    if((Xgc0.display = XOpenDisplay(display_name)) == NULL)
     {
      printf(" Cannot connect to XServer %s\n",
		   XDisplayName(display_name));
      exit(-1);
     }
      
/* Select the screen (0.0 => 0    or 0.1 => 1  ,...) */
Xgc0.screen = DefaultScreen(Xgc0.display);

mywhite =  WhitePixel(Xgc0.display,Xgc0.screen);
myblack =  BlackPixel(Xgc0.display,Xgc0.screen);
/* JLP2006: WhitePixel=16777216, MaxColorLevelForLUT=65535 BlackPixel=0 
       printf("White=%d Black=%d MaxColorLevelForLUT=%d\n",
              (int)mywhite, myblack, MaxColorLevelForLUT);
*/
	
/* Window size in device coordinates: */
Jgc0.devwidth = dev_width1; 
Jgc0.devheight = dev_height1;

/* Program imposes size parameters: */
size_hints.flags = PPosition|PSize|PMinSize|PMaxSize;
/* if we put USPosition displays the window in size_hints.x and ..y */
size_hints.x = 0;
size_hints.y = 0;
// Forces fixed size, to simplify cursor handling:
size_hints.width = Jgc0.devwidth; 
size_hints.height = Jgc0.devheight; 
size_hints.min_width = Jgc0.devwidth; 
size_hints.min_height = Jgc0.devheight;
size_hints.max_width = Jgc0.devwidth; 
size_hints.max_height = Jgc0.devheight;

/* Create opaque window: */
  border_width = 2;
/* border_width, foreground, background)
  Xgc0.win = XCreateSimpleWindow(Xgc0.display,
                                 RootWindow(Xgc0.display,Xgc0.screen),
                                 size_hints.x,size_hints.y,
                                 size_hints.width,size_hints.height,
                                 border_width,mywhite,myblack);
*/
/* Background */
  attributes.background_pixel = myblack;

// Need to use CreateWindow to set it as INputOutput
// and enable it to have children (needed for buttons)
// JLP98: DefaultDepth=16 on linux/ts-jlp 
  Xgc0.win = XCreateWindow(Xgc0.display, RootWindow(Xgc0.display,Xgc0.screen),
                           size_hints.x,size_hints.y, size_hints.width,
                           size_hints.height, border_width, 
                           DefaultDepth(Xgc0.display,Xgc0.screen),
                           InputOutput,CopyFromParent,
                           CWBackPixel, &attributes);
/* Last argument is background value  for window
   and last but one argument is border value (width is set to 12 here)*/

/* Set properties for window manager: */
/*
  XSetStandardProperties(Xgc0.display,Xgc0.win,"X11plot","X11plot",icon_pixmap,
                         NULL,0,&size_hints);
*/
/* Cf User Manual, p 60 */
  icon_pixmap = XCreateBitmapFromData(Xgc0.display,Xgc0.win, icon_bitmap_bits,
                                      icon_bitmap_width, icon_bitmap_height);
  XSetStandardProperties(Xgc0.display,Xgc0.win,title,title,icon_pixmap,
                         NULL,0,&size_hints);

/* Select event types wanted: */
 XSelectInput(Xgc0.display,Xgc0.win,
              ExposureMask|ButtonPressMask|ButtonReleaseMask|ButtonMotionMask
              |PointerMotionMask);

  Xgc0.backing = XCreatePixmap(Xgc0.display, Xgc0.win, Jgc0.devwidth, 
                               Jgc0.devheight, 
                               DefaultDepth(Xgc0.display,Xgc0.screen));

// Display window: 
   XMapRaised(Xgc0.display,Xgc0.win);

// Allocate memory for vectors 
  Xgc0.max_nvec = START_NVEC;
     if((Xgc0.xvec = (XSegment *)malloc((unsigned)(Xgc0.max_nvec+1)*
					 sizeof(XSegment))) == NULL) {
         fprintf(stderr,"Can't allocate vectors in x_setup\n");
         exit(-1);
      }
  Xgc0.nvec = 0;
  Xgc0.cmap = DefaultColormap(Xgc0.display,Xgc0.screen);

/* Define the default cursor as an arrow: */
  Xgc0.default_cursor = XCreateFontCursor(Xgc0.display, XC_arrow);
  XDefineCursor(Xgc0.display, Xgc0.win, Xgc0.default_cursor);

// Create all GC's that will be used by other routines of this class 
// (gc, erasegc, cursorgc)
   JLP_iGDev_X11::create_win_gc();

/* Erase the screen (with erasegc) */
  JLP_iGDev_X11::erase();

// Initialization of number of buttons (for the menu):
  Xgc0.nbuttons = 0;

/* Initialize Button structure: */
 for(i = 0; i < NBUT_MAX; i++) {
   Xgc0.But[i].subwin_is_created = 0;
   Xgc0.But[i].subwin_is_mapped = 0;
   Xgc0.But[i].win = 0;
   Xgc0.But[i].subwin = 0;
   }

// Create graphic context for the menu buttons:
 JLP_iGDev_X11::create_menu_gc();

// Status Bar:
  Xgc0.StatusBar.is_created = 0;
  Xgc0.CursorPosition.is_created = 0;

return(0);
}
/*************************************************************
* Create graphic contexts used with the class members 
* i.e. : 
*   gc, erasegc, cursorgc
*
************************************************************/
int JLP_iGDev_X11::create_win_gc()
{
XGCValues gcvalues, evalues, cvalues;
unsigned long int mywhite, myblack;

mywhite =  WhitePixel(Xgc0.display,Xgc0.screen);
myblack =  BlackPixel(Xgc0.display,Xgc0.screen);
	
/* Create GC for text and drawing: */ 
  gcvalues.function = GXcopy;
  gcvalues.fill_style = FillSolid;

// Look for fonts:
  JLP_LoadFonts(Xgc0.display, &Xgc0.fonts);
  gcvalues.font = Xgc0.fonts->fid; 

/* Note that foreground is used in all primitives whereas
   background is used only in XCopyPlane, ...*/
  gcvalues.foreground = Xgc0.gc_foreground = mywhite; 
  gcvalues.background = Xgc0.gc_background = myblack; 

// Create graphic context (useable for both Xgc0.win and Xgc0.backing
// since they have the same depth and same screen (cf. Vol 1 p 109))
  Xgc0.gc = XCreateGC(Xgc0.display, Xgc0.win,
                      GCForeground|GCBackground|GCFunction|GCFillStyle|GCFont,
	              &gcvalues);

// Set line width and line style:
   SetLineWidthAndType_to_gc(Xgc0.gc, Mgc0.lwidth, Mgc0.lltype);

// Erase GC function is chosen according to the background value of Xgc0.gc:
  if(Xgc0.gc_background == myblack)
    evalues.function = GXclear;
  else
     evalues.function = GXset;

  evalues.font = Xgc0.fonts->fid; 
  evalues.foreground = Xgc0.gc_foreground; 
  evalues.background = Xgc0.gc_background; 

  Xgc0.erasegc = XCreateGC(Xgc0.display, Xgc0.win, 
                           GCForeground|GCBackground|GCFunction|GCFont, 
                           &evalues);

//************** Graphic context for big crosshair cursor: 
  cvalues.function = GXxor;
  cvalues.background = Xgc0.gc_background; 
  cvalues.foreground = Xgc0.gc_foreground; 
  Xgc0.cursorgc = XCreateGC(Xgc0.display, Xgc0.win, 
                           GCForeground|GCBackground|GCFunction, 
                           &cvalues);

// Select faster parameters:
   SetLineWidthAndType_to_gc(Xgc0.cursorgc, 0, 0);

return(0);
}
/*************************************************************
* Draw a line
*
* INPUT:
* x1, y1: coordinates of starting point (mgo coordinates)
* x2, y2: coordinates of ending point (mgo coordinates)
* lwidth: line width
*
* OUTPUT:
* Mgc0.lwidth updated
************************************************************/
int JLP_iGDev_X11::line_device(int x1, int y1, int x2, int y2, int lwidth)
{
double d_x1, d_y1, d_x2, d_y2;

// When lwidth has to be changed, draw whole set with old settings
// and empty drawing vector
    if(lwidth != Mgc0.lwidth) {
       JLP_iGDev_X11::empty_xvec();
       Mgc0.lwidth = lwidth;
       }

// Conversion from mgo to device coordinates:
    conv_mgo_to_dev(x1, y1, d_x1, d_y1); 
    conv_mgo_to_dev(x2, y2, d_x2, d_y2); 

// Load this new (start,end) pair to drawing vector:
    (Xgc0.xvec)[Xgc0.nvec].x1 = (short int)d_x1;
    (Xgc0.xvec)[Xgc0.nvec].y1 = (short int)d_y1;
    (Xgc0.xvec)[Xgc0.nvec].x2 = (short int)d_x2;
    (Xgc0.xvec)[Xgc0.nvec].y2 = (short int)d_y2;

// When drawing vector is filled, draw whole set of (start,end) pairs:
    if(++(Xgc0.nvec) >= Xgc0.max_nvec) JLP_iGDev_X11::empty_xvec();

// Update current position (mgo coordinates):
   Jgc0.xp = x2;
   Jgc0.xp = y2;

return(0);
}

/*************************************************************
* To set line attributes to a graphic context 
*
* lwidth = width of the line
* ltype = 0 solid line, 1=dashed line, 2=dotted line, etc
*************************************************************/
int JLP_iGDev_X11::SetLineWidthAndType_to_gc(GC& MyGC, int lwidth, int ltype)
{
// By default, set to zero (i.e., fast line of width 1)
unsigned int line_width = 0;

 if(lwidth >= 0) line_width = lwidth;

/* Nice but slow: 
 XSetLineAttributes(Xgc0.display,MyGC,line_width,LineSolid,
                      CapRound,JoinRound);
*/

 if(ltype == 1)
   XSetLineAttributes(Xgc0.display,MyGC,line_width,
                      LineOnOffDash,CapButt,JoinBevel);
 else if (ltype > 1)
   XSetLineAttributes(Xgc0.display,MyGC,line_width,
                      LineDoubleDash,CapButt,JoinBevel);
/* JLP96: select faster parameters (VERY EFFICIENT!): */
 else
   XSetLineAttributes(Xgc0.display,MyGC,line_width,
                      LineSolid,CapButt,JoinBevel);
return(0);
}
/*************************************************************
* To draw and empty drawing vector xvec
*
*************************************************************/
int JLP_iGDev_X11::empty_xvec()
{

/* Return directly if empty vector buffer: */
   if(!Xgc0.nvec) return(0); 

   XDrawSegments(Xgc0.display, Xgc0.backing, Xgc0.gc, Xgc0.xvec, Xgc0.nvec);
   Xgc0.nvec = 0;

return(0);
}

/*************************************************************
* Draw a label at the location x,y (mgo coordinates)
*
* INPUT:
* xstart, ystart: coordinates
* OUTPUT:
* length in mgo coordinates
*****************************************************************/
double JLP_iGDev_X11::label_device(const char *s, int xstart, int ystart, 
                            double angle1, double expand1, int drawit)
{
double d_x, d_y;
int ix, iy, slen, status;
double length, expanded_font;


slen = strlen(s);

/* Font length in device coordinates */
expanded_font = FONT_SIZE * expand1;

/* Size of characters: (for "mongo.h" ) */
Jgc0.cwidth = 0.6 * expanded_font / Jgc0.g_dx;
Jgc0.cheight = 0.5 * expanded_font / Jgc0.g_dy;

/* Compute length (in SCREEN coordinates) and return if not display: */
length = Jgc0.cwidth * (double)(slen);

/* Draw the string if drawit == 1 */
 if(drawit != 0 || length == 0) { 

// Conversion from mgo (xstart,ystart) to device coordinates (d_x, d_y) :
  conv_mgo_to_dev(xstart, ystart, d_x, d_y);
  ix = NINT(d_x);
  iy = NINT(d_y);

/* Debug:
printf("JLP_iGDev_X11::label_device: s=%s< xstart=%d ystart=%d ix=%d iy=%d slen=%d\n",
        s,xstart,ystart, ix,iy,slen);
*/
/*
    XDrawString(Xgc0.display, Xgc0.win, Xgc0.gc, ix, iy, s, slen);
*/

  status = XDrawString(Xgc0.display, Xgc0.backing, Xgc0.gc, ix, iy, s, slen);
  switch (status) {
    case BadDrawable:
     printf("JLP_iGDev_X11::label_device/XDrawString/error: bad drawable\n");
     break;
    case BadMatch:
     printf("JLP_iGDev_X11::label_device/XDrawString/error: bad match\n");
     break;
    case BadGC:
     printf("JLP_iGDev_X11::label_device/XDrawString/error: bad graphic context\n");
     break;
    default:
     break;
    }
 }

return(length);
}
/*************************************************************
* Erase all the drawings inside the window
*
*************************************************************/
int JLP_iGDev_X11::erase()
{
/* Read the current size of the window: 

   XWindowAttributes window_attributes;

   XGetWindowAttributes(Xgc0.display, Xgc0.win, &window_attributes);
   if((Jgc0.devwidth != window_attributes.width) ||
       (Jgc0.devheight != window_attributes.height)){
         Jgc0.devwidth = window_attributes.width;
         Jgc0.devheight = window_attributes.height;
// Send a message and exit (TO BE CHANGED LATER...)
         printf("Window has been resized! Not handled in current version!\n");
         exit(-1);
       }
*/

/* Erase the exposed window: */
    XFillRectangle(Xgc0.display, Xgc0.win, Xgc0.erasegc, 0, 0, Jgc0.devwidth, 
                   Jgc0.devheight);

/* Erase the backup window: */
    XFillRectangle(Xgc0.display, Xgc0.backing, Xgc0.erasegc, 0, 0, 
                   Jgc0.devwidth, Jgc0.devheight);
    Xgc0.nvec = 0;

return(0);
}

/*********************************************************************
*
*********************************************************************/
/* look for color ressources: */
int JLP_iGDev_X11::color_ressources()
{
char argv[30];
Visual *visual1;
int depth;

strcpy(argv,"JLP_iGDev_X11::color_ressources");

depth = DisplayPlanes(Xgc0.display,Xgc0.screen);  
Xgc0.max_colors = (int) pow(2.,(double)depth);
#ifdef DEBUG
printf(" %s : Maximum number of colors = %d\n",argv,Xgc0.max_colors); 
#endif

visual1 = DefaultVisual(Xgc0.display,Xgc0.screen);
/* Read-only colors cells when color_code = 0*/
/* Xlib.h: class in C:
switch (visual1->class) {
*  c_class in C++ (Xlib.h):
*/
switch (visual1->c_class) {
   case PseudoColor:
#ifdef DEBUG
     printf(" %s : Pseudocolor read/write, limited colors \n",argv);
#endif
     Xgc0.color_code = 1;
     break;
   case StaticColor:
#ifdef DEBUG
     printf(" %s : StaticColor read-only, limited colors \n",argv);
#endif
     Xgc0.color_code = 0;
     break;
   case DirectColor:
#ifdef DEBUG
     printf(" %s : DirectColor read/write, many colors \n",argv);
#endif
     Xgc0.color_code = 1;
     break;
   case TrueColor:
#ifdef DEBUG
     printf(" %s : TrueColor read-only, many colors \n",argv);
#endif
     Xgc0.color_code = 0;
     break;
   case GrayScale:
#ifdef DEBUG
     printf(" %s : GrayScale read/write, monochrome shades \n",argv);
#endif
     Xgc0.color_code = 1;
     break;
   case StaticGray:
#ifdef DEBUG
     printf(" %s : StaticGray read-only, monochrome shades \n",argv);
#endif
     Xgc0.color_code = 0;
     break;
   }
 if(Xgc0.color_code == 0) JLP_iGDev_X11::look_for_visuals();
#ifdef DEBUG
printf(" color_code = %d\n", Xgc0.color_code);
#endif

return(0);
}

/************************************************************************
* JLP99: look for other visuals different from default visual allowing
* for R/W colorcells: 
************************************************************************/
int JLP_iGDev_X11::look_for_visuals()
{
XVisualInfo vTemplate;
XVisualInfo *visualList; 
int visualsMatched;

  vTemplate.depth = 8;
/* in C:
  vTemplate.class = PseudoColor;
* in C++:
*/
  vTemplate.c_class = PseudoColor;
  visualList = XGetVisualInfo(Xgc0.display, VisualClassMask, 
                              &vTemplate, &visualsMatched);
#ifdef DEBUG
  printf("Search result:  %d visuals with PseudoColors \n",visualsMatched);
#endif
  XFree(visualList);

  vTemplate.depth = 8;
/* In C:
  vTemplate.class = DirectColor;
* in C++:
*/
  vTemplate.c_class = DirectColor;
/*
  visualList = XGetVisualInfo(Xgc0.display, VisualClassMask && VisualDepthMask, 
*/
  visualList = XGetVisualInfo(Xgc0.display, VisualClassMask, 
                              &vTemplate, &visualsMatched);
  XFree(visualList);
#ifdef DEBUG
  printf("Search result:  %d visuals with DirectColors \n",visualsMatched);
#endif
return(0);
}
/****************************************************************
 * Set colour table with standard color tables
 * Doesn't work on the Sun, nor on IBM...
 * Input: r,g,b arrays between 0 and MaxColorLevelForLUT 
 *        ncolors: number of colors wanted 
 * Output: mylut_x11, Look-up-table
 ****************************************************************/
int JLP_iGDev_X11::load_standard_lut(int *r, int *g, int *b, int *mylut_x11,
                                   int ncolors)
{
   Visual *visual1;
   XStandardColormap mycmap;
   XSetWindowAttributes myattr;          /* Attributes for the window */
   unsigned long whitepixel;             /* computed value for white */ 
   unsigned long mymask;                 /* attr. mask for the window */

   register int i;
   int rstep,gstep,bstep;

/* Requires PseudoColor as ressource of the system:
*/
   visual1 = DefaultVisual(Xgc0.display,Xgc0.screen);

/* In C:
   if (visual1->class != PseudoColor) 
* in C++:
*/
   if (visual1->c_class != PseudoColor) 
   {printf(" JLP_iGDev_X11::load_standard_lut/no pseudo-color: i.e. no read/write color cells\n");
    JLP_iGDev_X11::color_ressources(); 
    return(-1);
    }

/* If OK load a standard color map: */
   if (XGetStandardColormap(Xgc0.display, RootWindow(Xgc0.display,Xgc0.screen),
	&mycmap,XA_RGB_BEST_MAP) == 0)
      {
      printf(" JLP_iGDev_X11::load_standard_lut/Error: Couldn't get RGB_BEST color map\n");
      if (XGetStandardColormap(Xgc0.display, RootWindow(Xgc0.display,Xgc0.screen),
	&mycmap,XA_RGB_DEFAULT_MAP) == 0) {
         printf(" JLP_iGDev_X11::load_standard_lut/Error: Couldn't get RGB_DEFAULT color map\n");
         return(-2);
         }
      }

/* When OK load this map as new window attributes: */
   mymask = CWColormap;
   myattr.colormap = mycmap.colormap;
   whitepixel = mycmap.base_pixel +
		   (mycmap.red_max + mycmap.red_mult)+
		   (mycmap.green_max + mycmap.green_mult)+
		   (mycmap.blue_max + mycmap.blue_mult);
   mymask |= CWBackPixel;
   myattr.background_pixel = whitepixel;
   XChangeWindowAttributes(Xgc0.display,Xgc0.win,mymask,&myattr);

/* Then build up the look up table: */
   rstep = mycmap.red_max*mycmap.red_mult/MaxColorLevelForLUT;
   gstep = mycmap.green_max*mycmap.green_mult/MaxColorLevelForLUT;
   bstep = mycmap.blue_max*mycmap.blue_mult/MaxColorLevelForLUT;

   for (i=0; i<ncolors; ++i)
   {
    mylut_x11[i] = mycmap.base_pixel +
	 (r[i] * rstep) + 
	 (g[i] * gstep) + 
	 (b[i] * bstep);
   }
return(0);
}

/********************************************************* 
*  To allocate a look-up table: 
* 
* Output:
*  mylut_x11 : addresses of the allocated cells
*  ncolors: number of colors successfully allocated
*  private_lut: set to one if private LUT is allowed (i.e., LUT can be changed)
**********************************************************/
int JLP_iGDev_X11::alloc_lut_device(int *mylut_x11, int& private_lut, int& ncolors)
{
register int i;
unsigned long lut_pixels[256];

  private_lut = 1;

/* Set values for max_colors and color_code: */
 JLP_iGDev_X11::color_ressources(); 

/* Check maximum number of colors: */
  if(ncolors > Xgc0.max_colors) 
     {printf(" JLP_iGDev_X11::alloc_lut/Error: Only %d colors are allowed!\n",Xgc0.max_colors);
      ncolors = Xgc0.max_colors;
     }

/* Read-only colors cells when color_code = 0 */
if( Xgc0.color_code == 0)
  { 
#ifdef DEBUG
  printf(" JLP_iGDev_X11::alloc_lut/Warning: read/write color cells cannot be allocated with that display\n"); 
#endif
  private_lut = 0;
#if 1
  return(2);
#else
{    // BOF Block
int value;
Visual *visual1;
XColor myxcol[256];

  XFreeColormap(Xgc0.display,Xgc0.cmap);
  visual1 = DefaultVisual(Xgc0.display,Xgc0.screen);

/* No allocation on creation, do this later: */
  Xgc0.cmap = XCreateColormap(Xgc0.display, Xgc0.win, visual1, AllocNone);
  if(!Xgc0.cmap) 
    {
    printf("Error creating color map \n");
    return(-1);
    }
  XSetWindowColormap(Xgc0.display, Xgc0.win, Xgc0.cmap);
   for(i=0; i < ncolors; i++)
     {
     value = (i * MaxColorLevelForLUT)/ ncolors;
     myxcol[i].red = value;
     myxcol[i].green = value;
     myxcol[i].blue = value;
     myxcol[i].flags = DoRed | DoGreen | DoBlue;
     if(!XAllocColor(Xgc0.display,Xgc0.cmap,&myxcol[i]))
      printf("JLP_iGDev_X11::alloc_lut/error allocating color #%d \n",i);
     else
      mylut_x11[i] = myxcol[i].pixel;
     }
  for(i = 0; i < ncolors; i++) printf(" %d ",mylut_x11[i]); 
}  // EOF Block
#endif
  }
else
  {
  if(XAllocColorCells(Xgc0.display,Xgc0.cmap,FALSE,NULL,0,lut_pixels,ncolors) == 0)
/* jlp92 */
    {
     printf(" JLP_iGDev_X11::alloc_lut/error: can't allocate %d read/write color cells!\n",
             (int)ncolors); 
     if(ncolors > Xgc0.max_colors) ncolors = Xgc0.max_colors;
        else ncolors = 4;
     if(XAllocColorCells(Xgc0.display,Xgc0.cmap,FALSE,NULL,0,lut_pixels,ncolors) == 0)
       {
       printf(" Second attempt: could NOT allocate %d rw color cells!\n",
              (int)ncolors); 
       return(-1);
       }
    }

  for(i = 0; i < ncolors; i++) mylut_x11[i] = lut_pixels[i];
  private_lut = 1;

#ifdef DEBUG
  printf(" JLP_iGDev_X11::alloc_lut/ %d read/write color cells successfully allocated\n",
          (int)ncolors); 
#endif
  }

return(0);
}

/*******************************************************************
 * Set color table with private cells: 
 * Loading (r,g,b)(i) colors to read/write color cells previously allocated, 
 * and whose address is stored in mylut_x11[i]: 
 * If not private lut i.e., read-only LUT, look for cells with nearest values
 * In this case, mylut values will be modified
 *
 * Input: r,g,b arrays between 0 and MaxColorLevelForLUT
 *        ncolors: number of colors wanted 
 * Input/Output:
 *        mylut_x11: color cell addresses
 *********************************************************************/
int JLP_iGDev_X11::load_lut_device(const int *r, const int *g, const int *b, 
                             int *mylut_x11, int ncolors)
{
register int i;
int value;
int fixed_lut4[4];
XColor myxcol[256];

/* Array with fixed LUT for the case when ncolors=4 (grey scale) */
fixed_lut4[0]=0;
fixed_lut4[1]=50;
fixed_lut4[2]=200;
fixed_lut4[3]=255;

/* Load XColor myxcol structure with addresses and values: */
   for(i=0; i < ncolors; i++)
     {
     myxcol[i].pixel = mylut_x11[i];
     myxcol[i].red = r[i];
     myxcol[i].green = g[i];
     myxcol[i].blue = b[i];
     myxcol[i].flags = DoRed | DoGreen | DoBlue;
     }

/* If rw private cells, stores rgb values in the preallocated color cells: */
  if(Xgc0.color_code == 1)
   {
   if(XStoreColors(Xgc0.display,Xgc0.cmap,myxcol,ncolors) == 0) 
     {
     printf(" JLP_iGDev_X11::load_lut/XStoreColors error when storing colors in rw cells\n"
);
     printf(" Maybe rw color cells have not been allocated...\n");
     exit(-1);
     }
   }
/* Read-only color cells, so I affect all of them
 (since they correspond to very few gray levels) */
  else if(ncolors < 16)
   {
/* Get index values of the read/only cell for mylut_x11
   corresponding to the closest (r,g,b): */
   for(i=0; i < ncolors; i++)
     {
     if(ncolors == 4)
       value = (fixed_lut4[i]*MaxColorLevelForLUT)/255;
     else
       value = (i * MaxColorLevelForLUT)/255;
     myxcol[i].red = value;
     myxcol[i].green = value;
     myxcol[i].blue = value;
     myxcol[i].flags = DoRed | DoGreen | DoBlue;
     if(!XAllocColor(Xgc0.display,Xgc0.cmap,&myxcol[i]))
      printf("JLP_iGDev_X11::load_lut/error allocating color #%d \n",i);
     else
      mylut_x11[i] = myxcol[i].pixel;
     }
   }
/* LINUX case, "TrueColors" read-only color cells */
  else
   {
/* Get index values of the read/only cell for mylut_x11
   corresponding to the closest (r,g,b): */
   for(i=0; i < ncolors; i++)
     {
     myxcol[i].pixel = i;
     myxcol[i].red = r[i];
     myxcol[i].green = g[i];
     myxcol[i].blue = b[i];
     myxcol[i].flags = DoRed | DoGreen | DoBlue;
     if(!XAllocColor(Xgc0.display,Xgc0.cmap,&myxcol[i]))
      printf("JLP_iGDev_X11::load_lut/error allocating color #%d \n",i);
     else
      mylut_x11[i] = myxcol[i].pixel;
     }
/* Just a message to jlp_change_lut() (in jlp_lut.c) */
    return(-2);
   }

   return(0);
}
/*************************************************************
* Set line colour (for further drawings...)
* r,g,b between 0. and 1.0 
* WARNING: Black background, hence should draw in white
* or with bright colors!
*
* Change the foreground value of the graphic context: Xgc0.gc
***************************************************************/
int JLP_iGDev_X11::setrgbcolor(double r, double g, double b)
{
int status = 0;
unsigned long mycolor, rr, gg, bb;

/* 
* r,g,b between 0. and 1.0 
* rr,gg,bb between 0 and  255 
*/
rr = (int)(255. * r);
gg = (int)(255. * g);
bb = (int)(255. * b);

/* First empties buffer with old setup: */
   JLP_iGDev_X11::empty_xvec();

#ifdef DEBUG
   printf("JLP_iGDev_X11::setrgbcolor/line color set to r, g, b: %d %d %d \n",
           (int)rr,(int)gg,(int)bb);
#endif

   status = JLP_Xcolor_pixel(Xgc0.display, Xgc0.cmap, mycolor, rr, gg, bb);

   if(!status) XSetForeground(Xgc0.display, Xgc0.gc, mycolor);

return(status);
}
/***************************************************************
*
***************************************************************/
int JLP_iGDev_X11::wait_for_events()
{
  XEvent event;
  XMappingEvent mapevent;
  int select;

  while(1)
    {
    XNextEvent(Xgc0.display, &event);
    if(event.xbutton.window == Xgc0.win) {
    switch(event.type)
       {
/* Change of size: 
       case ConfigureNotify:
         printf(" new width, new height (conf) %d %d\n",
		  event.xconfigure.width,
		  event.xconfigure.height);
         Jgc0.devwidth = event.xconfigure.width;
         Jgc0.devheight = event.xconfigure.height;
// Send a message and exit (TO BE CHANGED LATER...)
         printf("Window has been resized! Not handled in current version!\n");
         exit(-1);
*/
/* Case expose */
       case Expose:
         JLP_iGDev_X11::redraw();
         break;
/* Change of keyboard: */
       case MappingNotify:
         XRefreshKeyboardMapping(&mapevent);
	 break;
/* Exit from waiting loop only when pressed on the mouse or on the keyboard: */
/* JLP96: I add KeyPress: */
       case KeyPress:
/* (Before August 1996, only ButtonPress): */
       case ButtonPress:
#ifdef DEBUG
         printf("wait_for_events/OK: button pressed \n");
#endif
         return(0);
         break;
/* Unexpected event, or event to be thrown away: */
       default:
         break;
       }   /* end switch */
   }
/**********************************************************************
* Update StatusBar if needed: 
**********************************************************************/
   else if((event.xbutton.window == Xgc0.StatusBar.win)
             && (event.type == Expose)) {
      DrawStaticButton(Xgc0.StatusBar);
   }
/**********************************************************************
* Possibility of exit if menu button was pressed: 
**********************************************************************/
   else {
     if(button_was_pressed(event, select)) return(0);
    }
 }    /* end while */
return(-1);
}
/***************************************************************
* get_winlimits
* Draw rectangles or lines using cursorgc graphic context (xor function) 
*
* INPUT:
* type_of_win:  1=line; >1 =rectangle
* (to allow for Slice/Zoom/Statistics
*
* OUTPUT:
* u_x1, u_x2, u_y1, u_y2: user coordinates
* pressed_button:   1 if button#1,  2 if button#2,  3 if button#3
*
* JLP96
***************************************************************/
int JLP_iGDev_X11::get_winlimits(double &u_x1, double &u_y1, double &u_x2, double &u_y2, 
                           int type_of_win, int &pressed_button, int& in_frame)
{
int x2_old, y2_old, x1, y1, x2, y2, first_point_ok, x_ul, y_ul;
int waiting_for_second_point, status, select;
Cursor circ_cursor, resize_cursor;
XEvent event, report;

 circ_cursor = XCreateFontCursor(Xgc0.display, XC_circle);
 resize_cursor = XCreateFontCursor(Xgc0.display, XC_sb_h_double_arrow);
 XDefineCursor(Xgc0.display, Xgc0.win, circ_cursor);

// Initialize output values:
  in_frame = 1;
  u_x1 = u_y1 = u_x2 = u_y2 = -1;

/********************* Main loop on all events *********************/
  first_point_ok = 0;
  waiting_for_second_point = 1;
// Exit from "while loop" if point out of frame:
// (e.g. when first point is out of frame)
  while(in_frame && waiting_for_second_point)
    {
    XNextEvent(Xgc0.display, &event);
// Check if event is on the main window:
    if(event.xbutton.window == Xgc0.win) {
    switch(event.type)
       {
       case Expose:
           JLP_iGDev_X11::redraw();
         break;
/* Enter starting point of window: */
      case ButtonPress:
// Starting point:
         x1 = event.xkey.x;
         y1 = event.xkey.y;
// Conversion from dev (x1,y1) to user coordinates (u_x1,u_y1):
         conv_dev_to_user(x1, y1, u_x1, u_y1, in_frame);
// Initializing current ending point:
         x2 = x1; y2 = y1;
// Initializing previous ending point:
         x2_old = x2;
         y2_old = y2;
#ifdef DEBUG
         printf("OK ButtonPress x1,y1: %d %d (in_frame=%d)\n",x1,y1,in_frame);
#endif
              switch (event.xbutton.button)
                {
                case 2:
                   pressed_button = 2;
                   break;
                case 3:
                   pressed_button = 3;
                   break;
                default:
                   pressed_button = 1;
                   break;
                }
         if(in_frame) {
           first_point_ok = 1;
           XDefineCursor(Xgc0.display, Xgc0.win, resize_cursor);
           }
         break;
/* Exit when releasing the mouse: */
      case ButtonRelease:
         if(first_point_ok && (x2 != x1 || y2 != y1)) 
              {
// Conversion from dev (x2,y2) to user coordinates (u_x2,u_y2):
               conv_dev_to_user(x2, y2, u_x2, u_y2, in_frame);
#ifdef DEBUG
         printf("OK ButtonRelease x2,y2: %d %d (in_frame=%d)\n",x2,y2,in_frame);
#endif
               waiting_for_second_point = 0;
              }
         break;
      case MotionNotify:

/* can get rid of all MotionNotify events in queue,
* since otherwise the round-trip  delays caused by
* XQueryPointer may cause a backlog
* of MotionNotify events, which will cause additional
* wasted XQueryPointer calls. */
          while (XCheckTypedEvent(Xgc0.display, MotionNotify, &report));

/* Neutralize following part as long as the mouse has not been clicked in this
routine: */
           if(!first_point_ok) break;
/* pointer motion while mouse button is down */
           x2 = event.xkey.x;
           y2 = event.xkey.y;
/****** Draw a line: **************/
           if (type_of_win == 1) 
             {
/* Erase previous line if needed: */
             if(x2 != x2_old || y2 != y2_old)
                 XDrawLine(Xgc0.display,Xgc0.win,Xgc0.cursorgc,x1,y1,x2_old,y2_old);
/* Draw new line: */
             XDrawLine(Xgc0.display,Xgc0.win,Xgc0.cursorgc,x1,y1,x2,y2);
             }
/****** Draw a rectangle: ********/
         else 
             {
/* Erase previous rectangle if needed: */
             if(x2 != x2_old || y2 != y2_old) {
                 x_ul = MINI(x1, x2_old); y_ul = MINI(y1, y2_old);
                 XDrawRectangle(Xgc0.display,Xgc0.win,Xgc0.cursorgc,x_ul,y_ul,
                                ABS(x2_old-x1),ABS(y2_old-y1));
                 }
/* Draw new rectangle: */
             x_ul = MINI(x1, x2); y_ul = MINI(y1, y2);
             XDrawRectangle(Xgc0.display,Xgc0.win,Xgc0.cursorgc,x_ul,y_ul,
                            ABS(x2-x1), ABS(y2-y1));
             }
           x2_old = x2;
           y2_old = y2;
         break;
/* Unexpected event, or event to be thrown away: */
       default:
         break;
       }   /* end switch */
    }        // end of event on the main window
/**********************************************************************
* Update StatusBar if needed:
**********************************************************************/
    else if((event.xbutton.window == Xgc0.StatusBar.win)
             && (event.type == Expose)) {
      DrawStaticButton(Xgc0.StatusBar);
    }
/**********************************************************************
* Possibility of exit (with status = -1) if menu button was pressed: 
**********************************************************************/
    else {
     if(button_was_pressed(event, select)) {
      in_frame = 0;
      status = -1; 
      }
    }
  }    /* end while */

/* Reset cursor to default value: */
   XDefineCursor(Xgc0.display, Xgc0.win, Xgc0.default_cursor);

if(waiting_for_second_point) status = -1;
 else status = 0;

return(status);
}
/***************************************************************
* get_circles
* Draw the circles using cursorgc graphic context (xor function) 
* 
* To get one or two concentric circles for circular photometry 
* or patch applications
*
* INPUT:
* ncirc: number of circles that are needed (1 or 2)
*
* OUTPUT:
*
* x_cent, y_cent: (user) coordinates of the center
* diam1, diam2: (user) diameters of the small and big circles
*
***************************************************************/
int JLP_iGDev_X11::get_circles(double& x_cent, double& y_cent, double& diam1, 
                          double& diam2, int& ncirc)
{
int ix_cent, iy_cent, diam0, diam0_old, ix, iy;
int in_frame, npoints, wx, wy, status;
double xx, yy;
Cursor circ_cursor, resize_cursor;
XEvent event, report;

 circ_cursor = XCreateFontCursor(Xgc0.display, XC_circle);
 resize_cursor = XCreateFontCursor(Xgc0.display, XC_sb_h_double_arrow);
 XDefineCursor(Xgc0.display, Xgc0.win, circ_cursor);

/*
* Warning: Problem with Arcs when erasing arcs written with "zero" thickness
*/
/* Works better with thickness of one, but still not very well: 
*/
 SetLineWidthAndType_to_gc(Xgc0.cursorgc, 1, 0);

diam0_old = -1;
npoints = 0;
in_frame = 1;
while(in_frame && (npoints <= ncirc))
    {
    XNextEvent(Xgc0.display, &event);
// Check if event is on the main window:
    if(event.xbutton.window == Xgc0.win) {
    switch(event.type)
       {
       case Expose:
           JLP_iGDev_X11::redraw();
         break;
/* Enter center coordinates: */
      case ButtonPress:
         if(npoints == 0)
              {
              ix_cent = event.xkey.x;
              iy_cent = event.xkey.y;
/* Conversion from device (ix_cent,iy_cent) 
*  to user coordinates (x_cent,y_cent): */
              conv_dev_to_user(ix_cent, iy_cent, x_cent, y_cent, in_frame);
/* If in_frame, proceed, otherwise exit from while loop */
              if(in_frame) {
#ifdef DEBUG
                printf("OK x_cent=%f y_cent=%f in_frame=%d\n",
                        x_cent,y_cent,in_frame);
#endif
                XDefineCursor(Xgc0.display, Xgc0.win, resize_cursor);
                npoints++;
                }
              }
         break;
      case ButtonRelease:
          switch(npoints) 
            { 
/* Releasing the mouse for the 1st time: */
            case 1:
/* To convert diameters to user coordinates,
* the trick is to compute differences of converted coordinates */ 
              conv_dev_to_user(ix_cent + diam0 / 2, iy_cent,
                               xx, yy, in_frame);
              diam1 = 2. * (double)(xx - x_cent);
/* If too big, try the other way: */
              if(!in_frame) {
                  conv_dev_to_user(ix_cent - diam0 / 2, iy_cent, xx, yy, 
                                   in_frame);
                  diam1 = 2. * (double)(x_cent - xx);
                  }
/* If still too big, give up: */
              if(!in_frame) {
                  printf("ix,iy,diam0:%d %d %d \n", (int)xx,(int)yy,(int)diam0);
                  printf("Error, first circle is too big \n");
              } else {
#ifdef DEBUG
                  printf("OK diam1=%f\n",diam1);
#endif
                  npoints++;
/* To prevent from erasing this circle: */
                  diam0_old = -1;
               }
              break;
/* Releasing the mouse for the 2nd time: */
            case 2:
              conv_dev_to_user(ix_cent + diam0 / 2, iy_cent, xx, yy, in_frame);
              diam2 = 2. * (double)(xx - x_cent);
/* If too big, try the other way: */
              if(!in_frame) {
                 conv_dev_to_user(ix_cent - diam0 / 2, iy_cent, xx, yy, 
                                  in_frame);
                 diam2 = 2. * (double)(x_cent - xx);
                 }

              if(!in_frame) {
                 printf("Error, second circle is too big \n");
               } else {
#ifdef DEBUG
                 printf("OK diam2=%f\n",diam2);
#endif
                 npoints++;
                 }
              break;
            default:
               break;
            }
         break;
      case MotionNotify:
/* can get rid of all MotionNotify events in queue,
* since otherwise the round-trip  delays caused by
* XQueryPointer may cause a backlog
* of MotionNotify events, which will cause additional
* wasted XQueryPointer calls. */
          while (XCheckTypedEvent(Xgc0.display, MotionNotify, &report));

/* Neutralize following part as long as the mouse has not been clicked in this
routine: */
           if(npoints) {
/* pointer motion while mouse button is down */
           ix = event.xkey.x;
           iy = event.xkey.y;
           wx = ix - ix_cent;
           wy = iy - iy_cent;
           diam0 = (int)(2. * sqrt((double)(wx * wx + wy * wy)));
/* Won't write a zero-radius circle: */
           if(diam0 > 0 ) { 
/* Erase previous circle if needed: */
             if(diam0 != diam0_old && diam0_old != -1)
                 XDrawArc(Xgc0.display,Xgc0.win,Xgc0.cursorgc,
                           (int)(ix_cent - diam0_old/2),
                           (int)(iy_cent - diam0_old/2),
                           diam0_old,diam0_old,0,(int)(360*64));
/* Draw a new circle: */
             XDrawArc(Xgc0.display,Xgc0.win,Xgc0.cursorgc,
                           (int)(ix_cent - diam0/2),
                           (int)(iy_cent - diam0/2),
                           diam0,diam0,0,(int)(360*64));
             diam0_old = diam0;
             }   // EOF diam > 0
           }   // EOF npoints != 0
         break;
/* Unexpected event, or event to be thrown away: */
       default:
         break;
       }   /* end switch */
      }        // end of event on the main window
/**********************************************************************
* Possibility of exit (with status = -1) if menu button was pressed: 
**********************************************************************/
     }    /* end while */

/* Return if only one circle is needed: */
  if(npoints == ncirc + 1) {
   if(ncirc == 1) diam2 = diam1;
   status = 0;
  } else {
   diam1 = 0.; diam2 = 0.;
   status = -1;
  }
#ifdef DEBUG
 printf("Return with npoints=%d diam1=%f diam2=%f \n", npoints, diam1, diam2);
#endif

/* Reset cursor to default value: */
 XDefineCursor(Xgc0.display, Xgc0.win, Xgc0.default_cursor);

/* Reset fast parameters for cursor graphic context: */
 SetLineWidthAndType_to_gc(Xgc0.cursorgc, 0, 0);

return(status);
}
/***************************************************************
* Update display on the screen
*
***************************************************************/
int JLP_iGDev_X11::gflush()
{

#ifdef DEBUG
  printf("JLP_iGDev_X11::gflush: OK\n");
#endif

/* Empty vector buffer: */
    if(Xgc0.nvec) JLP_iGDev_X11::empty_xvec();

/* Send all graphic to screen : */
    JLP_iGDev_X11::redraw();

/* Synchronize the display: (Nov 2006: otherwise not immediatly visible)*/
   XFlush(Xgc0.display);

return(0);
}
/*********************************************************************
* To draw a polygon
* Do not compute the relative position vector if already called
* with same shape
*
* INPUT:
*   xc, yc: position of center in mgo coordinates
*   nsides: number of sides
*   expand: size of symbol
*   angle: starting angle
*   filled: flag set to one if polygon has to be filled 
*********************************************************************/
int JLP_iGDev_X11::polygon(int xc, int yc, double expand, double angle, int nsides, 
                     int filled)
{
   double dtheta, theta, d_xc, d_yc;
   static double old_ang;	/* old values of angle */
   int i; 
   double xpsize, ypsize;        /* scale for points == g_dx*pdef*expand */
// Declared as static to avoid computing the polygon each time
   static int num = -1;         /* number of vertices used last time */
   static double old_xpsize,old_ypsize;	/* old values of xpsize, ypsize */
   static XPoint vlist[MAX_POLYGON + 1];  /* vertices describing point */
   static int ix0, iy0; 	/* constant part of vertex[0].x, .y */

   if(nsides < 2) {
      JLP_iGDev_X11::line(xc, yc, xc, yc+1);
      return(0);
   }

   dtheta = 2*PI/nsides;
/* Compute length (in device coord.) of the polygon sides: */
   xpsize = 2. * (double)Mgc0.pdef * sin(dtheta/2) * expand * Jgc0.g_dx;
   ypsize = xpsize * Jgc0.g_dy / Jgc0.g_dx;

   if(nsides != num || angle != old_ang || xpsize != old_xpsize || ypsize != old_ypsize) {
      if(nsides > MAX_POLYGON) num = MAX_POLYGON;
      else num = nsides;

      theta = 3. * PI / 2. + dtheta/2 + angle*PI/180;

      old_ang = angle;
      old_xpsize = xpsize;
      old_ypsize = ypsize;

/* Translation (relative to the center) of the first position 
* of the polygon (in device coord.) */
      ix0 = NINT(xpsize * cos(theta) / 2.);
      iy0 = - NINT(ypsize * sin(theta) / 2.);

/* Now relative positions: (in device coord.) */
      theta += PI/2. + dtheta/2;
      for(i = 1; i <= num; i++) {
	 vlist[i].x = NINT(xpsize*cos(theta));
	 vlist[i].y = NINT(-ypsize*sin(theta));    /* screen is upside down */
	 theta += dtheta;
      }
   }
// This statement should be outside of the loop since 
// since the position of the absolute location should be changed each time!
      conv_mgo_to_dev(xc, yc, d_xc, d_yc);
      vlist[0].x = ix0 + NINT(d_xc);
      vlist[0].y = iy0 + NINT(d_yc);
if(filled)
   XFillPolygon(Xgc0.display, Xgc0.backing, Xgc0.gc, vlist, num+1,
		Convex, CoordModePrevious);
else
   XDrawLines(Xgc0.display, Xgc0.backing, Xgc0.gc, vlist, num+1,
	      CoordModePrevious);

return(0);
}

/***************************************************************** 
* INPUT
*   cursor_type: 
*              cross
*              "hand" for the menu
*              pencil	
*              big_crosshair 
*
* OUTPUT:
*   x, y: mgo coordinates
*   pressed_button: 0 if not recognized, 1 to 3 otherwise
********************************************************************/
int JLP_iGDev_X11::cursor(int& x, int& y, char *cursor_type, int& pressed_button)
{
  Cursor newcursor;		/* graphics cursor for this mode*/
  XEvent event, report;
  XColor xcolor;
  char buffer[40], null_data[1];
  int ix, iy, in_frame, big_crosshair, ix_old, iy_old, npoints;
  int status = 0, select;
  double x_user, y_user;
  Pixmap jlp_no_cursor;

  big_crosshair = 0;
  JLP_Xcolor(Xgc0.display, Xgc0.cmap, xcolor, 255, 255, 255);

/* define new cursor and makes it sensitive to mouse: */
   switch (cursor_type[0])
   {
/* Direct Input: XC_... (see list in ...../X11/include/cursorfont.h) */ 
   case 'X':
      newcursor = XCreateFontCursor(Xgc0.display,x);
      break;
/* Hand-made "big cross hair"  */
   case 'B':
   case 'b':
/* Suppress the cursor first */
      null_data[0] = '\0';
      jlp_no_cursor = XCreatePixmapFromBitmapData(Xgc0.display,Xgc0.win,
                                                   null_data,1,1,0,0,1);
      newcursor = XCreatePixmapCursor(Xgc0.display,jlp_no_cursor,jlp_no_cursor,
                          &xcolor,&xcolor,0,0);
/* Then set big_crosshair to one so that I generate later a giant cursor */
      big_crosshair = 1;
      break;
/* "hand" for the menu */
   case 'M':
   case 'm':
      newcursor = XCreateFontCursor(Xgc0.display, XC_hand2);
      break;
/* pencil */
   case 'P':
   case 'p':
      newcursor = XCreateFontCursor(Xgc0.display, XC_pencil);
      break;
/* cross or circle */
   case 'C':
   case 'c':
      if(cursor_type[1] == 'i' || cursor_type[1] == 'I')
        newcursor = XCreateFontCursor(Xgc0.display, XC_circle);
      else
        newcursor = XCreateFontCursor(Xgc0.display, XC_crosshair);
      break;
/* default is cross */
   default: 
      newcursor = XCreateFontCursor(Xgc0.display, XC_crosshair);
      break;
    }

   XDefineCursor(Xgc0.display, Xgc0.win, newcursor);

/* JLP96: select faster parameters (VERY EFFICIENT!): */
   if(big_crosshair)
       {

       ix_old = 0;
       iy_old = 0;
       }

/* sense cursor button */
  x = y = 0;
  npoints = 0;
  while(!npoints) {
     XNextEvent(Xgc0.display,&event);		
// Check if event is on the main window:
    if(event.xbutton.window == Xgc0.win) {
     switch (event.type) {
      case Expose: 
         JLP_iGDev_X11::redraw();
         break;
      case ButtonPress:
         pressed_button = event.xbutton.button;
        if( pressed_button < 1 && pressed_button > 3) pressed_button = 0; 
        conv_dev_to_mgo(event.xkey.x, event.xkey.y, x, y);
// Will exit from loop since npoints != 0: 
        npoints++;
#ifdef DEBUG
      printf("JLP_iGDev_X11::cursor/Pressed button: x=%d y=%d (mgo)\n", x, y);
#endif
        break;
      case MotionNotify: 
/* can get rid of all MotionNotify events in queue,
* since otherwise the round-trip  delays caused by
* XQueryPointer may cause a backlog
* of MotionNotify events, which will cause additional
* wasted XQueryPointer calls. */
          while (XCheckTypedEvent(Xgc0.display, MotionNotify, &report));

          ix = event.xkey.x;
          iy = event.xkey.y;
          conv_dev_to_user(ix, iy, x_user, y_user, in_frame);

// Format for images: 
          sprintf(buffer,"%.1f %.1f",x_user,y_user);

          DrawToCursorPosition(buffer);

/* Horizontal and vertical lines to form a big cross: */
          if(big_crosshair)
            {
/* Erase previous cross if needed and draw new cross: */
              if(iy != iy_old)
                   {
                   if(iy_old)
                        XDrawLine(Xgc0.display,Xgc0.win,Xgc0.cursorgc,0,iy_old,
                                  Jgc0.devwidth,iy_old);
                   XDrawLine(Xgc0.display,Xgc0.win,Xgc0.cursorgc,0,iy,Jgc0.devwidth,iy);
                   }
              if(ix != ix_old)
                   {
                   if(ix_old)
                       XDrawLine(Xgc0.display,Xgc0.win,Xgc0.cursorgc,ix_old,0,ix_old,
                                 Jgc0.devheight);
                   XDrawLine(Xgc0.display,Xgc0.win,Xgc0.cursorgc,ix,0,ix,Jgc0.devheight);
                   }
/* Update ix_old and iy_old: */
              ix_old = ix;
              iy_old = iy;
           }
          break;
        default:
          break;
       } // end of switch
     } // end of event on the main window
/**********************************************************************
* Update StatusBar if needed:
**********************************************************************/
    else if((event.xbutton.window == Xgc0.StatusBar.win)
             && (event.type == Expose)) {
      DrawStaticButton(Xgc0.StatusBar);
    }
/**********************************************************************
* Possibility of exit (with status = -1) if menu button was pressed: 
**********************************************************************/
    else {
     if(button_was_pressed(event, select)) {
      ix = iy = -1;
      status = -1; 
      npoints = 1;
      }
    }
   } // end of while

/* Erase big crosshair if needed: */
if(big_crosshair) {
     XDrawLine(Xgc0.display,Xgc0.win,Xgc0.cursorgc,0,iy_old,Jgc0.devwidth,iy_old);
     XDrawLine(Xgc0.display,Xgc0.win,Xgc0.cursorgc,ix_old,0,ix_old,Jgc0.devheight);
   }

/* Reset cursor to default value: */
   XDefineCursor(Xgc0.display, Xgc0.win, Xgc0.default_cursor);

   EraseCursorPosition();
return(status);
}
/*********************************************************************
* Close X11 device
*
*********************************************************************/
int JLP_iGDev_X11::close_device()
{
#ifdef DEBUG
printf("JLPX11::close_device OK?\n");
#endif
   if(Xgc0.xvec != NULL) free(Xgc0.xvec);
   XFreeGC(Xgc0.display,Xgc0.gc);
   XFreeGC(Xgc0.display,Xgc0.erasegc);
   XFreeGC(Xgc0.display,Xgc0.cursorgc);
   XFreeGC(Xgc0.display,Xgc0.Button_graygc);
   XFreeGC(Xgc0.display,Xgc0.Button_whitegc);
   XFreeFont(Xgc0.display,Xgc0.fonts);
   XCloseDisplay(Xgc0.display);
   return(0);
}

/*********************************************************************
*
*********************************************************************/
int JLP_iGDev_X11::redraw()
{

/* Read the current size of the window and resize it if necessary:
   NO LONGER USED!

   XWindowAttributes window_attributes;
   int newwidth, newheight;
   XGetWindowAttributes(Xgc0.display, Xgc0.win, &window_attributes);
   newwidth = window_attributes.width;
   newheight = window_attributes.height;
   printf(" width, height: %d %d\n",Jgc0.devwidth,Jgc0.devheight);
   if(newwidth > 200 && newwidth < 900) Jgc0.devwidth = newwidth;
   if(newheight > 200 && newheight < 900) Jgc0.devheight = newheight;
   XResizeWindow(Xgc0.display,Xgc0.win,Jgc0.devwidth,Jgc0.devheight);

*/

/* Copies backup window to exposed window: */
    XCopyArea(Xgc0.display, Xgc0.backing, Xgc0.win, Xgc0.gc, 0, 0, 
              Jgc0.devwidth, Jgc0.devheight, 0, 0);

/* Update Status Bar: */
   DrawStaticButton(Xgc0.StatusBar);

return(0);
}
/*****************************************************************************
*
* FUNCTION: plot_image
*
* PURPOSE: Draw an image in a window.
*
* INPUT: 
*         image2[0..nx2*ny2-1] = image with LUT cell adresses
*         nx2 = number of lines of the image in pixels
*         ny2 = number of columns of the image in pixels
*         xstart, ystart: position of the lower left corner in mgo coord.
*
* From X11_i_image (Eric ANTERRIEU) Version 29-11-90
*
* INPUT:
* gamma_d;             Reduction/magnification factor 
* black_and_white : flag set to 1 if B&W is wanted (not yet implemented here)
* xstart, ystart: location of the bottom-left corner in mgo coordinates
******************************************************************************/
int JLP_iGDev_X11::plot_image(int *image2, int nx2, int ny2, int idim,
                        int xstart, int ystart, int gamma_d,
                        int black_and_white)
/*****************************************************************************/
{
register  int i, j, ii, jj, k;
XImage    *xi;
int       x_ul, y_ul, status, nx, ny;
double d_x, d_y;

/* JLP2000: 24 bits/pixel: */
#if(BIT_PER_VIDEO_PIXEL == 24) 
union{
     int i;
     char c[4];
     } mdata;
/* JLP98: 16 bits/pixel: */
#elif(BIT_PER_VIDEO_PIXEL == 16) 
union{
     INT2 i;
     char c[2];
     } mdata;
/* JLP2004 (Algol/Merate) 8 bits/pixel: */
#else
union{
     INT1 i;
     char c[1];
     } mdata;
#endif
Visual *visual1;
visual1 = DefaultVisual(Xgc0.display,Xgc0.screen);

/* Magnification, i.e. correspondance between integer array image and
   graphic memory array, (set to one if original image has been reduced) */

/* New size: */
nx = nx2 * gamma_d;
ny = ny2 * gamma_d;
#ifdef DEBUG
printf("JLP_iGDev_X11::plot_image/nx2=%d ny2=%d gamma_d=%d, nx=%d, ny=%d\n",nx2, ny2, gamma_d, nx, ny);
#endif

/* For some unknown reason X11 has problems when numbers which are not
multiple of 4 */
if(nx < 4 || ny < 4) {
  fprintf(stderr,"JLP_iGDev_X11::plot_image/Error: too few points for display: nx=%d ny=%d\n",nx,ny);
  return(-1);
  }

if((nx/4)*4 != nx || (ny/4)*4 != ny) {
#ifdef DEBUG
  printf("JLP_iGDev_X11::plot_image/Warning, bad image format for display: nx=%d ny=%d\n",nx,ny);
#endif
  nx = (nx/4)*4;
  ny = (ny/4)*4;
  printf("JLP_iGDev_X11::plot_image/I correct it to: nx=%d ny=%d\n",nx,ny);
  }

xi = XGetImage(Xgc0.display,Xgc0.backing,0,0,nx,ny,AllPlanes,ZPixmap);
/*
printf("Bits per pixel: %d, ZPixmap=%d \n",xi->bits_per_pixel,ZPixmap);
*/
if (xi == NULL)  {
      printf("*** JLP_iGDev_X11::plot_image/XGetImage error!\n"); 
      printf("*** nx = %d  ny = %d \n",nx,ny); 
      return(-1);}

/* Inversion of the rows to get the origin in the lower left corner : */
k = 0;
for (j=ny-1; j>=0; j--)
  {jj = ( j / gamma_d ) * idim;
   for (i=0; i<nx; i++)
   {
    ii = i / gamma_d;
    mdata.i = image2[ii+jj]; 
/* BIT_PER_VIDEO_PIXEL == 8 */
    xi->data[k] = mdata.c[0]; k++;
#if( BIT_PER_VIDEO_PIXEL == 24)
    xi->data[k] = mdata.c[1]; k++;
    xi->data[k] = mdata.c[2]; k++; 
    xi->data[k] = mdata.c[3]; k++; 
#elif(BIT_PER_VIDEO_PIXEL == 16)
    xi->data[k] = mdata.c[1]; k++;
#endif
   }
  }

/*
x_ul, y_ul are the coordinates of the upper-left corner 
of the image relative to the X11-window's origin
*/
conv_mgo_to_dev(xstart, ystart, d_x, d_y);
x_ul = NINT(d_x);
y_ul = NINT(d_y) - ny;
/* JLP2007: to solve a problem when y offset is too large: */
y_ul = MAXI(0, y_ul);

if(x_ul > Jgc0.devwidth || y_ul < 0) {
  printf("JLP_iGDev_X11::plot_image/Fatal error: x_ul=%d y_ul=%d\n", x_ul, y_ul);
  printf("xstart=%d ystart=%d (mgo coord.)\n", x_ul, y_ul);
  exit(-1);
  }

/*
XPutImage(Xgc0.display,Xgc0.win,Xgc0.gc,xi,x_ul,y_ul,x_ul,y_ul,nx,ny);
*/

/* JLP2006: I also draw on the window to have a faster response: */
status = XPutImage(Xgc0.display,Xgc0.win,Xgc0.gc,xi,0,0,x_ul,y_ul,nx,ny);

/* Plot on the backing map (updated with gflush()): */
status = XPutImage(Xgc0.display,Xgc0.backing,Xgc0.gc,xi,0,0,x_ul,y_ul,nx,ny);
switch (status)
    {
    case BadDrawable:
     printf("JLP_iGDev_X11::plot_image/XPutImage/error: bad drawable\n");
     break;
    case BadMatch:
     printf("JLP_iGDev_X11::plot_image/XPutImage/error: bad match\n");
     break;
    case BadValue:
     printf("JLP_iGDev_X11::plot_image/XPutImage/error: bad value\n");
     break;
    default:
     break;
    }
XDestroyImage(xi);

return(0);
}
#endif /* EOF JLP_USE_X11 */
