/**************************************************************************
* JLP_GDev_X11 class
* Definition of the X11 graphic driver 
* used in my plotting package
* Derived from virtual class JLP_GDev defined in "jlp_gdev.h"
*
* JLP
* Version 11/01/2007
**************************************************************************/
#if JLP_USE_X11 /* New flag to disable X11 if necessary */

// To define the virtual class JLP_GDev (Curve graphic Devices) 
#include "jlp_gdev.h"

#ifndef __jlp_gd_X11_h                     /* sentry */
#define __jlp_gd_X11_h

#include "jlp_x11_def.h" // XBUTTON, XSTATIC, etc 
#include "jlp_gdev_def.h"  //  BIT_PER_VIDEO_PIXEL

#define NBUT_MAX 20               // Maximum number of buttons
#define POPUP_NITEMS_MAX 20       // Maximum number of items in popup menu

// To define "std" as the standard prefix (e.g. before printf, scanf, ...)
using namespace std;

class JLP_GDev_X11 : public JLP_GDev {

public:

/* Constructor for curves:
*   JLP_GDev_X11("xterm", "Xslice version 2006", 1);   
*/
 JLP_GDev_X11(const char *plotdev, const char *out_filename, const char* title,
              double& xminuser, double& xmaxuser, double &yminuser,
              double &ymaxuser, int &plan, int& status)
 {
  char err_messg[256];
  Xgc0.xvec = NULL;
  status = SetupForCurve(this, plotdev, out_filename, title, xminuser, xmaxuser,
                          yminuser, ymaxuser, plan, err_messg);
 };

/* Constructor for images:
*   JLP_GDev_X11("display", "Xdisp1", "test.fits", image_f1, 128, 128,
*                 &gamma1, &gamma_d, &status, 1);     (for images)
*/
 JLP_GDev_X11(const char *plotdev, const char *out_filename, const char* title, 
              const char *filename, double *image_f1, const int nx1,
              const int ny1, int *gamma1, int *gamma_d, int& status)
 {
  char err_messg[256];
  int nz1 = 1;
  Xgc0.xvec = NULL;
  status = SetupForImage(this, plotdev, out_filename, title, image_f1,
                         nx1, ny1, nz1, gamma1, gamma_d, err_messg);
 };

// Destructor:
  virtual ~JLP_GDev_X11() {
  JLP_GDev_X11::close_device();
  if(italk > 1) printf("JLP_GDev_X11: destructor has been called\n");
  }

// Close graphic device
  int close() {this->~JLP_GDev_X11(); return(0);}
  int close_device();

// To setup the device for curves:
// INPUT: plotdev, title, nx1, ny1
// OUTPUT: offx, offy, axlen, aylen, dev_width, dev_height
  int setup_device_for_curve(const char *plotdev, const char* title,
                             int& offx, int& offy, int& axlen, int& aylen, 
                             int& dev_width, int& dev_height, int& TeX_flag, 
                             int& devtype, int& landscape, int idev1);

// To setup the device for images:
// INPUT: plotdev, title, nx1, ny1
// OUTPUT: offx, offy, axlen, aylen, dev_width, dev_height
  int setup_device_for_image(const char *plotdev, const char* title,
                             const int nx1, const int ny1, int& offx,
                             int& offy, int& axlen, int& aylen,
                             int& dev_width, int& dev_height, int& TeX_flag,
                             int& devtype, int& landscape, int idev1);

// To open the device:
  int open(const char* title, int dev_width, int dev_height, 
           const int landscape);

// To draw a line:
  int line_device(int x1, int y1, int x2, int y2, int lwidth=0); 

// To draw a polygon:
  int polygon(int x, int y, double expand, double angle, int nsides, 
              int filled); 

// To draw a circle:
  int circle(int x, int y, int idiam, int filled) {return(-1);};

// To fill a rectangle (i.e. to paint it with a color):
  int gdev_FilledRect1(double x0, double y0, double x1, double y1, 
                       char *color){return(-1);}

// Set color of line:
  int setrgbcolor(double r, double g, double b);

// Default is white:
  int setdefaultcolor(){return(setrgbcolor(1., 1., 1.));};

// Set line width and line type:
  int SetLineWidthAndType(int lwidth, int ltype){
      return SetLineWidthAndType_to_gc(Xgc0.gc, lwidth, ltype);
   }
  int SetLineWidthAndType_to_gc(GC& MyGC, int lwidth, int ltype);

// Erase all the drawings inside the window: 
  int erase();

// Wait for next event (mouse button for X11)
  int wait_for_events();

// Flush graphic to device
  int gflush();

// Setup menu:
/*
  virtual int setup_menu(char *items, const int nitems,
                         const int menu_nsub, const int menu_slen,
                         const int vertical);
*/
  int setup_menu(char *items, const int nitems,
                 const int menu_nsub, const int menu_slen,
                 const int vertical);

// Select menu item:
/*
  virtual int select_menu(int& select, int& subselect);
*/
  int select_menu(int& select, int& subselect);

// Cursor function: 
  int cursor(int& x, int& y, char* cursor_type, int& pressed_button);

// Get one or two concentric circles (entered interactively by the user):
  int get_circles(double& x_cent, double& y_cent, double& diam1,
                   double& diam2, int& ncirc);

// Get limits of a small window (entered interactively by the user):
  int get_winlimits(double *x1, double *y1, double *x2, double *y2, 
                    int type_of_win, int *pressed_button, int *in_frame);

/* Allocate LUT:
* Output:
*  mylut: pointer array of the allocated cells (for X11)
*  ncolors: number of colors successfully allocated
*  private_lut: set to one if private LUT is allowed (i.e., LUT can be changed)
*/
  int alloc_lut_device(int *mylut, int& private_lut, int& ncolors);

// Load (r,g,b) values to private LUT when possible
// or, if read-only LUT, look for cells with nearest values
// In this case, mylut values will be modified
  int load_lut_device(const int *r, const int *g, const int *b, 
                      int *mylut, int ncolors);
  
// Plot an image on the graphic device 
  int plot_image(int *image, int nx, int ny, int idim, int xstart, int ystart, 
                 int gamma_d, int black_and_white);

// To plot a label (i.e. a string) on the graphic device:
// xstart, ystart are mgo coordinates
  double label_device(const char *s, int xstart, int ystart, double angle,
                     double expand, int draw_it);

// Conversion from mgo to device coordinates:
inline  void conv_mgo_to_dev(const int x, const int y, double *d_x, double *d_y) {
      double g_dx, g_dy;
      g_dx = (double)Jgc0.dev_width / (double)SCREEN_SIZE;
      g_dy = (double)Jgc0.dev_height / (double)SCREEN_SIZE;
      *d_x = (double)x * g_dx;
      *d_y = (double)Jgc0.dev_height  - (double)y * g_dy;
      }

// Conversion from device to mgo coordinates:
inline  void conv_dev_to_mgo(const double d_x, const double d_y, int *x, int *y) 
      {
      double g_dx, g_dy;
      g_dx = (double)Jgc0.dev_width / (double)SCREEN_SIZE;
      g_dy = (double)Jgc0.dev_height / (double)SCREEN_SIZE;
      *x = NINT(d_x / g_dx);
      *y = NINT(((double)Jgc0.dev_height  - d_y) / g_dy);
      }

// Conversion from device to user coordinates:
inline  void conv_dev_to_user(const double d_x, const double d_y, 
                              double *x, double *y, int *in_frame) 
      {
      int ix, iy;
      conv_dev_to_mgo(d_x, d_y, &ix, &iy);
      conv_mgo_to_user(ix, iy, x, y, in_frame);
      }

// Status bar:
  int CreateStatusBar(int ix, int iy, int width, int height){
        return CreateStaticButton(Xgc0.StatusBar," ",ix, iy,width,height); 
        };
  int DrawToStatusBar(char *label);
  int EraseStatusBar();

// To display the position of the cursor: 
  int CreateCursorPosition(int ix, int iy, int width, int height){
        return CreateStaticButton(Xgc0.CursorPosition," ",ix, iy,width,height); 
        };
  int DrawToCursorPosition(char *label);
  int EraseCursorPosition();

private:
  int CreateMenuButtons(char *items, const int nitems,
                        const int menu_nsub, const int menu_slen,
                        const int ix0, const int iy0, const int width,
                        const int height, const int vertical);
  int CreateButton(XBUTTON &MyBut, const char *label, const int x_ul,
                   const int y_ul, const int d_width, const int d_height);
  int CreateStaticButton(XSTATIC &MyStat, const char *label, const int ix,
                         const int iy, const int width, const int height);
  int CreatePopupMenu(XBUTTON &MyBut, const char *items, const int menu_slen, 
                      const int nitems, const int d_width, const int d_height,
                      const int vertical);
  int DrawStaticButton(XSTATIC &MyStat);
  int DrawButton(XBUTTON &MyBut);
  int DrawPopupMenu(XBUTTON &MyBut);
  int empty_xvec();
  int draw_label0(const char *s, int x, int y);
  int color_ressources();
  int look_for_visuals();
  int load_standard_lut(int *r, int *g, int *b, int *mylut_x11, int ncolors);
  int redraw();
  int create_win_gc();
  int create_menu_gc();
  int button_was_pressed(XEvent& event, int& select);

// Private variables:
  XCONTEXT Xgc0;   // X11 context
}; 

#endif    /* __jlp_gd_X11_h sentry */
#endif    /* EOF JLP_USE_X11 */
