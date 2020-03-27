/**************************************************************************
* JLP_GDev_PST class (image)
* Definition of the PST graphic driver 
* used in my plotting package
* Derived from virtual class JLP_iGDev defined in "jlp_gdev.h"
*
* JLP
* Version 11/02/2017
**************************************************************************/
// To define the virtual class JLP_GDev:
#include "jlp_gdev.h"

#include "jlp_macros.h"    // NINT ...
#include "jlp_gdev_idv.h"         // GDev_free()

#ifndef __jlp_gdev_PST_h                     /* sentry */
#define __jlp_gdev_PST_h

// To define "std" as the standard prefix (e.g. before printf, scanf, ...)
using namespace std;

class JLP_GDev_PST : public JLP_GDev {

public:

/* Constructor for images:
* JLP_GDev_PST("display", "Xdisp1 version 2006", 128, 128, &gamma1, &gamma_d, 
*                 &status, 1);     (for images)
*
*/
 JLP_GDev_PST(const char *plotdev, const char *out_filename,
              const char* title, 
              double *image_f1, const int nx1, const int ny1, int *gamma1,
              int *gamma_d, int *status, char *err_messg)
         { 
         int nz1 = 1;
         ncalls_image = 0;
         *status = SetupForImage(this, plotdev, out_filename, title, 
                                image_f1, nx1, ny1, nz1, gamma1, gamma_d,
                                 err_messg);
         };

/* Constructor for curves:
* JLP_GDev_PST("xterm", "Xslice version 2006", 1);                 (for curves)
*/
 JLP_GDev_PST(const char *plotdev, const char *out_filename,
              const char* title, double xmin_user, double xmax_user,
              double ymin_user, double ymax_user, int plan, int *status,
              char *err_messg)
         {
         ncalls_image = 0;
         *status = SetupForCurve(this, plotdev, out_filename, title, xmin_user,
                                xmax_user, ymin_user, ymax_user, plan,
                                err_messg);
         };


// Destructor:
// Should be declared as virtual:
  virtual ~JLP_GDev_PST() {
  JLP_GDev_PST::gdev_close();
  if(italk > 1)
      printf(" Hardcopy postscript file %s successfuly created\n", 
              Jgc0.fdv_pst_fname);
  GDev_free_idv(Jgc0.dev_idv);
  }

// To setup the device:
// INPUT: plotdev, title, nx1, ny1
// OUTPUT: offx1, offy1, axlen1, aylen1, dev_width, dev_height
  int setup_device_for_image(const char *plotdev, const char* title,
                             const int nx1, const int ny1, int *offx,
                             int *offy, int *axlen, int *aylen,
                             int *dev_width, int *dev_height, 
                             int *TeX_flag, int *devtype, int *landscape);
  int setup_device_for_curve(const char *plotdev, const char* title,
                             int *width_mm, int *height_mm, 
                             int *TeX_flag, int *devtype, int *landscape);

// To open the device:
  int open_device(const char* title, int dev_width, int dev_height, 
                  int landscape, int *jgc_dev_width, int *jgc_dev_height,
                  int *dev_yorigin_is_on_top);

// To set Jgc0.cwidth and Jgc0.cheight:
  int Jgc0_set_font_size_from_device(double expand0);

// Get limits of a small window (entered interactively by the user):
  int get_winlimits(double& x1, double& y1, double& x2, double& y2,
                    int type_of_win, int& pressed_button, int& in_frame) {
       x1 = y1 = x2 = y2 = pressed_button = 0;
       in_frame= 0;
       return(-1);
       };

// To draw a line:
  int line_device(int x1, int y1, int x2, int y2, int lwidth=0); 

// To draw a polygon:
  int polygon_device(int x, int y, double expand, double angle, int nsides, 
              int filled); 

// To draw a circle:
  int circle_device(int x, int y, int idiam, int filled);

// To draw a filled rectangle (i.e. to paint it with the loaded color):
  int gdev_FilledRect1(double x0, double y0, double x1, double y1, char *color)
    {
    int in_frame;
    double d_x0, d_x1, d_y0, d_y1;
    ConvUserToDev(x0, y0, &d_x0, &d_y0, &in_frame);
    ConvUserToDev(x1, y1, &d_x1, &d_y1, &in_frame);
/* DEBUG:
printf("FilledRect1: x0=%f, y0=%f x1=%f y1=%f\n", x0, y0, x1, y1);
printf("FilledRect1: d_x0=%f, d_y0=%f d_x1=%f d_y1=%f\n", d_x0, d_y0, d_x1, d_y1);
    fprintf(Jgc0.fdv,"%%%%FilledRect1\n");
*/
    fprintf(Jgc0.fdv,"newpath\n");
    fprintf(Jgc0.fdv,"%.2f %.2f m\n", d_x0, d_y0);
    fprintf(Jgc0.fdv,"%.2f %.2f l\n", d_x0, d_y1);
    fprintf(Jgc0.fdv,"%.2f %.2f l\n", d_x1, d_y1);
    fprintf(Jgc0.fdv,"%.2f %.2f l\n", d_x1, d_y0);
    fprintf(Jgc0.fdv,"closepath\n");
    fprintf(Jgc0.fdv,"gsave\n");
    SetPColor(color);
    fprintf(Jgc0.fdv,"fill\n grestore\n");
    return(0);
   }

// Set color of line:
// r, g, b values between 0 and 1. 
  int setrgbcolor(double r, double g, double b){
    fprintf(Jgc0.fdv,"%.2f %.2f %.2f setrgbcolor\n", r, g, b); 
    return(0);
    }

// Conversion user/device coordinates:
// (defined in jlp_gdev_wxwid.h, jlp_gdev_pstcopy.h, etc)
  void ConvDevToUserForImages(double dev_x0, double dev_y0,
                              double *user_x1, double *user_y1, int *in_frame);
  void ConvUserToDevForImages(double user_x1,  double user_y1,
                        double *dev_x0, double *dev_y0, int *in_frame);

// Default is black:
  int setdefaultcolor(){return(setrgbcolor(0., 0., 0.));};


// Set line width and line type:
  int SetLineWidthAndType(int lwidth, int ltype){
// JLP2007: default lwidth is 0, but this is too small for postscript:
    fprintf(Jgc0.fdv,"%d setlinewidth\n", (lwidth+1));
    switch (ltype) {
// See Postscript Reference manual (3rd edition) p 667
// Long dash, long blank 
      case 1:
        fprintf(Jgc0.fdv,"[3] 0 setdash\n");
        break;
// Short dash, short blank 
      case 2:
        fprintf(Jgc0.fdv,"[2] 0 setdash\n");
        break;
// Short dash, very short blank 
      case 3:
        fprintf(Jgc0.fdv,"[2 1] 0 setdash\n");
        break;
// Long dash, short blank, short dash, short blank 
      case 4:
        fprintf(Jgc0.fdv,"[3 2 1 2] 0 setdash\n");
        break;
// Solid, unbroken lines:
      case 0:
      default:
        fprintf(Jgc0.fdv,"[] 0 setdash\n");
        break;
      }
    return(0);
    }

// Set shade of gray: 0=black 1=white
  int setgray(double gray_shade) {
    fprintf(Jgc0.fdv,"%.1f setgray\n",gray_shade);
    return(0);
    }

// To erase all the drawings on the window: 
// and be ready for a new plot:
  int gdev_erase(){
    fprintf(Jgc0.fdv,"showpage grestore gsave \n");
    return(0);
    }

// Flush graphic to device
  int gdev_gflush() {
    fprintf(Jgc0.fdv,"stroke \n");
    return(0);
    }

// Close graphic device
// (End of file) 
  int gdev_close() {
    fprintf(Jgc0.fdv,"showpage \n");
    fclose(Jgc0.fdv);
    return(0);
    }

/************************************************************************ 
* Allocate LUT:
*
* Input:
*  ncolors: number of colors wanted by the user 
*
* Output:
*  mylut: pointer array of the allocated cells 
*  ncolors: number of colors successfully allocated
*  private_lut: set to one if private LUT is allowed (i.e., LUT can be changed)
**************************************************************************/
  int alloc_lut_device(int *mylut, int& private_lut, int& ncolors)
   {
    for(int i = 0; i < ncolors; i++) mylut[i] = i;
    private_lut = 1;
    return(0);
   }

/****************************************************************
* Load (r,g,b) values to private LUT 
*
* Return the index j in mylut array of i^th color in (r, g, b) array 
* In the case of postscript, it is an obvious relation: j=i
* but it is much more complex for X11 device
*****************************************************************/
  int load_lut_device(const int *r, const int *g, const int *b, 
                      int *mylut, int ncolors) 
   {
    int i;
    for(i = 0; i < ncolors; i++) mylut[i] = i;
    return(0);
   }
  
// in jlp_gd_pstcopy.cpp:
  int pstcopy1(int *image1, int nx1_0, int ny1_0, int idim,
               double low_thresh, double high_thresh, int ncolors,
               double width, double height, char *title, char *image_name,
               char *image_comments, char *extra_comments, int lut_scale, 
               int black_and_white, int high_resolution, int f1_xstart, 
               int f1_ystart,int f1_xend, int f1_yend);

// Plot an image on the graphic device 
  int plot_image(int *image, int nx, int ny, int idim, int xstart, int ystart, 
                 int gamma1, int gamma_d, int black_and_white);

// To plot a label (i.e. a string) on the graphic device:
// xstart, ystart are mgo coordinates
  double label_device(const char *s, int xstart, int ystart, double angle,
                     double expand, int draw_it);

private:
int pst_code16(int *image, int size, int *r, int ncolors);
int pst_code256(int *image, int size, int *r, int ncolors);
int plot_image16(int *image, int nx, int ny, int idim,
                 int *r, int ncolors, int xstart, int ystart);
int plot_image256(int *image, int nx, int ny, int idim,
                  int *r, int ncolors, int xstart, int ystart);
int plot_imageRGB(int *image, int nx, int ny, int idim,
                          int *r, int *g, int *b, int ncolors,
                          int xstart, int ystart);
// in jlp_gd_pstcopy.cpp:
int draw_extra_symbols(FILE *fp1); 

// Private variables:
int ncalls_image;
}; 

#endif    /* __jlp_gdev_PST_h sentry */
