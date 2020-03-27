/*************************************************************************
* \file jlp_gdev_utils.h
* \class JLP_GDev (image Graphic Device) 
* \author JLP
* \date 03/01/2017
* \brief Basic drawing and utilitary routines used by JLP_GDev
*
* JLP
* Version 03/01/2017
**************************************************************************/
#include <stdio.h>
#include <stdlib.h>   // exit();
#include <string.h>   // Problems with index which is defined differently 
                      // in wxwidgets...
#undef index          // Can solve the problem ?

#include "jlp_gdev.h"    // JLP_GDev class 
#include "jlp_gtext.h"    // JLP_GText
#include "jlp_macros.h"       // To define MAXI, MINI, etc

// #define DEBUG

/***************************************************************************
* gdev_line(...)
* To draw a line:
*
* INPUT:
* x1, y1, x2, y2 : mgo coordinates
***************************************************************************/
int JLP_GDev::gdev_line(int x1, int y1, int x2, int y2, int lwidth, 
                        int backup_to_file) 
{
 double xstart, ystart, xend, yend;
 int in_frame, status;
/* Possibility of saving this call into a file for further hardcopy...
* In user coordinates (better for interpretation
* and direct generation of graphic files)
*/
 if(Jgc0.fp_backup != NULL && backup_to_file) {
  conv_mgo_to_user(x1, y1, &xstart, &ystart, &in_frame);
  conv_mgo_to_user(x2, y2, &xend, &yend, &in_frame);
  fprintf(Jgc0.fp_backup,"jlp_line1 %.2f %.2f %.2f %.2f\n",
          xstart, ystart, xend, yend);
 }
 status = line_device(x1, y1, x2, y2, lwidth);
 return status;
} 
/***************************************************************************
* gdev_line1(...)
* To draw a line:
* Same as line but location is given here in user coordinates
*
* INPUT:
* user_x1, user_y1, user_x2, user_y2 : user coordinates
***************************************************************************/
int JLP_GDev::gdev_line1(double user_x1, double user_y1, double user_x2, 
                         double user_y2, int lwidth, int backup_to_file) 
{ 
 int x1_mgo, x2_mgo, y1_mgo, y2_mgo;
 conv_user_to_mgo(user_x1, user_y1, &x1_mgo, &y1_mgo);
 conv_user_to_mgo(user_x2, user_y2, &x2_mgo, &y2_mgo);

 return  gdev_line(x1_mgo, y1_mgo, x2_mgo, y2_mgo, lwidth, backup_to_file);  
}

/***************************************************************************
* "Move to" (update current location in mgo coord.)
*
* INPUT:
* x, y : MGO coordinates
***************************************************************************/
int JLP_GDev::gdev_moveto(const int x, const int y) 
{ 
    Jgc0.xp = x; 
    Jgc0.yp = y; 
    return 0;
}
/***************************************************************************
* "Draw a line to" (from current location) in mgo coord.):
*
* INPUT:
* x, y : MGO coordinates
***************************************************************************/
void JLP_GDev::gdev_lineto(const int x, const int y) 
{ 
  gdev_line(Jgc0.xp, Jgc0.yp, x, y); 
}
/***********************************************************************
* Get the current color used for drawing the lines and symbols
*
************************************************************************/
int JLP_GDev::GetCurrentPColor(char *pcolor)
{
strcpy(pcolor, cgdev_current_pcolor);
return(0);
}
/***********************************************************************
* Select the color to be used for drawing the lines and symbols
*
* General syntax is RGB_rr_gg_bb,
* for instance RGB_02_30_40  to obtain setrgb(0.02 0.30 0.40)
************************************************************************/
int JLP_GDev::SetPColor(char *pcolor)
{
int r, g, b, ival, status = 0;
double rr, gg, bb, cmax;
 if(!strncmp(pcolor,"Aquamarine",10)){
     setrgbcolor(0.59, 0.89, 0.89);
  } else if (!strncmp(pcolor,"LightPurple",6)){
     setrgbcolor(0.59, 0.49, 0.69);
  } else if(!strncmp(pcolor,"Red",3)) {
     setrgbcolor(1.0, 0.0, 0.0);
  } else if(!strncmp(pcolor,"Green",5)) {
// 010 is too bright, so I put:
     setrgbcolor(0.0, 0.8, 0.0);
  } else if(!strncmp(pcolor,"Blue",4)) {
     setrgbcolor(0.0, 0.0, 1.0);
  } else if(!strncmp(pcolor,"Yellow",6)) {
     setrgbcolor(1.0, 1.0, 0.0);
  } else if(!strncmp(pcolor,"Orange",6)) {
     setrgbcolor(1.0, 0.6, 0.0);
  } else if(!strncmp(pcolor,"Purple",6)) {
     setrgbcolor(1.0, 0.0, 1.0);
  } else if(!strncmp(pcolor,"SkyBlue",6)) {
     setrgbcolor(0.0, 1.0, 1.0);
  } else if(!strncmp(pcolor,"DarkGreen",5)) {
     setrgbcolor(0.0, 0.6, 0.4);
  } else if(!strncmp(pcolor,"Gray",4)) {
     setrgbcolor(0.5, 0.5, 0.5);
  } else if(!strncmp(pcolor,"Black",5)) {
     setrgbcolor(0.0, 0.0, 0.0);
  } else if(!strncmp(pcolor,"White",5)) {
     setrgbcolor(1.0, 1.0, 1.0);
/* RGB_50_61_80 => 0.5 0.61 0.80*/
  } else if(!strncmp(pcolor,"RGB",3)) {
     ival = sscanf(pcolor,"RGB_%d_%d_%d", &r, &g, &b);
     if(ival == 3) {
     rr = (double)r / 100.;
     gg = (double)g / 100.;
     bb = (double)b / 100.;
     setrgbcolor(rr, gg, bb);
     } else {
     status = -1;
     }
/* Default is black for postscript,
*             white for xterm
*/
  } else if(!strncmp(pcolor,"Default",7)) {
// JLP 2015:
       setdefaultcolor();
  } else {
    status = -1;
  }

/* Test on brightness if xterm (otherwise don't see the curve! )
*/
if(Jgc0.dev_type == 1) {
  cmax = MAXI(rr, MAXI(gg, bb));
  if(cmax < 0.5) {
   rr += (1.  - cmax);
   gg += (1.  - cmax);
   bb += (1.  - cmax);
   }
  }

return(status);
}
/******************************************************************************
* gdev_label()
* Draw a label (i.e. a string) on the graphic device:
* INPUT:
*  xstart, ystart are mgo coordinates
* OUTPUT
*  Return the length of the label (in device coordinates)
*******************************************************************************/
double JLP_GDev::gdev_label(const char *s, int ixstart, int iystart, 
                            double angle, double expand, int draw_it, 
                            int backup_to_file) 
{
char s_copy[120], *pc;
double length = 0.;

// TeX fonts if needed:
if(Jgc0.TeX_flag == 1) {
 length = gdev_label_TeX(s, ixstart, iystart, angle, expand, draw_it);
 return(length);
 }

/* Possibility of saving this call into a file for further hardcopy...
* In user coordinates (better for interpretation
* and direct generation of graphic files)
*/
 if(Jgc0.fp_backup != NULL && draw_it && backup_to_file) {
   int in_frame;
   double xstart, ystart;
   conv_mgo_to_user(ixstart, iystart, &xstart, &ystart, &in_frame);
// Warning: put string at the end to avoid pb with blanks... 
   fprintf(Jgc0.fp_backup,"jlp_label1 %.2f %.2f %.1f %.1f @%s@\n",
           xstart,ystart,angle,expand,s);
  }

/* Remove trailing blanks (needed when called by Fortran programs...)*/
 if(strlen(s) >= 120) {
   fprintf(stderr,"gdev_label: fatal error too long string: >%s<\n",
          s);
   exit(-1);
   }
 strcpy(s_copy,s);
 s_copy[119] = '\0';
 pc = s_copy;
// Look for the end of string:
 while(*pc) pc++;
// Remove blanks at the end of string if present,
// by writing a "zero" to reduce the useful length of the string:
 pc--; while(*pc == ' ') pc--;
 pc++; if(*pc) *pc = '\0';

 length = this->label_device(s_copy,ixstart,iystart,angle,expand,draw_it);

/* JLP2006: I reduce length when "." (needed for tick labels): */
  {char buf[120];
  strncpy(buf, s, 120);
  buf[119] = '\0';
  pc = buf;
  while(*pc) {if(*pc == '.') length -= (0.8 * Jgc0.cwidth); pc++;}
  }

return length;
}
/******************************************************************************
* gdev_label1()
* Same as gdev_label but location is given here in user coordinates
* Draw a label (i.e. a string) on the graphic device:
* INPUT:
*  xstart, ystart : user coordinates
* OUTPUT
*  Return the length of the label (in device coordinates)
*******************************************************************************/
double JLP_GDev::gdev_label1(const char *s, double xstart, double ystart, 
                             double angle, double expand, int draw_it, 
                             int backup_to_file) 
{
int ix, iy;
  conv_user_to_mgo(xstart, ystart, &ix, &iy);
  return gdev_label(s, ix, iy, angle, expand, draw_it, backup_to_file);  
}
/***********************************************************************
* To plot a label (i.e. a string) on the graphic device:
* xstart, ystart are mgo coordinates
* Return length of label (in device coordinates)
************************************************************************/
double JLP_GDev::gdev_label_TeX(const char *s, int xstart, int ystart,
                             double angle, double expand, int draw_it)
{
double length = -1;
  if(MyText != NULL) {
    length = MyText->label_TeX(*this, s, xstart, ystart, angle, expand,
                               draw_it);
    }
return length;
}
/*************************************************************
* ConvDevToUser
* to convert device coordinates to user coordinates
*
* INPUT:
*  dev_x0, dev_y0: device coordinates
*
* OUTPUT:
*  user_x1, user_y1: user coordinates
*  in_frame: 1 if x1 and y1 are in the range [0, nx1[ or [0, ny1[
*            0 otherwise
*
*************************************************************/
int JLP_GDev::ConvDevToUser(double dev_x0, double dev_y0, 
                            double *user_x1, double *user_y1, int *in_frame)
{
int x2_mgo, y2_mgo, status = 0;
double g_dx, g_dy;

/* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
*/
 if(Jgc0.gdev_graphic_type == 3) {
// Scrolled images:
  ConvDevToUserForImages(dev_x0, dev_y0, user_x1, user_y1, in_frame);
 } else {
   g_dx = (double)Jgc0.dev_width / (double)SCREEN_SIZE;
   g_dy = (double)Jgc0.dev_height / (double)SCREEN_SIZE;
   x2_mgo = NINT((double)dev_x0 / g_dx);
   y2_mgo = NINT((double)(Jgc0.dev_height  - dev_y0) / g_dy);
   conv_mgo_to_user(x2_mgo, y2_mgo, user_x1, user_y1, in_frame);
 }

return(status);
}
/*************************************************************
* ConvDevToMgo
* to convert device coordinates to mgo coordinates
*
* INPUT:
*  dev_x0, dev_y0: device coordinates
*
* OUTPUT:
*  mgo_x1, mgo_y1: mgo coordinates
*
*************************************************************/
int JLP_GDev::ConvDevToMgo(double dev_x0, double dev_y0, int *mgo_x1, int *mgo_y1)
{
int in_frame, status = 0;
double g_dx, g_dy, user_x1, user_y1;

*mgo_x1 = 0;
*mgo_y1 = 0;

/* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
*/
// Scrolled images:
 if(Jgc0.gdev_graphic_type == 3) {
  ConvDevToUserForImages(dev_x0, dev_y0, &user_x1, &user_y1, &in_frame);
  if(in_frame == 1)
    conv_user_to_mgo(user_x1, user_y1, mgo_x1, mgo_y1);
  else
    status = -1;
// Curves:
 } else {
   g_dx = (double)Jgc0.dev_width / (double)SCREEN_SIZE;
   g_dy = (double)Jgc0.dev_height / (double)SCREEN_SIZE;
   *mgo_x1 = NINT(dev_x0 / g_dx);
// Assume that the Y origin of output device is on top: 
   if(Jgc0.dev_yorigin_is_on_top == 1)
     *mgo_y1 = NINT(((double)Jgc0.dev_height  - dev_y0) / g_dy);
// Assume that the Y origin of output device is on the bottom: 
   else 
     *mgo_y1 = NINT(dev_y0 / g_dy);
 }

#ifdef DEBUG
printf("ConvDevToMgo: dev: %f %f user: %f %f mgo: %d %d\n", 
        dev_x0, dev_y0, user_x1, user_y1, *mgo_x1, *mgo_y1);
#endif
 
return(status);
}
/*************************************************************
* ConvUserToDev
* to convert user coordinates to device coordinates
*
* INPUT:
*  user_x1, user_y1: user coordinates
*
* OUTPUT:
*  dev_x0, dev_y0: device coordinates
*  in_frame: 1 if (ix, iy) is inside the user box
*            0 otherwise
*
*************************************************************/
int JLP_GDev::ConvUserToDev(double user_x1,  double user_y1,
                            double *dev_x0, double *dev_y0, int *in_frame)
{
int x2_mgo, y2_mgo, status = 0;
double g_dx, g_dy;

*dev_x0 = 0;
*dev_y0 = 0;
*in_frame = 1;

/* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
*/
 if(Jgc0.gdev_graphic_type == 3) {
// Scrolled images need special handling:
  ConvUserToDevForImages(user_x1, user_y1, dev_x0, dev_y0, in_frame);
 } else {
// Curves:
   g_dx = (double)Jgc0.dev_width / (double)SCREEN_SIZE;
   g_dy = (double)Jgc0.dev_height / (double)SCREEN_SIZE;
   conv_user_to_mgo(user_x1, user_y1, &x2_mgo, &y2_mgo);
   *dev_x0 = (double)x2_mgo * g_dx;
// If y starts from top
   if(Jgc0.dev_yorigin_is_on_top == 1) {
     *dev_y0 = (double)Jgc0.dev_height - (double)y2_mgo * g_dy;
     } else {
// If y starts from bottom 
     *dev_y0 = (double)y2_mgo * g_dy;
     }
/* DEBUG
printf("ConvUserToDev: user: x=%.3f y=%.3f dev: x=%.3f y=%.3f\n", 
        user_x1, user_y1, *dev_x0, *dev_y0);
*/
 }

return(status);
}
/*************************************************************
* ConvMgoToDev
* to convert user coordinates to device coordinates
*
* INPUT:
*  mgo_x1, mgo_y1: mgo coordinates
*
* OUTPUT:
*  dev_x0, dev_y0: device coordinates
*
*************************************************************/
int JLP_GDev::ConvMgoToDev(int mgo_x1, int mgo_y1, double *dev_x0, 
                           double *dev_y0)
{
int in_frame, status = 0;
double g_dx, g_dy, user_x1, user_y1;

*dev_x0 = 0.;
*dev_y0 = 0.;

/* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
*/
 if(Jgc0.gdev_graphic_type == 3) {
  conv_mgo_to_user(mgo_x1, mgo_y1, &user_x1, &user_y1, &in_frame);
// Scrolled images need special handling:
  if(in_frame == 1)
    ConvUserToDevForImages(user_x1, user_y1, dev_x0, dev_y0, &in_frame);
  if(in_frame == 0) status = -1;
 } else {
// Curves:
   g_dx = (double)Jgc0.dev_width / (double)SCREEN_SIZE;
   g_dy = (double)Jgc0.dev_height / (double)SCREEN_SIZE;
   *dev_x0 = (double)mgo_x1 * g_dx;
// Assume that the Y origin of output device is on the top 
   if(Jgc0.dev_yorigin_is_on_top == 1)
     *dev_y0 = (double)Jgc0.dev_height  - (double)mgo_y1 * g_dy;
// Assume that the Y origin of output device is on the bottom 
   else 
     *dev_y0 = (double)mgo_y1 * g_dy;
 }

#ifdef DEBUG
printf("ConvMgoToDev: mgo: %d %d user : %f %f dev: %f %f (in_frame=%d)\n", 
        mgo_x1, mgo_y1, user_x1, user_y1, *dev_x0, *dev_y0, in_frame);
#endif

return(status);
}
/**********************************************************************
* conv_user_to_mgo()
* Conversion from user to mgo coordinates:
* imgo = offx +  fsx * (x_user - box_xmin)
* x_user = box_xmin + (imgo - offx) / fsx 
* NB: fsx = axlen / (box_xmax - box_xmin)
**********************************************************************/
void JLP_GDev::conv_user_to_mgo(double user_x, double user_y, int *ix, int *iy) 
{
 *ix = Mgc0.gx1 + Mgc0.fsx * (user_x - Mgc0.fx1);
 *iy = Mgc0.gy1 + Mgc0.fsy * (user_y - Mgc0.fy1);

// Check if OK:
 *ix = MINI(SCREEN_SIZE, *ix);
 *ix = MAXI(0, *ix);
 *iy = MINI(SCREEN_SIZE, *iy);
 *iy = MAXI(0, *iy);
#ifdef DEBUG
printf("conv_user_to_mgo: user : %f %f mgo: %d %d fx1,fy1= %f %f fsx,fsy= %f %f\n", 
        user_x, user_y, *ix, *iy, Mgc0.fx1, Mgc0.fy1, Mgc0.fsx, Mgc0.fsy);
#endif

}
/**********************************************************************
* conv_mgo_to_user()
* Conversion from mgo to user coordinates:
* imgo = offx +  fsx * (x_user - box_xmin)
* x_user = box_xmin + (imgo - offx) / fsx 
* NB: fsx = axlen / (box_xmax - box_xmin)
**********************************************************************/
void JLP_GDev::conv_mgo_to_user(int ix, int iy, double *user_x, double *user_y,
                                 int *in_frame) 
{

/* Check if it is in the frame: */
 if( ix >= Jgc0.offx && ix < (Jgc0.offx+Jgc0.axlen)
     && iy >= Jgc0.offy && iy < (Jgc0.offy+Jgc0.aylen) ) {
// fsx = axlen / (box_xmax - box_xmin)
// fsy = aylen / (box_ymax - box_ymin)
   *user_x = Jgc0.axis_limits[0] + (double)(ix - Jgc0.offx) / Mgc0.fsx;
   *user_y = Jgc0.axis_limits[2] + (double)(iy - Jgc0.offy) / Mgc0.fsy;
   *in_frame = 1;
   } else {
   *user_x = *user_y = -1.;
   *in_frame = 0;
   }

return;
}
/************************************************************************
* Set new device size 
* 
************************************************************************/
int JLP_GDev::SetNewDeviceSize(const int width, const int height)
{
 Jgc0.dev_width = width;
 Jgc0.dev_height = height;
// Update mgo parameters:
 FromJgc0ToMgc0();
 return(0);
}
/************************************************************************
* Line type (0=solid, 1=dashed, 2=dash-dot, etc)
* Line width (thickness of lines)
************************************************************************/
int JLP_GDev::Set_ltype_and_lwidth(const int ltype, const int lwidth) 
{
 int status = -1;
// Empty buffer with old settings:
 gdev_gflush();
// Set private parameters to new values if correct: 
 if(ltype >= 0 && ltype < 8 && lwidth >= 0 && lwidth < 10){ 
   Mgc0.lltype = ltype; 
   Mgc0.lwidth = lwidth; 
// Set to new value: 
   status = SetLineWidthAndType(lwidth,ltype);
   }
return status;
}
