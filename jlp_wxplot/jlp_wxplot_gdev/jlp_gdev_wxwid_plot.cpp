/*************************************************************************
* JLP_GDev_wxWID Class
* Most of the basic plotting routines needed by JLP_igDev virtual class
*
* For LUT:
*   mylut_x11[]: array of X11 addresses of allocated read/write color cells
*   r[], g[], b[]: colors of corresponding cells
*
* JLP
* Version 29/10/2015
**************************************************************************/
#include "jlp_gdev.h"
#include "jlp_gdev_wxwid.h"
#include "jlp_wx_cursor.h"
#include "jlp_wxgdev_labels.h"      // JLP_wxGDevLabels class
#include "jlp_wx_gdproc1.h"        // For JLP_GDProc1

/*
#define DEBUG
*/

#ifndef PI
#define PI 3.1415926536
#endif

#define MAX_POLYGON 20		/* maximum corners for PTYPE 3 points */
/* JLP 91 200 --> 2000 */
#define START_NVEC 2000		/* starting value for max_nvec */
#define FONT_SIZE_FOR_12PT   12.         /* Size of fonts */

/*************************************************************************
* Set fonts
*
* default size is 12 points
**************************************************************************/
int JLP_GDev_wxWID::SetFontToBackupDC(char *font_name,
                                      const unsigned int font_size)
{
int status;
wxFont def_Font = wxSystemSettings::GetFont(wxSYS_DEFAULT_GUI_FONT);
/**** Bold:
wxFont font0(font_size, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_BOLD);
****/
// Some fonts do not allow rotation with DrawRotatedText
/*
wxFont font0(font_size, wxFONTFAMILY_DEFAULT, wxFONTSTYLE_NORMAL,
             wxFONTWEIGHT_NORMAL, false, "Courier");
*/

// wxSWISS_FONT allows rotation with DrawRotatedText
wxFont font0(font_size, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL,
             wxFONTWEIGHT_NORMAL);
/* default size is 96 for postscript
wxFont font00(96, wxFONTFAMILY_ROMAN, wxFONTSTYLE_NORMAL,
              wxFONTWEIGHT_NORMAL);
*/
wxFont font00(8 * font_size, wxFONTFAMILY_ROMAN, wxFONTSTYLE_NORMAL,
              wxFONTWEIGHT_NORMAL);

if(initialized != 1234) return(-1);

if(backup_dc != NULL) {
   if(strcmp(font_name, "default") == 0) {
    backup_dc->SetFont(font0);
   } else {
    backup_dc->SetFont(font0);
// backup_dc->SetFont(*wxSWISS_FONT);
   }
 status = 0;
 }

// Postscript:
if(backup_pst_dc1 != NULL) {
  backup_pst_dc1->SetFont(font00);
  }

return(status);
}
/*************************************************************
* Draw a line
*
* INPUT:
* mgo_x1, mgo_y1: coordinates of starting point (mgo coordinates)
* mgo_x2, mgo_y2: coordinates of ending point (mgo coordinates)
* lwidth: line width (NOT USED YET)
*
* OUTPUT:
* Mgc0.lwidth updated
************************************************************/
int JLP_GDev_wxWID::line_device(int mgo_x1, int mgo_y1, int mgo_x2,
                                int mgo_y2, int lwidth)
{
double d_x1, d_y1, d_x2, d_y2;

// Test if class parameters have been initialized:
 if(initialized != 1234) return(-1);

// Conversion from mgo to device coordinates:
  ConvMgoToDev(mgo_x1, mgo_y1, &d_x1, &d_y1);
  ConvMgoToDev(mgo_x2, mgo_y2, &d_x2, &d_y2);
  backup_dc->DrawLine( (int)d_x1, (int)d_y1, (int)d_x2, (int)d_y2);

return(0);
}

/*************************************************************
* To set line attributes to a graphic context
*
* lwidth = width of the line
* ltype = 0 solid line, 1=dashed line, 2=dotted line, etc
*************************************************************/
int JLP_GDev_wxWID::SetLineWidthAndType(int lwidth, int ltype)
{
wxPen my_pen;
int style0;

// Test if class parameters have been initialized:
 if(initialized != 1234) return(-1);

 my_pen = backup_dc->GetPen();
 my_pen.SetWidth(lwidth);

switch(ltype) {
 case 0:
 default:
   my_pen.SetStyle(wxSOLID);
   break;
 case 1:
   my_pen.SetStyle(wxLONG_DASH);
   break;
 case 2:
   my_pen.SetStyle(wxDOT);
   break;
 case 3:
   my_pen.SetStyle(wxDOT_DASH);
   break;
 case 4:
   my_pen.SetStyle(wxSHORT_DASH);
   break;
 case 5:
   my_pen.SetStyle(wxCROSS_HATCH);
   break;
 case 6:
   my_pen.SetStyle(wxDOT_DASH);
   break;
}

 backup_dc->SetPen(my_pen);

return(0);
}
/****************************************************************
* Set Jgc0.cwidth and Jgc0.cheight corresponding to expand0
****************************************************************/
int JLP_GDev_wxWID::Jgc0_set_font_size_from_device(double expand0)
{
int status = -1;
double g_dx, g_dy;

g_dx = (double)Jgc0.dev_width / (double)SCREEN_SIZE;
g_dy = (double)Jgc0.dev_height / (double)SCREEN_SIZE;

/* Size of characters: (for "mongo.h" ) */
// Default is expand1 = 1.2, FONT_SIZE_FOR_12PT= 12
// Checked in June 2017
Jgc0.cwidth = 0.8 * expand0 * FONT_SIZE_FOR_12PT  / g_dx;
Jgc0.cheight = 1.1 * expand0 * FONT_SIZE_FOR_12PT / g_dy;

return(status);
}
/*************************************************************
* Draw a label at the location x,y (mgo coordinates)
*
* INPUT:
* xstart, ystart: mgo coordinates
* OUTPUT:
* length in mgo coordinates
*****************************************************************/
double JLP_GDev_wxWID::label_device(const char *s, int xstart, int ystart,
                            double angle0, double expand0, int drawit)
{
double length = 0., d_x1, d_y1;
int ix, iy, slen;
double expanded_font;

// Test if class parameters have been initialized:
 if(initialized != 1234) return(-1);

 slen = strlen(s);

// Set Jgc0 font size from device:
 Jgc0_set_font_size_from_device(expand0);

// Font length in device coordinates:
 expanded_font = FONT_SIZE_FOR_12PT * expand0;

// Compute length (in SCREEN coordinates) and return if not display:
 length = Jgc0.cwidth * (double)(slen);

// Draw the string if drawit == 1
 if(drawit != 0 || length == 0) {

// Conversion from mgo (xstart,ystart) to device coordinates (d_x1, d_y1) :
  ConvMgoToDev(xstart, ystart, &d_x1, &d_y1);

  ix = NINT(d_x1);
  iy = NINT(d_y1);

// angle0 in degrees:
  if(angle0 == 0.)
   backup_dc->DrawText(s, ix, iy);
  else
   backup_dc->DrawRotatedText(s, ix, iy, angle0);

 }

return(length);
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
int JLP_GDev_wxWID::alloc_lut_device(int *mylut, int& private_lut, int& ncolors)
   {
    for(int i = 0; i < ncolors; i++) mylut[i] = i;
    private_lut = 1;
    return(0);
   }
/****************************************************************
* Load (r,g,b) values to private LUT
*
* Return the index j in mylut array of i^th color in (r, g, b) array
* In the case of Postscript or wxWidgets, it is an obvious relation: j=i
* but it is much more complex for X11 device
*****************************************************************/
int JLP_GDev_wxWID::load_lut_device(const int *r, const int *g, const int *b,
                                    int *mylut, int ncolors)
   {
    for(int i = 0; i < ncolors; i++) mylut[i] = i;
    return(0);
   }
/*************************************************************
* Set line colour (for further drawings...)
* r,g,b between 0. and 1.0
* WARNING: Black background, hence should draw in white
* or with bright colors!
*
* Called by SetPColor for mgo plots...
***************************************************************/
int JLP_GDev_wxWID::setrgbcolor(double r, double g, double b)
{
unsigned char rr, gg, bb;

// Test if class parameters have been initialized:
 if(initialized != 1234) return(-1);

 rr = (int)(r * 128.);
 gg = (int)(g * 128.);
 bb = (int)(b * 128.);

 wxgdev_settings1.pen_colour = wxColour(rr, gg, bb);
 SetPenColour_dc(wxgdev_settings1.pen_colour);

return(0);
}
/***************************************************************
* setdefaultcolor()
* Called by SetPColor for mgo plots...
*
* Default colour is now chosen by the popup menu
* (and initialized as black in InitializeAllParameters())
***************************************************************/
int JLP_GDev_wxWID::setdefaultcolor()
{
 wxgdev_settings1.pen_colour = wxgdev_settings1.pen_default_colour;
 SetPenColour_dc(wxgdev_settings1.pen_colour);
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
int JLP_GDev_wxWID::polygon_device(int xc, int yc, double expand, double angle,
                                   int nsides, int filled)
{
 wxBrush old_brush;
 wxPoint points[nsides];
 double dtheta, theta, d_xc, d_yc, g_dx, g_dy;
 static double old_ang;	/* old values of angle */
 int i;
 double xpsize, ypsize;        /* scale for points == g_dx*pdef*expand */
// Declared as static to avoid computing the polygon each time
 static int num = -1;         /* number of vertices used last time */
 static double old_xpsize,old_ypsize;	/* old values of xpsize, ypsize */
 static wxPoint vlist[MAX_POLYGON + 1];  /* vertices describing point */
 static int ix0, iy0; 	/* constant part of vertex[0].x, .y */

// Test if class parameters have been initialized:
 if(initialized != 1234) return(-1);

 if(nsides < 2) {
    gdev_line(xc, yc, xc, yc+1);
    return(0);
 }

g_dx = (double)Jgc0.dev_width / (double)SCREEN_SIZE;
g_dy = (double)Jgc0.dev_height / (double)SCREEN_SIZE;

 dtheta = 2*PI/nsides;
/* Compute length (in device coord.) of the polygon sides: */
 xpsize = 2. * (double)Mgc0.pdef * sin(dtheta/2) * expand * g_dx;
 ypsize = xpsize * g_dy / g_dx;

 if(nsides != num || angle != old_ang
    || xpsize != old_xpsize || ypsize != old_ypsize) {
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

// Those statements should be outside of the loop
// since the position of the absolute location should be changed each time!
    ConvMgoToDev(xc, yc, &d_xc, &d_yc);


    points[0].x = ix0 + NINT(d_xc);
    points[0].y = iy0 + NINT(d_yc);
    for(i = 1; i <= num; i++) {
      points[i].x = vlist[i].x + NINT(d_xc);
      points[i].y = vlist[i].y + NINT(d_yc);
      }


old_brush = backup_dc->GetBrush();
if(!filled) backup_dc->SetBrush(*wxTRANSPARENT_BRUSH);

backup_dc->DrawPolygon(num, points);

if(!filled) backup_dc->SetBrush(old_brush);

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
* gamma1, gamma_d; Reduction/magnification factors
* black_and_white : flag set to 1 if B&W is wanted (not yet implemented here)
* xstart, ystart: location of the bottom-left corner in mgo coordinates
******************************************************************************/
int JLP_GDev_wxWID::plot_image(int *image2, int nx2, int ny2, int idim,
                        int xstart, int ystart, int gamma1, int gamma_d,
                        int black_and_white)
/*****************************************************************************/
{
return(0);
}
/************************************************************************
* Pen colour
************************************************************************/
void JLP_GDev_wxWID::SetPenColour_dc(const wxColour wxC)
{
// For curves:
  backup_dc->SetPen(wxPen(wxC));
// For Text:
  backup_dc->SetTextForeground(wxC);

return;
}
/************************************************************************
* Background colour
************************************************************************/
void JLP_GDev_wxWID::SetBackgdColour_dc(const wxColour wxC)
{
// For curves:
  backup_dc->SetBackground(wxBrush(wxC));
// For Text:
  backup_dc->SetTextBackground(wxC);

return;
}

