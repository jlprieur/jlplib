/*************************************************************************
* JLP_wxGseg_Canvas Class (for curves)
* plot routines
* needed by the virtual definition of JLP_cgDev
*
* JLP
* Version 09/06/2015
**************************************************************************/
#include "jlp_splot_def.h"  // BITMAP_WIDTH, etc
#include "jlp_cgdev.h"
#include "jlp_wx_gscanvas.h"
#include "jlp_wx_cursor.h"
#include "jlp_ctime.h"   // JLP_CTIME

/*
#define DEBUG
*/
#ifndef PI 
#define PI 3.1415926536
#endif

#define MAX_POLYGON 20          /* maximum corners for PTYPE 3 points */
/* JLP 91 200 --> 2000 */
#define START_NVEC 2000         /* starting value for max_nvec */
#define FONT_SIZE   12.         /* Size of fonts */

/*************************************************************
* Draw a line
*
* INPUT:
* x1, y1: coordinates of starting point (mgo coordinates)
* x2, y2: coordinates of ending point (mgo coordinates)
* lwidth: line width (NOT USED YET)
*
* OUTPUT:
* Mgc0.lwidth updated
************************************************************/
int JLP_wxGseg_Canvas::line_device(int x1, int y1, int x2, int y2, int lwidth)
{
float d_x1, d_y1, d_x2, d_y2;

// Test if curve mode is active:
 if(initialized != 1234) return(-1);

// Write to MetaFile:
#if USE_METAFILE
  if(MetaFileIsOpened)
  fprintf(fp_MetaFile, "line_device(%d,%d,%d,%d,%d)\n", 
          x1, y1, x2, y2, lwidth);
#endif

// Conversion from mgo to device coordinates:
  conv_mgo_to_dev(x1, y1, d_x1, d_y1);
  conv_mgo_to_dev(x2, y2, d_x2, d_y2);
  backup_dc->DrawLine( (int)d_x1, (int)d_y1, (int)d_x2, (int)d_y2);

return(0);
}

/*************************************************************
* To set line attributes to a graphic context 
*
* lwidth = width of the line
* ltype = 0 solid line, 1=dashed line, 2=dotted line, etc
*************************************************************/
int JLP_wxGseg_Canvas::SetLineWidthAndType(int lwidth, int ltype)
{
wxPen my_pen;

// Test if curve mode is active:
 if(initialized != 1234) return(-1);

// Write to MetaFile:
#if USE_METAFILE
 if(MetaFileIsOpened)
   fprintf(fp_MetaFile, "SetLineWidthAndType(%d,%d)\n", lwidth, ltype);
#endif

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

/*************************************************************
* Draw a label at the location x,y (mgo coordinates)
*
* INPUT:
* xstart, ystart: mgo coordinates
*
* OUTPUT:
* length in mgo coordinates
*****************************************************************/
float JLP_wxGseg_Canvas::label_device(const char *s, int xstart, int ystart, 
                            float angle1, float expand1, int drawit)
{
float length = 0.;
float d_x, d_y;
int ix, iy, slen;
float expanded_font;

// Test if curve mode is active:
 if(initialized != 1234) return(-1);

// Write to MetaFile:
#if USE_METAFILE
 if(MetaFileIsOpened)
   if(drawit) fprintf(fp_MetaFile, "label_device(%s,%d,%d,%.2f,%.2f,1)\n", 
                      s, xstart, ystart, angle1, expand1);
#endif

slen = strlen(s);

/* Font length in device coordinates */
expanded_font = FONT_SIZE * expand1;

/* Size of characters: (for "mongo.h" ) */
Jgc0.cwidth = 0.6 * expanded_font / Jgc0.g_dx;
Jgc0.cheight = 0.5 * expanded_font / Jgc0.g_dy;

/* Compute length (in SCREEN coordinates) and return if not display: */
length = Jgc0.cwidth * (float)(slen);

/* Draw the string if drawit == 1 */
 if(drawit != 0 || length == 0) { 

// Conversion from mgo (xstart,ystart) to device coordinates (d_x, d_y) :
  conv_mgo_to_dev(xstart, ystart, d_x, d_y);
  ix = NINT(d_x);
  iy = NINT(d_y);

  if(angle1 == 0.) backup_dc->DrawText(s, ix, iy);
// angle1 in degrees:
   else backup_dc->DrawRotatedText(s, ix, iy, angle1);
 }

return(length);
}

/*************************************************************
* Set line colour (for further drawings...)
* r,g,b between 0. and 1.0 
*
* WARNING: When black background should draw in white or with bright colors!
*
* Called by SetPColor for mgo plots...
***************************************************************/
int JLP_wxGseg_Canvas::setrgbcolor(float r, float g, float b)
{
unsigned char rr, gg, bb;

 if(initialized != 1234) return(-1);

 rr = (int)(r * 255.);
 gg = (int)(g * 255.);
 bb = (int)(b * 255.);

// Write to MetaFile:
#if USE_METAFILE
 if(MetaFileIsOpened)
    fprintf(fp_MetaFile, "setrgbcolor(%.2f,%.2f,%.2f)\n", r, g, b);
#endif

 cgdev_settings1.pen_colour = wxColour(rr, gg, bb);
 SetPenColour_dc(cgdev_settings1.pen_colour);

return(0);
}
/***************************************************************
* setdefaultcolor()
* Called by SetPColor for mgo plots...
*
* Default colour is now chosen by the popup menu 
* (and initialized as black in InitializeAllParameters())
***************************************************************/
int JLP_wxGseg_Canvas::setdefaultcolor()
{
 cgdev_settings1.pen_colour = cgdev_settings1.pen_default_colour; 
 SetPenColour_dc(cgdev_settings1.pen_colour);
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
int JLP_wxGseg_Canvas::polygon(int xc, int yc, float expand, float angle, 
                             int nsides, int filled)
{
 wxBrush old_brush;
 wxPoint points[nsides];
 float dtheta, theta, d_xc, d_yc;
 static float old_ang;	/* old values of angle */
 int i; 
 float xpsize, ypsize;        /* scale for points == g_dx*pdef*expand */
// Declared as static to avoid computing the polygon each time
 static int num = -1;         /* number of vertices used last time */
 static float old_xpsize,old_ypsize;	/* old values of xpsize, ypsize */
 static wxPoint vlist[MAX_POLYGON + 1];  /* vertices describing point */
 static int ix0, iy0; 	/* constant part of vertex[0].x, .y */

// Test if curve mode is active:
 if(initialized != 1234) return(-1);

// Write to MetaFile:
#if USE_METAFILE
 if(MetaFileIsOpened)
   fprintf(fp_MetaFile, "polygon(%d,%d,%.2f,%.2f,%d,%d)\n", 
           xc, yc, expand, angle, nsides, filled);
#endif

 if(nsides < 2) {
    line(xc, yc, xc, yc+1);
    return(0);
 }

 dtheta = 2*PI/nsides;
/* Compute length (in device coord.) of the polygon sides: */
 xpsize = 2. * (float)Mgc0.pdef * sin(dtheta/2) * expand * Jgc0.g_dx;
 ypsize = xpsize * Jgc0.g_dy / Jgc0.g_dx;

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
      conv_mgo_to_dev(xc, yc, d_xc, d_yc);

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
