/*******************************************************************************
*
* jlp_gseg_wxwid_draw.cpp
* Interface with wxWidgets 
*
* JLP
* Version 25/11/2016
*******************************************************************************/
#include <math.h>
#include "jlp_gsegraf_defs.h"

#include "jlp_gseg_wxwid.h"     // JLP_Gseg_Wxwid class

static int jlp_print_uint32(UINT32 my_uint0);

/*************************************************************************
* Specify window minimum and maximum values
*
*************************************************************************/
void JLP_Gseg_Wxwid::GSEG_SetWindowLimits(const int x0, const int width0,
                                         const int y0, const int height0)
{
  xx1 = x0;
  window_width1 = width0;
  yy1 = y0;
  window_height1 =  height0;

return;
}
/*************************************************************************
* Get window minimum and maximum values
*
*************************************************************************/
int JLP_Gseg_Wxwid::GSEG_GetWindowLimits(int *x0, int *width0, int *y0,
                                         int *height0)
{
  *x0 = xx1;
  *width0 = window_width1;
  *y0 = yy1;
  *height0 = window_height1;

return(0);
}
/*************************************************************************
* Conversion fo coordinates from wxwidgets screen coords to postscript coords.
*
* INPUT:
*  xi, yi
*
* OUTPUT:
*  xo, yo
*
*************************************************************************/
int JLP_Gseg_Wxwid::jlp_convert_wx_to_pst(int xi, int yi, int *xo, int *yo, int *in_frame)
{
double x_scale, y_scale;
*in_frame = 1;
*xo = 0;
*yo = 0;
if((xi < xx1) || (xi > window_width1)
    || (yi < yy1) || (yi > window_height1)) *in_frame = 0;

x_scale = (double)pst_width1 / (double)window_width1;
y_scale = (double)pst_height1 / (double)window_height1;
*xo = (double)(xi - xx1) * x_scale + pst_xx1;
*yo = (double)(yi - yy1) * y_scale + pst_yy1;

/* DEBUG
printf("jlp_convert_wx_to_pst/ scale=%f %f and %d %d %d %d\n", 
        x_scale, y_scale, xi, yi, *xo, *yo);
*/
return(0);
}
/**************************************************************************
* JLP interface with wxwidgets: straight line 
**************************************************************************/
void JLP_Gseg_Wxwid::GSEG_DrawLine(JLP_CanvasPoints *points0, 
                                   UINT32 outline_color_rgba0, 
                                   unsigned int line_width0 )
{
/* Declare variables */
int xstart, ystart, xend, yend;
int x_start, y_start, x_end, y_end, in_frame;
int i, npts;
UINT32 fill_color0;

npts = points0->num_points;

/* Check linewidth and number of points */
if((line_width0 == 0) || (npts == 0)) return;

// Transparent brush:
fill_color0 = 0xFFFFFF00;
jlp_set_brush_color(fill_color0);

// Set pen color and width
jlp_set_pen_color(outline_color_rgba0, line_width0);

/* Draw line */
xstart = points0->coords[0];
ystart = points0->coords[1];
for(i = 1; i < npts; i++) {
  xend = points0->coords[2 * i];
  yend = points0->coords[2 * i + 1];
  gseg_backup_dc->DrawLine(xstart, ystart, xend, yend);
  if(gseg_backup_pst_dc != NULL) {
    jlp_convert_wx_to_pst(xstart, ystart, &x_start, &y_start, &in_frame);
    jlp_convert_wx_to_pst(xend, yend, &x_end, &y_end, &in_frame);
    gseg_backup_pst_dc->DrawLine(x_start, y_start, x_end, y_end);
  }
  xstart = xend;
  ystart = yend;
  }

return;
}
/**************************************************************************
* Set pen color and width
*
**************************************************************************/
void JLP_Gseg_Wxwid::jlp_set_pen_color(UINT32 color_rgba0,
                                       const unsigned int line_width0) 
{
unsigned char red0, green0, blue0, alpha0; 

jlp_decode_color_uint32(color_rgba0, &red0, &green0, &blue0, &alpha0);

// wxALPHA_OPAQUE = 255
// wxALPHA_TRANSPARENT = 0
// DEBUG:
// alpha0 = wxALPHA_OPAQUE;

/* Settings */
// wxPENSTYLE_DOT, wxPENSTYLE_LONG_DASH, wxPENSTYLE_SHORT_DASH, 
// wxPENSTYLE_DOT_DASH
gseg_backup_dc->SetPen(wxPen(wxColour(red0, green0, blue0, alpha0), 
                       line_width0, wxPENSTYLE_SOLID));
if(gseg_backup_pst_dc != NULL) {
   gseg_backup_pst_dc->SetPen(wxPen(wxColour(red0, green0, blue0, alpha0), line_width0, 
                                             wxPENSTYLE_SOLID));
  }

return;
}
/**************************************************************************
* Set brush color and width
*
**************************************************************************/
void JLP_Gseg_Wxwid::jlp_set_brush_color(UINT32 color_rgba0)
{
unsigned char red0, green0, blue0, alpha0; 

jlp_decode_color_uint32(color_rgba0, &red0, &green0, &blue0, &alpha0);

/* Settings */
// wxBRUSHSTYLE_FDIAGONAL_HATCH, wxBRUSHSTYLE_CROSSDIAG_HATCH
// wxBRUSHSTYLE_SOLID (default)
// JLP 2017: problem of transparent brush solved this way:
if(alpha0 == 0) { 
  gseg_backup_dc->SetBrush(*wxTRANSPARENT_BRUSH);
  if(gseg_backup_pst_dc != NULL) gseg_backup_pst_dc->SetBrush(*wxTRANSPARENT_BRUSH);
} else {
  gseg_backup_dc->SetBrush(wxBrush(wxColour(red0, green0, blue0, alpha0), 
                           wxBRUSHSTYLE_SOLID));
  if(gseg_backup_pst_dc != NULL) gseg_backup_pst_dc->SetBrush(
                                           wxBrush(wxColour(red0, green0, blue0, alpha0), 
                                           wxBRUSHSTYLE_SOLID));
}
/*
gseg_backup_dc->SetBrush(wxBrush(wxColour(red0, green0, blue0, alpha0), 
                         wxBRUSHSTYLE_CROSSDIAG_HATCH));
*/

return;
}

/**************************************************************************
* Set pen color and width
*
**************************************************************************/
int jlp_decode_color_uint32(UINT32 color_rgba0,
                            unsigned char *red0, unsigned char *green0,
                            unsigned char *blue0, unsigned char *alpha0)
{
UINT32 rr, gg, bb, aa;

// >> : right shift, which uses padding with ones or zeroes on the left
// (depending on the implementation)
#ifdef WORDS_BIGENDIAN
   aa = ((color_rgba0 & 0xFF000000) >> 24) ;
   bb = ((color_rgba0 & 0x00FF0000) >> 16) ;
   gg = ((color_rgba0 & 0x0000FF00) >> 8) ;
   rr = (color_rgba0 & 0x000000FF);
#else
   rr = ((color_rgba0 & 0xFF000000) >> 24) ;
   gg = ((color_rgba0 & 0x00FF0000) >> 16) ;
   bb = ((color_rgba0 & 0x0000FF00) >> 8) ;
   aa = (color_rgba0 & 0x000000FF);
#endif

rr = rr & 0x000000FF;
gg = gg & 0x000000FF;
bb = bb & 0x000000FF;
aa = aa & 0x000000FF;

*red0 = (unsigned char)rr;
*green0 = (unsigned char)gg;
*blue0 = (unsigned char)bb;
*alpha0 = (unsigned char)aa;

/*** DEBUG
printf("pen_color: rbga: %d \n", color_rgba0); 
jlp_print_uint32(color_rgba0); 
jlp_print_uint32(rr); 
jlp_print_uint32(gg); 
jlp_print_uint32(bb); 
jlp_print_uint32(aa); 
printf("jlp_decode_color_uint32/color_rgba: (r,b,g,a) = %d %d %d %d\n", 
     (int)*red0, (int)*green0, (int)*blue0, (int)*alpha0);
****/

return(0);
}

/**************************************************************************
* print binary UINT32 
**************************************************************************/
static int jlp_print_uint32(UINT32 my_uint0) 
{
  int i, k;
 
  printf(" %d in binary is: ", my_uint0);
 
  for (i = 31; i >= 0; i--)
  {
    k = my_uint0 >> i;
 
    if (k & 1)
      printf("1");
    else
      printf("0");
  }
 
  printf("\n");
 
  return 0;
}
/**************************************************************************
* JLP interface with gnome: straight line (GNOME_TYPE_CANVAS_LINE)
**************************************************************************/
void JLP_Gseg_Wxwid::GSEG_DrawLineWithArrow(JLP_CanvasPoints *points0, 
                                            UINT32 outline_color_rgba0, 
                                            unsigned int line_width0 )
{
/* Declare variables */
double xstart, ystart, xend, yend, dx, dy;
int x1, x2, y1, y2;
int x_1, x_2, y_1, y_2, x_end, y_end;
double angle0, angle1, length0;
int i, npts, in_frame;
UINT32 fill_color0;

npts = points0->num_points;

/* Check linewidth and number of points */
 if((line_width0 == 0) || (npts < 2)) return;

// Transparent brush:
fill_color0 = 0xFFFFFF00;
jlp_set_brush_color(fill_color0);

// Set pen color and width=2
jlp_set_pen_color(outline_color_rgba0, 2);

/* Draw an arrow at the end */
xstart = points0->coords[0];
ystart = points0->coords[1];
xend = points0->coords[2*npts - 2];
yend = points0->coords[2*npts - 1];
dx = xend - xstart;
dy = yend - ystart;
angle0 = atan2(dy, dx);

length0 = 8.;
angle1 = 0.3;

dx = length0 * cos(angle0 - angle1);
x1 = xend - dx;

dx = length0 * cos(angle0 + angle1);
x2 = xend - dx;

dy = length0 * sin(angle0 - angle1);
if(dy < 0.) dy *= -1;
y1 = yend + dy;

dy = length0 * sin(angle0 + angle1);
if(dy < 0.) dy *= -1;
y2 = yend + dy;

gseg_backup_dc->DrawLine(x1, y1, xend, yend);
gseg_backup_dc->DrawLine(x2, y2, xend, yend);
if(gseg_backup_pst_dc != NULL) {
   jlp_convert_wx_to_pst(x1, y1, &x_1, &y_1, &in_frame);
   jlp_convert_wx_to_pst(x2, y2, &x_2, &y_2, &in_frame);
   jlp_convert_wx_to_pst(xend, yend, &x_end, &y_end, &in_frame);
   gseg_backup_pst_dc->DrawLine(x_1, y_1, x_end, y_end);
   gseg_backup_pst_dc->DrawLine(x_2, y_2, x_end, y_end);
  }

// Set pen color and width
jlp_set_pen_color(outline_color_rgba0, line_width0);
// Draw the main line:
GSEG_DrawLine(points0, outline_color_rgba0, line_width0);

return;
}
/**************************************************************************
* JLP interface with gnome: polygon 
**************************************************************************/
void JLP_Gseg_Wxwid::GSEG_DrawPolygon(JLP_CanvasPoints *points0, 
                                      UINT32 fill_color_rgba0, 
                                      UINT32 outline_color_rgba0, 
                                      unsigned int line_width0)
{
wxPoint *points1, *points_1;
double xstart, ystart, xend, yend;
double x_start, y_start, x_end, y_end;
int i, npts, in_frame;

npts = points0->num_points;

// Check the number of points
 if(npts == 0) return;

// Set brush color
jlp_set_brush_color(fill_color_rgba0);

// Set pen color and width
jlp_set_pen_color(outline_color_rgba0, line_width0);

// Draw polygon 
/*
xstart = points0->coords[0];
ystart = points0->coords[1];
for(i = 1; i < npts; i++) {
  xend = points0->coords[2 * i]; 
  yend = points0->coords[2 * i + 1];
  gseg_backup_dc->DrawLine(xstart, ystart, xend, yend);
  if(gseg_backup_pst_dc != NULL) {
   jlp_convert_wx_to_pst(xstart, ystart, &x_start, &y_start, &in_frame);
   jlp_convert_wx_to_pst(xend, yend, &x_end, &y_end, &in_frame);
   gseg_backup_pst_dc->DrawLine(x_start, y_start, x_end, y_end);
  }
  xstart = xend;
  ystart = yend;
  }
*/
 points1 = new wxPoint[npts];
 for(i = 0; i < npts; i++) {
   points1[i].x = points0->coords[2 * i];
   points1[i].y = points0->coords[2 * i + 1];
   }
// The current pen is used for drawing the outline, 
// and the current brush for filling the shape. 
// Using a transparent brush suppresses filling
 gseg_backup_dc->DrawPolygon(npts, points1, 0, 0, wxODDEVEN_RULE);
// gseg_backup_dc->DrawPolygon(npts, points1, 0, 0, wxWINDING_RULE);

if(gseg_backup_pst_dc != NULL) {
  points_1 = new wxPoint[npts];
 for(i = 0; i < npts; i++) {
   jlp_convert_wx_to_pst(points1[i].x, points1[i].y, &(points_1[i].x), &(points_1[i].y),
                         &in_frame);
   }
 gseg_backup_pst_dc->DrawPolygon(npts, points1, 0, 0, wxODDEVEN_RULE);
// Free memory
 delete[] points_1;
 }

// Free memory
 delete[] points1;
 return;
}
/************************************************************************
*
************************************************************************/
void JLP_Gseg_Wxwid::GSEG_DrawRectangle(double x1, double x2,
                                        double y1, double y2,
                                        const UINT32 fill_color_rgba0,
                                        const UINT32 outline_color_rgba0,
                                        const unsigned int line_width0 )
{
int width0, height0;
int x_1, x_2, y_1, y_2, in_frame;

// Called with transparent brush:
 jlp_set_brush_color(fill_color_rgba0);

// Set pen color and width
jlp_set_pen_color(outline_color_rgba0, line_width0);

/* Draw rectangle */
 width0 = x2 - x1; height0 = y2 - y1;
 gseg_backup_dc->DrawRectangle(x1, y1, width0, height0);

// Postscript:
if(gseg_backup_pst_dc != NULL) {
   jlp_convert_wx_to_pst(x1, y1, &x_1, &y_1, &in_frame);
   jlp_convert_wx_to_pst(x2, y2, &x_2, &y_2, &in_frame);
   width0 = x_2 - x_1; height0 = y_2 - y_1;
   gseg_backup_pst_dc->DrawRectangle(x_1, y_1, width0, height0);
  }

return;
}
/************************************************************************
*
* INPUT:
*  x, y : center coordinates
*  size : radius
************************************************************************/
void JLP_Gseg_Wxwid::GSEG_DrawCircle(double x0, double y0, 
                                     const unsigned int diameter0,
                                     UINT32 fill_color_rgba0, 
                                     UINT32 outline_color_rgba0,
                                     const unsigned int line_width0)
{
int x_0, y_0, diameter_0, in_frame;

/* Draw circle */
if ( diameter0 == 0 ) return;

// Set brush color
 jlp_set_brush_color(fill_color_rgba0);


// Set pen color and width
 jlp_set_pen_color(outline_color_rgba0, line_width0);

/* Draw circle */
gseg_backup_dc->DrawCircle(x0, y0, diameter0 / 2);

// Postscript:
if(gseg_backup_pst_dc != NULL) {
   jlp_convert_wx_to_pst(x0, y0, &x_0, &y_0, &in_frame);
   diameter_0 = diameter0 * (double)pst_width1 / (double)window_width1;
   gseg_backup_dc->DrawCircle(x_0, y_0, diameter_0 / 2);
  }
return;
}
/************************************************************************
*
************************************************************************/
void JLP_Gseg_Wxwid::GSEG_DrawEllipse(double x1, double x2, double y1,
                                      double y2, UINT32 fill_color_rgba0,
                                      UINT32 outline_color_rgba0,
                                      unsigned int line_width0)
{
double x0, y0, width0, height0;
int x_0, y_0, width_0, height_0, in_frame;

// Set brush color
jlp_set_brush_color(outline_color_rgba0);

// Set pen color and width
jlp_set_pen_color(fill_color_rgba0, line_width0);

// Top left corner, width and height:
 x0 = x1;
 if(x2 < x1) x0 = x2;
 y0 = y1;
 if(y2 < y1) y0 = y2;
 width0 = x2 - x1;
 if(width0 < 0) width0 *= -1;
 height0 = y2 - y1;
 if(height0 < 0) height0 *= -1;

/* Draw ellipse */
 gseg_backup_dc->DrawEllipse(wxRect(x0, y0, width0, height0));

// Postscript:
if(gseg_backup_pst_dc != NULL) {
   jlp_convert_wx_to_pst(x0, y0, &x_0, &y_0, &in_frame);
   width_0 = width0 * (double)pst_width1 / (double)window_width1;
   height_0 = height0 * (double)pst_height1 / (double)window_height1;
   gseg_backup_dc->DrawEllipse(wxRect(x_0, y_0, width_0, height_0));
  }
return;
}
/************************************************************************
*
* INPUT:
*  x0, y0 : center coordinates
*  size0 : side of the square 
************************************************************************/
void JLP_Gseg_Wxwid::GSEG_DrawSquare(double x0, double y0, 
                                     const unsigned int size0,
                                     UINT32 fill_color_rgba0, 
                                     UINT32 outline_color_rgba0,
                                     const unsigned int line_width0)
{
double x1, y1, x2, y2;

x1 = x0 - (double)size0 / 2.;
y1 = y0 - (double)size0 / 2.;
x2 = x1 + (double)size0;
y2 = y1 + (double)size0;

 GSEG_DrawRectangle(x1, y1, x2, y2, fill_color_rgba0, 
                    outline_color_rgba0, line_width0);
}
