/********************************************************************
* jlp_gseg_wxwid.h
* declaration of JLP_Gseg_Wxwid class 
*
* Modified version of the GSEGrafix package (gsegrafix-1.0.6, sept. 2011)
* Copyright © 2008, 2009, 2010, 2011 Spencer A. Buckner
* http://savannah.gnu.org/projects/gsegrafix
*
* JLP
* Version 14/12/2016
********************************************************************/
#ifndef _jlp_gseg_wxwid_h_
#define _jlp_gseg_wxwid_h_

#include <stdio.h>
#include <math.h>
#include <string.h>

#include "wx/wx.h"
#include "wx/dcps.h"               // wxPostscriptDC
// In fact: #include "wx/generic/dcpsg.h"               // wxPostscriptDC

#include "jlp_gsegraf_defs.h"       // Definition of some structures used here
#include "jlp_gseg.h"               // Definition of JLP_Gseg class 

/* Definition of the JLP_GSeg_Gnome class
*/
class JLP_Gseg_Wxwid : public JLP_Gseg
{

public:

// Constructor:
   JLP_Gseg_Wxwid(int device_width0, int device_height0,
                  wxMemoryDC *backup_dc0, wxPostScriptDC *backup_pst_dc0);

 void GSEG_InitializePlot(int device_width0, int device_height0,
                          wxMemoryDC *backup_dc0, wxPostScriptDC *backup_pst_dc0);
 void InitPostscriptDC(wxPostScriptDC *backup_pst_dc0);

 void jlp_set_pen_color(UINT32 fill_color_rgba0, 
                        const unsigned int line_width0);
 void jlp_set_brush_color(UINT32 fill_color_rgba0); 
 void GSEG_ViewChangeAxisLimits();

// Syntax of the following routines are defined in virtual class JLP_Gseg:
// In "jlp_gseg_wxwid_draw.cpp"
  void GSEG_Clear();
  void GSEG_SetWindowLimits(const int x0, const int width0,
                            const int y0, const int height0);
  int GSEG_GetWindowLimits(int *x0, int *width0, int *y0, int *height0);
  int jlp_convert_wx_to_pst(int xi, int yi, int *xo, int *yo, int *in_frame);
  void GSEG_DrawLine(JLP_CanvasPoints *points0, UINT32 fill_color_rgba0,
                     unsigned int line_width0);
  void GSEG_DrawLineWithArrow(JLP_CanvasPoints *points0,
                              UINT32 fill_color_rgba0,
                              unsigned int line_width0);
  void GSEG_DrawPolygon(JLP_CanvasPoints *points0, UINT32 fill_color_rgba0,
                        UINT32 outline_color_rgba0,
                        unsigned int line_width0);
  void GSEG_DrawRectangle(double x1, double x2, double y1, double y2,
                          const UINT32 fill_color_rgba,
                          const UINT32 outline_color_rgba,
                          const unsigned int pixel_width);
  void GSEG_DrawCircle(double x, double y, const unsigned int diameter,
                       UINT32 fill_color_rgba, UINT32 outline_color_rgba,
                       const unsigned int pixel_width);
  void GSEG_DrawEllipse(double x1, double x2, double y1, double y2,
                       UINT32 fill_color_rgba, UINT32 outline_color_rgba,
                       const unsigned int pixel_width);
  void GSEG_DrawSquare(double x, double y, const unsigned int size,
                       UINT32 fill_color_rgba, UINT32 outline_color_rgba,
                       const unsigned int pixel_width);

// In "jlp_gseg_wxwid_labels.cpp"
  void GSEG_SetFontFamily(char *font_name0,
                        double font_size_date_time0, double font_size_legend0,
                        double font_size_text0, double font_size_tick_labels0,
                        double font_size_axis_labels0, double font_size_title0);
  void GSEG_DrawTickLabels(char *string0, double x0, double y0,
                           const char *anchor0, UINT32 fill_color_rgba0,
                           UINT32 canvas_bg_color0,
                           int raise_to_top0, double *x1, double *y1,
                           double *x2, double *y2);
  int jlp_wxwid_draw_label(wxString *str0, const char *text_anchor0,      
                           double x0, double y0, UINT32 canvas_bg_color0, 
                           const int raise_to_top0, 
                           double *x1, double *y1, double *x2, double *y2);
  int jlp_wxwid_draw_legend(wxString *str0, const char *text_anchor0,      
                            double x0, double y0, UINT32 canvas_bg_color0, 
                            const int legend_nlines);
  int jlp_set_font(char *font_name, const unsigned int font_size, 
                   UINT32 color_rgba0);
  void jlp_background_text(wxString *string0, int x1, int y1,
                           UINT32 canvas_bg_color0);
  void GSEG_DrawLegendGetSize(char *string0, double *x1, double *y1, 
                              double *x2, double *y2);
  void GSEG_DrawLegend(char *string0, double x0, double y0,
                       const char *text_anchor0, UINT32 fill_color_rgba0,
                       UINT32 canvas_bg_color0, const int legend_nlines);
  int GSEG_DrawLabel(char *text_str, double x0, double y0, int nlines,
                     UINT32 fill_color_rgba, UINT32 canvas_bg_color0,
                     const char *text_anchor0, const int raise_to_top,
                     double *x1, double *y1, double *x2, double *y2);
  int jlp_wxwid_decode_anchor(const char *text_anchor0, const int width0, 
                              const int height0, int *dx0, int *dy0);

// In "jlp_gseg_wxwid_image.cpp"
  int jlp_wxwid_DrawPixbuf(UINT32 *array_rgba0, int nx0, int ny0,
                     double x0, double y0, const char *text_anchor0);
  int GSEG_PlotExtraImage(const char *image_filename, double x0, double y0,
                          const char *text_anchor0);
  int GSEG_GetImageSize(const char *image_filename, int *width0,
                        int *height0);
  int GSEG_DrawBackgroundImage(const char *image_filename,
                               const int image_style0, double x0, double y0,
                               const int width_plot0, const int height_plot0,
                               const double outer_radius0,
                               const char *text_anchor0);
  int GSEG_DrawDPixbuf(JLP_DPixbuf *pixbuf0, double x0, double y0,
                      const char *anchor0);
  int GSEG_DrawTPixbuf(JLP_TPixbuf *pixbuf0, double x0, double y0,
                       const double angle0, const double dx0, const double dy0,
                       const char *anchor0);

// Backup Device Context (accessible to public, for drawing)
   wxMemoryDC *gseg_backup_dc;

// Backup Device Context (accessible to public, for drawing)
   wxPostScriptDC *gseg_backup_pst_dc;

private:
 int xx1, yy1, window_width1, window_height1;
 int pst_xx1, pst_yy1, pst_width1, pst_height1;
 double x_wx_to_pst_scale1, y_wx_to_pst_scale1;

// Fonts
 double font_size_date_time1, font_size_legend1, font_size_text1;
 double font_size_tick_labels1, font_size_axis_labels1, font_size_title1;
 char font_name1[128];
};

// jlp_wxgseg_utils.cpp
void JLP_ErrorDialog(const char *error_message);

// jlp_gseg_wxwid_draw.cpp
int jlp_decode_color_uint32(UINT32 color_rgba0,
                            unsigned char *red0, unsigned char *green0,
                            unsigned char *blue0, unsigned char *alpha0);

#endif
