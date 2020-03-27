/**
* \file jlp_gseg_gnome.h 
* \class JLP_Gseg_Gnome (Gnu Graphic Device) 
* \author JLP
* \date 13/12/2016
* \brief Definition of the gnome graphic drivers for JLP_Gseg 
*
* Class JLP_Gseg_Gnome ("Gnu Graphic Device")
* Definition of the graphic drivers 
* used in the JLP/GNU plotting package
*
* JLP
* Version 03/01/2015
**************************************************************************/
#ifndef __jlp_gseg_gnome__h                     /* sentry */
#define __jlp_gseg_gnome_h

#include <gnome.h>
#include <libgnomeprint/gnome-print.h>
#include <libgnomeprint/gnome-print-job.h>
#include <libgnomeprintui/gnome-print-dialog.h>
#include <libgnomeprintui/gnome-print-job-preview.h>

// Definition of some structures used here
#include "jlp_gnome_defs.h"

#include "jlp_gseg.h"            // JLP_Gseg class 

/* Definition of the JLP_GSeg_Gnome class
*/
class JLP_Gseg_Gnome : public JLP_Gseg 
{

public:

// Constructor:
 JLP_Gseg_Gnome(window_data_type *p_window_data_0);

// In "jlp_gseg_gnome.cpp"
void GSEG_gnome_InitializePlotCanvas(GnomeCanvasGroup *group,
                                     int height_menu_bar);
void GSEG_gnome_InitializePlotWindow(GnomeApp *window0, GtkWidget *canvas0);
void GSEG_gnome_SetPointersFromPlot(PangoFontDescription **font_text_0);
void GSEG_gnome_InitializeFonts ( void );

// Syntax of the following routines are defined in virtual class JLP_Gseg:
// In "jlp_gseg_gnome_draw.cpp"
  void GSEG_Clear();
  void GSEG_SetWindowLimits(const int x0, const int width0, 
                            const int y0, const int height0);
  int GSEG_GetWindowLimits(int *x0, int *width0, int *y0,
                           int *height0);
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
                          const unsigned int pixel_width );
  void GSEG_DrawCircle(double x, double y, const unsigned int diameter,
                       UINT32 fill_color_rgba, UINT32 outline_color_rgba,
                       const unsigned int pixel_width);
  void GSEG_DrawEllipse(double x1, double x2, double y1, double y2,
                       UINT32 fill_color_rgba, UINT32 outline_color_rgba,
                       const unsigned int pixel_width);
  void GSEG_DrawSquare(double x, double y, const unsigned int size,
                       UINT32 fill_color_rgba, UINT32 outline_color_rgba,
                       const unsigned int pixel_width);

// In "jlp_gseg_gnome_labels.cpp"
  void GSEG_SetFontFamily(char *font_name0,
                        double font_size_date_time0, double font_size_legend0,
                        double font_size_text0, double font_size_tick_labels0,
                        double font_size_axis_labels0, double font_size_title0);
  void GSEG_DrawTickLabels(char *string0, double x0, double y0,
                           const char *anchor0, UINT32 fill_color_rgba0, 
                           UINT32 canvas_bg_color0,
                           int raise_to_top0, double *x1, double *y1,
                           double *x2, double *y2);
  void GSEG_DrawLegendGetSize(char *string0, double *x1, double *y1, 
                              double *x2, double *y2);
  void GSEG_DrawLegend(char *string0, double x0, double y0,
                       const char *text_anchor0, UINT32 fill_color_rgba0,
                       UINT32 canvas_bg_color0, const int legend_nlines);
  int GSEG_DrawLabel(char *text_str, double x0, double y0, int nlines,
                     UINT32 fill_color_rgba, UINT32 canvas_bg_color0,
                     const char *text_anchor0, const int raise_to_top, 
                     double *x1, double *y1, double *x2, double *y2);

// In "jlp_gseg_gnome_image.cpp"
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

protected :

// In "jlp_gseg_gnome_labels.cpp" :
 int jlp_gnome_decode_anchor(const char *text_anchor0, GtkAnchorType *anchor1);
 void background_legend (char *legend_str, double height_lines,
                         double x1, double y1, UINT32 canvas_bg_color0);

// In "jlp_gseg_gnome_image.cpp" :
 int jlp_gnome_FromJLP2GnomePixbuf(JLP_TPixbuf *pixbuf0, GdkPixbuf **pixbuf1);
 void put_pixel(GdkPixbuf *pixbuf, int i, int j, UINT32 color);

private :

/* Window's parameters : */
 window_data_type   p_parent_window_data;
 GnomeCanvasGroup *parent_group;
 int parent_height_menu_bar;

/* Declare pango font-description pointers and font-size variables */
 PangoFontDescription *font_date_time1,
                     *font_legend1,
                     *font_text1,
                     *font_tick_labels1,
                     *font_axis_labels1,
                     *font_title1;
// Fonts
 double font_size_date_time1, font_size_legend1, font_size_text1;
 double font_size_tick_labels1, font_size_axis_labels1, font_size_title1;
 char font_name1[64];

/* Declare pixbufs */
 GdkPixbuf *pixbuf_window;

}; 

#endif    /* __jlp_gseg_gnome_h sentry */
