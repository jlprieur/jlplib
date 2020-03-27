/**
* \file jlp_gseg.h 
* \class JLP_Gseg (Gnu Graphic Device) 
* \author JLP
* \date 13/12/2016
* \brief Definition of the graphic drivers (as a virtual class)
*
* Abstract class JLP_Gseg ("Gnu Graphic Device")
* Definition of the graphic drivers (as a virtual class)
* used in the GNU plotting package
*
* JLP
* Version 03/01/2015
**************************************************************************/
#ifndef _jlp_gseg_h                     /* sentry */
#define _jlp_gseg_h

#include "jlp_gsegraf_defs.h"       // UINT32 ,JLP_CanvasPoints, etc 

/* Definition of the prototype of all JLP_GSeg classes
* (i.e. for postscript, gnome, wxwidgets, etc)
*/
class JLP_Gseg {

public:

// Abstract class should not have any definition of the constructors

// Basic plot functions: (either with gnome or wxwidgets libraries):
// In "jlp_gnome_draw.cpp" or "jlpwxgseg_draw.cpp"
virtual void GSEG_Clear() = 0;
virtual void GSEG_SetWindowLimits(const int x0, const int width0, 
                                  const int y0, const int height0) = 0;
virtual int GSEG_GetWindowLimits(int *x0, int *width0, int *y0,
                                 int *height0) = 0;
virtual void GSEG_DrawLine(JLP_CanvasPoints *points0, UINT32 fill_color_rgba0,
                           unsigned int line_width0) = 0;
virtual void GSEG_DrawLineWithArrow(JLP_CanvasPoints *points0,
                                    UINT32 fill_color_rgba0,
                                    unsigned int line_width0) = 0;
virtual void GSEG_DrawPolygon(JLP_CanvasPoints *points0, 
                              UINT32 fill_color_rgba0,
                              UINT32 outline_color_rgba0,
                              unsigned int line_width0) = 0;
virtual void GSEG_DrawRectangle(double x1, double x2, double y1, double y2,
                                const UINT32 fill_color_rgba0,
                                const UINT32 outline_color_rgba0,
                                const unsigned int pixel_width) = 0;
virtual void GSEG_DrawCircle(double x, double y, const unsigned int diameter,
                             UINT32 fill_color_rgba0, 
                             UINT32 outline_color_rgba0,
                             const unsigned int pixel_width) = 0;
virtual void GSEG_DrawEllipse(double x1, double x2, double y1, double y2,
                              UINT32 fill_color_rgba0, 
                              UINT32 outline_color_rgba0,
                              const unsigned int pixel_width) = 0;
virtual void GSEG_DrawSquare(double x, double y, const unsigned int size,
                             UINT32 fill_color_rgba0, 
                             UINT32 outline_color_rgba0,
                             const unsigned int pixel_width) = 0;

// In "jlp_gnome_image.cpp" or "jlpwxgseg_image.cpp"
virtual void GSEG_SetFontFamily(char *font_name0,
                        double font_size_date_time0, double font_size_legend0,
                        double font_size_text0, double font_size_tick_labels0,
                        double font_size_axis_labels0, 
                        double font_size_title0) = 0;

virtual void GSEG_DrawTickLabels(char *string0, double x0, double y0,
                                 const char *anchor0,
                                 UINT32 fill_color_rgba0, 
                                 UINT32 canvas_bg_color0,
                                 int raise_to_top0,
                                 double *x1, double *y1, 
                                 double *x2, double *y2) = 0;
virtual void GSEG_DrawLegendGetSize(char *string0, double *x1, double *y1, 
                                    double *x2, double *y2) = 0;
virtual void GSEG_DrawLegend(char *string0, double x0, double y0,
                             const char *text_anchor0,
                             UINT32 fill_color_rgba0, 
                             UINT32 canvas_bg_color0,
                             const int legend_nlines) = 0;
virtual int GSEG_DrawLabel(char *text_str, double x0, double y0, int nlines,
                           UINT32 fill_color_rgba, UINT32 canvas_bg_color0,
                           const char *text_anchor0, const int raise_to_top, 
                           double *x1, double *y1, double *x2, double *y2) = 0;
virtual int GSEG_PlotExtraImage(const char *image_filename, double x0, 
                                double y0, const char *text_anchor0) = 0;
virtual int GSEG_GetImageSize(const char *image_filename, int *width0,
                              int *height0) = 0;
virtual int GSEG_DrawBackgroundImage(const char *image_filename,
                                     const int image_style0, 
                                     double x0, double y0,
                                     const int width_plot0, 
                                     const int height_plot0,
                                     const double outer_radius0,
                                     const char *text_anchor0) = 0;
virtual int GSEG_DrawDPixbuf(JLP_DPixbuf *pixbuf0, double x0, double y0,
                             const char *anchor0) = 0;
virtual int GSEG_DrawTPixbuf(JLP_TPixbuf *pixbuf0, double x0, double y0,
                             const double angle0, const double dx0, 
                             const double dy0, const char *anchor0) = 0;

}; 

#endif    /* __jlp_gseg_h sentry */
