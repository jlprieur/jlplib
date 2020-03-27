/*****************************************************************************
*
* jlp_gseg_wxwid_labels.cpp
* Label routines of JLP_Gseg_Wxwid class 
* Interface with wxWidgets
*
* JLP
* Version 21/01/2017
*****************************************************************************/
#include <math.h>
#include "jlp_gsegraf_defs.h"

#include "jlp_gseg_wxwid.h"     // Gseg_Wxwid class

/*****************************************************************************
*
*****************************************************************************/
void JLP_Gseg_Wxwid::GSEG_SetFontFamily(char *font_name0,
                 double font_size_date_time0, double font_size_legend0,
                 double font_size_text0, double font_size_tick_labels0,
                 double font_size_axis_labels0, double font_size_title0)
{

// Save new settings to private variables:
strcpy(font_name1, font_name0);

font_size_date_time1 = font_size_date_time0;
font_size_legend1 = font_size_legend0;
font_size_text1 = font_size_text0;
font_size_tick_labels1 = font_size_tick_labels0;
font_size_axis_labels1 = font_size_axis_labels0;
font_size_title1 = font_size_title0;

return;
}
/**************************************************************************
* JLP interface with gnome: text (GNOME_TYPE_CANVAS_TEXT)
* INPUT:
*   char *string0 : string to be displayed on the label
*   double x0, y0 : coordinates of starting output label
*   char *text_anchor0 : centering mode of the text in the label
*   UINT32 fill_color_rgba0 : color of the label 
*   int raise_to_top : flag set to one if label has to be raised to top
*
* Private variables:
*   font_size_tick_labels1
*
* OUTPUT:
*   double x1, y1, x2, y2 : coordinates of the output label box
**************************************************************************/
void JLP_Gseg_Wxwid::GSEG_DrawTickLabels(char *string0, double x0, double y0, 
                                         const char *text_anchor0,
                                         UINT32 fill_color_rgba0, 
                                         UINT32 canvas_bg_color0, 
                                         int raise_to_top0, 
                                         double *x1, double *y1, 
                                         double *x2, double *y2) 
{
wxString str0;

 str0 = wxString(string0);

// Set font color and size 
jlp_set_font(font_name1, font_size_tick_labels1, fill_color_rgba0);

// Draw the string according to text_anchor0
jlp_wxwid_draw_label(&str0, text_anchor0, x0, y0, canvas_bg_color0,
                     raise_to_top0, x1, y1, x2, y2);

return;
}
/**************************************************************************
* Draw the string according to text_anchor0
**************************************************************************/
int JLP_Gseg_Wxwid::jlp_wxwid_draw_label(wxString *str0, 
                                         const char *text_anchor0, 
                                         double x0, double y0, 
                                         UINT32 canvas_bg_color0,
                                         const int raise_to_top0, 
                                         double *x1, double *y1, 
                                         double *x2, double *y2) 
{
int dx0, dy0, text_width0, text_height0;
int xx1, yy1;
int xx_1, yy_1, in_frame;

// Get size of label on the device:
 gseg_backup_dc->GetTextExtent(*str0, &text_width0, &text_height0);

// Determine the offset dx0, dy0 :
jlp_wxwid_decode_anchor(text_anchor0, text_width0, text_height0,
                         &dx0, &dy0);

// Compute label size:
 *x1 = x0 + dx0;
 *y1 = y0 + dy0;
 *x2 = *x1 + text_width0;
 *y2 = *y1 + text_height0;
 xx1 = (int)*x1;
 yy1 = (int)*y1;

// Raise to top:
 if(raise_to_top0 == 1) {
/* Draw text background rectangles */
// DEBUG opaque red: background rectangle not working yet...
//   canvas_bg_color0 = 0xFF0000FF;
   jlp_background_text(str0, xx1, yy1, canvas_bg_color0);
  }

/* Draw label */
 gseg_backup_dc->DrawText(*str0, xx1, yy1);

// Postscript:
if(gseg_backup_pst_dc != NULL) {
  jlp_convert_wx_to_pst(xx1, yy1, &xx_1, &yy_1, &in_frame);
  gseg_backup_pst_dc->DrawText(*str0, xx_1, yy_1);
  }

/*** DEBUG
if(raise_to_top0 == 1)
printf("jlp_wxwid_draw_label/label=>%s< (anchor=%s) width=%d height=%d dx=%d dy=%d x=%d y=%d raise=%d\n",
       (const char *)str0->mb_str(), text_anchor0, 
       text_width0, text_height0, dx0, dy0, x1, y1, raise_to_top0);
****/

return(0);
}
/**************************************************************************
* Draw the legend string according to text_anchor0
**************************************************************************/
int JLP_Gseg_Wxwid::jlp_wxwid_draw_legend(wxString *str0,
                                         const char *text_anchor0,
                                         double x0, double y0,
                                         UINT32 canvas_bg_color0,
                                         const int legend_nlines0)
{
int dx0, dy0, text_width0, text_height0;
int x1, y1, x_1, y_1, in_frame;

// Get size of label on the device:
 gseg_backup_dc->GetTextExtent(*str0, &text_width0, &text_height0);

// Determine the offset dx0, dy0 :
 jlp_wxwid_decode_anchor(text_anchor0, text_width0, text_height0,
                         &dx0, &dy0);

// Compute label size:
 x1 = x0 + dx0;
 y1 = y0 + dy0;

// Draw legend background 
 jlp_background_text(str0, x1, y1, canvas_bg_color0);

// Draw label on top
 gseg_backup_dc->DrawText(*str0, x1, y1);

// Postscript:
if(gseg_backup_pst_dc != NULL) {
  jlp_convert_wx_to_pst(x1, y1, &x_1, &y_1, &in_frame);
  gseg_backup_pst_dc->DrawText(*str0, x_1, y_1);
  }


/*** DEBUG
printf("label=%s (%s) width=%d height=%d dx=%d dy=%d x=%d y=%d\n",
       (const char *)str0->mb_str(), text_anchor0, 
       text_width0, text_height0, dx0, dy0, x1, y1);
****/

return(0);
}

/**************************************************************************
*
**************************************************************************/
int JLP_Gseg_Wxwid::jlp_set_font(char *font_name, const unsigned int font_size, 
                                 UINT32 color_rgba0)
{
unsigned char red0, green0, blue0, alpha0;
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
wxFont font00(96, wxFONTFAMILY_ROMAN, wxFONTSTYLE_NORMAL, 
             wxFONTWEIGHT_NORMAL);

 gseg_backup_dc->SetFont(font0);
// gseg_backup_dc->SetFont(*wxSWISS_FONT);

// Postscript:
if(gseg_backup_pst_dc != NULL) {
  gseg_backup_pst_dc->SetFont(font00);
  }

 jlp_decode_color_uint32(color_rgba0, &red0, &green0, &blue0, &alpha0);

 gseg_backup_dc->SetBackgroundMode(wxTRANSPARENT);
/*
 gseg_backup_dc->SetTextForeground(*wxBLACK);
*/
 gseg_backup_dc->SetTextForeground(wxColour(red0, green0, blue0, alpha0));
 gseg_backup_dc->SetTextBackground(*wxWHITE);

return(0);
}
/**************************************************************************
* JLP interface with gnome: text (GNOME_TYPE_CANVAS_TEXT)
*
* INPUT:
*   char *string0 : string to be displayed on the label

* Private variables:
*   font_size_legend1
*
* OUTPUT:
*   double x1, y1, x2, y2 : coordinates of the output label box
**************************************************************************/
void JLP_Gseg_Wxwid::GSEG_DrawLegendGetSize(char *string0, 
                                            double *x1, double *y1, 
                                            double *x2, double *y2)
{
int text_width0, text_height0; 
wxString str0;
UINT32 transparent_color;

 str0 = wxString(string0);

// Set font color and size 
 transparent_color = 0xFFFFFF00;
 jlp_set_font(font_name1, font_size_legend1, transparent_color);

// Get size of label on the device:
 gseg_backup_dc->GetTextExtent(str0, &text_width0, &text_height0);

// Compute label size:
 *x1 = 0.;
 *y1 = 0.;
 *x2 = *x1 + text_width0;
 *y2 = *y1 + text_height0;

return;
}
/**************************************************************************
* JLP interface with gnome: text (GNOME_TYPE_CANVAS_TEXT)
* INPUT:
*   char *string0 : string to be displayed on the label
*   UINT32 fill_color_rgba0 : color of the label
*
* OUTPUT:
*  double x1, y1, x2, y2 : coordinates of the output label box
**************************************************************************/
void JLP_Gseg_Wxwid::GSEG_DrawLegend(char *string0, double x0, double y0, 
                                     const char *text_anchor0,
                                     UINT32 fill_color_rgba0, 
                                     UINT32 canvas_bg_color0, 
                                     const int legend_nlines)
{
wxString str0;
 
 str0 = wxString(string0);

// Set font color and size 
 jlp_set_font(font_name1, font_size_legend1, fill_color_rgba0);

// Draw the string according to text_anchor0
 jlp_wxwid_draw_legend(&str0, text_anchor0, x0, y0, canvas_bg_color0,
                       legend_nlines); 

return;
}
/************************************************************************
* Determine the offset dx0, dy0 according to the anchor string value 
*
****************************************************************************/
int JLP_Gseg_Wxwid::jlp_wxwid_decode_anchor(const char *text_anchor0, 
                                            const int width0, const int height0,
                                            int *dx0, int *dy0)
{
char text_anchor2[64];
int dx2, dy2;

// Copy to larger string to allow strncmp with larger string than input string
 strcpy(text_anchor2, text_anchor0);

// First guess of x/y offset:
 dx2 = - width0 / 2.;
 dy2 = - height0 / 2.;

 if(!strcmp(text_anchor2, "CENTER")){
   dx2 = - width0 / 2.;
   dy2 = - height0 / 2.;
 } else if(!strcmp(text_anchor2, "NORTH")){
   dx2 = - width0 / 2.;
   dy2 = 0.;
 } else if(!strcmp(text_anchor2, "NORTH_EAST")){
   dx2 = - width0;
   dy2 = 0.;
 } else if(!strcmp(text_anchor2, "NORTH_WEST")){
   dx2 = 0.;
   dy2 = 0.;
 } else if(!strcmp(text_anchor2, "SOUTH")){
   dx2 = - width0 / 2.;
   dy2 = - height0;
 } else if(!strcmp(text_anchor2, "SOUTH_EAST")){
   dx2 = - width0;
   dy2 = - height0;
 } else if(!strcmp(text_anchor2, "SOUTH_WEST")){
   dx2 = 0.;
   dy2 = - height0;
 } else if(!strcmp(text_anchor2, "WEST")){
   dx2 = 0.;
   dy2 = - height0 / 2.;
 } else if(!strcmp(text_anchor2, "EAST")){
   dx2 = - width0;
   dy2 = - height0 / 2.;
 }

*dx0 = dx2;
*dy0 = dy2;

return(0);
}
/*****************************************************************************
* Draw text pixbuf on canvas (gnome version)
*
* INPUT:
* text_anchor0 : "NORTH", "CENTER", etc
*****************************************************************************/
int JLP_Gseg_Wxwid::GSEG_DrawLabel(char *string0, double x0, double y0,
                                   int nlines, UINT32 fill_color_rgba0,
                                   UINT32 canvas_bg_color0,
                                   const char *text_anchor0,
                                   const int raise_to_top0,
                                   double *x1, double *y1,
                                   double *x2, double *y2)
{
int height_lines, xx1, yy1;
wxString str0;

// May have problems, with non-ascii characters:
// str0 = wxString(string0);
 str0 = wxString::FromUTF8(string0);

// Set font color and size 
jlp_set_font(font_name1, font_size_text1, fill_color_rgba0);

// Draw the string according to text_anchor0
jlp_wxwid_draw_label(&str0, text_anchor0, x0, y0, canvas_bg_color0,
                     raise_to_top0, x1, y1, x2, y2);

return(0);
}
/*****************************************************************************
*
*****************************************************************************/
void JLP_Gseg_Wxwid::jlp_background_text(wxString *string0, int x1, int y1,
                                         UINT32 canvas_bg_color0)
{
/* Declare variables */
int i, i1, nline, line_width;
int text_width0, text_height0, text_full_width0, text_full_height0, height_lines; 
int x, y, x11, y11, x22, y22;
char ch, text_line[1024], text_str[1024];
wxString str0;

gseg_backup_dc->GetTextExtent(*string0, &text_full_width0, &text_full_height0);

strcpy(text_str, (const char *)string0->mb_str());

// First go, look for the number of lines:
 nline = 0;
 for(i = 0; i < 1024; i++) 
  {
   ch = text_str[i];
   if((ch == '\n') || (ch == '\0')) {
     nline++;
     if ( ch == '\0' ) break; 
   }
  }
 height_lines = text_full_height0 / nline;

/* Get lengths of text lines and draw text background */
 x = window_width1 / 2.0;
 y = 10.0;
 i1 = 0;
 nline = 0;
 for(i = 0; i < 1024; i++) 
  {
   ch = text_str[i];
   if((ch == '\n') || (ch == '\0'))
     {
     nline++;
     text_str[i] = '\0';
     strcpy(text_line, &(text_str[i1]));
     str0 = wxString(text_line);

     if ( strlen(text_line) > 0 ) {
// Get size of label item on the device:
       gseg_backup_dc->GetTextExtent(str0, &text_width0, &text_height0);
       x11 = x1 - 1.0;
       x22 = x11 + text_width0;
       y11 = y1 + (nline - 1) * height_lines;
       y22 = y11 + height_lines;
// Draw corresponding rectangle filled with canvas_bg_color0
       line_width = 10;
       GSEG_DrawRectangle(x11, x22, y11, y22, canvas_bg_color0,
                          canvas_bg_color0, line_width);
       }

     if ( ch == '\0' ) {
        return;
        }

     i1 = i + 1;
     }
 }

return;
}
