/*******************************************************************************
*
* jlp_gnome_labels.cpp
* Label routines of JLP_Gseg_Gnome class 
*
* JLP
* Version 13/12/2016
*******************************************************************************/
#include <math.h>
#include "jlp_gseg_gnome.h" 

static void background_text(JLP_Gseg *jlp_gseg2, double width0, char *text_str, 
                            double height_lines, double x1, double y1,
                            UINT32 canvas_bg_color0);

/*****************************************************************************
*
*****************************************************************************/
void JLP_Gseg_Gnome::GSEG_SetFontFamily(char *font_name0,
                 double font_size_date_time0, double font_size_legend0,
                 double font_size_text0, double font_size_tick_labels0,
                 double font_size_axis_labels0, double font_size_title0)
{
unsigned int size;

// Save new settings to private variables:
strcpy(font_name1, font_name0);

font_size_date_time1 = font_size_date_time0;
font_size_legend1 = font_size_legend0;
font_size_text1 = font_size_text0;
font_size_tick_labels1 = font_size_tick_labels0;
font_size_axis_labels1 = font_size_axis_labels0;
font_size_title1 = font_size_title0;

/* Set font family */
 pango_font_description_set_family(font_date_time1,   font_name1);
 pango_font_description_set_family(font_legend1,      font_name1);
 pango_font_description_set_family(font_text1,        font_name1);
 pango_font_description_set_family(font_tick_labels1, font_name1);
 pango_font_description_set_family(font_axis_labels1, font_name1);
 pango_font_description_set_family(font_title1,       font_name1);

/* Set font sizes */
 pango_font_description_set_absolute_size(font_date_time1,
                                          font_size_date_time1*PANGO_SCALE);
 pango_font_description_set_absolute_size(font_legend1,
                                          font_size_legend1*PANGO_SCALE);
 pango_font_description_set_absolute_size(font_text1,
                                          font_size_text1*PANGO_SCALE);
 pango_font_description_set_absolute_size(font_tick_labels1,
                                          font_size_tick_labels1*PANGO_SCALE);
 pango_font_description_set_absolute_size(font_axis_labels1,
                                          font_size_axis_labels1*PANGO_SCALE);
 pango_font_description_set_absolute_size(font_title1,
                                          font_size_title1*PANGO_SCALE);

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
*   PangoFontDescription *font_tick_labels : Pango Font Description 
*
* OUTPUT:
*   double x1, y1, x2, y2 : coordinates of the output label box
**************************************************************************/
void JLP_Gseg_Gnome::GSEG_DrawTickLabels(char *string0, double x0, double y0, 
                                         const char *text_anchor0,
                                         UINT32 fill_color_rgba0, 
                                         UINT32 canvas_bg_color0,
                                         int raise_to_top0, 
                                         double *x1, double *y1, 
                                         double *x2, double *y2) 
{
GnomeCanvasItem *text;
GtkAnchorType anchor1;

// Decode text_anchor0 and transfer to anchor1
  jlp_gnome_decode_anchor(text_anchor0, &anchor1);

  text = gnome_canvas_item_new(parent_group,
                               GNOME_TYPE_CANVAS_TEXT,
                               "text", string0,
                               "x", x0,
                               "y", y0,
                               "anchor", anchor1,
                               "font-desc", font_tick_labels1,
                               "fill_color_rgba", fill_color_rgba0, 
                               NULL);

  gnome_canvas_item_get_bounds(text, x1, y1, x2, y2);

if(raise_to_top0 == 1) gnome_canvas_item_raise_to_top(text);

return;
}
/**************************************************************************
* JLP interface with gnome: text (GNOME_TYPE_CANVAS_TEXT)
* INPUT:
*   char *string0 : string to be displayed on the label

* Private variables:
*   PangoFontDescription *font_legend : Pango Font Description
*
* OUTPUT:
*   double x1, y1, x2, y2 : coordinates of the output label box
**************************************************************************/
void JLP_Gseg_Gnome::GSEG_DrawLegendGetSize(char *string0, 
                                            double *x1, double *y1, 
                                            double *x2, double *y2)
{
GnomeCanvasItem *text;
UINT32 transparent_color;

transparent_color = 0xFFFFFF00; 
  text = gnome_canvas_item_new(parent_group,
                               GNOME_TYPE_CANVAS_TEXT,
                               "text", string0,
                               "font-desc", font_legend1,
                               "fill_color_rgba", transparent_color,
                               NULL);

  gnome_canvas_item_get_bounds(text, x1, y1, x2, y2);

return;
}
/**************************************************************************
* JLP interface with gnome: text (GNOME_TYPE_CANVAS_TEXT)
* INPUT:
*   char *string0 : string to be displayed on the label
*   UINT32 fill_color_rgba0 : color of the label

* Private variables:
*   PangoFontDescription *font_legend : Pango Font Description
*
* OUTPUT:
*   double x1, y1, x2, y2 : coordinates of the output label box
**************************************************************************/
void JLP_Gseg_Gnome::GSEG_DrawLegend(char *string0, double x0, double y0, 
                                     const char *text_anchor0,
                                     UINT32 fill_color_rgba0, 
                                     UINT32 canvas_bg_color0, 
                                     const int legend_nlines)
{
double height_lines, x1, y1, x2, y2;
GnomeCanvasItem *text_item;
GtkAnchorType anchor1;
UINT32 color0;

/* Specify legend text parent group and font properties */
   text_item = gnome_canvas_item_new(parent_group,
                                GNOME_TYPE_CANVAS_TEXT,
                                "text", string0,
                                "font-desc", font_legend1,
                                "fill_color_rgba", fill_color_rgba0, 
                                NULL);



// Decode text_anchor0 and transfer to anchor1
  jlp_gnome_decode_anchor(text_anchor0, &anchor1);

   gnome_canvas_item_set(text_item,
                         "x", x0,
                         "y", y0,
                         "anchor", anchor1,
                         NULL);

   /* Draw legend background rectangles */
   gnome_canvas_item_get_bounds(text_item, &x1, &y1, &x2, &y2);
   height_lines = (y2 - y1) / legend_nlines;
   background_legend(string0, height_lines, x1 - 70.0, y1, canvas_bg_color0);
   gnome_canvas_item_raise_to_top(text_item);

  /* Draw legend background rectangles */
   gnome_canvas_item_get_bounds(text_item, &x1, &y1, &x2, &y2);
   height_lines = (y2 - y1) / legend_nlines;
   background_legend(string0, height_lines, x1 - 70.0, y1, canvas_bg_color0);
   gnome_canvas_item_raise_to_top(text_item);

return;
}
/************************************************************************
*
****************************************************************************/
void JLP_Gseg_Gnome::background_legend (char *legend_str, double height_lines, 
                                        double x1, double y1, 
                                        UINT32 canvas_bg_color0 )
   {
   /* Declare variables */
   int i, i1, nline;
   double x, y, x1rec, y1rec, x2rec, y2rec,
          x11, y11, x22, y22;
   char ch;
   GnomeCanvasItem *text, *rectangle;

   /* Get lengths of text lines and draw text background */
   x = p_parent_window_data.width / 2.0;
   y = 10.0;
   i = 0;
   i1 = 0;
   nline = 0;
   while ( 1 )
      {
      if ( (ch = legend_str[i]) == '\n' || (ch = legend_str[i]) == '\0' )
         {
         nline++;
         legend_str[i] = '\0';

         if ( strlen(&legend_str[i1]) > 0 )
            {
            text = gnome_canvas_item_new(parent_group,
                                         GNOME_TYPE_CANVAS_TEXT,
                                         "text", &legend_str[i1],
                                         "x", x,
                                         "y", y,
                                         "anchor", GTK_ANCHOR_NORTH,
                                         "font-desc", font_legend1,
                                         "fill_color_rgba", 0xFFFFFF00,
                                         NULL);
            gnome_canvas_item_get_bounds(text, &x1rec, &y1rec, &x2rec, &y2rec);
            gtk_object_destroy((GtkObject*) text);

            x11 = x1 - 1.0;
            x22 = x1 + 70.0 + x2rec - x1rec + 1.0;
            y11 = y1 + (nline - 1)*height_lines;
            y22 = y11 + height_lines + 1.0;
            rectangle = gnome_canvas_item_new(parent_group,
                                              GNOME_TYPE_CANVAS_RECT,
                                              "x1", x11,
                                              "x2", x22,
                                              "y1", y11,
                                              "y2", y22,
                                              "fill_color_rgba", 
                                                    canvas_bg_color0,
                                              "outline_color_rgba", 
                                                    canvas_bg_color0,
                                              "width_pixels", 1,
                                              NULL);
            }

         if ( ch == '\0' )
            return;

         i++;
         i1 = i;
         }

      else
         i++;
      }
return;
}
/************************************************************************
*
****************************************************************************/
int JLP_Gseg_Gnome::jlp_gnome_decode_anchor(const char *text_anchor0, 
                                            GtkAnchorType *anchor1)
{
char text_anchor2[64];

// Copy to larger string to allow strncmp with larger string than input string
 strcpy(text_anchor2, text_anchor0);

  *anchor1 = GTK_ANCHOR_EAST;
  if(!strcmp(text_anchor2, "EAST")) {
   *anchor1 = GTK_ANCHOR_EAST;
  } else if(!strcmp(text_anchor2, "WEST")) {
   *anchor1 = GTK_ANCHOR_WEST;
  } else if(!strcmp(text_anchor2, "SOUTH_EAST")) {
   *anchor1 = GTK_ANCHOR_SOUTH_EAST;
  } else if(!strcmp(text_anchor2, "SOUTH_WEST")) {
   *anchor1 = GTK_ANCHOR_SOUTH_WEST;
  } else if(!strcmp(text_anchor2, "NORTH_EAST")) {
   *anchor1 = GTK_ANCHOR_NORTH_EAST;
  } else if(!strcmp(text_anchor2, "NORTH_WEST")) {
   *anchor1 = GTK_ANCHOR_NORTH_WEST;
  } else if(!strcmp(text_anchor2, "CENTER")) {
   *anchor1 = GTK_ANCHOR_CENTER;
// At the end, to avoid misleading with NORT_WEST, etc:
  } else if(!strcmp(text_anchor2, "NORTH")) {
   *anchor1 = GTK_ANCHOR_NORTH;
  } else if(!strcmp(text_anchor2, "SOUTH")) {
   *anchor1 = GTK_ANCHOR_SOUTH;
  }

return(0);
}
/*****************************************************************************
* Draw text pixbuf on canvas (gnome version)
*
* INPUT:
* text_anchor0 : "NORTH", "CENTER", etc
*****************************************************************************/
int JLP_Gseg_Gnome::GSEG_DrawLabel(char *text_str, double x0, double y0,
                                   int nlines, UINT32 fill_color_rgba,
                                   UINT32 canvas_bg_color0,
                                   const char *text_anchor0,
                                   const int raise_to_top,
                                   double *x1, double *y1,
                                   double *x2, double *y2)
{
GnomeCanvasItem *text_item;
GtkAnchorType anchor1;
double height_lines;

// Decode text_anchor0 and transfer to anchor1
  jlp_gnome_decode_anchor(text_anchor0, &anchor1);

  text_item = gnome_canvas_item_new(parent_group,
                               GNOME_TYPE_CANVAS_TEXT,
                               "text", text_str,
                               "x", x0,
                               "y", y0,
                               "anchor", anchor1,
                               "font-desc", font_text1,
                               "fill_color_rgba", fill_color_rgba,
                               NULL);

    gnome_canvas_item_get_bounds(text_item, x1, y1, x2, y2);

// Raise to top:
 if(raise_to_top == 1) {

/* Draw background rectangles for the displayed text*/
    height_lines = (*y2 - *y1)/nlines;
    background_text(this, p_parent_window_data.width, text_str,
                    height_lines, *x1, *y1, canvas_bg_color0);
    gnome_canvas_item_raise_to_top(text_item);
  }

return(0);
}
/*****************************************************************************
*
*****************************************************************************/
static void background_text(JLP_Gseg *jlp_gseg2, double width0, char *text_str,
                            double height_lines, double x1, double y1,
                            UINT32 canvas_bg_color0)
{
/* Declare variables */
int i, i1, nline;
double x, y, x1rec, y1rec, x2rec, y2rec, x11, y11, x22, y22;
char ch, text_anchor[64];

/* Get lengths of text lines and draw text background */
   x = width0 / 2.0;
   y = 10.0;
   i = 0;
   i1 = 0;
   nline = 0;
   while ( 1 )
      {
      if ( (ch = text_str[i]) == '\n' || (ch = text_str[i]) == '\0' )
         {
         nline++;
         text_str[i] = '\0';

         if ( strlen(&text_str[i1]) > 0 )
            {
            strcpy(text_anchor, "NORTH");
// Draw label with "raise_to_top=0"
            jlp_gseg2->GSEG_DrawLabel(&text_str[i1], x, y, nline, 0xFFFFFF00,
                                      canvas_bg_color0, text_anchor, 0,
                                      &x1rec, &y1rec, &x2rec, &y2rec);
            x11 = x1 - 1.0;
            x22 = x1 + x2rec - x1rec + 1.0;
            y11 = y1 + (nline - 1)*height_lines - 1.0;
            y22 = y11 + height_lines + 1.0;
// Draw backround rectangle:
            jlp_gseg2->GSEG_DrawRectangle(x11, x22, y11, y22, canvas_bg_color0,
                               canvas_bg_color0, 1);
            }

         if ( ch == '\0' )
            return;

         i++;
         i1 = i;
         }

      else
         i++;
      }

return;
}
