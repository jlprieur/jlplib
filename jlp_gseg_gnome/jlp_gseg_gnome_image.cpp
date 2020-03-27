/*******************************************************************************
*
* jlp_gnome_image.cpp
* Image routines of JLP_Gseg_Gnome class 
*
* JLP
* Version 13/12/2016
*******************************************************************************/
#include <math.h>
#include "jlp_gseg_gnome.h" 

static int roundint ( double x )
   {
   int i;

   if ( x >= 0.0 )
      i = (int) (x + 0.5);
   else
      i = (int) (x - 0.5);

   return i;
   }

/*****************************************************************************
* Draw data pixbuf on canvas (gnome version)
*
* INPUT:
*  nx0 : width of mask array
*  ny0 : height of mask array
*  x0, y0 : position
*  dx0, dy0 : translation vector ((0.,0.) if none)
*  angle0 : angle (-90 or 0)
*****************************************************************************/
int JLP_Gseg_Gnome::GSEG_DrawDPixbuf(JLP_DPixbuf *pixbuf0, double x0, 
                                     double y0, const char *text_anchor0)
{
GnomeCanvasItem *pixbuf_item;
GdkPixbuf *pixbuf_contour;
int i, j, nx0, ny0;
GtkAnchorType anchor1;
UINT32 color0;

nx0 = pixbuf0->nx;
ny0 = pixbuf0->ny;

// Decode text_anchor0 and transfer to anchor1
  jlp_gnome_decode_anchor(text_anchor0, &anchor1);

 pixbuf_contour = gdk_pixbuf_new(GDK_COLORSPACE_RGB, TRUE, 8,
                                  nx0, ny0);
/* alpha = 0xFF */
 gdk_pixbuf_fill(pixbuf_contour, 0xFFFFFF00);

// Transfer data:
 for(j = 0; j < ny0; j++) {
  for(i = 0; i < nx0; i++) {
   color0 = pixbuf0->array[i + j * nx0];
   if(color0 != 0) put_pixel(pixbuf_contour, i, j, color0);
  }
 }

 pixbuf_item = gnome_canvas_item_new(parent_group, GNOME_TYPE_CANVAS_PIXBUF,
                                     "pixbuf", pixbuf_contour,
                                     "x", x0, "y", y0,
                                     "anchor", anchor1, NULL);

 g_object_unref(pixbuf_contour);

return(0);
}
/*****************************************************************************
* Add pixel of specified color to pixbuf
*
*****************************************************************************/
void JLP_Gseg_Gnome::put_pixel(GdkPixbuf *pixbuf, int i, int j, UINT32 color)
{
int n_channels, rowstride;
guchar *pixels, *p, red, green, blue, alpha;

/* pointer to pixel data of pixbuf */
pixels = gdk_pixbuf_get_pixels(pixbuf);       

/* samples per pixel; n_channels = 4 */
n_channels = gdk_pixbuf_get_n_channels(pixbuf);   

/* bytes between start of one row and start of next row */
rowstride  = gdk_pixbuf_get_rowstride(pixbuf);    

red   = (guchar)  (color/0x1000000);
green = (guchar) ((color - red*0x1000000)/0x10000);
blue  = (guchar) ((color - red*0x1000000 - green*0x10000)/0x100);
alpha = (guchar)  (color - red*0x1000000 - green*0x10000 - blue*0x100);

p = pixels + i*n_channels + j*rowstride;
p[0] = red;
p[1] = green;
p[2] = blue;
p[3] = alpha;

return;
}
/*****************************************************************************
* Draw text pixbuf on canvas (gnome version)
*
* INPUT:
*  nx0 : width of mask array
*  ny0 : height of mask array
*  x0, y0 : position
*  dx0, dy0 : translation vector ((0.,0.) if none)
*  angle0 : angle (-90 or 0)
*****************************************************************************/
int JLP_Gseg_Gnome::GSEG_DrawTPixbuf(JLP_TPixbuf *pixbuf0, double x0, 
                                     double y0, const double angle0, 
                                     const double dx0, const double dy0,
                                     const char *text_anchor0)
{
GnomeCanvasItem *pixbuf_item;
GdkPixbuf *pixbuf1;
GtkAnchorType anchor1;
double affine1[6], affine2[6];

// Decode text_anchor0 and transfer to anchor1
  jlp_gnome_decode_anchor(text_anchor0, &anchor1);

jlp_gnome_FromJLP2GnomePixbuf(pixbuf0, &pixbuf1);

 pixbuf_item = gnome_canvas_item_new(parent_group, GNOME_TYPE_CANVAS_PIXBUF,
                                     "pixbuf", pixbuf1,
                                     "x", x0, "y", y0,
                                     "anchor", anchor1, NULL);

/* Rotate pixbuf canvas item */
 if(angle0 == -90.0) {
    art_affine_rotate(affine1, -90.0);
    gnome_canvas_item_affine_absolute(pixbuf_item, affine1);
   }

/* Translate zlabel pixbuf canvas item */
 if((dx0 != 0.0) || (dy0 != 0.0)) {
   art_affine_translate(affine2, dx0, dy0);
   gnome_canvas_item_affine_relative(pixbuf_item, affine2);
   }

return(0);
}
/**********************************************************************
*
***********************************************************************/
int JLP_Gseg_Gnome::jlp_gnome_FromJLP2GnomePixbuf(JLP_TPixbuf *pixbuf0, 
                                                  GdkPixbuf **pixbuf1)
{
/* Declare variables */
double x, y, x1, y1, x2, y2, font_size;
char *label, *font_name;
GnomeCanvasItem *text;
PangoFontDescription *font;
UINT32 canvas_fg_color = 0x000000FF;   /* black */

/* Draw label text canvas item */
   x = p_parent_window_data.width / 2.0;
   y = 10.0;

   label = pixbuf0->text;
   font_name = pixbuf0->font_name;
   font_size = pixbuf0->font_size;
   if(!strcmp(font_name, "font_axis_title")) {
     font = font_axis_labels1;
     } else {
     font = font_title1;
     }

  if ( label == NULL ) return(-1);

   text = gnome_canvas_item_new(parent_group,
                                GNOME_TYPE_CANVAS_TEXT,
                                "text", label,
                                "x", x,
                                "y", y,
                                "anchor", GTK_ANCHOR_NORTH,
                                "justification", GTK_JUSTIFY_CENTER,
                                "font-desc", font,
                                "fill_color_rgba", canvas_fg_color,
                                NULL);

/* Update canvas */
   gnome_canvas_update_now((GnomeCanvas*) p_parent_window_data.canvas);

/* Get label pixbuf from canvas */
   gnome_canvas_item_get_bounds(text, &x1, &y1, &x2, &y2);

   *pixbuf1 = gdk_pixbuf_get_from_drawable(NULL,
                    (GdkDrawable *) p_parent_window_data.canvas->window,
                                                NULL,
                                                roundint(x1),
                                                roundint(y1),
                                                0,
                                                0,
                                                roundint(x2 - x1) + 1,
                                                roundint(y2 - y1));

/* Destroy label text item */
   gtk_object_destroy((GtkObject*) text);

/* Update canvas */
   gnome_canvas_update_now((GnomeCanvas*) p_parent_window_data.canvas);

return(0);
}
/*****************************************************************************
* Plot an extra image on canvas (gnome version)
*
* INPUT:
*  x0, y0 : position
*****************************************************************************/
int JLP_Gseg_Gnome::GSEG_PlotExtraImage(const char *image_filename, double x0, 
                                        double y0, const char *text_anchor0)
{
GnomeCanvasItem *image_item;
GdkPixbuf *image1;
GtkAnchorType anchor1;

// Decode text_anchor0 and transfer to anchor1
 jlp_gnome_decode_anchor(text_anchor0, &anchor1);

 image1 = gdk_pixbuf_new_from_file(image_filename, NULL);
 image_item = gnome_canvas_item_new(parent_group,
                                    GNOME_TYPE_CANVAS_PIXBUF,
                                    "pixbuf", image1,
                                    "x", x0,
                                    "y", y0,
                                    "anchor", anchor1,
                                    NULL);
 gdk_pixbuf_unref(image1);

return(0);
}
/*****************************************************************************
* Get image info from canvas (gnome version)
*
* INPUT:
*  x0, y0 : position
*****************************************************************************/
int JLP_Gseg_Gnome::GSEG_GetImageSize(const char *image_filename, int *width0, 
                                      int *height0)
{
GdkPixbuf *image1;

 image1 = gdk_pixbuf_new_from_file(image_filename, NULL);
 *width0 = gdk_pixbuf_get_width(image1);
 *height0 = gdk_pixbuf_get_height(image1);

return(0);
}
/*****************************************************************************
* Draw image on canvas (gnome version)
*
* INPUT:
*  x0, y0 : canvas position
*  outer_radius: radius of the outer circle to make the background transparent
*                (0 if not relevant)
*****************************************************************************/
int JLP_Gseg_Gnome::GSEG_DrawBackgroundImage(const char *image_filename, 
                                             const int image_style0, 
                                             double x0, double y0,
                                             const int width_plot0, 
                                             const int height_plot0,
                                             const double outer_radius0,
                                             const char *text_anchor0)
{
int CENTER=1, FILL=2, SCALE=3, ZOOM=4;
int width_image0, height_image0;
int width_image1, height_image1, width_image2, height_image2;
int i, j, i0, j0;
double x_image, y_image, ratio_image, ratio_plot, rsq, rsq_max;
GnomeCanvasItem *image_item;
GdkPixbuf *image1, *image2, *image_file;
GtkAnchorType anchor1;

 GSEG_GetImageSize(image_filename, &width_image0, &height_image0);
 ratio_image = (double) height_image0 / (double) width_image0;
 ratio_plot = (double) height_plot0 / (double) width_plot0;

 image_file = gdk_pixbuf_new_from_file(image_filename, NULL);
 image1 = NULL;
 image2 = NULL;

 if(image_style0 == CENTER )
   {
   if ( width_image0 > width_plot0 && height_image0 <= height_plot0)
      {
      width_image1 = width_plot0;
      height_image1 = height_image0;
      x_image = roundint((width_image0 - width_image1)/2.0);
      y_image = 0;
      } 
   else if ( width_image0 <= width_plot0 && height_image0 > height_plot0)
      {
      width_image1 = width_image0;
      height_image1 = height_plot0;
      x_image = 0;
      y_image = roundint((height_image0 - height_image1)/2.0);
      } 
   else if ( width_image0 > width_plot0 && height_image0 > height_plot0)
      {
      width_image1 = width_plot0;
      height_image1 = height_plot0;
      x_image = roundint((width_image0 - width_image1)/2.0);
      y_image = roundint((height_image0 - height_image1)/2.0);
      }
   else
      {
      width_image1 = width_image0;
      height_image1 = height_image0;
      x_image = 0;
      y_image = 0;
      }
// Copy input image_file to image1
   image1 = gdk_pixbuf_new(GDK_COLORSPACE_RGB, TRUE, 8, width_image1, 
                           height_image1);
   gdk_pixbuf_copy_area(image_file, x_image, y_image, width_image1, 
                        height_image1, image1, 0, 0);
   } 
 else if (image_style0 == FILL )
   {
// Scale input image to image1
   width_image1 = width_plot0;
   height_image1 = height_plot0;
   image1 = gdk_pixbuf_scale_simple(image_file, width_image1,
                                          height_image1, GDK_INTERP_BILINEAR);
   }
 else if ( image_style0 == SCALE )
   {
// Scale input image_file to image1
   if ( ratio_image <= ratio_plot )
      {
      width_image1 = width_plot0;
      height_image1 = roundint(width_image1 * ratio_image);
      }
   else
      {
      height_image1 = height_plot0;
      width_image1 = roundint(height_image1 / ratio_image);
      }
   image1 = gdk_pixbuf_scale_simple(image_file, width_image1,
                                    height_image1, GDK_INTERP_BILINEAR);
   }
  else if ( image_style0 == ZOOM )
   {
// Copy input image_file to image2
   if ( ratio_image <= ratio_plot )
      {
      height_image2 = height_image0;
      width_image2 = roundint(height_image2 / ratio_plot);
      x_image = roundint((width_image0 - width_image2) / 2.0);
      y_image = 0;
      }
   else
      {
      width_image2 = width_image0;
      height_image2 = roundint(width_image2 * ratio_plot);
      x_image = 0;
      y_image = roundint((height_image0 - height_image2) / 2.0);
      }
    image2 = gdk_pixbuf_new(GDK_COLORSPACE_RGB, TRUE, 8, width_image2,
                            height_image2);
    gdk_pixbuf_copy_area(image_file, x_image, y_image,
                         width_image2, height_image2, image2, 0, 0);

// Scale image2 to image1
    width_image1 = width_plot0;
    height_image1 = height_plot0;
      image1 = gdk_pixbuf_scale_simple(image2, width_image1, height_image1,
                                       GDK_INTERP_BILINEAR);

      }

// Make image pixels outside plot circle transparent
 if(outer_radius0 > 1.) {
  i0 = (double) width_image1 / 2.0;
  j0 = (double) height_image1 / 2.0;
  rsq_max = outer_radius0 * outer_radius0;
  for ( i = 1; i <= width_image1; i++ )
    {
    for ( j = 1; j <= height_image1; j++ )
      {
      rsq = (i - i0)*(i - i0) + (j - j0)*(j - j0);
      if ( rsq >= rsq_max)
         put_pixel(image1, i-1, j-1, 0xFFFFFF00);
      }
    }
 }

// Decode text_anchor0 and transfer to anchor1
 jlp_gnome_decode_anchor(text_anchor0, &anchor1);

 image_item = gnome_canvas_item_new(parent_group,
                                    GNOME_TYPE_CANVAS_PIXBUF,
                                    "pixbuf", image1,
                                    "x", x0,
                                    "y", y0,
                                    "anchor", anchor1,
                                    NULL);

 if(G_IS_OBJECT(image_file)) gdk_pixbuf_unref(image_file);
 if(G_IS_OBJECT(image1)) gdk_pixbuf_unref(image1);
 if(G_IS_OBJECT(image2)) gdk_pixbuf_unref(image2);

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
            jlp_gseg2->GSEG_DrawLabel(&text_str[i1], x, y, nline, 0xFFFFFF00, 
                                      canvas_bg_color0, text_anchor, 0, 
                                      &x1rec, &y1rec, &x2rec, &y2rec);

            x11 = x1 - 1.0;
            x22 = x1 + x2rec - x1rec + 1.0;
            y11 = y1 + (nline - 1)*height_lines;
            y22 = y11 + height_lines + 1.0;
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
