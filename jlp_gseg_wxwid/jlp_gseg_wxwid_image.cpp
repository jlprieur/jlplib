/****************************************************************************
*
* jlp_gseg_wxwid_image.cpp
* Image routines of JLP_Gseg_Wxwid class 
* Interface with wxWidgets
*
* JLP
* Version 25/12/2016
*****************************************************************************/
#include <math.h>
#include "jlp_gsegraf_defs.h"

#include "jlp_gseg_wxwid.h"     // Gseg_Wxwid class

/****************************************************************************
*
****************************************************************************/
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
* Draw data pixbuf on canvas
*
* INPUT:
*  array_rgba0: rgba color data to be plotted
*  nx0 : width of array
*  ny0 : height of array
*  x0, y0 : position coordinates
*  text_anchor0 : position sector ("NORTH, SOUTH, etc)
*****************************************************************************/
int JLP_Gseg_Wxwid::jlp_wxwid_DrawPixbuf(UINT32 *array_rgba0, int nx0, int ny0,
                                         double x0, double y0, 
                                         const char *text_anchor0)
{
unsigned char aa, rr, gg, bb;
int i, j, idx1, idx2, dx0, dy0, xx0, yy0;
int new_nx0, new_ny0, xx_0, yy_0, in_frame;
int status = -1;
long color_rgba0;
wxImage *my_image;
wxBitmap *my_bitmap, *my_bitmap_masked, *my_resized_bitmap;
unsigned char *rgbdata0, *alpha0;

rgbdata0 = new unsigned char[3 * nx0 * ny0];
alpha0 = new unsigned char[nx0 * ny0];

idx1 = 0;
idx2 = 0;
for(j = 0; j < ny0; j++) {
  for(i = 0; i < nx0; i++) {
   color_rgba0 = array_rgba0[i + j * nx0];
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
   rgbdata0[idx1  ] = rr;
   rgbdata0[idx1 + 1 ] = gg;
   rgbdata0[idx1 + 2 ] = bb;
   idx1 += 3;
   alpha0[idx2] = aa;
   idx2 ++;
  }
}
	 
// To be plateform independent, needs to create first an image 
// and then a bitmap
// If false, the arrays data0 and alpha0 will be freed immediately
  my_image = new wxImage(nx0, ny0, rgbdata0, alpha0, false);
// Create a bitmap with the current display depth
  if(my_image->IsOk()) {
   my_bitmap = new wxBitmap(*my_image, -1);

// Determine the offset dx0, dy0 :
  jlp_wxwid_decode_anchor(text_anchor0, nx0, ny0, &dx0, &dy0);
  xx0 = x0 + dx0;
  yy0 = y0 + dy0;

// Draw the bitmap at x0, y0 on the destination device context:
   gseg_backup_dc->DrawBitmap(*my_bitmap, xx0, yy0, true);

// Postscript:
   if(gseg_backup_pst_dc != NULL) {
// Resize bitmap according to x_scale, y_scale
    new_nx0 = nx0 * x_wx_to_pst_scale1;
    new_ny0 = ny0 * y_wx_to_pst_scale1;
    my_resized_bitmap = new wxBitmap(
                  (*my_image).Scale(new_nx0, new_ny0, wxIMAGE_QUALITY_NORMAL));
    jlp_convert_wx_to_pst(xx0, yy0, &xx_0, &yy_0, &in_frame);
    gseg_backup_pst_dc->DrawBitmap(*my_resized_bitmap, xx_0, yy_0, true);
    delete my_resized_bitmap;
  }


   status = 0;
   delete my_bitmap;
  } else {
    fprintf(stderr, "jlp_wxwid_DrawPixbuf/Error creating temporary image\n"); 
  }

delete my_image;
return(status);
}
/*****************************************************************************
* Draw data pixbuf on canvas
* DataPixBuffer
typedef struct {
int nx, ny;
UINT32 *array;
} JLP_DPixbuf;
*  nx : width of bitmap array
*  ny : height of bitmap array
*
* INPUT:
*  x0, y0 : position coordinates
*  text_anchor0 : position sector ("NORTH, SOUTH, etc)
*****************************************************************************/
int JLP_Gseg_Wxwid::GSEG_DrawDPixbuf(JLP_DPixbuf *pixbuf0, double x0, 
                                     double y0, const char *text_anchor0)
{
int status, nx0, ny0;

nx0 = pixbuf0->nx;
ny0 = pixbuf0->ny;

status = jlp_wxwid_DrawPixbuf(pixbuf0->array, nx0, ny0, x0, y0, text_anchor0);

return(status);
}
/*****************************************************************************
* Draw text pixbuf on canvas
*
* TextPixBuffer
typedef struct {
char text[128];
char font_name[128];
double font_size;
UINT32 color;
} JLP_TPixbuf;
*
* INPUT:
*  nx0 : width of mask array
*  ny0 : height of mask array
*  x0, y0 : position
*  dx0, dy0 : translation vector ((0.,0.) if none)
*  angle0 : angle (-90 or 0)
*****************************************************************************/
int JLP_Gseg_Wxwid::GSEG_DrawTPixbuf(JLP_TPixbuf *pixbuf0, double x0, 
                                     double y0, const double angle0, 
                                     const double dx0, const double dy0,
                                     const char *text_anchor0)
{
unsigned int font_size2;
UINT32 color_rgba2;
wxString str2;
int dx2, dy2, text_width2, text_height2, xx2, yy2;
int xx_2, yy_2, in_frame;
double angle22;

str2 = wxString(pixbuf0->text);
 
// Get size of label on the device:
 gseg_backup_dc->GetTextExtent(str2, &text_width2, &text_height2);

// Determine the offset dx2, dy2 according to the value of text_anchor0 :
jlp_wxwid_decode_anchor(text_anchor0, text_width2, text_height2,
                         &dx2, &dy2);

font_size2 = pixbuf0->font_size;
color_rgba2 = pixbuf0->color;
jlp_set_font(font_name1, font_size2, color_rgba2);

/* Draw label */
if(angle0 == 0.) {
 xx2 = (int)x0 + dx2 - (int)dx0;
 yy2 = (int)y0 + dy2 - (int)dy0;
 gseg_backup_dc->DrawText(str2, xx2, yy2);
// Postscript:
 if(gseg_backup_pst_dc != NULL) {
   jlp_convert_wx_to_pst(xx2, yy2, &xx_2, &yy_2, &in_frame);
   gseg_backup_pst_dc->DrawText(str2, xx_2, yy_2);
 }
} else {
// In DrawAxis, swap x and y, for displaying y axis
 xx2 = (int)y0 + dy2 + (int)dy0;
 yy2 = (int)x0 - dx2 - (int)dx0;
/* DEBUG:
 printf("GSEG_DrawTPixbuf/label=%s xx2=%d y0=%d dy2=%d dy0=%d\n", 
          pixbuf0->text, xx2, (int)y0, dy2, (int)dy0);
 printf("GSEG_DrawTPixbuf/label=%s yy2=%d x0=%d dx2=%d dx0=%d\n", 
          pixbuf0->text, yy2, (int)x0, dx2, (int)dx0);
*/
 angle22 = 360. - angle0;
 gseg_backup_dc->DrawRotatedText(str2, xx2, yy2, angle22);
// Postscript:
 if(gseg_backup_pst_dc != NULL) {
   jlp_convert_wx_to_pst(xx2, yy2, &xx_2, &yy_2, &in_frame);
   gseg_backup_pst_dc->DrawRotatedText(str2, xx_2, yy_2, angle22);
 }
}

return(0);
}
/*****************************************************************************
* Plot an extra image on canvas
*
* INPUT:
*  x0, y0 : position
*****************************************************************************/
int JLP_Gseg_Wxwid::GSEG_PlotExtraImage(const char *image_filename, double x0, 
                                        double y0, const char *text_anchor0)
{
int status = -1, nx0, ny0, dx0, dy0, xx0, yy0;
int new_nx0, new_ny0;
int xx_0, yy_0, in_frame;
wxImage *my_image;
wxBitmap *my_bitmap, *my_resized_bitmap;

// Init all image handlers in case it was not already done:
// (Needed to read PNG,JPEG, etc)
  if(wxImage::CanRead(image_filename) == false) {
   wxInitAllImageHandlers();
   }

// To be plateform independent, needs to create first an image
// and then a bitmap
  my_image = new wxImage(image_filename);

// Create a bitmap with the current display depth
  if(my_image->IsOk() == true) {
   my_bitmap = new wxBitmap(*my_image, -1);

// Get size of image
  nx0 = my_image->GetWidth();
  ny0 = my_image->GetHeight();

// Determine the offset dx0, dy0  according to text_anchor0:
  jlp_wxwid_decode_anchor(text_anchor0, nx0, ny0, &dx0, &dy0);
  xx0 = x0 + dx0;
  yy0 = y0 + dy0;

// Draw the bitmap at x0, y0 on the destination device context:
   gseg_backup_dc->DrawBitmap(*my_bitmap, xx0, yy0);

// Postscript:
   if(gseg_backup_pst_dc != NULL) {
    new_nx0 = nx0 * x_wx_to_pst_scale1;
    new_ny0 = ny0 * y_wx_to_pst_scale1;
    my_resized_bitmap = new wxBitmap(
                  (*my_image).Scale(new_nx0, new_ny0, wxIMAGE_QUALITY_NORMAL));
    jlp_convert_wx_to_pst(xx0, yy0, &xx_0, &yy_0, &in_frame);
    gseg_backup_pst_dc->DrawBitmap(*my_resized_bitmap, xx_0, yy_0);
    delete my_resized_bitmap;
   }

   status = 0;
   delete my_bitmap;
  } else {
   fprintf(stderr, "GSEG_PlotExtraImage/Error reading >%s< file\n", image_filename);
  }

delete my_image;
return(status);
}
/*****************************************************************************
* Get image info from canvas
*
* INPUT:
*  image_filename 
* OUTPT:
*  width0, height0
*****************************************************************************/
int JLP_Gseg_Wxwid::GSEG_GetImageSize(const char *image_filename, int *width0, 
                                      int *height0)
{
int status = -1;
wxImage *my_image;

// To be plateform independent, needs to create first an image
// and then a bitmap
  my_image = new wxImage(image_filename);

// Get size of image
  if(my_image->IsOk()) {
    *width0 = my_image->GetWidth();
    *height0 = my_image->GetHeight();
    status = 0;
  } else {
   fprintf(stderr, "GSEG_GetImageSize/Error reading >%s< file\n", image_filename);
  }

delete my_image;

return(status);
}
/*****************************************************************************
* Draw image on canvas
*
* INPUT:
*  image_style: integer code of required position 
*               (1=center, 2=fill, 3=scale, 4=zoom)
*  x0, y0 : canvas position
*  outer_radius: radius of the outer circle to make the background transparent
*                (0 if not relevant)
*****************************************************************************/
int JLP_Gseg_Wxwid::GSEG_DrawBackgroundImage(const char *image_filename, 
                                             const int image_style0, 
                                             double x0, double y0,
                                             const int width_plot0, 
                                             const int height_plot0,
                                             const double outer_radius0,
                                             const char *text_anchor0)
{
int CENTER=1, FILL=2, SCALE=3, ZOOM=4;
int status = -1, nx0, ny0, dx0, dy0, xx0, yy0;
int width_image0, height_image0;
int new_nx0, new_ny0, xx_0, yy_0, in_frame;
int width_image1, height_image1, i0, j0, i, j;
double x_image, y_image, ratio_image0, ratio_plot0, rsq, rsq_max;
wxSize new_size;
wxImage *my_image0, *my_image_0;
wxBitmap *my_bitmap0, *my_resized_bitmap0;

// Init all image handlers in case it was not already done:
// (Needed to read PNG,JPEG, etc)
  if(wxImage::CanRead(image_filename) == false) {
   wxInitAllImageHandlers();
   }

// To be plateform independent, needs to create first an image
// and then a bitmap
  my_image0 = new wxImage(image_filename);
  if(!my_image0->IsOk()) {
    fprintf(stderr, "GSEG_DrawBackgroundImage/Error opening >%s< file\n", 
            image_filename);
    return(-1);
    }

// Get size of image
  width_image0 = my_image0->GetWidth();
  height_image0 = my_image0->GetHeight();
  ratio_image0 = (double) height_image0 / (double) width_image0;
  ratio_plot0 = (double) height_plot0 / (double) width_plot0;

// Determine the start,end position, and size of scaled image
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
   xx0 = x_image;
   yy0 = y_image;

// Rescale the image:
   my_image0->Rescale(width_image1, height_image1, wxIMAGE_QUALITY_NORMAL);

 } else if (image_style0 == FILL ) {
   xx0 = 0;
   yy0 = 0;
// Scale input image to image1
   width_image1 = width_plot0;
   height_image1 = height_plot0;
   my_image0->Rescale(width_image1, height_image1, wxIMAGE_QUALITY_NORMAL);

 } else if ( image_style0 == SCALE ) {
// Scale input image_file to image1
   if ( ratio_image0 <= ratio_plot0 ) {
      width_image1 = width_plot0;
      height_image1 = roundint(width_image1 * ratio_image0);
      }
   else
      {
      height_image1 = height_plot0;
      width_image1 = roundint(height_image1 / ratio_image0);
      }
   my_image0->Rescale(width_image1, height_image1, wxIMAGE_QUALITY_NORMAL);
   xx0 = 0;
   yy0 = 0;
  } else if ( image_style0 == ZOOM ) {
// Copy input image_file to image2
   if ( ratio_image0 <= ratio_plot0 ) {
      height_image1 = height_image0;
      width_image1 = roundint(height_image1 / ratio_plot0);
      x_image = roundint((width_image0 - width_image1) / 2.0);
      y_image = 0;
      }
   else
      {
      width_image1 = width_image0;
      height_image1 = roundint(width_image1 * ratio_plot0);
      x_image = 0;
      y_image = roundint((height_image0 - height_image1) / 2.0);
      }
    xx0 = 0;
    yy0 = 0;
    new_size.x = width_image1;
    new_size.y = width_image1;
    my_image0->Resize(new_size, wxPoint(0,0));
  }

// Check if the image has a alpha plane:
 if(my_image0->HasAlpha() == false) {
// If not create it and set it to wxIMAGE_ALPHA_OPAQUE
    my_image0->InitAlpha();
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
         my_image0->SetAlpha(i-1, j-1, wxIMAGE_ALPHA_TRANSPARENT);
      }
    }
 }
// Create a bitmap with the current display depth
   my_bitmap0 = new wxBitmap(*my_image0, -1);

// Determine the offset dx0, dy0  according to text_anchor0:
   jlp_wxwid_decode_anchor(text_anchor0, width_image1, height_image1, 
                           &dx0, &dy0);
   xx0 = x0 + dx0;
   yy0 = y0 + dy0;

// Draw the bitmap at x0, y0 on the destination device context:
   gseg_backup_dc->DrawBitmap(*my_bitmap0, xx0, yy0, true);

// Postscript:
   if(gseg_backup_pst_dc != NULL) {
// Get size of image
    nx0 = my_image0->GetWidth();
    ny0 = my_image0->GetHeight();
// Enlarge image acording to x_scale, y_scale :
    new_nx0 = nx0 * x_wx_to_pst_scale1;
    new_ny0 = ny0 * y_wx_to_pst_scale1;
    my_resized_bitmap0 = new wxBitmap(
                  (*my_image0).Scale(new_nx0, new_ny0, wxIMAGE_QUALITY_NORMAL));
    jlp_convert_wx_to_pst(xx0, yy0, &xx_0, &yy_0, &in_frame);
    gseg_backup_pst_dc->DrawBitmap(*my_resized_bitmap0, xx_0, yy_0);
    delete my_resized_bitmap0;
   }

delete my_bitmap0;
delete my_image0;

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
