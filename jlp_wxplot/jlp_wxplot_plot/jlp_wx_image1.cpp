/******************************************************************************
* Name:        JLP_wxImage1 class
* Purpose:     Parameters used for displaying a 2D image with wxImage
* Author:      JLP
* Version:     03/02/2016
******************************************************************************/
// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif

#include "jlp_wx_image1.h"
#include "jlp_gdev.h"          // jlp_change_lut_full()
#include "jlp_itt1.h"

#define BYTE unsigned char

// Contained here:
static int ConvertDoubleToColors(const double *dble_image, const int nx,
                                 const int ny, BYTE *image_rgb,
                                 const int *LUT_r, const int *LUT_g,
                                 const int *LUT_b, const int NColors,
                                 const int x1_box0, const int y1_box0,
                                 const int x2_box0, const int y2_box0,
                                 double *min_val, double *max_val,
                                 const wxString ITT_type,
                                 const int ITT_is_linear);

/************************************************************************
* Constructor from a wxImage as input
************************************************************************/
JLP_wxImage1::JLP_wxImage1(wxImage image)
{
BYTE rr, gg, bb, *image_data;
int i, should_fit_inside0 = 0;
int nx0, ny0;
// Dummy values for width and height of window:
wxSize window_size0(500, 300);

// Get initial size:
  nx0 = image.GetWidth();
  ny0 = image.GetHeight();

 m_bitmap2 = NULL;
 work_image = NULL;
 m_image1 = NULL;
 image1_rgb = NULL;
 LUT_r = NULL;
 LUT_g = NULL;
 LUT_b = NULL;
 dble_image1 = NULL;
 orig_dble_image1 = NULL;

// Allocate memory:
  Init_PrivateParameters(nx0, ny0, window_size0, should_fit_inside0);

// Retrieve the input data pointer
  image_data = image.GetData();

// Copy data to image1_rgb array:
  for(i = 0; i < 3*nx1 *ny1; i++) image1_rgb[i] = image_data[i];

// Create m_image1 from image1_rgb:
// (Static=true, to prevent from erasing this array when deleting m_image1)
  m_image1 = new wxImage(nx1, ny1, image1_rgb, true);
/*
NB:
 An interesting property is that when image1_rgb is modified
 those changes are immediately visible on m_image1
 since the data pointer of m_image1 is image1_rgb!
*/

// Allocate memory and copy mean of input RGB array to dble_image1:
  for(i = 0; i < nx1 *ny1; i++) {
     rr = image_data[3*i];
     gg = image_data[3*i + 1];
     bb = image_data[3*i + 2];
     dble_image1[i] = (double)(rr + gg + bb);
     orig_dble_image1[i] = dble_image1[i];
     }

// Create initial bitmap m_bitmap2 (from m_image1)
  UpdateBitmap();

return;
}
/************************************************************************
*
* INPUT:
* nx10, ny10
************************************************************************/
void JLP_wxImage1::Init_PrivateParameters(const int nx10, const int ny10,
                                          const wxSize window_size0,
                                          const int should_fit_inside0)
{
int i;

  nx1 = nx10;
  ny1 = ny10;
  should_fit_inside1 = should_fit_inside0;

// Compute new values nx2, ny2 for the initial size of the window and nx1,ny1:
  Compute_gamma_for_window(window_size0);

  image1_rgb = new BYTE[nx1 * ny1 * 3];
// Initialize image1_rgb array:
  for(i = 0; i < 3*nx1 *ny1; i++) image1_rgb[i] = 0;

// Allocate memory
  dble_image1 = new double[nx1 * ny1];
  orig_dble_image1 = new double[nx1 * ny1];

// Dummy values:
  for(i = 0; i < nx1 *ny1; i++) {
      dble_image1[i] = 0.;
      orig_dble_image1[i] = 0.;
      }

// Allocate memory and initialize the LUT:
  LUT_r = new int[NColors];
  LUT_g = new int[NColors];
  LUT_b = new int[NColors];
// Simple LUT in the case of GIG/JPEG, etc, in order to have a correct display:
  for(i = 0; i < NColors; i++) {
      LUT_r[i] = i;
      LUT_g[i] = i;
      LUT_b[i] = i;
      }

// Create initial bitmap m_bitmap2 (from m_image1)
  m_bitmap2 = NULL;
  work_image = NULL;

// Should be allocated only with malloc ?
//  image1_rgb = (BYTE *)malloc(sizeof(BYTE) * nx1 * ny1 * 3);
  image1_rgb = new BYTE[nx1 * ny1 * 3];
// Set parameters that are needed if we want this constructor to be compatible
// with the FITS ones:
 ITT_is_linear = 1;
 ITT_type = wxT("MinMax");
 Low_Threshold = 0.;
 Up_Threshold = 0.;

// LUT='pisco'
 strcpy(lut_type1, "pisco");
 LUT_ioff = NColors/2;
 LUT_islope = NColors/2;
 LUT_reversed = 0;

return;
}
/************************************************************************
* Constructor with no input parameters
************************************************************************/
JLP_wxImage1::JLP_wxImage1()
{
int nx0 = 32, ny0 = 32;
int should_fit_inside0 = 0;
// Dummy values for width and height of window:
wxSize window_size0(500, 300);

 m_bitmap2 = NULL;
 work_image = NULL;
 m_image1 = NULL;
 image1_rgb = NULL;
 LUT_r = NULL;
 LUT_g = NULL;
 LUT_b = NULL;
 dble_image1 = NULL;
 orig_dble_image1 = NULL;

 Init_PrivateParameters(nx0, ny0, window_size0, should_fit_inside0);

// Create initial bitmap m_bitmap2 (from m_image1)
  UpdateBitmap();

return;
}
/************************************************************************
* Constructor from a double precision array as input
************************************************************************/
JLP_wxImage1::JLP_wxImage1(double *dble_image, int nx, int ny,
                           wxSize window_size0, const int should_fit_inside0,
                           int max_lut_level)
{
 m_bitmap2 = NULL;
 work_image = NULL;
 m_image1 = NULL;
 image1_rgb = NULL;
 LUT_r = NULL;
 LUT_g = NULL;
 LUT_b = NULL;
 dble_image1 = NULL;
 orig_dble_image1 = NULL;

 Init_PrivateParameters(nx, ny, window_size0, should_fit_inside0);

 JLP_SetupFromDouble(dble_image, nx, ny, window_size0, should_fit_inside0,
                     max_lut_level);

 jlp_change_lut_full(lut_type1, LUT_reversed, &LUT_ioff, &LUT_islope,
                     LUT_r, LUT_g, LUT_b, NColors, max_lut_level);
return;
}
/************************************************************************
* Called by the constructors from a double precision array as input
************************************************************************/
int JLP_wxImage1::JLP_SetupFromDouble(double *dble_image, int nx, int ny,
                                      const wxSize window_size0,
                                      const int should_fit_inside0,
                                      int max_lut_level)
{
int i, status;

// Allocate memory
  Init_PrivateParameters(nx, ny, window_size0, should_fit_inside0);

// Copy input array to dble_image1:
  for(i = 0; i < nx1 * ny1; i++) {
     dble_image1[i] = dble_image[i];
     orig_dble_image1[i] = dble_image[i];
     }


// Use private variables: nx1, ny1, x1_box, y1_box, x2_box, y2_box
  status = ConvertDoubleToColors(dble_image1, nx1, ny1, image1_rgb,
                                 LUT_r, LUT_g, LUT_b, NColors, x1_box, y1_box,
                                 x2_box, y2_box, &Low_Threshold,
                                 &Up_Threshold, ITT_type, ITT_is_linear);
  if(status){
   fprintf(stderr,"JLP_SetupFromDouble/Error in ConvertDoubleToColors !\n");
   return(-1);
   }

/* Create new wxImage m_image1
 wxImage(int width, int height, unsigned char* data, bool static_data = false)

 Creates an image from given data with the given width and height.
 If static_data is true, then wxImage will not delete the actual image data
 in its destructor, otherwise it will free it by calling free().
 (In this case, it should have been allocated with malloc...)
NB:
 An interesting property is that when image1_rgb is modified
 those changes are immediately visible on m_image1
 since the data pointer of m_image1 is image1_rgb!
*/
 m_image1 = new wxImage(nx1, ny1, image1_rgb, true);

// Create initial bitmap m_bitmap2 (from m_image1)
  m_bitmap2 = NULL;
  UpdateBitmap();

return(0);
}
/***************************************************************************
* To avoid memory leaks...
***************************************************************************/
void JLP_wxImage1::MyFreeMemory()
{
 if(work_image != NULL) delete work_image;
 work_image = NULL;
 if(m_image1 != NULL) m_image1->Destroy();
 m_image1 = NULL;
 if(image1_rgb != NULL) delete[] image1_rgb;
 image1_rgb = NULL;
 if(LUT_r != NULL) delete[] LUT_r;
 LUT_r = NULL;
 if(LUT_g != NULL) delete[] LUT_g;
 LUT_g = NULL;
 if(LUT_b != NULL) delete[] LUT_b;
 LUT_b = NULL;
 if(dble_image1 != NULL) delete[] dble_image1;
 dble_image1 = NULL;
 if(orig_dble_image1 != NULL) delete[] orig_dble_image1;
 orig_dble_image1 = NULL;

// At the end...
 if(m_bitmap2 != NULL) delete m_bitmap2;
 m_bitmap2 = NULL;

return;
}
/***************************************************************************
* Convert the double precision input image
* to colors using the ITT and the LUT
*
* NB:
* An interesting property is that when image1_rgb is modified
* those changes are immediately visible on m_image1
* since the data pointer of m_image1 is image1_rgb!
*
* INPUT:
*   dble_image: double precision image
*   nx, ny: size of dble_image
*   LUT_r, LUT_g, LUT_b: Look Up Table used for display
*   NColors: size of LUT_r, LUT_g, LUT_b
*   x1_box0, y1, box0, x2_box0, y2_box0 : box used for computing
*                                         the thresholds if ITT is "FromBox"
*   min_val, max_val: threshold values (Compute new thresholds if needed:
*      (if Direct Input do not change the input values)
*   ITT_type: "MinMax", "DirectInput", "Interactiveselection", "HC", etc
*
*
* OUTPUT:
*   image_rgb
*
***************************************************************************/
static int ConvertDoubleToColors(const double *dble_image, const int nx,
                                 const int ny, BYTE *image_rgb,
                                 const int *LUT_r, const int *LUT_g,
                                 const int *LUT_b, const int NColors,
                                 const int x1_box0, const int y1_box0,
                                 const int x2_box0, const int y2_box0,
                                 double *min_val, double *max_val,
                                 const wxString ITT_type,
                                 const int ITT_is_linear)
{
BYTE *ima_k;
int i, k;
char itt_type0[64];

ima_k = new BYTE[nx * ny];

/*
// Compute new thresholds if needed:
// (if Direct Input, Interactive Selection, do not change the input values):
void LoadITT(BYTE* out_ima_k, const double* in_ima, const int nx, const int ny,
             double *min, double *max, const wxString ITT_type,
             int ITT_is_linear)
*/

/* Create an Intensity Transfer Table to display a double precision image.
* Compute min_val, max_val according to ITT_type
* This table is filled with the indices k to be used for LUT conversion
* of a double precision image
*/
 strcpy(itt_type0, (const char *)ITT_type.mb_str());
 LoadITT(ima_k, dble_image, nx, ny, x1_box0, y1_box0, x2_box0, y2_box0,
         min_val, max_val, itt_type0, ITT_is_linear);

// Conversion of ima_k to RGB, using the LUT values:
  for(i = 0; i < nx * ny; i++) {
     k = ima_k[i];
     k = MAXI(0, k);
     k = MINI(k, NColors-1);
     image_rgb[3*i] = (BYTE)LUT_r[k];
     image_rgb[3*i+1] = (BYTE)LUT_g[k];
     image_rgb[3*i+2] = (BYTE)LUT_b[k];
     }

delete[] ima_k;
return(0);
}
/************************************************************************
* Compute new values nx2, ny2 for the initial size of the window:
* Called also at each RESIZE event (JLP2015) to update zoom factor
* automatically
*
* gamma1: compression of the input image used for a smaller display
*          (in the case of large images)
* gamma_d: magnification of the input image used for a larger display
*          (in the case of small images)
*
* INPUT:
*  nx1, ny1: size of input image
*  window_size: (800,600) is good
*  should_fit_inside : true if the full image should fit in the window
*
* OUTPUT:
*  nx2, ny2: size of output image
*  gamma_d : ratio nx2/nx1
*  gamma1 : ratio nx1/nx2
************************************************************************/
void JLP_wxImage1::Compute_gamma_for_window(wxSize window_size)
{
int frac_x, frac_y, zoom_fact, offset = 20;
// JLP2016: it is better with offset=20...

// Compute new values nx_out, ny_out for the initial size of the window:

if(should_fit_inside1 == 1) {
  if((window_size.x + offset > nx1) && (window_size.y + offset > ny1)) {
    frac_x = (int)((double)window_size.x / (double)nx1);
    frac_y = (int)((double)window_size.y / (double)ny1);
    gamma_d = MAXI(1, MINI(frac_x, frac_y));
    gamma1 = 1;
  } else {
    frac_x = (int)((double)nx1 / (double)window_size.x);
    frac_y = (int)((double)ny1 / (double)window_size.y);
    gamma1 = MAXI(1, MINI(frac_x, frac_y));
    gamma_d = 1;
  }
} else {
  if((window_size.x + offset > nx1) && (window_size.y + offset > ny1)) {
    frac_x = (int)((double)window_size.x / (double)nx1);
    frac_y = (int)((double)window_size.y / (double)ny1);
    gamma_d = MAXI(1, MAXI(frac_x, frac_y));
    gamma1 = 1;
  } else {
    frac_x = (int)((double)nx1 / (double)window_size.x);
    frac_y = (int)((double)ny1 / (double)window_size.y);
    gamma1 = MAXI(1, MAXI(frac_x, frac_y));
    gamma_d = 1;
  }
}

/* DEBUG
printf("DEBUG inside=%d window_size.x=%d y=%d nx1,ny1=%d,%d frac_x,y=%d,%d gamma_d=%d gamma1=%d\n",
         should_fit_inside1, window_size.x, window_size.y, nx1, ny1, frac_x, frac_y, gamma_d, gamma1);
*/

 if(gamma1 == 1)
  zoom_fact = gamma_d;
 else
  zoom_fact = -gamma1;

// Compute new values nx2, ny2 from gamma_d, gamma1, nx1, and ny1:
  ComputeBitmapSize(zoom_fact);

return;
}
/************************************************************************
* ComputeBitmapSize to change the magnification used for displaying the image
* Compute new values nx2, ny2 from gamma_d, gamma1, nx1, and ny1:
*
* INPUT:
* zoom_fact: zoom factor
*
* OUTPUT:
*  nx2, ny2: size of output image
*  gamma_d : ratio nx2/nx1
*  gamma1 : ratio nx1/nx2
************************************************************************/
void JLP_wxImage1::ComputeBitmapSize(int zoom_fact)
{

// Values allowed for positive zoom: 1,2,3,4,5,6,8,10,15,20,40:
 if(zoom_fact > 6 && zoom_fact <= 8) zoom_fact = 8;
 if(zoom_fact > 8 && zoom_fact <= 12) zoom_fact = 10;
 else if(zoom_fact > 12 && zoom_fact <= 17 ) zoom_fact = 15;
 else if(zoom_fact > 17 && zoom_fact <= 28 ) zoom_fact = 20;
 else if(zoom_fact > 28) zoom_fact = 40;

// Values allowed for : 1,2,3,4,5,6,8
 if(zoom_fact <= -8 && zoom_fact < -6) zoom_fact = -8;

  if(zoom_fact > 1) {
    gamma_d = zoom_fact;
    gamma1 = 1;
    nx2 = nx1 * gamma_d;
    ny2 = ny1 * gamma_d;
  } else if(zoom_fact < -1) {
    gamma1 = -zoom_fact;
    gamma_d = 1;
    nx2 = nx1 / gamma1;
    ny2 = ny1 / gamma1;
  } else {
    gamma_d = 1;
    gamma1 = 1;
    nx2 = nx1;
    ny2 = ny1;
  }

return;
}
/************************************************************************
* SetZoom to change the magnification used for displaying the image
* Compute new values nx2, ny2 from gamma_d, gamma1, nx1, and ny1:
*
* INPUT:
* zoom_fact: zoom factor (magnification required by the user)
*
* OUTPUT:
*  nx2, ny2: size of output image
*  gamma_d : ratio nx2/nx1
*  gamma1 : ratio nx1/nx2
************************************************************************/
void JLP_wxImage1::SetZoom(int zoom_fact)
{

// Compute new values nx2, ny2 from gamma_d, gamma1, nx1, and ny1:
  ComputeBitmapSize(zoom_fact);

// Create new bitmap m_bitmap2 (from m_image1)
  UpdateBitmap();

// Then the window size can be changed
// by calling JLP_ImageFrame1::SetSizeAndScrollBars...

return;
}
/************************************************************************
* Update  bitmap m_bitmap2 from current value of m_image1
*
* Rescale in_image (=m_image1) to have a big image on the screen
* as the calling routine will set the Client size to this size.
*
* INPUT:
*  m_image1: wxImage with the input image
*  nx2,ny2
*
* OUTPUT:
*  m_bitmap2: wxBitmap to be displayed on the screen
************************************************************************/
void JLP_wxImage1::UpdateBitmap()
{
int zoom_fact;

// Compute zoom_factor:
  if(gamma1 == 1)
   zoom_fact = gamma_d;
  else
   zoom_fact = -gamma1;

// Compute new values nx2, ny2 from gamma_d, gamma1, nx1, and ny1:
  ComputeBitmapSize(zoom_fact);

// Origin of device coordinate is on top left:
// whereas I want to display the images with origin on bottom left:
// So I need to flip the image vertically:
//   iy1 = c_image1->Get_ny1() - iy1;
/* Examples:
    m_flipped_horiz = wxBitmap(image.Mirror(true));
    m_flipped_vert = wxBitmap(image.Mirror(false));
    m_flipped_both = wxBitmap(image.Mirror(true).Mirror(false));
*/

// Delete work_image
   if(work_image != NULL) delete work_image;

// Create work_image (and copy initial image to working image):
   work_image = new wxImage(*m_image1);

// Rescale work_image to this size
   work_image->Rescale(nx2, ny2);

// Delete bitmap
   if(m_bitmap2 != NULL) delete m_bitmap2;

// Create m_bitmap2 which is used for the display:
   m_bitmap2 = new wxBitmap((*work_image).Mirror(false));

return;
}
/************************************************************************
* Change ITT
*
* Create an Intensity Transfer Table to display a double precision image.
*
* This table is filled with the indices k to be used for LUT conversion
* of a double precision image
*
************************************************************************/
void JLP_wxImage1::Update_with_ITT()
{

// Conversion to colors using LUT and ITT:
// Use private variables: nx1, ny1, x1_box, y1_box, x2_box, y2_box
  ConvertDoubleToColors(dble_image1, nx1, ny1, image1_rgb,
                        LUT_r, LUT_g, LUT_b, NColors, x1_box, y1_box,
                        x2_box, y2_box, &Low_Threshold,
                        &Up_Threshold, ITT_type, ITT_is_linear);

// Update the value of bitmap m_bitmap2 (from m_image1)
  UpdateBitmap();

return;
}
/************************************************************************
* Change LUT
* lut_type: 'l'=log rainbow1 'r'=rainbow2 's'=saw 'g'=gray 'c'=curves
*           'p'=pisco
************************************************************************/
void JLP_wxImage1::Update_with_LUT_and_ITT(int max_lut_level)
{
  jlp_change_lut_full(lut_type1, LUT_reversed, &LUT_ioff, &LUT_islope,
                      LUT_r, LUT_g, LUT_b, NColors,  max_lut_level);

// Conversion to colors using LUT and ITT:
// Use private variables: nx1, ny1, x1_box, y1_box, x2_box, y2_box
  ConvertDoubleToColors(dble_image1, nx1, ny1, image1_rgb,
                        LUT_r, LUT_g, LUT_b, NColors, x1_box, y1_box,
                        x2_box, y2_box, &Low_Threshold,
                        &Up_Threshold, ITT_type, ITT_is_linear);

// Update the value of bitmap m_bitmap2 (from m_image1)
  UpdateBitmap();

return;
}
/************************************************************************
* To obtain information about the current image and display setup
************************************************************************/
void JLP_wxImage1::GetInfo(wxString& InfoMessage)
{
wxString str1, str2, str3;

str1.Printf(wxT(" nx=%d ny=%d \n \
Zoom factor: gamma1=%d gamma_d=%d\n \
Thresholds: min=%.4g max=%.4g\n"),
       nx1, ny1, gamma1, gamma_d,
       Low_Threshold, Up_Threshold);

str2.Printf(wxT(" ITT_is_linear=%d\n"), ITT_is_linear);
str3.Printf(wxT(" LUT_type: %s Ncolors: %d\n"), lut_type1, NColors);

InfoMessage = str1 + wxT(" ITT thresholds: ") + ITT_type + str2
              + str3;

// printf("GetInfo: \n %s\n", (const char *)InfoMessage.mb_str());

return;
}
