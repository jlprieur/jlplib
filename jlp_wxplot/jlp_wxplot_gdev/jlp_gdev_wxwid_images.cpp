/*************************************************************************
* JLP_GDev_wxWID Class
* Most of the routines include here are needed by JLP_GDev virtual class
*
* JLP
* Version 09/03/2017
**************************************************************************/
#include "jlp_gdev.h"
#include "jlp_gdev_wxwid.h"
#include "jlp_wx_cursor.h"
#include "jlp_wxgdev_labels.h"   // JLP_wxGDevLabels class
#include "jlp_wxgdev_shapes.h"   // JLP_wxGDevShapes class
#include "jlp_wx_gdproc1.h"      // For JLP_GDProc1
#include "jlp_wxgdev_popup.h"    // For JLP_wxGDev_Popup
#include "jlp_wx_scrolled1.h"    // JLP_wxScrolledImage

#define icon_bitmap_width 20
#define icon_bitmap_height 20
/* Cf User Manual p 60
static char icon_bitmap_bits[] = {
0x60, 0x00, 0x01, 0xb0, 0x00, 0x07, 0x0c, 0x03, 0x00, 0x04, 0x04, 0x00,
0xc2, 0x18, 0x00, 0x03, 0x30, 0x00, 0x01, 0x60, 0x00, 0xf1, 0xdf, 0x00,
0xc1, 0xf0, 0x01, 0x82, 0x01, 0x00, 0x02, 0x03, 0x00, 0x02, 0x0c, 0x00,
0x02, 0x38, 0x00, 0x04, 0x60, 0x00, 0x04, 0xe0, 0x00, 0x04, 0x38, 0x00,
0x84, 0x06, 0x00, 0x14, 0x14, 0x00, 0x0c, 0x34, 0x00, 0x00, 0x00, 0x00};
*/

/*
#define DEBUG
*/

#ifndef PI
#define PI 3.1415926536
#endif

/* Contained here:
  void Update_JGC_from_wxGDev_settings();
  void Update_wxGDev_settings_from_JGC();
***/

/***************************************************************
 int screen1 : screen number (usually 0)
 int nvec: number of vectors to be drawn
 int max_nvec: max number of vectors allocated
 Cursor cursor: graphics cursor to be used
****************************************************************/

/*************************************************************
* Constructor for images (from JLP_wxImagePanel)
*************************************************************/
JLP_GDev_wxWID::JLP_GDev_wxWID(JLP_wxImagePanel *parent_ipanel, wxWindowID id,
                                 wxStaticText *static_coord,
                                 wxStaticText *static_help,
                                 const wxPoint pos0, const wxSize size0,
                                 const int should_fit_inside)
// OLD VERSION : implicit creation of the scrolled window:
// : wxScrolledWindow((wxWindow *)parent_ipanel, id, pos0, size0,
//   wxHSCROLL | wxVSCROLL | wxBORDER_SUNKEN)
   : wxPanel((wxPanel *)parent_ipanel, id, pos0, size0, wxSUNKEN_BORDER)
{

 initialized = 0;
 m_parent_ipanel = parent_ipanel;
 m_parent_gpanel = NULL;
 m_StatusBar = NULL;
 m_StaticCoord = static_coord;
 m_StaticHelp = static_help;
 m_size = size0;
 should_fit_inside1 = should_fit_inside;

 MainInitForImages();

// Create the scrolled image panel
 CreateScrolledImagePanel(pos0, size0);

 initialized = 1234;
}

/*************************************************************
* Constructor for images (from JLP_wxImagePanel)
*************************************************************/
JLP_GDev_wxWID::JLP_GDev_wxWID(JLP_wxImagePanel *parent_ipanel,
                                 wxWindowID id, wxStatusBar *parent_status_bar,
                                 const wxPoint pos0, const wxSize size0,
                                 const int should_fit_inside)
// OLD VERSION : implicit creation of the scrolled window:
// : wxScrolledWindow((wxWindow *)parent_ipanel, id, pos0, size0,
//   wxHSCROLL | wxVSCROLL | wxBORDER_SUNKEN)
   : wxPanel((wxPanel *)parent_ipanel, id, pos0, size0, wxSUNKEN_BORDER)
{

 initialized = 0;
 m_parent_ipanel = parent_ipanel;
 m_parent_gpanel = NULL;
 m_StatusBar = parent_status_bar;
 m_StaticCoord = NULL;
 m_StaticHelp = NULL;
 m_size = size0;
 should_fit_inside1 = should_fit_inside;

 MainInitForImages();

// Create the scrolled image panel
 CreateScrolledImagePanel(pos0, size0);

 initialized = 1234;
}
/*************************************************************
* Create the scrolled image panel
*************************************************************/
void JLP_GDev_wxWID::CreateScrolledImagePanel(const wxPoint pos0,
                                              const wxSize size0)
{
// m_scrolled_window = new wxScrolledWindow(this, wxID_ANY, pos0, size0,
//                                     wxHSCROLL | wxVSCROLL | wxBORDER_SUNKEN);
  m_scrolled_window = new JLP_wxScrolled1(this, wxID_ANY);

}
/*************************************************************
* Initialization for images
*************************************************************/
void JLP_GDev_wxWID::MainInitForImages()
{
char plotdev[64], out_filename[64], title[64];
int status, nx0, ny0, nz0, gamma10, gamma_d0, i, j;
double *image_d0;
wxString fname0;
wxSize min_bitmap_size;
char err_messg[128];
wxString buffer;

 nx2 = BITMAP_WIDTH;
 ny2 = BITMAP_HEIGHT;

 strcpy(plotdev, "wxdisplay");
 strcpy(title, "Gdisplay1");

// Dummy image:
// Setting the parameters to dummy values:
 strcpy(out_filename, "Gdisplay1_output.txt");
 nx0 = 128; ny0 = 128; nz0 = 1;
// gamma1 is the ratio (size of input real image / size of integer LUT image)
// gamma_d: Reduction/magnification factor
 gamma10 = 1; gamma_d0 = 1;
 image_d0 = new double[nx0 * ny0];
 for(i = 0; i < nx0; i++)
 for(j = 0; j < ny0; j++) {
   image_d0[i + j * nx0] = (double)j;
   }

 fname0 = wxT("No File");

// Reset backup_dc, popup menu parameters, etc.
 ResetAllPrivateParametersForImages();

// Init plot parameters (and call JLP_GDev_wxWID::open())
// and set Jgc0.gdev_graphic_type=3
 status = SetupForImage(this, plotdev, out_filename, title,
                        image_d0, nx0, ny0, nz0, &gamma10, &gamma_d0,
                        err_messg);
 if(status) {
  buffer.Printf(wxT("SetupForImage/Error %s"), err_messg);
  wxMessageBox(buffer, wxT("JLP_wx_GDev_wxWID/MainInitForImages"),
               wxOK | wxICON_ERROR);
  }

// New processing setup
 m_gdproc1 = new JLP_wx_GDProc1(this);

// New labels:
 m_wxlabels1 = new JLP_wxGDevLabels();

// New shapes:
 m_wxshapes1 = new JLP_wxGDevShapes();

// Create popup menu:
 m_popup_menu1 = NULL;
 m_popup_menu1 = new JLP_wxGDev_Popup(this, wxgdev_settings1,
                                      Jgc0.gdev_graphic_type);

// Connection of events to this window and to the scrolled window
// m_popup_menu1->ConnectAllPopupMenuEvents();

 work_image1 = new JLP_wxImage1(image_d0, nx0, ny0, m_size,
                                should_fit_inside1, MaxColorLevelForLUT);
 LoadNewCImage1(work_image1);


delete[] image_d0;
};
/*************************************************************
* Reset all private parameters of gdev settings and some others
*
* Some parameters are also initialized in "SetupForImage()"
*************************************************************/
void JLP_GDev_wxWID::ResetAllPrivateParametersForImages()
{
// Fonts
   strcpy(font_name1, "default");
   font_size1 = 12;

// GSEG parameters:
  jlp_gseg_wxwid1 = NULL;
  jlp_gsegraf1 = NULL;
  gseg_is_activated = 0;
  gseg_iplot_coords1 = 1;
  gseg_iplot_contours1 = 1;

// Other parameters:
  c_image1 = NULL;
  work_image1 = NULL;
  backup_dc_bitmap2 = NULL;
  backup_pst_dc1 = NULL;
  backup_dc = NULL;

  wxgdev_settings1.pen_colour = *wxWHITE;
  wxgdev_settings1.pen_default_colour = *wxWHITE;
  wxgdev_settings1.backgd_colour = *wxBLACK;
  wxgdev_settings1.cursor_type= _T("Arrow");
// lut_type: 'l'=log rainbow1 'r'=rainbow2 's'=saw 'g'=gray 'c'=curves
//           'p'=pisco
  strcpy(wxgdev_settings1.lut_type, "pisco");
  wxgdev_settings1.lut_reversed = 0;
  wxgdev_settings1.itt_type = wxT("MinMax");
// Lin/log ITT:
  wxgdev_settings1.itt_is_linear = 0;
  wxgdev_settings1.zoom = 1;
// 0=NONE
  wxgdev_settings1.filter = 0;
  wxgdev_settings1.low_itt_thresh = 0;
  wxgdev_settings1.up_itt_thresh = 0;

// InternalProcessingMode
// -1=None 0=Automatic thresholds 1=Statistics, 2=Astrometry 3=Photometry
// 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
// 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
// 13=Add a ring 14=Remove a shape
  wxgdev_settings1.InternalProcessingMode = -1;

  active_gdproc = NULL;
  m_gdproc1 = NULL;
  m_label1 = _T("");

// Init all parameters for images:
// Box type (0: plain, 1:nice, 2:elaborated):
  wxgdev_settings1.box_type = 0;
  wxgdev_settings1.ticks_in = 1;
  wxgdev_settings1.xgrid = 0;
  wxgdev_settings1.ygrid = 0;

// Filename (used for the title of gseg frame)
  strcpy(filename_1, "");

return;
}
/***********************************************************************
*
***********************************************************************/
void JLP_GDev_wxWID::Load_wxGDevSettings(wxGDev_SETTINGS wxgdev_settings0)
{
  wxgdev_settings1.pen_colour = wxgdev_settings0.pen_colour;
  wxgdev_settings1.pen_default_colour = wxgdev_settings0.pen_default_colour;
  wxgdev_settings1.backgd_colour = wxgdev_settings0.backgd_colour;
  wxgdev_settings1.cursor_type= wxgdev_settings0.cursor_type;
  strcpy(wxgdev_settings1.lut_type, wxgdev_settings0.lut_type);
  wxgdev_settings1.lut_reversed = wxgdev_settings0.lut_reversed;
  wxgdev_settings1.itt_type = wxgdev_settings0.itt_type;
  wxgdev_settings1.itt_is_linear = wxgdev_settings0.itt_is_linear;
  wxgdev_settings1.zoom = wxgdev_settings0.zoom;
  wxgdev_settings1.filter = wxgdev_settings0.filter;
  wxgdev_settings1.low_itt_thresh = wxgdev_settings0.low_itt_thresh;
  wxgdev_settings1.up_itt_thresh = wxgdev_settings0.up_itt_thresh;
  wxgdev_settings1.InternalProcessingMode = wxgdev_settings0.InternalProcessingMode;
  wxgdev_settings1.box_type = wxgdev_settings0.box_type;
  wxgdev_settings1.ticks_in = wxgdev_settings0.ticks_in;
  wxgdev_settings1.xgrid = wxgdev_settings0.xgrid;
  wxgdev_settings1.ygrid = wxgdev_settings0.ygrid;
return;
}
/***********************************************************************
* Update JGdev graphic context parameters with current private variables
* displayed by the popup menu
*
***********************************************************************/
void JLP_GDev_wxWID::Update_JGC_from_GDProcSettings()
{
  init_JGC(Jgc0.fdv_pst_fname, Jgc0.offx, Jgc0.offy, Jgc0.axlen, Jgc0.aylen,
           Jgc0.expand, Jgc0.axis_limits[0], Jgc0.axis_limits[1],
           Jgc0.axis_limits[2], Jgc0.axis_limits[3],
           Jgc0.axis_limits[4], Jgc0.axis_limits[5],
           wxgdev_settings1.box_type, Jgc0.box_plan,
           wxgdev_settings1.xgrid, wxgdev_settings1.ygrid,
           wxgdev_settings1.ticks_in,
           Jgc0.box_xlabel, Jgc0.box_ylabel, Jgc0.box_zlabel, Jgc0.box_title,
           Jgc0.xaxis_type, Jgc0.yaxis_type, Jgc0.zaxis_type, Jgc0.dev_type,
           client_width1, client_height1, Jgc0.TeX_flag,
           Jgc0.xmin_user, Jgc0.xmax_user, Jgc0.ymin_user,
           Jgc0.ymax_user, Jgc0.zmin_user, Jgc0.zmax_user, Jgc0.image_f,
           Jgc0.nx, Jgc0.ny, Jgc0.nz, Jgc0.dev_idv);
return;
}
/*************************************************************************
* Update the parameters used by the popup menu
* Update some private variables using the JGdev graphic context parameters
*
********************* *****************************************************/
void JLP_GDev_wxWID::Update_GDProcSettings_from_JGC()
{
  wxgdev_settings1.box_type = Jgc0.box_type;
  wxgdev_settings1.xgrid = Jgc0.box_xgrid;
  wxgdev_settings1.ygrid = Jgc0.box_ygrid;
  wxgdev_settings1.ticks_in = Jgc0.box_ticks_in;
  client_width1 = Jgc0.dev_width;
  client_height1 = Jgc0.dev_height;
return;
}
/**************************************************************************
* For images
* Setup window parameters according to the device name
*
* INPUT:
* plotdev: "wxDisplay", "wxdisplay", "wxImage", or "wximage"
*          "wxImage_Landscape", or "wxImage_Square"
* title
* nx1, ny1
*
* OUTPUT:
* offx, offy, axlen, aylen
* dev_width, dev_height : size of window in device coordinates
* TeX_flag
* dev_type1 = 1 if X11, 2 if postscript, 3 if widgets ...
***************************************************************************/
int JLP_GDev_wxWID::setup_device_for_image(const char *plotdev,
                                      const char* title,
                                      const int nx1, const int ny1, int *offx,
                                      int *offy, int *axlen, int *aylen,
                                      int *dev_width, int *dev_height,
                                      int *TeX_flag, int *dev_type,
                                      int *landscape)
{
int status = 0;
wxString buffer;

// OLD VALUES
  *offx = 3500; *offy = 3500; *axlen = 26000; *aylen = 26000;

// Arbitrary values (changed later by Resize)
  *dev_width = 300; *dev_height = 300;

// Device type: 1 = X11, 2 = Postscript file,  3 = wxWidgets
  *dev_type = 3;

// Assuming no rotation:
  *landscape = 0;

/* Display of an image with wxWidgets with zoom=1:
  ("wxDisplay" or "wxdisplay")
*/
  if(!strncmp(plotdev, "wxD", 3) || !strncmp(plotdev, "wxd", 3)) {
      *offx = 0.;
      *offy = 0.;
      *axlen  = SCREEN_SIZE;
      *aylen  = SCREEN_SIZE;
  } else {
      *dev_type = 0;
      fprintf(stderr, "setup_device_for_image/Error: unknown wxWidgets graphic device\n (%s is unknown)\n", plotdev);
      status = -1;
  }

// TeX flag
  *TeX_flag = 0;

// Maximum level for LUT values :
  MaxColorLevelForLUT = 255;

return(status);
}
/*************************************************************
* Open the wxWidgets panel for image/curve display
* called both by JLP_GDev::SetupForCurves() and JLP_GDev::SetupForImages()
*
* INPUT:
* title: string to be displayed on top of the window
* dev_width1, dev_height1: size of window in device coordinates
*
* Jgc0 structure
*
* OUTPUT:
* Jgc0.dev_width, Jgc0.dev_height: window size in device coordinates
************************************************************/
int JLP_GDev_wxWID::open_device(const char* title, int dev_width1,
                                int dev_height1, const int landscape,
                                int *jgc_dev_width, int *jgc_dev_height,
                                int *dev_yorigin_is_on_top)
{
wxString buffer;

// Y axis starts on top:
Jgc0.dev_yorigin_is_on_top = 1;

/*
  printf(" WARNING: This program assumes %d bits per pixel of video board (%d colors)\n",
        BIT_PER_VIDEO_PIXEL, (int)pow(2.,(double)BIT_PER_VIDEO_PIXEL));
*/

 if(dev_width1 <= 0 || dev_height1 <= 0) {
   buffer.Printf(wxT("Fatal error: dev_width=%d dev_height=%d\n"),
           dev_width1, dev_height1);
   wxMessageBox(buffer, wxT("JLP_GDev_wxWID::open"), wxOK | wxICON_ERROR);
   exit(-2);
   }

// JLP 2012: to solve problem with the bottom of the plots:
 Jgc0.dev_width = dev_width1;
 Jgc0.dev_height = (int)((double)dev_height1 * 0.9);

// Correspondance of "jlp_X11.h" with "mongo.h":
 FromJgc0ToMgc0();

// Initialization of bitmap2 and backup_dc
 backup_dc_bitmap2 = NULL;
 backup_dc = new wxMemoryDC();
/* DEBUG
 printf("open_device/calling InitBackupDC with: %d %d \n",
       dev_width1, dev_height1);
*/
 InitBackupDC(dev_width1, dev_height1);

// Postscript backup DC (for GSEG routines):
 backup_printer1 = NULL;
 backup_pst_dc1 = NULL;

// Build an object to allow further correspondance to GSEG routines:
 jlp_gseg_wxwid1 = new JLP_Gseg_Wxwid(dev_width1, dev_height1, backup_dc,
                                      backup_pst_dc1);
 jlp_gsegraf1 = NULL;

*jgc_dev_width = Jgc0.dev_width;
*jgc_dev_height = Jgc0.dev_height;
*dev_yorigin_is_on_top = Jgc0.dev_yorigin_is_on_top;

 return(0);
}
/***************************************************************
* Not available with wxwidgets
***************************************************************/
int JLP_GDev_wxWID::wait_for_events()
{
return(-1);
}
/***************************************************************
* get_winlimits
* Draw rectangles or lines using cursorgc graphic context (xor function)
* Get limits of a small window (entered interactively by the user)
*
* INPUT:
* type_of_win:  1=line; >1 =rectangle
* (to allow for Slice/Zoom/Statistics
*
* OUTPUT:
* u_x1, u_x2, u_y1, u_y2: user coordinates
* pressed_button:   1 if button#1,  2 if button#2,  3 if button#3
*
* JLP96
***************************************************************/
int JLP_GDev_wxWID::get_winlimits(double *u_x1, double *u_y1, double *u_x2,
                                  double *u_y2, int type_of_win,
                                  int *pressed_button, int *in_frame)
{
return(0);
}
/***************************************************************
* get_circles
* Draw the circles using cursorgc graphic context (xor function)
*
* To get one or two concentric circles for circular photometry
* or patch applications
*
* INPUT:
* ncirc: number of circles that are needed (1 or 2)
*
* OUTPUT:
*
* x_cent, y_cent: (user) coordinates of the center
* diam1, diam2: (user) diameters of the small and big circles
*
***************************************************************/
int JLP_GDev_wxWID::get_circles(double *x_cent, double *y_cent, double *diam1,
                                double *diam2, int *ncirc)
{
return(0);
}
/***************************************************************
* Update display on the screen
*
***************************************************************/
int JLP_GDev_wxWID::gdev_gflush()
{

// Test if class parameters have been initialized:

/* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
*/
  if(Jgc0.gdev_graphic_type != 3) {
    wxGdev_Refresh();
  }

return(0);
}
/*********************************************************************
* Close device
*
*********************************************************************/
int JLP_GDev_wxWID::close_device()
{

/*
printf("JLP_Gdev_wxWID/Warning: initialized = %d \n", initialized);
printf("JLP_Gdev_wxWID/Warning you have called close_device (is it OK?)\n");
*/
// NEW:
  if(m_gdproc1 != NULL) delete m_gdproc1;
  if(backup_dc != NULL) delete backup_dc;
  if(work_image1 != NULL) delete work_image1;
  if(m_wxlabels1 != NULL) delete m_wxlabels1;
  if(m_wxshapes1 != NULL) delete m_wxshapes1;

return(0);
}
/*************************************************************************
* Set Client Size to (new_size.x new_size.y)
* and update scroll bars
**************************************************************************/
void JLP_GDev_wxWID::SetNewSize(wxSize new_size)
{
if(initialized != 1234 || c_image1 == NULL) return;

// Compute zoom factor for this new size:
  c_image1->Compute_gamma_for_window(new_size);

// Create a new bitmap with this zoom factor:
  c_image1->UpdateBitmap();

// Update user/device conversion parameters (in Jgc0) :
  SetNewDeviceSize(new_size.x, new_size.y);

// Set Client Size to (nx2, ny2) and update scroll bars
  MySetSize();

return;
}
/*************************************************************************
* Set Client Size to (nx2, ny2) and update scroll bars
**************************************************************************/
void JLP_GDev_wxWID::MySetSize()
{
int iwidth, iheight;

if(initialized != 1234 || c_image1 == NULL) return;

// Initialize backup device context (used as a buffer for a faster display):
// with the new size of the bitmap of c_image1
  iwidth = c_image1->Get_nx2();
  iheight = c_image1->Get_ny2();
/* DEBUG
  printf("MySetSize/calling InitBackupDC with: %d %d \n", iwidth, iheight);
*/
  InitBackupDC(iwidth, iheight);

// To avoid blanks at the edges, should take small values for scroll rate:
  if(m_scrolled_window != NULL) {
// Fit into the panel
   m_scrolled_window->SetSize(Jgc0.dev_width, Jgc0.dev_height);
// Set scrolling parameters:
   m_scrolled_window->SetScrollRate( 2, 2 );
   m_scrolled_window->SetVirtualSize(iwidth, iheight);
// Set background colour:
   m_scrolled_window->SetBackgroundColour(wxColour(*wxBLACK));
   }

return;
}
/*************************************************************************
*
**************************************************************************/
void JLP_GDev_wxWID::GetImageLabelOptions(bool *has_labels, bool *has_scale_bar,
                                          bool *has_north_east,
                                          bool *has_shapes)
{
 *has_labels = false;
 *has_scale_bar = false;
 *has_north_east = false;
 *has_shapes = false;
 if(m_wxlabels1 != NULL) {
   *has_labels = m_wxlabels1->HasLabels();
   *has_scale_bar = m_wxlabels1->HasScaleBar();
   *has_north_east = m_wxlabels1->HasNorthEast();
   }
 if(m_wxshapes1 != NULL) *has_shapes = (m_wxshapes1->NShapes() > 0);

return;
}
