/*************************************************************************
* JLP_wxGseg_Canvas Class (for curves)
*
* JLP
* Version 25/01/2017
**************************************************************************/
#include "jlp_splot_def.h"         // BITMAP_WIDTH, etc
#include "jlp_cgdev.h"
#include "jlp_gdev_idv.h"          // free_curve_idv()
#include "jlp_wx_gscanvas.h"
#include "jlp_wx_cursor.h"
#include "jlp_ctime.h"             // JLP_CTIME
#include "jlp_wx_curve_labels.h"   // JLP_wxCurveLabels class
#include "jlp_wx_gsproc1.h"        // For JLP_gsProc1

#include "jlp_gsegraf.h"           // JLP_GSEG_InitializePlot

/*
#define DEBUG
*/
#ifndef PI 
#define PI 3.1415926536
#endif

/***************************************************************
 int screen1 : screen number (usually 0) 
 int nvec: number of vectors to be drawn 
 int max_nvec: max number of vectors allocated 
 Cursor cursor: graphics cursor to be used
****************************************************************/

/*************************************************************
* Basic constructor from wxPanel 
*************************************************************/
 JLP_wxGseg_Canvas::JLP_wxGseg_Canvas(wxPanel *parent_panel, 
                                  wxWindowID id, const wxPoint &pos, 
                                  const wxSize &size, 
                                  wxStaticText *static_coord,
                                  wxStaticText *static_help)
   : wxPanel(parent_panel, id, pos, size, wxSUNKEN_BORDER)
{
char plotdev[64], out_filename[64], title[64];
int plan, status;
float xminuser, xmaxuser, yminuser, ymaxuser;
char err_messg[128];
wxString buffer;

// Creation of the Panel window:
//    m_panel = new wxPanel(parent_panel, id, pos, size, wxSUNKEN_BORDER);
  m_panel = this; 
  m_size = size;
// Initialize good size without borders with the input size:
  client_width1 = m_size.x;
  client_height1 = m_size.y;

  m_parent_panel = parent_panel; 
  m_StaticHelp = static_help;
  m_StaticCoord = static_coord;
  initialized = 0;
  ResetAllPrivateParameters();

// Init plot parameters:
// InitPlotData(int nmaxi, int ncurves_maxi, int nout_maxi)
  InitPlotData(1024, 32, 256);
   
// Setting the parameters to default values: 
  xminuser = 0; xmaxuser = 1; yminuser = 0; ymaxuser = 1; 
  plan = 0;
  status = SetupForCurve(this, plotdev, out_filename, title, 
                         &xminuser, &xmaxuser, &yminuser, &ymaxuser,
                         plan, &idv1, err_messg);
  if(status) {
   buffer.Printf(wxT("SetupForCurve/Error: %s"), err_messg);
   wxMessageBox(buffer, wxT("JLP_wxGseg_Canvas"), wxOK | wxICON_ERROR);
   } else {
#if USE_METAFILE
   NewNameForMetaFile();
   OpenMetaFile("w");
#endif
   }

// New processing setup:
 m_gsproc1 = new JLP_wx_gsProc1(this);

// New labels:
 m_curve_labels1 = new JLP_wxCurveLabels();

// Create popup menu:
 CreatePopupMenu();
 initialized = 1234;

// Set internal processing mode to 0 (thresholds selected with a box)
 SetInternalProcessingMode(0);

// Update PopupMenu (seems to be necessary for asteria):
UpdatePopupMenu();

return;
};
/*************************************************************
* Reset all private parameters of igdev settings and some others
*
* Some parameters are also initialized in "SetupForCurve()"
*************************************************************/
void JLP_wxGseg_Canvas::ResetAllPrivateParameters()
{
  jlp_gseg_wxwid0 = NULL;
  jlp_gsegraf0 = NULL;
  flag_contour_labelling = 0;

  crosshair_cursor = 0;
  active_gsproc = NULL;

  m_bitmap_width = BITMAP_WIDTH;
  m_bitmap_height = BITMAP_HEIGHT;
 
  initialized = 0;
  backup_dc_bitmap2 = NULL;
  backup_dc = NULL;
  PopupMenu1 = NULL;

#if USE_METAFILE
   MetaFileIsOpened = false;
#endif

  strcpy(title1, "JLP_cGDev");
  strcpy(xlabel1, "");
  strcpy(ylabel1, "");

  xplot1 = NULL;
  yplot1 = NULL;
  errx1 = NULL;
  erry1 = NULL;
  npts1 = NULL;
  xout1 = NULL;
  yout1 = NULL;
  ncurves1 = 0;
  should_plot_allcurves = 0;
  first_time_curves_are_plotted = 1;

  cgdev_settings1.pen_colour = *wxBLACK;
  cgdev_settings1.pen_default_colour = *wxBLACK;
  cgdev_settings1.backgd_colour = *wxWHITE;
  cgdev_settings1.cursor_type= _T("Arrow");

// Box type (0: plain, 1:nice, 2:elaborated):
  cgdev_settings1.box_type = 0;
  cgdev_settings1.ticks_in = 1;
  cgdev_settings1.xgrid = 0;
  cgdev_settings1.ygrid = 0;

// InternalProcessingMode
// -1=None 0=BoxLimits 1=Statistics, 2=Astrometry 3=Photometry
// 4=Add a label 5=Remove a label
  cgdev_settings1.InternalProcessingMode = -1;
  software_event1 = 0;

  s_nLeftDown = 0;
  s_nLeftUp = 0;
  nPointsAreRequired = 0;
  LimitsBoxType = 1;
  active_gsproc = NULL;
  m_gsproc1 = NULL;
  m_label1 = _T("");

return;
}
/*************************************************************
* Constructor from wxFrame and wxPanel 
* Version may 2013
*************************************************************/
 JLP_wxGseg_Canvas::JLP_wxGseg_Canvas(wxPanel *parent_panel, wxWindowID id, 
                const wxPoint &pos, const wxSize &size,
                const char *plotdev, 
                const char *out_filename, const char* title,
                float& xminuser, float& xmaxuser, float& yminuser,
                float& ymaxuser, const int plan, int& status)
   : wxPanel(parent_panel, id, pos, size, wxSUNKEN_BORDER)
{
char err_messg[128];
wxString buffer;

// Creation of the Panel window:
//  m_panel = new wxPanel(parent_panel, id, pos, size, wxSUNKEN_BORDER);
  m_panel = this;
  m_size = size;
// Initialize good size without borders with the input size:
  client_width1 = m_size.x;
  client_height1 = m_size.y;

  m_parent_panel = parent_panel;
  m_StaticHelp = NULL;
  m_StaticCoord = NULL;

  initialized = 0;
  ResetAllPrivateParameters();

// Init plot parameters:
  status = SetupForCurve(this, plotdev, out_filename, title, 
                         &xminuser, &xmaxuser, &yminuser, &ymaxuser, plan, 
                         &idv1, err_messg);
  if(status) {
   buffer.Printf(wxT("SetupForCurve/Error: %s"), err_messg);
   wxMessageBox(buffer, wxT("JLP_wxGseg_Canvas"), wxOK | wxICON_ERROR);
   } else {
#ifdef USE_METAFILE
     NewNameForMetaFile();
     OpenMetaFile("w");
#endif
   }

// New processing setup:
 m_gsproc1 = new JLP_wx_gsProc1(this);

// New labels:
 m_curve_labels1 = new JLP_wxCurveLabels();

// Create popup menu:
 CreatePopupMenu();
 initialized = 1234;

return;
};

/*************************************************************
* For curves
* Setup window parameters according to the device name
*
* INPUT:
* plotdev: "wxLand", wxland, "wxSquare", or "wxsquare"
* title
*
* OUTPUT:
* dev_width, dev_height : size of window in device coordinates
* TeX_flag
* devtype1 = 1 if X11, 2 if postscript, 3 if wxWidgets...
************************************************************************/
int JLP_wxGseg_Canvas::setup_device_for_curve(const char *plotdev, 
                                            const char *title,
                                            int *dev_width, 
                                            int *dev_height, int *TeX_flag, 
                                            int *devtype, int *landscape) 
{
int status = 0;

// Set device size: 
  *dev_width = m_size.GetWidth();

  *dev_height = m_size.GetHeight();

// Assuming no rotation: 
  *landscape = 0;

// TeX flag
  *TeX_flag = 0;

// Device type: 1 = X11, 2 = Postscript file,  3 = wxWidgets
  *devtype = 3;

// TeX flag
  *TeX_flag = 0;

/* A trick to select TeX interactively: if upper case, TeX selected */
// WTERM, wterm, WDISPLAY, wdisplay
 if(plotdev[2] == 'W') *TeX_flag = 1;

// Maximum level for LUT values :
  MaxColorLevelForLUT = 255;

return(status);
}
/*************************************************************
* Open the wxWidgets device for curve display
*
* INPUT:
* title: string to be displayed on top of the window
* dev_width1, dev_height1: size required for the window in device coordinates
* landscape: flag set to one if plot is rotated by 90 degrees
*            (Not used here)
*
* INPUT:
*  dev_width1, dev_height1: window size in device coordinates
************************************************************/
int JLP_wxGseg_Canvas::open(const char* title, int dev_width1, 
                          int dev_height1, const int landscape)
{
wxString title1(title, wxConvUTF8);
wxString buffer;

#ifdef DEBUG
  printf(" JLP/wxWidgets graphic environment Version 06-02-2016 \n");
/*
  printf(" WARNING: This program assumes %d bits per pixel of video board (%d colors)\n", 
        BIT_PER_VIDEO_PIXEL, (int)pow(2.,(double)BIT_PER_VIDEO_PIXEL));
*/
#endif

 if(dev_width1 <= 0 || dev_height1 <= 0) {
   fprintf(stderr," JLP_cGDev_WxWID/Open: fatal error: dev_width=%d dev_height=%d\n",
           dev_width1, dev_height1);
   buffer.Printf("Open: fatal error: dev_width=%d dev_height=%d\n",
                 dev_width1, dev_height1);
   wxMessageBox(buffer, wxT("JLP_cDev_wxWID"), wxOK | wxICON_ERROR);
   exit(-1);
   }

 backup_dc_bitmap2 = NULL;
 backup_dc = new wxMemoryDC();
 InitBackupDC(dev_width1, dev_height1);

// Postscript backup DC:
  backup_printer1 = NULL;
  backup_pst_dc1 = NULL;

// correspondance to GSEG: 
 jlp_gseg_wxwid0 = new JLP_Gseg_Wxwid(dev_width1, dev_height1, backup_dc, 
                                      backup_pst_dc1);
 jlp_gsegraf0 = NULL;

 initialized = 1234;

return(0);
}
/*************************************************************
* Erase all the drawings inside the window
*
*************************************************************/
int JLP_wxGseg_Canvas::erase()
{

// Test if curve mode is active:
 if(initialized != 1234) return(-1);

 backup_dc->Clear();
// backup_dc->SetPen( *wxBLACK_PEN);
// backup_dc->SetBackground( *wxGREY_BRUSH);

return(0);
}
/***************************************************************
* Not available with wxwidgets
***************************************************************/
int JLP_wxGseg_Canvas::wait_for_events()
{
return(-1);
}
/*********************************************************************
* Close device
*
*********************************************************************/
int JLP_wxGseg_Canvas::close_device()
{

/*
printf("JLP_Gdev_wxWID/Warning: initialized = %d \n", initialized);
printf("JLP_Gdev_wxWID/Warning you have called close_device (is it OK?)\n");
*/

return(0);
}
/*************************************************************
* Create a Device Context to be able to write on it outside
* of Paint events
* (Called each time the size of the canvas is modified)
*************************************************************/
void JLP_wxGseg_Canvas::InitBackupDC(int width, int height)
{
int x0, y0;
wxString buffer;

if(width <= 0 || height <= 0) {
  fprintf(stderr, "InitBackupDC/Fatal error: width=%d height=%d\n", 
          width, height);
  buffer.Printf(wxT("InitBackupDC/Fatal error: width=%d height=%d"), 
                width, height);
  wxMessageBox(buffer, wxT("JLP_cDev_wxWID"), wxOK | wxICON_ERROR);
  exit(-1);
  }
// backup Device Context:
// Create a new bitmap by copying the current image bitmap2 
// WARNING: this bitmap will be then attached to backup_dc...
  if(backup_dc_bitmap2 != NULL) delete backup_dc_bitmap2;

  backup_dc_bitmap2 = new wxBitmap(width, height, -1);
  m_bitmap_width = width;
  m_bitmap_height = height;

// Assign this bitmap to the memory Device Context:
  backup_dc->SelectObject(*backup_dc_bitmap2);

// Basic setup of the backup device:
  backup_dc->Clear();
//  backup_dc->SetPen( *wxBLACK_PEN);
//  backup_dc->SetBackground( *wxGREY_BRUSH);
//  backup_dc->SetBackground( *wxWHITE_BRUSH);

// Update graphic window size:
  if(jlp_gseg_wxwid0 != NULL) {
   x0 = 0;
   y0 = 0;
   jlp_gseg_wxwid0->GSEG_SetWindowLimits(x0, m_bitmap_width, 
                                          y0, m_bitmap_height);
   if(jlp_gsegraf0 != NULL) jlp_gsegraf0->GSEG_InitializePlotSize();
  }

#if 0
// draw a green circle
  backup_dc->SetBrush(*wxGREEN_BRUSH); // green filling
  backup_dc->SetPen( wxPen( wxColor(255,0,0), 5 ) ); // 5-pixels-thick red outline
  backup_dc->DrawCircle( wxPoint(200,100), 25 /* radius */ );
#endif

return;
}
/*************************************************************************
* Set Client Size to width, and height 
* Create new backup_dc_bitmap and update the size of the scrolled window
*
**************************************************************************/
int JLP_wxGseg_Canvas::SetNewBitmapSize(int width, int height)
{
wxString buffer;

if(initialized != 1234) return(-1);

 if((width <= 0) || (height <= 0)) {
 fprintf(stderr, "JLP_wxGseg_Canvas::SetNewBitmapSize/Fatal error: width=%d height=%d\n",
         width, height);
 buffer.Printf(wxT("SetNewBitmapSize/Fatal error: width=%d height=%d"),
         width, height);
 wxMessageBox(buffer, wxT("JLP_cDev_wxWID"), wxOK | wxICON_ERROR);
 return(-1);
 }

// Initialize backup device context (used as a buffer for a faster display):
// Routine defined in application program:
  InitBackupDC(width, height);

// Update user/device conversion parameters (in Jgc0) :
  JLP_cGDev::SetNewDeviceSize(width, height);

// Update the drawings:
  RedrawToBackupDC();

return(0);
}
/************************************************************************
* To enlarge the window at the maximum size: 
* JLP 2015: Should not be done at this level but on the jlp_wx_GraphicPanel class
* SEE ALSO : void JLP_wxGraphicPanel::OnResize( wxSizeEvent &event )
************************************************************************/
void JLP_wxGseg_Canvas::OnResize( wxSizeEvent &event )
{
wxSize new_size;

#ifdef TTR
// JLP 2015: Should not be done here, but with the jlp_wx_GraphicPanel class !
if(initialized != 1234) return;

// Size with borders
new_size = event.GetSize();
printf("JLP_wxGseg_Canvas::OnResize: size=%d %d\n", new_size.x, new_size.y); 

// Good size without borders
GetClientSize(&client_width1, &client_height1);
printf("JLP_wxGseg_Canvas::OnResize: ClientSize=%d %d\n", 
        client_width1, client_height1); 

// SHOULD NOT CALL SetnewbitmapSize FROM HERE
// (BUT FROM jlp_wx_gpanel.cpp)
// Create a new bitmap with the new size: 
 SetNewBitmapSize(client_width1, client_height1);
#endif

// Skip this event (to avoid infinite processing of the same event):
 event.Skip();

return;
}
/************************************************************************
* Output curve as a JPEG, PNG, etc. file
************************************************************************/
void JLP_wxGseg_Canvas::SaveGraphicToFile()
{
wxString savefilename;
bool saved = false;

wxImage image1 = backup_dc_bitmap2->ConvertToImage();

// Init all image handlers in case it was not already done:
// (Needed to read PNG,JPEG, etc)
::wxInitAllImageHandlers();

// Gif is not available yet ?
   savefilename = wxFileSelector( wxT("Save Image to jpg, png, tiff, gif"),
            wxT(""), wxT(""), wxT("jpg|png|tiff|gif"),
            wxT("Files (*.jpg;*.png;*.tif;*.gif)|*.jpg;*.png;*.tif;*.gif"),
            wxFD_SAVE, this);

   if ( savefilename.empty() ) return;

   wxString extension;
   wxFileName::SplitPath(savefilename, NULL, NULL, &extension);

   if ( extension == _T("jpg") ) {
// Set reasonable quality and compression:
       image1.SetOption(wxIMAGE_OPTION_QUALITY, 80);
// Save image:
       saved = image1.SaveFile(savefilename, wxBITMAP_TYPE_JPEG);
   }
   else if ( extension == _T("png") )
   {
       static const int pngvalues[] =
       {
           wxPNG_TYPE_COLOUR,
           wxPNG_TYPE_COLOUR,
           wxPNG_TYPE_GREY,
           wxPNG_TYPE_GREY,
           wxPNG_TYPE_GREY_RED,
           wxPNG_TYPE_GREY_RED,
       };
       const wxString pngchoices[] =
       {
           _T("Colour 8bpp"),
           _T("Colour 16bpp"),
           _T("Grey 8bpp"),
           _T("Grey 16bpp"),
           _T("Grey red 8bpp"),
           _T("Grey red 16bpp"),
       };

       int sel = wxGetSingleChoiceIndex(_T("Set PNG format"),
                                        _T("Image sample: save file"),
                                        WXSIZEOF(pngchoices),
                                        pngchoices,
                                        this);
       if ( sel != -1 )
       {
           image1.SetOption(wxIMAGE_OPTION_PNG_FORMAT, pngvalues[sel]);
           image1.SetOption(wxIMAGE_OPTION_PNG_BITDEPTH, sel % 2 ? 16 : 8);
       }
// Save image:
       saved = image1.SaveFile(savefilename, wxBITMAP_TYPE_PNG);
   }

return;
}
