/*************************************************************************
* JLP_GDev_wxWID Class (for curves)
*
* JLP
* Version 09/02/2017
**************************************************************************/
#include "jlp_gdev.h"        // BITMAP_WIDTH, etc
#include "jlp_gdev_wxwid.h"
#include "jlp_wx_cursor.h"
#include "jlp_time0.h"            // JLP_CTIME
#include "jlp_wxgdev_labels.h"    // JLP_wxGDevLabels class
#include "jlp_wx_gdproc1.h"       // For JLP_GDProc1
#include "jlp_wxgdev_popup.h"   // For JLP_wxGDev_Popup

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
JLP_GDev_wxWID::JLP_GDev_wxWID(wxPanel *parent_panel, wxWindowID id, 
                               const wxPoint &pos, const wxSize &size,
                               wxStaticText *static_coord,
                               wxStaticText *static_help)
             : wxPanel((wxPanel *)parent_panel, id, pos, size, wxSUNKEN_BORDER)
{
char plotdev[64], out_filename[64], title[64];
int plan, status;
double xminuser, xmaxuser, yminuser, ymaxuser;
char err_messg[128];
wxString buffer;

// Creation of the Panel window:
//    m_panel = new wxPanel(parent_panel, id, pos, size, wxSUNKEN_BORDER);
  m_size = size;
// Initialize good size without borders with the input size:
  client_width1 = m_size.x;
  client_height1 = m_size.y;

  m_scrolled_window = NULL;
  m_parent_gpanel = NULL;
  m_parent_ipanel = NULL;
  m_StaticHelp = static_help;
  m_StaticCoord = static_coord;
  m_StatusBar = NULL;
  initialized = 0;

// Reset backup_dc, popup menu parameters, etc.,
  ResetAllPrivateParametersForCurves();
  
// Setting the parameters to default values:
  xminuser = 0; xmaxuser = 1; yminuser = 0; ymaxuser = 1;
  plan = 0;
// Init plot parameters  (and call JLP_GDev_wxWID::open())
// and set Jgc0.gdev_graphic_type=1
  status = SetupForCurve(this, plotdev, out_filename, title,
                         xminuser, xmaxuser, yminuser, ymaxuser,
                         plan, err_messg);
  if(status) {
   buffer.Printf(wxT("SetupForCurve/Error: %s"), err_messg);
   wxMessageBox(buffer, wxT("JLP_GDev_wxWID"), wxOK | wxICON_ERROR);
   } else {
#if USE_METAFILE
   NewNameForMetaFile();
   OpenMetaFile("w");
#endif
   }

// New processing setup
 m_gdproc1 = new JLP_wx_GDProc1(this);

// New labels:
 m_wxlabels1 = new JLP_wxGDevLabels();

// New processing setup
 m_gdproc1 = new JLP_wx_GDProc1(this);

// New labels:
 m_wxlabels1 = new JLP_wxGDevLabels();

// New shapes:
 m_wxshapes1 = new JLP_wxGDevShapes();

// Create popup menu:
 m_popup_menu1 = new JLP_wxGDev_Popup(this, Jgc0.gdev_graphic_type);

// Connection of events to this window and to the scrolled window
// m_popup_menu1->ConnectAllPopupMenuEvents();

 initialized = 1234;

return;
};
/*************************************************************
* Constructor from JLP_wxGraphicPanel 
*************************************************************/
JLP_GDev_wxWID::JLP_GDev_wxWID(JLP_wxGraphicPanel *parent_panel, 
                               wxWindowID id, 
                               const wxPoint &pos, const wxSize &size, 
                               wxStaticText *static_coord,
                               wxStaticText *static_help)
             : wxPanel((wxPanel *)parent_panel, id, pos, size, wxSUNKEN_BORDER)
{
char plotdev[64], out_filename[64], title[64];
int plan, status;
double xminuser, xmaxuser, yminuser, ymaxuser;
char err_messg[128];
wxString buffer;

// Creation of the Panel window:
//    m_panel = new wxPanel(parent_panel, id, pos, size, wxSUNKEN_BORDER);
  m_size = size;
// Initialize good size without borders with the input size:
  client_width1 = m_size.x;
  client_height1 = m_size.y;

  m_scrolled_window = NULL;
  m_parent_gpanel = parent_panel; 
  m_parent_ipanel = NULL; 
  m_StaticHelp = static_help;
  m_StaticCoord = static_coord;
  m_StatusBar = NULL;
  initialized = 0;

// Reset backup_dc, popup menu parameters, etc.,
  ResetAllPrivateParametersForCurves();
   
// Setting the parameters to default values: 
  xminuser = 0; xmaxuser = 1; yminuser = 0; ymaxuser = 1; 
  plan = 0;
// Init plot parameters  (and call JLP_GDev_wxWID::open())
// and set Jgc0.gdev_graphic_type=1
  status = SetupForCurve(this, plotdev, out_filename, title, 
                         xminuser, xmaxuser, yminuser, ymaxuser,
                         plan, err_messg);
  if(status) {
   buffer.Printf(wxT("SetupForCurve/Error: %s"), err_messg);
   wxMessageBox(buffer, wxT("JLP_GDev_wxWID"), wxOK | wxICON_ERROR);
   } else {
#if USE_METAFILE
   NewNameForMetaFile();
   OpenMetaFile("w");
#endif
   }

// New processing setup
 m_gdproc1 = new JLP_wx_GDProc1(this);

// New labels:
 m_wxlabels1 = new JLP_wxGDevLabels();

// New shapes:
 m_wxshapes1 = new JLP_wxGDevShapes();

// Create popup menu:
 m_popup_menu1 = new JLP_wxGDev_Popup(this, Jgc0.gdev_graphic_type);

// Connection of events to this window and to the scrolled window
// m_popup_menu1->ConnectAllPopupMenuEvents();

 initialized = 1234;

return;
};
/*************************************************************
* Reset all private parameters of wxgdev_settings and some others
*
* Some parameters are also initialized in "SetupForCurve()"
*************************************************************/
void JLP_GDev_wxWID::ResetAllPrivateParametersForCurves()
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
  crosshair_cursor = 0;
  active_gdproc = NULL;

  m_bitmap_width = BITMAP_WIDTH;
  m_bitmap_height = BITMAP_HEIGHT;
 
  backup_dc = NULL;
  backup_pst_dc1 = NULL;
  backup_dc_bitmap2 = NULL;

#if USE_METAFILE
   MetaFileIsOpened = false;
#endif

  wxgdev_settings1.pen_colour = *wxBLACK;
  wxgdev_settings1.pen_default_colour = *wxBLACK;
  wxgdev_settings1.backgd_colour = *wxWHITE;
  wxgdev_settings1.cursor_type= _T("Arrow");

// Box type (0: plain, 1:nice, 2:elaborated):
  wxgdev_settings1.box_type = 0;
  wxgdev_settings1.ticks_in = 1;
  wxgdev_settings1.xgrid = 0;
  wxgdev_settings1.ygrid = 0;

// InternalProcessingMode
// -1=None 0=BoxLimits 1=Statistics, 2=Astrometry 3=Photometry
// 4=Add a label 5=Remove a label 18=zoom
  wxgdev_settings1.InternalProcessingMode = -1;

// Other options:
  active_gdproc = NULL;
  m_gdproc1 = NULL;
  m_label1 = _T("");

// Init all parameters for images:
  c_image1 = NULL;
  work_image1 = NULL;
// lut_type: 'l'=log rainbow1 'r'=rainbow2 's'=saw 'g'=gray 'c'=curves
//           'p'=pisco
  strcpy(wxgdev_settings1.lut_type, "pisco");
  wxgdev_settings1.lut_reversed = 0;
  wxgdev_settings1.itt_type = wxT("MinMax");
// Log ITT:
  wxgdev_settings1.itt_is_linear = 0;
  wxgdev_settings1.zoom = 1;
// 0=NONE
  wxgdev_settings1.filter = 0;
  wxgdev_settings1.low_itt_thresh = 0;
  wxgdev_settings1.up_itt_thresh = 0;

// Filename (used for the title of gseg frame)
  strcpy(filename_1, "");

return;
}
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
int JLP_GDev_wxWID::setup_device_for_curve(const char *plotdev, 
                                           const char *title, int *dev_width, 
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
/***************************************************************************
* ClearDrawingDisplay
* (called by external routines)
***************************************************************************/
void JLP_GDev_wxWID::ClearDrawingDisplay()
{
if(initialized != 1234) return; 
 Curves_ResetAllPrivateParameters();
 gdev_erase();
 wxGdev_Refresh();
}
/***************************************************************************
* PlotToDrawingDisplay
* (called by external routines)
***************************************************************************/
void JLP_GDev_wxWID::PlotToDrawingDisplay()
{
if(initialized != 1234) return; 

RedrawToBackupDC(1601);

}
