/**************************************************************************
* JLP_wxGseg_Canvas class (for Curves)
* Definition of the wxWidgets graphic driver 
* used in my plotting package
* Derived from virtual class JLP_GDev defined in "jlp_gdev.h"
*
* JLP
* Version 22/06/2015
**************************************************************************/
#ifndef __jlp_cgdev_wxwid_h                     /* sentry */
#define __jlp_cgdev_wxwid_h

#include <stdio.h>
//#include <math.h>
#include <ctype.h>

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
#undef index  // To solve problems with index (conflicts with "string.h") ...
    #include "wx/wx.h"
#endif

#include "wx/file.h"
#include "wx/filename.h"
#include "wx/mstream.h"
#include "wx/wfstream.h"
#include "wx/quantize.h"
#include "wx/dcmemory.h"         // wxMemoryDC
#include "wx/bitmap.h"
#include "wx/cursor.h"
#include "wx/dcps.h"               // Postscript

// For clipboard:
#if wxUSE_CLIPBOARD
    #include "wx/dataobj.h"
    #include "wx/clipbrd.h"
#endif // wxUSE_CLIPBOARD

/* To define "std" as the standard prefix (e.g. before printf, scanf, ...) */
using namespace std;

/* To define the JLP_Gdev virtual class */ 
#include "jlp_gdev.h"

#include "jlp_gseg_wxwid.h"       // JLP_Gseg_Wxwid class
#include "jlp_gsegraf.h"          // JLP_Gsegraf class

#include "jlp_gdev_idv.h"         // jlp_curve_free()
#include "jlp_macros.h"           // NINT

#define NBUT_MAX 20               // Maximum number of buttons
#define POPUP_NITEMS_MAX 20       // Maximum number of items in popup menu
#define NCURVES_MAX 256           // Maximum number of curves (in "newplot")

// Simple declaration to avoid "boomerang" error
class  JLP_wx_gsProc1;
class  JLP_wx_gsProc;
class JLP_wxCurveLabels;

class JLP_wxGseg_Canvas : public wxPanel, public JLP_GDev {

public:

/* Constructors:
*/
 JLP_wxGseg_Canvas(wxPanel *parent_panel, wxWindowID id,
                const wxPoint &pos, const wxSize &size,
                const char *plotdev,
                const char *out_filename, const char* title,
                float& xminuser, float& xmaxuser, float& yminuser,
                float& ymaxuser, const int plan, int& status);

 JLP_wxGseg_Canvas(wxPanel *parent_panel, wxWindowID id, const wxPoint &pos,
                 const wxSize &size, wxStaticText *static_coord,
                 wxStaticText *static_help);

/* Destructor:
* (Should be declared as virtual)
*/
  ~JLP_wxGseg_Canvas() {
#if USE_METAFILE
    CloseMetaFile();
#endif
// Free allocated index:
  free_curve_idv(idv1);
// Close the device:
  JLP_wxGseg_Canvas::close_device();
#ifdef DEBUG
  printf("JLP_wxGseg_Canvas: destructor has been called\n");
#endif
  return;
  };

/* Close graphic device
*/
  int close() {
    printf("JLP_wxGseg_Canvas: closed called here \n");
    this->~JLP_wxGseg_Canvas(); 
    return(0);
    }
  int close_device();
  void ResetAllPrivateParameters();
  void UpdateCursor(const double x_position);

// "jlp_wx_canvas_gseg.cpp"
// Graphic interface:
  int InitializePlotWithParameterFile(char *param_filename0);
  void CloseBackupPostscriptDC();
  int OpenBackupPostscriptDC(char *save_filename0);
  void ViewChangeAxisLimits();
  void ViewAxisRotate();
  void Set_ViewLabelContours();
  int Callback_ViewLabelContours(const double xmouse_0, const double ymouse_0,
                                 char *label_0);
  void Set_ZoomIn();
  int Callback_ZoomIn(const double xmouse1_0, const double ymouse1_0,
                      const double xmouse2_0, const double ymouse2_0);
  int FromDeviceToUserCoord(const double x1, const double y1,   
                            double *dev_x1, double *dev_y1, int *in_frame);

/* To setup the device:
* INPUT: plotdev, title, nx1, ny1
* OUTPUT: dev_width, dev_height
*/
  int setup_device_for_curve(const char *plotdev, const char* title,
                             int *dev_width, int *dev_height, int *TeX_flag, 
                             int *devtype, int *landscape);

// Open the device for curves:
  int open(const char* title, int dev_width, int dev_height, 
           const int landscape);

// Erase all the drawings inside the window: 
  int erase();

// Compulsory, since virtual routine in jlp_splot class
  int gflush(){return(0);};

// Wait for next event (mouse button for X11)
  int wait_for_events();

  void OnResize( wxSizeEvent &event );
  void InitBackupDC(int width, int height);
  int SetNewBitmapSize(int width, int height);
  void SaveGraphicToFile();
  void SaveGraphicToPostscriptFile();
  void SaveBackupPostscriptToFile();
 
//***********************************************************************//
// Plot routines in "jlp_cgdev_wxwid_newplot.cpp":
//************************************************************************//
  void ClearDrawingDisplay();
  void PlotToDrawingDisplay();
  int RefreshAllCurves();
  int PstCopyOfDisplay(char *pst_filename);
  int LoadPlotData(double *xplot1, double *yplot1, const int npts1,
                   const char *nchar_type, const char *pcolor,
                   const char *plot_fname, const int reset_first);
  void InitPlotData(const int nmaxi1, const int ncurves_maxi1,
                    const int nout_maxi1);
  int LoadPlotDataFromFile(char *filename, const int reset_first);
  void ComputeXYMinMaxForCurves(double& xminuser, double& xmaxuser,
                                double& yminuser, double& ymaxuser);
  void ComputeYMinMaxForCurves(double xminuser, double xmaxuser,
                               double& yminuser, double& ymaxuser);
  int LoadPlotSettings(const char *xlabel, const char *ylabel,
                       const char *title, const int xgrid_is_wanted,
                       const int ygrid_is_wanted,
                       const int jlp_axes_are_wanted, const int iplan,
                       const double x1, const double x2,
                       const double y1, const double y2);
  void InitBeforePlot();
  void PlotAllCurves(int idv1);

//***********************************************************************//
// Plot routines in "jlp_cgdev_wxwid_plot.cpp":
// needed by the virtual definition of JLP_cgDev
//************************************************************************//
// To draw a line:
  int line_device(int x1, int y1, int x2, int y2, int lwidth=0); 

// To draw a polygon:
  int polygon(int x, int y, float expand, float angle, int nsides, 
              int filled); 

// To draw a circle:
  int circle(int x, int y, int idiam, int filled) {return(-1);};

// To fill a rectangle (i.e. to paint it with a color):
  int FilledRect1(float x0, float y0, float x1, float y1, 
                  char *color){return(-1);}

// Set color of line:
  int setrgbcolor(float r, float g, float b);
  int setdefaultcolor();

// Set line width and line type:
  int SetLineWidthAndType(int lwidth, int ltype);

// Pen colour
  void SetPenColour_dc(const wxColour wxC);

// Background colour
  void SetBackgdColour_dc(const wxColour wxC);

/*
  int DrawToCursorPosition(char *label);
  int EraseCursorPosition();
*/

// MetaFile utilities
#if USE_METAFILE
  int OpenMetaFile(const char *mode);
  int CloseMetaFile();
  int NewNameForMetaFile();
  int ReadAndExecuteMetaFile();
  int ExecuteMetaFileLine(char *metaline);
#endif

// Backup Device Context (accessible to public, for drawing)
  wxMemoryDC *backup_dc;
  wxMemoryDC *GetBackupDc(){return backup_dc;};

// Defined in jlp_cgdev_wxwid_onclick.cpp:
// Popup menu by clicking on the right button of the mouse:
#if USE_CONTEXT_MENU
  void OnContextMenu(wxContextMenuEvent& event);
#else
  void OnRightUp(wxMouseEvent& event)
        { ShowPopupMenu(event.GetPosition()); }
#endif
  void ShowPopupMenu(const wxPoint& pos);
  void OnSetCursor(wxCommandEvent& event);
  void OnSetPenColour(wxCommandEvent& event);
  void OnSetBackgdColour(wxCommandEvent& event);
  void OnSetBoxType(wxCommandEvent& event);
  void OnInfoCurve(wxCommandEvent& WXUNUSED(event));
  void OnChangeBoxLimits(wxCommandEvent& event);
  void OnSaveToPostscript(wxCommandEvent& event);
  void OnSave(wxCommandEvent& event);
#if wxUSE_CLIPBOARD
  void OnCopy(wxCommandEvent& event);
#endif // wxUSE_CLIPBOARD
  void OnSetInteractiveProcessing(wxCommandEvent& event);
  void BoxLimitsPrompt();

// Defined in jlp_cgdev_wxwid_menu.cpp:
  void CreatePopupMenu();
  void UpdatePopupMenu();
  void UpdatePopupMenu_Cursor();
  void UpdatePopupMenu_PenColour();
  void UpdatePopupMenu_BackgroundColour();
  void UpdatePopupMenu_BoxType();
  void UpdatePopupMenu_InternalProcessingMode();
  void PopupMenuEraseProcessingCheckBoxes();
  void BoxLimitsAuto();
  void BoxLimitsZoom(const bool zoom_in);
  void BoxLimitsMove(const bool move_to_right);
  void BoxLimitsMoveLeft();

// in jlp_cgdev_wxwid_process.cpp:
  void OnMotion(wxMouseEvent& event);
  void OnLeftDown(wxMouseEvent& event);
  void OnLeftUp(wxMouseEvent& event);
  void HandleDraggingMotion(wxMouseEvent& event);
// ProcessingMode: auto_scale, photometry, labels, etc.
  int SetInternalProcessingMode(int processing_mode);
// External processingMode: binary star measurements, for instance
  int SetExternalProcessingMode(JLP_wx_gsProc *external_gsproc,
                                int limits_box_type, int npts_required);
  int SetNewRequiredPoints(int npts_are_required, int limits_box_type);
  int GetUserCoordFromCursor(double *x1, double *y1, int *in_frame);
  int LeftUp_GetLastData(int *nn, double *x1, double *y1);
  int LeftDown_GetLastData(int *nn, double *x1, double *y1);

// in jlp_cgdev_wxwid_canvas.cpp:
  void DisplayCoordinates(wxString coordinates_text);
  void cgdev_render( wxDC *dc1);
  void OnPaint( wxPaintEvent &event );
  void MyRefreshScreen();
  void RedrawToBackupDC();

// Image labels:
// in jlp_cgdev_wxwid_labels.cpp:
  void OnAddLabel(wxCommandEvent& event);
  void OnRemoveLabel(wxCommandEvent& event);
  void AddLabel(wxString new_label, double x0, double y0);
  void RemoveLabel(double x0, double y0);

//************************************************************************
// inline graphic functions:
//************************************************************************
// Cursor function: 
  int CursorGetCoord(int& x, int& y, char* cursor_type, int& pressed_button) {
    fprintf(stderr,"Sorry cursor function not available here\n");
    return(-1);
  };

// To plot a label (i.e. a string) on the graphic device:
// xstart, ystart are mgo coordinates
  float label_device(const char *s, int xstart, int ystart, float angle,
                     float expand, int draw_it);

// Conversion from mgo to device coordinates:
inline  void conv_mgo_to_dev(const int x, const int y, float& d_x, float& d_y) {
      d_x = (float)x * Jgc0.g_dx;
      d_y = (float)Jgc0.devheight  - (float)y * Jgc0.g_dy;
      }

// Conversion from device to mgo coordinates:
inline  void conv_dev_to_mgo(const float d_x, const float d_y, int& x, int& y) {
      x = NINT(d_x / Jgc0.g_dx);
      y = NINT(((float)Jgc0.devheight  - d_y) / Jgc0.g_dy);
      }

/************************************************************************
* Get idev number for plotting (for external routines) to this panel: 
************************************************************************/
    int Get_idev(int& idev){
      int status;
      if(initialized == 1234) {
        idev = idv1; 
        status = 0;
        } else {
        idev = -1; 
        status = 1;
        }
      return(status); 
    };
/************************************************************************
* Get box limits for plotting (for external routines) to this panel: 
************************************************************************/
    void GetBoxLimits(double& xmin, double& xmax, double& ymin, double& ymax){
      xmin = Jgc0.xminuser;
      xmax = Jgc0.xmaxuser;
      ymin = Jgc0.yminuser;
      ymax = Jgc0.ymaxuser;
    }
/************************************************************************
* Get box limits for plotting (for external routines) to this panel: 
************************************************************************/
    void GetBoxLimits_float(float *xmin, float *xmax, float *ymin, 
                            float *ymax){
      *xmin = Jgc0.xminuser;
      *xmax = Jgc0.xmaxuser;
      *ymin = Jgc0.yminuser;
      *ymax = Jgc0.ymaxuser;
    }
/************************************************************************
* Set box limits for plotting (for external routines) to this panel: 
************************************************************************/
    int SetBoxLimits(const double xmin, const double xmax, const double ymin,
                     const double ymax){
      if(xmax == xmin || ymax == ymin) return(-1);
      Jgc0.xminuser = xmin;
      Jgc0.xmaxuser = xmax;
      Jgc0.yminuser = ymin;
      Jgc0.ymaxuser = ymax;
// Conversion to MGO
      JLP_GDev::FromJgc0ToMgc0();
      return(0);
    };

/************************************************************************
* Get plot data (for external routines) from this panel: 
************************************************************************/
  int GetPlotData(double **xplot0, double **yplot0, int *npts0, 
                  const int icurve)
    {
    int i;
      if((icurve < 0) || (icurve >= ncurves1)) return(-1);
      if(npts1[icurve] <= 0) return(-1);
      *npts0 = npts1[icurve];
      *xplot0 = new double[*npts0];
      *yplot0 = new double[*npts0];
      for(i = 0; i < *npts0; i++) {
         (*xplot0)[i] = xplot1[i + icurve * nmaxi1];
         (*yplot0)[i] = yplot1[i + icurve * nmaxi1];
         }
      return(0);
    };
/*********************************************************************
* Get the client size 
* (needed for obtaining the right coordinates with the cursor)
**********************************************************************/
  void Get_ClientSize(int *width0, int *height0)
    {
// Good size without borders
     GetClientSize(&client_width1, &client_height1);
// Copy to output parameters:
      *width0 = client_width1;
      *height0 = client_height1;
      return;
    }

private:

  wxPanel *m_parent_panel, *m_panel;
  wxSize m_size;
  int client_width1, client_height1;
  int idv1, initialized, software_event1;
  wxStaticText *m_StaticCoord, *m_StaticHelp;

// Settings:
  GDev_SETTINGS gdev_settings1;

// Setting parameters used by "jlp_cgdev_wxwid_newplot" routines:
  float *xplot1, *yplot1, *errx1, *erry1, *xout1, *yout1;
  int should_plot_allcurves, first_time_curves_are_plotted;
  float expand1;
  char xlabel1[40], ylabel1[40], title1[40];
  int iplan1, error_bars1, full_caption1;
  int x_is_log10_1, y_is_log10_1;
  wxString m_filename1;

// Data used by "jlp_cgdev_wxwid_newplot" routines
  char nchar1[4*NCURVES_MAX], pcolor1[32*NCURVES_MAX];
  char plot_fname1[128*NCURVES_MAX];
  int *npts1, nmaxi1, nout_maxi1, ncurves1, ncurves_maxi1;
  double x1_1, x2_1, y1_1, y2_1;
 
// Metafile:
#if USE_METAFILE
  FILE *fp_MetaFile;
  char MetaFileName[64];
  bool MetaFileIsOpened;
#endif

// Bitmap associated to backup_dc (= backup device context)
  wxBitmap *backup_dc_bitmap2;
  int m_bitmap_width, m_bitmap_height;

// Postscript backup Device Context: 
  wxPostScriptDC *backup_pst_dc1;
  wxPrintData *backup_printer1;

// Popup menu:
  wxMenu *PopupMenu1, *menuCurve, *menuSetup, *menuCursor;
  wxMenu *menuPen, *menuBackgd, *menuBoxType, *menuBoxLimits;
  wxMenu *menuProcess, *menuLabel;
  int crosshair_cursor;

// Processing popup menu:
  JLP_wx_gsProc1 *m_gsproc1;
  JLP_wx_gsProc *active_gsproc;

// Data points stored for internal use:
  int s_nLeftDown, s_nLeftUp;
  double s_LeftDown_x[MAX_CURSOR_NPOINTS], s_LeftDown_y[MAX_CURSOR_NPOINTS];
  double s_LeftUp_x[MAX_CURSOR_NPOINTS], s_LeftUp_y[MAX_CURSOR_NPOINTS];
// Limits box type: 1=line, 2=rectangle, 3=circle
  int nPointsAreRequired, LimitsBoxType;

// Labels:
  JLP_wxCurveLabels *m_curve_labels1;
  wxString m_label1;

// Gsegraf:
  JLP_Gseg_Wxwid *jlp_gseg_wxwid0;
  JLP_Gsegraf *jlp_gsegraf0;
  int flag_contour_labelling;

  DECLARE_EVENT_TABLE()
}; 

#endif
