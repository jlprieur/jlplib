/**************************************************************************
* jlp_gdev_wxwid.h
* JLP_GDev_wxWID class (for Curves/Images)
* Definition of the wxWidgets graphic driver
* used in my plotting package
* Derived from virtual class JLP_GDev defined in "jlp_gdev.h"
*
* JLP
* Version 12/02/2017
**************************************************************************/
#ifndef __jlp_gdev_wxwid_h                     /* sentry */
#define __jlp_gdev_wxwid_h

#include <stdio.h>
//#include <math.h>
#include <ctype.h>

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
#undef index  // To solve problems with index (conflicts with "string.h") ...
    #include "wx/wx.h"
#endif

#include "wx/image.h"
#include "wx/file.h"
#include "wx/filename.h"
#include "wx/mstream.h"
#include "wx/wfstream.h"
#include "wx/quantize.h"
#include "wx/dcmemory.h"         // wxMemoryDC
#include "wx/bitmap.h"
#include "wx/cursor.h"
#include "wx/event.h"

// For clipboard:
#if wxUSE_CLIPBOARD
    #include "wx/dataobj.h"
    #include "wx/clipbrd.h"
#endif // wxUSE_CLIPBOARD

// To define the JLP_Gdevice virtual class,
// and JLP graphic context (JLP_GC) definition
#include "jlp_gdev.h"

#include "jlp_gdev_idv.h"         // GDev_free()
#include "jlp_macros.h"           // NINT, MAXI, MINI, etc
#include "jlp_wx_image1.h"
#include "jlp_wxgdev_shapes.h"    // JLP_SHAPE

#include "jlp_gseg_wxwid.h"       // JLP_Gseg_wxWID class
#include "jlp_gsegraf.h"          // JLP_Gsegraf class and UINT32

// Maximum number of data points entered by the user with the cursor:
#define MAX_CURSOR_NPOINTS 1024

#define NBUT_MAX 20               // Maximum number of buttons
#define POPUP_NITEMS_MAX 20       // Maximum number of items in popup menu

//************* wxGDev_SETTINGS: ****************************************//
typedef struct {
wxColour pen_colour;
wxColour pen_default_colour;
wxColour backgd_colour;
/* Cursor types: Arrow, DownArrow, Cross, CrossHair, CrossHair1
*/
wxString cursor_type;
int ticks_in, box_type, xgrid, ygrid;
char lut_type[32];
int lut_reversed;
// Filter_type : 0= none
// 1=soft unsharp (UNSH1) 2=medium unsharp (UNSH2) 3=hard unsharp (UNSH3)
// 4=high contrast1 (HC1) 5=high contrast2 (HC2) 6=Very high contrast (VHC)
int filter;
/*
* ITT_thresh: Threshold determination
* "FromBox" "DirectInput" "MinMax" "HC" (high contrast)
* or "VHC" (very high contrast)
*/
wxString itt_type;
int itt_is_linear;
// Box used on the displayed image to compute the ITT thresholds: low_itt_thresh, up_itt_thresh)
int itt_x1_box, itt_y1_box, itt_x2_box, itt_y2_box;
double low_itt_thresh, up_itt_thresh;
//
int zoom;
/* InternalProcessingMode
* -1=None 0=Automatic thresholds 1=Statistics, 2=Astrometry 3=Photometry
* 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
* 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
* 13=Add a ring 14=Remove a shape 15=Rotate a shape 16=Magnify a shape
* 18=ZoomWithBox 19=GetCoordinates
*/
int InternalProcessingMode;
}
wxGDev_SETTINGS;
//*********************************************************************//

// Simple declaration to avoid "boomerang" error
class  JLP_wx_GDProc1;
class  JLP_wx_GDProc;
class JLP_wxGDevLabels;
class JLP_wxImageShapes;
class JLP_wxImagePanel;
class JLP_wxGraphicPanel;
class JLP_wxGDev_Popup;
class JLP_wxScrolled1;

 class JLP_GDev_wxWID : public wxPanel, public JLP_GDev {

public:

/* Constructor for scrolled images:
* in "jlp_gdev_wxwid_images.cpp"
*   JLP_GDev_wxWID("display", "Xdisp1 version 2006", 64, 64, 1, &gamma1,
*                  &gamma_d, &status, 1);
*/
 JLP_GDev_wxWID(JLP_wxImagePanel *parent_ipanel, wxWindowID id,
                 wxStatusBar *parent_status_bar,
                 const wxPoint pos0, const wxSize size0,
                 const int should_fit_inside);

 JLP_GDev_wxWID(JLP_wxImagePanel *parent_ipanel, wxWindowID id,
                 wxStaticText *static_coord, wxStaticText *static_help,
                 const wxPoint pos0, const wxSize size0,
                 const int should_fit_inside);

/* Constructors for curves:
* in "jlp_gdev_wxwid_curves.cpp"
*/
/**
 JLP_GDev_wxWID(wxPanel *parent_panel, wxWindowID id,
                const wxPoint &pos, const wxSize &size,
                const char *plotdev,
                const char *out_filename, const char* title,
                double xmin_user, double xmax_user, double ymin_user,
                double ymax_user, const int plan, int& status);
***/

 JLP_GDev_wxWID(JLP_wxGraphicPanel *parent_panel, wxWindowID id,
                const wxPoint &pos,
                const wxSize &size, wxStaticText *static_coord,
                wxStaticText *static_help);

 JLP_GDev_wxWID(wxPanel *parent_panel, wxWindowID id, const wxPoint &pos,
                 const wxSize &size, wxStaticText *static_coord,
                 wxStaticText *static_help);

/* Destructor:
* (Should be declared as virtual)
*/
  ~JLP_GDev_wxWID() {
#if USE_METAFILE
    CloseMetaFile();
#endif
// Free allocated index:
  GDev_free_idv(Jgc0.dev_idv);
#ifdef DEBUG
  printf("JLP_GDev_wxWID: destructor has been called\n");
#endif
  return;
  };

/* Close graphic device
*/
  int gdev_close() {
    printf("JLP_GDev_wxWID: gdev_close() called here \n");
    this->~JLP_GDev_wxWID();
    return(0);
    }

// To set Jgc0.cwidth and Jgc0.cheight:
  int Jgc0_set_font_size_from_device(double expand0);

// Wait for next event (mouse button for X11)
  int wait_for_events();

// (virtual routines of JLP_GDev) contained in "jlp_wx_wxwid_images.cpp"
  void Update_JGC_from_GDProcSettings();
  void Update_GDProcSettings_from_JGC();
  int setup_device_for_image(const char *plotdev, const char* title,
                             const int nx1, const int ny1, int *offx,
                             int *offy, int *axlen, int *aylen,
                             int *dev_width, int *dev_height, int *TeX_flag,
                             int *devtype, int *landscape);
  int setup_device_for_curve(const char *plotdev, const char* title,
                             int *dev_width, int *dev_height, int *TeX_flag,
                             int *devtype, int *landscape);
  int open_device(const char* title, int dev_width, int dev_height,
                  const int landscape, int *jgc_dev_width, int *jgc_dev_height,
                  int *dev_yorigin_is_on_top);
  int get_winlimits(double *u_x1, double *u_y1, double *u_x2,
                    double *u_y2, int type_of_win, int *pressed_button,
                    int *in_frame);
  int get_circles(double *x_cent, double *y_cent, double *diam1,
                   double *diam2, int *ncirc);
  int gdev_gflush();
  int close_device();
// other routines included in "jlp_wx_wxwid_images.cpp"
  void CreateScrolledImagePanel(const wxPoint pos0, const wxSize size0);
  void MainInitForImages();
  void ResetAllPrivateParametersForCurves();
  void ResetAllPrivateParametersForImages();
  void GDevGet_wxGDevSettings(wxGDev_SETTINGS *wxgdev_settings0);
  void GDevLoad_wxGDevSettings(wxGDev_SETTINGS wxgdev_settings0);
  void SetNewSize(wxSize new_size);
  void MySetSize();
  void GetImageLabelOptions(bool *has_labels, bool *has_scale_bar,
                            bool *has_north_east, bool *has_shapes);
/*
* Setup menu:
*/
/*
  virtual int setup_menu(char *items, const int nitems,
                         const int menu_nsub, const int menu_slen,
                         const int vertical);
*/

// Select menu item:
/*
  virtual int select_menu(int& select, int& subselect);
*/

// Defined in jlp_gdev_wxwid_popup_update.cpp:
  void ApplyCursorSettings(wxGDev_SETTINGS wxgdev_settings0);
  void ApplyLUTSettings(wxGDev_SETTINGS wxgdev_settings0,
                               const int update_display);
  void ApplyGsegrafSettings(wxGDev_SETTINGS wxgdev_settings0,
                                   const int update_display);
  void ApplyITTSettings(wxGDev_SETTINGS wxgdev_settings0,
                               const int update_display);
  void ApplyPenColourSettings(wxGDev_SETTINGS wxgdev_settings0,
                                     UINT32 canvas_fg_color0,
                                     const int update_display);
  void ApplyBackgroundColourSettings(wxGDev_SETTINGS wxgdev_settings0,
                                            UINT32 canvas_bg_color0,
                                            const int update_display);
  void ApplyBoxTypeSettings(wxGDev_SETTINGS wxgdev_settings0,
                                   const int update_display);
  void ApplyInternalProcessingModeSettings(wxGDev_SETTINGS wxgdev_settings0);
  void PromptForNewThresholds();

// Cursor function not available here:
  int cursor(int& x, int& y, char* cursor_type, int& pressed_button) {
    fprintf(stderr,"Sorry cursor function not available here\n");
    return(-1);};


// in "jlp_dev_wxwid_images.cpp"
  void ClearDrawingDisplay();
  void PlotToDrawingDisplay();

/***********************************************************************
* Plot routines in "jlp_gdev_wxwid_plot.cpp":
* needed by the virtual definition of JLP_GDev
************************************************************************/
  int SetFontToBackupDC(char *font_name, const unsigned int font_size);
  int line_device(int x1, int y1, int x2, int y2, int lwidth=0);
  int SetLineWidthAndType(int lwidth, int ltype);
  double label_device(const char *s, int xstart, int ystart, double angle,
                     double expand, int draw_it);
  int gdev_erase();
  int alloc_lut_device(int *mylut, int& private_lut, int& ncolors);
  int load_lut_device(const int *r, const int *g, const int *b,
                      int *mylut, int ncolors);
  int setrgbcolor(double r, double g, double b);
  int setdefaultcolor();
  int polygon_device(int x, int y, double expand, double angle, int nsides,
                     int filled);
  int plot_image(int *image, int nx, int ny, int idim, int xstart, int ystart,
                 int gamma1, int gamma_d, int black_and_white);
// Complementary routines:
  void SetPenColour_dc(const wxColour wxC);
  void SetBackgdColour_dc(const wxColour wxC);


// needed by the virtual definition of JLP_GDev
// but not available:
// To draw a circle:
  int circle_device(int x, int y, int idiam, int filled) {return(-1);};


// MetaFile utilities
#if USE_METAFILE
  int OpenMetaFile(const char *mode);
  int CloseMetaFile();
  int NewNameForMetaFile();
  int ReadAndExecuteMetaFile();
  int ExecuteMetaFileLine(char *metaline);
#endif

// Defined in "jlp_gdev_wxwid_events.cpp"
  void ShowPopupMenu();

// Defined in jlp_gdev_wxwid_onclick.cpp:
  void DisplayDbleImage3D();
  void DisplayDbleImageContours();
  void BoxLimitsPrompt();
  void ApplyZoomSettings(wxGDev_SETTINGS wxgdev_settings0,
                     const int update_display);
  void GDevDisplayInfoImage();
  void GDevDisplayInfoCurve();
  void GDevUpdatePopupMenu_SelectFilter(int filter0);
  void GDevCopyToClipboard();
  void GDevPasteFromClipboard();

// In jlp_igdev_wxwid_save.cpp:
  void SaveGraphicToFile();
  int PstCopyOfDisplayFromBitmapForGseg(char *pst_filename);
  int PstCopyOfDisplayFromBackupForGseg(char *pst_filename);
  int PstCopyOfDisplayForCurves(char *pst_filename);
  int PstCopyOfDisplayForImages(char *pst_filename);
  void SaveGraphicToPostscriptFile();
  void SaveBackupPostscriptToFile();

//


// Defined in jlp_gdev_wxwid_utils.cpp:
  int CursorGetCoord(int& x, int& y, char* cursor_type, int& pressed_button);
  int CreateCursorPosition(int ix, int iy, int width, int height);
  int Get_idev(int& idev);
  void Get_ClientSize(int *width0, int *height0);
// UC=user_coordinates
  int DrawRectangle_UC(double xstart, double ystart, double xend, double yend,
                       const int rr, const int gg, const int bb, 
                       const int pen_width, const int filled);
// To fill a rectangle (i.e. to paint it with a color):
  int gdev_FilledRect1(double x0, double y0, double x1, double y1,
                       char *pcolor) {
   int status, pen_width = 1, filled = 1;
   int rr, gg, bb;
// ZZZ: not implemented yet with the good color, so I set it to red:
   rr = 255; 
   gg = 0; 
   bb = 0; 
   status = DrawRectangle_UC(x0, y0, x1, y1, rr, gg, bb, pen_width, filled);
   return(status);
   }

  void SetPenColour(const wxColour wxC);
  void SetPenColour(const int rr, const int gg, const int bb);
  void SetPenColour(const UINT32 color_rgba0);
  int GetImageArray(double **array0, int *nx0, int *ny0);
  int GetFilterSelection();
  int GDWX_WriteToLogbook(wxString str1, bool save_to_file0);

// Interface with c_image1:
  int GDevSet_ITT_Thresh(wxString itt_mode_str, const double lower_itt,
                     const double upper_itt, const int box_x1, const int box_y1,
                     const int box_x2, const int box_y2);
  void GDevSetITT_type(wxString itt_type);
  void GDevSetITT_linear(int is_linear);
  void GDevSetLUT(char *lut_type);
  void GDevSetFilter(int filter_type);

// in jlp_gdev_wxwid_process.cpp:
  void OnMotion(wxMouseEvent& event);
  void OnLeftDown(wxMouseEvent& event);
  void OnLeftUp(wxMouseEvent& event);
  void OnRightUp(wxMouseEvent& event);
  void HandleDraggingMotion(wxMouseEvent& event, wxString str_coordinates1);
// ProcessingMode: auto_scale, photometry, labels, etc.
  int SetInternalProcessingMode(int processing_mode);
// External processingMode: binary star measurements, for instance
  int SetExternalProcessingMode(JLP_wx_GDProc *external_gdproc);
  int GetActiveExternalProcessingHelpText(wxString *str0, int *ix0, int *iy0);
  int GetUserCoordFromCursor(double *x1, double *y1, int *in_frame) {
   fprintf(stderr, "GetUserCoordFromCursor/NOT USED HERE \n");
   return(-1);
   };
  int LeftUp_GetLastData(int *nn, double *x1, double *y1);
  int LeftDown_GetLastData(int *nn, double *x1, double *y1);
// >>>aa
// in jlp_igdev_wxwid_process.cpp:
  int UserCoordFromMouse(long idev_x0, long idev_y0,
                         wxString *str_coordinates0, int *in_frame);
  void DisplayCoordinatesToScreen( wxDC *dc0, wxString str_coordinates0);
  void wxGdev_Refresh();

// in jlp_gdev_wxwid_canvas.cpp:
  int LoadNewCImage1(JLP_wxImage1 *image1);
  void ApplyCurrentSettingsToCImage1();
  int RestoreOriginalCImage1();
  int ReadCImage1Data(double *array, int nx, int ny);
  int UpdateCImage1Values(double *array, int nx, int ny,
                          const bool reset_ITT_to_MinMax);
  void ConvDevToUserForImages(double dev_x0, double dev_y0,
                              double *user_x1, double *user_y1, int *in_frame);
  void ConvUserToDevForImages(double user_x1, double user_y1,
                              double *dev_x0, double *dev_y0, int *in_frame);
  void InitBackupDC(int width0, int height0);
  void DrawHelpInfo(wxDC *dc1, wxString help_text);
  void DisplayCoordinatesForCurves(wxString coordinates_text);
  void DisplayCoordinatesForImages(wxDC *dc1, wxString str1);
  void OnPaint(wxPaintEvent &event);
  void OnScrollPaint(wxPaintEvent &event);
  void wxGDev_RenderForCurves(wxDC *dc1, const int width1, const int height1);
  void wxGDev_RenderForScrolledImages( wxDC *scroll_dc0 );
  void RedrawToBackupDC(const int calling_routine_nber);
  void RedrawScrolledImageToBackupDC();
  void PlotAllCurves_gseg();
  void ResizePlot1(const int width0, const int height0);
  void OnResize(wxSizeEvent &event);
  int SetNewBitmapSize(int width, int height);

// Image labels:
// in jlp_gdev_wxwid_labels.cpp:
  void GDevAddLabel();
  void AddLabel(wxString new_label, double dev_x0, double dev_y0);
  int AddLabel_gseg(wxString new_label, double dev_x0, double dev_y0);
  void RemoveLabel(double x0, double y0);
  void GDevAddScaleBar();
  void GDevAddNorthEastLabel();
  void RemoveScaleBar();
  void MoveScaleBar(double x0, double y0);
  void RemoveNorthEastLabel();
  void UpdateNorthEastLabel(double radius, double xc, double yc);
  void GDevSetLabelContours();

// Image shapes:
// in jlp_gdev_wxwid_shapes.cpp:
  void AddShape(double x1, double y1, double x2, double y2, int shape_type);
  void RemoveShape(double x0, double y0);
  void MoveShape(double x1, double y1, double x2, double y2);
  void RotateShape(double x1, double y1, double x2, double y2);
  void MagnifyShape(double x1, double y1, double x2, double y2);
  int GetShape(const int ishape, JLP_SHAPE *shape0);
  int GetNShapes(int *nshapes);
  int GetClosestShape(double x0, double y0, int *imin, JLP_SHAPE *shape0);
  void EraseAllShapes();
  void CancelLastShape();

// TBD to be done later
//  void ZoomIn(double x1, double y1, double x2, double y2);

// Gdev with plot parameter file:
// "jlp_gdev_wxwid_gdev_param.cpp"
  int InitializeGdevPlotWithParamFile(char *param_filename0);

// Gsegraf:
// "jlp_gdev_wxwid_gseg.cpp"
// Graphic interface:
  int InitializeGsegPlotWithParamFile(char *param_filename0);
  int InitializeGsegCurvePlotWithPrivateData(GSEG_AXIS_DATA gseg_axdata0,
                                             int n_datafiles);
  int InitializeGsegImagePlotWithPrivateData(int gdev_graphic_type0,
                                        const int contours_option0);
  int LoadDbleImage1ToGsegPlot(GSEG_PLOT_DATA *gseg_pltdata0,
                               GSEG_AXIS_DATA *gseg_axdata0,
                               int gseg_plot_type0, char *axis_type0,
                               const int contours_option0,
                               const int pen_width0);
  int LoadCImage1ToGsegPlot(GSEG_PLOT_DATA *gseg_pltdata0,
                            GSEG_AXIS_DATA *gseg_axdata0);
  int InitializeGsegPlotWithPlotData(GSEG_PLOT_DATA *gseg_pltdata0,
                                     const int nplots0,
                                     GSEG_AXIS_DATA gseg_axdata0);
  int InitializePlotSetupWithGDevGraphicType(int gdev_graphic_type0);
  void CloseBackupPostscriptDC();
  int OpenBackupPostscriptDC(char *save_filename0);
  void ViewChangeAxisLimits();
  bool ViewAxisRotateIsPossible();
  int ViewAxisRotate();
  bool ViewLabelContoursIsPossible();
  int Set_ViewLabelContours();
  int Callback_ViewLabelContours(const double dev_x10, const double dev_y10,
                                 char *label_0, const int keypress_inc);
  void Set_ZoomIn();
  int Callback_ZoomIn(const double dev_x10, const double dev_y10,
                      const double dev_x20, const double dev_y0);
  int Set_ViewDisplayCoordinates();
  int Callback_ViewDisplayCoordinates(const double dev_x10,
                                      const double dev_y10, char *label_0,
                                      const int keypress_inc);
  int Callback_ViewDisplayCoordinates(const double dev_x10,
                                      const double dev_y10,
                                      double *x_data, double *y_data,
                                      double *z_data, const int keypress_inc);
  int FromDeviceToUserCoord(const double dev_x1, const double dev_y1,
                            double *user_x1, double *user_y1, int *in_frame);
  int Gseg_GetImageArray(double **array0, int *nx0, int *ny0);
  int Gseg_GetCurveData(double **xplot0, double **yplot0, int *npts0,
                         const int icurve);
  int Gseg_LoadCurvePlotDataFromFile(char *filename0, const int reset_first0,
                                     char *axis_type0);
  int Gseg_LoadImagePlotDataFromFitsFile(char *filename0,
                                         const int reset_first0,
                                         char *axis_type0);

// Accessors:
 void GetInternalProcessingMode(int *pmode){
   *pmode = wxgdev_settings1.InternalProcessingMode;
  }
// LUT settings
 int GetLUT_Settings(char *lut_type0, int *lut_reversed)
  {
  int status = -1;
    strcpy(lut_type0, "");
    *lut_reversed = 0;
    if(c_image1 != NULL) {
      c_image1->Get_LUT_type(lut_type0);
      *lut_reversed = c_image1->Get_LUT_reversed();
      status = 0;
      }
  return(status);
  }
// ITT settings
 int GDevGet_ITT_Thresh(wxString *itt_type0, int *is_linear0,
                        double *low_threshold0, double *upper_threshold0,
                        int *box_x10, int *box_x20, int *box_y10, int *box_y20)
  {
  int status = -1;
    *low_threshold0 = 0.;
    *upper_threshold0 = 1.;
    *itt_type0 = wxT("");
    *is_linear0 = 0;
    *box_x10 = 0;
    *box_x20 = 0;
    *box_y10 = 0;
    *box_y20 = 0;
    if(c_image1 != NULL) {
// ZZZQQQ      *itt_type0 = wxgdev_settings1.itt_type;
      c_image1->GetITT_Thresh(itt_type0,
                              low_threshold0, upper_threshold0,
                              box_x10, box_y10, box_x20, box_y20);
      *is_linear0 = c_image1->ITT_Is_Linear();
      status = 0;
      }
  return(status);
  }
// Zoom gamma values
 int GetZoomGammaValues(int *gamma1, int *gamma_d)
  {
  int status = -1;
    *gamma1 = 1;
    *gamma_d = 1;
    if(c_image1 != NULL) {
      c_image1->Get_gamma(gamma1, gamma_d);
      status = 0;
      }
  return(status);
  }
wxBitmap *backup_dc_bitmap2_ptr(){return backup_dc_bitmap2;};
// Backup Device Context (accessible to public, for drawing)
  wxMemoryDC *backup_dc;

private:

  wxStatusBar *m_StatusBar;
  wxStaticText *m_StaticCoord, *m_StaticHelp;
  char font_name1[64];
  unsigned int font_size1;

//  wxPanel *m_parent_panel, *m_panel;
  JLP_wxImagePanel *m_parent_ipanel;
  JLP_wxGraphicPanel *m_parent_gpanel;
  wxSize m_size;
  int client_width1, client_height1;
  int initialized;

// Settings:
  wxGDev_SETTINGS wxgdev_settings1;

// Metafile:
#if USE_METAFILE
  FILE *fp_MetaFile;
  char MetaFileName[64];
  bool MetaFileIsOpened;
#endif

// Bitmap associated to backup_dc (= backup device context)
  wxBitmap *backup_dc_bitmap2;
  int nx2, ny2, should_fit_inside1;
  int m_bitmap_width, m_bitmap_height;

// Popup menu (PopupMenu1):
  JLP_wxGDev_Popup *m_popup_menu1;
  int crosshair_cursor;

// Temporary only:
  wxBoxSizer *m_topsizer;

// Processing popup menu:
  JLP_wx_GDProc1 *m_gdproc1;
  JLP_wx_GDProc *active_gdproc;

// Labels:
  JLP_wxGDevLabels *m_wxlabels1;
  wxString m_label1;

// Shapes:
  JLP_wxGDevShapes *m_wxshapes1;

// Scrolled image
  JLP_wxScrolled1 *m_scrolled_window;

  JLP_wxImage1 *c_image1, *work_image1;

// Parameters for cosmetic patches:
  double m_radius_fact, m_sigma_noise;
  int m_poly_order, m_polynomial_method;

//************ Gsegraf:
  JLP_Gseg_Wxwid *jlp_gseg_wxwid1;
  JLP_Gsegraf *jlp_gsegraf1;
  int gseg_is_activated, gseg_iplot_contours1, gseg_iplot_coords1;

// Postscript backup Device Context:
  wxPostScriptDC *backup_pst_dc1;
  wxPrintData *backup_printer1;

  DECLARE_EVENT_TABLE()
};

// in "jlp_gdev_wxwid_utils.cpp"
int jlp_convert_uint32_to_wxcolour(const UINT32 color_rgba0, wxColour *wxC);

#endif
