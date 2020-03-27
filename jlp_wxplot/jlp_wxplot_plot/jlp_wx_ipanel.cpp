/****************************************************************************
* Name: jlp_wx_ipanel.cpp
* Image Panel
*
* JLP
* Version 21/10/2015
***************************************************************************/
#include <stdio.h>
#include <stdlib.h>       // exit()
// lib_splot files:
#include "jlp_gdev_idv.h"
/*
#include "jlp_gdev_pst.h"
#include "jlp_gdev_wxwid.h"
*/
#include "jlp_splot_idv.h"  // jlp_hardcopy_image()
#include "jlp_wx_ipanel.h"
#include "jlp_gdev_pst.h"  // JLP_GDev_PST class
#include "jlp_wx_pstcopy_dlg.h"  // JLP_PstCopy_Dlg()

//----------------------------------------------------------------------
// JLP_wxImagePanel
//----------------------------------------------------------------------

BEGIN_EVENT_TABLE(JLP_wxImagePanel, wxPanel)
// catch size events
EVT_SIZE (JLP_wxImagePanel::OnResize)

END_EVENT_TABLE()

/*******************************************************************************
* Constructor without Logbook and StatusBar
*******************************************************************************/
JLP_wxImagePanel::JLP_wxImagePanel(wxFrame *frame, wxStaticText *static_coord,
                                   wxStaticText *static_help,
                                   int x, int y, int w, int h,
                                   const int should_fit_inside)
                         : wxPanel(frame, wxID_ANY, wxPoint(x, y), wxSize(w, h))
{
m_StatusBar = NULL;
jlp_logbook = NULL;
m_StaticCoord = static_coord;
m_StaticHelp = static_help;
should_fit_inside1 = should_fit_inside;
wxIP_MainInit();
}
/*******************************************************************************
* Constructor with StatusBar
*******************************************************************************/
JLP_wxImagePanel::JLP_wxImagePanel(wxFrame *frame, JLP_wxLogbook *logbook,
                                   wxStatusBar *frame_status_bar,
                                   int x, int y, int w, int h,
                                   const int should_fit_inside)
                         : wxPanel(frame, wxID_ANY, wxPoint(x, y), wxSize(w, h))
{
m_StatusBar = frame_status_bar;
jlp_logbook = logbook;
m_StaticCoord = NULL;
m_StaticHelp = NULL;
should_fit_inside1 = should_fit_inside;
wxIP_MainInit();
}
/*******************************************************************************
*
*******************************************************************************/
void JLP_wxImagePanel::wxIP_MainInit()
{
wxBoxSizer  *my_hsizer;
int i;

// Transform coma into point for numbers:
setlocale(LC_NUMERIC, "C");

// Set background colour:
SetBackgroundColour(wxColour(*wxBLACK));

initialized = 0;

dble_image1 = NULL;
dble_orig_image1 = NULL;
c_image1 = NULL;
UnresolvedAutoc = NULL;
UnresolvedModsq = NULL;
SigmaUnresolvedModsq = 1.e-6;

// Initialization of the private parameters:
  size0 = this->GetClientSize();
  size0.x -= 20;
  size0.y -= 100;

// Create new GDev, image/graphic device (with popup menu)

if(m_StatusBar != NULL) {
  Image1_wxgdev = new JLP_GDev_wxWID(this, wxID_ANY, m_StatusBar,
                                     wxPoint(10, 70), size0,
                                     should_fit_inside1);
  } else {
  Image1_wxgdev = new JLP_GDev_wxWID(this, wxID_ANY,
                                     m_StaticCoord, m_StaticHelp,
                                     wxPoint(10, 70), size0,
                                     should_fit_inside1);
  }

// Create a sizer to locate panel
  my_hsizer = new wxBoxSizer( wxVERTICAL );

// The initial size of m_scrolled1 is interpreted as the minimal size:
// 1 : make vertically stretchable
// wxEXPAND : make horizontally stretchable, and the item will be expanded
// to fill the space assigned to the item.
  my_hsizer->Add(Image1_wxgdev, 1, wxEXPAND, wxALL );

// Load a dummy image:
  nx1 = 64;
  ny1 = 64;
  dble_image1 = new double[nx1 * ny1];
  dble_orig_image1 = new double[nx1 * ny1];
  max_lut_level1 =  Image1_wxgdev->GetMaxLevelForLUT();
  for(i = 0; i < nx1 *nx1; i++) {
   dble_orig_image1[i] = (double) i;
   dble_image1[i] = (double) i;
   }
  c_image1 = new JLP_wxImage1(dble_image1, nx1, ny1, size0,
                              should_fit_inside1, max_lut_level1);
  Image1_wxgdev->LoadNewCImage1(c_image1);

  SetSizer(my_hsizer);

// NB: SHOULD NOT DELETE my_hsizer !!
}
/************************************************************************
* Destructor
*************************************************************************/
JLP_wxImagePanel::~JLP_wxImagePanel()
{
if(Image1_wxgdev != NULL) delete Image1_wxgdev;
if(c_image1 != NULL) delete c_image1;
if(dble_image1 != NULL) delete[] dble_image1;
if(dble_orig_image1 != NULL) delete[] dble_orig_image1;
Close();
}
/************************************************************************
* To enlarge the image bitmap to fill the new window
*
************************************************************************/
void JLP_wxImagePanel::OnResize( wxSizeEvent &event )
{
wxSize main_size, new_size;

main_size = event.GetSize();

#ifdef OLDDDD

// Resize plot area:
  new_size = m_GdpFrame->GetSize();

// Minimum size is (100,200)
  if(new_size.x <= 100 || new_size.y <= 200) new_size = main_size;

#else

  new_size = main_size;

#endif

// Resize Image panel:
 Image1_wxgdev->SetNewSize(new_size);

 Image1_wxgdev->wxGdev_Refresh();

// Skip this event (to avoid infinite processing of the same event):
 event.Skip();
return;
}
/************************************************************************
* Dialog with Wdisplay1:
************************************************************************/
int JLP_wxImagePanel::wxIP_LoadImage(double *dble_image0, int nx0, int ny0)
{
int i, filter0;
wxString buffer;
bool reset_ITT_to_MinMax, change_size0;
wxSize window_size0;

change_size0 = (nx0 != nx1 || ny0 != ny1);

if(change_size0) {
  if(dble_image1 != NULL) delete[] dble_image1;
  if(dble_orig_image1 != NULL) delete[] dble_orig_image1;
  nx1 = nx0;
  ny1 = ny0;
  dble_image1 = new double[nx1 * ny1];
  dble_orig_image1 = new double[nx1 * ny1];
 }

for(i = 0; i < nx1 * ny1; i++) dble_image1[i] = dble_image0[i];
for(i = 0; i < nx1 * ny1; i++) dble_orig_image1[i] = dble_image0[i];

// Load double image to private array dble_image1 of JLP_GDev Image1_wxgdev
 Image1_wxgdev->LoadDbleImage1(dble_image1, nx1, ny1);

// If new size create new c_image
if(change_size0) {
// Update window_size0 (the panel size may have been changed by the user)
 window_size0 = GetClientSize();

   max_lut_level1 =  Image1_wxgdev->GetMaxLevelForLUT();
// Initialize the parameters of m_image1 from input dble_image:
   if(c_image1 != NULL) delete c_image1;
   c_image1 = new JLP_wxImage1(dble_image1, nx1, ny1, window_size0,
                               should_fit_inside1, max_lut_level1);

   Image1_wxgdev->LoadNewCImage1(c_image1);
// If same size:
} else {
  filter0 = Image1_wxgdev->GetFilterSelection();
  reset_ITT_to_MinMax = false;
  if(filter0 != 0) {ApplyFilter(filter0);}
  Image1_wxgdev->UpdateCImage1Values(dble_image1, nx1, ny1, reset_ITT_to_MinMax);
}

initialized = 1234;

return(0);
}
/************************************************************************
* Erase image and original image
************************************************************************/
int JLP_wxImagePanel::wxIP_Erase()
{
int i;
wxString str1;
bool reset_ITT_to_MinMax;

if(dble_image1 == NULL || dble_orig_image1 == NULL) return(-1);
for(i = 0; i < nx1 * ny1; i++) {
  dble_image1[i] = 0.;
  dble_orig_image1[i] = 0.;
}

// Do not reset ITT settings to keep previous settings
// (that may also be entered by the user):
  reset_ITT_to_MinMax = false;
  Image1_wxgdev->UpdateCImage1Values(dble_image1, nx1, ny1, reset_ITT_to_MinMax);

return(0);
}
/************************************************************************
* Restore original image
************************************************************************/
int JLP_wxImagePanel::RestoreOriginalImage()
{
int i;
wxString str1;
bool reset_ITT_to_MinMax;

  if(dble_image1 == NULL || dble_orig_image1 == NULL)return(-1);
  for(i = 0; i < nx1 * ny1; i++) dble_image1[i] = dble_orig_image1[i];

  reset_ITT_to_MinMax = true;
  Image1_wxgdev->UpdateCImage1Values(dble_image1, nx1, ny1, reset_ITT_to_MinMax);

// Write to logbook window (and record on the logbook file):
  str1 = wxT("%% Back to original image\n");
  if(jlp_logbook != NULL) jlp_logbook->WriteToLogbook(str1, true);

return(0);
}
/************************************************************************
* To save display to postscript file
* (called by Gdisp1)
************************************************************************/
void JLP_wxImagePanel::PstCopyOfDisplay(char *input_filename, char *pst_filename)
{
JLP_PstCopy_Dlg *PstCopyDlg;
int answer, tex_fonts, black_and_white, high_resolution, lut_scale;
float *image_f1;
int *image_i, f1_nx1, f1_ny1, nx2, ny2;
double low_thresh, high_thresh, plot_width, plot_height;
int ncolors, f1_xstart, f1_ystart, f1_xend, f1_yend;
char title[80], image_comments[80], graphic_file[64];
char plotdev[60];
int i, lut_reversed, lut_offset, lut_slope, itt_is_linear;
char lut_type[32];

if(c_image1 == NULL) return;

// Options for hardcopy:
 PstCopyDlg = new JLP_PstCopy_Dlg(NULL, wxT("Options for postscript copy"));

// WARNING: There is a bug here coming from "wxwidgets"
// when using ShowModal, the system doesn't return
// The computer may even crash if there are too many of those processed hanging
// around and using CPU time !
 answer = PstCopyDlg->ShowModal();

 if(answer == 0) {
// Retrieve the patch parameters have another try with those parameters
   PstCopyDlg->RetrieveData(&tex_fonts, &black_and_white, &high_resolution,
                            &lut_scale);
   delete PstCopyDlg;
// Return without harcopy:
   } else {
   delete PstCopyDlg;
   return;
   }

f1_nx1 = nx1;
f1_ny1 = ny1;
image_f1 = new float[nx1 * ny1];
for(i = 0; i < nx1 * ny1; i++) image_f1[i] = (float)dble_image1[i];

// Work space for pstcopy1 only:
nx2 = nx1;
ny2 = ny1;
image_i = new int[nx1 * ny1];
for(i = 0; i < nx1 * ny1; i++) image_i[i] = 0;

 ncolors = 256;
 strcpy(image_comments, "");


/* Zoomed image:
r TOBEDONE later: zoom ...
* f1_xstart, f1_ystart, ,f1_xend, f1_yend
*/
f1_xstart = 0; f1_ystart = 0;
f1_xend = nx1; f1_yend = ny1;
// width, height: output size of the image on the plot (cm)
 plot_width = 15.;
 plot_height = 15.;
 itt_is_linear = c_image1->ITT_Is_Linear();
 low_thresh = Get_Low_Threshold();
 high_thresh = Get_Up_Threshold();
 c_image1->Get_LUT_type(lut_type);
 c_image1->Get_LUT_param(lut_offset, lut_slope, lut_reversed);
 strcpy(graphic_file, "");
 strcpy(plotdev, "square");
 strcpy(title, "");

/*int jlp_hardcopy_image(float *image_f1, int *image_i, int *i_nx22,
                       int *i_ny22, int *nx2, int *ncolors,
                       char *input_filename,
                       char *comments, int lut_scale, int black_and_white,
                       int high_resolution, int *nx1, int *ny1,
                       int f1_xmin, int f1_ymin, int *f1_nx11,
                       int *f1_ny11, int width_frame, int height_frame,
                       int itt_is_linear, double lower_itt, double upper_itt,
                       char *lut_type, int inversed_lut, int lut_offset,
                       int lut_slope, char *graphic_file, char *pst_plotdev,
                       char *out_filename, char *title)
*/
jlp_hardcopy_image(image_f1, image_i, &nx2, &ny2, &nx2, &ncolors,
                   input_filename, image_comments,
                   lut_scale, black_and_white, high_resolution,
                   &nx1, & ny1, f1_xstart, f1_ystart, &f1_nx1,
                   &f1_ny1, plot_width, plot_height,
                   itt_is_linear, low_thresh, high_thresh,
                   lut_type, lut_reversed, lut_offset, lut_slope,
                   graphic_file, plotdev, pst_filename, title);


delete[] image_f1;
delete[] image_i;

return;
}
/**********************************************************************
* Display/Save string to logbook
***********************************************************************/
int JLP_wxImagePanel::wxIP_WriteToLogbook(wxString str1, bool save_to_file0)
{
int status = -1;

if(initialized == 1234 && jlp_logbook != NULL) {
 status = jlp_logbook->WriteToLogbook(str1, save_to_file0);
 }

return(status);
}
