/****************************************************************************
* Name: jlp_wx_gpanel_menu.cpp
* 
* JLP
* Version 17/03/2019
***************************************************************************/
#include "jlp_wxlogbook.h"        // JLP_wxLogbook

#include "jlp_wx_gpanel.h"        // jlp_wx_spanel files:
#include "jlp_gdev_wxwid.h"       // JLP_GDev_wxWID class
#include "jlp_gsegraf.h"          // JLP_Gsegraf class
#include "jlp_read_ascii_data.h"  // jlp_ascii_get_ncolumns 
#include "jlp_gseg_axis_data1.h"  // jlp_gseg_init_axis_data()

static int PromptForColumnNumbers(int ncols, int *colx, int *col_y, 
                                  int *col_errorx, int *col_errory);
static int PromptForCurveOptions(char *nchar_type0, char *pcolor0,
                                 int *reset_first0);

/*************************************************************************
* Select and load input gsegrafix parameter file 
*************************************************************************/
int JLP_wxGraphicPanel::wxGP_SelectAndLoadGsegParamFile(wxString &input_filename,
                                                        wxString &err_msg)
{
int status;
char param_filename1[128];

err_msg = _T("");
input_filename = wxFileSelector(_T("Select gsegrafix parameter file"), 
                                _T(""), _T(""), _T("txt|dat"));

if(input_filename.IsEmpty()) {
 err_msg = _T("No file selected: please have another try");
 return(-1);
 }

strncpy(param_filename1, input_filename.mb_str(), 128);

// Initialize plot of drawing panel 
// (and call InitializePlotSetupWithGraphicType)
 status = Drawing_wxgdev->InitializeGsegPlotWithParamFile(param_filename1);

if (status) {
  err_msg = _T("Couldn't load data from '") + input_filename + _T("'.");
  return(-2);
  } else  {
  err_msg = input_filename + _T(" successfuly loaded");
  }

// Write to logbook window (and do not record on the logbook file):
if(jlp_logbook != NULL) jlp_logbook->WriteToLogbook(err_msg, false);


return(0);
}
/*************************************************************************
* Select and load input gdev parameter file 
* (i.e., JLP's version of gsegraph param file) 
*************************************************************************/
int JLP_wxGraphicPanel::wxGP_SelectAndLoadGdevParamFile(wxString &input_filename,
                                                        wxString &err_msg)
{
int status;
char param_filename1[128];

err_msg = _T("");
input_filename = wxFileSelector(_T("Select gdev parameter file"), 
                                _T(""), _T(""), _T("txt|dat"));

if(input_filename.IsEmpty()) {
 err_msg = _T("No file selected: please have another try");
 return(-1);
 }

strncpy(param_filename1, input_filename.mb_str(), 128);

// Initialize plot of drawing panel 
// (and call InitializePlotSetupWithGraphicType)
 status = Drawing_wxgdev->InitializeGdevPlotWithParamFile(param_filename1);

if (status) {
  err_msg = _T("Couldn't load data from '") + input_filename + _T("'.");
  return(-2);
  } else  {
  err_msg = input_filename + _T(" successfuly loaded");
  }

// Write to logbook window (and do not record on the logbook file):
if(jlp_logbook != NULL) jlp_logbook->WriteToLogbook(err_msg, false);


return(0);
}
/*************************************************************************
* Select and load curve from input data file 
*************************************************************************/
int JLP_wxGraphicPanel::wxGP_SelectAndLoadCurveFromFile(
                                                   wxString &input_filename0,
                                                   wxString &err_msg)
{
int status, reset_first1, n_datafiles0, dummy_graphic_gdev0;
int icol_x, icol_y, icol_errorx, icol_errory; 
long ncols, nlines, ndatalines;
char filename1[128], nchar_type1[64], pcolor1[64];
GSEG_AXIS_DATA gseg_axdata0;

err_msg = _T("");
input_filename0 = wxFileSelector(_T("Select ASCII file"), _T(""), _T(""),
                                _T("txt|dat"));

if(input_filename0.IsEmpty()) {
 err_msg = _T("No file selected: please have another try");
 return(-1);
 }

strncpy(filename1, input_filename0.mb_str(), 128);

// reset_first: used to erase previous curves and start from scratch:
reset_first1 = 1;

jlp_ascii_get_ncolumns(filename1, &ncols, &ndatalines, &nlines);
if((ncols < 2) || (ndatalines < 2)) {
 err_msg.Printf(wxT("File not consistent with X,Y plot: ncols=%d ndatalines=%d\n"),
                ncols, ndatalines);
 return(-1);
 }

// Chose column numbers for x and y
if(ncols == 2) {
 icol_x = 1;
 icol_y = 2;
 } else {
 PromptForColumnNumbers(ncols, &icol_x, &icol_y, &icol_errorx, &icol_errory);
 }

// Enter plot settings for this curve:
 PromptForCurveOptions(nchar_type1, pcolor1, &reset_first1);

// Loading curve to jlp_splot private variables (in all cases, even if GSEG)
// In "../jlp_splot/jlp_gdev_curves_process.cpp"
// and defined in "jlp_gdev.h"
 if( Drawing_wxgdev != NULL) {
 status = Drawing_wxgdev->Curves_LoadPlotDataToPrivateFromFile(filename1, 
                                                icol_x, icol_y, icol_errorx, 
                                                icol_errory, nchar_type1, 
                                                pcolor1, reset_first1);
 } else {
 status = -1;
 } 

if (status) {
  err_msg = _T("Couldn't load data from '") + input_filename0 + _T("'.");
  return(-2);
  } else  {
// Load private data to Gseg parameters (dbleimage_1, xplot&, yplot1, etc)
// Load curve/image for Gseg plot to JLP_GDev_wxWID drawing panel
// (use private data : dbleimage_1, xplot&, yplot1, etc)
   n_datafiles0 = 1;  // TOBEDONE LATER...
   dummy_graphic_gdev0 = 4; // Gsegraph 2d curve
// Minimum configuration:
   jlp_gseg_init_axis_data(dummy_graphic_gdev0, &gseg_axdata0);
   status = Drawing_wxgdev->InitializeGsegCurvePlotWithPrivateData(gseg_axdata0,
                                                                  n_datafiles0);
// If "Cancel" was pressed on the popup menu, exit from here
  if(status){
   printf("Cancel was selected: hence, return from here \n");
   return(-1);
   }
  err_msg = input_filename0 + _T(" successfuly loaded");
  }

// Write to logbook window (and do not record on the logbook file):
if(jlp_logbook != NULL) jlp_logbook->WriteToLogbook(err_msg, false);

// Refresh screen:
 Drawing_wxgdev->RedrawToBackupDC(6019);

return(0);
}
/*************************************************************************
* Select and load image from input FITS file 
*************************************************************************/
int JLP_wxGraphicPanel::wxGP_SelectAndLoadFitsImage(wxString &input_filename0,
                                                    wxString &err_msg)
{
int status, reset_first1, iplane, gdev_graphic_type0, contours_option0;
char filename1[128];

err_msg = _T("");
input_filename0 = wxFileSelector(_T("Select FITS file"), _T(""), _T(""),
                  _T("fits|fit|FIT|FITS"),
                  _T("FITS files (*.fits;*.fit;*.FIT:*.FITS)|*.fits;*.fit;*.FIT;*.FITS"));

if(input_filename0.IsEmpty()) {
 err_msg = _T("No file selected: please have another try");
 return(-1);
 }

strcpy(filename1, input_filename0.mb_str());

// reset_first: used to erase previous images and start from scratch:
reset_first1 = 1;
// iplane: number of the image plane of 3D data (from 1 to nz1)
iplane = 1;

// Loading data to dble_image_1... in all cases, even if GSEG
// In "../jlp_splot/jlp_gdev_curves_process.cpp"
// and defined in "jlp_gdev.h"
 status = Drawing_wxgdev->Load2DFitsImage(filename1, iplane);

if (status) {
  err_msg = _T("Couldn't load data from '") + input_filename0 + _T("'.");
  return(-2);
  } else  {
// No contours by default:
 contours_option0 = 0;
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
 gdev_graphic_type0 = 5; // Set it to gsegraf/2d images by default
// Load private data to Gseg parameters (dbleimage_1, xplot&, yplot1, etc)
  status = wxGP_InitializeGsegImagePlotWithPrivateData(gdev_graphic_type0,
                                                       contours_option0);
  status = -1;
  if( Drawing_wxgdev != NULL) {
   }
// If "Cancel" was pressed on the popup menu, exit from here
   if(status){
     return(-1);
     }
   err_msg = input_filename0 + _T(" successfuly loaded");
  }

// Write to logbook window (and do not record on the logbook file):
if(jlp_logbook != NULL) jlp_logbook->WriteToLogbook(err_msg, false);

return(0);
}
/************************************************************************
* Load private data to Gseg parameters (dbleimage_1)
* 
************************************************************************/
int JLP_wxGraphicPanel::wxGP_InitializeGsegImagePlotWithPrivateData(
                                  int gdev_graphic_type0, int contours_option0)
{
int status = -1;

  if( Drawing_wxgdev != NULL) {
// Takes time in case of 3D plots:
// Busy cursor:
   wxBeginBusyCursor();
// Load curve/image for Gseg plot to JLP_GDev_wxWID drawing panel
// (use private data : dbleimage_1, xplot&, yplot1, etc)
   status = Drawing_wxgdev->InitializeGsegImagePlotWithPrivateData(gdev_graphic_type0,
                                                         contours_option0);
// End of busy cursor:
   wxEndBusyCursor();
   }
return(status);
}
/************************************************************************
* Output curve as a JPEG, PNG, etc. file
************************************************************************/
void JLP_wxGraphicPanel::wxGP_SaveGraphicToFile()
{

if(Drawing_wxgdev != NULL) Drawing_wxgdev->SaveGraphicToFile();

return;
}
/************************************************************************
* Output curve as a postscript file
************************************************************************/
void JLP_wxGraphicPanel::wxGP_SaveGraphicToPostscriptFile()
{

if(Drawing_wxgdev != NULL) Drawing_wxgdev->SaveGraphicToPostscriptFile();

return;
}
/************************************************************************
* Interactive change of axis limits 
************************************************************************/
void JLP_wxGraphicPanel::wxGP_ViewChangeAxisLimits()
{

if(Drawing_wxgdev != NULL) Drawing_wxgdev->ViewChangeAxisLimits();

return;
}
/************************************************************************
* Interactive change of projection angles for 3D plots
************************************************************************/
int JLP_wxGraphicPanel::wxGP_ViewAxisRotate()
{
int status = -1;

if(Drawing_wxgdev != NULL) status = Drawing_wxgdev->ViewAxisRotate();

return(status);
}
/************************************************************************
* Interactive contour labelling
************************************************************************/
int JLP_wxGraphicPanel::wxGP_Set_ViewLabelContours()
{
int status = -1;

if(Drawing_wxgdev != NULL) status = Drawing_wxgdev->Set_ViewLabelContours();

return(status);
}
/************************************************************************
* Interactive display coordinates 
************************************************************************/
int JLP_wxGraphicPanel::wxGP_Set_ViewDisplayCoordinates()
{
int status = -1;

if(Drawing_wxgdev != NULL) status = Drawing_wxgdev->Set_ViewDisplayCoordinates();

return(status);
}
/************************************************************************
* Prompt for column numbers 
************************************************************************/
static int PromptForColumnNumbers(int ncols, int *icol_x, int *icol_y,
                                  int *icol_errorx, int *icol_errory)
{
wxString s_values, s_question;
int icolx, icoly, icol_errx, icol_erry;
char buffer[128];

s_question.Printf(wxT("Column number for x, y, x_errors and y_errors (nber of columns = %d) (NB: 0 for error columns if not available) :"), 
                  ncols);

*icol_x = 1;
*icol_y = 2;
*icol_errorx = 0;
*icol_errory = 0;
s_values.Printf(wxT("%d,%d,%d,%d"), *icol_x, *icol_y, *icol_errorx, *icol_errory);

// Prompt for a new value of iframe with a dialog box:
wxString result = wxGetTextFromUser(s_question, _T("PromptForColumnNumbers"),
                                    s_values, NULL);
if(!result.IsEmpty()){
  strcpy(buffer, (const char *)result.mb_str());
    if(sscanf(buffer, "%d,%d", &icolx, &icoly) == 2) {
      *icol_x = icolx;
      *icol_y = icoly;
      }
    if(sscanf(buffer, "%d,%d,%d,%d", &icolx, &icoly, &icol_errx, &icol_erry) == 4) {
      *icol_x = icolx;
      *icol_y = icoly;
      *icol_errorx = icol_errx;
      *icol_errory = icol_erry;
      }
  }

return(0);
}
/************************************************************************
* Prompt for curve options:
*  nchar_type0 
*  pcolor0
*  reset_first0
************************************************************************/
static int PromptForCurveOptions(char *nchar_type0, char *pcolor0,
                                 int *reset_first0)
{
wxString s_values, s_question;
char buffer1[128], buffer2[128], *pc1, *pc2;

s_question.Printf(wxT("nchar_type (L0, L1, 43, 82, etc), pcolor (Black, Blue, RGB_50_50_50, etc), reset_first (to erase the previously entered curves)\n"));

strcpy(nchar_type0, "L0");
strcpy(pcolor0, "Black");
*reset_first0 = 1;
s_values.Printf(wxT("%s,%s,%d"), nchar_type0, pcolor0, *reset_first0);

// Prompt for a new value of iframe with a dialog box:
wxString result = wxGetTextFromUser(s_question, _T("PromptForCurveOptions"),
                                    s_values, NULL);
if(result.IsEmpty() != true){
  strcpy(buffer1, (const char *)result.mb_str());
  buffer1[127]= '\0';
  strcpy(buffer2, buffer1);
  pc1 = buffer1;
  while(*pc1 && *pc1 != ',') pc1++;
 if(*pc1 == ',') {
    *pc1 = '\0';
    pc2 = pc1 + 1;
    strcpy(buffer2, pc2);
    buffer2[127]= '\0';
    strcpy(nchar_type0, buffer1);
   }
  pc2 = buffer2;
// Second parameter;
  while(*pc2 && *pc2 != ',') pc2++;
  if(*pc2 == ',') {
      pc1 = pc2 + 1;
      sscanf(pc1, "%d", reset_first0);
      *pc2 = '\0';
      strcpy(pcolor0, buffer2);
  }
}

return(0);
}
