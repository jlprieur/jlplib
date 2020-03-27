/*************************************************************************
* JLP_wxGseg_Canvas Class (for curves)
*
* JLP
* Version 28/01/2017
**************************************************************************/
#include "jlp_wx_gscanvas.h"
#include "jlp_gsegraf.h"            // JLP_GSEG_InitializePlot

/*************************************************************
* Initialize drawing setup with a parameter file 
*
*************************************************************/
int JLP_wxGseg_Canvas::InitializePlotWithParameterFile(char *param_filename0)
{
char *save_filename0;
int close_flag0;
int x0, y0, dev_width0, dev_height0;

if(jlp_gseg_wxwid0 == NULL) return(-1);
jlp_gseg_wxwid0->GSEG_GetWindowLimits(&x0, &dev_width0, &y0, &dev_height0);

if(jlp_gsegraf0 != NULL) delete jlp_gsegraf0;
jlp_gsegraf0 = new JLP_Gsegraf(jlp_gseg_wxwid0, param_filename0, 
                               &save_filename0, &close_flag0);
ncurves1 = 1;

// Forces screen updating:
 RedrawToBackupDC();

return(0);
}
/*************************************************************
* Open Postscript backup Device Context 
*
*************************************************************/
void JLP_wxGseg_Canvas::CloseBackupPostscriptDC()
{
if(backup_pst_dc1 != NULL) {
  backup_pst_dc1->EndPage();
  backup_pst_dc1->EndDoc();
  delete backup_pst_dc1;
  backup_pst_dc1 = NULL;
// Update the pointer in jlp_gsgeg_wxwid0:
  jlp_gseg_wxwid0->InitPostscriptDC(backup_pst_dc1);
  }
if(backup_printer1 != NULL) {
  delete backup_printer1;
  backup_printer1 = NULL;
  }

return;
}
/*************************************************************
* Open Postscript backup Device Context 
*
*************************************************************/
int JLP_wxGseg_Canvas::OpenBackupPostscriptDC(char *save_filename0)
{
int close_flag0, status = -1;
int x0, y0, dev_width0, dev_height0;

// Close backup DC if present:
CloseBackupPostscriptDC();

// Open first the printer interface
backup_printer1 = new wxPrintData();

backup_printer1->SetPaperId(wxPAPER_A4);
backup_printer1->SetFilename(save_filename0);
backup_printer1->SetPrintMode(wxPRINT_MODE_FILE);
backup_pst_dc1 = new wxPostScriptDC(*backup_printer1);

if(backup_pst_dc1->IsOk() == true) {
// Update the pointer in jlp_gsgeg_wxwid0:
  jlp_gseg_wxwid0->InitPostscriptDC(backup_pst_dc1);
// Tell it where to find the AFM files
// backup_pst_dc1.GetPrintData().SetFontMetricPath(wxT("afm/"));
// Set the resolution in points per inch (the default is 720)
// backup_pst_dc1->SetResolution(1440);

// Path for Metric fonts: no longer possible
// Windows:
//  wxSetAFMPath("d:\\wxw161\\afm\\");
//  wxSetAFMPath("/usr/share/enscript/afm/");
//  backup_pst_dc1->GetPrintData().SetFontMetricpath(wxT("/usr/share/enscript/afm/"));
  backup_pst_dc1->StartDoc(wxT("JLP's program is printing now ..."));
  backup_pst_dc1->StartPage();
//  backup_pst_dc1->SetFont(new wxFontInfo(10).Family(wxFONTFAMILY_ROMAN));

/* DEBUG
  backup_pst_dc1->DrawLine(1, 1, 100, 100);
  backup_pst_dc1->SetBrush(*wxTRANSPARENT_BRUSH);
  backup_pst_dc1->DrawRectangle(200, 200, 1000, 1000);
*/
  status = 0;
  }

return(0);
}
/************************************************************************
* Redraw the curves to the backup postscript dc 
* and save it to a postscript file
************************************************************************/
void JLP_wxGseg_Canvas::SaveBackupPostscriptToFile()
{
wxString save_filename;
char save_filename0[128];

   wxFileDialog dialog(NULL, _T("save to postscript file"), wxEmptyString,
                       wxEmptyString, _T("files (*.ps)|*.ps"),
                       wxFD_SAVE|wxFD_OVERWRITE_PROMPT);
   if (dialog.ShowModal() != wxID_OK) return;
 
   save_filename = dialog.GetPath();

   if ( save_filename.empty() ) return;
   strcpy(save_filename0, (const char*)save_filename.mb_str());

// Open file and backup postscriptdc
  OpenBackupPostscriptDC(save_filename0);

// Draw to postscriptdc :
  RedrawToBackupDC();

// Close file and backup postscriptdc
  CloseBackupPostscriptDC();

return;
}
/************************************************************************
* Output curve as a postscript file
************************************************************************/
void JLP_wxGseg_Canvas::SaveGraphicToPostscriptFile()
{
wxString savefilename;
wxPrintData *my_printer = new wxPrintData();
wxPostScriptDC *pst_dc0;
int width2, height2, pst_x0, pst_y0, pst_width0, pst_height0;

   wxFileDialog dialog(NULL, _T("save to postscript file"), wxEmptyString,
                       wxEmptyString, _T("files (*.ps)|*.ps"),
                       wxFD_SAVE|wxFD_OVERWRITE_PROMPT);
   if (dialog.ShowModal() != wxID_OK) return;

   savefilename = dialog.GetPath();

   if ( savefilename.empty() ) return;

// my_printer->SetPaperId(wxPAPER_A4_rotated);
my_printer->SetPaperId(wxPAPER_A4);
my_printer->SetFilename(savefilename);
my_printer->SetPrintMode(wxPRINT_MODE_FILE);
pst_dc0 = new wxPostScriptDC(*my_printer);

if(pst_dc0->IsOk() == true) {
// tell it where to find the afm files
// pst_dc0.GetPrintData().SetFontMetricPath(wxT("afm/"));
// set the resolution in points per inch (the default is 720)
// pst_dc0->SetResolution(1440);

  pst_dc0->StartDoc(wxT("pri*nting..."));
  pst_dc0->StartPage();
  width2 = backup_dc_bitmap2->GetWidth();
  height2 = backup_dc_bitmap2->GetHeight();

// Draw on the device context:
//pst_dc0->Blit(0, 0, width2, height2, backup_dc, 0, 0, wxCOPY);
  pst_x0 = 200;
  pst_y0 = 200;
  pst_width0 = 5000;
  pst_height0 = 5000;
  pst_dc0->StretchBlit(pst_x0, pst_y0, pst_width0, pst_height0,
                        backup_dc, 0, 0, width2, height2, wxCOPY,
                        false, 0, 0);
  pst_dc0->EndPage();
  pst_dc0->EndDoc();
 }

return;
}
/************************************************************************
* Interactive change of axis limits 
************************************************************************/
void JLP_wxGseg_Canvas::ViewChangeAxisLimits()
{
wxString s_values, s_question, result;
double w1, w2, w3, w4; 
double xmin, xmax, ymin, ymax, zmin, zmax;
int status, dcflag;
char old_values_string[128], prompt_string[512];
char new_values_string[128];
char error_message[128];

if(initialized != 1234) return;

// Create the string that will be displayed for prompting new axes limits:
jlp_gsegraf0->GSEG_QuestionForChangingAxesLimits(prompt_string, 
                                                    old_values_string);

s_question = wxString(prompt_string);

s_values = wxString(old_values_string);

/* Open dialog box */
result = wxGetTextFromUser(s_question, _T("new thresholds:"), s_values, NULL);

if (result.IsEmpty()) return;

strcpy(new_values_string, result.char_str());

// Decode the input string to get the values of the axes limits
 status = jlp_gsegraf0->GSEG_ChangeAxesLimits(new_values_string, 
                                                 &xmin, &xmax, &ymin, &ymax, 
                                                 &zmin, &zmax, error_message);
 if (status != 0) {
  JLP_ErrorDialog(error_message);
  return;
  }

// Redraw plot with the new values of the axes limits 
// dcflag = display coordinates flag
 jlp_gsegraf0->GSEG_ReDrawPlot_Scaled(xmin, xmax, ymin, ymax, zmin, zmax,
                                         &dcflag);

 Refresh();

return;
}
/************************************************************************
* Interactive change of projection angles for 3d plot
************************************************************************/
void JLP_wxGseg_Canvas::ViewAxisRotate()
{
wxString s_values, s_question, result;
double  phi, theta;
int status;
char old_values_string[128], prompt_string[512];
char new_values_string[128];
char error_message[128];

if(initialized != 1234) return;

// Create the string that will be displayed for prompting new axes limits:
jlp_gsegraf0->GSEG_QuestionForChangingAxesRotation(prompt_string,
                                                    old_values_string);

s_question = wxString(prompt_string);

s_values = wxString(old_values_string);

/* Open dialog box */
result = wxGetTextFromUser(s_question, _T("new projection angle values:"), 
                           s_values, NULL);

if (result.IsEmpty()) return;

strcpy(new_values_string, result.char_str());

/* get view-direction data */
if ( sscanf(new_values_string, "%lf %lf", &phi, &theta) != 2 ) {
  strcpy(error_message, "incorrect number of values;\ntwo expected.");
  JLP_ErrorDialog(error_message);
  return;
  }

if ( theta >= 0.0 && theta <= 90.0 ) {
// redraw plot with the new values of the axes limits
// dcflag = display coordinates flag
  jlp_gsegraf0->GSEG_ReDrawPlot3D_Rotated(phi, theta);
  } else {
  strcpy(error_message, "Elevation angle out of range.");
  JLP_ErrorDialog(error_message);
  return;
  }

 Refresh();

return;
}
/************************************************************************
* Interactive contour labelling 
* Set processing mode to ContourLabelling
************************************************************************/
void JLP_wxGseg_Canvas::Set_ViewLabelContours()
{
/* InteractiveProcessingMode :
* -1=None 0=BoxLimits 1=Statistics, 2=Astrometry 3=Photometry
* 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
* 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
* 13=Add a ring 14=Remove a shape
*/

// ProcessingMode=4 
    cgdev_settings1.InternalProcessingMode = 4;
// Apply settings and update popup menu:
    UpdatePopupMenu_InternalProcessingMode();

// Flag for labelling the contours
  flag_contour_labelling = 1;

RedrawToBackupDC();

return;
}
/************************************************************************
* Interactive contour labelling 
* Called when processing mode is set to ContourLabelling
*
* INPUT:
* x1_0, y1_0 : (user coord) location of the center of the label 
************************************************************************/
int JLP_wxGseg_Canvas::Callback_ViewLabelContours(const double x1_0, 
                                                  const double y1_0,
                                                  char *label_0)

{
int status, ncontour_plots0, icontour_plots;
int xmouse1, ymouse1, in_frame;
double z_data, nearest_contour_value;

// Check if OK:
 if(jlp_gsegraf0 == NULL) return(-1);
 jlp_gsegraf0->FromUserToDeviceCoord(x1_0, y1_0, &xmouse1, &ymouse1, &in_frame);

// Flag for labelling the contours
 if(flag_contour_labelling != 1) return(-1); 

 jlp_gsegraf0->GSEG_get_ncontour_plots(&ncontour_plots0);

/* Check icontour_plots value 
 icontour_plots = icontour_plots + inc;
 if ( icontour_plots < 1 )
    icontour_plots = 1;
 else if ( icontour_plots > ncontour_plots0)
    icontour_plots = ncontour_plots0;
*/

// DEBUG:
 icontour_plots = 1;

//  z_data : z data in user coordinates
//  nearest_contour_value : value of the nearest contour
 status = jlp_gsegraf0->GSEG_CreateContourLabel(xmouse1, ymouse1, 
                                                icontour_plots, &z_data, 
                                                &nearest_contour_value);

 if(status == 0) {
// Copy to the label string used by "jlp_wx_gscanvas_process.cpp"
  sprintf(label_0, "%.3g", nearest_contour_value);
  } else {
  strcpy(label_0, "");
  }

/* DEBUG:
printf("Callback_ViewLabelContours: ncontours=%d x=%d y=%d status=%d z_data=%f label=>%s<\n", 
          ncontour_plots0, ix_mouse, iy_mouse, status, z_data, label_0);
*/

return(0);
}
/************************************************************************
* Set processing mode to ZoomIn 
************************************************************************/
void JLP_wxGseg_Canvas::Set_ZoomIn()
{
/* InteractiveProcessingMode :
* -1=None 0=BoxLimits 1=Statistics, 2=Astrometry 3=Photometry
* 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
* 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
* 13=Add a ring 14=Remove a shape
*/

// ProcessingMode=0
    cgdev_settings1.InternalProcessingMode = 0;
// Apply settings and update popup menu:
    UpdatePopupMenu_InternalProcessingMode();

RedrawToBackupDC();

return;
}
/************************************************************************
* Interactive contour labelling 
* Called when processing mode is set to ZoomIn 
*
* INPUT:
* x1_0, y1_0 : (user coord) lower left corner of selection rectangle 
* x2_0, y2_0 : (user coord) upper right corner of selection rectangle 
************************************************************************/
int JLP_wxGseg_Canvas::Callback_ZoomIn(const double x1_0, 
                                       const double y1_0,
                                       const double x2_0,
                                       const double y2_0)
{
int xmouse1, ymouse1, xmouse2, ymouse2, in_frame;

 jlp_gsegraf0->FromUserToDeviceCoord(x1_0, y1_0, &xmouse1, &ymouse1, &in_frame);
 jlp_gsegraf0->FromUserToDeviceCoord(x2_0, y2_0, &xmouse2, &ymouse2, &in_frame);

// If empty box, redraw the plot with initial size
if((x1_0 == x2_0) || (y1_0 == y2_0))
  {
// Draw graph 
   jlp_gsegraf0->GSEG_GetAxisLabelPixbufs();
   jlp_gsegraf0->GSEG_DrawGraph();
// Update extra labels if any
   jlp_gsegraf0->GSEG_DrawExtraLabels();
// ProcessingMode=-1
    cgdev_settings1.InternalProcessingMode = -1;
// Apply settings and update popup menu:
    UpdatePopupMenu_InternalProcessingMode();
  } else {
// Redraw plot 
   jlp_gsegraf0->GSEG_ZoomIn(xmouse1, ymouse1, xmouse2, ymouse2);
  }

return(0);
}
/************************************************************************
* Conversion from device to user coordinates 
************************************************************************/
int JLP_wxGseg_Canvas::FromDeviceToUserCoord(const double x1, const double y1, 
                                             double *dev_x1, double *dev_y1,
                                             int *in_frame)
{
int status;
 status = jlp_gsegraf0->FromDeviceToUserCoord(x1, y1, dev_x1, dev_y1, in_frame);

return(status);
}
