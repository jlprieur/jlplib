/****************************************************************************
* Name: jlp_wx_gspanel.cpp
* 
* JLP
* Version 06/11/2016
***************************************************************************/
#include "jlp_wxlogbook.h"         // JLP_wxLogbook

#include "jlp_wx_gspanel.h"        // jlp_wx_gspanel files:
#include "jlp_wx_gscanvas.h"       // jlp_wxgseg_Canvas class
#include "jlp_gsegraf.h"           // JLP_Gsegraf class

/*************************************************************************
* Select and load input parameter file 
*************************************************************************/
int JLP_wxGsegPanel::SelectAndLoadInputParamFile(wxString &input_filename)
{
int status;
wxString err_msg;
char param_filename1[128];

err_msg = _T("");
input_filename = wxFileSelector(_T("Select ASCII parameter file"), 
                                _T(""), _T(""), _T("txt|dat"));

if(input_filename.IsEmpty()) {
 err_msg = _T("No file selected: please have another try");
 return(-1);
 }

strncpy(param_filename1, input_filename.mb_str(), 128);

/* Initialize plot */
 status = jlp_wxgseg_canvas1->InitializePlotWithParameterFile(param_filename1);

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
* Select and load input data file 
*************************************************************************/
int JLP_wxGsegPanel::SelectAndLoadInputDataFile(wxString &input_filename)
{
int status, reset_first;
wxString err_msg;
char filename1[128];

err_msg = _T("");
input_filename = wxFileSelector(_T("Select ASCII file"), _T(""), _T(""),
                                _T("txt|dat"));

if(input_filename.IsEmpty()) {
 err_msg = _T("No file selected: please have another try");
 return(-1);
 }

strncpy(filename1, input_filename.mb_str(), 80);

// reset_first: used to erase previous curves and start from scratch:
reset_first = 1;
status = jlp_wxgseg_canvas1->LoadPlotDataFromFile(filename1, reset_first);
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
/************************************************************************
* Output curve as a JPEG, PNG, etc. file
************************************************************************/
void JLP_wxGsegPanel::SaveGraphicToFile()
{

if(jlp_wxgseg_canvas1 != NULL) jlp_wxgseg_canvas1->SaveGraphicToFile();

return;
}
/************************************************************************
* Output curve as a postscript file
************************************************************************/
void JLP_wxGsegPanel::SaveGraphicToPostscriptFile()
{

if(jlp_wxgseg_canvas1 != NULL) jlp_wxgseg_canvas1->SaveGraphicToPostscriptFile();

return;
}
/************************************************************************
* Output backup postscript curve as a postscript file
************************************************************************/
void JLP_wxGsegPanel::SaveBackupPostscriptToFile()
{

if(jlp_wxgseg_canvas1 != NULL) jlp_wxgseg_canvas1->SaveBackupPostscriptToFile();

return;
}
/************************************************************************
* Interactive change of axis limits 
************************************************************************/
void JLP_wxGsegPanel::ViewChangeAxisLimits()
{

if(jlp_wxgseg_canvas1 != NULL) jlp_wxgseg_canvas1->ViewChangeAxisLimits();

return;
}
/************************************************************************
* Interactive change of projection angles for 3D plots
************************************************************************/
void JLP_wxGsegPanel::ViewAxisRotate()
{

if(jlp_wxgseg_canvas1 != NULL) jlp_wxgseg_canvas1->ViewAxisRotate();

return;
}
/************************************************************************
* Interactive contour labelling
************************************************************************/
void JLP_wxGsegPanel::Set_ViewLabelContours()
{

if(jlp_wxgseg_canvas1 != NULL) jlp_wxgseg_canvas1->Set_ViewLabelContours();

return;
}
