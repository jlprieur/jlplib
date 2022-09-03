/****************************************************************************
* Name: jlp_gseg_param_panel_onclick.cpp
* JLP_Gseg_ParamPanel class
* 
* JLP
* Version 06/06/2017
***************************************************************************/
#include "jlp_gseg_param_panel.h" // JLP_Gseg_ParamPanel class 
#include "jlp_gseg_param_panel_id.h" 

#include "jlp_wxlogbook.h"          // JLP_wxLogbook
#include "jlp_gsegraf.h"            // JLP_Gsegraf class
#include "jlp_gsegset_rw_ascii.h"   // WriteParamToKeywordFile
#include "jlp_gsegset_params.h"     // Init_GSEG_SETTINGS_with_default_values
#include "jlp_gseg_weight_dlg.h"    // JLP_GsegWeight_Dlg class

BEGIN_EVENT_TABLE(JLP_Gseg_ParamPanel, wxPanel)
  EVT_COMBOBOX(ID_PARAM_CMB_WEIGHT, JLP_Gseg_ParamPanel::OnSelectWeightBlk)
  EVT_BUTTON(ID_PARAM_WEIGHT_BUTTON, JLP_Gseg_ParamPanel::OnFillWeightBlk)
  EVT_BUTTON(ID_PARAM_VALID,      JLP_Gseg_ParamPanel::OnParamValid)
  EVT_BUTTON(ID_PARAM_LOAD1,      JLP_Gseg_ParamPanel::OnParamLoad)
  EVT_BUTTON(ID_PARAM_LOAD2,      JLP_Gseg_ParamPanel::OnParamLoad)
  EVT_BUTTON(ID_PARAM_SAVE1,      JLP_Gseg_ParamPanel::OnParamSave)
  EVT_BUTTON(ID_PARAM_SAVE2,      JLP_Gseg_ParamPanel::OnParamSave)
  EVT_BUTTON(ID_PARAM_DEFAULT,    JLP_Gseg_ParamPanel::OnParamDefault)
END_EVENT_TABLE()

/**************************************************************************
* Handle "ParamValid" button:
**************************************************************************/
void JLP_Gseg_ParamPanel::OnParamValid( wxCommandEvent& WXUNUSED(event) )
{

// Read the current values of the fit parameters:
  ParamPanel_GetValues(GsegSet1);

}
/**************************************************************************
* Handle "ParamSave" button:
**************************************************************************/
void JLP_Gseg_ParamPanel::OnParamSave( wxCommandEvent& event )
{

// Read the current values of the fit parameters:
  ParamPanel_GetValues(GsegSet1);

// Save parameter file:
  if(event.GetId() == ID_PARAM_SAVE1)
    SaveParamToKwdFile();
  else
    SaveParamToGsegrafFile();

}
/**************************************************************************
* Handle "ParamLoad" button:
**************************************************************************/
void JLP_Gseg_ParamPanel::OnParamLoad( wxCommandEvent& event)
{

// Load parameter file:
if(event.GetId() == ID_PARAM_LOAD1)
  LoadKeywordParamFile(GsegSet1);
else
  LoadGsegrafParamFile(GsegSet1);

// Set those values to the fit parameters:
  ParamPanel_SetValues(GsegSet1);

}
/**************************************************************************
* Handle "ParamDefault" button:
**************************************************************************/
void JLP_Gseg_ParamPanel::OnParamDefault( wxCommandEvent& WXUNUSED(event) )
{

 Init_GSEG_SETTINGS_with_default_values(GsegSet1);

// Set those values to the fit parameters:
  ParamPanel_SetValues(GsegSet1);

}
/**************************************************************************
* Handle "n_weightblk" combobox:
**************************************************************************/
void JLP_Gseg_ParamPanel::OnSelectWeightBlk(wxCommandEvent& event)
{

GsegSet1.n_weightblk = ParCmb_n_weightblk.combo->GetSelection(); 

// Open popup window for filling weight parameters
if(GsegSet1.n_weightblk != 0) OnFillWeightBlk(event);

}
/**************************************************************************
* Handle "n_weightblk" combobox:
**************************************************************************/
void JLP_Gseg_ParamPanel::OnFillWeightBlk(wxCommandEvent& WXUNUSED(event))
{
JLP_GsegWeight_Dlg *WeightDlg;
int i, status;
int Int1[NBLK_MAX], Int2[NBLK_MAX];
double Weight[NBLK_MAX];

// Open popup window for filling weight parameters
if(GsegSet1.n_weightblk != 0) {

 WeightDlg = new JLP_GsegWeight_Dlg(GsegSet1);

 status = WeightDlg->ShowModal();

// Copy To Pset1 structure if status is OK:
 if(status == 0) {

// Retrieve the weight parameters:
   WeightDlg->RetrieveData(Int1, Int2, Weight);

   for(i = 0; i < GsegSet1.n_weightblk; i++) {
     GsegSet1.int1_blk[i] = Int1[i];
     GsegSet1.int2_blk[i] = Int2[i];
     GsegSet1.weight_blk[i] = Weight[i];
   }
 }

delete WeightDlg;
}

return;
}
/************************************************************************
** Output ascii parameter keyword file
*************************************************************************/
void JLP_Gseg_ParamPanel::SaveParamToKwdFile()
{
wxString filename;
char param_filename[128];

if(initialized != 1234) return;

   wxFileDialog dialog(NULL, _T("Save parameters to ASCII file with keywords"),
                       wxEmptyString, wxEmptyString, _T("Files (*.txt)|*.txt"),
                       wxFD_SAVE|wxFD_OVERWRITE_PROMPT);
   if (dialog.ShowModal() == wxID_OK) {
       filename = dialog.GetPath();
       strcpy(param_filename,filename.c_str());
       WriteParamToKeywordFile(param_filename, GsegSet1);
   }

}
/************************************************************************
** Output ascii parameters to gsegraf-formatted file
*************************************************************************/
void JLP_Gseg_ParamPanel::SaveParamToGsegrafFile()
{
wxString filename;
char param_filename[128];

if(initialized != 1234) return;

  wxFileDialog dialog(NULL,
                      _T("Save parameters to ASCII file (gsegraf format)"),
                       wxEmptyString, wxEmptyString, _T("Files (*.txt)|*.txt"),
                       wxFD_SAVE|wxFD_OVERWRITE_PROMPT);
   if (dialog.ShowModal() == wxID_OK) {
       filename = dialog.GetPath();
       strcpy(param_filename,filename.c_str());
       WriteParamToGsegrafFile(param_filename, GsegSet1);
   }

}
/************************************************************************
** Input gsegraf formatted parameter file
*************************************************************************/
int JLP_Gseg_ParamPanel::LoadGsegrafParamFile(GSEG_SETTINGS &GsegSet0)
{
wxString full_filename, str1, path, extension, fname;
char filename1[128], comments1[80];
wxString m_filename0, buffer, err_msg1;
int status;

full_filename = wxFileSelector( wxT("Select parameter file (gsegraf format)"),
                                _T(""), _T(""), _T("txt|dat"));

if(full_filename.empty()) {
 err_msg1 = _T("No file selected: please have another try");
 return(-1);
 }

  strncpy(filename1, full_filename.mb_str(), 128);
  status = ReadParamFromGsegrafFile(filename1, GsegSet0);

// Removes the directory name (since the full path is generally too long...)
  wxFileName::SplitPath(full_filename, &path, &fname, &extension);
  m_filename0 = fname + _T(".") + extension;

  if (status) {
    err_msg1 = _T("Couldn't load data from '") + m_filename0 + _T("'.");
    status = -2;
    } else  {
    err_msg1 = m_filename0 + _T(" successfuly loaded");
    }

// Write to logbook window (and record on the logbook file):
if(jlp_logbook != NULL) jlp_logbook->WriteToLogbook(err_msg1, true);

return(status);
}

/************************************************************************
** Input ascii parameter file
*************************************************************************/
int JLP_Gseg_ParamPanel::LoadKeywordParamFile(GSEG_SETTINGS &GsegSet0)
{
wxString full_filename, str1, path, extension, fname;
char filename1[128], comments1[80];
wxString m_filename0, buffer, err_msg1;
int status;

full_filename = wxFileSelector( wxT("Load parameter file (ASCII with keywords)"));

if(full_filename.empty()) {
 err_msg1 = _T("No file selected: please have another try");
 return(-1);
 }

  strncpy(filename1, full_filename.mb_str(), 128);
  status = ReadParamFromKeywordFile(filename1, GsegSet0);

// Removes the directory name (since the full path is generally too long...)
  wxFileName::SplitPath(full_filename, &path, &fname, &extension);
  m_filename0 = fname + _T(".") + extension;

  if (status) {
    err_msg1 = _T("Couldn't load data from '") + m_filename0 + _T("'.");
    status = -2;
    } else  {
    err_msg1 = m_filename0 + _T(" successfuly loaded");
    }

// Write to logbook window (and record on the logbook file):
  if(jlp_logbook != NULL) jlp_logbook->WriteToLogbook(err_msg1, true);

return(status);
}
