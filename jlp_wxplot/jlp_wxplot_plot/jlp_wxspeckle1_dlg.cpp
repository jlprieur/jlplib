/******************************************************************************
* jlp_wxspeckle1_dlg.cpp
* To process measurements and save them to Latex file 
*
* Author:   JLP 
* Version:  08/07/2012
******************************************************************************/
#include "jlp_wxspeckle1_dlg.h"
#include "jlp_wxspeckle1_dlg_utils.h"

//*************************************************************************
enum
{
   ID_SPECKLE1_OK_PROCESS_OLD = 820,
   ID_SPECKLE1_OK_PROCESS_NEW = 821,
   ID_SPECKLE1_CANCEL     = 822
};

BEGIN_EVENT_TABLE(JLP_wxSpeckle1_Dlg, wxDialog)
EVT_BUTTON  (ID_SPECKLE1_OK_PROCESS_OLD, JLP_wxSpeckle1_Dlg::OnOKProcessButton)
EVT_BUTTON  (ID_SPECKLE1_OK_PROCESS_NEW, JLP_wxSpeckle1_Dlg::OnOKProcessButton)
EVT_BUTTON  (ID_SPECKLE1_CANCEL, JLP_wxSpeckle1_Dlg::OnCancelButton)
END_EVENT_TABLE()

/**************************************************************************
* Constructor:
*
* INPUT:
*  original_fits_fname: FITS file obtained from the observations
*                        (full name with directory and extension)
*  processed_fits_fname: processed FITS file used for the measurements
*                        (name without directory and extension)
*
**************************************************************************/
JLP_wxSpeckle1_Dlg::JLP_wxSpeckle1_Dlg(wxFrame *parent, const wxString logbook, 
                                   const wxString original_fits_fname, 
                                   const wxString processed_fits_fname, 
                                   const wxString &title, const wxSize size1) 
        : wxDialog(parent, -1, title, wxPoint(100,100), size1 + wxSize(0,100),
                   wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
original_FITS_fname = original_fits_fname;
processed_FITS_fname = processed_fits_fname;

// Initialize processing status flag: 
   not_processed = true;

// Create the text control: 
   m_Text = new wxTextCtrl(this, wxID_ANY, _T(""), wxDefaultPosition, size1, 
                           wxTE_MULTILINE ); // | wxTE_READONLY );

   wxBoxSizer *topsizer = new wxBoxSizer( wxVERTICAL );

   topsizer->Add(m_Text, 1, wxEXPAND);

   wxBoxSizer *button_sizer = new wxBoxSizer( wxHORIZONTAL );

//create two buttons that are horizontally unstretchable, 
// with an all-around border with a width of 10 and implicit top alignment
 m_OKProcessNewButton = new wxButton(this, ID_SPECKLE1_OK_PROCESS_NEW, 
                                     _T("Process and save to new file") ); 
 m_OKProcessOldButton = new wxButton(this, ID_SPECKLE1_OK_PROCESS_OLD, 
                                     _T("Process and append to old file") ); 

 button_sizer->Add( m_OKProcessNewButton, 0, wxALL, 10);
 button_sizer->Add( m_OKProcessOldButton, 0, wxALL, 10);

 m_CancelButton = new wxButton(this, ID_SPECKLE1_CANCEL, _T("Cancel") ); 
 button_sizer->Add( m_CancelButton, 0, wxALL, 10);


  //create a sizer with no border and centered horizontally
  topsizer->Add(button_sizer, 0, wxALIGN_CENTER);

  SetSizer(topsizer);      // use the sizer for layout

//  topsizer->SetSizeHints( this );   // set size hints to honour minimum size

// Display the content of the logfile:
  m_Text->SetValue(logbook);

return;
}
/**************************************************************************
* Handle "OK"/"Process" button:
* This routine is called twice: once for selecting old/new file,
* and a second time for saving confirmation when processing has been done)
**************************************************************************/
void JLP_wxSpeckle1_Dlg::OnOKProcessButton( wxCommandEvent& event )
{
int status, new_latex_file = 1;
wxString data_fname = wxT("tmp0001.txt");
wxString err_message;
wxFile *data_file, *latex_file;
wxString error_message;

switch (event.GetId()) {
  case ID_SPECKLE1_OK_PROCESS_NEW:
    new_latex_file = 1;
    break;
  case ID_SPECKLE1_OK_PROCESS_OLD:
    new_latex_file = 0;
    break;
  }

//*********************************************************
if(not_processed) {

// Save current text to dummy temporary file 
// (It may be different from Logfile since it may have been modified by the user)
 data_file = new wxFile();
// Overwrite = true:
 data_file->Create(data_fname, true);
 if(!data_file->IsOpened()) {
   err_message = _T("OnOKProcessButton/Error opening dummy temporary data file >")
                 + data_fname + _T("<!\n");
   return;
 } else {
   data_file->Write(m_Text->GetValue());
   data_file->Close();
   delete data_file;
 }

// Prompt for the name of a new Latex file:
  if(new_latex_file) {
  m_Latex_Filename = wxFileSelector(_T("New Latex file"), _T(""), _T(""),
                                wxT("tex"), wxT("Latex files (*.tex)|*.tex"), 
                                wxFD_SAVE);
   if ( m_Latex_Filename.empty() ) return;

   latex_file = new wxFile();
// Overwrite = false:
   latex_file->Create(m_Latex_Filename, false);
   if(!latex_file->IsOpened()) { 
     err_message = _T("OnOKProcessButton/Error opening new LaTeX file >")
                   + m_Latex_Filename + _T("<!\n");
     wxLogError(err_message);
     return;
     }
   latex_file->Write(_T("%%\n"));
   latex_file->Close();
   delete latex_file;
// Prompt for the name of an existing Latex file:
  } else { 
  m_Latex_Filename = wxFileSelector(_T("Select Latex file"), _T(""), _T(""),
                                 _T("tex"), _T("Latex Files (*.tex)|*.tex"));
  if (m_Latex_Filename.empty()) return;
  }

/* Process the temporary data file and load it to LaTeX file
in "jlp_wxspeckle1_dlg_utils.cpp":
int speckle_process_data(const wxString data_fname,
                         const wxString LatexFilename,
                         const wxString original_FITS_fname,
                         const wxString processed_FITS_fname,
                         wxString &error_message);
*/

  status = speckle_process_data(data_fname, m_Latex_Filename, 
                                original_FITS_fname, processed_FITS_fname,
                                error_message);
  if(status){
    err_message.Printf(_T("OnOKProcessButton/speckle_process_data/Error status = %d\n"),
                       status);
    wxLogError(err_message + error_message);
    return;
   }
// If OK, display the modified Latex file:
  m_Text->LoadFile(m_Latex_Filename);

// Display a new label "OK/Process" ont the previous "save to old file" button:
  m_OKProcessOldButton->SetLabel(_T("OK"));
// Destroy previous "save to new file" button:
  m_OKProcessNewButton->Destroy();
  not_processed = false;

//*********************************************************
// End of processing: saving the new version of the Latex file
// (i.e., second call of this routine, but this time processing has been done)
  } else {

// Save current text to file
  if(m_Text->IsModified()){
   if( wxMessageBox(_T("You have modified the Latex File\n\
Do you want to save those changes?"), _T("Please confirm"),
                    wxICON_QUESTION | wxYES_NO) == wxYES ) {
     m_Text->SaveFile(m_Latex_Filename);
    }
  }

// Close dialog and return status = wxID_OK:
  EndModal(wxID_OK); 
  }
return;
}
/**************************************************************************
* Handle "Cancel" button:
**************************************************************************/
void JLP_wxSpeckle1_Dlg::OnCancelButton( wxCommandEvent& WXUNUSED(event) )
{
// Close dialog and return status = 1:
  EndModal(1); 
}
