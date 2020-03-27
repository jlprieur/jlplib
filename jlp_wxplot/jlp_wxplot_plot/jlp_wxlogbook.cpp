/******************************************************************************
* Name:        jlp_wxlogbook.cpp (GpdLogbook class)
* Purpose:     Logbook utilities 
* Author:      JLP 
* Version:     03/01/2015 
******************************************************************************/
#include "jlp_wxlogbook.h"
#include "jlp_wxspeckle1_dlg.h"  // JLP_wxSpeckle1_Dlg

/*
    JLP_wxLogbook(wxWindow *window, const wxString title, const int iwidth, 
               const int iheight);
    ~JLP_wxLogbook();
int  SaveLogbook(wxString save_filename)
void Clear()
void Clean()
int  WriteToLogbook(wxString str1, bool SaveToFile);
int BinariesSaveMeasurements(wxString m_full_filename1);
*/

BEGIN_EVENT_TABLE(JLP_wxLogbook, wxTextCtrl)
END_EVENT_TABLE()

/**********************************************************************
* JLP_wxLogbook constructor
*
* INPUT:
*   xx,yy : size of created window
*
***********************************************************************/
JLP_wxLogbook::JLP_wxLogbook(wxWindow *window, const wxString title, const int xx, const int yy)
       : wxTextCtrl(window, wxID_ANY, title, wxPoint(-1, -1), wxSize(xx, yy),
                         wxTE_MULTILINE | wxTE_READONLY /* | wxTE_RICH */)
{
return;
}
/**********************************************************************
* JLP_wxLogbook destructor
*
* INPUT:
*   xx,yy : size of created window
*
***********************************************************************/
JLP_wxLogbook::~JLP_wxLogbook()
{
return;
}
/************************************************************************
* Save useful content of logbook to file 
* Input:
* save_filename: wxString whose value is set in gdp_frame_menu.cpp
************************************************************************/
int JLP_wxLogbook::SaveLogbook(wxString save_filename)
{
int status = 0;
wxFile *m_log_file;

 m_log_file = new wxFile();
// Overwrite = true:
 m_log_file->Create(save_filename, true);
 if(!m_log_file->IsOpened()) {
   wxLogError(_T("Error opening log file!\n"));
   status = -1;
 } else {
   m_log_file->Write(m_LogString);
   m_log_file->Close();
   delete m_log_file;
 }

return(status);
}
/*******************************************************************
* Clear the logbook: erase all its content
********************************************************************/
void JLP_wxLogbook::Clear()
{
 m_LogString.Clear();
 SetValue(m_LogString);

return;
}
/*******************************************************************
* Clean the logbook: only keep its useful content
********************************************************************/
void JLP_wxLogbook::Clean()
{
wxString str1;

// First erase screen: 
  str1 = wxString("");
  SetValue(str1);

// Then display usefull content:
  SetValue(m_LogString);
}
/*******************************************************************
* Write a string to the logbook
********************************************************************/
int JLP_wxLogbook::WriteToLogbook(wxString str1, bool SaveToFile) 
{
int status = 0;
 if(SaveToFile) m_LogString.Append(str1);
 *this << str1; 
return(status);
}
/**************************************************************************
* Binaries: process and saving measurements to Latex file 
*
* INPUT:
*  original_fits_fname: FITS file obtained from the observations
*                        (full name with directory and extension)
*  processed_fits_fname: processed FITS file used for the measurements
*                        (name without directory and extension)
*
**************************************************************************/
int JLP_wxLogbook::BinariesSaveMeasurements(wxString original_fits_fname,
                                            wxString processed_fits_fname)
{
double font_width;
int status = -1;

// One possibility is:
//  iwFont = wxNORMAL_FONT->GetPointSize();
//
wxFont m_Font = wxSystemSettings::GetFont(wxSYS_DEFAULT_GUI_FONT);
font_width = m_Font.GetPointSize();

// Smaller width if font width is not a fixed:
if(!m_Font.IsFixedWidth()) font_width *= 0.8;
wxSize size1 = wxSize((int)(80. * font_width), (int)(32. * font_width));

// Speckle, DVA mode or Lucky imaging: 
// process and save measurements to Latex file 
// Read the header of the FITS file to fill the result latex line 
 if( (new JLP_wxSpeckle1_Dlg((wxFrame *)this, m_LogString, original_fits_fname,
                             processed_fits_fname, 
                             _T("Processing and saving measurements"), 
                             size1))->ShowModal() == wxID_OK) status = 0;

return(status);
}
