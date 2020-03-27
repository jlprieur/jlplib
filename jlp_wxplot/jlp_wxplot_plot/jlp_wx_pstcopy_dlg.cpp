/******************************************************************************
* jlp_wx_pstcopy_dlg.cpp
* Dialog box used for selecting pstcop parameters 
*
* Author:      JLP 
* Version:     28/01/2013
******************************************************************************/
#include "jlp_wx_pstcopy_dlg.h"

enum
{
   ID_COPY_TEX_FONTS    = 800,
   ID_COPY_BLACK_AND_WHITE,
   ID_COPY_HIGH_RES,
   ID_COPY_LUT_SCALE,
   ID_COPY_OK,
   ID_COPY_CANCEL
};

BEGIN_EVENT_TABLE(JLP_PstCopy_Dlg, wxDialog)
EVT_CHECKBOX (ID_COPY_TEX_FONTS, JLP_PstCopy_Dlg::OnChangeParam)
EVT_CHECKBOX (ID_COPY_BLACK_AND_WHITE, JLP_PstCopy_Dlg::OnChangeParam)
EVT_CHECKBOX (ID_COPY_HIGH_RES, JLP_PstCopy_Dlg::OnChangeParam)
EVT_CHECKBOX (ID_COPY_LUT_SCALE, JLP_PstCopy_Dlg::OnChangeParam)
EVT_BUTTON  (ID_COPY_OK, JLP_PstCopy_Dlg::OnOKButton)
EVT_BUTTON  (ID_COPY_CANCEL, JLP_PstCopy_Dlg::OnCancelButton)
END_EVENT_TABLE()

// Constructor:
JLP_PstCopy_Dlg::JLP_PstCopy_Dlg(wxFrame *parent, const wxString &title) 
        : wxDialog(parent, -1, title, wxPoint(400,100), wxDefaultSize,
                   wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{

// Default values: 
  TeX_fonts1 = false;
  black_and_white1 = false;
  high_resolution1 = true;
  lut_scale1 = false;

// To avoid initialization problems with Windows:
// (An event is sent to "ChangeParameters"
//  as soon as a Text control is created...)
// First initialize the pointers to NULL
  TexFonts_CheckBox = NULL;
  BlackWhite_CheckBox = NULL;
  HighRes_CheckBox = NULL;
  LutScale_CheckBox = NULL;

  MySetupMenu();
return;
}
/*************************************************************************
*
************************************************************************/
int JLP_PstCopy_Dlg::MySetupMenu() 
{
wxBoxSizer *topsizer = new wxBoxSizer( wxVERTICAL );

// First row:
wxBoxSizer *hsizer1 = new wxBoxSizer( wxHORIZONTAL);

 TexFonts_CheckBox = new wxCheckBox(this, ID_COPY_TEX_FONTS, wxT("Tex fonts"));
 hsizer1->Add(TexFonts_CheckBox, 0, wxALIGN_LEFT, 10); 
 BlackWhite_CheckBox = new wxCheckBox(this, ID_COPY_BLACK_AND_WHITE, 
                                      wxT("Black and white"));
 hsizer1->Add(BlackWhite_CheckBox, 0, wxALIGN_LEFT, 10); 

 topsizer->Add(hsizer1, 0, wxALIGN_CENTER|wxALL, 10);

// Second row:
wxBoxSizer *hsizer2 = new wxBoxSizer( wxHORIZONTAL);

 HighRes_CheckBox = new wxCheckBox(this, ID_COPY_HIGH_RES, 
                                   wxT("High resolution"));
 hsizer2->Add(HighRes_CheckBox, 0, wxALIGN_LEFT, 10); 
 LutScale_CheckBox = new wxCheckBox(this, ID_COPY_LUT_SCALE,
                                   wxT("LUT scale"));
 hsizer2->Add(LutScale_CheckBox, 0, wxALIGN_LEFT, 10); 
 topsizer->Add(hsizer2, 0, wxALIGN_CENTER|wxALL, 10);

// Third row: 
wxBoxSizer *button_sizer = new wxBoxSizer( wxHORIZONTAL );

//create two buttons that are horizontally unstretchable, 
  // with an all-around border with a width of 10 and implicit top alignment
 button_sizer->Add(
    new wxButton(this, ID_COPY_OK, _T("OK") ), 0, wxALL, 10);

 button_sizer->Add(
   new wxButton(this, ID_COPY_CANCEL, _T("Cancel") ), 0, wxALIGN_CENTER|wxALL, 10);

  //create a sizer with no border and centered horizontally
  topsizer->Add(button_sizer, 0, wxALIGN_CENTER);

  SetSizer(topsizer);      // use the sizer for layout

  topsizer->SetSizeHints( this );   // set size hints to honour minimum size

// Set current values:
 TexFonts_CheckBox->SetValue(TeX_fonts1); 
 BlackWhite_CheckBox->SetValue(black_and_white1);
 HighRes_CheckBox->SetValue(high_resolution1);
 LutScale_CheckBox->SetValue(lut_scale1);

return(0);
}
/**************************************************************************
* Handle "OK" button:
**************************************************************************/
void JLP_PstCopy_Dlg::OnOKButton( wxCommandEvent& WXUNUSED(event) )
{
// Close dialog and return status = 0:
  EndModal(0); 
}
/**************************************************************************
* Handle "Cancel" button:
**************************************************************************/
void JLP_PstCopy_Dlg::OnCancelButton( wxCommandEvent& WXUNUSED(event) )
{
// Close dialog and return status = 1:
  EndModal(1); 
}
/**************************************************************************
* Handle check box changes 
*
**************************************************************************/
void JLP_PstCopy_Dlg::OnChangeParam( wxCommandEvent& WXUNUSED(event) )
{
// First check that all check boxes are created:
 if(!TexFonts_CheckBox || !BlackWhite_CheckBox
    || !HighRes_CheckBox || !LutScale_CheckBox) return;     

 black_and_white1 = BlackWhite_CheckBox->GetValue(); 
 high_resolution1 = HighRes_CheckBox->GetValue(); 
 lut_scale1 = LutScale_CheckBox->GetValue(); 
 TeX_fonts1 = TexFonts_CheckBox->GetValue(); 

return;
}
