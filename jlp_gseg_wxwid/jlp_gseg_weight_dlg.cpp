/******************************************************************************
* jlp_gseg_weight_dlg.cpp
* Dialog box used to select the weights 
*
* Author:      JLP 
* Version:     12/06/2017
******************************************************************************/
#include "jlp_gseg_param_panel_id.h"       // ID_WEIGHT_OK, ...
#include "jlp_gseg_weight_dlg.h"

BEGIN_EVENT_TABLE(JLP_GsegWeight_Dlg, wxDialog)
EVT_BUTTON  (ID_WEIGHT_OK, JLP_GsegWeight_Dlg::OnOKButton)
EVT_BUTTON  (ID_WEIGHT_CANCEL, JLP_GsegWeight_Dlg::OnCancelButton)
END_EVENT_TABLE()


/********************************************************************
* Constructor:
********************************************************************/
JLP_GsegWeight_Dlg::JLP_GsegWeight_Dlg(GSEG_SETTINGS GsegSet0)
        : wxDialog(NULL, -1, wxT("Selection of the weights"), 
                  wxPoint(400,100), wxDefaultSize,
                   wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
// Save to private variable
 n1 = GsegSet0.n_weightblk;

// To avoid initialization problems with Windows:
// (An event is sent to "OnSelectLanguage"
//  as soon as a Text control is created...)
 initialized = 0; 

 SetupPanel();

 initialized = 1234;

 UpdatePanel(GsegSet0);

return;
}
/********************************************************************
* Setup the display: 
********************************************************************/
void JLP_GsegWeight_Dlg::SetupPanel()
{
wxBoxSizer *topsizer;
wxFlexGridSizer *fgs1;
int k, nrows, ncols, vgap = 12, hgap = 12;
wxString str1;

// Flexible grid sizer:
  nrows = n1 + 1;
  ncols = 4;
fgs1 = new wxFlexGridSizer(nrows, ncols, vgap, hgap);

 fgs1->Add(new wxStaticText(this, -1, wxT("Index")));
 fgs1->Add(new wxStaticText(this, -1, wxT("Int1")));
 fgs1->Add(new wxStaticText(this, -1, wxT("Int2")));
 fgs1->Add(new wxStaticText(this, -1, wxT("Weight")));

 for(k = 0; k < n1; k++) {
 str1.Printf(wxT("%d"), k+1);
 fgs1->Add(new wxStaticText(this, -1, str1));
 ParCtrl_int1_blk[k] = new wxTextCtrl(this, -1, wxT("1"),
                                      wxPoint(-1, -1), wxSize(90, 28));
 fgs1->Add(ParCtrl_int1_blk[k]);
 ParCtrl_int2_blk[k] = new wxTextCtrl(this, -1, wxT("1"),
                                      wxPoint(-1, -1), wxSize(90, 28));
 fgs1->Add(ParCtrl_int2_blk[k]);
 ParCtrl_weight_blk[k] = new wxTextCtrl(this, -1, wxT("1.0"),
                                      wxPoint(-1, -1), wxSize(90, 28));
 fgs1->Add(ParCtrl_weight_blk[k]);
 }

  topsizer = new wxBoxSizer( wxVERTICAL );


// Sizer surrounded with a rectangle, with a title on top:
  wxStaticBoxSizer *weight_sizer = new wxStaticBoxSizer(wxVERTICAL, this, 
                                   _T("Selection of weight intervals"));
    
  weight_sizer->Add(fgs1, 0, wxALIGN_CENTER|wxALL, 20);
  topsizer->Add(weight_sizer, 0, wxALIGN_CENTER|wxALL, 20);

wxBoxSizer *button_sizer = new wxBoxSizer( wxHORIZONTAL );

//create two buttons that are horizontally unstretchable, 
  // with an all-around border with a width of 10 and implicit top alignment
 button_sizer->Add(
    new wxButton(this, ID_WEIGHT_OK, _T("OK") ), 0, wxALIGN_LEFT|wxALL, 10);

 button_sizer->Add(
   new wxButton(this, ID_WEIGHT_CANCEL, _T("Cancel") ), 0, wxALIGN_CENTER|wxALL, 10);

  //create a sizer with no border and centered horizontally
  topsizer->Add(button_sizer, 0, wxALIGN_CENTER);

  SetSizer(topsizer);      // use the sizer for layout

  topsizer->SetSizeHints( this );   // set size hints to honour minimum size

return;
}
/**************************************************************************
* Load input parameters when creating the dialog window: 
**************************************************************************/
void JLP_GsegWeight_Dlg::UpdatePanel(GSEG_SETTINGS GsegSet0)
{
int i;
wxString buffer;

for(i = 0; i < n1; i++) {

// Update values of display:
  buffer.Printf(_T("%d"), GsegSet0.int1_blk[i]);
  ParCtrl_int1_blk[i]->SetValue(buffer);
  buffer.Printf(_T("%d"), GsegSet0.int2_blk[i]);
  ParCtrl_int2_blk[i]->SetValue(buffer);
  buffer.Printf(_T("%.2f"), GsegSet0.weight_blk[i]);
  ParCtrl_weight_blk[i]->SetValue(buffer);

// Store to private variables:
  Int1[i] = GsegSet0.int1_blk[i];
  Int2[i] = GsegSet0.int2_blk[i];
  Weight[i] = GsegSet0.weight_blk[i];

  }

return;
}
/**************************************************************************
* Handle "OK" button:
**************************************************************************/
void JLP_GsegWeight_Dlg::OnOKButton( wxCommandEvent& WXUNUSED(event) )
{
int i; 
long ivalue;
double fvalue;
bool data_is_ok;

for(i = 0; i < n1; i++) {
  if(ParCtrl_int1_blk[i]->GetValue().ToLong(&ivalue))
           Int1[i] = ivalue;
  if(ParCtrl_int2_blk[i]->GetValue().ToLong(&ivalue))
           Int2[i] = ivalue;
  if(ParCtrl_weight_blk[i]->GetValue().ToDouble(&fvalue))
           Weight[i] = fvalue;
  }

// Check if the data entered by the user is correct:
data_is_ok = true;
 for(i = 0; i < n1; i++) {
   if((Int2[i] < Int1[i]) || (Weight[i] < 0 )) {
     data_is_ok = false;
     break;
   }
 }

if(data_is_ok == false) {
 wxMessageBox(wxT("Error: bad parameters: please correct them!"),
              wxT("JLP_GsegWeight_Dlg"), wxOK | wxICON_ERROR);
 return;
 }

// Close dialog and return status = 0:
  EndModal(0); 
}
/**************************************************************************
* Handle "Cancel" button:
**************************************************************************/
void JLP_GsegWeight_Dlg::OnCancelButton( wxCommandEvent& WXUNUSED(event) )
{
// Close dialog and return status = 1:
  EndModal(1); 
}
