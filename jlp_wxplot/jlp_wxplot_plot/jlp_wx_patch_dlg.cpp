/******************************************************************************
* jlp_wx_patch_dlg.cpp
* Dialog box used when performing cosmetic corrections on images
*
* Author:      JLP 
* Version:     28/01/2009
******************************************************************************/
#include "jlp_wx_patch_dlg.h"

//*************************************************************************
enum
{
   ID_PATCH_X    = 800,
   ID_PATCH_Y,
   ID_PATCH_RAD1,
   ID_PATCH_RAD2,
   ID_PATCH_ORDER,
   ID_PATCH_NOISE,
   ID_PATCH_POLY,
   ID_PATCH_PROF,
   ID_PATCH_OK,
   ID_PATCH_CANCEL,
   ID_PATCH_NEWTRY
};

BEGIN_EVENT_TABLE(JLP_Patch_Dlg, wxDialog)
EVT_RADIOBUTTON  (ID_PATCH_POLY, JLP_Patch_Dlg::OnCheckPolynomial)
EVT_RADIOBUTTON  (ID_PATCH_PROF, JLP_Patch_Dlg::OnCheckProfile)
EVT_BUTTON  (ID_PATCH_OK, JLP_Patch_Dlg::OnOKButton)
EVT_BUTTON  (ID_PATCH_CANCEL, JLP_Patch_Dlg::OnCancelButton)
EVT_BUTTON  (ID_PATCH_NEWTRY, JLP_Patch_Dlg::OnNewTryButton)
EVT_TEXT    (ID_PATCH_X, JLP_Patch_Dlg::OnChangeParam)
EVT_TEXT    (ID_PATCH_Y, JLP_Patch_Dlg::OnChangeParam)
EVT_TEXT    (ID_PATCH_RAD1, JLP_Patch_Dlg::OnChangeParam)
EVT_TEXT    (ID_PATCH_RAD2, JLP_Patch_Dlg::OnChangeParam)
EVT_TEXT    (ID_PATCH_ORDER, JLP_Patch_Dlg::OnChangeParam)
EVT_TEXT    (ID_PATCH_NOISE, JLP_Patch_Dlg::OnChangeParam)
END_EVENT_TABLE()

// Constructor:
JLP_Patch_Dlg::JLP_Patch_Dlg(wxFrame *parent, double xc0, double yc0,
                             double radius0, double radius_fact0, 
                             int poly_order0, double sigma_noise0, 
                             int nx0, int ny0, int polynomial_method0,
                             const wxString &title) 
        : wxDialog(parent, -1, title, wxPoint(400,100), wxDefaultSize,
                   wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
wxString x_str, y_str, radius_str, radius_fact_str, poly_order_str, noise_str;

// Save input parameters to private variables:
xc = xc0; yc = yc0; 
radius = radius0; radius_fact = radius_fact0;
poly_order = poly_order0;
sigma_noise = sigma_noise0;
polynomial_method = polynomial_method0;
nx = nx0; ny = ny0;

// Initialize the text strings:
  x_str.Printf(_T("%.2f"), xc);
  y_str.Printf(_T("%.2f"), yc);
  radius_str.Printf(_T("%.2f"), radius);
  radius_fact_str.Printf(_T("%.2f"), radius_fact);
  poly_order_str.Printf(_T("%d"), poly_order);
  noise_str.Printf(_T("%.2f"), sigma_noise);

// To avoid initialization problems with Windows:
// (An event is sent to "ChangeParameters"
//  as soon as a Text control is created...)
// First initialize the pointers to NULL
  TextCtrl_x = NULL;
  TextCtrl_y = NULL;
  TextCtrl_radius = NULL;
  TextCtrl_radius_fact = NULL;
  TextCtrl_poly_order = NULL;
  TextCtrl_noise = NULL;

// Create the text controls: 
  TextCtrl_x = new wxTextCtrl( this, ID_PATCH_X, x_str);
  TextCtrl_y = new wxTextCtrl( this, ID_PATCH_Y, y_str);
  TextCtrl_radius = new wxTextCtrl( this, ID_PATCH_RAD1, radius_str);
  TextCtrl_radius_fact = new wxTextCtrl( this, ID_PATCH_RAD2, radius_fact_str);
  TextCtrl_poly_order = new wxTextCtrl( this, ID_PATCH_ORDER, poly_order_str);
  TextCtrl_noise = new wxTextCtrl( this, ID_PATCH_NOISE, noise_str);

wxBoxSizer *topsizer = new wxBoxSizer( wxVERTICAL );

wxBoxSizer *xy_sizer = new wxBoxSizer( wxHORIZONTAL);

// Sizer surrounded with a rectangle, with a title on top:
wxStaticBoxSizer *NewTry_sizer = new wxStaticBoxSizer(wxVERTICAL, this, 
                                   _T(" Change those parameters for a new try"));
    
    xy_sizer->Add( new wxStaticText( this, wxID_ANY, _T("Center (pixels):") ),
                  0, wxALIGN_LEFT|wxALIGN_CENTRE_VERTICAL|wxLEFT|wxRIGHT, 10); 
    xy_sizer->Add( new wxStaticText( this, wxID_ANY, _T("x=") ),
                  0, wxALIGN_CENTRE_VERTICAL|wxRIGHT, 10); 
    xy_sizer->Add(TextCtrl_x, 0, wxRIGHT, 20); 
    xy_sizer->Add( new wxStaticText( this, wxID_ANY, _T("y=") ),
                  0, wxALIGN_CENTRE_VERTICAL|wxRIGHT, 10); 
    xy_sizer->Add(TextCtrl_y, 0, wxRIGHT, 0); 

    NewTry_sizer->Add(xy_sizer, 0, wxALL, 10);

wxBoxSizer *rad_sizer = new wxBoxSizer( wxHORIZONTAL);
    rad_sizer->Add( new wxStaticText( this, wxID_ANY, _T("Radius (pixels)=") ),
                   0, wxALIGN_LEFT|wxALIGN_CENTRE_VERTICAL|wxLEFT|wxRIGHT, 10); 
    rad_sizer->Add(TextCtrl_radius, 0, wxALIGN_LEFT|wxRIGHT, 20); 
    rad_sizer->Add( new wxStaticText( this, wxID_ANY, _T("outer/inner radius=") ),
                   0, wxALIGN_CENTRE_VERTICAL|wxRIGHT, 10); 
    rad_sizer->Add(TextCtrl_radius_fact, 0, wxRIGHT, 0 ); 
    NewTry_sizer->Add(rad_sizer, 0, wxALL, 10);

wxBoxSizer *poly_sizer = new wxBoxSizer( wxHORIZONTAL);
    poly_sizer->Add( new wxStaticText( this, wxID_ANY, 
                    _T("Polynomial order=") ),
                   0, wxALIGN_LEFT|wxALIGN_CENTRE_VERTICAL|wxLEFT|wxRIGHT, 10); 
    poly_sizer->Add(TextCtrl_poly_order, 0, wxRIGHT, 20); 
    poly_sizer->Add( new wxStaticText( this, wxID_ANY, _T(" Sigma of noise=") ),
                   0, wxALIGN_CENTRE_VERTICAL|wxRIGHT, 10); 
    poly_sizer->Add(TextCtrl_noise, 0, wxRIGHT, 0 ); 
    NewTry_sizer->Add(poly_sizer, 0, wxALL, 10);

wxBoxSizer *method_sizer = new wxBoxSizer( wxHORIZONTAL);
    method_sizer->Add( new wxStaticText( this, wxID_ANY, 
                    _T("Method to fit the background:") ),
                   0, wxALIGN_LEFT|wxALIGN_CENTRE_VERTICAL|wxLEFT|wxRIGHT, 10); 
    Poly_RadioButton = new wxRadioButton( this, ID_PATCH_POLY, 
                          _T("Polynomial"), wxPoint(-1,-1), wxSize(-1,-1),
                          wxRB_GROUP);
    Prof_RadioButton = new wxRadioButton( this, ID_PATCH_PROF, _T("Profile"));
    method_sizer->Add( Poly_RadioButton, 0, wxRIGHT, 20);
    method_sizer->Add( Prof_RadioButton, 0, 
                       wxALIGN_CENTRE_VERTICAL|wxRIGHT, 10); 
    NewTry_sizer->Add(method_sizer, 0, wxALL, 10);
  topsizer->Add(NewTry_sizer, 0, wxALL, 20);

wxBoxSizer *button_sizer = new wxBoxSizer( wxHORIZONTAL );

//create two buttons that are horizontally unstretchable, 
  // with an all-around border with a width of 10 and implicit top alignment
 button_sizer->Add(
    new wxButton(this, ID_PATCH_OK, _T("OK") ), 0, wxALL, 10);

 button_sizer->Add(
   new wxButton(this, ID_PATCH_CANCEL, _T("Cancel") ), 0, wxALL, 10);

 button_sizer->Add(
   new wxButton( this, ID_PATCH_NEWTRY, _T("New Try")), 0, wxALL, 10 );

  //create a sizer with no border and centered horizontally
  topsizer->Add(button_sizer, 0, wxALIGN_CENTER);

  SetSizer(topsizer);      // use the sizer for layout

  topsizer->SetSizeHints( this );   // set size hints to honour minimum size

if(polynomial_method) {
  Poly_RadioButton->SetValue(true);
  Prof_RadioButton->SetValue(false);
  } else {
  Poly_RadioButton->SetValue(false);
  Prof_RadioButton->SetValue(true);
  }

return;
}
/**************************************************************************
* Handle "OK" button:
**************************************************************************/
void JLP_Patch_Dlg::OnOKButton( wxCommandEvent& WXUNUSED(event) )
{
// Close dialog and return status = 0:
  EndModal(0); 
}
/**************************************************************************
* Handle "Cancel" button:
**************************************************************************/
void JLP_Patch_Dlg::OnCancelButton( wxCommandEvent& WXUNUSED(event) )
{
// Close dialog and return status = 1:
  EndModal(1); 
}
/**************************************************************************
* Handle "New Try" button:
**************************************************************************/
void JLP_Patch_Dlg::OnNewTryButton( wxCommandEvent& WXUNUSED(event) )
{
double value;
long ivalue;

// First check that fields contain real/integer values
// (and hence that have been loaded to the private variables...) 
    if(!TextCtrl_x->GetValue().ToDouble(&value)
       || !TextCtrl_x->GetValue().ToDouble(&value)
       || !TextCtrl_radius->GetValue().ToDouble(&value)
       || !TextCtrl_radius_fact->GetValue().ToDouble(&value)
       || !TextCtrl_poly_order->GetValue().ToLong(&ivalue)
       || !TextCtrl_noise->GetValue().ToDouble(&value)) {
        wxMessageBox(_T("Bad values: please first correct the fields!"), 
                     _T("Patch parameters"), wxICON_ERROR);
        return;
       }

  if(DataIsOK() != true) {
// If bad new value restore previous value:
     wxMessageBox(_T("Bad parameters! (check that values are coherent or cancel)"), 
                  _T("Patch parameters"), wxICON_ERROR);
   } else {
// Close dialog and return status = 2:
     EndModal(2); 
   }

return;
}
/**************************************************************************
* Handle radio button
* to select the polynomial method
**************************************************************************/
void JLP_Patch_Dlg::OnCheckPolynomial( wxCommandEvent& WXUNUSED(event) )
{
polynomial_method = 1;
}
/**************************************************************************
* Handle radio button
* to select the profile method
**************************************************************************/
void JLP_Patch_Dlg::OnCheckProfile( wxCommandEvent& WXUNUSED(event) )
{
polynomial_method = 0;
}
/**************************************************************************
* Handle text editing 
*
    TextCtrl_x
    TextCtrl_y
    TextCtrl_radius
    TextCtrl_radius_fact
    TextCtrl_poly_order
    TextCtrl_noise
*
**************************************************************************/
void JLP_Patch_Dlg::OnChangeParam( wxCommandEvent& event )
{
double old_value, new_value;
int old_ivalue;
long new_ivalue;
int status = 0;
wxString w_str;

// First check that all text controls are created:
 if(!TextCtrl_x || !TextCtrl_y
    || !TextCtrl_radius || !TextCtrl_radius_fact
    || !TextCtrl_poly_order || !TextCtrl_noise) return; 

  switch (event.GetId())
  {
   case ID_PATCH_X:
    {
// Get new value
    if(TextCtrl_x->GetValue().ToDouble(&new_value)) {
      xc = new_value; 
      }
      break;
    }
   case ID_PATCH_Y:
    {
// Get new value
    if(TextCtrl_y->GetValue().ToDouble(&new_value)) {
      yc = new_value; 
      }
      break;
    }
   case ID_PATCH_RAD1:
    {
// Get new value (without calling DataIsOK since radius can be larger that radius_fact
// temporarily)
    if(TextCtrl_radius->GetValue().ToDouble(&new_value)) {
      radius = new_value; 
      }
      break;
    }
   case ID_PATCH_RAD2:
    {
// Get new value
    if(TextCtrl_radius_fact->GetValue().ToDouble(&new_value)) {
      radius_fact = new_value; 
      }
      break;
    }
   case ID_PATCH_ORDER:
    {
    old_ivalue = poly_order;
// Get new value
    if(TextCtrl_poly_order->GetValue().ToLong(&new_ivalue)) {
       poly_order = new_ivalue; 
       if(DataIsOK() == false) status = -1;
       }
// If bad new value restore previous value:
    if(status) { 
      wxMessageBox(_T("Bad poly order value!"), _T("Patch parameters"), 
                   wxICON_ERROR);
      w_str.Printf(_T("%d"), old_ivalue); 
      TextCtrl_poly_order->SetValue(w_str);
      }
      break;
    }
   case ID_PATCH_NOISE:
    {
    old_value = sigma_noise;
// Get new value
    if(TextCtrl_noise->GetValue().ToDouble(&new_value)) {
       sigma_noise = new_value; 
       if(DataIsOK() == false) status = -1;
       }
// If bad new value restore previous value:
    if(status) { 
      wxMessageBox(_T("Bad sigma noise value!"), _T("Patch parameters"), 
                   wxICON_ERROR);
      w_str.Printf(_T("%.2f"), old_value); 
      TextCtrl_noise->SetValue(w_str);
      }
      break;
    }
  }  // EOF switch
return;
}
