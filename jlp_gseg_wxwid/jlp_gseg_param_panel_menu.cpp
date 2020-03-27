/****************************************************************************
* Name: jlp_gseg_param_panel_menu.cpp 
* 
* JLP
* Version 05/06/2017
****************************************************************************/
#include "jlp_gseg_param_panel_id.h"
#include "jlp_gseg_param_panel.h"
#include "jlp_gsegset_params.h"    // Init_GSEG_SETTINGS_with_default_values()
/***
#include "spea_weight_dlg.h"    // Spea_Weight_Dlg class
#include "spea_drop_dlg.h"    // Spea_Drop_Dlg class
***/

/********************************************************************
* Setup of ParamPanel
*
********************************************************************/
int JLP_Gseg_ParamPanel::ParamPanel_Setup()
{
wxBoxSizer *w_topsizer, *w_hsizer1, *w_hsizer2;
wxBoxSizer *param_left_vsizer, *param_right_vsizer, *param_right_vsizer2;
wxButton *ParamValidButton; 
wxButton*ParamSave1Button, *ParamLoad1Button;
wxButton*ParamSave2Button, *ParamLoad2Button;
wxButton *ParamDefaultButton;
int status;

  w_topsizer = new wxBoxSizer(wxVERTICAL);

  w_hsizer1 = new wxBoxSizer( wxHORIZONTAL );

// Create widget setup: 
  param_left_vsizer = new wxBoxSizer(wxVERTICAL);
  ParamPanel_LeftSetup(param_left_vsizer);
  w_hsizer1->Add(param_left_vsizer, 0, wxALIGN_CENTRE_VERTICAL | wxALL, 6);

  param_right_vsizer = new wxBoxSizer(wxVERTICAL);
  ParamPanel_RightSetup(param_right_vsizer);
  w_hsizer1->Add(param_right_vsizer, 0, wxALIGN_CENTRE_VERTICAL | wxALL, 10);

  param_right_vsizer2 = new wxBoxSizer(wxVERTICAL);
  ParamPanel_RightSetup2(param_right_vsizer2);
  w_hsizer1->Add(param_right_vsizer2, 0, wxALIGN_CENTRE_VERTICAL | wxALL, 10);

  w_topsizer->Add(w_hsizer1, 0, wxALIGN_CENTER | wxTOP, 10);

///////////////////////////////////////////////////////////////////////
// Create five buttons at bottom: 
// OK, Standard value, Save config, Load config
 w_hsizer2 = new wxBoxSizer( wxHORIZONTAL );

 ParamValidButton = new wxButton(this, ID_PARAM_VALID,
                                        _T("Validate current settings"));
// "Keyword format":
 ParamSave1Button = new wxButton(this, ID_PARAM_SAVE1,
                                 _T("Save param (kwd)"));
// "LoopBase.f90 format"
 ParamSave2Button = new wxButton(this, ID_PARAM_SAVE2,
                             _T("Save param (f90)"));
// "Keyword format":
 ParamLoad1Button = new wxButton(this, ID_PARAM_LOAD1,
                                 _T("Load param (kwd)")); 
// "LoopBase.f90 format"
 ParamLoad2Button = new wxButton(this, ID_PARAM_LOAD2,
                             _T("Load param (f90)")); 
 ParamDefaultButton = new wxButton(this, ID_PARAM_DEFAULT,
                                        _T("Default values"));

// Add buttons, horizontally unstretchable, with minimal size:
 w_hsizer2->Add( ParamValidButton, 0);
 w_hsizer2->Add( ParamSave1Button, 0, wxALIGN_RIGHT |wxLEFT | wxRIGHT, 20);
 w_hsizer2->Add( ParamSave2Button, 0, wxALIGN_RIGHT |wxLEFT | wxRIGHT, 20);
 w_hsizer2->Add( ParamLoad1Button, 0, wxALIGN_RIGHT |wxLEFT | wxRIGHT, 20);
 w_hsizer2->Add( ParamLoad2Button, 0, wxALIGN_RIGHT |wxLEFT | wxRIGHT, 20);
 w_hsizer2->Add( ParamDefaultButton, 0, wxALIGN_RIGHT |wxLEFT | wxRIGHT, 20);

// Add button sizer with minimal size:
  w_topsizer->Add(w_hsizer2, 0, wxALIGN_CENTER | wxALL, 20);

  this->SetSizer(w_topsizer);

  Centre();

return(0);
}
/********************************************************************
* Param Setup panel (left side)
********************************************************************/
void JLP_Gseg_ParamPanel::ParamPanel_LeftSetup(wxBoxSizer *param_left_vsizer)
{
wxStaticBoxSizer *dust_sizer, *spect_sizer;
wxBoxSizer *hsizer1, *hsizer2;
int irows, icols, vgap, hgap;
wxFlexGridSizer *fgs1, *fgs2;

// ****************** spectrum ********************************************/
 spect_sizer = new wxStaticBoxSizer(wxVERTICAL, this,
                                   _T("Spectrum parameters"));
 irows = 10; icols = 2;
 vgap = 10; hgap = 10;
 fgs1 = new wxFlexGridSizer(irows, icols, vgap, hgap);

 fgs1->Add(new wxStaticText(this, -1, wxT("Sigma gauss (convolution)")),
           0, wxALIGN_CENTRE_VERTICAL);
 ParCtrl_sigma_gauss = new wxTextCtrl(this, -1, wxT("0."),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs1->Add(ParCtrl_sigma_gauss);
 fgs1->Add(new wxStaticText(this, -1, wxT("N_pixels")),
           0, wxALIGN_CENTRE_VERTICAL);
 ParCtrl_n_pix = new wxTextCtrl(this, -1, wxT("1"),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs1->Add(ParCtrl_n_pix);
 fgs1->Add(new wxStaticText(this, -1, wxT("Norm. wavelength")),
           0, wxALIGN_CENTRE_VERTICAL);
 ParCtrl_norm_wavel = new wxTextCtrl(this, -1, wxT("5089."),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs1->Add(ParCtrl_norm_wavel);
 fgs1->Add(new wxStaticText(this, -1, wxT("Wavelength unit")),
           0, wxALIGN_CENTRE_VERTICAL);
 ParCtrl_wavel_unit = new wxTextCtrl(this, -1, wxT("0"),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs1->Add(ParCtrl_wavel_unit);
 fgs1->Add(new wxStaticText(this, -1, wxT("Alpha inf.")),
           0, wxALIGN_CENTRE_VERTICAL);
 ParCtrl_alpha_inf = new wxTextCtrl(this, -1, wxT("0"),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs1->Add(ParCtrl_alpha_inf);
 fgs1->Add(new wxStaticText(this, -1, wxT("Alpha sup.")),
           0, wxALIGN_CENTRE_VERTICAL);
 ParCtrl_alpha_sup = new wxTextCtrl(this, -1, wxT("0"),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs1->Add(ParCtrl_alpha_sup);
 fgs1->Add(new wxStaticText(this, -1, wxT("Red1")),
           0, wxALIGN_CENTRE_VERTICAL);
 ParCtrl_red_ext1 = new wxTextCtrl(this, -1, wxT("0"),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs1->Add(ParCtrl_red_ext1);
 fgs1->Add(new wxStaticText(this, -1, wxT("Red2")),
           0, wxALIGN_CENTRE_VERTICAL);
 ParCtrl_red_ext2 = new wxTextCtrl(this, -1, wxT("0"),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs1->Add(ParCtrl_red_ext2);
 fgs1->Add(new wxStaticText(this, -1, wxT("SNR")),
           0, wxALIGN_CENTRE_VERTICAL);
 ParCtrl_SNR = new wxTextCtrl(this, -1, wxT("0."),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs1->Add(ParCtrl_SNR);
 fgs1->Add(new wxStaticText(this, -1, wxT("Best fit")),
           0, wxALIGN_CENTRE_VERTICAL);
 ParCtrl_bestfit = new wxTextCtrl(this, -1, wxT("0.01"),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs1->Add(ParCtrl_bestfit);
 spect_sizer->Add(fgs1, 0, wxALL, 20);
param_left_vsizer->Add(spect_sizer, 0, wxALL, 20);

// ****************** dust ********************************************/
 dust_sizer = new wxStaticBoxSizer(wxVERTICAL, this,
                                   _T("Dust absorption - AGN emission "));

// Dust agn
 hsizer1 = new wxBoxSizer(wxHORIZONTAL);
 ParCmb_dust_and_agn.label = wxT("Dust / AGN :");
 ParCmb_dust_and_agn.nchoices = 4;
 ParCmb_dust_and_agn.choices[0] = wxT("n");
 ParCmb_dust_and_agn.choices[1] = wxT("D");
 ParCmb_dust_and_agn.choices[2] = wxT("A");
 ParCmb_dust_and_agn.choices[3] = wxT("B");
 ParCmb_dust_and_agn.combo = new wxComboBox(this, ID_PARAM_CMB_DUST, 
                                            wxT(""),
                                            wxDefaultPosition, wxSize(60, 28),
                                            ParCmb_dust_and_agn.nchoices,
                                            ParCmb_dust_and_agn.choices);
 hsizer1->Add(new wxStaticText(this, -1, ParCmb_dust_and_agn.label),
                  0, wxRIGHT | wxALIGN_CENTRE_VERTICAL, 20);
 hsizer1->Add(ParCmb_dust_and_agn.combo);
 dust_sizer->Add(hsizer1, 0, wxLEFT | wxRIGHT | wxTOP, 20);

 irows = 5; icols = 4;
 vgap = 10; hgap = 10;
 fgs2 = new wxFlexGridSizer(irows, icols, vgap, hgap);

 fgs2->Add(new wxStaticText(this, -1, wxT("")));
 fgs2->Add(new wxStaticText(this, -1, wxT("Nber")));
 fgs2->Add(new wxStaticText(this, -1, wxT("Start")));
 fgs2->Add(new wxStaticText(this, -1, wxT("Step")));

// Dust:
 fgs2->Add(new wxStaticText(this, -1, wxT("Dust:")),
           0, wxALIGN_CENTRE_VERTICAL);
 ParCtrl_n_dust = new wxTextCtrl(this, -1, wxT("0"),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs2->Add(ParCtrl_n_dust);
 ParCtrl_start_dust = new wxTextCtrl(this, -1, wxT("0."),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs2->Add(ParCtrl_start_dust);
 ParCtrl_step_dust = new wxTextCtrl(this, -1, wxT("0."),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs2->Add(ParCtrl_step_dust);

// AGN:
 fgs2->Add(new wxStaticText(this, -1, wxT("AGN:")),
           0, wxALIGN_CENTRE_VERTICAL);
 ParCtrl_n_agn = new wxTextCtrl(this, -1, wxT("0"),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs2->Add(ParCtrl_n_agn);
 ParCtrl_start_agn = new wxTextCtrl(this, -1, wxT("0."),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs2->Add(ParCtrl_start_agn);
 ParCtrl_step_agn = new wxTextCtrl(this, -1, wxT("0."),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs2->Add(ParCtrl_step_agn);

 dust_sizer->Add(fgs2, 0, wxALL, 20);
param_left_vsizer->Add(dust_sizer, 0, wxALL, 20);

}
/********************************************************************
* Param Setup panel (right side)
********************************************************************/
void JLP_Gseg_ParamPanel::ParamPanel_RightSetup(wxBoxSizer *param_right_vsizer)
{
wxStaticBoxSizer *disp_sizer, *opt_sizer, *z_sizer;
wxBoxSizer *hsizer1;
int irows, icols, vgap, hgap;
wxFlexGridSizer *fgs1, *fgs2, *fgs3;

// ****************** dispersion ********************************************/
 disp_sizer = new wxStaticBoxSizer(wxVERTICAL, this,
                                   _T("Dispersion"));
 irows = 2; icols = 3;
 vgap = 10; hgap = 10;
 fgs1 = new wxFlexGridSizer(irows, icols, vgap, hgap);

 fgs1->Add(new wxStaticText(this, -1, wxT("Nber")));
 fgs1->Add(new wxStaticText(this, -1, wxT("Start")));
 fgs1->Add(new wxStaticText(this, -1, wxT("Step")));

 ParCtrl_n_disp = new wxTextCtrl(this, -1, wxT("0"),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs1->Add(ParCtrl_n_disp);
 ParCtrl_start_disp = new wxTextCtrl(this, -1, wxT("0."),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs1->Add(ParCtrl_start_disp);
 ParCtrl_step_disp = new wxTextCtrl(this, -1, wxT("0."),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs1->Add(ParCtrl_step_disp);

 disp_sizer->Add(fgs1, 0, wxALL, 20);
param_right_vsizer->Add(disp_sizer, 0, wxALL, 20);

// ****************** redshift ********************************************/
 z_sizer = new wxStaticBoxSizer(wxVERTICAL, this,
                                   _T("Redshift"));
 irows = 4; icols = 2;
 vgap = 10; hgap = 10;
 fgs2 = new wxFlexGridSizer(irows, icols, vgap, hgap);

 fgs2->Add(new wxStaticText(this, -1, wxT("Start")));
 fgs2->Add(new wxStaticText(this, -1, wxT("Step")));

 ParCtrl_start_z = new wxTextCtrl(this, -1, wxT("0."),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs2->Add(ParCtrl_start_z);
 ParCtrl_step_z = new wxTextCtrl(this, -1, wxT("0."),
                                     wxPoint(-1, -1), wxSize(80, 28));
 fgs2->Add(ParCtrl_step_z);

 fgs2->Add(new wxStaticText(this, -1, wxT("End_z0")));
 fgs2->Add(new wxStaticText(this, -1, wxT("End_z")));
 ParCtrl_endz0 = new wxTextCtrl(this, -1, wxT("0"),
                               wxPoint(-1, -1), wxSize(80, 28));
 fgs2->Add(ParCtrl_endz0);
 ParCtrl_endz = new wxTextCtrl(this, -1, wxT("0"),
                               wxPoint(-1, -1), wxSize(80, 28));
 fgs2->Add(ParCtrl_endz);

 z_sizer->Add(fgs2, 0, wxALL, 20);
param_right_vsizer->Add(z_sizer, 0, wxALL, 20);

// ****************** Options ********************************************/
 opt_sizer = new wxStaticBoxSizer(wxVERTICAL, this,
                                   _T("Options"));
 irows = 9; icols = 1;
 vgap = 10; hgap = 10;
 fgs3 = new wxFlexGridSizer(irows, icols, vgap, hgap);

// Check boxes:
 ParCheck_dust_or_agn = new wxCheckBox(this, -1, wxT("Dust or AGN"));
 fgs3->Add(ParCheck_dust_or_agn);

 ParCheck_std_dev = new wxCheckBox(this, -1, wxT("standard dev."));
 fgs3->Add(ParCheck_std_dev);

 ParCheck_const_sigma = new wxCheckBox(this, -1, wxT("const. sigma"));
 fgs3->Add(ParCheck_const_sigma);

 ParCheck_all_flux = new wxCheckBox(this, -1, wxT("all flux"));
 fgs3->Add(ParCheck_all_flux);

 ParCheck_full_spect = new wxCheckBox(this, -1, wxT("full spect"));
 fgs3->Add(ParCheck_full_spect);

 ParCheck_use_solution = new wxCheckBox(this, -1, wxT("use solution"));
 fgs3->Add(ParCheck_use_solution);

 opt_sizer->Add(fgs3, 0, wxALL, 20);
param_right_vsizer->Add(opt_sizer, 0, wxALL, 20);

}
/********************************************************************
* Param Setup panel (far right side)
********************************************************************/
void JLP_Gseg_ParamPanel::ParamPanel_RightSetup2(wxBoxSizer *param_right_vsizer)
{
wxStaticBoxSizer *file_sizer, *weight_sizer, *drop_sizer;
wxBoxSizer *hsizer1;
int i, k, irows, icols, vgap, hgap;
wxFlexGridSizer *fgs1, *fgs2, *fgs3;

// ****************** Files ********************************************/
 file_sizer = new wxStaticBoxSizer(wxVERTICAL, this, _T("Files"));
 irows = 6; icols = 2;
 vgap = 10; hgap = 10;
 fgs1 = new wxFlexGridSizer(irows, icols, vgap, hgap);

 fgs1->Add(new wxStaticText(this, -1, wxT("Redlaw filename:")),
           0, wxALIGN_CENTRE_VERTICAL);
 ParCtrl_redlaw_fname = new wxTextCtrl(this, -1, 
                                      wxT("LoiHowarthMILESf.txt"),
                                      wxPoint(-1, -1), wxSize(200, 28));
 fgs1->Add(ParCtrl_redlaw_fname);
 fgs1->Add(new wxStaticText(this, -1, wxT("Solution filename:")),
           0, wxALIGN_CENTRE_VERTICAL);
 ParCtrl_solution_fname = new wxTextCtrl(this, -1, wxT(""),
                                      wxPoint(-1, -1), wxSize(200, 28));
 fgs1->Add(ParCtrl_solution_fname);
 fgs1->Add(new wxStaticText(this, -1, wxT("Database filename:")),
           0, wxALIGN_CENTRE_VERTICAL);
 ParCtrl_database_fname = new wxTextCtrl(this, -1, 
                                         wxT("MILES_libnorm.txt"),
                                         wxPoint(-1, -1), wxSize(200, 28));
 fgs1->Add(ParCtrl_database_fname);
 fgs1->Add(new wxStaticText(this, -1, wxT("Database directory:")),
           0, wxALIGN_CENTRE_VERTICAL);
 ParCtrl_database_dir = new wxTextCtrl(this, -1, wxT("."),
                                         wxPoint(-1, -1), wxSize(200, 28));
 fgs1->Add(ParCtrl_database_dir);
 fgs1->Add(new wxStaticText(this, -1, wxT("List of galaxies:")),
           0, wxALIGN_CENTRE_VERTICAL);
 ParCtrl_listgal_fname = new wxTextCtrl(this, -1, 
                                         wxT("test.dat"),
                                         wxPoint(-1, -1), wxSize(200, 28));
 fgs1->Add(ParCtrl_listgal_fname);
 fgs1->Add(new wxStaticText(this, -1, wxT("Results filename:")),
           0, wxALIGN_CENTRE_VERTICAL);
 ParCtrl_results_fname = new wxTextCtrl(this, -1, 
                                         wxT("results.txt"),
                                         wxPoint(-1, -1), wxSize(200, 28));
 fgs1->Add(ParCtrl_results_fname);

 file_sizer->Add(fgs1, 0, wxALL, 20);
param_right_vsizer->Add(file_sizer, 0, wxLEFT | wxRIGHT, 20);

// ****************** Weights ********************************************/
 weight_sizer = new wxStaticBoxSizer(wxVERTICAL, this,
                                     _T("Weight blocks"));
// Weight blocks: 
 ParCmb_n_weightblk.label = wxT("Nber of blocks:");
 ParCmb_n_weightblk.nchoices = 11;
 for(i = 0; i < 11; i++) ParCmb_n_weightblk.choices[i].Printf(wxT("%d"), i);
 ParCmb_n_weightblk.combo = new wxComboBox(this, ID_PARAM_CMB_WEIGHT, 
                                           wxT(""),
                                           wxDefaultPosition, wxSize(60, 28),
                                           ParCmb_n_weightblk.nchoices,
                                           ParCmb_n_weightblk.choices);
// Fill weight selection button:
 ParBut_fill_weight = new wxButton(this, ID_PARAM_WEIGHT_BUTTON, 
                                   _T("Fill weight selection"));

 irows = 2; icols = 2;
 vgap = 10; hgap = 10;
 fgs2 = new wxFlexGridSizer(irows, icols, vgap, hgap);

 fgs2->Add(new wxStaticText(this, -1, ParCmb_n_weightblk.label),
           0, wxALIGN_CENTRE_VERTICAL);
 fgs2->Add(ParCmb_n_weightblk.combo);
 fgs2->Add(ParBut_fill_weight);

 weight_sizer->Add(fgs2, 0, wxALL, 20);
param_right_vsizer->Add(weight_sizer, 0, wxALL, 20);

}
/********************************************************************
* Param Setup panel: read values of all items
********************************************************************/
void JLP_Gseg_ParamPanel::ParamPanel_GetValues(GSEG_SETTINGS &GsegSet0)
{
int i;
long ivalue;
double fvalue;

if(ParCtrl_sigma_gauss->GetValue().ToDouble(&fvalue)) 
             GsegSet0.sigma_gauss = fvalue;
if(ParCtrl_bestfit->GetValue().ToDouble(&fvalue)) 
             GsegSet0.bestfit = fvalue;
if(ParCtrl_n_pix->GetValue().ToLong(&ivalue)) GsegSet0.n_pix = ivalue;

GsegSet0.dust_or_agn = ParCheck_dust_or_agn->GetValue();

i = ParCmb_dust_and_agn.combo->GetSelection();
GsegSet0.dust_and_agn = ParCmb_dust_and_agn.choices[i]; 

if(ParCtrl_start_dust->GetValue().ToDouble(&fvalue)) GsegSet0.start_dust = fvalue;
if(ParCtrl_step_dust->GetValue().ToDouble(&fvalue)) GsegSet0.step_dust = fvalue;
if(ParCtrl_n_dust->GetValue().ToLong(&ivalue)) GsegSet0.n_dust = ivalue;

if(ParCtrl_start_agn->GetValue().ToDouble(&fvalue)) GsegSet0.start_agn = fvalue;
if(ParCtrl_step_agn->GetValue().ToDouble(&fvalue)) GsegSet0.step_agn = fvalue;
if(ParCtrl_n_agn->GetValue().ToLong(&ivalue)) GsegSet0.n_agn = ivalue;

if(ParCtrl_norm_wavel->GetValue().ToDouble(&fvalue)) GsegSet0.norm_wavel = fvalue;
if(ParCtrl_wavel_unit->GetValue().ToLong(&ivalue)) GsegSet0.wavel_unit = ivalue;

GsegSet0.std_dev = ParCheck_std_dev->GetValue();

if(ParCtrl_alpha_inf->GetValue().ToLong(&ivalue)) GsegSet0.alpha_inf = ivalue;
if(ParCtrl_alpha_sup->GetValue().ToLong(&ivalue)) GsegSet0.alpha_sup = ivalue;
if(ParCtrl_red_ext1->GetValue().ToLong(&ivalue)) GsegSet0.red_ext1 = ivalue;
if(ParCtrl_red_ext2->GetValue().ToLong(&ivalue)) GsegSet0.red_ext2 = ivalue;

GsegSet0.redlaw_fname = ParCtrl_redlaw_fname->GetValue(); 

if(ParCtrl_start_disp->GetValue().ToDouble(&fvalue)) GsegSet0.start_disp = fvalue;
if(ParCtrl_step_disp->GetValue().ToDouble(&fvalue)) GsegSet0.step_disp = fvalue;
if(ParCtrl_n_disp->GetValue().ToLong(&ivalue)) GsegSet0.n_disp = ivalue;

GsegSet0.const_sigma = ParCheck_const_sigma->GetValue();

if(ParCtrl_start_z->GetValue().ToDouble(&fvalue)) GsegSet0.start_z = fvalue;
if(ParCtrl_step_z->GetValue().ToDouble(&fvalue)) GsegSet0.step_z = fvalue;
if(ParCtrl_endz0->GetValue().ToLong(&ivalue)) GsegSet0.endz0 = ivalue;
if(ParCtrl_endz->GetValue().ToLong(&ivalue)) GsegSet0.endz = ivalue;

GsegSet0.all_flux = ParCheck_all_flux->GetValue();
GsegSet0.full_spect = ParCheck_full_spect->GetValue();
GsegSet0.use_solution = ParCheck_use_solution->GetValue();

GsegSet0.solution_fname = ParCtrl_solution_fname->GetValue();
if(ParCtrl_SNR->GetValue().ToDouble(&fvalue)) GsegSet0.SNR = fvalue;
GsegSet0.database_fname = ParCtrl_database_fname->GetValue();
GsegSet0.database_dir = ParCtrl_database_dir->GetValue();
GsegSet0.listgal_fname = ParCtrl_listgal_fname->GetValue();
GsegSet0.results_fname = ParCtrl_results_fname->GetValue();

GsegSet0.n_weightblk = ParCmb_n_weightblk.combo->GetSelection(); 

return;
}
/********************************************************************
* Param Setup panel: write values to all items
********************************************************************/
void JLP_Gseg_ParamPanel::ParamPanel_SetValues(GSEG_SETTINGS GsegSet0)
{
int i;
long ivalue;
double fvalue;
wxString buffer;
char c_agn, s_agn[20];

buffer.Printf(_T("%.2f"), GsegSet0.sigma_gauss);
ParCtrl_sigma_gauss->SetValue(buffer);

buffer.Printf(_T("%.2f"), GsegSet0.bestfit);
ParCtrl_bestfit->SetValue(buffer);

buffer.Printf(_T("%d"), GsegSet0.n_pix);
ParCtrl_n_pix->SetValue(buffer);

// Dust/AGN :

ParCheck_dust_or_agn->SetValue(GsegSet0.dust_or_agn);

strcpy(s_agn,GsegSet0.dust_and_agn.mb_str());
c_agn = s_agn[0];
switch(c_agn) {
  default:
  case 'n':
    i = 0;
    break;
  case 'D':
    i = 1;
    break;
  case 'A':
    i = 2;
    break;
  case 'B':
    i = 3;
    break;
  }
ParCmb_dust_and_agn.combo->SetSelection(i);

// Dust:
buffer.Printf(_T("%.2f"), GsegSet0.start_dust);
ParCtrl_start_dust->SetValue(buffer);
buffer.Printf(_T("%.2f"), GsegSet0.step_dust);
ParCtrl_step_dust->SetValue(buffer);
buffer.Printf(_T("%d"), GsegSet0.n_dust);
ParCtrl_n_dust->SetValue(buffer);

// AGN: 
buffer.Printf(_T("%.2f"), GsegSet0.start_agn);
ParCtrl_start_agn->SetValue(buffer);
buffer.Printf(_T("%.2f"), GsegSet0.step_agn);
ParCtrl_step_agn->SetValue(buffer);
buffer.Printf(_T("%d"), GsegSet0.n_agn);
ParCtrl_n_agn->SetValue(buffer);

buffer.Printf(_T("%.2f"), GsegSet0.norm_wavel);
ParCtrl_norm_wavel->SetValue(buffer);
buffer.Printf(_T("%d"), GsegSet0.wavel_unit);
ParCtrl_wavel_unit->SetValue(buffer);

ParCheck_std_dev->SetValue(GsegSet0.std_dev);

buffer.Printf(_T("%d"), GsegSet0.alpha_inf);
ParCtrl_alpha_inf->SetValue(buffer);
buffer.Printf(_T("%d"), GsegSet0.alpha_sup);
ParCtrl_alpha_sup->SetValue(buffer);
buffer.Printf(_T("%d"), GsegSet0.red_ext1);
ParCtrl_red_ext1->SetValue(buffer);
buffer.Printf(_T("%d"), GsegSet0.red_ext2);
ParCtrl_red_ext2->SetValue(buffer);

strcpy(s_agn, GsegSet0.redlaw_fname.mb_str());

ParCtrl_redlaw_fname->SetValue(GsegSet0.redlaw_fname); 

// Disp:
buffer.Printf(_T("%.2f"), GsegSet0.start_disp);
ParCtrl_start_disp->SetValue(buffer);
buffer.Printf(_T("%.2f"), GsegSet0.step_disp);
ParCtrl_step_disp->SetValue(buffer);
buffer.Printf(_T("%d"), GsegSet0.n_disp);
ParCtrl_n_disp->SetValue(buffer);

ParCheck_const_sigma->SetValue(GsegSet0.const_sigma);

// Redshift 
buffer.Printf(_T("%.2f"), GsegSet0.start_z);
ParCtrl_start_z->SetValue(buffer);
buffer.Printf(_T("%.2f"), GsegSet0.step_z);
ParCtrl_step_z->SetValue(buffer);
buffer.Printf(_T("%d"), GsegSet0.endz0);
ParCtrl_endz0->SetValue(buffer);
buffer.Printf(_T("%d"), GsegSet0.endz);
ParCtrl_endz->SetValue(buffer);

ParCheck_all_flux->SetValue(GsegSet0.all_flux);
ParCheck_full_spect->SetValue(GsegSet0.full_spect);
ParCheck_use_solution->SetValue(GsegSet0.use_solution);

ParCtrl_solution_fname->SetValue(GsegSet0.solution_fname);

buffer.Printf(_T("%.2f"), GsegSet0.SNR);
ParCtrl_SNR->SetValue(buffer);

ParCtrl_database_fname->SetValue(GsegSet0.database_fname);
ParCtrl_database_dir->SetValue(GsegSet0.database_dir);
ParCtrl_listgal_fname->SetValue(GsegSet0.listgal_fname);
ParCtrl_results_fname->SetValue(GsegSet0.results_fname);

// Weight blocks
GsegSet0.n_weightblk = MAXI(0, GsegSet0.n_weightblk);
GsegSet0.n_weightblk = MINI(10, GsegSet0.n_weightblk);
ParCmb_n_weightblk.combo->SetSelection(GsegSet0.n_weightblk);

// Refresh screen:
Refresh();

return;
}
