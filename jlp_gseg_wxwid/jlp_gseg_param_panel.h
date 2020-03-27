/****************************************************************************
* Name: jlp_gseg_param_panel.h
* 
* JLP
* Version 12/06/2017
****************************************************************************/
#ifndef _jlp_gseg_param_panel_ 
#define _jlp_gseg_param_panel_

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif

#include "wx/tglbtn.h"
#include "wx/statusbr.h"
#include "wx/imaglist.h"
#include "wx/cshelp.h"

#if wxUSE_TOOLTIPS
    #include "wx/tooltip.h"
#endif

#include "wx/progdlg.h"

#include "jlp_gsegset_defs.h"       // GSEG_SETTINGS
#include "jlp_gsegset_params.h"     // COPY_GSEG_SETTINGS
#include "jlp_gdev_wxwid.h"     // JLP_GDev_wxWID 

// Should be larger than NBLK_MAX and NDROP_MAX:
#define NCHOICES 12

typedef struct {
wxComboBox *combo;
wxString choices[NCHOICES];
wxString label;
int nchoices;
} JLP_ComboBox;

class JLP_wxLogbook;

//----------------------------------------------------------------------
// class definitions
//----------------------------------------------------------------------
class JLP_Gseg_ParamPanel: public wxPanel
{
public:

  JLP_Gseg_ParamPanel(wxFrame *frame, JLP_wxLogbook *logbook); 
  ~JLP_Gseg_ParamPanel();
  int GsegPP_WriteToLogbook(wxString str1, bool save_to_file0);

// jlp_gseg_param_panel_menu.cpp
  int ParamPanel_Setup();
  void ParamPanel_LeftSetup(wxBoxSizer *left_vsizer);
  void ParamPanel_RightSetup(wxBoxSizer *right_vsizer);
  void ParamPanel_RightSetup2(wxBoxSizer *right2_vsizer);
  void ParamPanel_GetValues(GSEG_SETTINGS &GsegSet0);
  void ParamPanel_SetValues(GSEG_SETTINGS GsegSet0);

// jlp_gseg_param_panel_onclick.cpp
  void OnParamValid(wxCommandEvent& event);
  void OnParamSave(wxCommandEvent& event);
  void OnParamLoad(wxCommandEvent& event);
  void OnParamDefault(wxCommandEvent& event);
  void OnSelectWeightBlk(wxCommandEvent& event);
  void OnFillWeightBlk(wxCommandEvent& event);
  void SaveParamToKwdFile();
  void SaveParamToGsegrafFile();
  int LoadKeywordParamFile(GSEG_SETTINGS &GsegSet0);
  int LoadGsegrafParamFile(GSEG_SETTINGS &GsegSet0);

// Accessors:
   int GetSpea1(GSEG_SETTINGS& GsegSet0) {
    Copy_GSEG_SETTINGS(GsegSet0, GsegSet1);
    return(0);
    };

private:
  int initialized;

// Main parameters:
  JLP_wxLogbook *jlp_logbook;
  wxPanel *main_panel;

  GSEG_PARAM GsegParam1[512];
  GSEG_SETTINGS GsegSet1;

  JLP_ComboBox ParCmb_dust_and_agn, ParCmb_n_weightblk;
  wxCheckBox *ParCheck_dust_or_agn;
  wxCheckBox *ParCheck_std_dev, *ParCheck_const_sigma, *ParCheck_all_flux;
  wxCheckBox *ParCheck_full_spect, *ParCheck_use_solution;
  wxTextCtrl *ParCtrl_sigma_gauss, *ParCtrl_bestfit;
  wxTextCtrl *ParCtrl_n_pix, *ParCtrl_norm_wavel;
  wxTextCtrl *ParCtrl_wavel_unit, *ParCtrl_alpha_inf, *ParCtrl_alpha_sup;
  wxTextCtrl *ParCtrl_red_ext1, *ParCtrl_red_ext2, *ParCtrl_redlaw_fname;
  wxTextCtrl *ParCtrl_n_disp, *ParCtrl_start_disp, *ParCtrl_step_disp;
  wxTextCtrl *ParCtrl_endz, *ParCtrl_endz0, *ParCtrl_start_z, *ParCtrl_step_z;
  wxTextCtrl *ParCtrl_n_dust, *ParCtrl_start_dust, *ParCtrl_step_dust;
  wxTextCtrl *ParCtrl_n_agn, *ParCtrl_start_agn, *ParCtrl_step_agn;
  wxTextCtrl *ParCtrl_SNR;
  wxTextCtrl *ParCtrl_solution_fname, *ParCtrl_database_fname;
  wxTextCtrl *ParCtrl_database_dir, *ParCtrl_listgal_fname;
  wxTextCtrl *ParCtrl_results_fname;
  wxButton *ParBut_fill_weight;

  DECLARE_EVENT_TABLE()
};

#endif
