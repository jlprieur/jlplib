/****************************************************************************
* Name: jlp_gseg_param_panel.cpp
* 
* JLP
* Version 06/06/2017
***************************************************************************/
#include <stdio.h>
// wxplot files:
#include "jlp_gseg_param_panel.h"  // JLP_Gseg_ParamPanel
#include "jlp_gdev_wxwid.h"
#include "jlp_wxlogbook.h"         // JLP_wxLogbook

/*******************************************************************************
* Constructor as a subwindow from wxFrame:
*******************************************************************************/
JLP_Gseg_ParamPanel::JLP_Gseg_ParamPanel( wxFrame *frame, 
                                        JLP_wxLogbook *logbook)
                                        : wxPanel( frame)
{
int nparams;
wxString input_filename;

// Transform coma into point for numbers:
setlocale(LC_NUMERIC, "C");

initialized = 0;

jlp_logbook = logbook;

// Initialize private variables:
 Init_GSEG_SETTINGS_with_default_values(GsegSet1);
 Init_GSEG_PARAM_from_SETTINGS(GsegParam1, GsegSet1, &nparams);

 ParamPanel_Setup();

 initialized = 1234;

// Initialize panel with current settings:
  ParamPanel_SetValues(GsegSet1);

return;
}
/************************************************************************
* Destructor 
*************************************************************************/
JLP_Gseg_ParamPanel::~JLP_Gseg_ParamPanel()
{
// Free Memory:

Close();
}
/**********************************************************************
* Display/Save string to logbook
***********************************************************************/
int JLP_Gseg_ParamPanel::GsegPP_WriteToLogbook(wxString str1, 
                                               bool save_to_file0)
{
int status = -1;

if(initialized == 1234 && jlp_logbook != NULL) {
 status = jlp_logbook->WriteToLogbook(str1, save_to_file0);
 }

return(status);
}
