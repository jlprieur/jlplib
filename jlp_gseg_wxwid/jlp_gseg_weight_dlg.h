/******************************************************************************
* jlp_gseg_weight_dlg.h
* To select the weight block intervals 
*
* Author:  JLP 
* Version: 05/10/2015
******************************************************************************/
#ifndef jlp_gseg_param_weight_dlg_h    // sentry 
#define jlp_gseg_param_weight_dlg_h

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif

#include "wx/filename.h"

#include "jlp_gsegset_defs.h"         // NBLK_MAX, GSEG_SETTINGS, ... 

/********************************************************************
* Class JLP_GsegWeight_Dlg
*********************************************************************/

class JLP_GsegWeight_Dlg: public wxDialog
{
public:

// Constructor:
   JLP_GsegWeight_Dlg(GSEG_SETTINGS Spea0);

// Destructor: 
   ~JLP_GsegWeight_Dlg(){
       };

// Handling events:
   void OnOKButton( wxCommandEvent &event );
   void OnCancelButton( wxCommandEvent &event );

// Accessors:
   int RetrieveData(int *Int1_0, int *Int2_0, double *Weight0) { 
    int i;
    for(i = 0; i < n1; i++) {
      Int1_0[i] = Int1[i];
      Int2_0[i] = Int2[i];
      Weight0[i] = Weight[i];
      }
    return(0);
    };

protected:
   void SetupPanel();
   void UpdatePanel(GSEG_SETTINGS Spea0);

private:
   int n1, initialized;
   int Int1[NBLK_MAX], Int2[NBLK_MAX]; 
   double Weight[NBLK_MAX];
   wxTextCtrl *ParCtrl_int1_blk[NBLK_MAX], *ParCtrl_int2_blk[NBLK_MAX];
   wxTextCtrl *ParCtrl_weight_blk[NBLK_MAX];

   DECLARE_EVENT_TABLE()
};

#endif               // EOF sentry
