/******************************************************************************
* jlp_wxgseg_curve_dlg.h
* To select graphic options for curves 
*
* Author:      JLP 
* Version:     18/03/2019
******************************************************************************/
#ifndef jlp_wxgseg_curve_dlg_h    // sentry 
#define jlp_wxgseg_curve_dlg_h

#include "jlp_wxgseg_defs.h"      // wxwidgets, NCHOICES and JLP_ComboBox 
#include "jlp_gseg_axis_data1.h"  // jlp_gseg_copy_axis_data()

/********************************************************************
* Class JLP_wxGseg_Curve_Dlg
*********************************************************************/

class JLP_wxGseg_Curve_Dlg: public wxDialog
{
public:

// Constructor:
     JLP_wxGseg_Curve_Dlg(wxFrame *parent, const wxString &title,
                          const int gdev_graphic_type2, 
                          GSEG_AXIS_DATA gseg_axdata2); 
// Destructor: 
    ~JLP_wxGseg_Curve_Dlg(){
       };

   int  MySetupMenu();

// Handling events:
   void OnOKButton( wxCommandEvent &event );
   void OnCancelButton( wxCommandEvent &event );
   void OnChangeParam( wxCommandEvent& event );
   void OnSelectGraphicType(wxCommandEvent& event);
   void OnSelectGsegPlotType(wxCommandEvent& event);
   void OnSelectPenWidth(wxCommandEvent& event);
   void OnSelectAxisType(wxCommandEvent& event);
   void CheckAndUpdateParameters();
   void DecodeAxisTypeValue();

// Accessors:
   int RetrieveData(int *gdev_graphic_type0, GSEG_AXIS_DATA *gseg_axdata0,
                    int *gseg_plot_type0, int *pen_width0);

private:
  GSEG_AXIS_DATA gseg_axdata1;
  int initialized, gdev_graphic_type1, gseg_plot_type1, axis_type_val1;
  int x_rev1, y_rev1, z_rev1, grid1, equal_scale1, pen_width1;
  char axis_type1[64], xlabel1[128], ylabel1[128], zlabel1[128], title1[128];
  JLP_ComboBox ParCmb_graphic_type, ParCmb_gseg_plot_type, ParCmb_axis_type;
  JLP_ComboBox ParCmb_pen_width;
  wxTextCtrl *XLabel_TextCtrl, *YLabel_TextCtrl, *ZLabel_TextCtrl;
  wxTextCtrl *Title_TextCtrl, *PlotDevice_TextCtrl;
  wxCheckBox *Grid_CheckBox, *EqualScale_CheckBox;
  wxCheckBox *Xrev_CheckBox, *Yrev_CheckBox, *Zrev_CheckBox;

  DECLARE_EVENT_TABLE()
};

#endif               // EOF sentry
