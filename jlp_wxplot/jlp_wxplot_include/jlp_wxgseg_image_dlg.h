/******************************************************************************
* jlp_wxgseg_image_dlg.h
* To select graphic options for curves 
*
* Author:      JLP 
* Version:     08/01/2017
******************************************************************************/
#ifndef jlp_wxgseg_image_dlg_h    // sentry 
#define jlp_wxgseg_image_dlg_h

#include "jlp_wxgseg_defs.h"  // wxwidgets, NCHOICES and JLP_ComboBox

/********************************************************************
* Class JLP_wxGseg_Image_Dlg
*********************************************************************/

class JLP_wxGseg_Image_Dlg: public wxDialog
{
public:

// Constructor:
     JLP_wxGseg_Image_Dlg(wxFrame *parent, const wxString &title,
                          const int gdev_graphic_type0, 
                          const int contours_option0);
// Destructor: 
    ~JLP_wxGseg_Image_Dlg(){
       };

   int  MySetupMenu();

// Handling events:
   void OnOKButton( wxCommandEvent &event );
   void OnCancelButton( wxCommandEvent &event );
   void OnChangeParam( wxCommandEvent& event );
   void OnSelectGraphicType(wxCommandEvent& event);
   void OnSelectGsegPlotType(wxCommandEvent& event);
   void OnSelectContours(wxCommandEvent& event);
   void OnSelectPenWidth(wxCommandEvent& event);
   void SetAxisType();
   void CheckAndUpdateParameters();
   void DecodeAxisTypeValue();

// Accessors:
   int RetrieveData(int *gdev_graphic_type0, int *gseg_plot_type0, 
                    char *axis_type0, int *x_rev0, int *y_rev0, int *z_rev0,
                    int* contours_option0, int* pen_width0,
                    char *xlabel0, char *ylabel0, char *zlabel0, char *title0)
   {
   if(initialized != 1234) return(-1);
   *gdev_graphic_type0 = gdev_graphic_type1;
   *gseg_plot_type0 = gseg_plot_type1;
   strcpy(axis_type0, axis_type1);
   strcpy(xlabel0, xlabel1);
   strcpy(ylabel0, ylabel1);
   strcpy(zlabel0, zlabel1);
   strcpy(title0, title1);
// X_rev, Y_rev, Z_rev
   *x_rev0 = x_rev1;
   *y_rev0 = y_rev1;
   *z_rev0 = z_rev1;
// Contours option:
   *contours_option0 = contours_option1;
// Pen width:
   *pen_width0 = pen_width1;
   return(0);
   }

private:
  int initialized, gdev_graphic_type1, gseg_plot_type1, axis_type_val1;
  int x_rev1, y_rev1, z_rev1, contours_option1, pen_width1;
  char axis_type1[64], xlabel1[128], ylabel1[128], zlabel1[128], title1[128];
  JLP_ComboBox ParCmb_graphic_type, ParCmb_gseg_plot_type;
  JLP_ComboBox ParCmb_contours, ParCmb_pen_width;
  wxTextCtrl *XLabel_TextCtrl, *YLabel_TextCtrl, *ZLabel_TextCtrl;
  wxTextCtrl *Title_TextCtrl;
  wxCheckBox *Xrev_CheckBox, *Yrev_CheckBox, *Zrev_CheckBox;

  DECLARE_EVENT_TABLE()
};

#endif               // EOF sentry
