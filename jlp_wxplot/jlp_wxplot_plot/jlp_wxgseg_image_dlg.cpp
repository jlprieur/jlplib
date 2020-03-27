/******************************************************************************
* jlp_wxgseg_image_dlg.cpp
* Dialog box used for plotting images with GDev/GsegPlot
*
* Author:      JLP
* Version:     08/01/2018
******************************************************************************/

#include "stdlib.h" //exit(-1)
#include "jlp_wxgseg_image_dlg.h"

//#define DEBUG

BEGIN_EVENT_TABLE(JLP_wxGseg_Image_Dlg, wxDialog)
  EVT_COMBOBOX (ID_GSEG_CMB_GRAPHIC_TYPE, JLP_wxGseg_Image_Dlg::OnSelectGraphicType)
  EVT_COMBOBOX (ID_GSEG_CMB_PLOT_TYPE, JLP_wxGseg_Image_Dlg::OnSelectGsegPlotType)
  EVT_COMBOBOX (ID_GSEG_CMB_CONTOURS, JLP_wxGseg_Image_Dlg::OnSelectContours)
  EVT_COMBOBOX (ID_GSEG_CMB_PEN_WIDTH, JLP_wxGseg_Image_Dlg::OnSelectPenWidth)
  EVT_CHECKBOX (ID_GSEG_X_REVERSED, JLP_wxGseg_Image_Dlg::OnChangeParam)
  EVT_CHECKBOX (ID_GSEG_Y_REVERSED, JLP_wxGseg_Image_Dlg::OnChangeParam)
  EVT_CHECKBOX (ID_GSEG_Z_REVERSED, JLP_wxGseg_Image_Dlg::OnChangeParam)
  EVT_TEXT     (ID_GSEG_XLABEL, JLP_wxGseg_Image_Dlg::OnChangeParam)
  EVT_TEXT     (ID_GSEG_YLABEL, JLP_wxGseg_Image_Dlg::OnChangeParam)
  EVT_TEXT     (ID_GSEG_ZLABEL, JLP_wxGseg_Image_Dlg::OnChangeParam)
  EVT_TEXT     (ID_GSEG_TITLE, JLP_wxGseg_Image_Dlg::OnChangeParam)
  EVT_BUTTON   (ID_GSEG_OK, JLP_wxGseg_Image_Dlg::OnOKButton)
  EVT_BUTTON   (ID_GSEG_CANCEL, JLP_wxGseg_Image_Dlg::OnCancelButton)
END_EVENT_TABLE()

/**************************************************************************
* Constructor:
*
* INPUT:
* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
*
* contours option:
* 0="none"
* 1="black 2D contours"
* 2="white 2D contours"
* 3="auto-colored 2D contours"
* 4="3D contours"
*
**************************************************************************/
JLP_wxGseg_Image_Dlg::JLP_wxGseg_Image_Dlg(wxFrame *parent, const wxString &title,
                               const int gdev_graphic_type0,
                               const int contours_option0)
        : wxDialog(parent, -1, title, wxPoint(400,100), wxDefaultSize,
                   wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
int status = 0;
wxString err_msg;

/* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
*/
// Load input values to private parameters:
  gdev_graphic_type1 = gdev_graphic_type0;
  contours_option1 = contours_option0;

// Default values:
  x_rev1 = 0;
  y_rev1 = 0;
  z_rev1 = 0;
  pen_width1 = 1;
  strcpy(xlabel1, "");
  strcpy(ylabel1, "");
  strcpy(zlabel1, "");
  strcpy(title1, "");

/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*/
/* axis_type:
* 1="linear"   (points, histogram, contour, and color plot types)
* 2="semilogx" (points plot type only)
* 3="semilogy"  (points plot type only)
* 4="loglog"  (points plot type only)
* 5="polar" (points plot type only)
* 6="3d" (points, contour, color, and mesh plot types)
*/
 
  switch(gdev_graphic_type1) {
// 1 = jlp_splot_curves
// 2 = jlp_splot_images
// 3 = wx_scrolled/jlp_splot_images
    case 1:
      status = -1;
      break;
    case 2:
    case 3:
      gseg_plot_type1 = 4;  // 4="color"
      axis_type_val1 = 1;   // 1="linear"
      break;
    default:
// 4 = gsegraf_2d_curves
    case 4:
      status = -1;
      break;
// 5 = gsegraf_2d_images
    case 5:
      gseg_plot_type1 = 4;  // 4="color"
      axis_type_val1 = 1;   // 1="linear"
      if(contours_option1 > 0) gseg_plot_type1 = 3;  // 3="contour"
      break;
// 6 = gsegraf_3d_curves
    case 6:
      status = -1;
      break;
// 7 = gsegraf_3d_images
    case 7:
      gseg_plot_type1 = 4;  // 4="color"
      axis_type_val1 = 6;   // 6="3d"
      if(contours_option1 > 0) gseg_plot_type1 = 3;  // 3="contour"
      break;
// 8 = gsegraf_polar_curve
    case 8:
      status = -1;
      break;
  }
  SetAxisType();

// To avoid initialization problems with Windows:
// (An event is sent to "ChangeParameters"
//  as soon as a Text control is created...)
  initialized = 0;

if(status == -1){
  err_msg.Printf(wxT("Invalid input graphic_type: %d\n"), gdev_graphic_type1);
  wxMessageBox(err_msg, "JLP_wxGseg_Image_Dlg", wxICON_ERROR);
  exit(-1);
  }

  MySetupMenu();

  initialized = 1234;

 CheckAndUpdateParameters();

return;
}
/*************************************************************************
*
************************************************************************/
int JLP_wxGseg_Image_Dlg::MySetupMenu()
{
wxBoxSizer *hsizer1, *hsizer10, *hsizer2, *hsizer20, *hsizer3, *hsizer4;
wxBoxSizer *gdev_params_sizer, *gseg_params_sizer, *topsizer;
wxString empty_string;

topsizer = new wxBoxSizer( wxVERTICAL );

// First row:
hsizer1 = new wxBoxSizer( wxHORIZONTAL);

// **************** GDev parameters ***********************************
 gdev_params_sizer = new wxStaticBoxSizer(wxVERTICAL, this,
                                        _T("GDev parameters"));
 hsizer10 = new wxBoxSizer(wxHORIZONTAL);

/* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
*/
 ParCmb_graphic_type.label = wxT("GDev graphic type :");
 ParCmb_graphic_type.nchoices = 4;
 ParCmb_graphic_type.choices[0] = wxT("2=jlp_splot images");
 ParCmb_graphic_type.choices[1] = wxT("3=wx_scrolled/jlp_splot images");
 ParCmb_graphic_type.choices[2] = wxT("5=gsegraf 2d images");
 ParCmb_graphic_type.choices[3] = wxT("7=gsegraf 3d images");
 ParCmb_graphic_type.combo = new wxComboBox(this, ID_GSEG_CMB_GRAPHIC_TYPE,
                                            wxT(""),
                                            wxDefaultPosition, wxSize(200, 28),
                                            ParCmb_graphic_type.nchoices,
                                            ParCmb_graphic_type.choices);
 hsizer10->Add(new wxStaticText(this, -1, ParCmb_graphic_type.label),
                  0, wxLEFT | wxRIGHT | wxALIGN_CENTRE_VERTICAL, 20);
 hsizer10->Add(ParCmb_graphic_type.combo);
 gdev_params_sizer->Add(hsizer10, 0, wxALL, 20);
 hsizer1->Add(gdev_params_sizer, 0, wxALL, 10);
 topsizer->Add(hsizer1, 0, wxALL, 10);

// Second row:
hsizer2 = new wxBoxSizer( wxHORIZONTAL);

// **************** Gsegraf parameters ***********************************
 gseg_params_sizer = new wxStaticBoxSizer(wxVERTICAL, this,
                                        _T("Gsegraf parameters"));

 hsizer20 = new wxBoxSizer(wxHORIZONTAL);

// gseg_plot_type
/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*/
 ParCmb_gseg_plot_type.label = wxT("Gseg plot type :");
 ParCmb_gseg_plot_type.nchoices = 3;
 ParCmb_gseg_plot_type.choices[0] = wxT("contour");
 ParCmb_gseg_plot_type.choices[1] = wxT("color");
 ParCmb_gseg_plot_type.choices[2] = wxT("mesh");
 ParCmb_gseg_plot_type.combo = new wxComboBox(this, ID_GSEG_CMB_PLOT_TYPE,
                                            wxT(""),
                                            wxDefaultPosition, wxSize(140, 28),
                                            ParCmb_gseg_plot_type.nchoices,
                                            ParCmb_gseg_plot_type.choices);
 hsizer20->Add(new wxStaticText(this, -1, ParCmb_gseg_plot_type.label),
                  0, wxRIGHT | wxALIGN_CENTRE_VERTICAL, 20);
 hsizer20->Add(ParCmb_gseg_plot_type.combo);

// ****************** option for contours ***********************************
/* contours option:
* 0="none"
* 1="black 2D contours"
* 2="white 2D contours"
* 3="auto-colored 2D contours"
* 4="3D contours"
*/

 ParCmb_contours.label = wxT("Contours :");
 ParCmb_contours.nchoices = 5;
 ParCmb_contours.choices[0] = wxT("None");
 ParCmb_contours.choices[1] = wxT("Black 2D contours");
 ParCmb_contours.choices[2] = wxT("White 2D contours");
 ParCmb_contours.choices[3] = wxT("Auto-colored 2D contours");
 ParCmb_contours.choices[4] = wxT("3D contours");
 ParCmb_contours.combo = new wxComboBox(this, ID_GSEG_CMB_CONTOURS,
                                        wxT(""),
                                        wxDefaultPosition, wxSize(140, 28),
                                        ParCmb_contours.nchoices,
                                        ParCmb_contours.choices);
 hsizer20->Add(new wxStaticText(this, -1, ParCmb_contours.label),
                  0, wxRIGHT | wxLEFT | wxALIGN_CENTRE_VERTICAL, 20);
 hsizer20->Add(ParCmb_contours.combo);


 gseg_params_sizer->Add(hsizer20, 0, wxALL, 20);
 hsizer2->Add(gseg_params_sizer, 0, wxALL, 10);

// Add both static sizers:
 topsizer->Add(hsizer2, 0, wxALIGN_CENTER|wxALL, 10);

// Third row:
 hsizer3 = new wxBoxSizer( wxHORIZONTAL);

/* pen_width
*/

 ParCmb_pen_width.label = wxT("Pen width :");
 ParCmb_pen_width.nchoices = 5;
 ParCmb_pen_width.choices[0] = wxT("1");
 ParCmb_pen_width.choices[1] = wxT("2");
 ParCmb_pen_width.choices[2] = wxT("3");
 ParCmb_pen_width.choices[3] = wxT("4");
 ParCmb_pen_width.choices[4] = wxT("5");
 ParCmb_pen_width.combo = new wxComboBox(this, ID_GSEG_CMB_PEN_WIDTH,
                                        wxT(""),
                                        wxDefaultPosition, wxSize(60, 28),
                                        ParCmb_pen_width.nchoices,
                                        ParCmb_pen_width.choices);
 hsizer3->Add(new wxStaticText(this, -1, ParCmb_pen_width.label),
                  0, wxRIGHT | wxALIGN_CENTRE_VERTICAL, 20);
 hsizer3->Add(ParCmb_pen_width.combo, 0, wxRIGHT | wxALIGN_CENTRE_VERTICAL, 20);

// Xreversed check box:
 Xrev_CheckBox = new wxCheckBox(this, ID_GSEG_X_REVERSED, wxT("Reversed X"));
 hsizer3->Add(Xrev_CheckBox, 0, wxALL, 20);

// Yreversed check box:
 Yrev_CheckBox = new wxCheckBox(this, ID_GSEG_Y_REVERSED, wxT("Reversed Y"));
 hsizer3->Add(Yrev_CheckBox, 0, wxALL, 20);

// Zreversed check box:
 Zrev_CheckBox = new wxCheckBox(this, ID_GSEG_Z_REVERSED, wxT("Reversed Z"));
 hsizer3->Add(Zrev_CheckBox, 0, wxALL, 20);

 topsizer->Add(hsizer3, 0, wxALIGN_CENTER|wxALL, 10);

// Fourth row:
 hsizer4 = new wxBoxSizer( wxHORIZONTAL);

// Create the text controls:
 empty_string = wxT("");
 XLabel_TextCtrl = new wxTextCtrl( this, ID_GSEG_XLABEL, empty_string);
 hsizer4->Add(new wxStaticText( this, wxID_ANY, _T("XLabel:") ),
                  0, wxALIGN_LEFT|wxALIGN_CENTRE_VERTICAL|wxLEFT|wxRIGHT, 10);
 hsizer4->Add(XLabel_TextCtrl, 0, wxRIGHT, 20);
 YLabel_TextCtrl = new wxTextCtrl( this, ID_GSEG_YLABEL, empty_string);
 hsizer4->Add(new wxStaticText( this, wxID_ANY, _T("YLabel:") ),
                  0, wxALIGN_LEFT|wxALIGN_CENTRE_VERTICAL|wxLEFT|wxRIGHT, 10);
 hsizer4->Add(YLabel_TextCtrl, 0, wxRIGHT, 20);
 ZLabel_TextCtrl = new wxTextCtrl( this, ID_GSEG_ZLABEL, empty_string);
 hsizer4->Add(new wxStaticText( this, wxID_ANY, _T("ZLabel:") ),
                  0, wxALIGN_LEFT|wxALIGN_CENTRE_VERTICAL|wxLEFT|wxRIGHT, 10);
 hsizer4->Add(ZLabel_TextCtrl, 0, wxRIGHT, 20);
 Title_TextCtrl = new wxTextCtrl( this, ID_GSEG_TITLE, empty_string);
 hsizer4->Add(new wxStaticText( this, wxID_ANY, _T("Title:") ),
                  0, wxALIGN_LEFT|wxALIGN_CENTRE_VERTICAL|wxLEFT|wxRIGHT, 10);
 hsizer4->Add(Title_TextCtrl, 0, wxRIGHT, 20);
 topsizer->Add(hsizer4, 0, wxALIGN_CENTER|wxALL, 10);

// Last row:
wxBoxSizer *button_sizer = new wxBoxSizer( wxHORIZONTAL );

//create two buttons that are horizontally unstretchable,
  // with an all-around border with a width of 10 and implicit top alignment
 button_sizer->Add(
    new wxButton(this, ID_GSEG_OK, _T("OK") ), 0, wxALL, 10);

 button_sizer->Add(
   new wxButton(this, ID_GSEG_CANCEL, _T("Cancel") ), 0, wxALIGN_CENTER|wxALL, 10);

  //create a sizer with no border and centered horizontally
  topsizer->Add(button_sizer, 0, wxALIGN_CENTER);

  SetSizer(topsizer);      // use the sizer for layout

  topsizer->SetSizeHints( this );   // set size hints to honour minimum size

// Set current values:
 Xrev_CheckBox->SetValue(x_rev1);
 Yrev_CheckBox->SetValue(y_rev1);
 Zrev_CheckBox->SetValue(z_rev1);

return(0);
}
/**************************************************************************
* Handle "OK" button:
**************************************************************************/
void JLP_wxGseg_Image_Dlg::OnOKButton( wxCommandEvent& WXUNUSED(event) )
{
 if(initialized != 1234) return;

// Close dialog and return status = 0:
  EndModal(0);
}
/**************************************************************************
* Handle "Cancel" button:
**************************************************************************/
void JLP_wxGseg_Image_Dlg::OnCancelButton( wxCommandEvent& WXUNUSED(event) )
{
 if(initialized != 1234) return;

// Close dialog and return status = 1:
  EndModal(1);
}
/**************************************************************************
* Handle check box changes
*
**************************************************************************/
void JLP_wxGseg_Image_Dlg::OnChangeParam( wxCommandEvent& event )
{
wxString str1;

 if(initialized != 1234) return;

 switch (event.GetId()) {
   case ID_GSEG_X_REVERSED :
    x_rev1 = Xrev_CheckBox->GetValue();
    break;
   case ID_GSEG_Y_REVERSED :
    y_rev1 = Yrev_CheckBox->GetValue();
    break;
   case ID_GSEG_Z_REVERSED :
    z_rev1 = Zrev_CheckBox->GetValue();
    break;
   case ID_GSEG_XLABEL :
    str1 = XLabel_TextCtrl->GetValue();
    strcpy(xlabel1, (const char *)str1.mb_str());
    break;
   case ID_GSEG_YLABEL :
    str1 = YLabel_TextCtrl->GetValue();
    strcpy(ylabel1, (const char *)str1.mb_str());
    break;
   case ID_GSEG_ZLABEL :
    str1 = ZLabel_TextCtrl->GetValue();
    strcpy(zlabel1, (const char *)str1.mb_str());
    break;
   case ID_GSEG_TITLE :
    str1 = Title_TextCtrl->GetValue();
    strcpy(title1, (const char *)str1.mb_str());
    break;
  }

return;
}
/**************************************************************************
* Handle "graphic_type" combobox:
*
* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
*
**************************************************************************/
void JLP_wxGseg_Image_Dlg::OnSelectGraphicType(wxCommandEvent& event)
{
int sel0;
 if(initialized != 1234) return;

/*
 ParCmb_graphic_type.choices[0] = wxT("2=jlp_splot images");
 ParCmb_graphic_type.choices[1] = wxT("3=wx_scrolled/jlp_splot images");
 ParCmb_graphic_type.choices[2] = wxT("5=gsegraf 2d images");
 ParCmb_graphic_type.choices[3] = wxT("7=gsegraf 3d images");
*/

 sel0 = ParCmb_graphic_type.combo->GetSelection() + 1;
 switch(sel0) {
   case 0:
     gdev_graphic_type1 = 2; 
     break;
   case 1:
     gdev_graphic_type1 = 3; 
     break;
   default:
   case 2:
     gdev_graphic_type1 = 5; 
     break;
   case 3:
     gdev_graphic_type1 = 7; 
     break;
   }

// Set corresponding contours option if needed:
 if(gdev_graphic_type1 != 5 && gdev_graphic_type1 != 7) contours_option1 = 0;

 CheckAndUpdateParameters();

return;
}
/**************************************************************************
* Handle "gseg_plot_type" combobox:
*
* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*
**************************************************************************/
void JLP_wxGseg_Image_Dlg::OnSelectGsegPlotType(wxCommandEvent& event)
{
 if(initialized != 1234) return;

// 0 -> 3
 gseg_plot_type1 = ParCmb_gseg_plot_type.combo->GetSelection() + 3;

// Set corresponding contours option if needed:
 if(gseg_plot_type1 != 3) contours_option1 = 0;

 CheckAndUpdateParameters();

return;
}
/**************************************************************************
*
**************************************************************************/
void JLP_wxGseg_Image_Dlg::CheckAndUpdateParameters()
{
/* gseg_plot_type:
* 3="contour"
* 4="color"
* 5="mesh"
*/
if(gseg_plot_type1 == 3) {
// 2D contours:
//  gseg_graphic_type = 5 (gsegraf_2d_images):  2D contours
  if(gdev_graphic_type1 == 5
      && (contours_option1 == 0 || contours_option1 == 4 )) {
    contours_option1 = 1;
// 3D contours:
//  gseg_graphic_type = 7 (gsegraf_3d_images):  3D contours
  } else if(gdev_graphic_type1 == 7 && contours_option1 != 4) {
    contours_option1 = 4;
  }
}

/* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
*/

// Set axis type to "3d" automatically if 3d_images/3d_curves are selected:
 SetAxisType();

// Change contour option if needed:
if(gdev_graphic_type1  == 5 ){
  if(contours_option1 > 3) contours_option1 = 1;
  } else if (gdev_graphic_type1  == 7 ){
  if(contours_option1 > 0) contours_option1 = 4;
  }

#ifdef DEBUG
printf("CheckAndUpdateParameters/plot_type=%d graphic_type=%d contours_option=%d axis_type=%d\n",
       gseg_plot_type1, gdev_graphic_type1, contours_option1, axis_type_val1);
#endif

// Update all the combo boxes:
 ParCmb_gseg_plot_type.combo->SetSelection(gseg_plot_type1 - 3);
 ParCmb_contours.combo->SetSelection(contours_option1);
 ParCmb_pen_width.combo->SetSelection(pen_width1 - 1);
/*
 ParCmb_graphic_type.choices[0] = wxT("2=jlp_splot images");
 ParCmb_graphic_type.choices[1] = wxT("3=wx_scrolled/jlp_splot images");
 ParCmb_graphic_type.choices[2] = wxT("5=gsegraf 2d images");
 ParCmb_graphic_type.choices[3] = wxT("7=gsegraf 3d images");
*/

 switch(gdev_graphic_type1) {
   case 2:
     ParCmb_graphic_type.combo->SetSelection(0);
     break;
   case 3:
     ParCmb_graphic_type.combo->SetSelection(1);
     break;
   default:
   case 5:
     ParCmb_graphic_type.combo->SetSelection(2);
     break;
   case 7:
     ParCmb_graphic_type.combo->SetSelection(3);
     break;
   }

return;
}
/**************************************************************************
* Handle "contours" combobox:
*
* contours option:
* 0="none"
* 1="black 2D contours"
* 2="white 2D contours"
* 3="auto-colored 2D contours"
* 4="3D contours"
*
**************************************************************************/
void JLP_wxGseg_Image_Dlg::OnSelectContours(wxCommandEvent& event)
{
 if(initialized != 1234) return;

 contours_option1 = ParCmb_contours.combo->GetSelection();

// Set plot type if contours_options1 needs it
if(contours_option1 > 0) {
/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*/
  gseg_plot_type1 = 3;
 }

 CheckAndUpdateParameters();

return;
}
/**************************************************************************
* Handle "pen_width" combobox:
*
**************************************************************************/
void JLP_wxGseg_Image_Dlg::OnSelectPenWidth(wxCommandEvent& event)
{
 if(initialized != 1234) return;

 pen_width1 = ParCmb_pen_width.combo->GetSelection() + 1;

return;
}
/**************************************************************************
* axis_type:
* 1="linear"   (points, histogram, contour, and color plot types)
* 2="semilogx" (points plot type only)
* 3="semilogy"  (points plot type only)
* 4="loglog"  (points plot type only)
* 5="polar" (points plot type only)
* 6="3d" (points, contour, color, and mesh plot types)
*
**************************************************************************/
void JLP_wxGseg_Image_Dlg::SetAxisType()
{
 if(initialized != 1234) return;

/* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
*/
// Set axis type to "3d" if 3d_curves/3d_images if "3d" was selected:
if(gdev_graphic_type1 == 7) { 
     axis_type_val1  = 6;
  } else {
     axis_type_val1  = 1;
  }

// Convert integer axis type value to string
 DecodeAxisTypeValue();

return;
}
/*************************************************************************
* Convert integer axis type value to string
*
*************************************************************************/
void JLP_wxGseg_Image_Dlg::DecodeAxisTypeValue()
{
/* axis_type:
* 1="linear"   (points, histogram, contour, and color plot types)
* 2="semilogx" (points plot type only)
* 3="semilogy"  (points plot type only)
* 4="loglog"  (points plot type only)
* 5="polar" (points plot type only)
* 6="3d" (points, contour, color, and mesh plot types)
*/
  switch(axis_type_val1) {
    default:
    case 1:
      strcpy(axis_type1, "linear");
      break;
    case 2:
      strcpy(axis_type1, "semilogx");
      break;
    case 3:
      strcpy(axis_type1, "semilogy");
      break;
    case 4:
      strcpy(axis_type1, "loglog");
      break;
    case 5:
      strcpy(axis_type1, "polar");
      break;
    case 6:
      strcpy(axis_type1, "3d");
      break;

  }
return;
}
