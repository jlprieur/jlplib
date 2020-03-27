/******************************************************************************
* jlp_wxgseg_curve_dlg.cpp
* Dialog box used for plotting curves with GDev/GsegPlot 
*
* Author:      JLP
* Version:     08/01/2018
******************************************************************************/
#include "jlp_wxgseg_curve_dlg.h"

BEGIN_EVENT_TABLE(JLP_wxGseg_Curve_Dlg, wxDialog)
  EVT_COMBOBOX(ID_GSEG_CMB_GRAPHIC_TYPE, JLP_wxGseg_Curve_Dlg::OnSelectGraphicType)
  EVT_COMBOBOX(ID_GSEG_CMB_PLOT_TYPE, JLP_wxGseg_Curve_Dlg::OnSelectGsegPlotType)
  EVT_COMBOBOX(ID_GSEG_CMB_AXIS_TYPE, JLP_wxGseg_Curve_Dlg::OnSelectAxisType)
  EVT_COMBOBOX(ID_GSEG_CMB_PEN_WIDTH, JLP_wxGseg_Curve_Dlg::OnSelectPenWidth)
  EVT_CHECKBOX (ID_GSEG_GRID, JLP_wxGseg_Curve_Dlg::OnChangeParam)
  EVT_CHECKBOX (ID_GSEG_EQUAL_SCALE, JLP_wxGseg_Curve_Dlg::OnChangeParam)
  EVT_CHECKBOX (ID_GSEG_X_REVERSED, JLP_wxGseg_Curve_Dlg::OnChangeParam)
  EVT_CHECKBOX (ID_GSEG_Y_REVERSED, JLP_wxGseg_Curve_Dlg::OnChangeParam)
  EVT_CHECKBOX (ID_GSEG_Z_REVERSED, JLP_wxGseg_Curve_Dlg::OnChangeParam)
  EVT_TEXT (ID_GSEG_XLABEL, JLP_wxGseg_Curve_Dlg::OnChangeParam)
  EVT_TEXT (ID_GSEG_YLABEL, JLP_wxGseg_Curve_Dlg::OnChangeParam)
  EVT_TEXT (ID_GSEG_ZLABEL, JLP_wxGseg_Curve_Dlg::OnChangeParam)
  EVT_TEXT (ID_GSEG_TITLE, JLP_wxGseg_Curve_Dlg::OnChangeParam)
  EVT_TEXT (ID_GSEG_PLOT_DEVICE, JLP_wxGseg_Curve_Dlg::OnChangeParam)
  EVT_BUTTON  (ID_GSEG_OK, JLP_wxGseg_Curve_Dlg::OnOKButton)
  EVT_BUTTON  (ID_GSEG_CANCEL, JLP_wxGseg_Curve_Dlg::OnCancelButton)
END_EVENT_TABLE()

// #define DEBUG 1

/**************************************************************************
* Constructor:
*
**************************************************************************/
JLP_wxGseg_Curve_Dlg::JLP_wxGseg_Curve_Dlg(wxFrame *parent, 
                          const wxString &title, const int gdev_graphic_type2,
                          GSEG_AXIS_DATA gseg_axdata2)
        : wxDialog(parent, -1, title, wxPoint(400,100), wxDefaultSize,
                   wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{

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
  gdev_graphic_type1 = gdev_graphic_type2; 

// Copy the input gseg_axdata2 structure to private gseg_axdata1 structure 
  jlp_gseg_copy_axis_data(&gseg_axdata1, gseg_axdata2);

// Default values:
  if(!strcmp(gseg_axdata1.grid, "on")) grid1 = 1;
   else grid1 = 0;
  x_rev1 = gseg_axdata1.reversed_axis[0];
  y_rev1 = gseg_axdata1.reversed_axis[1];
  z_rev1 = gseg_axdata1.reversed_axis[2];
/* DEBUG
printf("Input values: gseg_data1/ x_rev1=%d y_rev1=%d z_rev1=%d\n", 
        gseg_axdata1.reversed_axis[0], gseg_axdata1.reversed_axis[1],
        gseg_axdata1.reversed_axis[2]); 
*/

  if(!strcmp(gseg_axdata1.axis_scale, "equal")) equal_scale1 = 1;
   else equal_scale1 = 0;
  strcpy(xlabel1, gseg_axdata1.xlabel);
  strcpy(ylabel1, gseg_axdata1.ylabel);
  strcpy(zlabel1, gseg_axdata1.zlabel);
  strcpy(title1, gseg_axdata1.title);

// Pen width: gspdata0->style_size = 1;
  pen_width1 = 1;

// Default values for axis_type_val1:
/* axis_type:
* 1="linear"   (points, histogram, contour, and color plot types)
* 2="semilogx" (points plot type only)
* 3="semilogy"  (points plot type only)
* 4="loglog"  (points plot type only)
* 5="polar" (points plot type only)
* 6="3d" (points, contour, color, and mesh plot types)
*/
  if(!strcmp(gseg_axdata1.axis_type, "polar")) 
     axis_type_val1 = 5;   // 5="polar"
  else if(!strcmp(gseg_axdata1.axis_type, "3d")) 
     axis_type_val1 = 6;   // 6="3d"
  else
     axis_type_val1 = 1;   // 1="linear"
   

// Default values for gseg_plot_type1:
/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*/
  switch(gdev_graphic_type1) {
// 1 = jlp_splot_curves
// 2 = jlp_splot_images
// 3 = wx_scrolled/jlp_splot_images
// 4 = gsegraf_2d_curves
// 6 = gsegraf_3d_curves
// 8 = gsegraf_polar_curve
    default:
    case 1:
    case 2:
    case 3:
    case 4:
    case 8:
//  gspdata0->gseg_plot_type = 1 ("points");
      gseg_plot_type1 = 1;  // 1="points"
      break;
// 5 = gsegraf_2d_images
// 7 = gsegraf_3d_images
    case 5:
    case 7:
//  gspdata0->gseg_plot_type = 4 ("color");
      gseg_plot_type1 = 4;  // 4="color"
      break;
  }

// Convert integer axis type value to string
  DecodeAxisTypeValue();

// To avoid initialization problems with Windows:
// (An event is sent to "ChangeParameters"
//  as soon as a Text control is created...)
  initialized = 0;

  MySetupMenu();

  initialized = 1234;

 CheckAndUpdateParameters();

return;
}
/*************************************************************************
*
************************************************************************/
int JLP_wxGseg_Curve_Dlg::MySetupMenu()
{
wxBoxSizer *hsizer1, *hsizer10, *hsizer11;
wxBoxSizer *hsizer2, *hsizer20, *hsizer3, *hsizer4;
wxBoxSizer *gdev_params_sizer, *gseg_params_sizer, *topsizer;
wxString empty_string;

topsizer = new wxBoxSizer( wxVERTICAL );

// First row:
hsizer1 = new wxBoxSizer( wxHORIZONTAL);

// **************** GDev parameters ***********************************
 gdev_params_sizer = new wxStaticBoxSizer(wxVERTICAL, this,
                                        _T("GDev parameters"));
 hsizer10 = new wxBoxSizer(wxHORIZONTAL);
 hsizer11 = new wxBoxSizer(wxHORIZONTAL);

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
 ParCmb_graphic_type.nchoices = 8;
 ParCmb_graphic_type.choices[0] = wxT("1=jlp_splot curves");
 ParCmb_graphic_type.choices[1] = wxT("2=jlp_splot images");
 ParCmb_graphic_type.choices[2] = wxT("3=wx_scrolled/jlp_splot images");
 ParCmb_graphic_type.choices[3] = wxT("4=gsegraf 2d curves");
 ParCmb_graphic_type.choices[4] = wxT("5=gsegraf 2d images");
 ParCmb_graphic_type.choices[5] = wxT("6=gsegraf 3d curves");
 ParCmb_graphic_type.choices[6] = wxT("7=gsegraf 3d images");
 ParCmb_graphic_type.choices[7] = wxT("8=gsegraf polar curve");
 ParCmb_graphic_type.combo = new wxComboBox(this, ID_GSEG_CMB_GRAPHIC_TYPE,
                                            wxT(""),
                                            wxDefaultPosition, wxSize(200, 28),
                                            ParCmb_graphic_type.nchoices,
                                            ParCmb_graphic_type.choices);
 hsizer10->Add(new wxStaticText(this, -1, ParCmb_graphic_type.label),
                  0, wxLEFT | wxRIGHT, 20);
 hsizer10->Add(ParCmb_graphic_type.combo);
 gdev_params_sizer->Add(hsizer10, 0, wxALL, 20);

// Create the plotdev text controls:
 empty_string = wxT("");
 PlotDevice_TextCtrl = new wxTextCtrl( this, ID_GSEG_PLOT_DEVICE, empty_string);
 hsizer11->Add(new wxStaticText( this, wxID_ANY, _T("GDev plot device:") ),
                  0, wxLEFT|wxRIGHT, 20);
 hsizer11->Add(PlotDevice_TextCtrl, 0, wxLEFT | wxRIGHT | wxALIGN_CENTRE_VERTICAL, 20);
 gdev_params_sizer->Add(hsizer11, 0, wxALL, 20);

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
 ParCmb_gseg_plot_type.nchoices = 5;
 ParCmb_gseg_plot_type.choices[0] = wxT("points");
 ParCmb_gseg_plot_type.choices[1] = wxT("histogram");
 ParCmb_gseg_plot_type.choices[2] = wxT("contour");
 ParCmb_gseg_plot_type.choices[3] = wxT("color");
 ParCmb_gseg_plot_type.choices[4] = wxT("mesh");
 ParCmb_gseg_plot_type.combo = new wxComboBox(this, ID_GSEG_CMB_PLOT_TYPE,
                                            wxT(""),
                                            wxDefaultPosition, wxSize(140, 28),
                                            ParCmb_gseg_plot_type.nchoices,
                                            ParCmb_gseg_plot_type.choices);
 hsizer20->Add(new wxStaticText(this, -1, ParCmb_gseg_plot_type.label),
                  0, wxRIGHT | wxALIGN_CENTRE_VERTICAL, 20);
 hsizer20->Add(ParCmb_gseg_plot_type.combo);

// ****************** axis_type ***********************************
/* axis_type:
* 1="linear"   (points, histogram, contour, and color plot types)
* 2="semilogx" (points plot type only)
* 3="semilogy"  (points plot type only)
* 4="loglog"  (points plot type only)
* 5="polar" (points plot type only)
* 6="3d" (points, contour, color, and mesh plot types)
*/

// axis_type
 ParCmb_axis_type.label = wxT("Axis type :");
 ParCmb_axis_type.nchoices = 6;
 ParCmb_axis_type.choices[0] = wxT("linear");
 ParCmb_axis_type.choices[1] = wxT("semi-logx");
 ParCmb_axis_type.choices[2] = wxT("semi-logy");
 ParCmb_axis_type.choices[3] = wxT("loglog");
 ParCmb_axis_type.choices[4] = wxT("polar");
 ParCmb_axis_type.choices[5] = wxT("3d");
 ParCmb_axis_type.combo = new wxComboBox(this, ID_GSEG_CMB_AXIS_TYPE,
                                            wxT(""),
                                            wxDefaultPosition, wxSize(140, 28),
                                            ParCmb_axis_type.nchoices,
                                            ParCmb_axis_type.choices);
 hsizer20->Add(new wxStaticText(this, -1, ParCmb_axis_type.label),
                  0, wxRIGHT | wxLEFT | wxALIGN_CENTRE_VERTICAL, 20);
 hsizer20->Add(ParCmb_axis_type.combo);

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

// Grid check box:
 Grid_CheckBox = new wxCheckBox(this, ID_GSEG_GRID, wxT("Grid"));
 hsizer3->Add(Grid_CheckBox, 0, wxALL, 20);

// Equal scale check box:
 EqualScale_CheckBox = new wxCheckBox(this, ID_GSEG_EQUAL_SCALE,
                                     wxT("Equal scale"));
 hsizer3->Add(EqualScale_CheckBox, 0, wxALL, 20);

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

return(0);
}
/**************************************************************************
* Handle "OK" button:
**************************************************************************/
void JLP_wxGseg_Curve_Dlg::OnOKButton( wxCommandEvent& WXUNUSED(event) )
{
 if(initialized != 1234) return;

// Close dialog and return status = 0:
  EndModal(0);
}
/**************************************************************************
* Handle "Cancel" button:
**************************************************************************/
void JLP_wxGseg_Curve_Dlg::OnCancelButton( wxCommandEvent& WXUNUSED(event) )
{
 if(initialized != 1234) return;

// Close dialog and return status = 1:
  EndModal(1);
}
/**************************************************************************
* Handle check box changes
*
**************************************************************************/
void JLP_wxGseg_Curve_Dlg::OnChangeParam( wxCommandEvent& event )
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
   case ID_GSEG_GRID :
    grid1 = Grid_CheckBox->GetValue();
    break;
   case ID_GSEG_EQUAL_SCALE :
    equal_scale1 = EqualScale_CheckBox->GetValue();
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
void JLP_wxGseg_Curve_Dlg::OnSelectGraphicType(wxCommandEvent& event)
{
 if(initialized != 1234) return;

 gdev_graphic_type1 = ParCmb_graphic_type.combo->GetSelection() + 1;

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
void JLP_wxGseg_Curve_Dlg::OnSelectGsegPlotType(wxCommandEvent& event)
{
 if(initialized != 1234) return;

 gseg_plot_type1 = ParCmb_gseg_plot_type.combo->GetSelection() + 1;

 CheckAndUpdateParameters();

return;
}
/**************************************************************************
*
**************************************************************************/
void JLP_wxGseg_Curve_Dlg::CheckAndUpdateParameters()
{
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
if(gdev_graphic_type1  == 6 || gdev_graphic_type1  == 7) {
  axis_type_val1 = 6; // "3d"
  ParCmb_axis_type.combo->SetSelection(axis_type_val1 - 1);
// Convert integer axis type value to string
  DecodeAxisTypeValue();
  }

/* DEBUG
printf("DEBUG/ after: plot_type=%d graphic_type=%d\n",
       gseg_plot_type1, gdev_graphic_type1);
*/

// Update all the combo boxes:
 ParCmb_graphic_type.combo->SetSelection(gdev_graphic_type1 - 1);
 ParCmb_gseg_plot_type.combo->SetSelection(gseg_plot_type1 - 1);
 ParCmb_axis_type.combo->SetSelection(axis_type_val1 - 1);
 ParCmb_pen_width.combo->SetSelection(pen_width1 - 1);

// Update the labels:
 XLabel_TextCtrl->ChangeValue(xlabel1);
 YLabel_TextCtrl->ChangeValue(ylabel1);
 ZLabel_TextCtrl->ChangeValue(zlabel1);
 Title_TextCtrl->ChangeValue(title1);

// Update the checkboxes:  
 Xrev_CheckBox->SetValue(x_rev1);
 Yrev_CheckBox->SetValue(y_rev1);
 Zrev_CheckBox->SetValue(z_rev1);
 Grid_CheckBox->SetValue(grid1);
 EqualScale_CheckBox->SetValue(equal_scale1);


return;
}
/**************************************************************************
* Handle "pen_width" combobox:
*
**************************************************************************/
void JLP_wxGseg_Curve_Dlg::OnSelectPenWidth(wxCommandEvent& event)
{
 if(initialized != 1234) return;

 pen_width1 = ParCmb_pen_width.combo->GetSelection() + 1;

return;
}
/**************************************************************************
* Handle "axis_type" combobox:
* axis_type:
* 1="linear"   (points, histogram, contour, and color plot types)
* 2="semilogx" (points plot type only)
* 3="semilogy"  (points plot type only)
* 4="loglog"  (points plot type only)
* 5="polar" (points plot type only)
* 6="3d" (points, contour, color, and mesh plot types)
*
**************************************************************************/
void JLP_wxGseg_Curve_Dlg::OnSelectAxisType(wxCommandEvent& event)
{
 if(initialized != 1234) return;

 axis_type_val1 = ParCmb_axis_type.combo->GetSelection() + 1;

// Convert integer axis type value to string
 DecodeAxisTypeValue();

// Set gdev_graphic_type automatically to 3d_curves/3d_images if "3d" was selected:
if(axis_type_val1  == 6) {
  if(gdev_graphic_type1 == 4) gdev_graphic_type1 = 6;
  if(gdev_graphic_type1 == 5) gdev_graphic_type1 = 7;
  }

 CheckAndUpdateParameters();

return;
}
/*************************************************************************
* Convert integer axis type value to string
*
*************************************************************************/
void JLP_wxGseg_Curve_Dlg::DecodeAxisTypeValue()
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
/*************************************************************************
* Retrieve all the modified parameters at the end, 
* when the user closes the dialog window 
*
*************************************************************************/
int JLP_wxGseg_Curve_Dlg::RetrieveData(int *gdev_graphic_type0, 
                                       GSEG_AXIS_DATA *gseg_axdata0,
                                       int *gseg_plot_type0, int *pen_width0)
/* Old version:
int *gseg_plot_type0, char *axis_type0, int *x_rev0, int *y_rev0, int *z_rev0,
int* grid0, int* equal_scale0, int* pen_width0, char *xlabel0,
char *ylabel0, char *zlabel0, char *title0)
*/
   {
   if(initialized != 1234) return(-1);
   *gdev_graphic_type0 = gdev_graphic_type1;

// Reversed axes:
   gseg_axdata1.reversed_axis[0] = x_rev1;
   gseg_axdata1.reversed_axis[1] = y_rev1;
   gseg_axdata1.reversed_axis[2] = z_rev1;
/* DEBUG
printf("Output values: x_rev1=%d y_rev1=%d z_rev1=%d\n", 
        x_rev1, y_rev1, z_rev1); 
*/

// Equal scale if needed:
    if(equal_scale1 == 1)
      strcpy(gseg_axdata1.axis_scale, "equal");
    else
      strcpy(gseg_axdata1.axis_scale, "auto");

// Grid:
    if(grid1 == 1) {
      strcpy(gseg_axdata1.grid, "on1");
      gseg_axdata1.gridchar1 = 'l';
      gseg_axdata1.gridchar2 = 's';
      gseg_axdata1.gridcolor1 = 0x0F0F00FF;
    } else {
      strcpy(gseg_axdata1.grid, "off");
    }
// Labels:
   strcpy(gseg_axdata1.axis_type, axis_type1);
   strcpy(gseg_axdata1.xlabel, xlabel1);
   strcpy(gseg_axdata1.ylabel, ylabel1);
   strcpy(gseg_axdata1.zlabel, zlabel1);
   strcpy(gseg_axdata1.title, title1);

// Copy from private gseg_axdata1 to output gseg_axdata0 structure:
   jlp_gseg_copy_axis_data(gseg_axdata0, gseg_axdata1);

// Other parameters depending on gseg_plot_type:
   *gseg_plot_type0 = gseg_plot_type1;
   *pen_width0 = pen_width1;

/* Old version:
   strcpy(axis_type0, axis_type1);
   strcpy(xlabel0, xlabel1);
   strcpy(ylabel0, ylabel1);
   strcpy(zlabel0, zlabel1);
   strcpy(title0, title1);
// X_rev, Y_rev, Z_rev
   *x_rev0 = x_rev1;
   *y_rev0 = y_rev1;
   *z_rev0 = z_rev1;
// On/off grid
   *grid0 = grid1;
// On/off equal scale for X and Y axes
   *equal_scale0 = equal_scale1;
// Pen width:
   *pen_width0 = pen_width1;
*/
   return(0);
   }

