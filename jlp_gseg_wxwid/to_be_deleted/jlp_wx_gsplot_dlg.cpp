/******************************************************************************
* jlp_wxplot_dlg.cpp
* To plot slice or auxiliary data on popup windows
*
* Author:   JLP 
* Version:  28/12/2015
******************************************************************************/
#include "jlp_wx_gsplot_dlg.h"

//*************************************************************************
enum
{
   ID_PLOT_OK = 3920,
   ID_PLOT_CANCEL     = 3921
};

BEGIN_EVENT_TABLE(JLP_wx_GsegPlot_Dlg, wxDialog)
EVT_BUTTON  (ID_PLOT_OK, JLP_wx_GsegPlot_Dlg::OnOKButton)
EVT_BUTTON  (ID_PLOT_CANCEL, JLP_wx_GsegPlot_Dlg::OnCancelButton)
END_EVENT_TABLE()

/**************************************************************************
* Constructor:
*
* INPUT:
*  original_fits_fname: FITS file obtained from the observations
*                        (full name with directory and extension)
*  processed_fits_fname: processed FITS file used for the measurements
*                        (name without directory and extension)
*
**************************************************************************/
JLP_wx_GsegPlot_Dlg::JLP_wx_GsegPlot_Dlg(double *xplot0, double *yplot0, int nplot0, 
                               const wxString title0, const wxString xlabel0,
                               const wxString ylabel0)
                  : wxDialog(NULL, -1, title0, wxDefaultPosition, 
                   wxSize(800,600), wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
int i;

PanelSetup();

nplot1 = nplot0;
xplot1 = new double[nplot1];
yplot1 = new double[nplot1];
for(i = 0; i < nplot1; i++) xplot1[i] = xplot0[i];
for(i = 0; i < nplot1; i++) yplot1[i] = yplot0[i];

title1 = title0;
xlabel1 = xlabel0;
ylabel1 = ylabel0;
InitPlot();

return;
}
/**************************************************************************
* 
**************************************************************************/
void JLP_wx_GsegPlot_Dlg::PanelSetup()
{
wxSize size1;
int iwidth, iheight;

 wxBoxSizer *topsizer = new wxBoxSizer( wxVERTICAL );

 m_StaticText = new wxStaticText(this, -1, wxT("x=12.3456 y=12.3456 z=123.456") ); 

// Create the display panel: 
 size1 = GetClientSize();
 iwidth = size1.x;
// Leave some space for the OK/Cancel buttons
 iheight = size1.y - 40;
// Dialog frames cannot have a status bar:
 m_GraphicPanel = new JLP_wxGsegPanel((wxFrame *)this, wxID_ANY, 
                                         NULL, 20, 20, iwidth, iheight);
 topsizer->Add(m_GraphicPanel, 1, wxEXPAND);

 wxBoxSizer *button_sizer = new wxBoxSizer( wxHORIZONTAL );


//create two buttons that are horizontally unstretchable, 
// with an all-around border with a width of 10 and implicit top alignment
 m_OKButton = new wxButton(this, ID_PLOT_OK, 
                                     _T("Process and save to file") ); 

 button_sizer->Add( m_OKButton, 0, wxALIGN_RIGHT|wxALL, 10);

 m_CancelButton = new wxButton(this, ID_PLOT_CANCEL, _T("Cancel") ); 
 button_sizer->Add( m_CancelButton, 0, wxALIGN_RIGHT|wxALL, 10);

// Coordinates at the end:
 button_sizer->Add( m_StaticText, 0, wxALIGN_RIGHT|wxALL, 10);

  //create a sizer with no border and centered horizontally
  topsizer->Add(button_sizer, 0, wxALIGN_CENTER);

  SetSizer(topsizer);      // use the sizer for layout

//  topsizer->SetSizeHints( this );   // set size hints to honour minimum size

return;
}
/**************************************************************************
* 
**************************************************************************/
void JLP_wx_GsegPlot_Dlg::MyFreeMemory()
{
 delete m_GraphicPanel;
 delete m_OKButton; 
 delete m_CancelButton; 
 delete m_StaticText;
 delete[] xplot1; 
 delete[] yplot1; 
return;
}
/**************************************************************************
* 
**************************************************************************/
void JLP_wx_GsegPlot_Dlg::InitPlot()
{
int ncurves_maxi, nplot_maxi, nout_maxi;
char nchar_type[40], pcolor[32], plot_fname[128];
char xlabel[40], ylabel[40], title[80];
int xgrid_is_wanted, ygrid_is_wanted, jlp_axes_are_wanted;
double wavel_start, wavel_step;

// Init plot parameters:
// InitPlotData(int nmaxi, int ncurves_maxi, int nout_maxi)
  nplot_maxi = nplot1 + 1;
  ncurves_maxi = 2;
  nout_maxi = 64;
// Dummy values here since Muse cube is not there...
  wavel_start = 0.;
  wavel_step = 1.;
  m_GraphicPanel->InitPlotData(nplot_maxi, ncurves_maxi, nout_maxi,
                               wavel_start, wavel_step);

  strcpy(pcolor, "Black");
  strcpy(nchar_type, "L");
  strcpy(plot_fname, "");
  m_GraphicPanel->LoadPlotData(xplot1, yplot1, nplot1, nchar_type, pcolor, 
                               plot_fname, 0);

  strcpy(title, (const char*)title1.mb_str());
  strcpy(xlabel, (const char*)xlabel1.mb_str());
  strcpy(ylabel, (const char*)ylabel1.mb_str());

// no grid, but JLP_axes:
// JLP2015: grid does not seem to be working...
  xgrid_is_wanted = 0;
  ygrid_is_wanted = 0;
// JLP axes are better ?
  jlp_axes_are_wanted = 0;

// iplan=0 x1=0 x2=0 y1=0 y2=0
  m_GraphicPanel->LoadPlotSettings(xlabel, ylabel, title, xgrid_is_wanted,
                                   ygrid_is_wanted, jlp_axes_are_wanted,
                                   0, 0, 0, 0, 0);

// Call Newplot():
  m_GraphicPanel->PlotToDrawingDisplay();

return;
}
/**************************************************************************
* Handle "OK"/"Process" button:
* This routine is called twice: once for selecting old/new file,
* and a second time for saving confirmation when processing has been done)
**************************************************************************/
void JLP_wx_GsegPlot_Dlg::OnOKButton( wxCommandEvent& event )
{

// Close dialog and return status = wxID_OK:
  EndModal(wxID_OK); 

return;
}
/**************************************************************************
* Handle "Cancel" button:
**************************************************************************/
void JLP_wx_GsegPlot_Dlg::OnCancelButton( wxCommandEvent& WXUNUSED(event) )
{
// Close dialog and return status = 1:
  EndModal(1); 
}
