/******************************************************************************
* jlp_wxgplot_dlg.cpp
*
* To plot measurements on a popup dialog frame
* for graphic curves (slices for instance)
*
* To plot slice or auxiliary data on popup windows
*
* Author:   JLP 
* Version:  28/12/2015
******************************************************************************/
#include "jlp_wxgplot_dlg.h"

//*************************************************************************
enum
{
   ID_PLOT_OK = 3920,
   ID_PLOT_CANCEL     = 3921
};

BEGIN_EVENT_TABLE(JLP_wxGPlot_Dlg, wxDialog)
EVT_BUTTON  (ID_PLOT_OK, JLP_wxGPlot_Dlg::OnOKButton)
EVT_BUTTON  (ID_PLOT_CANCEL, JLP_wxGPlot_Dlg::OnCancelButton)
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
JLP_wxGPlot_Dlg::JLP_wxGPlot_Dlg(double *xplot0, double *yplot0, 
                                 double *errorx0, double *errory0, int nplot0, 
                               const wxString title0, const wxString xlabel0,
                               const wxString ylabel0)
                  : wxDialog(NULL, -1, title0, wxDefaultPosition, 
                   wxSize(800,600), wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
int i;

m_GraphicPanel = NULL;
m_OKButton = NULL; 
m_CancelButton = NULL; 
m_StaticText = NULL;
xplot1 = NULL; 
yplot1 = NULL; 
errorx1 = NULL;
errory1 = NULL;
initialized = 0;

nplot1 = nplot0;
title1 = title0;
xlabel1 = xlabel0;
ylabel1 = ylabel0;
xplot1 = new double[nplot1];
yplot1 = new double[nplot1];
errorx1 = new double[nplot1];
errory1 = new double[nplot1];
for(i = 0; i < nplot1; i++) {
  xplot1[i] = xplot0[i];
  yplot1[i] = yplot0[i];
  errorx1[i] = errorx0[i];
  errory1[i] = errory0[i];
  }

PanelSetup();

InitPlot();

initialized = 1234;

return;
}
/**************************************************************************
* 
**************************************************************************/
void JLP_wxGPlot_Dlg::PanelSetup()
{
wxSize size1;
int iwidth, iheight;

 wxBoxSizer *topsizer = new wxBoxSizer( wxVERTICAL );

// Text on the line with the buttons that will remain empty,
// since the coordinates are now displayed in m_GraphicPanel...
 m_StaticText = new wxStaticText(this, -1, wxT("") ); 

// Create the display panel: 
 size1 = GetClientSize();
 iwidth = size1.x;
// Leave some space for the OK/Cancel buttons
 iheight = size1.y - 40;
// Dialog frames cannot have a status bar:
 m_GraphicPanel = new JLP_wxGraphicPanel((wxFrame *)this, wxID_ANY, 
                                          NULL, 20, 20, iwidth, iheight);
 topsizer->Add(m_GraphicPanel, 1, wxEXPAND);

 wxBoxSizer *button_sizer = new wxBoxSizer( wxHORIZONTAL );

// Create two buttons that are horizontally unstretchable, 
// with an all-around border with a width of 10 and implicit top alignment
 m_OKButton = new wxButton(this, ID_PLOT_OK, _T("OK") ); 

 button_sizer->Add( m_OKButton, 0, wxALL, 10);

 m_CancelButton = new wxButton(this, ID_PLOT_CANCEL, _T("Cancel") ); 
 button_sizer->Add( m_CancelButton, 0, wxALL, 10);

// Coordinates at the end:
 button_sizer->Add( m_StaticText, 0, wxALL, 10);

//create a sizer with no border and centered horizontally
 topsizer->Add(button_sizer, 0, wxALIGN_CENTER);

 SetSizer(topsizer);      // use the sizer for layout

//  topsizer->SetSizeHints( this );   // set size hints to honour minimum size

return;
}
/**************************************************************************
* 
**************************************************************************/
void JLP_wxGPlot_Dlg::MyFreeMemory()
{

 if(m_GraphicPanel != NULL) m_GraphicPanel->Destroy();
 m_GraphicPanel = NULL;
 if(m_OKButton != NULL) delete m_OKButton; 
 m_OKButton = NULL;
 if(m_CancelButton != NULL) delete m_CancelButton; 
 m_CancelButton = NULL;
 if(m_StaticText != NULL) delete m_StaticText;
 m_StaticText = NULL;
 if(xplot1 != NULL) delete[] xplot1; 
 xplot1 = NULL;
 if(yplot1 != NULL) delete[] yplot1; 
 yplot1 = NULL;
 if(errorx1 != NULL) delete[] errorx1; 
 errorx1 = NULL;
 if(errory1 != NULL) delete[] errory1; 
 errory1 = NULL;

return;
}
/**************************************************************************
* Init plot parameters when creating this class
* 
**************************************************************************/
void JLP_wxGPlot_Dlg::InitPlot()
{
char nchar_type[40], pcolor[32], plot_fname[128];
char xlabel[40], ylabel[40], title[80];
int xgrid_is_wanted, ygrid_is_wanted, jlp_axes_are_wanted;
int reset_first;


  strcpy(pcolor, "Black");
  strcpy(nchar_type, "L");
  strcpy(plot_fname, "");
  reset_first = 0;
  m_GraphicPanel->wxGP_LoadPlotDataToPrivateParameters(xplot1, yplot1, 
                                    errorx1, errory1, nplot1, nchar_type, 
                                    pcolor, plot_fname, reset_first);

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
  m_GraphicPanel->wxGP_LoadPlotSettings(xlabel, ylabel, title, xgrid_is_wanted,
                                        ygrid_is_wanted, jlp_axes_are_wanted,
                                        0, 0, 0, 0, 0);

// Call Newplot():
  m_GraphicPanel->wxGP_PlotToDrawingDisplay();

return;
}
/**************************************************************************
* Handle "OK"/"Process" button:
* This routine is called twice: once for selecting old/new file,
* and a second time for saving confirmation when processing has been done)
**************************************************************************/
void JLP_wxGPlot_Dlg::OnOKButton( wxCommandEvent& event )
{
  if(initialized != 1234) return;

// Close dialog and return status = wxID_OK:
  EndModal(wxID_OK); 

return;
}
/**************************************************************************
* Handle "Cancel" button:
**************************************************************************/
void JLP_wxGPlot_Dlg::OnCancelButton( wxCommandEvent& WXUNUSED(event) )
{
  if(initialized != 1234) return;

// Close dialog and return status = 1:
  EndModal(1); 

return;
}
