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
   ID_PLOT_SAVE = 3920,
   ID_PLOT_CLOSE = 3921
};

BEGIN_EVENT_TABLE(JLP_wxGPlot_Dlg, wxDialog)
EVT_BUTTON  (ID_PLOT_SAVE, JLP_wxGPlot_Dlg::OnSaveButton)
EVT_BUTTON  (ID_PLOT_CLOSE, JLP_wxGPlot_Dlg::OnCloseButton)
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
m_SaveButton = NULL; 
m_CloseButton = NULL; 
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
 m_SaveButton = new wxButton(this, ID_PLOT_SAVE, _T("Save to JPEG, PNG, etc") ); 

 button_sizer->Add( m_SaveButton, 0, wxALL, 10);

 m_CloseButton = new wxButton(this, ID_PLOT_CLOSE, _T("Close") ); 
 button_sizer->Add( m_CloseButton, 0, wxALL, 10);

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
 if(m_SaveButton != NULL) delete m_SaveButton; 
 m_SaveButton = NULL;
 if(m_CloseButton != NULL) delete m_CloseButton; 
 m_CloseButton = NULL;
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
char nchar_type[40], plot_fname[128];
char xlabel[40], ylabel[40], title[80];
char pen_colour[64], pen_default_colour[64], backgd_colour[64];
int xgrid_is_wanted, ygrid_is_wanted, jlp_axes_are_wanted;
int reset_first;
JLP_GDev_wxWID *m_GDev = m_GraphicPanel->Get_JLP_GDev_wxWID();

  strcpy(pen_colour, "Black");
  strcpy(pen_default_colour, "Black");
  strcpy(backgd_colour, "White");
  strcpy(nchar_type, "L");
  strcpy(plot_fname, "");
  reset_first = 0;
  m_GDev->Curves_LoadPlotDataToPrivateParameters(xplot1, yplot1, 
                                    errorx1, errory1, nplot1, nchar_type, 
                                    pen_colour, plot_fname, reset_first);

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
  m_GDev->Curves_LoadPlotSettings(xlabel, ylabel, title, pen_colour, 
                                  pen_default_colour, backgd_colour, 
                                  xgrid_is_wanted, ygrid_is_wanted, 
                                  jlp_axes_are_wanted, 0, 0, 0, 0, 0);

// Call Newplot():
  m_GDev->PlotToDrawingDisplay();

return;
}
/**************************************************************************
* Handle "SAVE"/"Process" button:
* This routine is called twice: once for selecting old/new file,
* and a second time for saving confirmation when processing has been done)
**************************************************************************/
void JLP_wxGPlot_Dlg::OnSaveButton( wxCommandEvent& event )
{
JLP_GDev_wxWID *m_GDev = m_GraphicPanel->Get_JLP_GDev_wxWID();

  if(initialized != 1234) return;

// Save current bitmap to file in format JPEG, etc
  m_GDev->SaveGraphicToFile();
// Close dialog and return status = wxID_OK:
  EndModal(wxID_OK);
  MyFreeMemory();

return;
}
/**************************************************************************
* Handle "Close" button:
**************************************************************************/
void JLP_wxGPlot_Dlg::OnCloseButton( wxCommandEvent& WXUNUSED(event) )
{
  if(initialized != 1234) return;

// Close dialog and return status = 1 (for CANCEL):
//  EndModal(1); 
// Close dialog and return status = wxID_OK:
  EndModal(wxID_OK); 
  MyFreeMemory();

return;
}
