/******************************************************************************
* jlp_wx_gframe.cpp
* To plot spectra or auxiliary data on a (popup) separate window
*
* Author:   JLP 
* Version:  26/02/2016
******************************************************************************/
#include "jlp_wx_gframe.h"

//*************************************************************************
BEGIN_EVENT_TABLE(JLP_wxGraphicFrame, wxFrame)
END_EVENT_TABLE()

/**************************************************************************
* Constructor from VideoPanel:
*
* INPUT:
*  original_fits_fname: FITS file obtained from the observations
*                        (full name with directory and extension)
*  processed_fits_fname: processed FITS file used for the measurements
*                        (name without directory and extension)
*
**************************************************************************/
JLP_wxGraphicFrame::JLP_wxGraphicFrame(JLP_wxVideoPanel *jlp_video_panel0,
                                       const wxString title0, 
                                       const wxString xlabel0,
                                       const wxString ylabel0, 
                                       const int width0, const int height0)
   : wxFrame(NULL, wxID_ANY, title0, wxPoint(-1, -1), wxSize(width0, height0),
// | wxSTAY_ON_TOP
             wxCAPTION | wxMAXIMIZE_BOX | wxMINIMIZE_BOX 
             | wxRESIZE_BORDER | wxFRAME_TOOL_WINDOW)
{

// Set minimum size:
SetMinSize(wxSize(width0, height0));

title1 = title0;
xlabel1 = xlabel0;
ylabel1 = ylabel0;
wavel_start1 = 0.;
wavel_step1 = 1.;

m_GraphicPanel = NULL;
m_VideoPanel = jlp_video_panel0;
initialized = 0;

PanelSetup();

initialized = 1234;

return;
}
/**************************************************************************
* Minimal constructor (from scratch)
*
* INPUT:
*  original_fits_fname: FITS file obtained from the observations
*                        (full name with directory and extension)
*  processed_fits_fname: processed FITS file used for the measurements
*                        (name without directory and extension)
*
**************************************************************************/
JLP_wxGraphicFrame::JLP_wxGraphicFrame(const wxString title0, 
                                       const wxString xlabel0,
                                       const wxString ylabel0, 
                                       const int width0, const int height0)
   : wxFrame(NULL, wxID_ANY, title0, wxPoint(-1, -1), wxSize(width0, height0),
// | wxSTAY_ON_TOP
             wxCAPTION | wxMAXIMIZE_BOX | wxMINIMIZE_BOX 
             | wxRESIZE_BORDER | wxFRAME_TOOL_WINDOW)
{

// Set minimum size:
SetMinSize(wxSize(width0, height0));

title1 = title0;
xlabel1 = xlabel0;
ylabel1 = ylabel0;
wavel_start1 = 0.;
wavel_step1 = 1.;

m_GraphicPanel = NULL;
m_VideoPanel = NULL;
initialized = 0;

// Instantiate a new JLP_GraphicPanel object
PanelSetup();

initialized = 1234;

return;
}
/**************************************************************************
* 
**************************************************************************/
void JLP_wxGraphicFrame::PanelSetup()
{
int iwidth, iheight;

// Set background to white by default:
 SetBackgroundColour(wxColour(* wxWHITE));

 wxBoxSizer *topsizer = new wxBoxSizer( wxVERTICAL );

// Create the display panel: 
 GetClientSize(&iwidth, &iheight);

// This frame has not any logbook, so I use NULL: 
 m_GraphicPanel = new JLP_wxGraphicPanel((wxFrame *)this, wxID_ANY, 
                                         NULL, m_VideoPanel,
                                         0, 0, iwidth, iheight);
 topsizer->Add(m_GraphicPanel, 1, wxEXPAND);

// To set the minimum size that fits this setup:
 SetSizerAndFit(topsizer);      // use the sizer for layout

//  topsizer->SetSizeHints( this );   // set size hints to honour minimum size

// Link it to the video panel:
 if(m_VideoPanel != NULL) m_VideoPanel->ConnectSpectrumToVideoPanel(m_GraphicPanel);
return;
}
/**************************************************************************
* 
**************************************************************************/
void JLP_wxGraphicFrame::MyFreeMemory()
{
 if(m_GraphicPanel != NULL) delete m_GraphicPanel;
return;
}
/**************************************************************************
* 
**************************************************************************/
int JLP_wxGraphicFrame::LoadNewPlot(double *xplot0, double *yplot0, 
                                    double *errorx0, double *errory0, 
                                    int nplot0, const int reset_first0)
{
char nchar_type[40];
char xlabel[40], ylabel[40], title[80], plot_fname[128];
char pen_colour[64], pen_default_colour[64], backgd_colour[64];
int xgrid_is_wanted, ygrid_is_wanted, jlp_axes_are_wanted;
JLP_GDev_wxWID *m_GDev = m_GraphicPanel->Get_JLP_GDev_wxWID();

if((initialized != 1234) || (m_GraphicPanel == NULL)) return(-1); 

  strcpy(pen_colour, "Black");
  strcpy(pen_default_colour, "Black");
  strcpy(backgd_colour, "White");

  strcpy(nchar_type, "L");
  strcpy(plot_fname, "");
  m_GDev->Curves_LoadPlotDataToPrivateParameters(xplot0, yplot0, 
                                       errorx0, errory0, nplot0, 
                                       nchar_type, pen_colour, plot_fname, 
                                       reset_first0);

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
  m_GDev->Curves_LoadPlotSettings(xlabel, ylabel, title, 
                                   pen_colour, pen_default_colour,
                                   backgd_colour,
                                   xgrid_is_wanted, ygrid_is_wanted, 
                                   jlp_axes_are_wanted, 0, 0, 0, 0, 0);

// Call Newplot():
  m_GDev->PlotToDrawingDisplay();

return(0);
}
/*********************************************************************
* Interface with JLP_wxGraphicPanel
*********************************************************************/
int JLP_wxGraphicFrame::wxGF_LoadPlotDataToPrivateParameters0(double *xplot0, 
                                    double *yplot0, 
                                    const int npts0, const char *nchar_type, 
                                    const char *pcolor, const char *plot_fname,
                                    const int reset_first)
{
int status = -1;
JLP_GDev_wxWID *m_GDev = m_GraphicPanel->Get_JLP_GDev_wxWID();

if(m_GraphicPanel != NULL) 
 status = m_GDev->Curves_LoadPlotDataToPrivateParameters0(xplot0, yplot0, 
                                            npts0, nchar_type, pcolor, 
                                            plot_fname, reset_first);

return(status); 
}
/*********************************************************************
* Interface with JLP_wxGraphicPanel
*********************************************************************/
int JLP_wxGraphicFrame::wxGF_LoadPlotDataToPrivateParameters(double *xplot0, 
                                    double *yplot0, 
                                    double *errorx0, double *errory0, 
                                    const int npts0, const char *nchar_type, 
                                    const char *pcolor, const char *plot_fname,
                                    const int reset_first)
{
int status = -1;
JLP_GDev_wxWID *m_GDev = m_GraphicPanel->Get_JLP_GDev_wxWID();

if(m_GraphicPanel != NULL) 
 status = m_GDev->Curves_LoadPlotDataToPrivateParameters(xplot0, yplot0, 
                                            errorx0, errory0,
                                            npts0, nchar_type, pcolor, 
                                            plot_fname, reset_first);

return(status); 
}
/*********************************************************************
* Interface with JLP_wxGraphicPanel
*********************************************************************/
int JLP_wxGraphicFrame::wxGF_LoadPlotSettings(const char *xlabel, 
                           const char *ylabel, const char *title, 
                           const char *pen_colour, 
                           const char *pen_default_colour, 
                           const char *backgd_colour, 
                           const int xgrid_is_wanted, const int ygrid_is_wanted,
                           const int jlp_axes_are_wanted, const int iplan,
                           const double x1, const double x2,
                           const double y1, const double y2)
{
int status = -1;
JLP_GDev_wxWID *m_GDev = m_GraphicPanel->Get_JLP_GDev_wxWID();

if(m_GraphicPanel != NULL) 
 status = m_GDev->Curves_LoadPlotSettings(xlabel, ylabel, title, 
                                           pen_colour, pen_default_colour,
                                           backgd_colour,
                                           xgrid_is_wanted, ygrid_is_wanted,
                                           jlp_axes_are_wanted, iplan, 
                                           x1, x2, y1, y2);

return(status); 
}
/*********************************************************************
* Interface with JLP_wxGraphicPanel
*********************************************************************/
void JLP_wxGraphicFrame::wxGF_ClearDrawingDisplay()
{
JLP_GDev_wxWID *m_GDev = m_GraphicPanel->Get_JLP_GDev_wxWID();

if(m_GraphicPanel != NULL) m_GDev->ClearDrawingDisplay();

return; 
}
/*********************************************************************
* Interface with JLP_wxGraphicPanel
*********************************************************************/
void JLP_wxGraphicFrame::wxGF_PlotToDrawingDisplay()
{
JLP_GDev_wxWID *m_GDev = m_GraphicPanel->Get_JLP_GDev_wxWID();

if(m_GraphicPanel != NULL) m_GDev->PlotToDrawingDisplay();

return; 
}
