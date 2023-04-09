/******************************************************************************
* jlp_wxplot_frame.cpp
*
* To plot measurements on a separate frame
* for images (3D or contours)
*
* Author:   JLP 
* Version:  23/08/2017
******************************************************************************/
#include "jlp_wxplot_frame.h"

/**************************************************************************
* Constructor for gseg/image configuration:
*
* INPUT:
*   option0 : "3d", or "contours")
**************************************************************************/
JLP_wxPlot_Frame::JLP_wxPlot_Frame(double *dble_image0, int nx0, int ny0,
                                 const wxString title0, const wxString xlabel0,
                                 const wxString ylabel0, const wxString zlabel0,
                                 const char *option0, 
                                 const int contours_option0)
                  : wxFrame(NULL, -1, title0, wxDefaultPosition, 
                   wxSize(800,600), wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
int i;

m_GraphicPanel = NULL;
xplot1 = NULL;
yplot1 = NULL; 
errorx1 = NULL;
errory1 = NULL;
contours_option1 = contours_option0;
initialized = 0;

if(!strcmp(option0, "3d"))
 strcpy(option1, "3d");
else
 strcpy(option1, "contours");

nx1 = nx0;
ny1 = ny0;
title1 = title0;
xlabel1 = xlabel0;
ylabel1 = ylabel0;
zlabel1 = zlabel0;
dble_image1 = new double[nx1 * ny1];
for(i = 0; i < nx1 * ny1; i++) dble_image1[i] = dble_image0[i];

PanelSetup();

InitPlotForImage();

initialized = 1234;

return;
}
/**************************************************************************
* Constructor for jlp_splot/slice configuration:
*
* INPUT:
*
**************************************************************************/
JLP_wxPlot_Frame::JLP_wxPlot_Frame(double *xplot0, double *yplot0, 
                                   double *errorx0, double *errory0, int nplot0,
                                  const wxString title0, const wxString xlabel0,
                                  const wxString ylabel0)
                  : wxFrame(NULL, -1, title0, wxDefaultPosition,
                   wxSize(800,600), wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
int i;


m_GraphicPanel = NULL;
xplot1 = NULL;
yplot1 = NULL; 
errorx1 = NULL;
errory1 = NULL;
dble_image1 = NULL;
initialized = 0;

strcpy(option1, "slice");

nplot1 = nplot0;
title1 = title0;
xlabel1 = xlabel0;
ylabel1 = ylabel0;
zlabel1 = wxString(_T(""));
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

InitPlotForSlice();

// JP2023: to try to solve problem of black frames in Calern:
  m_GraphicPanel->SetBackgroundColour(wxColour(*wxWHITE));

initialized = 1234;

return;
}

/**************************************************************************
* 
**************************************************************************/
void JLP_wxPlot_Frame::PanelSetup()
{
wxSize size1;
int iwidth, iheight;

 wxBoxSizer *topsizer = new wxBoxSizer( wxVERTICAL );

// Create the display panel: 
 size1 = GetClientSize();
 iwidth = size1.x;
 iheight = size1.y;
// Here, use frames without a status bar:
 m_GraphicPanel = new JLP_wxGraphicPanel((wxFrame *)this, wxID_ANY, 
                                          NULL, 20, 20, iwidth, iheight);

 topsizer->Add(m_GraphicPanel, 1, wxEXPAND);

 SetSizer(topsizer);      // use the sizer for layout

//  topsizer->SetSizeHints( this );   // set size hints to honour minimum size

return;
}
/**************************************************************************
* 
**************************************************************************/
void JLP_wxPlot_Frame::MyFreeMemory()
{

 if(m_GraphicPanel != NULL) m_GraphicPanel->Destroy();
 m_GraphicPanel = NULL;
 if(dble_image1 != NULL) delete[] dble_image1; 
 dble_image1 = NULL;
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
* Init plot parameters when creating this class with image configuration
* 
**************************************************************************/
void JLP_wxPlot_Frame::InitPlotForImage()
{
int graphic_type0, status;

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
if(!strcmp(option1, "3d"))
  graphic_type0 = 7; 
else
  graphic_type0 = 5;

status = -1;
if(dble_image1 != NULL) {
 status = m_GraphicPanel->wxGP_LoadDbleImage1(dble_image1, nx1, ny1);
 }

// Load curve/image for Gseg plot to JLP_GDev_wxWID  Drawing panel 
 if(status == 0) {
  status = m_GraphicPanel->wxGP_InitializeGsegImagePlotWithPrivateData(
                                                        graphic_type0,
                                                        contours_option1);
  }

return;
}
/**************************************************************************
* Init plot parameters when creating this class with slice configuration
*
**************************************************************************/
void JLP_wxPlot_Frame::InitPlotForSlice()
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
  m_GDev->Curves_LoadPlotDataToPrivateParameters(xplot1, yplot1, errorx1,
                                             errory1, nplot1, nchar_type, 
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

