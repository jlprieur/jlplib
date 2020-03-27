/******************************************************************************
* jlp_wxplot_frame.h
*
* To plot measurements on a separate frame 
* for images (3D or contours)
* or for slices
*
* Author:   JLP
* Version:  24/08/2017
******************************************************************************/
#ifndef jlp_wxplot_frame_h    // sentry 
#define jlp_wxplot_frame_h

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif

#include "wx/file.h"
#include "wx/textfile.h"
#include "wx/filename.h"
#include "wx/mstream.h"
#include "wx/wfstream.h"
#include "wx/quantize.h"

#include "jlp_wx_gpanel.h"    // JLP_wxGraphicPanel class

/********************************************************************
* Class JLP_wxPlot_Frame
*********************************************************************/

class JLP_wxPlot_Frame: public wxFrame
{
public:

// Constructor for gseg/image configuration:
   JLP_wxPlot_Frame(double *dble_image0, int nx0, int ny0, 
                    const wxString title0, const wxString xlabel0,
                    const wxString ylabel0, const wxString zlabel0,
                    const char *option0, const int contours_option0);

// Constructor for jlp_splot/slice configuration:
   JLP_wxPlot_Frame(double *xplot0, double *yplot0, double *errorx0,
                    double *errory0, int nplot0,
                    const wxString title0, const wxString xlabel0,
                    const wxString ylabel0);

// Destructor: 
   ~JLP_wxPlot_Frame(){MyFreeMemory();};

   void PanelSetup();
   void InitPlotForImage();
   void InitPlotForSlice();
   void MyFreeMemory();

private:
  int initialized, contours_option1;
  wxString title1, xlabel1, ylabel1, zlabel1;
  JLP_wxGraphicPanel *m_GraphicPanel;
  double *dble_image1;
  double *xplot1, *yplot1, *errorx1, *errory1;
  int nx1, ny1, nplot1;
  char option1[64];

};

#endif               // EOF sentry
