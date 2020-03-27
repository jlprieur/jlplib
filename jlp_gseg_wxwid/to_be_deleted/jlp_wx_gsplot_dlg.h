/******************************************************************************
* jlp_wx_gsplot_dlg.h
*
* To plot measurements on a popup dialog frame 
*
* Author:   JLP 
* Version:  28/12/2016
******************************************************************************/
#ifndef jlp_wx_gsplot_dlg_h    // sentry 
#define jlp_wx_gsplot_dlg_h

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

#include "jlp_wx_gspanel.h"    // JLP_wxGsegPanel class

/********************************************************************
* Class JLP_wx_GsegPlot_Dlg
*********************************************************************/

class JLP_wx_GsegPlot_Dlg: public wxDialog
{
public:

// Constructor:
     JLP_wx_GsegPlot_Dlg(double *xplot0, double *yplot0, int nplot0, 
                    const wxString title0, const wxString xlabel0,
                    const wxString ylabel0);

// Destructor: 
    ~JLP_wx_GsegPlot_Dlg(){MyFreeMemory();};

    void PanelSetup();
    void InitPlot();
    void MyFreeMemory();

// Handling events:
     void OnOKButton( wxCommandEvent &event );
     void OnCancelButton( wxCommandEvent &event );

private:

  wxString title1, xlabel1, ylabel1;
  wxStaticText *m_StaticText;
  JLP_wxGsegPanel *m_GraphicPanel;
  wxButton *m_OKButton, *m_CancelButton;
  double *xplot1, *yplot1;
  int nplot1;

  DECLARE_EVENT_TABLE()
};

#endif               // EOF sentry
