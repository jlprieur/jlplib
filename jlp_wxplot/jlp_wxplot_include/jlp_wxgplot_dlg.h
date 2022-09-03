/******************************************************************************
* jlp_wxgplot_dlg.h
*
* To plot measurements on a popup dialog frame 
* for graphic curves (slices for instance)
*
* Author:   JLP 
* Version:  28/12/2015
******************************************************************************/
#ifndef jlp_wxgplot_dlg_h    // sentry 
#define jlp_wxgplot_dlg_h

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
* Class JLP_wxGPlot_Dlg
*********************************************************************/

class JLP_wxGPlot_Dlg: public wxDialog
{
public:

// Constructor:
     JLP_wxGPlot_Dlg(double *xplot0, double *yplot0, 
                     double *errorx0, double *errory0, int nplot0,
                     const wxString title0, const wxString xlabel0,
                     const wxString ylabel0);

// Destructor: 
    ~JLP_wxGPlot_Dlg(){MyFreeMemory();};

    void PanelSetup();
    void InitPlot();
    void MyFreeMemory();

// Handling events:
     void OnSaveButton( wxCommandEvent &event );
     void OnCloseButton( wxCommandEvent &event );

private:
  int initialized;
  wxString title1, xlabel1, ylabel1;
  wxStaticText *m_StaticText;
  JLP_wxGraphicPanel *m_GraphicPanel;
  wxButton *m_CloseButton, *m_SaveButton;
  double *xplot1, *yplot1, *errorx1, *errory1;
  int nplot1;

  DECLARE_EVENT_TABLE()
};

#endif               // EOF sentry
