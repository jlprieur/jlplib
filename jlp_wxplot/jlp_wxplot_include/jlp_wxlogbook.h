/****************************************************************************
* Name: jlp_wxlogbook.h JLP_wxLogbook class
* Purpose: display and process FITS images obtained with PISCO and PISCO2
* 
* JLP
* Version 03/01/2015
****************************************************************************/
#ifndef _jlp_wxlogbook__ 
#define _jlp_wxlogbook__ 

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif

#include "wx/tglbtn.h"
#include "wx/bookctrl.h"
#include "wx/imaglist.h"
#include "wx/cshelp.h"

#if wxUSE_TOOLTIPS
    #include "wx/tooltip.h"
#endif

//----------------------------------------------------------------------
// class definitions
//----------------------------------------------------------------------

class JLP_wxLogbook: public wxTextCtrl
{
public:

// In "jlp_wxlogbook.cpp":
    JLP_wxLogbook(wxWindow *window, const wxString title, const int iwidth, 
               const int iheight);
    ~JLP_wxLogbook();

    int SaveLogbook(wxString save_filename);
    void Clear();
    void Clean();
    int WriteToLogbook(wxString str1, bool save_to_file0);
    int BinariesSaveMeasurements(wxString original_fits_fname,
                                 wxString processed_fits_fname);

private:

// Logbook:
  wxString    m_LogString;

  DECLARE_EVENT_TABLE()
};

#endif
