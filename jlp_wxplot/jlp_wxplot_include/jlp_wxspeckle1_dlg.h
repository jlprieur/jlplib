/******************************************************************************
* jlp_wxspeckle1_dlg.h
* To process measurements and save them to Latex file 
*
* Author:   JLP 
* Version:  08/01/2015
******************************************************************************/
#ifndef jlp_wxspeckle1_h    // sentry 
#define jlp_wxspeckle1_h

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif

#include "wx/image.h"
#include "wx/file.h"
#include "wx/textfile.h"
#include "wx/filename.h"
#include "wx/mstream.h"
#include "wx/wfstream.h"
#include "wx/quantize.h"
#include "wx/dcmemory.h"         // wxMemoryDC

/********************************************************************
* Class JLP_wxSpeckle1_Dlg
*********************************************************************/

class JLP_wxSpeckle1_Dlg: public wxDialog
{
public:

// Constructor:
     JLP_wxSpeckle1_Dlg(wxFrame *parent, const wxString logbook,
                        const wxString original_fits_fname, 
                        const wxString processed_fits_fname, 
                        const wxString &title, 
                        const wxSize size1);

// Destructor: 
    ~JLP_wxSpeckle1_Dlg(){};

// Handling events:
     void OnOKProcessButton( wxCommandEvent &event );
     void OnCancelButton( wxCommandEvent &event );

private:
    wxButton *m_OKProcessOldButton, *m_OKProcessNewButton, *m_CancelButton;
    wxTextCtrl *m_Text; 
    wxString m_Latex_Filename, original_FITS_fname, processed_FITS_fname;
    bool not_processed;

    DECLARE_EVENT_TABLE()
};

#endif               // EOF sentry
