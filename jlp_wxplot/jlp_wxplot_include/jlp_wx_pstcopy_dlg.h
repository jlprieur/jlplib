/******************************************************************************
* jlp_wx_pstcopy_dlg.h
* To make a postscript copy of the displayed image on the screen 
*
* Author:      JLP 
* Version:     28/01/2009
******************************************************************************/
#ifndef jlp_wx_pstcopy_dlg_h    // sentry 
#define jlp_wx_pstcopy_dlg_h

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif

#include "wx/image.h"
#include "wx/file.h"
#include "wx/filename.h"
#include "wx/mstream.h"
#include "wx/wfstream.h"
#include "wx/quantize.h"
#include "wx/dcmemory.h"         // wxMemoryDC

/********************************************************************
* Class JLP_PstCopy_Dlg
*********************************************************************/

class JLP_PstCopy_Dlg: public wxDialog
{
public:

// Constructor:
     JLP_PstCopy_Dlg(wxFrame *parent, const wxString &title);
// Destructor: 
    ~JLP_PstCopy_Dlg(){
       };

   int  MySetupMenu();

// Handling events:
   void OnOKButton( wxCommandEvent &event );
   void OnCancelButton( wxCommandEvent &event );
   void OnChangeParam( wxCommandEvent& event );

// Accessors:
   int RetrieveData(int* tex_fonts0, int* black_and_white0, 
                    int* high_res0, int* lut_scale0)
   {
   *tex_fonts0 = (TeX_fonts1 == true) ? 1 : 0;
   *black_and_white0 = (black_and_white1 == true) ? 1 : 0;
   *high_res0 = (high_resolution1 == true) ? 1 : 0;
   *lut_scale0 = (lut_scale1 == true) ? 1 : 0;
   return(0);
   }

private:
    bool TeX_fonts1, black_and_white1, high_resolution1, lut_scale1;
    wxCheckBox *TexFonts_CheckBox, *BlackWhite_CheckBox;
    wxCheckBox *HighRes_CheckBox, *LutScale_CheckBox;

    DECLARE_EVENT_TABLE()
};

#endif               // EOF sentry
