/******************************************************************************
* jlp_wx_patch.h
* To perform cosmetic corrections on images
*
* Author:      JLP 
* Version:     28/01/2009
******************************************************************************/
#ifndef jlp_wx_patch_h    // sentry 
#define jlp_wx_patch_h

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

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
* Class JLP_Patch_Dlg
*********************************************************************/

class JLP_Patch_Dlg: public wxDialog
{
public:

// Constructor:
     JLP_Patch_Dlg(wxFrame *parent, double xc0, double yc0,
                   double radius0, double radius_fact0, int poly_order0, 
                   double sigma_noise0, int nx0, int ny0, 
                   int polynomial_method, const wxString &title);
// Destructor: 
    ~JLP_Patch_Dlg(){
       };

// Handling events:
     void OnOKButton( wxCommandEvent &event );
     void OnCancelButton( wxCommandEvent &event );
     void OnNewTryButton( wxCommandEvent &event );
     void OnChangeParam( wxCommandEvent& event );
     void OnCheckProfile( wxCommandEvent& event );
     void OnCheckPolynomial( wxCommandEvent& event );

// Accessors:
   int RetrieveData(double *xc2, double *yc2, double *radius2, 
                    double *radius_fact2, int *poly_order2, 
                    double *sigma_noise2, int *polynomial_method2) {
       *xc2 = xc; *yc2 = yc; *radius2 = radius; *radius_fact2 = radius_fact;
       *poly_order2 = poly_order, *sigma_noise2 = sigma_noise;
       *polynomial_method2 = polynomial_method;
       return(0);
       }

protected:
   int DataIsOK() {
       if(xc < 0. || xc > nx || yc < 0. || yc > ny) return(false); 
       if(xc - radius_fact * radius < 0. || xc + radius_fact * radius > nx
          || yc - radius_fact * radius < 0. || yc + radius_fact * radius > ny)
         return(false); 
       if(radius_fact < 1.1 || radius_fact > 5) return(false); 
       if(poly_order < 0. || poly_order > 7) return(false); 
       if(sigma_noise < 0. || sigma_noise > 10.) return(false); 
       return(true);
       }

private:
    wxTextCtrl *TextCtrl_x, *TextCtrl_y; 
    wxTextCtrl *TextCtrl_radius, *TextCtrl_radius_fact;
    wxTextCtrl *TextCtrl_poly_order, *TextCtrl_noise;
    wxRadioButton *Poly_RadioButton, *Prof_RadioButton;
    double xc, yc, radius, radius_fact, sigma_noise;
    int poly_order, nx, ny, polynomial_method; 

    DECLARE_EVENT_TABLE()
};

#endif               // EOF sentry
