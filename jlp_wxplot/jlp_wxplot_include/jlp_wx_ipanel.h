/****************************************************************************
* Name: jlp_wxipanel.h
* JLP_wxImagePanel class
*
* JLP
* Version 21/10/2015
****************************************************************************/
#ifndef _jlp_wx_ipanel_
#define _jlp_wx_ipanel_

#include "time.h"

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

#include "wx/progdlg.h"

#if !wxUSE_TOGGLEBTN
    #define wxToggleButton wxCheckBox
    #define EVT_TOGGLEBUTTON EVT_CHECKBOX
#endif

// My files:
#include "jlp_gdev_wxwid.h"  // JLP_GDev_wxWID
#include "jlp_wx_image1.h"    // JLP_wxImage1
#include "jlp_wxlogbook.h"    // JLP_wxLogbook

//----------------------------------------------------------------------
// class definitions
// Image Panel
//----------------------------------------------------------------------
class JLP_wxImagePanel: public wxPanel
{
public:
    JLP_wxImagePanel(wxFrame *frame, wxStaticText *static_coord,
                     wxStaticText *static_help, int x, int y, int w, int h,
                     const int should_fit_inside);
    JLP_wxImagePanel(wxFrame *frame, JLP_wxLogbook *logbook,
                     wxStatusBar *frame_status_bar,
                     int x, int y, int w, int h, const int should_fit_inside);
    void wxIP_MainInit();

    virtual ~JLP_wxImagePanel();
    void OnResize(wxSizeEvent &event);

    int wxIP_LoadImage(double *dble_image0, int nx0, int ny0);
    int wxIP_Erase();
    int RestoreOriginalImage();
    void PstCopyOfDisplay(char* input_filename, char *pst_filename);
    void OnEnableAll(wxCommandEvent& event);
    int wxIP_WriteToLogbook(wxString str1, bool save_to_file0);

// in "jlp_wxipanel_process.cpp":
    void ApplyFilter(int filter0);
    int FilterUnsharp(int box_width);
    int FilterHighContrast(int filter_option);
    int FilterVeryHighContrast();
    int SpeckleModelSubtraction();
    int LoadUnresolvedAutoc();
    int ComputeUnresolvedModsq();
    int OffsetCorrection(int positive);
    int FlatFieldCorrection(int sigma_level);
    int AutoMeasureShackHartmann(double *pupil_mask0, int nx0, int ny0,
                                 int x_half_width, int y_half_width,
                                 double sigma_threshold,
                                 double *peak_x, double *peak_y,
                                 int *npeaks, int npeaks_maxi);


// ITT and LUT Settings:
  void wxIP_SetITT_type(wxString itt_thresh)
    {
     Image1_wxgdev->GDevSetITT_type(itt_thresh);
    };
// Linear/log
  void wxIP_SetITT_linear(int is_linear)
    {
     Image1_wxgdev->GDevSetITT_linear(is_linear);
    };
  void wxIP_SetLUT(char *lut_type)
    {
     Image1_wxgdev->GDevSetLUT(lut_type);
    };
  void wxIP_SetFilter(int filter_type)
    {
     Image1_wxgdev->GDevSetFilter(filter_type);
    };

  int wxIP_DrawRectangle_UC(double xstart, double ystart, double xend, 
                            double yend, int rr, int gg, int bb, int pen_width,
                            int filled) 
   {
   return(Image1_wxgdev->DrawRectangle_UC(xstart, ystart, xend, yend,
                                          rr, gg, bb, pen_width, filled));
   }
// Accessors:
  JLP_GDev_wxWID *Image_gdev(){return Image1_wxgdev;};
  JLP_wxImage1 *wxImage1(){return c_image1;};
//
  double Get_Low_Threshold(){
      wxString itt_type0;
      double low_thresh0 = 0., up_thresh0 = 0.;
      int x1_box0, y1_box0, x2_box0, y2_box0;
      if(c_image1 != NULL)
        c_image1->GetITT_Thresh(&itt_type0, &low_thresh0, &up_thresh0, &x1_box0,
                                &y1_box0, &x2_box0, &y2_box0);
      return low_thresh0;
      }
  double Get_Up_Threshold(){
      wxString itt_type0;
      double low_thresh0 = 0., up_thresh0 = 0.;
      int x1_box0, y1_box0, x2_box0, y2_box0;
      if(c_image1 != NULL)
        c_image1->GetITT_Thresh(&itt_type0, &low_thresh0, &up_thresh0, &x1_box0,
                                &y1_box0, &x2_box0, &y2_box0);
      return up_thresh0;
      }
  void wxIP_UpdateImageDisplay();

private:
// Main parameters:
  JLP_wxLogbook *jlp_logbook;
  wxStatusBar *m_StatusBar;
  wxStaticText *m_StaticCoord, *m_StaticHelp;
  int initialized, max_lut_level1;
  wxSize size0;

// Image:
  JLP_wxImage1 *c_image1;
  double *dble_image1, *dble_orig_image1;
  int nx1, ny1, should_fit_inside1;

// UnresolvedModsq (needed for very high contrast filter):
  double *UnresolvedAutoc, *UnresolvedModsq, SigmaUnresolvedModsq;
  wxString UnresolvedAutocFilename;

// Image/Graphic device (with popup menu)
  JLP_GDev_wxWID *Image1_wxgdev;
  int Image_idev;

  DECLARE_EVENT_TABLE()
};

#endif
