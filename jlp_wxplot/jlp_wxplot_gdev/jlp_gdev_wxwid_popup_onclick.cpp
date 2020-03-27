/******************************************************************************
* jlp_gdev_wxwid_onclick.cpp
* JLP_GDev_wxWID class
* Purpose:     Functions used by the popup menu when displaying
*              a curve/image with wxwidgets
*
* Author:      JLP
* Version:     12/09/2017
******************************************************************************/
#include <stdio.h>
#include <stdlib.h>

#include "jlp_gdev_wxwid.h"
#include "jlp_wxgdev_labels.h"
#include "jlp_wx_cursor.h"
#include "jlp_itt1.h"
#include "jlp_wxplot_frame.h"  // JLP_wxPlot_Frame
#include "jlp_wx_ipanel.h"           // Filters

/************************************************************************
* Display the current (double precision) image as a 3D plot
* in a separate window
*
************************************************************************/
void JLP_GDev_wxWID::DisplayDbleImage3D()
{
JLP_wxPlot_Frame *my_gseg_plot;
char option0[32], title0[128], xlabel0[128], ylabel0[128], zlabel0[128];
// No contours:
int contours_option0 = 0;

  strcpy(option0, "3d");

  strcpy(title0, filename_1);
  strcpy(xlabel0, "");
  strcpy(ylabel0, "");
  strcpy(zlabel0, "");

// DisplayDbleImage3D();
// Open a separate graphic window:
  my_gseg_plot = new JLP_wxPlot_Frame(dble_image_1, nx_1, ny_1,
                                       wxString(title0), wxString(xlabel0),
                                       wxString(ylabel0), wxString(zlabel0),
                                       option0, contours_option0);
  my_gseg_plot->Show();

}
/************************************************************************
* Display the current (double precision) image as a countour plot
* in a separate window
*
************************************************************************/
void JLP_GDev_wxWID::DisplayDbleImageContours()
{
JLP_wxPlot_Frame *my_gseg_plot;
char option0[32], title0[128], xlabel0[128], ylabel0[128], zlabel0[128];
// Colored contours:
int contours_option0 = 3;

  strcpy(option0, "contours");

  strcpy(title0, filename_1);
  strcpy(xlabel0, "");
  strcpy(ylabel0, "");
  strcpy(zlabel0, "");

// DisplayDbleImage3D();
// Open a pop-up separate graphic window:
  my_gseg_plot = new JLP_wxPlot_Frame(dble_image_1, nx_1, ny_1,
                                       wxString(title0), wxString(xlabel0),
                                       wxString(ylabel0), wxString(zlabel0),
                                       option0, contours_option0);
  my_gseg_plot->Show();

}
/************************************************************************
* Prompt the user for new Box Limits
* Box_xmin, Box_xmax, Box_ymin, Box_ymax, chosen by the user
************************************************************************/
void JLP_GDev_wxWID::BoxLimitsPrompt()
{
wxString s_values, s_question;
double w1, w2, w3, w4;

if(initialized != 1234) return;

s_question.Printf(wxT("Current box boundaries are: xmin,xmax,ymin,ymax:%.4g %.4g %.4g %.4g\n Enter new values (CR to cancel):"),
      Jgc0.axis_limits[0], Jgc0.axis_limits[1], Jgc0.axis_limits[2],
      Jgc0.axis_limits[3]);

s_values.Printf(wxT("%.4g %.4g %.4g %.4g"),
      Jgc0.axis_limits[0], Jgc0.axis_limits[1], Jgc0.axis_limits[2],
      Jgc0.axis_limits[3]);

wxString result = wxGetTextFromUser(s_question, _T("New thresholds:"),
                                   s_values, NULL);
if (!result.IsEmpty()){
 if(sscanf(result.char_str(), "%lf %lf %lf %lf", &w1, &w2, &w3, &w4) == 4) {
   SetBoxLimits(w1, w2, w3, w4, 0., 1.);
   }

// Set axes to MGO axes:
 wxgdev_settings1.box_type = 0;

// Draw the graph again:
 RedrawToBackupDC(4005);
 }

return;
}
/************************************************************************
* Change Zoom factor
*
* NB: wxWidgets automatically toggles the flag value when the item is clicked
************************************************************************/
void JLP_GDev_wxWID::ApplyZoomSettings(wxGDev_SETTINGS wxgdev_settings0,
                                       const int update_display)
{
if(c_image1 == NULL || initialized != 1234) return;

// Copy wxGDev_SETTINGS:
 Load_wxGDevSettings(wxgdev_settings0);

 c_image1->SetZoom(wxgdev_settings1.zoom);

// Change the size of the canvas:
 MySetSize();

// Draw the graph again:
 if(update_display) {
  RedrawToBackupDC(4007);
  }

return;
}
/************************************************************************
* InfoImage: to display information about current image
************************************************************************/
void JLP_GDev_wxWID::GDevDisplayInfoImage()
{
wxString info_image;

if(c_image1 == NULL || initialized != 1234) return;

// Get information about the displayed image:
 c_image1->GetInfo(info_image);

 (void)wxMessageBox(info_image, _T("Info. about image"),
                    wxICON_INFORMATION | wxOK );
return;
}
/************************************************************************
* InfoCurve: to display information about plotted curve(s)
************************************************************************/
void JLP_GDev_wxWID::GDevDisplayInfoCurve()
{
wxString str1, str0;
int k;

// Full information:
/*
 char nchar1[4*NCURVES_MAX], pcolor1[32*NCURVES_MAX];
  char plot_fname1[128*NCURVES_MAX];
  int *npts1, nmaxi1, nout_maxi1, ncurves1, ncurves_maxi1;
*/

str1.Printf(wxT("ncurves=%d \n"), ncurves_1);
for(k = 0; k < ncurves_1; k++) {
 str0.Printf(wxT("Curve #%d : %s \n\
   (npts=%d, pcolor=%s, line_type=%s)\n"),
             k + 1, &plot_fname_1[k * 128], npts_1[k], &pcolor_1[32 * k],
             &nchar_1[4 * k]);
 str1.Append(str0);
 }

// Frame boundaries:
str0.Printf(wxT("Boundaries: xmin,xmax,ymin,ymax: %.4g %.4g %.4g %.4g\n"),
             Jgc0.axis_limits[0], Jgc0.axis_limits[1], Jgc0.axis_limits[2],
             Jgc0.axis_limits[3]);

str1.Append(str0);
 (void)wxMessageBox(str1, _T("Info. about displayed curve(s)"),
                    wxICON_INFORMATION | wxOK );
/* For debug:
 wxPuts(wxT("INFO/OK: ") + info_message);
*/
return;
}
/**************************************************************************
* OnSelectFilter
*
* 0=NONE
* 1=soft unsharp (UNSH1) 2=medium unsharp (UNSH2) 3=hard unsharp (UNSH3)
* 4=high contrast1 (VHC1) 5=high contrast2 (VHC2) 6=high contrast3 (VHC3)
* 7=high contrast4 (VHC4) 8=medium gradient (GRAD1) 9=hard gradient (GRAD2)
* 10=CPROF circ. profile 11=CIRC1 circ. gradient(rot 10 deg)
* 12=CIRC2 circ. gradient(rot 20 deg)  13=CIRC3 circ. gradient(rot 30 deg)
**************************************************************************/
void JLP_GDev_wxWID::GDevUpdatePopupMenu_SelectFilter(int filter0)
{

// Store input value to wxgdev SETTINGS:
  wxgdev_settings1.filter = filter0;

  m_parent_ipanel->ApplyFilter(filter0);

return;
}
/************************************************************************
* Copy to clipboard
************************************************************************/
void JLP_GDev_wxWID::GDevCopyToClipboard()
{
#if wxUSE_CLIPBOARD
    wxBitmapDataObject *dobjBmp = new wxBitmapDataObject;
    dobjBmp->SetBitmap(*backup_dc_bitmap2);

    wxTheClipboard->Open();

    if ( !wxTheClipboard->SetData(dobjBmp) ) {
        wxLogError(_T("Failed to copy bitmap to clipboard"));
    }

    wxTheClipboard->Close();
#endif // wxUSE_CLIPBOARD
return;
}
/************************************************************************
* Get image from clipboard
************************************************************************/
void JLP_GDev_wxWID::GDevPasteFromClipboard()
{
/*
#if wxUSE_CLIPBOARD
JLP_wxImageFrame1 *new_JLPImageFrame1;
wxBitmapDataObject dobjBmp;

wxTheClipboard->Open();
  if ( !wxTheClipboard->GetData(dobjBmp) ) {
      wxLogMessage(_T("No bitmap data in the clipboard"));
  } else {
        new_JLPImageFrame1 = new JLP_wxImageFrame1(NULL,
                               dobjBmp.GetBitmap().ConvertToImage(),
                               _T("Pasted image"), _T("From ClipBoard"));
        new_JLPImageFrame1->Show();
  }
  wxTheClipboard->Close();
#endif
*/
return;
}
