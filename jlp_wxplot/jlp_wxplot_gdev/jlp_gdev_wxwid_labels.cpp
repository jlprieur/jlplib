/******************************************************************************
* jlp_gdev_wxwid_labels.cpp
* JLP_GDev_wxWID class
* Purpose:     displaying labels on the image
*
* Author:      JLP 
* Version:     13/02/2017 
******************************************************************************/
#include "jlp_gdev_wxwid.h"
#include "jlp_wxgdev_labels.h"  // JLP_wxGDevLabels class
#include "jlp_wxgdev_popup.h"   // For JLP_wxGDev_Popup

/**********************************************************************
* Display a label on the image
* The user will then position this label by
* clicking on the mouse (see OnLeftDown in "jlp_image_canvas.cpp")
**********************************************************************/
// Image labels:
void JLP_GDev_wxWID::GDevAddLabel()
{
// Prompt the user for the label:
       wxString new_label = wxGetTextFromUser(
       _T("The label will be positioned by clicking on the mouse\n\
Please enter the label:"), _T("Adding a new label"), _T(""), NULL);

// InternalProcessingMode
// -1=None 0=Automatic thresholds 1=Statistics, 2=Astrometry 3=Photometry
// 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
// 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
// 13=Add a ring 14=Remove a shape
if(!new_label.IsEmpty()) {
 m_label1 = new_label;
 wxgdev_settings1.InternalProcessingMode = 4;
// Apply settings and update popup menu:
 if(m_popup_menu1 != NULL) m_popup_menu1->UpdatePopupMenu_InternalProcessingMode();
 }

return;
}
/**********************************************************************
* Add a new label to the image/curve plot
*
* INPUT:
*  user_x0, user_y0 : (user coordinates) location of the center of the label
**********************************************************************/
void JLP_GDev_wxWID::AddLabel(wxString new_label, 
                              double user_x0, double user_y0)
{

if(m_wxlabels1 != NULL) {
  m_wxlabels1->AddNewLabel(new_label, user_x0, user_y0);
  RedrawToBackupDC(2001);
  }

return;
}
/**********************************************************************
* Add a new label to the image/curve plot
*
* INPUT:
*  dev_x0, dev_y0 : (device coordinates) location of the center of the label
**********************************************************************/
int JLP_GDev_wxWID::AddLabel_gseg(wxString new_label, double dev_x0,
                                  double dev_y0)
{
int raise_to_top0;
char label0[128], text_anchor0[128];
UINT32 canvas_fg_color, canvas_bg_color;

 if(jlp_gsegraf1 == NULL) return(-1);

 strcpy(label0, (const char*)new_label.mb_str());
 strcpy(text_anchor0, "CENTER");
 raise_to_top0 = 1;
// Foreground color: black
 canvas_fg_color = 0x000000FF;
// Background color: white 
 canvas_bg_color = 0xFFFFFFFF;
 jlp_gsegraf1->GSEG_AddExtraLabel(label0, dev_x0, dev_y0, canvas_fg_color,
                                     canvas_bg_color, text_anchor0,
                                     raise_to_top0);

return(0);
}
/**********************************************************************
* Remove a label to the image
**********************************************************************/
void JLP_GDev_wxWID::RemoveLabel(double x0, double y0)
{
if(m_wxlabels1 != NULL) {
  m_wxlabels1->RemoveLabel(x0, y0);
  RedrawToBackupDC(2002);
  }
}
/**********************************************************************
* Display a scale bar on the image
**********************************************************************/
void JLP_GDev_wxWID::GDevAddScaleBar()
{
double length, height;
double xx = 0, yy = 0;
wxString result = wxGetTextFromUser(
_T("The scale bar will be positioned by clicking on the mouse\n\
Length, height (in image pixels):"), _T("Adding a scale bar"), _T(""), NULL);

if(!result.IsEmpty()){

  if(sscanf(result.char_str(), "%lf %lf", &length, &height) != 2) 
      sscanf(result.char_str(), "%lf,%lf", &length, &height); 

   m_wxlabels1->AddNewScaleBar(xx, yy, length, height);

// InternalProcessingMode
// -1=None 0=Automatic thresholds 1=Statistics, 2=Astrometry 3=Photometry
// 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
// 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
// 13=Add a ring 14=Remove a shape
   wxgdev_settings1.InternalProcessingMode = 6;
// Apply settings and update popup menu:
   if(m_popup_menu1 != NULL) m_popup_menu1->UpdatePopupMenu_InternalProcessingMode();
  }

return;
}
/**********************************************************************
* Display North-East labels on the image
**********************************************************************/
void JLP_GDev_wxWID::GDevAddNorthEastLabel()
{
double angle, size, clkwise;
double xx = 0., yy = 0.;
wxString result = wxGetTextFromUser(_T("Size will be selected by dragging the laft button\n Angle of North, orientation (1=clockwise,-1=anti-clockwise):"), 
                                    _T("Adding North-East labels"), 
                                    _T(""), NULL);
if(!result.IsEmpty()){
  if(sscanf(result.char_str(), "%lf %lf", &angle, &clkwise) != 2) 
                  sscanf(result.char_str(), "%lf,%lf", &angle, &clkwise);

// Validate the display of the North-East labels 
   angle *= 3.14 /180.;
   size = 0.0001;
   m_wxlabels1->AddNewNorthEast(xx, yy, size, angle, clkwise);

// InternalProcessingMode
// -1=None 0=Automatic thresholds 1=Statistics, 2=Astrometry 3=Photometry
// 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
// 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
// 13=Add a ring 14=Remove a shape
   wxgdev_settings1.InternalProcessingMode = 7;
// Apply settings and update popup menu:
   if(m_popup_menu1 != NULL) m_popup_menu1->UpdatePopupMenu_InternalProcessingMode();
 }

return;
}
/**********************************************************************
* Remove the scale bar from the image
**********************************************************************/
void JLP_GDev_wxWID::RemoveScaleBar()
{
if(m_wxlabels1 != NULL) {
 m_wxlabels1->RemoveScaleBar();
 RedrawToBackupDC(2003);
 }
}
/**********************************************************************
* Move the scale bar in the image 
**********************************************************************/
void JLP_GDev_wxWID::MoveScaleBar(double x0, double y0)
{
if(m_wxlabels1 != NULL) {
 m_wxlabels1->MoveScaleBar(x0, y0);
 RedrawToBackupDC(2004);
 }
}
/**********************************************************************
* Remove the North-East label from the image
**********************************************************************/
void JLP_GDev_wxWID::RemoveNorthEastLabel()
{
if(m_wxlabels1 != NULL) {
  m_wxlabels1->RemoveNorthEast();
  RedrawToBackupDC(2005);
  }
}
/**********************************************************************
* Update the North-East label from the image
**********************************************************************/
void JLP_GDev_wxWID::UpdateNorthEastLabel(double radius, double xc, double yc)
{
if(m_wxlabels1 != NULL) {
  m_wxlabels1->UpdateNorthEast(radius, xc, yc);
  RedrawToBackupDC(2006);
  }
}
/**********************************************************************
* Handle label contours popup menu item 
* (Toggle processing mode to -1 if button selected twice)
*
* Display labels on a contour of the image
* The user position those labels by
* clicking on the mouse at the wanted location 
*
**********************************************************************/
void JLP_GDev_wxWID::GDevSetLabelContours()
{
/*
* InteractiveProcessingMode :
* -1=None 0=Automatic thresholds 1=Statistics, 2=Astrometry 3=Photometry
* 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
* 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
* 13=Add a ring 14=Remove a shape 15=Rotate a shape 16=Magnify a shape
* 17=Move a shape 18=ZoomIn 19=DisplayCoordinates 20=LabelContours
*/

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

printf("type=%d mode=%d\n", Jgc0.gdev_graphic_type, wxgdev_settings1.InternalProcessingMode);
if(jlp_gsegraf1 != NULL && Jgc0.gdev_graphic_type == 5) {
// Toggle processing mode to -1 if button selected twice:
  if(wxgdev_settings1.InternalProcessingMode == 20) {
   wxgdev_settings1.InternalProcessingMode = -1;
// Apply settings and update popup menu:
   if(m_popup_menu1 != NULL) m_popup_menu1->UpdatePopupMenu_InternalProcessingMode();
   } else {  
// in "jlp_gdev_wxwid_gseg.cpp" :
   Set_ViewLabelContours();
   }
 }

return;
}
