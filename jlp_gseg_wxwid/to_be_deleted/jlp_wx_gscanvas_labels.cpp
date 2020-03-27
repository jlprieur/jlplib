/******************************************************************************
* jlp_cgdev_wxwid_labels.cpp
* JLP_wxGseg_Canvas class
* Purpose:     displaying labels on the curve
*
* Author:      JLP 
* Version:     03/08/2013 
******************************************************************************/
#include "jlp_wx_gscanvas.h"
#include "jlp_wx_curve_labels.h"  // JLP_wxCurveLabels class

/**********************************************************************
* Display a label on the curve
* The user will then position this label by
* clicking on the mouse (see OnLeftDown in "jlp_curve_canvas.cpp")
**********************************************************************/
// Curve labels:
void JLP_wxGseg_Canvas::OnAddLabel(wxCommandEvent& WXUNUSED(event))
{
// Prompt the user for the label:
       wxString new_label = wxGetTextFromUser(
       _T("The label will be positioned by clicking on the mouse\n\
Please enter the label:"), _T("Adding a new label"), _T(""), NULL);

// InternalProcessingMode
// -1=None 0=BoxLimits 1=Statistics, 2=Astrometry 3=Photometry
// 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
// 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
// 13=Add a ring 14=Remove a shape
if(!new_label.IsEmpty()) {
 m_label1 = new_label;
 cgdev_settings1.InternalProcessingMode = 4;
// Apply settings and update popup menu:
 UpdatePopupMenu_InternalProcessingMode();
 }

return;
}
/**********************************************************************
* Remove the last entered label from the curve
**********************************************************************/
void JLP_wxGseg_Canvas::OnRemoveLabel(wxCommandEvent& WXUNUSED(event))
{
// InternalProcessingMode
// -1=None 0=BoxLimits 1=Statistics, 2=Astrometry 3=Photometry
// 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
// 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
// 13=Add a ring 14=Remove a shape
 cgdev_settings1.InternalProcessingMode = 5;
// Apply settings and update popup menu:
 UpdatePopupMenu_InternalProcessingMode();
}
/**********************************************************************
* Add a new label to the curve
*
* INPUT:
*  x0, y0 : (user coordinates) location of teh center of the label
**********************************************************************/
void JLP_wxGseg_Canvas::AddLabel(wxString new_label, double x0, 
                                 double y0)
{
int raise_to_top0, dev_x0, dev_y0, in_frame;
char label0[128], text_anchor0[128];
UINT32 canvas_fg_color, canvas_bg_color;

 if(jlp_gsegraf0 == NULL) return;
 jlp_gsegraf0->FromUserToDeviceCoord(x0, y0, &dev_x0, &dev_y0, &in_frame);

 strcpy(label0, (const char*)new_label.mb_str());
/* DEBUG
 printf("DEBUG: AddLabel >%s< at %d %d (from %g %g)\n", 
         label0, dev_x0, dev_y0, x0, y0);
*/
 strcpy(text_anchor0, "CENTER");
 raise_to_top0 = 1;
 canvas_fg_color = 0x000000FF;
 canvas_bg_color = 0xFFFFFFFF;
 jlp_gsegraf0->GSEG_AddExtraLabel(label0, dev_x0, dev_y0, canvas_fg_color, 
                                     canvas_bg_color, text_anchor0, 
                                     raise_to_top0);

return;
}
/**********************************************************************
* Remove a label to the curve
**********************************************************************/
void JLP_wxGseg_Canvas::RemoveLabel(double x0, double y0)
{
if(m_curve_labels1 != NULL) {
  m_curve_labels1->RemoveLabel(x0, y0);
// TBD
//  UpdateBackupDC();
  }
}
