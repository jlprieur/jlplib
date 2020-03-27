/***********************************************************************
* jlp_wxgdev_labels.cpp
* JLP_wxGDevLabels class defined for adding labels to an image.
* Used by jlp_igdev_wxwid routines
*
* JLP
* Version 10/08/2013
***********************************************************************/
#include "jlp_wxgdev_labels.h"  // JLP_wxGDevLabels class
#include "jlp_gdev_wxwid.h"     // JLP_GDev_wxWID class
#include "jlp_macros.h"         // SQUARE, etc 
 
/**************************************************************************
* Constructor
**************************************************************************/
JLP_wxGDevLabels::JLP_wxGDevLabels()
{
  nlabels = 0;
  scale_bar = 0;
  ne_labels = 0;
}

/**************************************************************************
* Adding a label to the current set of labels 
*
* INPUT:
*  dev_x0, dev_y0: location of the label (device coordinates) 
*  new_label: label text 
**************************************************************************/
int JLP_wxGDevLabels::AddNewLabel(wxString label, double dev_x0, double dev_y0) 
{
  if(nlabels == MAX_xLABELS) {
    wxMessageBox(_T("WARNING: maximum number of labels has been reached!"),
                 _T("ImageLabels::AddNewLabel"), wxICON_ERROR);
    fprintf(stderr, "AddNewLabel/Error: nlabels=%d has reached upper limit\n",
            nlabels);
    return(-1);
    }
  label_text[nlabels] = label;
  label_x[nlabels] = dev_x0;
  label_y[nlabels] = dev_y0;
  nlabels++;
  return(0);
}
/**********************************************************************
* Removing a label from the current set of labels 
* by looking at the closest label to the input point
*
* INPUT:
*  dev_x0, dev_y0: location of the label (device coordinates) 
**********************************************************************/
int JLP_wxGDevLabels::RemoveLabel(double dev_x0, double dev_y0)
{
int i, i_min;
double d, d_min;

// Look for the closest label: 
d_min = 10000;
i_min = 0;
for(i = 0; i < nlabels; i++) {
  d = SQUARE(label_x[i] - dev_x0) + SQUARE(label_y[i] - dev_y0);
  if(d < d_min){ d_min = d; i_min = i;}
  }

// Remove the label in the list:
// and shift all the numbers
for(i = i_min; i < nlabels - 1; i++) {
  label_text[i] = label_text[i + 1];
  label_x[i] = label_x[i + 1];
  label_y[i] = label_y[i + 1];
  }

nlabels--;

return(0);
}
/*************************************************************
* To draw the labels on a graphic context
*************************************************************/
void JLP_wxGDevLabels::DrawLabelsToBackupDC(JLP_GDev_wxWID *jlp_wx_gdev1)
{
int i, drawit = 1;
double angle0 = 0., expand0 = 1.2;

// Draw all the labels:
 for(i = 0; i < nlabels; i++) {
    jlp_wx_gdev1->label_device(label_text[i], label_x[i], label_y[i],
                                angle0, expand0, drawit);
  }

return;
}
/*************************************************************
* To draw the labels on a graphic context  
*************************************************************/
void JLP_wxGDevLabels::DrawAllImageLabelsToBackupDC(
                                             JLP_GDev_wxWID *jlp_wx_igdev1,
                                             wxMemoryDC *backup_dc,
                                             wxColour c_PenColour,
                                             int scroll_x, int scroll_y)
{
int i, ix, iy, in_frame, ix1, iy1, ix2, iy2;
int pen_width = 2, PointSize;
double x1, x2, y1, y2, wx, wy, wx1, wy1, wx2, wy2;
wxFont def_Font = wxSystemSettings::GetFont(wxSYS_DEFAULT_GUI_FONT);
#ifdef NEW_FONTS
wxFont *my_Font;
#endif

PointSize = 2 * def_Font.GetPointSize();
// Select nicer fonts than default fonts:
#ifdef NEW_FONTS
// wxSWISS_FONT allows rotation with DrawRotatedText
my_Font = new wxFont(PointSize, wxFONTFAMILY_SWISS,
                     wxFONTSTYLE_NORMAL, wxFONTWEIGHT_BOLD,
                     false, wxEmptyString, wxFONTENCODING_DEFAULT);
backup_dc->SetFont(*my_Font);
#endif

// Set the color and width:
// For curves:
  backup_dc->SetPen(wxPen(c_PenColour, pen_width));
// For Text:
  backup_dc->SetTextForeground(c_PenColour);

// Draw all the labels:
 for(i = 0; i < nlabels; i++) {
    jlp_wx_igdev1->ConvUserToDev(label_x[i], label_y[i], 
                                               &wx1, &wy1, &in_frame);
    ix1 = (int)wx1;
    iy1 = (int)wy1;
    backup_dc->DrawText(label_text[i], ix1 + scroll_x, iy1 + scroll_y);
  }

// Draw the scale bar:
 if(scale_bar) {
  x1 = scale_bar_x + scale_bar_length;
  y1 = scale_bar_y - scale_bar_height/2;
  x2 = x1;
  y2 = scale_bar_y + scale_bar_height/2;
  jlp_wx_igdev1->ConvUserToDev(x1, y1, &wx1, &wy1, &in_frame);
  ix1 = (int)wx1;
  iy1 = (int)wy1;
  jlp_wx_igdev1->ConvUserToDev(x2, y2, &wx2, &wy2, &in_frame);
  ix2 = (int)wx2;
  iy2 = (int)wy2;
  backup_dc->DrawLine(ix1 + scroll_x, iy1 + scroll_y, ix2 + scroll_x, 
                      iy2 + scroll_y);

  x1 = scale_bar_x;
  y1 = scale_bar_y - scale_bar_height/2;
  x2 = x1;
  y2 = scale_bar_y + scale_bar_height/2;
  jlp_wx_igdev1->ConvUserToDev(x1, y1, &wx1, &wy1, &in_frame);
  ix1 = (int)wx1;
  iy1 = (int)wy1;
  jlp_wx_igdev1->ConvUserToDev(x2, y2, &wx2, &wy2, &in_frame);
  ix2 = (int)wx2;
  iy2 = (int)wy2;
  backup_dc->DrawLine(ix1 + scroll_x, iy1 + scroll_y, ix2 + scroll_x, 
                      iy2 + scroll_y);

  x1 = scale_bar_x;
  y1 = scale_bar_y;
  x2 = scale_bar_x + scale_bar_length;
  y2 = y1;
  jlp_wx_igdev1->ConvUserToDev(x1, y1, &wx1, &wy1, &in_frame);
  ix1 = (int)wx1;
  iy1 = (int)wy1;
  jlp_wx_igdev1->ConvUserToDev(x2, y2, &wx2, &wy2, &in_frame);
  ix2 = (int)wx2;
  iy2 = (int)wy2;
  backup_dc->DrawLine(ix1 + scroll_x, iy1 + scroll_y, ix2 + scroll_x, 
                      iy2 + scroll_y);
  }

// Draw the North-East labels:
 if(ne_labels) {
  jlp_wx_igdev1->ConvUserToDev(ne_x, ne_y, &wx, &wy, &in_frame);
  ix = (int)wx;
  iy = (int)wy;
  x1 = ne_x + ne_size * cos(ne_north_angle);
  y1 = ne_y + ne_size * sin(ne_north_angle);
  jlp_wx_igdev1->ConvUserToDev(x1, y1, &wx1, &wy1, &in_frame);
  ix1 = (int)wx1;
  iy1 = (int)wy1;
  backup_dc->DrawLine(ix + scroll_x, iy + scroll_y, ix1 + scroll_x, 
                      iy1 + scroll_y);

  x2 = ne_x + ne_size * cos(ne_north_angle - (3.1415/2.)*ne_clockwise);
  y2 = ne_y + ne_size * sin(ne_north_angle - (3.1415/2.)*ne_clockwise);
  jlp_wx_igdev1->ConvUserToDev(x2, y2, &wx2, &wy2, &in_frame);
  ix2 = (int)wx2;
  iy2 = (int)wy2;
  backup_dc->DrawLine(ix + scroll_x, iy + scroll_y, ix2 + scroll_x, 
                      iy2 + scroll_y);
  }

// Go back to default fonts:
#ifdef NEW_FONTS
backup_dc->SetFont(def_Font);
delete my_Font;
#endif

return;
}
/*************************************************************
* To draw the scale bar (for images) 
*
* INPUT:
*  dev_x0, dev_y0: location of the label (device coordinates) 
*************************************************************/
int JLP_wxGDevLabels::AddNewScaleBar(double dev_x0, double dev_y0, 
                                     double length, double height)
{
 scale_bar = 1;
 scale_bar_x = dev_x0;
 scale_bar_y = dev_y0;
 scale_bar_length = length;
 scale_bar_height = height;
 return(0);
}
/*************************************************************
* Move the scale bar (for images) 
*
* INPUT:
*  dev_x0, dev_y0: location of the label (device coordinates) 
*************************************************************/
int JLP_wxGDevLabels::MoveScaleBar(double dev_x0, double dev_y0)
{
 if(!scale_bar) return(-1);
 scale_bar_x = dev_x0;
 scale_bar_y = dev_y0;
 return(0);
}
/*************************************************************
* Remove the scale bar (for images) 
*************************************************************/
void JLP_wxGDevLabels::RemoveScaleBar() 
{ 
 scale_bar = 0; 
}
/*************************************************************
* Remove the North-East label (for images) 
*************************************************************/
void JLP_wxGDevLabels::RemoveNorthEast() 
{ 
 ne_labels = 0; 
}
/*************************************************************
* Draw the North-East label for orientation (for images) 
*
* INPUT:
*  dev_x0, dev_y0: location of the label (device coordinates) 
*************************************************************/
int JLP_wxGDevLabels::AddNewNorthEast(double dev_x0, double dev_y0, 
                                      double size, double angle, 
                                      double clockwise)
{
 ne_labels = 1;
 ne_x = dev_x0;
 ne_y = dev_y0;
 ne_size = size;
 ne_north_angle = angle;
 ne_clockwise = clockwise;
 return(0);
}
/*************************************************************
* Update the North-East label for orientation (for images) 
*
* INPUT:
*  dev_x0, dev_y0: location of the label (device coordinates) 
*************************************************************/
int JLP_wxGDevLabels::UpdateNorthEast(double size, double dev_x0, 
                                      double dev_y0)
{
 if(!ne_labels) return(-1);
 ne_size = size;
 ne_x = dev_x0;
 ne_y = dev_y0;
 return(0);
 }
