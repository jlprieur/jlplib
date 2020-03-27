/***********************************************************************
* jlp_wxgdev_labels.h
* JLP_wxGDevLabels class defined for adding labels to an image.
* Used by jlp_gdev_wxwid routines
*
* JLP
* Version 10/02/2017
***********************************************************************/
#ifndef _jlp_wxgdev_labels_h 
#define _jlp_wxgdev_labels_h 

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
#undef index  // To solve problems with index (conflicts with "string.h") ...
    #include "wx/wx.h"
#endif

#define MAX_xLABELS 1024

// Simple declaration here to avoid "boomerang" effect:
class JLP_GDev_wxWID;

class JLP_wxGDevLabels
{
public:
 
 JLP_wxGDevLabels();

 int AddNewLabel(wxString new_label, double x1, double y1);
 int RemoveLabel(double x1, double y1);
 int AddNewScaleBar(double xx, double yy, double length, double height);
 int MoveScaleBar(double xx, double yy);
 int AddNewNorthEast(double xx, double yy, double size, double angle,
                     double clockwise);
 int UpdateNorthEast(double size, double xx, double yy);
 void RemoveScaleBar();
 void RemoveNorthEast();
 void DrawLabelsToBackupDC(JLP_GDev_wxWID *jlp_wx_gdev1);
 void DrawAllImageLabelsToBackupDC(JLP_GDev_wxWID *jlp_wx_gdev1, 
                           wxMemoryDC *backup_dc, wxColour c_PenColour,
                           int scroll_x, int scroll_y);

// Accessors:
 bool HasScaleBar(){return(scale_bar == 1);};
 bool HasNorthEast(){return(ne_labels == 1);};
 bool HasLabels(){return(nlabels > 0);};

private:

// Labels:
int nlabels, scale_bar, ne_labels;
wxString label_text[MAX_xLABELS];
double label_x[MAX_xLABELS], label_y[MAX_xLABELS];
double scale_bar_length, scale_bar_height;
double scale_bar_x, scale_bar_y; 
double ne_x, ne_y, ne_size, ne_north_angle, ne_clockwise;
};

#endif
