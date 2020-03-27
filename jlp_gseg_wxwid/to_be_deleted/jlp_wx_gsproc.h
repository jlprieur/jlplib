/*************************************************************************
* \file jlp_wx_gsproc.h (virtual abstract class) 
* \class JLP_wx_gsProc called by JLP_wxGseg_Canvas (curve Graphic Device) 
* \brief Curve processing with data points entered interactively 
*
* JLP
* Version 05/02/2016
**************************************************************************/
#ifndef __jlp_wx_gsproc_h                     /* sentry */
#define __jlp_wx_gsproc_h

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
#undef index  // To solve problems with index (conflicts with "string.h") ...
    #include "wx/wx.h"
#endif

#define MAX_PROCESSING_NBER 20

class JLP_wxGseg_Canvas;

/* Definition of the prototype of all JLP_cGDev classes
* (i.e. for X11, postscript, gxwindows, etc)
*/
class JLP_wx_gsProc {

public:

// Abstract class should not have any definition of the constructors

// Processing with box limits obtained by JLP_wxGseg_Canvas:
// virtual routine: should be defined by children:
 virtual int DataProcessing(double *x_down, double *y_down, int n_down, 
                            double *x_up, double *y_up, int n_up, 
                            wxString label) = 0;

// virtual routine: should be defined by children:
 virtual int SetNewProcessingMode(int processing_mode, int *limits_box_type,
                                  int *n_points_required) = 0;

// Set new configuration of JLP_wxGseg_Canvas
// in jlp_wx_gsproc.cpp 
// (it seems necessary to have something in this file for
// building the library...)
 int AskNewBoxLimits_to_cgdev();
 wxString Get_HelpText(){return help_text1;};

// Common parameters (should not be declared as private in an abstract class!)
protected:

 JLP_wxGseg_Canvas *m_wxgseg_canvas1;
 int n_PointsRequired1;
 int LimitsBoxType1;
 wxString help_text1;

}; 

#endif    /* __jlp_wx_gsproc_h sentry */
