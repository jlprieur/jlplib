/*************************************************************************
* \file jlp_wx_gdproc1.h 
* \class JLP_wx_GDProc1 derived from JLP_wx_GDProc abstract class
* and used by JLP_GDev_wxWID (curve/image Graphic Device) 
* \brief Image processing with data points entered interactively 
* \author JLP
* \date 15/01/2017
*
* InteractiveProcessingMode :
* -1=None 0=Automatic thresholds 1=Statistics, 2=Astrometry 3=Photometry
* 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
* 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
* 13=Add a ring 14=Remove a shape 15=Rotate a shape 16=Magnify a shape
* 17=Move a shape 18=ZoomIn
*
* JLP
* Version 15/01/2017
**************************************************************************/
#ifndef __jlp_wx_gdproc1_h                     /* sentry */
#define __jlp_wx_gdproc1_h

#include <stdio.h>
// #include <math.h>
#include <ctype.h>

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
#undef index  // To solve problems with index (conflicts with "string.h") ...
#include "wx/wx.h"
#endif

/* To define the JLP_wx_GDProc virtual class */ 
#include "jlp_wx_gdproc.h"

#include "jlp_gdev_wxwid.h" // JLP_GDev_wxWID class

/* To define "std" as the standard prefix (e.g. before printf, scanf, ...) */
using namespace std;

// class JLP_wx_GDProc1:
class JLP_wx_GDProc1 : public JLP_wx_GDProc {

public:

// Constructor:
  JLP_wx_GDProc1(JLP_GDev_wxWID* gdev_wxwid);

/* Destructor:
* (Should be declared as virtual)
*/
  virtual ~JLP_wx_GDProc1() {
  return;
  }

// Virtual routine of JLP_wx_GDProc: should be defined in this class! 
// (defined in jlp_wx_gdproc1.cpp)
 int DataProcessing(double *x_down, double *y_down, int n_down, 
                    double *x_up, double *y_up, int n_up, wxString label);

// Virtual routine of JLP_wx_GDProc: should be defined in this class! 
// (defined in jlp_wx_gdproc1.cpp)
 int SetNewProcessingMode(int processing_mode0);
 int GetActiveProcessingMode(int *processing_mode0, int *limits_box_type0, 
                             int *n_points_required0);

private:

// WARNING: there are other private variables declared in jlp_wx_gdproc.h

}; 

#endif    /* __jlp_wx_gdproc1_h sentry */
