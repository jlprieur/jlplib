/*************************************************************************
* \file jlp_wx_gdproc.h (virtual abstract class) 
* \class JLP_wx_GDProc called by JLP_iGDev_wxWID (image Graphic Device) 
* \brief Image processing with data points entered interactively 
* \author JLP
* \date 05/08/2013
*
* JLP
* Version 05/08/2013
**************************************************************************/
#ifndef __jlp_wx_igdproc_h                     /* sentry */
#define __jlp_wx_igdproc_h

#include "jlp_gdev_wxwid.h" // JLP_GDev_wxWID class

/* Definition of the prototype of all JLP_iGDev classes
* (i.e. for X11, postscript, gxwindows, etc)
*/
class JLP_wx_GDProc {

public:

// Abstract class should not have any definition of the constructors

// Processing with box limits obtained by JLP_iGDev_wxWID:
// virtual routines: should be defined by children:
 virtual int DataProcessing(double *x_down, double *y_down, int n_down, 
                            double *x_up, double *y_up, int n_up, 
                            wxString label) = 0;
 virtual int SetNewProcessingMode(int processing_mode0) = 0;

// Set new configuration of JLP_iGDev_wxWID
// in jlp_wx_igdproc.cpp 
// (it seems necessary to have something in this file for
// building the library...)
 int GetActiveProcessingMode(int *processing_mode0, int *limits_box_type0,
                             int *n_points_required0);
// JLP2020: Error here "failed in FromWChar(): trying to encode undefined
// Unicode character"
// JLP2022: error solved by returning my_help_text
 wxString Get_HelpText() {
   wxString my_help_text(" ");
//   printf("Get HelpText/ZZZ/DEBUG: ");
//   printf("text=%s\n", (const char *)help_text1.mb_str());
// return help_text1;
 return my_help_text;
 };

// Common parameters (should not be declared as private in an abstract class!)
protected:

// Popup menu:
 JLP_GDev_wxWID *m_gdev_wxwid1;
 int ProcessingMode1, n_PointsRequired1, LimitsBoxType1;
 wxString help_text1;

}; 

#endif    /* __jlp_wx_gdproc_h sentry */
