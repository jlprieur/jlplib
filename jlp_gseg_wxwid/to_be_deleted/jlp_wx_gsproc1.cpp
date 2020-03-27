/*************************************************************************
* jlp_wx_gsproc1.cpp
* JLP_wx_gsProc1 derived from JLP_wx_gsProc abstract class
* and used by JLP_wxGseg_Canvas (image Graphic Device) 
* Image processing with data points entered interactively 
*
* JLP
* Version 06/02/2016
**************************************************************************/
/* To define the JLP_wx_gsProc1 class */ 
#include "jlp_wx_gsproc1.h"

#include "jlp_wx_image1.h" // JLP_wxImage1 class
#include "jlp_process_curve.h"  // GetRectangleBoxLimits1(), StatisticsFromBox1, etc
#include "jlp_wx_gsplot_dlg.h"  // JLP_wx_GsegPlot_Dlg 

//#define DEBUG

/**************************************************************************
* Constructor
*
**************************************************************************/
JLP_wx_gsProc1::JLP_wx_gsProc1(JLP_wxGseg_Canvas* wxgseg_canvas0) 
{
int pmode, limits, npts;

m_wxgseg_canvas1 = wxgseg_canvas0; 

// To avoid memory problems with long strings:
help_text1.Alloc(512);

// Initialize help text:
help_text1 = wxT("");

// Dummy selection for initialisation only:
pmode = -1;
SetNewProcessingMode(pmode, &limits, &npts);

return;
}
/**************************************************************************
*
* InteractiveProcessingMode :
* -1=None 18=BoxLimits/ZoomIn 1=Statistics, 2=Astrometry 3=Photometry
* 4=Add a label 5=Remove a label 
*
* Input: 
*  processing_mode: interactive processing mode
*
* Output: 
*  limits_box_type: 0=none 1=line, 2=rectangle, 3=circle, 4=ellipse, 5=ring
*  n_points_required: set according to box type
*
**************************************************************************/
int JLP_wx_gsProc1::SetNewProcessingMode(int processing_mode,
                                          int *limits_box_type, 
                                          int *n_points_required)
{

// Save to private variable:
processing_mode1 = processing_mode;

// Set limits box type: 0=none 1=line, 2=rectangle, 3=circle, 4=ellipse, 5=ring
// and number of points that are required: 
switch(processing_mode) {
  case -1:  
    help_text1 = wxT("");
    LimitsBoxType1 = 0;
    n_PointsRequired1 = 0;
    break;
// 1=Statistics 
  case 1:
    help_text1 = wxT("Statistics: click and drag mouse left button to select the box");
    LimitsBoxType1 = 2;
    n_PointsRequired1 = 2;
    break;
// 2=Astrometry 
  case 2:
    help_text1 = wxT("Astrometry: click and drag mouse left button to select the box");
    LimitsBoxType1 = 2;
    n_PointsRequired1 = 2;
    break;
// 3=Photometry
  case 3:
    help_text1 = wxT("Photometry: click and drag mouse left button to select the box");
    LimitsBoxType1 = 2;
    n_PointsRequired1 = 2;
    break;
// 4=Add a label 
  case 4:
    help_text1 = wxT("Add a label: click left button to position this label");
    LimitsBoxType1 = 0;
    n_PointsRequired1 = 1;
    break;
// 5=Remove a label 
  case 5:
    help_text1 = wxT("Remove a label: click left button to choose this label");
    LimitsBoxType1 = 0;
    n_PointsRequired1 = 1;
    break;
// 18=BoxLimits/ZoomIn 
  case 18:  
    help_text1 = wxT("BoxLimits: click and drag mouse left button to select the box");
    LimitsBoxType1 = 2;
    n_PointsRequired1 = 2;
    break;
}

// Set output values:
*limits_box_type = LimitsBoxType1;
*n_points_required = n_PointsRequired1;

return(0);
}
/******************************************************************
* Processing with box limits obtained by JLP_wxGseg_Canvas:
* Virtual routine of JLP_wx_gsProc: should be defined! 
*
* InternalProcessingMode
* -1=None 18=BoxLimits/ZoomIn 1=Statistics, 2=Astrometry 3=Photometry
* 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
* 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
* 13=Add a ring 14=Remove a shape
*
* Input:
*  x_down, y_down, n_down: x,y and number of points entered by pressing 
*                          the mouse down 
*  x_up, y_up, n_up: x,y and number of points entered by releasing 
*                          the mouse 
*  label: label to be plotted (used when a label mode is selected)
********************************************************************/
int JLP_wx_gsProc1::DataProcessing(double *x_down, double *y_down, int n_down, 
                                    double *x_up, double *y_up, int n_up, 
                                    wxString label)

{
double *xplot0, *yplot0; 
double x1_1, y1_1, x1_2, y1_2;
int status, npts0, icurve, in_frame;
char results[512], label0[128];
wxString str1;

str1.Alloc(512);

if(n_down != 1 || n_up != 1) { 
   fprintf(stderr,"JLP_wx_gsProc1::DataProcessing:Error: n_down=%d n_up=%d\n",
           n_down, n_up);
   return(-1);
   }

x1_1 = x_down[0];
y1_1 = y_down[0];

if(n_PointsRequired1 == 2) {
 x1_2 = x_up[0];
 y1_2 = y_up[0];
 }

#ifdef OLD_VERSION
 icurve = 0;
 status = m_wxgseg_canvas1->GetPlotData(&xplot0, &yplot0, &npts0, icurve);
 if(status != 0) return(-1);
#endif

#ifdef DEBUG
printf("JLP_wx_gsProc1/Dataprocessing/nPointsRequired=%d x1=%f y1=%f x2=%f y2=%f\n", 
        n_PointsRequired1, x1_1, y1_1, x1_2, y1_2);
#endif

/* InteractiveProcessingMode :
* -1=None 0=Automatic thresholds 1=Statistics, 2=Astrometry 3=Photometry
* 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
* 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
* 13=Add a ring 14=Remove a shape 15=Rotate a shape 16=Magnify a shape
* 17=Move a shape 18=ZoomIn
*/

switch(processing_mode1) {
 case 0:
   status = GetRectangleBoxLimits1(&x1_1, &y1_1, &x1_2, &y1_2);
#ifdef OLD_VERSION
   if(!status) {
      if(!status) m_wxgseg_canvas1->SetBoxLimits(x1_1, x1_2, y1_1, y1_2);
      }
#endif
   break;
 case 1:
   status = GetRectangleBoxLimits1(&x1_1, &y1_1, &x1_2, &y1_2);
#ifdef OLD_VERSION
   if(!status) {
      status = StatisticsFromBox1(xplot0, yplot0, npts0, x1_1, x1_2, results);
// Open a pop-up window:
      if(!status) { 
       str1 = wxString(results);
       wxMessageBox(str1, _T("Statistics"), wxICON_INFORMATION | wxOK );
       }
      } 
#endif
   break;
 case 2:
   status = GetRectangleBoxLimits1(&x1_1, &y1_1, &x1_2, &y1_2);
#ifdef OLD_VERSION
   if(!status) {
      status = AstrometryInBox1(xplot0, yplot0, npts0, x1_1, x1_2, results);
// Open a pop-up window:
      if(!status) { 
       str1 = wxString(results);
       wxMessageBox(str1, _T("Astrometry"), wxICON_INFORMATION | wxOK );
       }
      }
#endif
   break;
 case 3:
   status = GetRectangleBoxLimits1(&x1_1, &y1_1, &x1_2, &y1_2);
#ifdef OLD_VERSION
   if(!status) {
      status = PhotometryInBox1(xplot0, yplot0, npts0, x1_1, x1_2, results);
// Open a pop-up window:
      if(!status) { 
        str1 = wxString(results);
        wxMessageBox(str1, _T("Photometry"), wxICON_INFORMATION | wxOK );
        }
      }
#endif
   break;
 case 4:
   status = m_wxgseg_canvas1->Callback_ViewLabelContours(x1_1, y1_1, 
                                                         label0);
   if(status == 0) str1 = wxString(label0);
   else str1 = label;
   m_wxgseg_canvas1->AddLabel(str1, x1_1, y1_1);
// Update screen:
   m_wxgseg_canvas1->RedrawToBackupDC();
   break;
 case 5:
   m_wxgseg_canvas1->RemoveLabel(x1_1, y1_1);
   break;
 case 18:
   status = GetRectangleBoxLimits1(&x1_1, &y1_1, &x1_2, &y1_2);
   if(!status) m_wxgseg_canvas1->Callback_ZoomIn(x1_1, y1_1, x1_2, y1_2);
   break;
}

#ifdef OLD_VERSION
delete[] xplot0;
delete[] yplot0;
#endif
return(0);
}
