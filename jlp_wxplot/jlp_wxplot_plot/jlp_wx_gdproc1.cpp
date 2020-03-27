/*************************************************************************
* jlp_wx_gdproc1.cpp
* JLP_wx_GDProc1 derived from JLP_wx_GDProc abstract class
* and used by JLP_iGDev_wxWID (image Graphic Device)
* Image processing with data points entered interactively
*
* JLP
* Version 19/05/2017
**************************************************************************/
/* To define the JLP_wx_GDProc1 class */
#include "jlp_wx_gdproc1.h"

#include "jlp_wx_image1.h" // JLP_wxImage1 class
#include "jlp_wxgplot_dlg.h"  // JLP_wxGPlot_Dlg

#include "jlp_process_images.h" // GetRectangleBoxLimits2(), StatisticsFromBox2
#include "jlp_process_curves.h" // GetRectangleBoxLimits1(), StatisticsFromBox1

//#define DEBUG

/**************************************************************************
* Constructor
*
**************************************************************************/
JLP_wx_GDProc1::JLP_wx_GDProc1(JLP_GDev_wxWID* gdev_wxwid)
{
int pmode;

m_gdev_wxwid1 = gdev_wxwid;

// To avoid memory problems with long strings:
help_text1.Alloc(512);

// Initialize help text:
help_text1 = wxT("");

// Dummy selection for initialisation only:
pmode = -1;
SetNewProcessingMode(pmode);

return;
}
/**************************************************************************
*
* InteractiveProcessingMode :
* -1=None 0=Automatic thresholds 1=Statistics, 2=Astrometry 3=Photometry
* 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
* 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
* 13=Add a ring 14=Remove a shape 15=Rotate a shape 16=Magnify a shape
* 17=Move a shape 18=ZoomIn 19=DisplayCoordinates 20=LabelContours
*
* Input:
*  processing_mode: interactive processing mode
*
**************************************************************************/
int JLP_wx_GDProc1::SetNewProcessingMode(int processing_mode0)
{
int gdev_graphic_type0, plot_with_curves_only;

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
gdev_graphic_type0 = m_gdev_wxwid1->GDevGraphicType();
if((gdev_graphic_type0 == 1) || (gdev_graphic_type0 == 4)) {
  plot_with_curves_only = 1;
  } else {
  plot_with_curves_only = 0;
  }

// Save to private variable:
ProcessingMode1 = processing_mode0;

// Set limits box type: 0=none 1=line, 2=rectangle, 3=circle, 4=ellipse, 5=ring
// and number of points that are required:
switch(processing_mode0) {
  case -1:
    help_text1 = wxT("");
    LimitsBoxType1 = 0;
    n_PointsRequired1 = 0;
    break;
// 0=Automatic thresholds
  case 0:
    help_text1 = wxT("Automatic thresholds: click and drag mouse left button to select box");
    LimitsBoxType1 = 2;
    n_PointsRequired1 = 2;
    break;
// 1=Statistics
  case 1:
    help_text1 = wxT("Statistics: click and drag mouse left button to select box");
    LimitsBoxType1 = 2;
    n_PointsRequired1 = 2;
    break;
// 2=Astrometry (circle (=3) for images, rectangle (=2) for curves)
  case 2:
    help_text1 = wxT("Astrometry: click and drag mouse left button to select circle");
    if(plot_with_curves_only)
      LimitsBoxType1 = 2;
    else
      LimitsBoxType1 = 3;
    n_PointsRequired1 = 2;
    break;
// 3=Photometry
  case 3:
    help_text1 = wxT("Photometry: click and drag mouse left button to select circle");
    if(plot_with_curves_only)
      LimitsBoxType1 = 2;
    else
      LimitsBoxType1 = 3;
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
// 6=Add the scale bar
  case 6:
    help_text1 = wxT("Add a scale bar: click left button to position it");
    LimitsBoxType1 = 0;
    n_PointsRequired1 = 1;
    break;
// 7=Add the North-East
  case 7:
    help_text1 = wxT("Add North-East: click and drag left button to select circle");
    LimitsBoxType1 = 3;
    n_PointsRequired1 = 2;
    break;
// 8=Slice
  case 8:
    help_text1 = wxT("Slice: click and drag left button to select the line");
    LimitsBoxType1 = 1;
    n_PointsRequired1 = 2;
    break;
// 9=Add a line (shape)
  case 9:
    help_text1 = wxT("Add a line: click and drag left button to select the line");
    LimitsBoxType1 = 1;
    n_PointsRequired1 = 2;
    break;
// 10=Add a rectangle (shape)
  case 10:
    help_text1 = wxT("Add a rectangle: click and drag left button");
    LimitsBoxType1 = 2;
    n_PointsRequired1 = 2;
    break;
// 11=Add a circle (shape)
  case 11:
    help_text1 = wxT("Add a circle: click and drag left button");
    LimitsBoxType1 = 3;
    n_PointsRequired1 = 2;
    break;
// 12=Add an ellipse (shape)
  case 12:
    help_text1 = wxT("Add an ellipse: click and drag left button");
    LimitsBoxType1 = 4;
    n_PointsRequired1 = 2;
    break;
// 13=Add a ring (shape)
  case 13:
    help_text1 = wxT("Add a ring: click and drag left button");
    LimitsBoxType1 = 5;
    n_PointsRequired1 = 2;
    break;
// 14=Remove a shape
  case 14:
    help_text1 = wxT("Removal: click left button to select the shape");
    LimitsBoxType1 = 0;
    n_PointsRequired1 = 1;
    break;
// 15=Rotate a shape
  case 15:
    help_text1 = wxT("Rotation: click and drag left button from edge");
    LimitsBoxType1 = 0;
    n_PointsRequired1 = 2;
    break;
// 16=Magnify a shape
  case 16:
    help_text1 = wxT("Removal: click and drag left button from edge");
    LimitsBoxType1 = 0;
    n_PointsRequired1 = 2;
    break;
// 17=Move a shape
  case 17:
    help_text1 = wxT("Motion: click and drag left button from edge");
    LimitsBoxType1 = 0;
    n_PointsRequired1 = 2;
    break;
// 18=ZoomIn
  case 18:
    help_text1 = wxT("ZoomIn: click and drag mouse left button to select box");
    LimitsBoxType1 = 2;
    n_PointsRequired1 = 2;
    break;
// 19=DisplayCoordinates
  case 19:
    help_text1 = wxT("DisplayCoordinates: move mouse and click inside the frame");
    LimitsBoxType1 = 0;
    n_PointsRequired1 = 1;
    break;
// 20=LabelContours
  case 20:
    help_text1 = wxT("LabelContours: move mouse and click on the contour");
    LimitsBoxType1 = 0;
    n_PointsRequired1 = 1;
    break;
}

return(0);
}
/******************************************************************
* Processing with box limits obtained by JLP_iGDev_wxWID:
* Virtual routine of JLP_wx_GDProc: should be defined!
*
* InteractiveProcessingMode :
* -1=None 0=Automatic thresholds 1=Statistics, 2=Astrometry 3=Photometry
* 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
* 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
* 13=Add a ring 14=Remove a shape 15=Rotate a shape 16=Magnify a shape
* 17=Move a shape 18=ZoomIn 19=DisplayCoordinates 20=LabelContours
*
* Input:
*  x_down, y_down, n_down: (device coord) x,y and number of points
*                          entered by pressing the mouse down
*  x_up, y_up, n_up: (device coord) x,y and number of points entered
                      by releasing the mouse
*  label: label to be plotted (used when a label mode is selected)
********************************************************************/
int JLP_wx_GDProc1::DataProcessing(double *x_down, double *y_down, int n_down,
                                   double *x_up, double *y_up, int n_up,
                                    wxString wxlabel)
{
double *xplot0, *yplot0, *errorx0, *errory0, *array0; 
double *xplot_slice, *yplot_slice, *errorx_slice, *errory_slice;
double user_x1, user_x2, user_y1, user_y2;
double dev_x1, dev_x2, dev_y1, dev_y2;
double xc, yc, radius, x_data, y_data, z_data;
int i, status, nx0, ny0, npts0, icurve, nplot_slice, shape_type;
char results[512], title0[128], xlabel0[128], ylabel0[128], label0[128];
wxString str1;
JLP_wxGPlot_Dlg *my_popup_plot;
int in_frame, gdev_graphic_type0, keypress_inc = 0;

str1.Alloc(512);

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
gdev_graphic_type0 = m_gdev_wxwid1->GDevGraphicType();

// At least one complete pair of points (one down + one up) is needed:
if(n_down != 1 || n_up != 1) {
   fprintf(stderr,"JLP_wx_GDProc1::DataProcessing:Error: n_down=%d n_up=%d\n",
           n_down, n_up);
   return(-1);
   }

array0 = NULL;
xplot0 = NULL;
yplot0 = NULL;
errorx0 = NULL;
errory0 = NULL;
npts0 = 0;
nx0 = 0;
ny0 = 0;
icurve = 0;
status = -1;
// graphic_type=1 : jlp_splot_curves
if(gdev_graphic_type0 == 1) {
// In "jlp_splot/jlp_gdev_curves_process.cpp"
    status = m_gdev_wxwid1->GetCurveData(&xplot0, &yplot0, &errorx0, &errory0,
                                         &npts0, icurve);
  } else if((gdev_graphic_type0 == 2) || (gdev_graphic_type0 == 3)) {
// In "jlp_wxplot/jlp_gdev_wxwid_utils.cpp"
    status = m_gdev_wxwid1->GetImageArray(&array0, &nx0, &ny0);
  } else if((gdev_graphic_type0 == 5) || (gdev_graphic_type0 == 7)) {
    status = m_gdev_wxwid1->Gseg_GetImageArray(&array0, &nx0, &ny0);
  } else {
    status = m_gdev_wxwid1->Gseg_GetCurveData(&xplot0, &yplot0, &npts0, icurve);
  }
if(status != 0) {
  fprintf(stderr, "DataProcessing/Error retrieving data (graphic_type=%d)\n",
          gdev_graphic_type0);
  return(-1);
  }

// Load the device coordinates that have been loaded into x_down/y_down
dev_x1 = x_down[0];
dev_y1 = y_down[0];
// Conversion to user coordinates
 m_gdev_wxwid1->ConvDevToUser(dev_x1, dev_y1, &user_x1, &user_y1, &in_frame);

if(n_PointsRequired1 == 2) {
  dev_x2 = x_up[0];
  dev_y2 = y_up[0];
  m_gdev_wxwid1->ConvDevToUser(dev_x2, dev_y2, &user_x2, &user_y2, &in_frame);
  } else {
  user_x2 = 0;
  user_y2 = 0;
  }

#ifdef DEBUG
printf("gdproc1/DataProcessing/user_x1/y1: %f %f user_x2/y2 : %f %f dev x1/y1: %f %f dev x2/y2 : %f %f \n",
        user_x1, user_y1, user_x2, user_y2, dev_x1, dev_x2, dev_y1, dev_y2);
#endif

/* InteractiveProcessingMode :
* -1=None 0=Automatic thresholds 1=Statistics, 2=Astrometry 3=Photometry
* 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
* 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
* 13=Add a ring 14=Remove a shape 15=Rotate a shape 16=Magnify a shape
* 17=Move a shape 18=ZoomIn 19=DisplayCoordinates 20=LabelContours
*/

switch(ProcessingMode1) {
 case 0:
// Box limits for curves
      if(gdev_graphic_type0 == 1) {
// Box limits for curves:
         status = GetRectangleBoxLimits1(&user_x1, &user_y1, &user_x2,
                                         &user_y2);
         if(status == 0) {
          m_gdev_wxwid1->SetBoxLimits(user_x1, user_x2, user_y1, user_y2,
                                      0., 1.);
          }
       } else if((gdev_graphic_type0 == 2) || (gdev_graphic_type0 == 3)) {
// Box limits for images (with nx0, ny0...):
       status = GetRectangleBoxLimits2(&user_x1, &user_y1, &user_x2,
                                       &user_y2, nx0, ny0);
// ITT box selection for images
        if(status == 0) {
        m_gdev_wxwid1->GDevSet_ITT_Thresh(wxT("FromBox"), 0., 0.,
                                          user_x1, user_y1, user_x2, user_y2);
        }
       }
   break;
// Statistics for curves and images
 case 1:
      status = -1;
// Statistics for curves
      if(gdev_graphic_type0 == 1) {
        status = GetRectangleBoxLimits1(&user_x1, &user_y1, &user_x2, &user_y2);
        if(status == 0) {
          status = StatisticsFromBox1(xplot0, yplot0, npts0, user_x1,
                                      user_x2, results);
          }
      } else if((gdev_graphic_type0 == 2) || (gdev_graphic_type0 == 3)) {
      status = GetRectangleBoxLimits2(&user_x1, &user_y1, &user_x2, &user_y2,
                                      nx0, ny0);
// Statistics for images
      status = StatisticsFromBox2(array0, nx0, ny0, user_x1, user_y1,
                                  user_x2, user_y2, results);
      }
// Open a pop-up window:
     if(status == 0) {
      str1 = wxString(results);
      wxMessageBox(str1, _T("Statistics"), wxICON_INFORMATION | wxOK );
      }
   break;
 case 2:
      status = -1;
// Astrometry for curves and images
      if(gdev_graphic_type0 == 1) {
       status = GetRectangleBoxLimits1(&user_x1, &user_y1, &user_x2, &user_y2);
       if(status == 0) {
// Astrometry for curves
         status = AstrometryInBox1(xplot0, yplot0, npts0, user_x1, user_x2,
                                   results);
         }
      } else if((gdev_graphic_type0 == 2) || (gdev_graphic_type0 == 3)) {
      status = CircleFromBox2(nx0, ny0, user_x1, user_y1, user_x2, user_y2,
                              &xc, &yc, &radius);
       if(status == 0) {
// Astrometry for images
        status = AstrometryInCircle2(array0, nx0, ny0, xc, yc, radius, results);
       }
      }
// Open a pop-up window:
    if(status == 0) {
      str1 = wxString(results);
      wxMessageBox(str1, _T("Astrometry"), wxICON_INFORMATION | wxOK );
     }
   break;
 case 3:
      status = -1;
// Photometry for curves and images
      if(gdev_graphic_type0 == 1) {
        status = GetRectangleBoxLimits1(&user_x1, &user_y1, &user_x2, &user_y2);
        if(status == 0) {
// Photometry for curves
          status = PhotometryInBox1(xplot0, yplot0, npts0, user_x1,
                                    user_x2, results);
          }
      } else if((gdev_graphic_type0 == 2) || (gdev_graphic_type0 == 3)) {
        status = CircleFromBox2(nx0, ny0, user_x1, user_y1, user_x2,
                                user_y2, &xc, &yc, &radius);
        if(status == 0) {
// Photometry for images
          status = PhotometryInCircle2(array0, nx0, ny0, xc, yc, radius,
                                       results);
          }
       }
// Open a pop-up window:
   if(status == 0) {
      str1 = wxString(results);
      wxMessageBox(str1, _T("Photometry"), wxICON_INFORMATION | wxOK );
      }
   break;
// Add label
 case 4:
// Add label for jlp_wxplot:
   if(gdev_graphic_type0 <= 3) {
// User coordinates here:
     m_gdev_wxwid1->AddLabel(wxlabel, user_x1, user_y1);
// Update screen:
     m_gdev_wxwid1->RedrawToBackupDC(1901);
     }
   break;
// Remove label
 case 5:
// User coordinates here:
   m_gdev_wxwid1->RemoveLabel(user_x1, user_y1);
   break;
// Move Scale bar
 case 6:
// User coordinates here:
   m_gdev_wxwid1->MoveScaleBar(user_x1, user_y1);
   break;
// North-East label
 case 7:
// For jlp_wxplot images
   if((gdev_graphic_type0 == 2) || (gdev_graphic_type0 == 3)) {
     status = CircleFromBox2(nx0, ny0, user_x1, user_y1, user_x2,
                             user_y2, &xc, &yc, &radius);
     if(status == 0) {
        m_gdev_wxwid1->UpdateNorthEastLabel(radius, xc, yc);
     }
   }
   break;
// Slice
 case 8:
// For jlp_wxplot images
   if((gdev_graphic_type0 == 2) || (gdev_graphic_type0 == 3)) {
// Diagonal is the maximum length for the slice:
      nplot_slice = 1 + sqrt((double)(nx0*nx0 + ny0*ny0));
      xplot_slice = new double[nplot_slice];
      yplot_slice = new double[nplot_slice];
      errorx_slice = new double[nplot_slice];
      errory_slice = new double[nplot_slice];
      status = SliceFromLine2(array0, nx0, ny0, user_x1, user_y1, user_x2,
                              user_y2, xplot_slice, yplot_slice, &nplot_slice,
                              title0, xlabel0, ylabel0);
      for(i = 0; i < nplot_slice; i++) {
        errorx_slice[i] = 0.;
        errory_slice[i] = 0.;
        }
// Open a pop-up graphic window:
      if(status == 0) {
// Read the header of the FITS file to fill the result latex line
       my_popup_plot = new JLP_wxGPlot_Dlg(xplot_slice, yplot_slice,
                                           errorx_slice, errory_slice,
                                           nplot_slice, wxString(title0),
                                           wxString(xlabel0),
                                           wxString(ylabel0));
       my_popup_plot->ShowModal();
// Cleanup (problem here)
//     my_popup_plot->Destroy();
       }
      delete[] xplot_slice;
      delete[] yplot_slice;
      delete[] errorx_slice;
      delete[] errory_slice;
    }
   break;
// shape_type: 1=line 2=rectangle 3=circle 4=ellipse 5=ring
// Add a line
 case 9:
   shape_type = 1;
   m_gdev_wxwid1->AddShape(user_x1, user_y1, user_x2, user_y2, shape_type);
   break;
// Add a rectangle
 case 10:
   shape_type = 2;
   m_gdev_wxwid1->AddShape(user_x1, user_y1, user_x2, user_y2, shape_type);
   break;
// Add a circle
 case 11:
   shape_type = 3;
   m_gdev_wxwid1->AddShape(user_x1, user_y1, user_x2, user_y2, shape_type);
   break;
// Add an ellipse
 case 12:
   shape_type = 4;
   m_gdev_wxwid1->AddShape(user_x1, user_y1, user_x2, user_y2, shape_type);
   break;
// Add a ring
 case 13:
   shape_type = 5;
   m_gdev_wxwid1->AddShape(user_x1, user_y1, user_x2, user_y2, shape_type);
   break;
// Remove a shape
 case 14:
   m_gdev_wxwid1->RemoveShape(user_x1, user_y1);
   break;
// Rotate a shape
 case 15:
   m_gdev_wxwid1->RotateShape(user_x1, user_y1, user_x2, user_y2);
   break;
// Magnify a shape
 case 16:
   m_gdev_wxwid1->MagnifyShape(user_x1, user_y1, user_x2, user_y2);
   break;
// Move a shape
 case 17:
   m_gdev_wxwid1->MoveShape(user_x1, user_y1, user_x2, user_y2);
   break;
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
// 18=ZoomIn
 case 18:
// For jlp_wxplot images
   if((gdev_graphic_type0 == 2) || (gdev_graphic_type0 == 3)) {
     m_gdev_wxwid1->SetBoxLimits(user_x1, user_x2, user_y1, user_y2, 0, 1.);
// GSEG curves or 2d-images :
   } else if ((gdev_graphic_type0 == 4)  || (gdev_graphic_type0 == 5)) {
     m_gdev_wxwid1->Callback_ZoomIn(dev_x1, dev_y1, dev_x2, dev_y2);
   }
   break;
// 19=DisplayCoordinates
 case 19:
   if(gdev_graphic_type0 >= 4) {
     status = m_gdev_wxwid1->Callback_ViewDisplayCoordinates(dev_x1, dev_y1,
                                                         &x_data, &y_data,
                                                         &z_data, keypress_inc);
     if(status == 0) {
// Write to logbook window (and record on the logbook file):
       if(z_data != -1234.0)
         str1.Printf(wxT("Coordinates: %f %f %f\n"), x_data, y_data, z_data);
       else
         str1.Printf(wxT("Coordinates: %f %f\n"), x_data, y_data);
       m_gdev_wxwid1->GDWX_WriteToLogbook(str1, true);
       }
   }
   break;
// 20=LabelContours of 2D images
 case 20:
// Add label for GSEG:
   if(gdev_graphic_type0 == 5) {
     status = m_gdev_wxwid1->Callback_ViewLabelContours(dev_x1, dev_y1,
                                                        label0, keypress_inc);
     if(status == 0) {
       str1 = wxString(label0);
       m_gdev_wxwid1->AddLabel_gseg(str1, dev_x1, dev_y1);
       }
     }
   break;
}

if(xplot0 != NULL) delete[] xplot0;
if(yplot0 != NULL) delete[] yplot0;
if(errorx0 != NULL) delete[] errorx0;
if(errory0 != NULL) delete[] errory0;
if(array0 != NULL) delete[] array0;
return(0);
}
