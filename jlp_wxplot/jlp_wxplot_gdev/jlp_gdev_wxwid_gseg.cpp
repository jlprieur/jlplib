/*************************************************************************
* JLP_GDev_wxWID Class (for curves and images)
*
* JLP
* Version 18/08/2017
**************************************************************************/
#include "jlp_gdev_wxwid.h"
#include "jlp_gsegraf.h"            // JLP_GSEG_PLOT_DATA, GSEG_AXIS_DATA
#include "jlp_gseg_plot_data1.h"    // jlp_gseg_init_plot_data()
#include "jlp_gseg_axis_data1.h"    // jlp_gseg_init_axis_data()
#include "jlp_wxgdev_popup.h"       // For JLP_wxGDev_Popup

/*
*/
#define DEBUG
#define DBLE_MIN_VALUE 1.e-12

/*************************************************************************
* Initialize drawing setup with a parameter file 
*
*************************************************************************/
int JLP_GDev_wxWID::InitializeGsegPlotWithParamFile(char *param_filename0)
{
char *save_filename0;
int close_flag0;

// This object should have been created by open_device
 if(jlp_gseg_wxwid1 == NULL) {
   fprintf(stderr, "InitializeGsegPlotWithParamFile/Error, jlp_gseg_wxwid is NULL: (should have been created by open_device before calling this routine...)\n");
   return(-1);
  }

// Save to private variable (used for the title of the popup frame)
 strcpy(filename_1, param_filename0);

 if(jlp_gsegraf1 != NULL) delete jlp_gsegraf1;
 jlp_gsegraf1 = new JLP_Gsegraf(jlp_gseg_wxwid1, param_filename0,
                                &save_filename0, &close_flag0);
 gseg_is_activated = 1;
 ncurves_1 = 0;
 c_image1 = NULL;

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
 jlp_gsegraf1->Get_graphic_type(&Jgc0.gdev_graphic_type);

// Initialize plot setup and popup menu:
 InitializePlotSetupWithGDevGraphicType(Jgc0.gdev_graphic_type);

return(0);
}
/*************************************************************************
* Load a double precision image array to jlp_gsegraf plot 
* 
* INPUT:
* contours option: 
* 0="none"
* 1="black contours"
* 2="white contours"
* 3="auto-colored contours"
*************************************************************************/
int JLP_GDev_wxWID::LoadDbleImage1ToGsegPlot(GSEG_PLOT_DATA *gseg_pltdata0,
                                             GSEG_AXIS_DATA *gseg_axdata0,
                                             int gseg_plot_type0,
                                             char *axis_type0,
                                             const  int contours_option0,
                                             const int pen_width0)
{
int i, ix, iy, gdev_graphic_type0; 

#ifdef DEBUG
printf("LoadDbleImage1ToGsegPlot/gseg_plot_type0=%d axis_type0=%s\n",
        gseg_plot_type0, axis_type0);
printf("LoadDbleImage1ToGsegPlot/nx_1=%d ny_1=%d\n", nx_1, ny_1);
#endif

/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
  if (gseg_plot_type0 < 3 || gseg_plot_type0 > 5) {
   fprintf(stderr, "LoadDbleImage1ToGsegPlot/Error: gseg_plot_type0=%d\n",
           gseg_plot_type0);
   return(-1);
   }

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
 gdev_graphic_type0 = 5; // By default...

// Load selected options:
  strcpy(gseg_axdata0->axis_type, axis_type0);
  jlp_gseg_init_plot_data(gdev_graphic_type0, gseg_pltdata0);
  gseg_pltdata0->gseg_plot_type = gseg_plot_type0;

// Pen width (default is 1):
  gseg_pltdata0->style_size = pen_width0;

// 3="contour"
  if (gseg_plot_type0 == 3) {

/* contours option:
* 0="none"
* 1="black 2D contours"
* 2="white 2D contours"
* 3="auto-colored 2D contours"
* 4="3D contours"
*/
// gseg_plot_type0="contour" and axis_type0="linear" should be associated 
// with styleflag = 1(=constant color), 3(=constant color) 
// or 7(=auto, variable colors)
     switch(contours_option0) { 
       default:
       case 0:
         break;
// 1="black 2D contours"
       case 1:
         gseg_pltdata0->style_flag = 1;            // constant colored contours
         gseg_pltdata0->style_color1 = 0x000000FF; // opaque black 
         break;
// 2="white 2D contours"
       case 2:
         gseg_pltdata0->style_flag = 1;            // constant colored contours
         gseg_pltdata0->style_color1 = 0xFFFFFFFF; // opaque white
         break;

// 3="auto-colored 2D contours"
       case 3:
         gseg_pltdata0->style_flag = 7;            // auto-colored contours 
         break;
 // 4="3D contours"
       case 4:
// gseg_plot_type0="contour" and axis_type0="3d"
// should be associated with styleflag = 2, 4, 5 or 6
// colors read directly with style_color1/2
         gseg_pltdata0->style_flag = 6; 
         gseg_pltdata0->contour3d_color = 0x000000FF; // opaque black
         gseg_pltdata0->style_color1 = 0xD0723EFF; // copper 
         gseg_pltdata0->style_color2 = 0x9C562EFF; // dark copper 
       }
    
// By default set ncontours to 9 contours...
    gseg_pltdata0->ncontours = 9;

    gseg_pltdata0->nxcontour = nx_1;
    gseg_pltdata0->nycontour = ny_1;
    gseg_pltdata0->xcontour = new double[nx_1];
    gseg_pltdata0->ycontour = new double[ny_1]; 
    gseg_pltdata0->zcontour = new double[nx_1 * ny_1];
    for(ix = 0; ix < nx_1; ix++)
      gseg_pltdata0->xcontour[ix] = (double)ix;
    for(iy = 0; iy < ny_1; iy++)
      gseg_pltdata0->ycontour[iy] = (double)iy;
    
    for(iy = 0; iy < ny_1; iy++) { 
      for(ix = 0; ix < nx_1; ix++) { 
// WARNING: Gsegraph swaps X/Y:
       gseg_pltdata0->zcontour[iy + ix * ny_1] = dble_image_1[ix + iy * nx_1];
      }
    }

// 5="mesh"
  } else if (gseg_plot_type0 == 5) {

// gseg_plot_type0="mesh" and axis_type0="3d"
// should be associated with styleflag = 2, 4, 5, 6  or 7
    gseg_pltdata0->style_flag = 2;
// Mesh color:
    gseg_pltdata0->mesh_color = 0xFF0000FF;
    gseg_pltdata0->alpha_color = 0xC0;

    gseg_pltdata0->nxmesh = nx_1;
    gseg_pltdata0->nymesh = ny_1;
    gseg_pltdata0->xmesh = new double[nx_1];
    gseg_pltdata0->ymesh = new double[ny_1];
    gseg_pltdata0->zmesh = new double[nx_1 * ny_1];
    for(ix = 0; ix < nx_1; ix++)
      gseg_pltdata0->xmesh[ix] = (double)ix;
    for(iy = 0; iy < ny_1; iy++)
      gseg_pltdata0->ymesh[iy] = (double)iy;

    for(iy = 0; iy < ny_1; iy++) {
      for(ix = 0; ix < nx_1; ix++) {
// WARNING: Gsegraph swaps X/Y:
       gseg_pltdata0->zmesh[iy + ix * ny_1] = dble_image_1[ix + iy * nx_1];
      }
    }

// 4="color"
  } else {

// Set gseg_plot_type = 4 to "color"
    gseg_pltdata0->gseg_plot_type = 4;

// gseg_plot_type0="color" and axis_type0="3d"
// should be associated with styleflag = 7 (find polygon min. and max. z coords)
   if(!strcmp(axis_type0, "3d")) {
    gseg_pltdata0->style_flag = 7;
    } else {
// gseg_plot_type0="color" and axis_type0="linear"
// should be associated with styleflag = 8(bilinear interpolation) or 9(nearest)
    gseg_pltdata0->style_flag = 8;
    }

    gseg_pltdata0->nxcolor = nx_1;
    gseg_pltdata0->nycolor = ny_1;
    gseg_pltdata0->xcolor = new double[nx_1];
    gseg_pltdata0->ycolor = new double[ny_1];
    gseg_pltdata0->zcolor = new double[nx_1 * ny_1];
    for(ix = 0; ix < nx_1; ix++)
      gseg_pltdata0->xcolor[ix] = (double)ix;
    for(iy = 0; iy < ny_1; iy++)
      gseg_pltdata0->ycolor[iy] = (double)iy;

    for(iy = 0; iy < ny_1; iy++) {
      for(ix = 0; ix < nx_1; ix++) {
// WARNING: Gsegraph swaps X/Y:
       gseg_pltdata0->zcolor[iy + ix * ny_1] = dble_image_1[ix + iy * nx_1];
      }
    }
  } 

// set_axis_limits : 0 if automatic scale, 1 if axis_limits are to be used
  for(i = 0; i < 4; i++) gseg_axdata0->set_axis_limits[i] = 1;
  gseg_axdata0->axis_limits[0] = 0;
  gseg_axdata0->axis_limits[1] = nx_1 - 1;
  gseg_axdata0->axis_limits[2] = 0;
  gseg_axdata0->axis_limits[3] = ny_1 - 1;
  for(i = 4; i < 6; i++) gseg_axdata0->set_axis_limits[i] = 0;

// Set high contrast ITT scale by default
  gseg_axdata0->high_contrast_for_z_axis = 1;

/* DEBUG:
printf("LoadDbleImage1ToGsegPlot/axis_type=%s style_flag=%d\n", 
        axis_type0, gseg_pltdata0->style_flag);
*/
return(0);
}
/*************************************************************************
* 
*************************************************************************/
int JLP_GDev_wxWID::LoadCImage1ToGsegPlot(GSEG_PLOT_DATA *gseg_pltdata0,
                                          GSEG_AXIS_DATA *gseg_axdata0)
{
int i, ix, iy, nx1, ny1, gdev_graphic_type0;
double *array;

  if(c_image1 == NULL) return(-1); 

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
 gdev_graphic_type0 = 5;

// Set axis_type to "linear"
     strcpy(gseg_axdata0->axis_type, "linear");
     jlp_gseg_init_plot_data(gdev_graphic_type0, gseg_pltdata0);
/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
// Set gseg_plot_type = 4 to "color"
     gseg_pltdata0->gseg_plot_type = 4;
// gseg_plot_type0="color" and axis_type0="linear"
// should be associated 
// with styleflag = 8("bilinear interpolation") or 9("nearest")
     gseg_pltdata0->style_flag = 8;

     nx1 = c_image1->Get_nx1();
     ny1 = c_image1->Get_ny1();

// set_axis_limits : 0 if automatic scale, 1 if axis_limits are to be used
     for(i = 0; i < 6; i++) gseg_axdata0->set_axis_limits[i] = 0;

     gseg_pltdata0->nxcolor = nx1;
     gseg_pltdata0->nycolor = ny1;
     gseg_pltdata0->xcolor = new double[nx1];
     gseg_pltdata0->ycolor = new double[ny1];
     gseg_pltdata0->zcolor = new double[nx1 * ny1];
     for(ix = 0; ix < nx1; ix++)
        gseg_pltdata0->xcolor[ix] = (double)ix / (double)(nx1 - 1);
     for(iy = 0; iy < ny1; iy++)
        gseg_pltdata0->ycolor[iy] = (double)iy / (double)(ny1 - 1);

// Retrieve data from c_image1 and convert it with LUT/ITT settings

// Apply current settings (LUT, ITT, etc) to c_image1
     ApplyCurrentSettingsToCImage1();
     array = new double[nx1 * ny1];
// Get access to c_image1 data,i.e. read c_image1 data
     ReadCImage1Data(array, nx1, ny1);

     for(ix = 0; ix < nx1; ix++)
       for(iy = 0; iy < ny1; iy++)
       {
        gseg_pltdata0->zcolor[ix + iy * nx1] = array[ix + iy * nx1];
       }

delete[] array;

return(0);
}
/*************************************************************************
* Initialize drawing setup with a GSEG_PLOT_DATA structure 
*
*************************************************************************/
int JLP_GDev_wxWID::InitializeGsegPlotWithPlotData(GSEG_PLOT_DATA *gseg_pltdata0,
                                                   const int nplots0,
                                                   GSEG_AXIS_DATA gseg_axdata0)
{
// This object should have been created by open_device
 if(jlp_gseg_wxwid1 == NULL) {
   fprintf(stderr, "InitializeGsegPlotWithParamFile/Error, jlp_gseg_wxwid1 is NULL: (should have been created by open_device before calling this routine...)\n");
   return(-1);
  }

 if(jlp_gsegraf1 != NULL) delete jlp_gsegraf1;
 jlp_gsegraf1 = new JLP_Gsegraf(jlp_gseg_wxwid1, gseg_pltdata0, nplots0, 
                                gseg_axdata0); 
 ncurves_1 = 0;
 gseg_is_activated = 1;
 c_image1 = NULL;

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
 jlp_gsegraf1->Get_graphic_type(&Jgc0.gdev_graphic_type);

// Initialize plot setup and popup menu:
 InitializePlotSetupWithGDevGraphicType(Jgc0.gdev_graphic_type);

return(0);
}
/*************************************************************************
* Initialize the plot setup with the graphic type number 
* and use jlp_splot context only if required
*
*************************************************************************/
int JLP_GDev_wxWID::InitializePlotSetupWithGDevGraphicType(int gdev_graphic_type0)
{
int dev_x0, dev_y0, dev_width0, dev_height0;

#ifdef DEBUG
printf("InitializePlotSetupWithGraphicType: gdev_graphic_type=%d\n", gdev_graphic_type0);
#endif

 if(gdev_graphic_type0 >= 1 && gdev_graphic_type0 <= 8)
  Jgc0.gdev_graphic_type = gdev_graphic_type0;
 else 
  Jgc0.gdev_graphic_type = 1;

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
 switch(gdev_graphic_type0) 
  {
  case 1:
  case 2:
  case 3:
   gseg_is_activated = 0;
   break;
  case 4:
  default:
   gseg_is_activated = 1;
   break;
  }

if(gseg_is_activated == 1) {
// Get windows limits:
 jlp_gseg_wxwid1->GSEG_GetWindowLimits(&dev_x0, &dev_width0, &dev_y0, 
                                       &dev_height0);
// Update Jgc0 and mgc0 structures from Gsegraf settings:
 GDev_UpdateJgcFromGsegSettings(jlp_gsegraf1, dev_x0, dev_y0, dev_width0, 
                           dev_height0);
 }

// Update fonts with settings loaded in private variables:
 SetFontToBackupDC(font_name1, font_size1);

// Create a new popup menu corresponding to Jgc0.gdev_graphic_type:
 if(m_popup_menu1 != NULL) delete m_popup_menu1;
 m_popup_menu1 = new JLP_wxGDev_Popup(this, Jgc0.gdev_graphic_type);
// Connection of events to this window and to the scrolled window
// m_popup_menu1->ConnectAllPopupMenuEvents();

// Force screen updating:
  RedrawToBackupDC(7002);

return(0);
}
/*************************************************************************
* Close Postscript backup Device Context 
*
*************************************************************************/
void JLP_GDev_wxWID::CloseBackupPostscriptDC()
{

if(backup_pst_dc1 != NULL) {
  backup_pst_dc1->EndPage();
  backup_pst_dc1->EndDoc();
  delete backup_pst_dc1;
  backup_pst_dc1 = NULL;
// Update the pointer in jlp_gsgeg_wxwid0:
// This object should have been created by open_device
   if(jlp_gseg_wxwid1 == NULL) {
     fprintf(stderr, "CloseBackupPostscriptDC/Error, jlp_gseg_wxwid is NULL: (should have been created by open_device before calling this routine...)\n");
    } else {
    jlp_gseg_wxwid1->InitPostscriptDC(backup_pst_dc1);
    }
  }

if(backup_printer1 != NULL) {
  delete backup_printer1;
  backup_printer1 = NULL;
  }

return;
}
/*************************************************************************
* Open Postscript backup Device Context 
*
*************************************************************************/
int JLP_GDev_wxWID::OpenBackupPostscriptDC(char *save_filename0)
{
int status = -1;

// Close backup DC if present:
CloseBackupPostscriptDC();

// Open first the printer interface
backup_printer1 = new wxPrintData();

backup_printer1->SetPaperId(wxPAPER_A4);
backup_printer1->SetFilename(save_filename0);
backup_printer1->SetPrintMode(wxPRINT_MODE_FILE);
backup_pst_dc1 = new wxPostScriptDC(*backup_printer1);

if(backup_pst_dc1->IsOk() == true) {
// Update the pointer in jlp_gsgeg_wxwid0:
  jlp_gseg_wxwid1->InitPostscriptDC(backup_pst_dc1);
// Tell it where to find the AFM files
// backup_pst_dc1.GetPrintData().SetFontMetricPath(wxT("afm/"));
// Set the resolution in points per inch (the default is 720)
// backup_pst_dc1->SetResolution(1440);

// Path for Metric fonts: no longer possible
// Windows:
//  wxSetAFMPath("d:\\wxw161\\afm\\");
//  wxSetAFMPath("/usr/share/enscript/afm/");
//  backup_pst_dc1->GetPrintData().SetFontMetricpath(wxT("/usr/share/enscript/afm/"));
  backup_pst_dc1->StartDoc(wxT("JLP's program is printing now ..."));
  backup_pst_dc1->StartPage();
//  backup_pst_dc1->SetFont(new wxFontInfo(10).Family(wxFONTFAMILY_ROMAN));

/* DEBUG
  backup_pst_dc1->DrawLine(1, 1, 100, 100);
  backup_pst_dc1->SetBrush(*wxTRANSPARENT_BRUSH);
  backup_pst_dc1->DrawRectangle(200, 200, 1000, 1000);
*/
  status = 0;
  }

return(status);
}
/************************************************************************
* Interactive change of axis limits 
************************************************************************/
void JLP_GDev_wxWID::ViewChangeAxisLimits()
{
wxString s_values, s_question, result;
double xmin, xmax, ymin, ymax, zmin, zmax;
int dev_x0, dev_y0, dev_width0, dev_height0;
int status, dcflag;
char old_values_string[128], prompt_string[512];
char new_values_string[128], error_message[128];

if(initialized != 1234) return;

// Create the string that will be displayed for prompting new axes limits:
jlp_gsegraf1->GSEG_QuestionForChangingAxesLimits(prompt_string, 
                                                 old_values_string);

s_question = wxString(prompt_string);

s_values = wxString(old_values_string);

/* Open dialog box */
result = wxGetTextFromUser(s_question, _T("New limits:"), s_values, NULL);

if (result.IsEmpty()) return;

strcpy(new_values_string, result.char_str());

// Decode the input string to get the values of the axes limits
 status = jlp_gsegraf1->GSEG_DecodeStringForChangingAxesLimits(new_values_string, 
                                                 &xmin, &xmax, &ymin, &ymax, 
                                                 &zmin, &zmax, error_message);
 if (status != 0) {
  JLP_ErrorDialog(error_message);
  return;
  }

// Redraw plot with the new values of the axes limits 
// dcflag = display coordinates flag
 jlp_gsegraf1->GSEG_ReDrawPlot_Scaled(xmin, xmax, ymin, ymax, zmin, zmax,
                                         &dcflag);

// Get windows limits:
 jlp_gseg_wxwid1->GSEG_GetWindowLimits(&dev_x0, &dev_width0, &dev_y0, 
                                       &dev_height0);

// Update new settings to private values:
// (NB: limits may have changed from xmin, etc) 
 GDev_UpdateJgcFromGsegSettings(jlp_gsegraf1, dev_x0, dev_y0, dev_width0, 
                           dev_height0);

 wxGdev_Refresh();

return;
}
/************************************************************************
* Check if interactive contour labelling is possible
************************************************************************/
bool JLP_GDev_wxWID::ViewAxisRotateIsPossible()
{
int flag_2d, flag_3d, flag_2d_rect, flag_polar, flag_linear, flag_logx;
int flag_logy, flag_loglog, ncontours, nplots;
bool is_possible = false;

if(gseg_is_activated && jlp_gsegraf1 != NULL) {
  jlp_gsegraf1->GSEG_GetCurrentPlotSettings(&flag_2d, &flag_3d, &flag_2d_rect,
                                            &flag_polar, &flag_linear,
                                            &flag_logx, &flag_logy,
                                            &flag_loglog, &ncontours, &nplots);

  if(flag_3d == 1) {
    is_possible = true;
    }
}

return(is_possible);
}
/************************************************************************
* Interactive change of projection angles for 3d plot
* Return -5 if incompatible mode
************************************************************************/
int JLP_GDev_wxWID::ViewAxisRotate()
{
wxString s_values, s_question, result;
double  phi, theta;
char old_values_string[128], prompt_string[512];
char new_values_string[128];
char error_message[128];

if(ViewAxisRotateIsPossible() == false) return(-5);

// Create the string that will be displayed for prompting new axes limits:
jlp_gsegraf1->GSEG_QuestionForChangingAxesRotation(prompt_string,
                                                    old_values_string);

s_question = wxString(prompt_string);

s_values = wxString(old_values_string);

/* Open dialog box */
result = wxGetTextFromUser(s_question, _T("new projection angle values:"), 
                           s_values, NULL);

if (result.IsEmpty()) return(-1);

strcpy(new_values_string, result.char_str());

/* get view-direction data */
if ( sscanf(new_values_string, "%lf %lf", &phi, &theta) != 2 ) {
  strcpy(error_message, "incorrect number of values;\ntwo expected.");
  JLP_ErrorDialog(error_message);
  return(-1);
  }

if ( theta >= 0.0 && theta <= 90.0 ) {
// redraw plot with the new values of the axes limits
// dcflag = display coordinates flag
  jlp_gsegraf1->GSEG_ReDrawPlot3D_Rotated(phi, theta);
  } else {
  strcpy(error_message, "Elevation angle out of range.");
  JLP_ErrorDialog(error_message);
  return(-1);
  }

 wxGdev_Refresh();

return(0);
}
/************************************************************************
* Interactive display coordinates 
* Set processing mode to DisplayCoordinates 
*
* Return -5 if incompatible mode
************************************************************************/
int JLP_GDev_wxWID::Set_ViewDisplayCoordinates()
{

// Flag for displaying the coordinates 
 if((gseg_is_activated == false) || (jlp_gsegraf1 == NULL)) {
   return(-5);
   }

/* InteractiveProcessingMode :
* -1=None 0=BoxLimits 1=Statistics, 2=Astrometry 3=Photometry
* 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
* 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
* 13=Add a ring 14=Remove a shape 15=Rotate a shape 16=Magnify a shape
* 17=Move a shape 18=ZoomIn 19=DisplayCoordinates 20=LabelContours
*/

// ProcessingMode=19
  wxgdev_settings1.InternalProcessingMode = 19;

// Apply settings and update popup menu:
  if(m_popup_menu1 != NULL) m_popup_menu1->UpdatePopupMenu_InternalProcessingMode();

return(0);
}
/************************************************************************
* Called when processing mode is set to DisplayCoordinates 
*
* INPUT:
* dev_x10, dev_y10 : (device coord) location of the center of the label 
* keypress_inc : +1 if "up arrow", -1 if "down arrow", 0 other key
*
************************************************************************/
int JLP_GDev_wxWID::Callback_ViewDisplayCoordinates(const double dev_x10, 
                                                    const double dev_y10,
                                                    double *x_data, 
                                                    double *y_data, 
                                                    double *z_data,
                                                    const int keypress_inc)

{
int status, nplots;

// Check if OK:
 if(jlp_gsegraf1 == NULL) return(-1);

// Flag for displaying the coordinates 
 if(wxgdev_settings1.InternalProcessingMode != 19) return(-1);


/* Get number of color or contour plots */
 jlp_gsegraf1->GSEG_get_nplots(&nplots);

// Possibility of changing the image plot number, 
// by increasing gseg_iplot_coords1 
 gseg_iplot_coords1 += keypress_inc;
 if ( gseg_iplot_coords1 < 1 )
    gseg_iplot_coords1 = 1;
 else if ( gseg_iplot_coords1 > nplots )
    gseg_iplot_coords1 = nplots;

//  z_data : user coordinates
 status = jlp_gsegraf1->GSEG_UserFromDeviceCoord(dev_x10, dev_y10, 
                                                 gseg_iplot_coords1,
                                                 x_data, y_data, z_data);


/* DEBUG:
// char label_0[64];
//  sprintf(label_0, "%.3g %.3g %.3g", x_data, y_data, z_data);
printf("jlp_gdev_wxwid_gseg/Callback_ViewDisplayCoordinates: dev_x=%f dev_y=%f status=%d x_data=%f y_data=%f z_data=%f label=>%s<\n",
          dev_x10, dev_y10, status, x_data, y_data, z_data, label_0);
*/

return(status);
}
/************************************************************************
* Check if interactive contour labelling is possible 
************************************************************************/
bool JLP_GDev_wxWID::ViewLabelContoursIsPossible()
{
int status = -1, ncontour_plots0; 
bool is_possible = false;

if(gseg_is_activated && jlp_gsegraf1 != NULL) {
  status = jlp_gsegraf1->GSEG_get_ncontour_plots(&ncontour_plots0);
/** DEBUG
printf("JLP_GDev_wxWID::ViewLabelContoursIsPossible/status=%d ncontours=%d\n", 
        status, ncontour_plots0);
**/
  if((status == 0) || (ncontour_plots0 > 0)) {
    is_possible = true; 
    }
}

return(is_possible);
}
/************************************************************************
* Interactive contour labelling 
* Set processing mode to ContourLabelling
*
* Return -5 if incompatible mode
************************************************************************/
int JLP_GDev_wxWID::Set_ViewLabelContours()
{

// Flag for labelling the contours
if(ViewLabelContoursIsPossible() == false) {
  return(-5);
  }

/* InteractiveProcessingMode :
* -1=None 0=BoxLimits 1=Statistics, 2=Astrometry 3=Photometry
* 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
* 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
* 13=Add a ring 14=Remove a shape 15=Rotate a shape 16=Magnify a shape
* 17=Move a shape 18=ZoomIn 19=DisplayCoordinates 20=LabelContours
*/

// ProcessingMode=20 
  wxgdev_settings1.InternalProcessingMode = 20;

// Apply settings and update popup menu:
  if(m_popup_menu1 != NULL) m_popup_menu1->UpdatePopupMenu_InternalProcessingMode();

return(0);
}
/************************************************************************
* Interactive contour labelling 
* Called when processing mode is set to ContourLabelling
*
* INPUT:
* dev_x10, dev_y10 : (device coord) location of the center of the label 
* keypress_inc : +1 if "up arrow", -1 if "down arrow", 0 other key
*
************************************************************************/
int JLP_GDev_wxWID::Callback_ViewLabelContours(const double dev_x10, 
                                               const double dev_y10,
                                               char *label_0,
                                               const int keypress_inc)

{
int status, ncontour_plots0;
double z_data, nearest_contour_value;

// Check if OK:
 if(jlp_gsegraf1 == NULL) return(-1);

// Flag for labelling the contours
 if(wxgdev_settings1.InternalProcessingMode != 20) return(-1);

 status = jlp_gsegraf1->GSEG_get_ncontour_plots(&ncontour_plots0);
 if((status != 0) || (ncontour_plots0 == 0)) return(-1); 

// Check icontour_plots value 
 gseg_iplot_contours1 = gseg_iplot_contours1 + keypress_inc;
 if ( gseg_iplot_contours1 < 1 )
    gseg_iplot_contours1 = 1;
 else if ( gseg_iplot_contours1 > ncontour_plots0)
    gseg_iplot_contours1 = ncontour_plots0;

//  z_data : z data in user coordinates
//  nearest_contour_value : value of the nearest contour
 status = jlp_gsegraf1->GSEG_CreateContourLabel(dev_x10, dev_y10, 
                                                gseg_iplot_contours1, &z_data, 
                                                &nearest_contour_value);

 if(status == 0) {
// Copy to the label string used by "jlp_wx_gscanvas_process.cpp"
  sprintf(label_0, "%.3g", nearest_contour_value);
  } else {
  strcpy(label_0, "");
  }

/* DEBUG:
printf("jlp_gdev_wxwid_gseg/Callback_ViewLabelContours: ncontours=%d dev_x=%f dev_y=%f status=%d z_data=%f label=>%s<\n", 
          ncontour_plots0, dev_x10, dev_y10, status, z_data, label_0);
*/

return(0);
}
/************************************************************************
* Set processing mode to ZoomIn 
************************************************************************/
void JLP_GDev_wxWID::Set_ZoomIn()
{
/* InteractiveProcessingMode :
* -1=None 0=BoxLimits 1=Statistics, 2=Astrometry 3=Photometry
* 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
* 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
* 13=Add a ring 14=Remove a shape
*/

// ProcessingMode=0
    wxgdev_settings1.InternalProcessingMode = 0;
// Apply settings and update popup menu:
    if(m_popup_menu1 != NULL) m_popup_menu1->UpdatePopupMenu_InternalProcessingMode();

return;
}
/************************************************************************
* Interactive box selection 
* Called when processing mode is set to ZoomIn 
*
* INPUT:
* dev_x10, dev_y10 : (device coord) lower left corner of selection rectangle 
* dev_x20, dev_y20 : (device coord) upper right corner of selection rectangle 
************************************************************************/
int JLP_GDev_wxWID::Callback_ZoomIn(const double dev_x10, const double dev_y10,
                                    const double dev_x20, const double dev_y20)
{

// If empty box, redraw the plot with initial size
if((dev_x10 == dev_x20) || (dev_y10 == dev_y20))
  {
// Draw graph 
   jlp_gsegraf1->GSEG_DrawGraph();
// Update extra labels if any
   jlp_gsegraf1->GSEG_DrawExtraLabels();
// ProcessingMode=-1
    wxgdev_settings1.InternalProcessingMode = -1;
// Apply settings and update popup menu:
    if(m_popup_menu1 != NULL) m_popup_menu1->UpdatePopupMenu_InternalProcessingMode();
  } else {
// Redraw plot 
   jlp_gsegraf1->GSEG_ZoomIn(dev_x10, dev_y10, dev_x20, dev_y20);
  }
wxGdev_Refresh();

return(0);
}
/************************************************************************
* Conversion from device to user coordinates 
************************************************************************/
int JLP_GDev_wxWID::FromDeviceToUserCoord(const double dev_x1, 
                                          const double dev_y1, 
                                          double *user_x1, double *user_y1,
                                          int *in_frame)
{
int status = -1;

 if(jlp_gsegraf1 != NULL) {
   status = jlp_gsegraf1->GSEG_ContourLabel_FromDevToUser(dev_x1, dev_y1, 
                                                          user_x1, user_y1, 
                                                          in_frame);
   }

return(status);
}
/************************************************************************
* Get plot data for curves (called by external routines)
* from this graphic device
************************************************************************/
int JLP_GDev_wxWID::Gseg_GetCurveData(double **xplot0, double **yplot0, 
                                      int *npts0, const int icurve)
{
/*** TBD
int i;
  if((icurve < 0) || (icurve >= ncurves_1)) {
     fprintf(stderr, "JLP_GDev::GetCurveData/Error: icurve=%d ncurves=%d\n",
             icurve, ncurves_1);
     return(-1);
     }
  if(npts_1[icurve] <= 0) {
     fprintf(stderr, "JLP_GDev::GetCurveData/Error: npts = 0\n");
     return(-1);
     }
  *npts0 = npts_1[icurve];
  *xplot0 = new double[*npts0];
  *yplot0 = new double[*npts0];
  for(i = 0; i < *npts0; i++) {
      (*xplot0)[i] = xplot_1[i + icurve * nmaxi_1];
      (*yplot0)[i] = yplot_1[i + icurve * nmaxi_1];
      }
***/

return(0);
}
/************************************************************************
* Get the image data
************************************************************************/
int JLP_GDev_wxWID::Gseg_GetImageArray(double **array0, int *nx0, int *ny0)
{
int status = -1;
  if(jlp_gsegraf1 != NULL) {
    status = jlp_gsegraf1->GSEG_GetImageArray(array0, nx0, ny0);
    }
return(status);
}
/***************************************************************************
* LoadCurvePlotDataFromFile
***************************************************************************/
int JLP_GDev_wxWID::Gseg_LoadCurvePlotDataFromFile(char *filename0,
                                                   const int reset_first0,
                                                   char *axis_type0)
{
int status = -1;
  if(jlp_gsegraf1 != NULL) {
    status = jlp_gsegraf1->GSEG_LoadCurvePlotDataFromFile(filename0, 
                                                          reset_first0,
                                                          axis_type0);
    }
return(status);
}
/***************************************************************************
* LoadImagePlotDataFromFitsFile
***************************************************************************/
int JLP_GDev_wxWID::Gseg_LoadImagePlotDataFromFitsFile(char *filename0,
                                                       const int reset_first0,
                                                       char *axis_type0)
{
int status = -1;
  if(jlp_gsegraf1 != NULL) {
    status = jlp_gsegraf1->GSEG_LoadImagePlotDataFromFitsFile(filename0, 
                                                              reset_first0,
                                                              axis_type0);
    }
return(status);
}
