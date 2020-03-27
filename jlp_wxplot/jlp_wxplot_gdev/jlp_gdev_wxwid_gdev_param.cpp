/*************************************************************************
* JLP_GDev_wxWID Class (for curves and images)
* Routines dealing with the plot parameter file
*
* JLP
* Version 18/03/2019
**************************************************************************/
#include "jlp_gdev_wxwid.h"
#include "jlp_wxgseg_curve_dlg.h"   // JLP_wxGseg_Curve_Dlg
#include "jlp_wxgseg_image_dlg.h"   // JLP_wxGseg_Image_Dlg
#include "jlp_gsegraf.h"            // JLP_GSEG_InitializePlot
#include "jlp_gseg_plot_data1.h"    // jlp_gseg_init_plot_data()
#include "jlp_gseg_axis_data1.h"    // jlp_gseg_init_axis_data()
#include "jlp_plot1_def.h"          // PLOT1_FILE_DATA structure
#include "jlp_plot1_utils.h"        // Plot1_ReadParamFile()
#include "jlp_plot1_gsegraf.h"      //  Plot1_PlotSettingsToGsegAxisData() 


/*
*/
#define DEBUG
#define DBLE_MIN_VALUE 1.e-12

/*************************************************************************
* Initialize drawing setup with a parameter file 
*
*************************************************************************/
int JLP_GDev_wxWID::InitializeGdevPlotWithParamFile(char *plot_param_fname0)
{
// n_pfile_param_max: size of the lines of the pfiledata0 as read
// by Plot1_ReadParamFile()...
int n_datafiles, n_pfile_param_max;
/*
* NB: declarations in the calling program of Plot1_ReadParamFile():
*  PLOT1_SETTINGS pset0[N_PFILE_PARAM_MAX], i.e., pset0[128]
*  PLOT1_FILE_DATA pfiledata0[N_PFILE_PARAM_MAX * N_DATA_FILE_MAX]
*                  i.e., pfiledata0[128 * 64]
*/
PLOT1_SETTINGS pset0;
PLOT1_FILE_DATA pfiledata0[128 * 64];
GSEG_AXIS_DATA gseg_axdata0;

// This object should have been created by open_device
 if(jlp_gseg_wxwid1 == NULL) {
   fprintf(stderr, "InitializeGdevPlotWithParamFile/Fatal error, jlp_gseg_wxwid is NULL: (should have been created by open_device before calling this routine...)\n");
   gseg_is_activated = 0;
  return(-1);
  }

// Save to private variable (used for the title of the popup frame)
 strcpy(filename_1, plot_param_fname0);

/*
int Plot1_ReadParamFile(char *plot_param_fname0, PLOT1_SETTINGS *pset0,
                         PLOT1_FILE_DATA *pfiledata0, int *n_pfiledata0,
                         int *n_pfile_param_max)
*/
// n_pfile_param_max: size of the lines of the pfiledata0 as read
// by Plot1_ReadParamFile()...

 Plot1_ReadParamFile(plot_param_fname0, &pset0, pfiledata0, &n_datafiles,
                     &n_pfile_param_max);

/* Load the curves from the file names contained in a parameter file 
*/
 Curves_LoadPlotDataToPrivateFromParamFile(pfiledata0, n_datafiles, 
                                           n_pfile_param_max);

// Load the axis settings of pset0 (from the parameter file) to gseg_axdata0 
 Plot1_PlotSettingsToGsegAxisData(pset0, &gseg_axdata0);

 InitializeGsegCurvePlotWithPrivateData(gseg_axdata0, n_datafiles);

return(0);
}
/*************************************************************************
*
* Load private data to Gseg parameters for curves (xplot&, yplot1, etc)
*
* INPUT:
*  n_datafiles: number of input plot data files
**************************************************************************/
int JLP_GDev_wxWID::InitializeGsegCurvePlotWithPrivateData(
                              GSEG_AXIS_DATA gseg_axdata2, int n_datafiles)
{
GSEG_PLOT_DATA gseg_pltdata0[128]; // 128 files at maximum
GSEG_AXIS_DATA gseg_axdata0;
int iplot, status, gdev_graphic_type0, gseg_plot_type0, pen_width0;
int dev_x0, dev_y0, dev_width0, dev_height0;
int answer, gdev_graphic_type2;
char axis_type0[64]; 
JLP_wxGseg_Curve_Dlg *m_popup_Dlg;

 if(jlp_gseg_wxwid1 == NULL) {
   fprintf(stderr, "InitializeGsegCurvePlotWithPrivateData/Warning: jlp_gseg_wxwid1 is null\n");
   gseg_is_activated = 0;
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
  gdev_graphic_type2 = 4; // Set input value to gsegraf/2d curves by default

/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*/
 gseg_plot_type0 = 1;  // Set it to "points" by default

/* axis_type:
* "linear"   (points, histogram, contour, and color plot types)
* "semilogx" (points plot type only)
* "semilogy"  (points plot type only)
* "loglog"  (points plot type only)
* "polar" (points plot type only)
* "3d" (points, contour, color, and mesh plot types)
*/
 strcpy(axis_type0, "linear");  // Set it to "linear" by default

// Open a pop-up dialog window with input value of graphic_type:
 m_popup_Dlg = new JLP_wxGseg_Curve_Dlg(NULL, 
                          wxT("Options for plotting curves with GDev/GsegPlot"),
                          gdev_graphic_type2, gseg_axdata2);

// WARNING: There is a bug here coming from "wxwidgets"
// when using ShowModal, the system doesn't return
// The computer may even crash if there are too many of those processed hanging
// around and using CPU time !
 answer = m_popup_Dlg->ShowModal();

 if(answer == 0) {
// Retrieve the patch parameters have another try with those parameters
   m_popup_Dlg->RetrieveData(&gdev_graphic_type0, &gseg_axdata0, 
                            &gseg_plot_type0, &pen_width0);

#ifdef DEBUG
printf("RetrieveData/gdev_graphic_type0=%d gseg_plot_type0=%d pen_width0=%d axis_type0=%s\n",
       gdev_graphic_type0, gseg_plot_type0, pen_width0, gseg_axdata0.axis_type);
printf("RetrieveData/x_rev0=%d y_rev0=%d z_rev0=%d \n",
       gseg_axdata0.reversed_axis[0], gseg_axdata0.reversed_axis[1],
       gseg_axdata0.reversed_axis[2]);
#endif

   delete m_popup_Dlg;
// Return without validating the changes:
   } else {
   delete m_popup_Dlg;
   return(-1);
   }

// Load axis data settings to Jgc structure (xlabel, ylabel, etc):
   GDev_UpdateJgcFromGsegAxisData(gseg_axdata0);

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

/* axis_type:
* "linear"   (points, histogram, contour, and color plot types)
* "semilogx" (points plot type only)
* "semilogy"  (points plot type only)
* "loglog"  (points plot type only)
* "polar" (points plot type only)
* "3d" (points, contour, color, and mesh plot types)
*/

/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*/

//************************* gsegraf plot:
 if(gdev_graphic_type0 > 3) {
// Copy jlp_splot data to gseg data structure:
// Curves:
// 4 = gsegraf_2d_curves, 7 = gsegraf_3d_curves 8 = gsegraf_polar_curve
// iplot index starts at iplot=1 !
    iplot = 1;
// Load the curves to the gseg_pltdata0 and gseg_axdata0 objects
// (with gdev routine using private arrays: npts_1, xplot_1, yplot_1, nmax_1...
   for(iplot = 1; iplot <= n_datafiles; iplot++) {
     status = LoadXYPlot1ToGsegPlot(&gseg_pltdata0[iplot], iplot, 
                                    gdev_graphic_type0, gseg_plot_type0, 
                                    gseg_axdata0.axis_type, pen_width0);
    }
// Create jlp_gsegraf1 with n_datafiles curves :
   if(jlp_gsegraf1 != NULL) delete jlp_gsegraf1;
   jlp_gsegraf1 = new JLP_Gsegraf(jlp_gseg_wxwid1, gseg_pltdata0,
                                n_datafiles, gseg_axdata0);
// Get windows device limits:
   jlp_gseg_wxwid1->GSEG_GetWindowLimits(&dev_x0, &dev_width0, &dev_y0,
                                       &dev_height0);
// Update Jgc0 and Mgc0 structures from Gsegraf settings
// (NB: limits may have changed from xmin, etc)
   GDev_UpdateJgcFromGsegSettings(jlp_gsegraf1, dev_x0, dev_y0, dev_width0,
                           dev_height0);

//************************* jlp splot:
  } else {
// Update Jgc0 and Mgc0 structures from Gsegraf settings
// (NB: limits may have changed from xmin, etc)
   GDev_UpdateJgcFromGsegAxisData(gseg_axdata0);
  }

// Initialize plot setup and popup menu
// and update the screen with "RedrawToBackupDC(7002)"
 InitializePlotSetupWithGDevGraphicType(gdev_graphic_type0);

 return(0);
}
/*************************************************************************
*
* Load private data to Gseg parameters for images (dbleimage_1, etc)
*
* INPUT:
*
* contours option: 
* 0="none"
* 1="black contours"
* 2="white contours"
* 3="auto-colored contours"
**************************************************************************/
int JLP_GDev_wxWID::InitializeGsegImagePlotWithPrivateData(
                                                     int gdev_graphic_type0,
                                                     const int contours_option0)
{
GSEG_PLOT_DATA gseg_pltdata0[2];
GSEG_AXIS_DATA gseg_axdata0;
int nplots0, k, status, gseg_plot_type0, pen_width0, gdev_graphic_type2;
int answer, x_rev0, y_rev0, z_rev0, contours_option2;
char axis_type0[64], xlabel0[128], ylabel0[128], zlabel0[128], title0[128];
JLP_wxGseg_Image_Dlg *m_popup_Dlg;

 if(jlp_gseg_wxwid1 == NULL) {
   fprintf(stderr, "InitializeGsegImagePlotWithPrivateData/Warning: jlp_gseg_wxwid1 is null\n");
   gseg_is_activated = 0;
  return(-1);
  }


/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*/
/* axis_type:
* "linear"   (points, histogram, contour, and color plot types)
* "semilogx" (points plot type only)
* "semilogy"  (points plot type only)
* "loglog"  (points plot type only)
* "polar" (points plot type only)
* "3d" (points, contour, color, and mesh plot types)
*/

// Open a pop-up dialog window with input value of graphic_type:
 m_popup_Dlg = new JLP_wxGseg_Image_Dlg(NULL, 
                          wxT("Options for plotting images with GDev/GsegPlot"),
                          gdev_graphic_type0, contours_option0);

// WARNING: There is a bug here coming from "wxwidgets"
// when using ShowModal, the system doesn't return
// The computer may even crash if there are too many of those processed hanging
// around and using CPU time !
 answer = m_popup_Dlg->ShowModal();

 if(answer == 0) {
// Retrieve the patch parameters have another try with those parameters
   m_popup_Dlg->RetrieveData(&gdev_graphic_type2, &gseg_plot_type0, 
                             axis_type0, &x_rev0, &y_rev0, &z_rev0,
                             &contours_option2, &pen_width0,
                             xlabel0, ylabel0, zlabel0, title0);
#ifdef DEBUG
printf("RetrieveData/gdev_graphic_type2=%d gseg_plot_type0=%d axis_type0=%s\n",
        gdev_graphic_type2, gseg_plot_type0, axis_type0);
printf("RetrieveData/x_rev0=%d y_rev0=%d z_rev0=%d\n",
       x_rev0, y_rev0, z_rev0);
#endif

   delete m_popup_Dlg;
// Return without hardcopy:
   } else {
   delete m_popup_Dlg;
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

 if(gdev_graphic_type2 > 3) {
   gseg_is_activated = 1;
// Minimum configuration:
    jlp_gseg_init_axis_data(gdev_graphic_type2, &gseg_axdata0);

   strcpy(gseg_axdata0.xlabel, xlabel0);
   strcpy(gseg_axdata0.ylabel, ylabel0);
   strcpy(gseg_axdata0.zlabel, zlabel0);
   strcpy(gseg_axdata0.title, title0);

// Reversed axes:
   gseg_axdata0.reversed_axis[0] = x_rev0;
   gseg_axdata0.reversed_axis[1] = y_rev0;
   gseg_axdata0.reversed_axis[2] = z_rev0;

// Copy jlp_splot data to gseg data structure:
// Images:
// iplot index starts at iplot=1 !
   k = 1;
   status = LoadDbleImage1ToGsegPlot(&gseg_pltdata0[k], &gseg_axdata0,
                                     gseg_plot_type0, axis_type0,
                                     contours_option0, pen_width0);

// Create jlp_gsegraf1 :
   nplots0 = 1;
   if(jlp_gsegraf1 != NULL) delete jlp_gsegraf1;
   jlp_gsegraf1 = new JLP_Gsegraf(jlp_gseg_wxwid1, gseg_pltdata0, 
                                  nplots0, gseg_axdata0);

// Initialize plot setup and popup menu:
   status = InitializePlotSetupWithGDevGraphicType(gdev_graphic_type2);

   if(status) return(-1);
  }

return(0);
}
