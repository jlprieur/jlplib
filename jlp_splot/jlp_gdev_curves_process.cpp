/**************************************************************************
* "jlp_gdev_curves_process.cpp"
*
* Definition of the members of the JLP_GDev class
* to compute the box limits for curves
*
* JLP
* Version 18/03/2019
**************************************************************************/
#include <math.h>
#include <string.h>

#include "jlp_gdev.h"
#include "jlp_macros.h"             // MINI, MAXI, etc
#include "jlp_read_ascii_data.h"    // jlp_ascii_read_two_columns
#include "jlp_read_latex_tables.h"  // jlp_read_latex_table_file

/*
#define DEBUG 1
*/

/** Defined in this file
  void Curves_BoxLimitsAuto();
  void Curves_BoxLimitsZoom(const bool zoom_in);
  void Curves_BoxLimitsMove(const bool move_to_right);
  void Curves_ComputeBoxLimits(double xmin_user, double xmax_user,
                               double ymin_user, double ymax_user,
                               double *box_xmin, double *box_xmax,
                               double *box_ymin, double *box_ymax);
  void Curves_ComputeXYMinMax(double *xmin_user, double *xmax_user,
                              double *ymin_user, double *ymax_user);
  void Curves_ComputeYMinMax(double xmin_user, double xmax_user,
                             double *ymin_user, double *ymax_user);
**/

/************************************************************************
* Automatic determination of the new Box Limits
************************************************************************/
void JLP_GDev::Curves_BoxLimitsAuto()
{
double box_xmin, box_xmax, box_ymin, box_ymax;
double xmin0, xmax0, ymin0, ymax0;

// Exit if no curve entered yet:
  if(ncurves_1 == 0) return;

  Curves_ComputeXYMinMax(&xmin0, &xmax0, &ymin0, &ymax0);

// Set the new box limits:
  Curves_ComputeBoxLimits(xmin0, xmax0, ymin0, ymax0, &box_xmin, &box_xmax, 
                          &box_ymin, &box_ymax);
  SetBoxLimits(box_xmin, box_xmax, box_ymin, box_ymax, 0., 1.);

return;
}
/************************************************************************
* INPUT:
* zoom_in: if true, divide the x range by two
*          if false (i.e. zoom_out) multiply the x range by two
************************************************************************/
void JLP_GDev::Curves_BoxLimitsZoom(const bool zoom_in)
{
double box_xmin, box_xmax, box_ymin, box_ymax;
double xmin0, xmax0, ymin0, ymax0;
double box_xc, box_xrange;

// Exit if no curve entered yet:
  if(ncurves_1 == 0) return;

// Load previous settings
  box_xmin = Jgc0.axis_limits[0];
  box_xmax = Jgc0.axis_limits[1];

// Range:
  box_xrange = box_xmax - box_xmin;

// Coordinates of the center:
  box_xc = (box_xmin + box_xmax) / 2.;

  if(zoom_in) {
// Divide the x range by two:
     box_xmin = box_xc - box_xrange / 4.;
     box_xmax = box_xmin + box_xrange / 2.;
   } else {
// Double the x range
     box_xmin = box_xc - box_xrange;
     box_xmax = box_xmin + 2. * box_xrange;
   }

// Compute the maximum limits for the X and Y axes:
  Curves_ComputeXYMinMax(&xmin0, &xmax0, &ymin0, &ymax0);
  box_xmin = MAXI(xmin0, box_xmin);
  box_xmax = MINI(xmax0, box_xmax);

// Compute the new limits for the Y axis:
  Curves_ComputeYMinMax(box_xmin, box_xmax, &ymin0, &ymax0);

// Set the new box limits:
  Curves_ComputeBoxLimits(box_xmin, box_xmax, ymin0, ymax0,
                            &box_xmin, &box_xmax, &box_ymin, &box_ymax);
  SetBoxLimits(box_xmin, box_xmax, box_ymin, box_ymax, 0., 1.);

return;
}
/************************************************************************
* Move to the right (by half width of current x range)
************************************************************************/
void JLP_GDev::Curves_BoxLimitsMove(const bool move_to_right)
{
double box_xmin, box_xmax, box_ymin, box_ymax;
double xmin0, xmax0, ymin0, ymax0;
double box_xc, box_xrange;

// Exit if no curve entered yet:
  if(ncurves_1 == 0) return;

  box_xmin = Jgc0.axis_limits[0];
  box_xmax = Jgc0.axis_limits[1];

// Range:
  box_xrange = box_xmax - box_xmin;

// Coordinates of the center:
  box_xc = (box_xmin + box_xmax) / 2.;

  if(move_to_right) {
     box_xmin = box_xmin + box_xrange / 4.;
   } else {
     box_xmin = box_xmin - box_xrange / 4.;
   }
   box_xmax = box_xmin + box_xrange;

// Compute the maximum limits for the X and Y axes:
  Curves_ComputeXYMinMax(&xmin0, &xmax0, &ymin0, &ymax0);

// Return if left/right limit has been reached:
  if((box_xmin  <= xmin0) || (box_xmax  >= xmax0)) return;

// Compute the limits for the Y axis:
  Curves_ComputeYMinMax(box_xmin, box_xmax, &ymin0, &ymax0);

// Set the new box limits:
  Curves_ComputeBoxLimits(box_xmin, box_xmax, ymin0, ymax0,
                            &box_xmin, &box_xmax, &box_ymin, &box_ymax);
  SetBoxLimits(box_xmin, box_xmax, box_ymin, box_ymax, 0., 1.);

return;
}
/*************************************************************
* Compute the box limits of a set of curves
*
*************************************************************/
void JLP_GDev::Curves_ComputeBoxLimits(double xmin_user, double xmax_user,
                                       double ymin_user, double ymax_user,
                                       double *box_xmin, double *box_xmax,
                                       double *box_ymin, double *box_ymax)
{
double delta, wscale;

// box_type : 1=JLP axes 0=SMongo axes
// JLP_axes : takes +/- 4 percents larger frame
 if(Jgc0.box_type == 1) {
   wscale = 0.04;
   } else {
// SMongo axes: takes +/- 5 percents larger frame
   wscale = 0.05;
   }

// For the x axis:
 delta = (xmax_user - xmin_user) * wscale;
 *box_xmax = xmax_user + delta;
 *box_xmin = xmin_user - delta;

// For the y axis:
 delta = (ymax_user - ymin_user) * wscale;
 *box_ymax = ymax_user + delta;
 *box_ymin = ymin_user - delta;
   
/* DEBUG
printf("ComputeBoxLimitsForCurves/delta=%f xmin_user=%f xmax_user=%f ymin_user=%f ymax_user=%f\n",
        delta, xmin_user, xmax_user, ymin_user, ymax_user);
*/

return;
}
/*************************************************************
* Compute the min/max values of a set of curves
* (using private parameters xplot_1, yplot_1, nmaxi_1, etc)
*************************************************************/
void JLP_GDev::Curves_ComputeXYMinMax(double *xmin_user, double *xmax_user,
                                      double *ymin_user, double *ymax_user)
{
double ww;
int i, k;

// Compute the minimum and maximum :
// Compute minimum and maximum for the Y axis:
*ymin_user = yplot_1[0];
*ymax_user = yplot_1[0];
for(k = 0; k < ncurves_1; k++) { 
  for(i = 0; i < npts_1[k]; i++) {
      ww = yplot_1[i + k * nmaxi_1];
      *ymin_user = MINI(*ymin_user, ww);
      *ymax_user = MAXI(*ymax_user, ww);
     }
  }

// Compute minimum and maximum for the X axis:
*xmin_user = xplot_1[0];
*xmax_user = xplot_1[0];
for(k = 0; k < ncurves_1; k++) { 
  for(i = 0; i < npts_1[k]; i++) {
      ww = xplot_1[i + k * nmaxi_1];
      *xmin_user = MINI(*xmin_user, ww);
      *xmax_user = MAXI(*xmax_user, ww);
     }
  }

#ifdef DEBUG
printf("computeXYMinMaxOf ncurves=%d npts0=%d ymin_user=%f ymax_user=%f\n",
       ncurves_1, npts_1[0], *ymin_user, *ymax_user);
#endif

return;
}
/*************************************************************
* Compute the min/max values of the curves
* (using private parameters xplot_1, yplot_1, nmaxi_1, etc)
*************************************************************/
void JLP_GDev::Curves_ComputeYMinMax(double xmin_user, double xmax_user,
                                     double *ymin_user, double *ymax_user)
{
double ww;
int i, k;

// Compute the minimum and maximum :
// Compute minimum and maximum for the Y axis:
*ymin_user = +1.e+10;
*ymax_user = - *ymin_user;
for(k = 0; k < ncurves_1; k++) {
  for(i = 0; i < npts_1[k]; i++) {
     if((xplot_1[i + k * nmaxi_1] >= xmin_user)
        && (xplot_1[i + k * nmaxi_1] < xmax_user)) {
        ww = yplot_1[i + k * nmaxi_1];
        *ymin_user = MINI(*ymin_user, ww);
        *ymax_user = MAXI(*ymax_user, ww);
        }
     }
  }

#ifdef DEBUG
printf("computeYMinMaxOf ncurves=%d npts0=%d ymin_user=%f ymax_user=%f\n",
       ncurves_1, npts_1[0], *ymin_user, *ymax_user);
#endif

return;
}
/*************************************************************
* Reset all private parameters of igdev settings and some others
*
* Some parameters are also initialized in "SetupForCurve()"
*************************************************************/
void JLP_GDev::Curves_ResetAllPrivateParameters()
{
 strcpy(Jgc0.box_title, "JLP_GDev");
 strcpy(Jgc0.box_xlabel, "");
 strcpy(Jgc0.box_ylabel, "");
 strcpy(filename_1, "");

 ncurves_1 = 0;
 npts_1[0] = 0;
 npts_LeftDown_1 = 0;
 npts_LeftUp_1 = 0;

 Jgc0.box_type = 0;
 Jgc0.box_xgrid = 0;
 Jgc0.box_ygrid = 0;
// xaxis_type : 0=linear 1=log10
 Jgc0.xaxis_type = 0;
// yaxis_type : 0=linear 1=log10
 Jgc0.yaxis_type = 0;
 Jgc0.expand = 1.2;

return;
}
/*************************************************************
* Reset all private parameters of igdev settings and some others
*
* Some parameters are also initialized in "SetupForCurve()"
*************************************************************/
void JLP_GDev::Images_ResetAllPrivateParameters()
{

 npts_LeftDown_1 = 0;
 npts_LeftUp_1 = 0;
 strcpy(fits_filename_1, "");
 iplane_1 = -1;
 nx_1 = 0;
 ny_1 = 0;
 nz_1 = 0;

return;
}
/************************************************************************
* Setup plot configuration
* (initializing private parameters xplot_1, yplot_1, nmaxi_1, etc)
* (called by external routines)
*
* Allocate memory if needed
*
* Typically: 
* nmaxi_1 = 1024, ncurves_maxi_1 = 128, nout_maxi = 256 
************************************************************************/
void JLP_GDev::CreatePlotDataArrays(const int nmaxi, const int ncurves_maxi,
                                    const int nout_maxi)
{
// Set the value of private paramaters:
nmaxi_1 = nmaxi;
ncurves_maxi_1 = ncurves_maxi;
nout_maxi_1 = nout_maxi;

  xplot_1 = NULL;
  yplot_1 = NULL;
  errorx_1 = NULL;
  errory_1 = NULL;
  npts_1 = NULL;
  xout_1 = NULL;
  yout_1 = NULL;
  dble_image_1 = NULL;
  nx_1 = 0;
  ny_1 = 0;

if(xplot_1 != NULL) delete[] xplot_1;
if(yplot_1 != NULL) delete[] yplot_1;
if(errorx_1 != NULL) delete[] errorx_1;
if(errory_1 != NULL) delete[] errory_1;
if(npts_1 != NULL) delete[] npts_1;

xplot_1 = new double[nmaxi_1 * ncurves_maxi_1];
yplot_1 = new double[nmaxi_1 * ncurves_maxi_1];
errorx_1 = new double[nmaxi_1 * ncurves_maxi_1];
errory_1 = new double[nmaxi_1 * ncurves_maxi_1];
npts_1 = new int[ncurves_maxi_1];

// Nout used when interactive input of data points:
if(xout_1 != NULL) delete[] xout_1;
if(yout_1 != NULL) delete[] yout_1;
xout_1 = new double[nout_maxi_1];
yout_1 = new double[nout_maxi_1];

return;
}
/***************************************************************************
* Curves_LoadPlotData
* Load data to xplot and yplot arrays
* (called by external routines)
* (using private variables xplot_1, yplot_1, errorx_1, errory_1, etc)
*
* INPUT:
*   reset_first: flag used to reset all private arrays
*                (if 1: erase all old data; if 0: add this data to old data)
*
* double *xplot, *yplot;
* int *npts, nmaxi,  ncurves, ncurves_maxi;
* char nchar_type*4, pcolor*32;
* char filename_1;
***************************************************************************/
int JLP_GDev::Curves_LoadPlotDataToPrivateParameters0(double *xplot0, 
                                  double *yplot0, const int npts0, 
                                  const char *nchar_type0,
                                  const char *pcolor0, const char *plot_fname0,
                                  const int reset_first0)
{
int i, status;
double *errorx0, *errory0;

// Allocate memory and initialize error arrays
  errorx0 = new double[npts0];
  errory0 = new double[npts0];
  for(i = 0; i < npts0; i++) {
    errorx0[i] = 0.;
    errory0[i] = 0.;
    }


  status = Curves_LoadPlotDataToPrivateParameters(xplot0, yplot0,
                                  errorx0, errory0, npts0, nchar_type0,
                                  pcolor0, plot_fname0, reset_first0);

// Free memory
 delete[] errorx0;
 delete[] errory0;

return(status);
}
/***************************************************************************
* Curves_LoadPlotData
* Load data to xplot and yplot arrays
* (called by external routines)
* (using private variables xplot_1, yplot_1, errorx_1, errory_1, etc)
*
* INPUT:
*   reset_first: flag used to reset all private arrays
*                (if 1: erase all old data; if 0: add this data to old data)
*
* double *xplot, *yplot;
* int *npts, nmaxi,  ncurves, ncurves_maxi;
* char nchar_type*4, pcolor*32;
* char filename_1;
***************************************************************************/
int JLP_GDev::Curves_LoadPlotDataToPrivateParameters(double *xplot, 
                                  double *yplot,
                                  double *errorx, double *errory,
                                  const int npts, const char *nchar_type,
                                  const char *pcolor, const char *plot_fname,
                                  const int reset_first)
{
int icur, k, kk;
char nchar_type0[128], pcolor0[128], plot_fname0[128];

strcpy(nchar_type0, nchar_type);
strcpy(pcolor0, pcolor);
strcpy(plot_fname0, plot_fname);

if(reset_first) {
  icur = 0;
  } else {
  icur = ncurves_1;
  }

if(icur >= ncurves_maxi_1){
  fprintf(stderr, "LoadPlotData/Error icurve=%d ncurves_maxi=%d\n",
          icur, ncurves_maxi_1);
  return(-2);
  }

if(npts >= nmaxi_1) {
  fprintf(stderr, "LoadPlotData/Error: npts=%d and maximum limit of npts is %d ! \n",
               npts, nmaxi_1);
  return(-1);
  }


kk = nmaxi_1 * icur;
  for(k = 0; k < npts; k++) {
     xplot_1[k + kk] = (double)xplot[k];
     yplot_1[k + kk] = (double)yplot[k];
     errorx_1[k + kk] = (double)errorx[k];
     errory_1[k + kk] = (double)errory[k];
    }

// Set number of points of curve #icur
  npts_1[icur] = npts;

// Set color and line type for the curves:
  nchar_type0[4] = '\0';
  strcpy(&nchar_1[icur*4], nchar_type0);
  pcolor0[32] = '\0';
  strcpy(&pcolor_1[icur*32], pcolor0);
  plot_fname0[128] = '\0';
  strcpy(&plot_fname_1[icur*128], plot_fname0);

/* DEBUG
printf("Curves_LoadPlotData: icur=%d, nchar_type=%s, pcolor=%s plot_fname=%s\n",
        icur, nchar_type, pcolor, plot_fname);
*/

// Increment number of curves:
  ncurves_1 = icur + 1;

// Initialize box limits:
  Curves_BoxLimitsAuto();

return(0);
}
/***************************************************************************
* Curves_LoadPlotDataToPrivateFromFile
* Read an ASCII file and load data to xplot and yplot arrays
*
* INPUT:
*  plot_filename0 : name of the input filename with the curve data
*  icol_x, icol_y : column numbers to be used for reading the data 
*  icol_errx, icol_erry : column numbers to be used for reading the errors 
*                         (0 if no errorx, 0 if no errory)
*  reset_first0: flag used to reset all private arrays
*                (if 1: erase all old data; if 0: add this data to old data)
*
* Used private variables:
* double *xplot, *yplot;
* int *npts, nmaxi,  ncurves, ncurves_maxi;
* char nchar_type*4, pcolor*32;
* char filename_1;
***************************************************************************/
int JLP_GDev::Curves_LoadPlotDataToPrivateFromFile(char *plot_filename0, 
                                          int icol_x, int icol_y, 
                                          int icol_errx, int icol_erry, 
                                          const char *nchar_type0,
                                          const char *pcolor0,
                                          const int reset_first0)
{
int i, status, npts0;
double w1, w2;
char buffer[80];
double *xplot0, *yplot0, *errorx0, *errory0;

xplot0 = NULL;
yplot0 = NULL;
errorx0 = NULL;
errory0 = NULL;

// Read the two columns with the data:
 if(icol_errx == 0 && icol_erry == 0) {
   status = jlp_ascii_read_two_columns(plot_filename0, icol_x, icol_y, 
                                       &xplot0, &yplot0, &npts0);
   if((status == 0) && (npts0 > 0)){
     errorx0 = new double[npts0];
     errory0 = new double[npts0];
     for(i = 0; i < npts0; i++) {
      errorx0[i] = 0.;
      errory0[i] = 0.;
     }
   }
 } else {
   status = jlp_ascii_read_four_columns(plot_filename0, icol_x, icol_y,
                                        icol_errx, icol_erry, 
                                        &xplot0, &yplot0, 
                                        &errorx0, &errory0, &npts0);
 }

// Load data to private arrays:
 if((status == 0) && (npts0 > 0)) {
   status = Curves_LoadPlotDataToPrivateParameters(xplot0, yplot0, 
                                errorx0, errory0, npts0, 
                                nchar_type0, pcolor0, plot_filename0, 
                                reset_first0);
  }

if(xplot0 != NULL) delete[] xplot0;
if(yplot0 != NULL) delete[] yplot0;
if(errorx0 != NULL) delete[] errorx0;
if(errory0 != NULL) delete[] errory0;

return(status);
}
/***************************************************************************
* Curves_LoadPlotDataToPrivateFromLatex file
* Read a Latex table file and load data to xplot and yplot arrays
*
* INPUT:
*  plot_filename0 : name of the input filename with the curve data
*  icol_x, icol_y : column numbers to be used for reading the data 
*  icol_errx, icol_erry : column numbers to be used for reading the errors 
*                         (0 if no errorx, 0 if no errory)
*  reset_first0: flag used to reset all private arrays
*                (if 1: erase all old data; if 0: add this data to old data)
*
* Used private variables:
* double *xplot, *yplot;
* int *npts, nmaxi,  ncurves, ncurves_maxi;
* char nchar_type*4, pcolor*32;
* char filename_1;
***************************************************************************/
int JLP_GDev::Curves_LoadPlotDataToPrivateFromLatex(char *latex_fname0, 
                                          int icol_x, int icol_y, 
                                          int icol_errx, int icol_erry, 
                                          const char *nchar_type0,
                                          const char *pcolor0,
                                          const int reset_first0)
{
int i, status, npts0, error_bars0;
double w1, w2;
char buffer[80];
double *xplot0, *yplot0, *errorx0, *errory0;

xplot0 = NULL;
yplot0 = NULL;
errorx0 = NULL;
errory0 = NULL;

/*
int jlp_read_latex_table_file(char *latex_fname0, int icol_x, int icol_y,
                          int icol_errorx, int icol_errory, double **xplot0,
                          double **yplot0, double **errorx0, double **errory0,
                          int *npts0, int *error_bars0);
*/
   status = jlp_read_latex_table_file(latex_fname0, icol_x, icol_y,
                                      icol_errx, icol_erry, &xplot0, &yplot0, 
                                      &errorx0, &errory0, &npts0, &error_bars0);

// Load data to private arrays:
 if((status == 0) && (npts0 > 0)) {
   status = Curves_LoadPlotDataToPrivateParameters(xplot0, yplot0, 
                                errorx0, errory0, npts0, 
                                nchar_type0, pcolor0, latex_fname0, 
                                reset_first0);
  } else {
   fprintf(stderr, "Curves_LoadPlotDataToPrivateFromLatex/Error reading %s: status=%d npts0=%d\n",
   latex_fname0, status, npts0);
  }

/* DEBUG
   printf("Curves_LoadPlotDataToPrivateFromLatex/Reading %s: status=%d npts0=%d\n",
   latex_fname0, status, npts0);
*/

if(xplot0 != NULL) delete[] xplot0;
if(yplot0 != NULL) delete[] yplot0;
if(errorx0 != NULL) delete[] errorx0;
if(errory0 != NULL) delete[] errory0;

return(status);
}
/*************************************************************************
* Load the curves corresponding to the file names contained
* in a parameter file
*
*************************************************************************/
int JLP_GDev::Curves_LoadPlotDataToPrivateFromParamFile(
                             PLOT1_FILE_DATA *pfiledata0,
                             int n_datafiles, int n_pfile_param_max)
{ 
int status = -1, reset_first0 = 0;
int k, icol_x, icol_y, icol_errorx, icol_errory;
long ncols, nlines, ndatalines;
char filename0[128], nchar_type0[64], pcolor0[64], format0[64], plot_style0[64];
char fmt_ascii[64], fmt_latex[64];
strcpy(fmt_ascii,"ascii_cols");
strcpy(fmt_latex,"latex_table");

#ifdef DEBUG
printf("Curves_LoadPlotDataToPrivateFromParamFile: ndatafiles=%d\n",
       n_datafiles);
#endif

 for(k = 0; k < n_datafiles; k++) {
/* Example:
// For File parameters:
 i=0 key=file_name cvalue="toto.dat" 
 i=1 key=file_format cvalue="ascii_cols" 
 i=2 key=file_col_x ivalue=1 
 i=3 key=file_col_y ivalue=2 
 i=6 key=file_plot_style cvalue="92,Red" 
*/
    strcpy(filename0, pfiledata0[k].file_name);
    strcpy(format0, pfiledata0[k].file_format);
    icol_x = pfiledata0[k].file_col_x;
    icol_y = pfiledata0[k].file_col_y;
    icol_errorx = pfiledata0[k].file_col_xerr;
    icol_errory = pfiledata0[k].file_col_yerr;
    strcpy(plot_style0, pfiledata0[k].file_plot_style);
#ifdef DEBUG
    printf("1/3: File k=%d filename=%s format=%s icol_x=%d icol_y=%d, icol_errorx=%d icol_errory=%d\n",
           k, filename0, format0, icol_x, icol_y, icol_errorx, icol_errory);
#endif
    nchar_type0[0] = plot_style0[0];
    nchar_type0[1] = plot_style0[1];
    nchar_type0[2] = '\0';
    if((plot_style0[2] == ',') && (plot_style0[3] != '\0'))
       strcpy(pcolor0, &plot_style0[3]);
    else
       strcpy(pcolor0, "Black");
#ifdef DEBUG
    printf("2/3: plot_style=%s nchar_type=%s pcolor=%s\n",
           plot_style0, nchar_type0, pcolor0);
#endif
// Save plot data to private variables (xplot_1, yplot_1, errorx_plot1
// (in "jlp_splot/jlp_gdev_curves_process.cpp")
   reset_first0 = 0;
   if(!strcmp(format0, fmt_ascii)) { 
   status = Curves_LoadPlotDataToPrivateFromFile(filename0, icol_x, icol_y, 
                                      icol_errorx, icol_errory, nchar_type0,
                                      pcolor0, reset_first0);
   } else if(!strcmp(format0, fmt_latex)) { 
   status = Curves_LoadPlotDataToPrivateFromLatex(filename0, icol_x, icol_y,
                                      icol_errorx, icol_errory, nchar_type0,
                                      pcolor0, reset_first0);
   } // EOF else if fmt_latex
#ifdef DEBUG
   printf("3/3: filename0=%s ncurves_1 = %d\n", filename0, ncurves_1);
#endif
  }
return(status);
}
/************************************************************************
* Get plot data for curves (called by external routines) 
* from this graphic device 
* (using private parameters xplot_1, yplot_1, errorx_1, errory_1, nmaxi_1, ...)
************************************************************************/
int JLP_GDev::GetCurveData(double **xplot0, double **yplot0, 
                           double **errorx0, double **errory0,  int *npts0,
                           const int icurve)
{
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
  *errorx0 = new double[*npts0];
  *errory0 = new double[*npts0];
  for(i = 0; i < *npts0; i++) {
      (*xplot0)[i] = xplot_1[i + icurve * nmaxi_1];
      (*yplot0)[i] = yplot_1[i + icurve * nmaxi_1];
      (*errorx0)[i] = errorx_1[i + icurve * nmaxi_1];
      (*errory0)[i] = errory_1[i + icurve * nmaxi_1];
      }

return(0);
}
