/*************************************************************************
* JLP_wxGseg_Canvas Class (for curves)
* Routines to plot curves
*
* JLP
* Version 09/02/2016
**************************************************************************/
#include "jlp_splot_def.h"  // BITMAP_WIDTH, etc
#include "jlp_cgdev.h"
#include "jlp_wx_gscanvas.h"

#include "jlp_splot.h"        // newplot210
#include "jlp_plotlib.h"      // JLP_SET_PLOT_PARAM
#include "jlp_idv.h"          // alloc_curve_idv()
#include "jlp_cgdev_pst.h"    // JLP_cGDev_PST

#include "jlp_gsegraf.h"

/*
#define DEBUG
*/
/***************************************************************************
* ClearDrawingDisplay 
* (called by external routines)
***************************************************************************/
void JLP_wxGseg_Canvas::ClearDrawingDisplay()  
{
if(initialized != 1234) return; 
ncurves1 = 0; 
should_plot_allcurves = 0;
erase();
Refresh();

}
/***************************************************************************
* PlotToDrawingDisplay
* (called by external routines)
***************************************************************************/
void JLP_wxGseg_Canvas::PlotToDrawingDisplay()                                
{
if(initialized != 1234) return; 
should_plot_allcurves = 1;
first_time_curves_are_plotted = 1;

RedrawToBackupDC();

}
/*************************************************************
* Plot all the curves with current values of cgdev_settings1
* (used for redoing the drawings from scratch outside of Paint events)
* 
*************************************************************/
int JLP_wxGseg_Canvas::RefreshAllCurves()
{

if(initialized != 1234) return(-1); 

if(ncurves1 == 0) return(1); 

if(should_plot_allcurves) {
// If first time call BoxLimits_Auto:
  if(first_time_curves_are_plotted) {
    InitBeforePlot();
    first_time_curves_are_plotted = 0;
  } 

  PlotAllCurves(idv1);
}

return(0);
}
/***************************************************************************
* LoadPlotData
* Load data to xplot and yplot arrays
* (called by external routines)
*
* INPUT: 
*   reset_first: flag used to reset all private arrays
*                (if 1: erase all old data; if 0: add this data to old data)
*
* Used private variables:
* double *xplot, *yplot;
* int *npts, nmaxi,  ncurves, ncurves_maxi;
* char nchar_type*4, pcolor*32;
* wxString m_filename;
***************************************************************************/
int JLP_wxGseg_Canvas::LoadPlotData(double *xplot, double *yplot, 
                                  const int npts, const char *nchar_type, 
                                  const char *pcolor, const char *plot_fname, 
                                  const int reset_first)
{
int icur, k, kk;

if(reset_first) {
  should_plot_allcurves = 0;
  ncurves1 = 0;
  }

icur = ncurves1;
if(icur >= ncurves_maxi1){
  fprintf(stderr, "LoadPlotData/Error icurve=%d ncurves_maxi=%d\n",
          icur, ncurves_maxi1);
  return(-2);
  }

if(npts >= nmaxi1) {
  fprintf(stderr, "LoadPlotDataFromFile/Error: npts=%d and maximum limit of npts is %d ! \n",
               npts, nmaxi1);
  return(-1);
  }

kk = nmaxi1 * icur;
  for(k = 0; k < npts; k++) {
     xplot1[k + kk] = (float)xplot[k];
     yplot1[k + kk] = (float)yplot[k]; 
     errx1[k + kk] = 0.;
     erry1[k + kk] = 0.;
    }

// Set number of points of curve #icur
  npts1[icur] = npts;

// Set color and line type for the curves:
  sprintf(&nchar1[icur*4], nchar_type);
  strcpy(&pcolor1[icur*32], pcolor);
  strcpy(&plot_fname1[icur*128], plot_fname);

// Increment number of curves:
  ncurves1++;

return(0);
}
/***************************************************************************
* LoadPlotDataFromFile
* Read an ASCII file and load data to xplot and yplot arrays
*
* Used private variables:
* double *xplot, *yplot;
* int *npts, nmaxi,  ncurves, ncurves_maxi;
* wxString m_filename;
***************************************************************************/
int JLP_wxGseg_Canvas::LoadPlotDataFromFile(char *filename, 
                                          const int reset_first)
{
int status, icur, k, kk, nval;
float w1, w2;
FILE *fp;
char buffer[80], nchar_type[4], pcolor[32];

strcpy(nchar_type, "L0");
strcpy(pcolor, "Default");

if(reset_first) {
  should_plot_allcurves = 0;
  ncurves1 = 0;
  }

icur = ncurves1;
if(icur >= ncurves_maxi1){
  fprintf(stderr, "LoadPlotDataFromFile/Error icurve=%d ncurves_maxi=%d\n",
          icur, ncurves_maxi1);
  return(-2);
  }

if((fp = fopen(filename, "r")) == NULL) {
   fprintf(stderr, "LoadPlotDataFromFile/Error opening input file >%s<\n", 
           filename);
   return(-1);
  }

kk = nmaxi1 * icur;
k = 0;
  while(!feof(fp)) {
   fgets(buffer, 80, fp);
   nval = sscanf(buffer, "%f %f", &w1, &w2);
   if(nval == 2) {
     xplot1[k + kk] = w1;
     yplot1[k + kk] = w2;
     errx1[k + kk] = 0.;
     erry1[k + kk] = 0.;
     k++;
     if(k == nmaxi1 - 1) {
       fprintf(stderr, "LoadPlotDataFromFile/Warning: maximum limit of npts (%d) has been reached ! \n",
               nmaxi1);
       break;
       }
    }
  }

if(k > 1) {
  status = 0;
  npts1[icur] = k;
  strcpy(&nchar1[4*icur], nchar_type);
  strcpy(&pcolor1[32*icur], pcolor);
  strcpy(&plot_fname1[icur*128], filename);
  ncurves1++;
  m_filename1.Printf("%s", filename);
} else {
  fprintf(stderr,"LoadPlotDataFromFile/Error reading %s\n npoints=%d", 
          filename, k);
  status = -1;
}

fclose(fp);
return(status);
}
/************************************************************************
* Setup plot configuration
* (called by external routines)
*
* Allocate memory if needed
************************************************************************/
void JLP_wxGseg_Canvas::InitPlotData(const int nmaxi, const int ncurves_maxi, 
                                   const int nout_maxi)
{
// Set the value of private paramaters:
nmaxi1 = nmaxi;
ncurves_maxi1 = ncurves_maxi;
nout_maxi1 = nout_maxi;

if(xplot1 != NULL) delete[] xplot1;
if(yplot1 != NULL) delete[] yplot1;
if(errx1 != NULL) delete[] errx1;
if(erry1 != NULL) delete[] erry1;
if(npts1 != NULL) delete[] npts1;

xplot1 = new float[nmaxi1 * ncurves_maxi1];
yplot1 = new float[nmaxi1 * ncurves_maxi1];
errx1 = new float[nmaxi1 * ncurves_maxi1];
erry1 = new float[nmaxi1 * ncurves_maxi1];
npts1 = new int[ncurves_maxi1];

ncurves1 = 0;
npts1[0] = 0;
should_plot_allcurves = 0;
first_time_curves_are_plotted = 1;

// LoadPlotSettings(xlabel, ylabel, title, xgrid_is_wanted,
//                  ygrid_is_wanted, jlp_axes_are_wanted, iplan, x1, x2, y1, y2)
LoadPlotSettings("Title", "X axis", "Y axis", cgdev_settings1.xgrid, 
                 cgdev_settings1.ygrid, cgdev_settings1.box_type, 
                 0, 0., 0., 0., 0.);
m_filename1 = wxT("");

// Other options:
error_bars1 = 0;
full_caption1 = 0;
x_is_log10_1 = 0;
y_is_log10_1 = 0;

expand1 = 1.2;

// Nout used when interactive input of data points:
if(xout1 != NULL) delete[] xout1;
if(yout1 != NULL) delete[] yout1;
xout1 = new float[nout_maxi1];
yout1 = new float[nout_maxi1];

return;
}
/*************************************************************
* Compute the min/max values for the box
*************************************************************/
void JLP_wxGseg_Canvas::ComputeXYMinMaxForCurves(double& xmindata, 
                                               double& xmaxdata, 
                                               double& ymindata, 
                                               double& ymaxdata)
{
double ww;
float delta, wscale;
int i, k;

// Compute the minimum and maximum :
// Compute minimum and maximum for the Y axis:
ymindata = yplot1[0];
ymaxdata = yplot1[0];
for(k = 0; k < ncurves1; k++) {
  for(i = 1; i < npts1[k]; i++) {
      ww = yplot1[i + k * nmaxi1];
      ymindata = MINI(ymindata, ww);
      ymaxdata = MAXI(ymaxdata, ww);
     }
  }

// Compute minimum and maximum for the X axis:
xmindata = xplot1[0];
xmaxdata = xplot1[0];
for(k = 0; k < ncurves1; k++) {
  for(i = 1; i < npts1[k]; i++) {
      ww = xplot1[i + k * nmaxi1];
      xmindata = MINI(xmindata, ww);
      xmaxdata = MAXI(xmaxdata, ww);
     }
  }

// JLP_axes : takes +/- 4 percents larger frame (for y only)
 if(cgdev_settings1.box_type == 1) {
   wscale = 0.04;
   } else {
// SMongo axes: takes +/- 5 percents larger frame (for y only)
   wscale = 0.05;
   }
 delta = (ymaxdata - ymindata) * wscale;
 ymaxdata += delta;
 ymindata -= delta;
  
/*
printf("DDDD/delta=%f xmindata=%f xmaxdata=%f ymindata=%f ymaxdata=%f\n",
        delta, xmindata, xmaxdata, ymindata, ymaxdata); 
*/

return;
}
/*************************************************************
* Compute the min/max values for the box
*************************************************************/
void JLP_wxGseg_Canvas::ComputeYMinMaxForCurves(double xmindata, double xmaxdata,
                                              double& ymindata, 
                                              double& ymaxdata)
{
double ww;
float delta, wscale;
int i, k;

// Compute the minimum and maximum :
// Compute minimum and maximum for the Y axis:
ymindata = +1.e+10;
ymaxdata = - ymindata;
for(k = 0; k < ncurves1; k++) {
  for(i = 1; i < npts1[k]; i++) {
     if((xplot1[i + k * nmaxi1] >= xmindata)
        && (xplot1[i + k * nmaxi1] < xmaxdata)) {
        ww = yplot1[i + k * nmaxi1];
        ymindata = MINI(ymindata, ww);
        ymaxdata = MAXI(ymaxdata, ww);
        }
     }
  }

// JLP_axes : takes +/- 4 percents larger frame (for y only)
 if(cgdev_settings1.box_type == 1) {
   wscale = 0.04;
   } else {
// SMongo axes: takes +/- 5 percents larger frame (for y only)
   wscale = 0.05;
   }
 delta = (ymaxdata - ymindata) * wscale;
 ymaxdata += delta;
 ymindata -= delta;

/*
printf("DDDD/delta=%f xmindata=%f xmaxdata=%f ymindata=%f ymaxdata=%f\n",
        delta, xmindata, xmaxdata, ymindata, ymaxdata);
*/

return;
}
/*************************************************************
* Load plot the settings that will be used by PlotDisplay() 
* (generally called by external routines)
* INPUT:
* iplan: same scale in X and Y if iplan=1
* x1,x2,y1,y2 : x and y boundaries (user coordinates)
*               x1=x2=0 if automatic x scale
*               y1=y2=0 if automatic y scale
*************************************************************/
int JLP_wxGseg_Canvas::LoadPlotSettings(const char *xlabel, const char *ylabel,
                                      const char *title, 
                                      const int xgrid_is_wanted, 
                                      const int ygrid_is_wanted, 
                                      const int jlp_axes_are_wanted,
                                      const int iplan, 
                                      const double x1, const double x2,
                                      const double y1, const double y2)
{
// Labels:
strcpy(xlabel1, xlabel);
strcpy(ylabel1, ylabel);
strcpy(title1, title);

// iplan: same scale in X and Y if iplan=1
iplan1 = iplan;

// x1,x2,y1,y2 : x and y boundaries (user coordinates)
//               x1=x2=0 if automatic x scale
//               y1=y2=0 if automatic y scale
x1_1 = x1; 
x2_1 = x2; 
y1_1 = y1; 
y2_1 = y2; 

// Background grid flag:
cgdev_settings1.xgrid = xgrid_is_wanted;
cgdev_settings1.ygrid = ygrid_is_wanted;

// JLp axes flag:
cgdev_settings1.box_type = jlp_axes_are_wanted;

return(0);
}
/*************************************************************
* Init plot parameters before plotting the curves with PlotAllCurves()
*************************************************************/
void JLP_wxGseg_Canvas::InitBeforePlot()
{

// Exit if no curve entered yet:
if(ncurves1 == 0 || initialized != 1234) return;

// Rescale the box with xmindata,xmaxdata,ymindata,ymaxdata
// with ComputeMinMaxForCurves
BoxLimitsAuto();

return;
}
/*************************************************************
* Plot the curves to idv2 device 
* idv1 = idv1 for display on screen
*  or postscript dev for copy to file 
*************************************************************/
void JLP_wxGseg_Canvas::PlotAllCurves(int idv2)
{
/* Draw graph */
   jlp_gsegraf0->GSEG_GetAxisLabelPixbufs();
   jlp_gsegraf0->GSEG_DrawGraph();
// Update extra labels if any
   jlp_gsegraf0->GSEG_DrawExtraLabels();
return;
}
/************************************************************************
* To save display to postscript file
* (called by Popup menu)
************************************************************************/
int JLP_wxGseg_Canvas::PstCopyOfDisplay(char *pst_filename)
{
float xmin0, xmax0, ymin0, ymax0;
int plan0, idv0, status = -1;
JLP_cGDev *Jwx;
char plotdev[60], title[60], err_messg[128];
wxString buffer;

// plotdev: "square" or "fullpage" (horizontal landscape)
sprintf(title," ");
//strcpy(plotdev, "square");
strcpy(plotdev, "fullpage");

xmin0 = 0.; xmax0 = 1.;
ymin0 = 0.; ymax0 = 1.;
plan0 = 0; italk = 0;

// New JLP_wxGseg_Canvas object (for display on postscript file)
Jwx = new JLP_cGDev_PST(plotdev, pst_filename, title, &xmin0, &xmax0,
                        &ymin0, &ymax0, plan0, &status, &idv0, err_messg);
if(status) {
  buffer.Printf(wxT("Creating new JLP_cGDev_PST/Error: %s"), err_messg); 
  wxMessageBox(buffer, wxT("JLP_cGdev_wxID"), wxOK | wxICON_ERROR);
 } else {
  InitBeforePlot();
  PlotAllCurves(idv0);
  free_curve_idv(idv0);
  delete Jwx;
 }

return(status);
}
/************************************************************************
* Update the position of the vertical cursor
* INPUT:
*  x_position_value: x position_value in user coordinates 
************************************************************************/
void JLP_wxGseg_Canvas::UpdateCursor(const double x_position_value)
{
float x1;
float xmindata0, xmaxdata0, ymindata0, ymaxdata0;
int offx0, offy0, axlen0, aylen0, iplan0;

if(ncurves1 > 0) {
  RefreshAllCurves();
// Draw a blue vertical line:
  setrgbcolor(0., 0., 1.);
// Read the values of offx0, offy0, axlen0, aylen0 :
  JLP_GET_PLOT_PARAM(&offx0, &offy0, &axlen0, &aylen0, &xmindata0, &xmaxdata0,
                     &ymindata0, &ymaxdata0, &iplan0, &idv1);

  x1 = x_position_value; 
  JLP_LINE1(&x1, &ymindata0, &x1, &ymaxdata0, &idv1);
// Go back to black color:
  setrgbcolor(0., 0., 0.);
 } 
}
