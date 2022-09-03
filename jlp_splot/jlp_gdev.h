/*************************************************************************
* \file jlp_gdev.h (virtual abstract class) 
* \class JLP_GDev (image/curve Graphic Device) 
* \author JLP
* \date 03/01/2017
* \brief Definition of the graphic drivers (as a virtual class)
*
* Abstract class JLP_GDev ("Graphic Device")
* Definition of the graphic drivers (as a virtual class)
* used in my plotting package
*
* JLP
* Version 13/03/2019
**************************************************************************/
#ifndef __jlp_gdev_h                     /* sentry */
#define __jlp_gdev_h

#include <stdlib.h>          // exit()
#include "jlp_gdev_def.h"    // To define MGO_GC, JLP_GC
#include "jlp_plot1_def.h"   // To define PLOT1_SETTINGS, PLOT1_FILE_DATA 
#include "jlp_gsegraf.h"     // JLP_GSEG_PLOT_DATA, GSEG_AXIS_DATA

#define MAX_CURSOR_NPOINTS 1024

#define NCURVES_MAX 256           // Maximum number of curves

class JLP_GText;

/* Definition of the prototype of all JLP_GDev classes
* (i.e. for X11, postscript, gxwindows, etc)
*/
class JLP_GDev {

public:

// Abstract class should not have any definition of the constructors

// Open the device: 
  virtual int open_device(const char* title, int dev_width, int dev_height,
                          const int landscape, int *jgc_dev_width, 
                          int *jgc_dev_height, int *dev_yorigin_is_on_top) = 0; 

// To set Jgc0.cwidth and Jgc0.cheight:
  virtual int Jgc0_set_font_size_from_device(double expand0) = 0;

// Correspondance with popup menu:
  virtual void Update_JGC_from_GDProcSettings() {return;};
  virtual void Update_GDProcSettings_from_JGC() {return;};

// To draw a line:
  virtual int line_device(int x1, int y1, int x2, int y2, int lwidth=0) = 0; 

// To draw a polygon:
  virtual int polygon_device(int x, int y, double expand, double angle, 
                             int nsides, int filled) = 0; 
// To draw a circle:
  virtual int circle_device(int x, int y, int idiam, int filled) = 0; 

// To fill a rectangle (i.e. to paint it with a color):
  virtual int gdev_FilledRect1(double x0, double y0, double x1, double y1, 
                               char *color) = 0;

// Set color of line:
  virtual int setrgbcolor(double r, double g, double b) = 0; 
  virtual int setdefaultcolor() = 0; 

// Set line width and line type:
  virtual int SetLineWidthAndType(int lwidth, int ltype) = 0;

// Erase everything:
  virtual int gdev_erase() = 0; 

// Wait for next event (mouse button for X11)
  virtual int wait_for_events() {return -1;};

// Flush graphic to device
  virtual int gdev_gflush() = 0; 

// Cursor function: 
// (direct display means that values will be printed to stdout) 
  virtual int cursor(int& x, int& y, char* cursor_type, int& pressed_button) {
                     x = y = pressed_button = 0;
                     return -1;
                     }; 

// Get one or two concentric circles (entered interactively by the user):
  virtual int get_circles(double& x_cent, double& y_cent, double& diam1,
                          double& diam2, int& ncirc) {
                          x_cent = y_cent = diam1 = diam2 = 0.; 
                          ncirc = 0;
                          return -1; 
                          };

// Get limits of a small window (entered interactively by the user):
  virtual int get_winlimits(double& x1, double& y1, double& x2, double& y2, 
                            int type_of_win, int& pressed_button, 
                            int& in_frame) {
                            x1 = y1 = x2 = y2 = pressed_button = 0;
                            in_frame= 0; 
                            return(-1); 
                            };

// Close graphic device (and call destructor)
  virtual int gdev_close() = 0; 

// Status Bar (for interactive display only):
  virtual int CreateStatusBar(int ix, int iy, int width, int height)
       { return(-1);};
  virtual int DrawToStatusBar(char *label) { return(-1);};
  virtual int EraseStatusBar() { return(-1);};

// Cursor Position (for interactive display only):
  virtual int CreateCursorPosition(int ix, int iy, int width, int height)
       { return(-1);};
  virtual int DrawToCursorPosition(char *label) { return(-1);};
  virtual int EraseCursorPosition() { return(-1);};

// Write a label with device driver:
  virtual double label_device(const char *s, int xstart, int ystart, double angle,
                             double expand, int draw_it) = 0; 

// Conversion user/device coordinates:
// (defined in jlp_gdev_wxwid.h, jlp_gdev_pstcopy.h, etc)
  virtual void ConvDevToUserForImages(double dev_x0, double dev_y0,      
                        double *user_x1, double *user_y1, int *in_frame) = 0;
  virtual void ConvUserToDevForImages(double user_x1,  double user_y1,
                        double *dev_x0, double *dev_y0, int *in_frame) = 0;

// BOF *********** virtual image SPECIFIC 
// General setup called by all constructors (directly,
// since it does not work with a constructor located here): 
  virtual int setup_device_for_image(const char *plotdev, const char *title,
                                     const int nx1, const int ny1, int *offx1,
                                     int *offy1, int *axlen1, int *aylen1,
                                     int *dev_width, int *dev_height, 
                                     int *TeX_flag, int *dev_type, 
                                     int *landscape) {
      return(-1);
      };
// Allocate LUT: part devoted to the specific device:
  virtual int alloc_lut_device(int *mylut, int *private_lut, int *ncolors) {
      return(-1);
      };
// Load (r,g,b) values to cells whose addresses are stored in mylut array:
// Warning: the values of mylut may be modified in the case of read-only cells. 
  virtual int load_lut_device(const int *r, const int *g, const int *b, 
                              int *mylut, int ncolors) { 
      return(-1);
      };
// Plot an image on the graphic device 
  virtual int plot_image(int *image, int nx, int ny, int idim, int xstart, 
                         int ystart, int gamma1, int gamma_d, 
                         int black_and_white) { 
      return(-1);
      };
// EOF *********** virtual image SPECIFIC 

// BOF *********** virtual curve SPECIFIC
// General setup called by all constructors (directly,
// since it does not work with a constructor located here):
  virtual int setup_device_for_curve(const char *plotdev, const char *title,
                                     int *dev_width, int *dev_height,
                                     int *TeX_flag,
                                     int *dev_type, int *landscape) = 0;
// Setup menu:
  virtual int setup_menu(char *items, const int nitems, const int menu_nsub,
                         const int menu_slen, const int vertical) {
      return(-1);
      };
// Select menu item:
  virtual int select_menu(int& select, int& subselect) {
     select = subselect = -1;
     return(-1);
     };
// Hardcopy possibility called by jlp_hardcopy_image (in jlp_splot.cpp):
// (only available for postscript (in jlp_gd_pstcopy.cpp))
  virtual int pstcopy1(int *image1, int nx1_0, int ny1_0, int idim,
                       double low_thresh, double high_thresh, int ncolors,
                       double width, double height, char *title, 
                       char *image_name, char *image_comments, 
                       char *extra_comments,
                       int lut_scale, int black_and_white, int high_resolution,
                       int f1_xstart, int f1_ystart, int f1_xend, int f1_yend){
    return(-1);
    };
// EOF *********** virtual curve SPECIFIC

// *********************************
// In "jlp_gdev_utils.cpp":
// ********************************
  int gdev_line(int x1, int y1, int x2, int y2, int lwidth=0, 
                int backup_to_file=0);
  int gdev_line1(double user_x1, double user_y1, double user_x2, 
                 double user_y2, int lwidth=0, int backup_to_file=0); 
  int gdev_moveto(const int x, const int y);
  void gdev_lineto(const int x, const int y);
  int SetPColor(char *pcolor); 
  int GetCurrentPColor(char *pcolor);
  double gdev_label(const char *s, int ixstart, int iystart, double angle,
                    double expand, int draw_it, int backup_to_file = 0);
  double gdev_label1(const char *s, double xstart, double ystart, double angle,
             double expand, int draw_it, int backup_to_file);
  double gdev_label_TeX(const char *s, int xstart, int ystart, double angle,
                  double expand, int draw_it);
  void conv_user_to_mgo(double user_x, double user_y, int *ix, int *iy);
  void conv_mgo_to_user(int ix, int iy, double *user_x, double *user_y,
                        int *in_frame);
  int ConvDevToUser(double dev_x0, double dev_y0,      
                        double *user_x1, double *user_y1, int *in_frame);
  int ConvUserToDev(double user_x1,  double user_y1,
                    double *dev_x0, double *dev_y0, int *in_frame);
  int ConvMgoToDev(int mgo_x1,  int mgo_y1, double *dev_x0, double *dev_y0); 
  int ConvDevToMgo(double dev_x0, double dev_y0, int *mgo_x1, int *mgo_y1);
  int SetNewDeviceSize(const int width, const int height);
  int Set_ltype_and_lwidth(const int ltype, const int lwidth);
// *******************************


// Contained in "jlp_gdev.cpp"
  int SetupForCurve(JLP_GDev* Jgd, const char *plotdev,
                    const char *out_fname, const char* title,
                    double xmin_user, double xmax_user, double ymin_user,
                    double ymax_user, const int plan, char *err_messg);
  int SetupForImage(JLP_GDev* Jgd, const char *plotdev, 
                    const char *out_fname, const char* title, double *image_f1,
                    const int nx1, const int ny1, const int nz1,
                    int *gamma1, int *gamma_d, char *err_messg);
  int open_backup_file(const char* fname);
  int close_backup_file();
  int init_JGC(const char *out_fname0, const int offx0,
                       const int offy0, const int axlen0, const int aylen0,
                       const double expand0, const char *pen_colour, 
                       const char *pen_default_colour,
                       const char *backgd_colour, 
                       const double box_xmin0, const double box_xmax0,
                       const double box_ymin0, const double box_ymax0,
                       const double box_zmin0, const double box_zmax0,
                       const int box_type0, const int box_plan0,
                       const int box_xgrid0, const int box_ygrid0,
                       const int box_ticks_in0, 
                       const char *box_xlabel0, const char *box_ylabel0,
                       const char *box_zlabel0, const char *box_title0,
                       const int xaxis_type, const int yaxis_type,
                       const int zaxis_type, const int dev_type0,
                       const int dev_width0, const int dev_height0,
                       const int Tex_flag0,
                       const double xmin_user0, const double xmax_user0,
                       const double ymin_user0, const double ymax_user0,
                       const double zmin_user0, const double zmax_user0,
                       double* image_f0, const int nx0, const int ny0,
                       const int nz0, const int idv0);
  int FromJgc0ToMgc0();
  void GetBoxLimits(double *box_xmin, double *box_xmax, double *box_ymin,
                    double *box_ymax, double *box_zmin, double *box_zmax);
  int SetBoxLimits(const double box_xmin, const double box_xmax,
                   const double box_ymin, const double box_ymax,
                   const double box_zmin, const double box_zmax);
  int SetNewBoxLimitsForPlan(double *xmin, double *xmax, double *ymin,
                             double *ymax);
  int SetNewBoxSetup(const int offx, const int offy,
                     const int axlen, const int aylen);
  int ComputeGamma(const int nx1, const int ny1, const int max_size,
                   int *gamma1, int *gamma_d);

// Contained in "jlp_gdev_box.cpp"
  int jlp_box(double box_xmin0, double box_xmax0, double box_ymin0,
              double box_ymax0, char *xlabel, char *ylabel, char *title,
              int ticks_in, int box_numbers, int full_caption,
              int jlp_axes_are_wanted, int xgrid_is_wanted, int ygrid_is_wanted,
              int x_is_log10, int y_is_log10, double expand, char *axis_color);
  int jlp_box_for_image(char *xlabel, char *ylabel, char *title, int ticks_in,
                        int box_numbers, int full_caption, double expand,
                        char *axis_color);
  int jlp_box_type0(char *xlabel, char *ylabel, char *title, int ticks_in,
               int box_numbers, int full_caption, int xgrid_is_wanted,
               int ygrid_is_wanted, double box_xmin0, double box_xmax0,
               double box_ymin0, double box_ymax0, int ix0, int iy0,
               int ixlen, int iylen, double expand, char *axis_color);
  int jlp_axis(int xorigin, int yorigin, int axis_length,
               double box_min, double box_max, double angle, int label_flag,
               int ticks_up, int TeX_flag, double expand, int *smallest_ix,
               int *smallest_iy);
  int jlp_DrawBoxLabels(char *xlabel, char *ylabel, char *title,
                         int full_caption, int ix0, int iy0, int ixlen,
                         int iylen, double expand, int smallest_ix, 
                         int smallest_iy);
  int jlp_grid21(int xorigin, int yorigin, int axis_length,
                 int other_axis_length, double box_min, double box_max,       
                 double axis_angle, double other_axis_angle, char *nchar,
                 char *axis_color);

/* jlp_gdev_box_sciplot.cpp */
int jlp_box_sciplot(char *xlabel, char *ylabel, char *title, int ticks_in,
                    int box_numbers, int full_caption,
                    int xgrid_is_wanted, int ygrid_is_wanted,
                    int x_is_log10, int y_is_log10,
                    double xmin_data, double xmax_data, double ymin_data,
                    double ymax_data, int ix0, int iy0, int ixlen, int iylen,
                    char *axis_color);
int jlp_sciplot_InitializeAxes(SciPlotAxis *x_axis, SciPlotAxis *y_axis);
int jlp_sciplot_DrawAxis(SciPlotAxis Axis, int xorigin, int yorigin,
                        int axis_length, double min_data, double max_data,
                        double angle, int plot_AxisNumbers,
                        int ticks_up, int labels_down, int Axis_is_log10);
int jlp_sciplot_DrawAxis_part2(SciPlotAxis Axis, int xorigin, int yorigin,
                        int axis_length, double box_min, double box_max,
                        double angle, int plot_AxisNumbers,
                        int ticks_up, int labels_down, int Axis_is_log10,
                        int xgrid, int big_ticklen, int small_ticklen);
int jlp_sciplot_DrawGrid(SciPlotAxis Axis, int xorigin, int yorigin,
                        int axis_length, int other_axis_length,
                        double box_min, double box_max, double axis_angle,
                        double other_axis_angle, char *nchar,
                        char *pcolor, int axis_is_log10);

// Contained in "jlp_gdev_lut.cpp"
  int convert_to_lut(double *image1, int nx1, int ny1, int idim1,
                     int *image2, int nx2, int ny2, int idim2,
                     int ncolors, int itt_is_linear, double lower_itt,
                     double upper_itt);
  int jlp_alloc_lut(int *ncolors, int *private_lut);
  int jlp_load_lut(const int *r, const int *g, const int *b, int ncolors);
  int jlp_reverse_lut();
  int jlp_change_lut(char *lut_type, int reversed, int *ioff, int *islope,
                     int ncolors);
  int jlp_key(int nn1, double lower_itt, double upper_itt, char *zlabel,
              int horiz, int axis_also, int gamma1, int gamma_d,
              double scale_x, double scale_y);


// Contained in "jlp_gdev_images.cpp"
  int Load2DFitsImage(char *filename0, const int iplane0);
  int LoadDbleImage1(double *dble_image0, int nx0, int ny0);

// Contained in "jlp_gdev_curves.cpp"
  int PlotAllCurves_splot();
  int Curves_LoadPlotSettings(const char *xlabel, const char *ylabel,
                      const char *title, const char *pen_colour,
                      const char *pen_default_colour, const char *backgd_colour,
                      const int xgrid_is_wanted, const int ygrid_is_wanted,
                      const int jlp_axes_are_wanted, const int iplan,
                      const double x1, const double x2,
                      const double y1, const double y2);
  void UpdateCursor(const double x_position);

// Contained in "jlp_gdev_gsegraf.cpp"
  int GDev_UpdateJgcFromGsegAxisData(GSEG_AXIS_DATA gseg_axdata0);
  int GDev_UpdateJgcFromGsegSettings(JLP_Gsegraf *jlp_gsegraf0, int dev_x0, 
                                int dev_y0, int dev_width0, int dev_height0);
  int LoadXYPlot1ToGsegPlot(GSEG_PLOT_DATA *gseg_pltdata0, const int iplot, 
                            int gdev_graphic_type0, int gseg_plot_type0,
                            char *axis_type0, const int pen_width0);

// Contained in "jlp_gdev_symbol.cpp"
  int symbol_errorx1(double x, double y, double errx, int size);
  int symbol_errory1(double x, double y, double erry, int size);
  int symbol(int ix, int iy, int isize, int isymb);
  int symbol1(double x, double y, int size, int isymb);
  int circle1(double x, double y, double diam, int ifill);

// Contained in "jlp_gdev_curves_draw.cpp":
  int draw_curve(double *xplot, double *yplot, double *errx, double *erry,
              int npts0, char *nchar0, char *pcolor0, int error_bars0);
  int draw_curve_line(double *xplot, double *yplot, int npts0, char *nchar0,
                      char *pcolor0);
  int draw_curve_histo(double *xplot, double *yplot, int npts0, char *nchar0,
                       char *pcolor0);
  int draw_dashed_line(double *x, double *y, int npts0, int ltype0, 
                       char *pcolor0);
  int draw_mgo_chopper(int ixa, int iya, int ixb, int iyb, int ltype, int ldef);

// Contained in "jlp_gdev_curves_process.cpp":
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
  void Curves_ResetAllPrivateParameters();
  void Images_ResetAllPrivateParameters();
  int Curves_LoadPlotDataToPrivateParameters0(double *xplot1, 
                          double *yplot1, const int npts1,
                          const char *nchar_type, const char *pcolor,
                          const char *plot_fname, const int reset_first);
  int Curves_LoadPlotDataToPrivateParameters(double *xplot1, 
                          double *yplot1, double *errorx1, 
                          double *errory1, const int npts1,
                          const char *nchar_type, const char *pcolor,
                          const char *plot_fname, const int reset_first);
  void CreatePlotDataArrays(const int nmaxi1, const int ncurves_maxi1,
                            const int nout_maxi1);
  int Curves_LoadPlotDataToPrivateFromFile(char *plot_filename0, 
                                  int icol_x, int icol_y,
                                  int icol_errx, int icol_erry,
                                  const char *nchar_type, const char *pcolor,
                                  const int reset_first0);
  int Curves_LoadPlotDataToPrivateFromLatex(char *plot_filename0, 
                                  int icol_x, int icol_y,
                                  int icol_errx, int icol_erry,
                                  const char *nchar_type, const char *pcolor,
                                  const int reset_first0);
  int Curves_LoadPlotDataToPrivateFromParamFile(PLOT1_FILE_DATA *pfiledata0,
                             int n_datafiles, int n_pfile_param_max);
  int GetCurveData(double **xplot0, double **yplot0, double **errorx0,
                   double **errory0,  int *npts0, const int icurve);

// Contained in "jlp_gdev_mouse.cpp":
  int Mouse_AddLeftDownPoint(const double dev_x0, const double dev_y0);
  int Mouse_AddLeftUpPoint(const double dev_x0, const double dev_y0);
  int Mouse_GetLastLeftDownPoint(double *dev_x0, double *dev_y0, int *npts0);
  int Mouse_GetLastLeftUpPoint(double *dev_x0, double *dev_y0, int *npts0);
  int Mouse_RemoveLastLeftDownPoint();
  int Mouse_RemoveLastLeftUpPoint();
  void Mouse_EraseLeftDownPoints();
  void Mouse_EraseLeftUpPoints();

/************************************************************************
* Accessors to Jgc0 in jlp_gdev.cpp (not defined in this abstract virtual class:
*************************************************************************/
  int GetMaxLevelForLUT();
  int Jgc0_ncolors();
  int Jgc0_lut(const int i);
  int Jgc0_TeX_flag();
  int Jgc0_dev_type();
  int Jgc0_dev_width();
  int Jgc0_dev_height();
  int Jgc0_dev_yorigin_is_on_top();
  int Jgc0_dev_idv();
  int Jgc0_offx();
  int Jgc0_offy();
  int Jgc0_axlen();
  int Jgc0_aylen();
  int Jgc0_pdef();
  int Jgc0_ldef();
  int Jgc0_ltype();
  int Jgc0_lwidth();
  int Jgc0_box_plan();
  double Jgc0_box_xmin();
  double Jgc0_box_xmax();
  double Jgc0_box_ymin();
  double Jgc0_box_ymax();
  double Jgc0_box_zmin();
  double Jgc0_box_zmax();
  double Jgc0_xmin_user();
  double Jgc0_xmax_user();
  double Jgc0_ymin_user();
  double Jgc0_ymax_user();
  double Jgc0_zmin_user();
  double Jgc0_zmax_user();
  double Jgc0_fsx();
  double Jgc0_fsy();
  double Jgc0_cheight();
  double Jgc0_cwidth();
  FILE *Jgc0_fp_backup();
  int GDevGraphicType();

protected:

/**********************************************************************/
// JLP graphic context: 
  JLP_GC Jgc0;

// JLP mgo graphic context:
  MGO_GC Mgc0;

// For Latex labels:
  JLP_GText *MyText;

// italk flag
// (program is talkative if this flag is set to one or more)
  int italk;       

  char cgdev_current_pcolor[64];
  char xlabel_1[64], ylabel_1[64], title_1[64], filename_1[128];

// Data points stored for internal use:
  int npts_LeftDown_1, npts_LeftUp_1;
  double LeftDown_dev_x_1[MAX_CURSOR_NPOINTS]; 
  double LeftDown_dev_y_1[MAX_CURSOR_NPOINTS];
  double LeftUp_dev_x_1[MAX_CURSOR_NPOINTS]; 
  double LeftUp_dev_y_1[MAX_CURSOR_NPOINTS];

// Maximum level for LUT values (to be set in SetupForImage):
  int MaxColorLevelForLUT;

// Curves:
// Data used by "jlp_gdev_curves.cpp" and "jlp_gdev_curves_process.cpp" routines
  char nchar_1[4*NCURVES_MAX], pcolor_1[32*NCURVES_MAX];
  char plot_fname_1[128*NCURVES_MAX];
  double *xplot_1, *yplot_1, *errorx_1, *errory_1, *xout_1, *yout_1;
  int *npts_1, nmaxi_1, nout_maxi_1, ncurves_1, ncurves_maxi_1;

// Images:
// dble_image_1: 2D array with the data contained in the FITS file
// nx_1, ny_1, nz_1: size of the full data cube
// iplane_1: index of image plane to be loaded from 1 to nz (in case of 3D data)
  double *dble_image_1;
  int nx_1, ny_1, nz_1, iplane_1;
  char fits_filename_1[128];

  bool first_time_curves_are_plotted;

}; 

// In "jlp_lut.cpp"
  int jlp_change_lut_full(char *lut_type, const int reversed, int *ioff,
                        int *islope, int *r, int *g, int *b, const int ncolors,
                        const int max_lut_level);

#endif    /* __jlp_gdev_h sentry */
