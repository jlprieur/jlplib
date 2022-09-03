/**************************************************************************
* Definition of the members of the JLP_GDev class (Image/Curve Graphic Devices)
* used in my plotting package
*
* JLP
* Version 13/02/2017
**************************************************************************/
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "jlp_gdev.h"
#include "jlp_gtext.h"    // JLP_GText
#include "jlp_gdev_idv.h" // GDev_alloc_idv()

/**************************************************************************
* Accessors :
**************************************************************************/
  int JLP_GDev::GetMaxLevelForLUT(){int i0 = MaxColorLevelForLUT; return(i0);};
  // Not possible: char* Jgc0_backgd_colour()
  // Not possible: char* Jgc0_pen_colour()
  // Not possible: char* Jgc0_pen_default_colour()
  int JLP_GDev::Jgc0_ncolors() {int i0 = Jgc0.gclr.ncolors; return(i0);};
  int JLP_GDev::Jgc0_lut(const int i) {
     int i0 = -1;
     if((i < 0) || (i >= Jgc0.gclr.ncolors)){
     fprintf(stderr, "jlp_gdev.h/lut/Fatal error: i=%d (ncolors=%d)\n",
             i, Jgc0.gclr.ncolors);
     } else {
     i0 = Jgc0.gclr.lut[i];
     }
    return(i0);
    };
  int JLP_GDev::Jgc0_TeX_flag() {int i0 = Jgc0.TeX_flag; return(i0);};
  int JLP_GDev::Jgc0_dev_type() {int i0 = Jgc0.dev_type; return(i0);};
  int JLP_GDev::Jgc0_dev_width() {int i0 = Jgc0.dev_width; return(i0);};
  int JLP_GDev::Jgc0_dev_height() {int i0 = Jgc0.dev_height; return(i0);};
  int JLP_GDev::Jgc0_dev_yorigin_is_on_top() {
    int i0 = Jgc0.dev_yorigin_is_on_top; return(i0);
    };
  int JLP_GDev::Jgc0_dev_idv() {int i0 = Jgc0.dev_idv; return(i0);};
  int JLP_GDev::Jgc0_offx() {int i0 = Jgc0.offx; return(i0);};
  int JLP_GDev::Jgc0_offy() {int i0 = Jgc0.offy; return(i0);};
  int JLP_GDev::Jgc0_axlen() {int i0 = Jgc0.axlen; return(i0);};
  int JLP_GDev::Jgc0_aylen() {int i0 = Jgc0.aylen; return(i0);};
  int JLP_GDev::Jgc0_pdef() {int i0 = Mgc0.pdef; return(i0);};
  int JLP_GDev::Jgc0_ldef() {int i0 = Mgc0.ldef; return(i0);};
  int JLP_GDev::Jgc0_ltype() {int i0 = Mgc0.lltype; return(i0);};
  int JLP_GDev::Jgc0_lwidth() {int i0 = Mgc0.lwidth; return(i0);};
  int JLP_GDev::Jgc0_box_plan() {int i0 = Jgc0.box_plan; return(i0);};
  double JLP_GDev::Jgc0_box_xmin() {
    double d0 = Jgc0.axis_limits[0];
    return(d0);
    }
  double JLP_GDev::Jgc0_box_xmax() {
    double d0 = Jgc0.axis_limits[1]; return(d0);
    }
  double JLP_GDev::Jgc0_box_ymin() {
    double d0 = Jgc0.axis_limits[2]; return(d0);
    }
  double JLP_GDev::Jgc0_box_ymax() {
    double d0 = Jgc0.axis_limits[3]; return(d0);
    }
  double JLP_GDev::Jgc0_box_zmin() {
    double d0 = Jgc0.axis_limits[4]; return(d0);
    }
  double JLP_GDev::Jgc0_box_zmax() {
    double d0 = Jgc0.axis_limits[5]; return(d0);
    }
  double JLP_GDev::Jgc0_xmin_user() {double d0 = Jgc0.xmin_user; return(d0);};
  double JLP_GDev::Jgc0_xmax_user() {double d0 = Jgc0.xmax_user; return(d0);};
  double JLP_GDev::Jgc0_ymin_user() {double d0 = Jgc0.ymin_user; return(d0);};
  double JLP_GDev::Jgc0_ymax_user() {double d0 = Jgc0.ymax_user; return(d0);};
  double JLP_GDev::Jgc0_zmin_user() {double d0 = Jgc0.zmin_user; return(d0);};
  double JLP_GDev::Jgc0_zmax_user() {double d0 = Jgc0.zmax_user; return(d0);};
  double JLP_GDev::Jgc0_fsx() {double d0 = Mgc0.fsx; return(d0);};
  double JLP_GDev::Jgc0_fsy() {double d0 = Mgc0.fsy; return(d0);};
  double JLP_GDev::Jgc0_cheight() {double d0 = Jgc0.cheight; return(d0);};
  double JLP_GDev::Jgc0_cwidth() {double d0 = Jgc0.cwidth; return(d0);};
  FILE* JLP_GDev::Jgc0_fp_backup() {FILE *f0 = Jgc0.fp_backup; return(f0);};
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
  int JLP_GDev::GDevGraphicType(){int i0 = Jgc0.gdev_graphic_type; return(i0);}

/**************************************************************************
* General setup in case of curves
*
* Example of call:
* SetupForCurve(*this, "xterm", "Xslice version 1", -1, 3., -23., 45., 0, 1);
***************************************************************************/
int JLP_GDev::SetupForCurve(JLP_GDev* cJgd, const char *plotdev,
                            const char *out_fname, const char* title0,
                            double xmin_user, double xmax_user, 
                            double ymin_user, double ymax_user, 
                            const int box_plan, char *err_messg)
{
int dev_width, dev_height, status, TeX_flag, dev_type, box_type, landscape;
int jgc_dev_width, jgc_dev_height, dev_yorigin_is_on_top;
int offx, offy, axlen, aylen, ix, iy, iwidth, iheight, idv0;
double box_xmin, box_xmax, box_ymin, box_ymax, expand;
char xlabel0[64], ylabel0[64], zlabel0[64];
char pen_colour[64], pen_default_colour[64], backgd_colour[64];

strcpy(pen_colour, "Black");
strcpy(pen_default_colour, "Black");
strcpy(backgd_colour, "White");

// Store pointer to idv static variables and get an identifier (idv0):
status = GDev_alloc_idv(cJgd, &idv0, err_messg);
#ifdef DEBUG
printf("JLP_GDev::SetupForCurve//idv0=%d cJgd=%ld\n", idv0, (long int)cJgd);
#endif
if(status) {
 return(-2);
 }

// Create private plot arrays:
// nmaxi=5120 at least is needed for Spea1
// CreatePlotDataArrays(int nmaxi, int ncurves_maxi, int nout_maxi)
 CreatePlotDataArrays(5120, 128, 256);
 Curves_ResetAllPrivateParameters();

// Old values for screen : 3500,  3500, 28000, 28000 (but problems for curves)
// Old values for postscript 4000, 4000, 26000, 26000
// /* Plot window size:  SCREEN_SIZE = 32767
// JLP2018: I put larger offsets to avoid problems with Y title with LaTex fonts:
 offx = 6000;
 offy = 6000;
 axlen = 26000;
 aylen = 22000;

// Define device window parameters:
// dev_width, dev_height, TeX_flag, and dev_type.
 status = cJgd->setup_device_for_curve(plotdev, title0, &dev_width, &dev_height,
                                       &TeX_flag, &dev_type, &landscape);
// Exit in case of failure to avoid further problems...
 if(status) {
    fprintf(stderr,
            "JLP_GDev/SetupForCurve/Failure in setup_device_for_curve\n");
    return(-1);
    }

#ifdef DEBUG
 printf("JLP_Device/plotdev=%s TeX_flag=%d dev_type=%d landscape=%d \n",
         plotdev, TeX_flag, dev_type, landscape);
 printf("JLP_Device/offx=%d offy=%d axlen=%d aylen=%d dev_width=%d dev_height=%d \n",
        offx, offy, axlen, aylen, dev_width, dev_height);
#endif

// For Tex labels:
if(TeX_flag) MyText = new JLP_GText(status);
 else MyText = NULL;

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
   Jgc0.gdev_graphic_type = 1;

// Initialization of Jgc0 structure:
// (before opening the device, since it is necessary for postscript)
  box_xmin = xmin_user;
  box_xmax = xmax_user;
  box_ymin = ymin_user;
  box_ymax = ymax_user;
// box_type : 1=JLP axes 0=SMongo axes
  box_type = 0;
  expand = 1.2;
  strcpy(xlabel0, "");
  strcpy(ylabel0, "");
  strcpy(zlabel0, "");
  init_JGC(out_fname, offx, offy, axlen, aylen, expand, 
           pen_colour, pen_default_colour, backgd_colour, box_xmin, box_xmax, 
           box_ymin, box_ymax, 0., 1., box_type, box_plan, 0, 0, 1,  
           xlabel0, ylabel0, zlabel0, title0, 0, 0, 0, 
           dev_type,  dev_width, dev_height, TeX_flag, 
           xmin_user, xmax_user, ymin_user, 
           ymax_user, 0., 1., NULL, 0, 0, 0, idv0);

// Check if box_plan == 1,
//          i.e. same scale should be the same for both X and Y axes:
// In that case, change the values of box_xmin, box_xmax, box_ymin, box_ymax:
  if(box_plan) {
      SetNewBoxLimitsForPlan(&box_xmin, &box_xmax, &box_ymin, &box_ymax);
    }

// Open device:
// For postscript: dev_width in mm as input, jgc_dev_width as pts as output
// For postscript: dev_height in mm as input, jgc_dev_height as pts as output
 status = cJgd->open_device(title0, dev_width, dev_height, landscape,
                            &jgc_dev_width, &jgc_dev_height, 
                            &dev_yorigin_is_on_top);
 Jgc0.dev_width = jgc_dev_width;
 Jgc0.dev_height = jgc_dev_height;
 Jgc0.dev_yorigin_is_on_top = dev_yorigin_is_on_top;

// Exit in case of failure to avoid further problems...
  if(status) {
     fprintf(stderr,"JLP_Device/SetupForCurve/Error opening graphic device\n");
     fprintf(stderr,"status = %d\n", status);
     exit(-1);
     }

/* Linear axes: */
 Jgc0.xaxis_type = 0;
 Jgc0.yaxis_type = 0;

// Initialize backup file to NULL:
 Jgc0.fp_backup = NULL;
 strcpy(Jgc0.backup_fname," ");

/* Create Status Bar in the bottom of the display: (only for X11 device yet)*/
ix = 0;
iwidth = (int)((double)SCREEN_SIZE * 0.77);
iheight = (int)((double)SCREEN_SIZE * 0.04);
iy = 0;
CreateStatusBar(ix, iy, iwidth, iheight);

ix += iwidth;
iwidth = (int)((double)(SCREEN_SIZE - ix) * 0.95);
CreateCursorPosition(ix, iy, iwidth, iheight);

// Set color to default:
strcpy(cgdev_current_pcolor, "Default");

// Update MGO parameters:
 FromJgc0ToMgc0();

/* DEBUG
printf("SetupForCurve: xmin,xmax,ymin,ymax: %f %f %f %f\n",
        Jgc0.xmin_user, Jgc0.xmax_user, Jgc0.ymin_user, Jgc0.ymax_user);
*/

return(0);
};

/**************************************************************************
* General setup in case of image display
*
* gamma1 is the ratio nx1/nx2
* gamma_d is the ratio Frame_nx/nx2, 
*         correspondance between output graphic memory array and 
*         integer image_i2 array (set to 1 if original array has been reduced)
*
* Example of call:
* SetupForImage(*this, "display", "Xdisp1 version 1", 128, 128, 1, 1, 1); 
***************************************************************************/
int JLP_GDev::SetupForImage(JLP_GDev* iJgd, const char *plotdev, 
                            const char *out_fname, const char* title0,
                            double *image_f1, const int nx1, const int ny1,
                            const int nz1, int *gamma1, int *gamma_d, 
                            char *err_messg)
{
double box_xmin, box_xmax, box_ymin, box_ymax, expand;
char xlabel0[64], ylabel0[64], zlabel0[64];
char pen_colour[64], pen_default_colour[64], backgd_colour[64];
int idv0;

strcpy(pen_colour, "White");
strcpy(pen_default_colour, "White");
strcpy(backgd_colour, "Black");

/* To be changed later:
*  (the image is reduced by a factor of gamma1 if nx1 or ny1 > max_size)
* max_size=512 leads to the same (big) size for 128x128 (gamma_d=4) 
*              and 256x256 (gamma_d = 2)
* max_size=384 is OK for 128x128 (gamma_d=3) but too small 
*              for 256x256 (gamma_d=1)
*/
 int max_size = 384;
 int dev_width, dev_height, TeX_flag, dev_type, box_type;
 int jgc_dev_width, jgc_dev_height, dev_yorigin_is_on_top;
 int landscape, box_plan, offx, offy, axlen, aylen;
 int ix, iy, iwidth, iheight, status;
 double xmin_user, xmax_user, ymin_user, ymax_user; 

// Create private plot arrays:
// CreatePlotDataArrays(int nmaxi, int ncurves_maxi, int nout_maxi)
  CreatePlotDataArrays(1024, 32, 512);

// Initialize npts_LeftDown_1 and npts_LeftUp_1 to zero
  Images_ResetAllPrivateParameters();

// Boundaries in user coord.:
 xmin_user = ymin_user = 0.;
 xmax_user = (double)nx1;
 ymax_user = (double)ny1;

// Same scale in X and Y, so box_plan is set to one:
 box_plan = 1;

// Store pointer to idv static variables and get an identifier (idv0):
 status = GDev_alloc_idv(iJgd, &idv0, err_messg);
#ifdef DEBUG
printf("JLP_GDev::SetupForImage//idv0=%d cJgd=%ld\n", idv0, (long int)cJgd);
#endif
 if(status) {
  return(-2);
  }

/***************************************************************
* Meaning of gamma1 (compression factor) and gamma_d (dilatation factor)
* (used for image display):
*
*  image_f1[nx1,ny1] is the initial image
*  image_i[nx2,ny2] is the reduced or initial image converted to LUT numbers
*  (the image is reduced if nx1 or ny1 > max_size)
*
* width_frame, height_frame: size of the frame in pixels (X11 display)
* gamma1 is the ratio nx1/nx2
* gamma_d is the ratio width_frame/nx2,
*         correspondance between output graphic memory array and
*         integer image_i array (set to one if original array has been reduced)
* We always have gamma1 >= 1 and gamma_d >= 1
* and if gamma1 > 1 then gamma_d =1
*  or if gamma_d > 1 then gamma1 = 1
*****************************************************************/
  ComputeGamma(nx1, ny1, max_size, gamma1, gamma_d);

// Define device window parameters: 
//    offx, offy, axlen, aylen, dev_width, dev_height, TeX_flag, and dev_type.
 status = iJgd->setup_device_for_image(plotdev, title0, 
                                     (nx1 * (*gamma_d)) / *gamma1, 
                                     (ny1 * (*gamma_d))/ *gamma1 , 
                                     &offx, &offy, &axlen, &aylen, 
                                     &dev_width, &dev_height, 
                                     &TeX_flag, &dev_type, &landscape);

 if(status) {
    fprintf(stderr,
            "SetupForImage/Fatal error: failure in setup_device_for_image\n");
    return(-1);
    }

#ifdef DEBUG
    printf("SetupForImage/TeX_flag=%d dev_type=%d landscape =%d gamma1=%d gamma_d=%d\n", 
            TeX_flag, dev_type, landscape, *gamma1, *gamma_d);
    printf("SetupForImage/offx=%d offy=%d axlen=%d aylen=%d dev_width=%d dev_height=%d \n",
           offx, offy, axlen, aylen, dev_width, dev_height);
#endif

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
  Jgc0.gdev_graphic_type = 3;

// Initialization of Jgc0 structure: 
  box_xmin = xmin_user;
  box_xmax = xmax_user;
  box_ymin = ymin_user;
  box_ymax = ymax_user;
// (before opening the device, since it is necessary for postscript)
// box_type : 1=JLP axes 0=SMongo axes
  box_type = 0;
  expand = 1.2;
  strcpy(xlabel0, "");
  strcpy(ylabel0, "");
  strcpy(zlabel0, "");
  init_JGC(out_fname, offx, offy, axlen, aylen, expand, 
           pen_colour, pen_default_colour, backgd_colour, box_xmin, box_xmax, 
           box_ymin, box_ymax, 0., 1., box_type, box_plan, 0, 0, 1,  
           xlabel0, ylabel0, zlabel0, title0, 0, 0, 0, 
           dev_type,  dev_width, dev_height, TeX_flag, 
           xmin_user, xmax_user, ymin_user, 
           ymax_user, 0., 1., image_f1, nx1, ny1, nz1, idv0); 

// JLP2017: load current image (needed for displaying 3D plot with jlp_gsegraf)
  LoadDbleImage1(image_f1, nx1, ny1);

// Open device:
// For postscript: dev_width in mm as input, jgc_dev_width as pts as output
// For postscript: dev_height in mm as input, jgc_dev_height as pts as output
 status = iJgd->open_device(title0, dev_width, dev_height, landscape,
                            &jgc_dev_width, &jgc_dev_height, 
                            &dev_yorigin_is_on_top);
 Jgc0.dev_width = jgc_dev_width;
 Jgc0.dev_height = jgc_dev_height;
 Jgc0.dev_yorigin_is_on_top = dev_yorigin_is_on_top;

 if(status) {
    fprintf(stderr,"SetupForImage/Fatal error opening graphic device\n");
    return(-1);
    }

// Initialize backup file to NULL:
 Jgc0.fp_backup = NULL;
 strcpy(Jgc0.backup_fname," ");

/* Linear axes: */
 Jgc0.xaxis_type = 0;
 Jgc0.yaxis_type = 0;

/* Create Status Bar on top of display (only for X11 device yet): */
ix = 0;
iwidth = (int)((double)SCREEN_SIZE * 0.77);
iheight = (int)((double)SCREEN_SIZE * 0.04);
iy = SCREEN_SIZE - iheight;
CreateStatusBar(ix, iy, iwidth, iheight);

ix += iwidth;
iwidth = (int)((double)(SCREEN_SIZE - ix) * 0.95);
CreateCursorPosition(ix, iy, iwidth, iheight);

return(0);
};

/*********************************************************************
* Possibility of handling a backup file: 
* Open backup file:
*********************************************************************/
int JLP_GDev::open_backup_file(const char* fname) {

// Close previous file if a backup file has been already opened:
 if(Jgc0.fp_backup != NULL) {
   fclose(Jgc0.fp_backup);
   Jgc0.fp_backup = NULL;
   }

// Open new file: 
 strcpy(Jgc0.backup_fname,fname);
 if((Jgc0.fp_backup = fopen(Jgc0.backup_fname,"w")) == NULL) {
   fprintf(stderr,"JLP_GDev::open_backup_file/Fatal error opening %s \n",
           Jgc0.backup_fname);
   exit(-1);
   }

return(0);
}
/*********************************************************************
* Possibility of handling a backup file:
* Close backup file:
*********************************************************************/
int JLP_GDev::close_backup_file() {
int status = -1;

// Close previous file if a backup file has been already opened:
 if(Jgc0.fp_backup != NULL) {
   fclose(Jgc0.fp_backup);
   Jgc0.fp_backup = NULL;
   status = 0;
   }

return(status);
}
/**********************************************************************
* To initialize JLP Graphic Context
* This routine should be called by setup functions
*
**********************************************************************/
int JLP_GDev::init_JGC(const char *out_fname0, const int offx0, 
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
                       const int nz0, const int idv0)
{
/* Save output postscript filename to Jgc0 graphic context:  */
 strcpy(Jgc0.fdv_pst_fname, out_fname0);

 Jgc0.offx = offx0;
 Jgc0.offy = offy0;
 Jgc0.axlen = axlen0;
 Jgc0.aylen = aylen0;
 Jgc0.expand = expand0;

 strcpy(Jgc0.pen_colour, pen_colour);
 strcpy(Jgc0.pen_default_colour, pen_default_colour);
 strcpy(Jgc0.backgd_colour, backgd_colour);

// Box parameters:
 Jgc0.axis_limits[0] = box_xmin0; 
 Jgc0.axis_limits[1] = box_xmax0; 
 Jgc0.axis_limits[2] = box_ymin0; 
 Jgc0.axis_limits[3] = box_ymax0; 
 Jgc0.axis_limits[4] = box_zmin0; 
 Jgc0.axis_limits[5] = box_zmax0; 
 Jgc0.box_type = box_type0;
 Jgc0.box_plan = box_plan0;
 Jgc0.box_xgrid = box_xgrid0;
 Jgc0.box_ygrid = box_ygrid0;
 Jgc0.box_ticks_in = box_ticks_in0;
 strcpy(Jgc0.box_xlabel, box_xlabel0);
 strcpy(Jgc0.box_ylabel, box_ylabel0);
 strcpy(Jgc0.box_zlabel, box_zlabel0);
 strcpy(Jgc0.box_title, box_title0);

// Axis type: 0=linear 1=log10
// Assume linear axes
 Jgc0.xaxis_type = 0;
 Jgc0.yaxis_type = 0;
 Jgc0.zaxis_type = 0;

 Jgc0.dev_type = dev_type0;
 Jgc0.dev_width = dev_width0;
 Jgc0.dev_height = dev_height0;
 Jgc0.dev_idv = idv0;

 Jgc0.TeX_flag = Tex_flag0;

// Data parameters
 Jgc0.xmin_user = xmin_user0; 
 Jgc0.xmax_user = xmax_user0; 
 Jgc0.ymin_user = ymin_user0; 
 Jgc0.ymax_user = ymax_user0; 
 Jgc0.zmin_user = zmin_user0; 
 Jgc0.zmax_user = zmax_user0; 

 Jgc0.image_f = image_f0;
 Jgc0.nx = nx0;
 Jgc0.ny = ny0;
 Jgc0.nz = nz0;

 Jgc0.cheight = 1.0;                // official height of characters
 Jgc0.xp = 0;                       // Present plot x position (mgo coord.)
 Jgc0.yp = 0;                       // Present plot y position (mgo coord.) 

// Update MGO parameters:
 FromJgc0ToMgc0();
return(0);
}
/*********************************************************************
* Correspondance from Jgc0 to Mgc0 parameters
* (from the old correspondance between "jlp_X11.h" and "mongo.h")
*********************************************************************/
int JLP_GDev::FromJgc0ToMgc0()
{
/* Conversion from user to mgo coordinates:
* imgo = offx +  fsx * (x - box_xmin)
*/
 Mgc0.fx1 = Jgc0.axis_limits[0];          // User coords of graphics area
 Mgc0.fx2 = Jgc0.axis_limits[1];
 Mgc0.fy1 = Jgc0.axis_limits[2];
 Mgc0.fy2 = Jgc0.axis_limits[3];
 Mgc0.gx1 = Jgc0.offx;         // mgo coords of graphics area
 Mgc0.gx2 = Jgc0.offx+Jgc0.axlen;
 Mgc0.gy1 = Jgc0.offy;
 Mgc0.gy2 = Jgc0.offy+Jgc0.aylen;
// axis_limits[6]: 0=xmin, 1=xmax, 2=ymin, 3=ymax, 4=zmin, 5=zmax
 Mgc0.fsx = (double)(Mgc0.gx2 - Mgc0.gx1) / (double)(Mgc0.fx2 - Mgc0.fx1); 
                                    // fsx = (gx2-gx1)/(fx2-fx1)
 Mgc0.fsy = (double)(Mgc0.gy2 - Mgc0.gy1) / (double)(Mgc0.fy2 - Mgc0.fy1); 
                                    // fsy = (gy2-gy1)/(fy2-fy1)

// Default values:
 Mgc0.aangle = 0.;                  // rotation of chars and points
 Mgc0.eexpand = Jgc0.expand;        // Global expansion of chars, pts
 Mgc0.ffcos = 1.;
 Mgc0.ffsin = 0.;                   // cos(aangle), sin(aangle)
                                        // DO NOT USE fcos and fsin...
 Mgc0.ldef = 32;                    // scale spacing for lwidth>0 lines
 Mgc0.lltype = 0;                   // Line type: dotted, etc.
 Mgc0.lwidth = 0;                   // Line width: 0,1,2...etc
 Mgc0.pdef = 128;                   // scale size for point
 Mgc0.termout = 1;                  // True if plotting on terminal

/* DEBUG
printf("FromJgc0ToMgc0/offx,offy,axlen,aylen: %d %d %d %d\n",
        Jgc0.offx, Jgc0.offy, Jgc0.axlen, Jgc0.aylen);
printf("FromJgc0ToMgc0/device width=%d height=%d\n",
        Jgc0.dev_width, Jgc0.dev_height);
*/

return 0;
}
/***********************************************************
* limits_for_plan
* Compute xmin,xmax,ymin,ymax for same scale in X and Y
*
* INPUT:
* xmin,xmax,ymin,ymax: as given by the user
*
* OUTPUT:
* xmin,xmax,ymin,ymax: with same scale in X and Y
************************************************************/
int JLP_GDev::SetNewBoxLimitsForPlan(double *box_xmin, double *box_xmax,
                                      double *box_ymin, double *box_ymax)
{
double xl, yl, xs, ys;
double box_width, box_height, axlen, aylen;

axlen = Jgc0.axlen;
aylen = Jgc0.aylen;

box_width = (double)(Jgc0.dev_width * axlen) / (double)(axlen + Jgc0.offx);
box_height = (double)(Jgc0.dev_height * aylen) / (double)(aylen + Jgc0.offy);

/* We compute the maximum scale: */
    xl = (*box_xmax - *box_xmin) / box_width;
    if(xl < 0) xl = -xl;
    yl = (*box_ymax - *box_ymin) / box_height;
    if(yl < 0) yl = -yl;

/* Now change the limits if needed (i.e., increases the smallest): */
   if(xl < yl) {
    xs = (yl - xl)/2. ;
    if (*box_xmax > *box_xmin) {
     *box_xmin -= xs * box_width;
     *box_xmax += xs * box_width;
     } else {
     *box_xmin += xs * box_width;
     *box_xmax -= xs * box_width;
     }
   } else if(yl < xl) {
    ys = (xl - yl)/2. ;
    if (*box_ymax > *box_ymin) {
     *box_ymin -= ys * box_height;
     *box_ymax += ys * box_height;
     } else {
     *box_ymin += ys * box_height;
     *box_ymax -= ys * box_height;
     }
   }

Jgc0.box_plan = 1;

SetBoxLimits(*box_xmin, *box_xmax, *box_ymin, *box_ymax, 0., 1.);

return(0);
}
/************************************************************************
* Get box limits for plotting (for external routines) to this panel:
************************************************************************/
void JLP_GDev::GetBoxLimits(double *box_xmin, double *box_xmax,
                                  double *box_ymin, double *box_ymax,
                                  double *box_zmin, double *box_zmax)
{
 *box_xmin = Jgc0.axis_limits[0];
 *box_xmax = Jgc0.axis_limits[1];
 *box_ymin = Jgc0.axis_limits[2];
 *box_ymax = Jgc0.axis_limits[3];
 *box_zmin = Jgc0.axis_limits[4];
 *box_zmax = Jgc0.axis_limits[5];
}
/************************************************************************
* Set box limits for plotting (for external routines) to this panel:
************************************************************************/
int JLP_GDev::SetBoxLimits(const double box_xmin, const double box_xmax,
                                 const double box_ymin, const double box_ymax,
                                 const double box_zmin, const double box_zmax)
{
 if(box_xmax == box_xmin || box_ymax == box_ymin) return(-1);

 Jgc0.axis_limits[0] = box_xmin;
 Jgc0.axis_limits[1] = box_xmax;
 Jgc0.axis_limits[2] = box_ymin;
 Jgc0.axis_limits[3] = box_ymax;
 Jgc0.axis_limits[4] = box_zmin;
 Jgc0.axis_limits[5] = box_zmax;

// Update MGO parameters:
 FromJgc0ToMgc0();

return(0);
}
/**************************************************************************
* Set new box setup (mgo coordinates)
**************************************************************************/
int JLP_GDev::SetNewBoxSetup(const int offx0, const int offy0, 
                              const int axlen0, const int aylen0)
{

 Jgc0.offx = offx0;
 Jgc0.offy = offy0;
 Jgc0.axlen = axlen0;
 Jgc0.aylen = aylen0;

// Update MGO parameters:
 FromJgc0ToMgc0();

return(0);
}
/***************************************************************
*
*  image_f1[nx1,ny1] is the initial image
*  image_i[nx2,ny2] is the reduced or initial image converted to LUT numbers
*  (the image is reduced a factor of gamma1 if nx1 or ny1 > max_size)
* INPUT:
* max_size: maximum size of arrays which are not reduced for display
*           e.g., max_size = 512
*
****************************************************************/
 int JLP_GDev::ComputeGamma(const int nx1, const int ny1, const int max_size,
                               int *gamma1, int *gamma_d)
{
 int nxy1_max, nxy2_max;

/* nxy1_max is the largest size in X or Y : */
 nxy1_max = MAXI(nx1, ny1);

/* Compression factor: (via a subsampling of array image_f1) */
 *gamma1 = MAXI(1, nxy1_max/max_size);

// Corresponding size (nx2, ny2) of integer "LUT" array:
 nxy2_max = nxy1_max/ (*gamma1);

/* Dilatation factor: (via direct enlargement with X11) */
 *gamma_d = MAXI(1, max_size/nxy2_max);

return(0);
}
