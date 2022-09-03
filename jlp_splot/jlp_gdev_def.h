/**********************************************************************
* NAME: jlp_gdev_def.h
*
* Derived from "mongo.h" and "jlp_X11.h"
*
* GCOLOR, JLP_GC and MGO_GC 
*
* JLP
* VERSION: 02/06/2017 
**************************************************************************/
#include <stdio.h>   // FILE definition (needed in JLP_GC structure)

#ifndef jlp_gdev_h_ /* Sentry */
#define jlp_gdev_h_ 

/* JLP98: 16 bits/pixel */
/* JLP 2000: new video board: 24 bits/pixel */
/* FOR TOULOUSE and Cambridge: */
#define BIT_PER_VIDEO_PIXEL 24
/* algol in Merate: 256 colors only 
#define BIT_PER_VIDEO_PIXEL 8
*/
/* PC/Debian-Linux  in Merate: 16 bits/pixel 
*/
// #define BIT_PER_VIDEO_PIXEL 16 

/* For X11: NCOLORS= (int)pow(2.,(double)BIT_PER_VIDEO_PIXEL) */
/* NO LONGER DEFINED HERE (JLP2013)
#if(BIT_PER_VIDEO_PIXEL == 8)
#define MAX_COLORS_LUT 256 
#else
#define MAX_COLORS_LUT 65535
#endif
*/

#define BITMAP_WIDTH 800
#define BITMAP_HEIGHT 600

/* For postscript output: */
#define PSA4_RESOLUTION 72.0
#define PSA4_WIDTH_CM   21.0
#define PSA4_HEIGHT_CM  29.7
#define PSA4_WIDTH_IN   8.2677165
#define PSA4_HEIGHT_IN  11.692913
#define PSA4_WIDTH_PT   595.27559
#define PSA4_HEIGHT_PT  841.88976

/* Plot window size:  */
#define SCREEN_SIZE 32767

/* TTY descriptor structure (previously in "tty.h")
 * Full termcap entry is the 'caplist' string.
 * The caplist is indexed at open time to permit a binary search for
 * capabilities at run time.
 */
#define MAX_CAPS 100                    /* maximum capabilities */

typedef struct {
int t_baud,                     /* baud rate for delays */
    t_nlines,                   /* nlines on terminal at open */
    t_ncols,                    /* ncols on terminal at open */
    t_ncaps,                    /* number of capabilities */
    t_caplen,                   /* length of caplist, chars */
    t_len,                      /* size of t_caplist */
    t_op,                       /* index of end of caplist */
    t_capcode[MAX_CAPS],        /* cap code array */
    t_capindex[MAX_CAPS];       /* cap index array */
char t_padchar,                         /* pad character for delays */
     *t_caplist;                        /* termcap entry */
} TTY;

#define GCOLOR_MAX 256

// GCOLOR contain the (r,g,b) values of the color cells 
// whose addresses are stored in Jgc0.gclr.lut array.
// (only used for devices with pre-allocated color cells)
typedef struct {
int r[GCOLOR_MAX];
int g[GCOLOR_MAX];
int b[GCOLOR_MAX];
int lut[GCOLOR_MAX];
int ncolors;
} GCOLOR;

/* COLOR structure (previously in "mongo.h") */
typedef struct {
   unsigned char red, green, blue;
} COLOR;

/* JLP graphic context: 
* Derived from "mongo.h" and "jlp_X11.h"
*
*    offx = x coordinate of the upper-left corner of the box in pixels
*    offy = y coordinate of the upper-left corner of the box in pixels
*    axlen = width of the box in pixels
*    aylen = height of the box in pixels
*    xmin_user = lower user value in X
*    xmax_user = upper user value in X
*    ymin_user = lower user value in Y
*    ymax_user = upper user value in Y
*
**************************************************************************/
typedef struct {
int      offx, offy, axlen, aylen; // location of xy axes in mgo units 
double   expand;          // Expansion of characters

double   cheight, cwidth; // cheight,cwidth: canonical height and width of characters

int xp,yp;                // Present plot position (in mgo coordinates)

GCOLOR   gclr;            // GCOLOR structure 
// pen_color: White, Black, Red, Green, Blue, etc
char pen_colour[64], pen_default_colour[64], backgd_colour[64];

/* Device type:
  1 = X11
  2 = Postscript file
  3 = wxWidgets
No longer used:
  4 = HPGL
  5 = Tektronix
*/
int dev_type;                  
int dev_width, dev_height;  // Size of plot (device coordinates) 
int dev_yorigin_is_on_top;  // 1 if device Y origin is on top, 0 otherwise
int dev_idv;                // device number
/* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gegraf_3d_plot
* 7 = gsegraf_polar_curve
*/
int gdev_graphic_type;     // 1=jlp_splot_curve, 2=jlp_splot_image 4=gsegraf... 

// Box parameters:
double   axis_limits[6];   // axis_limits[6]: 0=box_xmin, 1=box_xmax, 
                           // 2=box_ymin, 3=box_ymax, 4=box_zmin, 5=box_zmax
int box_type;              // 1=JLP axes 2=Smongo axes 
int box_plan;              // 1 if same scale in X and Y  
int box_ticks_in;          // 1 if ticks_in, 0 otherwise 
int box_xgrid, box_ygrid;  // 1 if grid, 0 otherwise
char box_xlabel[128], box_ylabel[128], box_zlabel[128], box_title[128];
int xaxis_type, yaxis_type, zaxis_type; // Axis type: 0=linear 1=log10 

/* Data parameters: */
double   xmin_user, xmax_user, ymin_user, ymax_user, zmin_user, zmax_user; 

double *image_f;           // current image array
int nx, ny, nz;            // size of image_f
int TeX_flag;              // Flag set to unity if TeX labels are wanted: 

/* Main output file (for postscript) */
FILE *fdv;
char fdv_pst_fname[256];

/* Backup file: */
FILE *fp_backup;
char backup_fname[256];
} JLP_GC;

/* Parameters from "mongo.h" (version of 1989): 
*/
typedef struct {
double fx1,fx2,fy1,fy2;    /* User coords of graphics area    */
double fsx,fsy;            /* Scale and offset to conversion  */
int   gx1,gx2,gy1,gy2;     /* MGO coords of graphics area     */
int   lwidth;              /* Line width: 0,1,2,3...etc       */
int   lltype;              /* Line type: dotted, etc.         */
double eexpand;            /* Global expansion of chars, pts  */
double aangle,ffsin,ffcos; /* Rotation of chars and points    */
int   termout;             /* True for term, false for print  */
int   ldef;		   /* scale spacing for lwidth        */
int   pdef;		   /* scale size for points           */
} MGO_GC;

// For jlp_box_sciplot  (in "jlp_gdev_box_sciplot.cpp")
typedef struct {
  double Origin;
  double Size;
  double AxisPos;
  double DrawOrigin;
  double DrawSize;
  double DrawMax;
  double MajorInc;
  int MajorNum;
  int MinorNum;
  int Precision;
  int Axis_is_log10;
} SciPlotAxis;

#endif /* EOF sentry */
