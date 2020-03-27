/*************************************************************************
* "jlp_gdev_pst.cpp" (curves/images)
* Definition of the routines of the JLP_GDev_PST class
* that are not defined in "jlp_gdev_pst.h"
* Warning: some routines are also defined in "jlp_gdev_pstcopy.cpp"
*
*  JLP
*  Version 03-01-2015
****************************************************************/
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "jlp_gdev_pst.h"
#include "jlp_time0.h"          // JLP_CTIME()

/*
#define DEBUG
*/

#define MAX_POLYGON 20
// Postscript units: inch/72
#define PAGE_WIDTH 72.*167./25.4
#define PAGE_HEIGHT 72.*216./25.4
#define FONT_SIZE_FOR_12PT   12.
/* 16 gray levels: */
#define BITS_PER_PIXEL  4

/*************************************************************
* For images
* Setup window parameters according to the device name
*
* INPUT:
* plotdev
* title
* nx1, ny1: size of input image (not used here)
*
* OUTPUT:
* offx, offy, axlen, aylen
* width_mm, height_mm : size of plot in mm 
* TeX_flag
* dev_type1 = 1 if X11, 2 if postscript, ...
************************************************************************/
int JLP_GDev_PST::setup_device_for_image(const char *plotdev, 
                                    const char* title,
                                    const int nx1, const int ny1, int *offx,
                                    int *offy, int *axlen, int *aylen,
                                    int *width_mm, int *height_mm, 
                                    int *TeX_flag, int *dev_type, int *landscape)
{
int status = 0;

  *offx = 4000; *offy = 4000; *axlen = 26000; *aylen = 26000;
  *width_mm = 210; *height_mm = 297; 

// Landscape option set to zero by default:
  *landscape = 0;

// Device type (for postscript dev_type=2)
  *dev_type = 2;

switch(plotdev[0])
    {
/* Laser Postscript: portrait ("postscript") */
    case 'P':
    case 'p':
/* 180 * 210 mm */
      *width_mm = 180;
      *height_mm = 210;
      break;
/* Laser Postscript: landscape */
    case 'L':
    case 'l':
/* 260 * 180 mm  for offx = 4000 axlen = 26000 offy = 4000 aylen = 26000*/
      *width_mm = 260;
      *height_mm = 180;
/* Rotation of 90. degrees: */
      *landscape = 1;
      break;
/* Laser Postscript: fullpage (landscape) */
    case 'F':
    case 'f':
/* 270 * 180 mm */
      *width_mm = 270;
      *height_mm = 180;
      *landscape = 1;
      break;
/* Laser Postscript: squared */
    case 'S':
    case 's':
/* 180 * 180 mm */
      *width_mm = 180;
      *height_mm = 180;
      break;
    default:
      printf("setup_device_for_image/Error: unknown postscript graphic device\n (%s is unknown)\n",
             plotdev);
      status = -1;
      break;
    }

// TeX flag
  *TeX_flag = 0;

// pst device: POSTSCRIPT, postscript, SQUARE, square, LANDSCAPE, landscape
/* A trick to select TeX interactively: if upper case, TeX selected */
 if(plotdev[0] == 'P' || plotdev[0] == 'S' || plotdev[0] == 'L'
    || plotdev[0] == 'F') *TeX_flag = 1;

// Maximum level for LUT values 256*256 - 1 = 65535:
  MaxColorLevelForLUT = 65535;

return(status);
}
/*************************************************************
* For curves  (NB: virtual definition: do not change the parameters !)
* Setup window parameters according to the device name
*
* INPUT:
* plotdev
* title
*
* OUTPUT:
* dev_width, dev_height : size of window
* TeX_flag
* dev_type1 = 1 if X11, 2 if postscript, ...
************************************************************************/
int JLP_GDev_PST::setup_device_for_curve(const char *plotdev,
                                   const char* title,
                                   int *width_mm, int *height_mm,
                                   int *TeX_flag, int *dev_type, int *landscape)
{
int status = 0;

  *width_mm = 210; *height_mm = 297;

// TeX flag
  *TeX_flag = 0;

// Landscape option set to zero by default:
  *landscape = 0;

// Device type (for postscript dev_type=2)
  *dev_type = 2;

switch(plotdev[0])
    {
/* Laser Postscript: portrait ("postscript") */
    case 'P':
    case 'p':
/* 180 * 210 mm */
      *width_mm = 180;
      *height_mm = 210;
      break;
/* Laser Postscript: landscape */
    case 'L':
    case 'l':
/* 260 * 180 mm  for offx = 4000 axlen = 26000 offy = 4000 aylen = 26000*/
      *width_mm = 260;
      *height_mm = 180;
/* Rotation of 90. degrees: */
      *landscape = 1;
      break;
/* Laser Postscript: fullpage not rotated */
    case 'F':
    case 'f':
/* 260 * 180 mm */
      *width_mm = 260;
      *height_mm = 180;
/* No rotation: */
      *landscape = 0;
      break;
/* Laser Postscript: squared */
    case 'S':
    case 's':
/* 180 * 180 mm */
      *width_mm = 180;
      *height_mm = 180;
      break;
    default:
      printf("setup_device_for_image/Error: unknown postscript graphic device\n (%s is unknown)\n",
             plotdev);
      status = -1;
      break;
    }

// TeX flag
  *TeX_flag = 0;

// pst device: POSTSCRIPT, postscript, SQUARE, square, LANDSCAPE, landscape
/* A trick to select TeX interactively: if upper case, TeX selected */
 if(plotdev[0] == 'P' || plotdev[0] == 'S' || plotdev[0] == 'L'
    || plotdev[0] == 'F') *TeX_flag = 1;

// Maximum level for LUT values :
  MaxColorLevelForLUT = 255;

return(status);
}

/*************************************************************
* Open the graphic postscript file
*
* INPUT:
* title: not used for postscript 
* width_mm, height_mm: size of plot in mm 
* landscape: flag set to one if plot is to be rotated by 90 degrees
* is_curve: flag set to one if curve or to zero if displaying an image
* Jgc0 structure
*
* OUTPUT:
* Jgc0.dev_width, Jgc0.dev_height: window size in device coordinates
************************************************************/
int JLP_GDev_PST::open_device(const char* title, int width_mm, int height_mm,
                              int landscape, int *jgc_dev_width,
                              int *jgc_dev_height, int *dev_yorigin_is_on_top)
{
char  date[60];
int   err;

// Image devices are generally starting at the upper-left corner,
// but here this is not the case:
  Jgc0.dev_yorigin_is_on_top = 0;

// Postscript units: inch/72
Jgc0.dev_width = (int)((72. * (double)width_mm * (double)SCREEN_SIZE) 
                  / (25.4 * (double)(Jgc0.offx + Jgc0.axlen))); 
Jgc0.dev_height = (int)((72. * (double)height_mm * (double)SCREEN_SIZE) 
                  / (25.4 * (double)(Jgc0.offy + Jgc0.aylen))); 

printf("JLP_GDev_PST::open/width=%d height=%d (mm) => %d %d (pts)\n", 
        width_mm, height_mm, Jgc0.dev_width, Jgc0.dev_height);
#ifdef DEBUG
printf("JLP_GDev_PST::open/width=%d height=%d (mm) => %d %d (pts)\n", 
        width_mm, height_mm, Jgc0.dev_width, Jgc0.dev_height);
#endif

// Opening output postscript file:
if((Jgc0.fdv = fopen(Jgc0.fdv_pst_fname,"w")) == NULL) {
 printf(" JLP_GDev_PST::open/fatal error opening %s\n",Jgc0.fdv_pst_fname); 
 exit(-1);
 }

fprintf(Jgc0.fdv,"%%!PS-Adobe\n");

/* Comments to be compatible with encapsulated postscript: */
/*
fprintf(Jgc0.fdv,"%%%%Title: %s\n",title);
*/
fprintf(Jgc0.fdv,"%%%%Creator: JLP-gdev-pst; Version 15/03/2017\n");
JLP_CTIME(date,&err);
if(err == 0) fprintf(Jgc0.fdv,"%%%%CreationDate: %s\n",date);

if (landscape) 
   fprintf(Jgc0.fdv,"%%%%Orientation: Landscape\n");
else
   fprintf(Jgc0.fdv,"%%%%Orientation: Portrait\n");

fprintf(Jgc0.fdv,"%%%%Pages: 1\n");
fprintf(Jgc0.fdv,"%%%%DocumentFonts: Times-Roman\n");

/****************** Set Bounding box for curve: *********/
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
 if((Jgc0.gdev_graphic_type == 2) || (Jgc0.gdev_graphic_type == 3) 
    || (Jgc0.gdev_graphic_type == 5)) {
/****************** Set Bounding box for image: *********/  
/*
2008: I simply use the values that are correct for POSTSCRIPT hardcopy plots
*/
  fprintf(Jgc0.fdv,"%%%%BoundingBox: %d %d %d %d\n",
            60, 230, 600, 690);
 } else { 
   if (landscape)
     fprintf(Jgc0.fdv,"%%%%BoundingBox: %d %d %d %d\n",
             0, 0, (int)Jgc0.dev_width, (int)Jgc0.dev_height);
   else
/* Offset of 30,30 (JLP 2010):
*/
    fprintf(Jgc0.fdv,"%%%%BoundingBox: %d %d %d %d\n",
            30, 30, (int)(30 + Jgc0.dev_width), (int)(30 + Jgc0.dev_height));
 }

fprintf(Jgc0.fdv,"%%%%EndComments\n");

/* Macros: */
fprintf(Jgc0.fdv,"/m {moveto} def \n");
fprintf(Jgc0.fdv,"/l {lineto} def \n");
fprintf(Jgc0.fdv,"/rm {rmoveto} def \n");
fprintf(Jgc0.fdv,"/lsm {lineto currentpoint stroke moveto} def \n");
fprintf(Jgc0.fdv,"/rl {rlineto} def \n");
fprintf(Jgc0.fdv,"/sf {exch findfont exch scalefont setfont} def \n");
fprintf(Jgc0.fdv,"/sh {show} def \n");

/* Other macros: (not used here) 
fprintf(Jgc0.fdv,"/ln {setdash setlinewidth} def \n");
fprintf(Jgc0.fdv,"/ba [15 3 6 3] def \n");
fprintf(Jgc0.fdv,"/ld [12] def \n");
fprintf(Jgc0.fdv,"/sd [3] def \n");
fprintf(Jgc0.fdv,"/rt {rotate} def \n");

fprintf(Jgc0.fdv,"/box {0 50 rl -50 0 rl 0 -100 rl 100 0 rl 0 100 rl \n");
fprintf(Jgc0.fdv," -50 0 rl} def \n");
fprintf(Jgc0.fdv,"/oct {0 50 rl -20 0 rl -30 -30 rl 0 -40 rl 30 -30 rl \n");
fprintf(Jgc0.fdv," 40 0 rl 30 30 rl 0 40 rl -30 30 rl -20 0 rl} def \n");
fprintf(Jgc0.fdv,"/tri {0 50 rl -50 -100 rl 100 0 rl -50 100 rl} def \n");
fprintf(Jgc0.fdv,"/plu {0 50 rm 0 -100 rl -50 50 rm 100 0 rl} def \n");
fprintf(Jgc0.fdv,"/cro {-50 -50 rm 100 100 rl -100 0 rm 100 -100 rl} def \n");
fprintf(Jgc0.fdv,"/dia {0 50 rl -50 -50 rl 50 -50 rl 50 50 rl -50 50 rl} def \n");
fprintf(Jgc0.fdv,"/hat {0 -50 rm 0 100 rl 50 -50 rl -100 0 rl 50 50 rl} def \n");
fprintf(Jgc0.fdv,"/tre {-50 -50 rm 100 100 rl -100 0 rl 100 -100 rl} def \n");
fprintf(Jgc0.fdv,"/zed {-50 50 rm 100 0 rl -100 -100 rl 100 0 rl} def \n");
fprintf(Jgc0.fdv,"/why {50 50 rl -50 -100 rm 0 50 rl -50 50 rl} def \n");
fprintf(Jgc0.fdv,"/spi {50 50 rl -20 -20 rm 0 -60 rl 20 -20 rl -20 20 rm \n");
fprintf(Jgc0.fdv,"-60 0 rl -20 -20 rl 20 20 rm \n");
fprintf(Jgc0.fdv," 0 60 rl -20 20 rl 20 -20 rm 60 0 rl} def \n");
fprintf(Jgc0.fdv,"/sta {0 50 rm 0 -100 rl -50 0 rm 100 100 rl 0 -100 rm \n");
fprintf(Jgc0.fdv," -100 100 rl} def \n");
fprintf(Jgc0.fdv,"/hou {-50 50 rm 100 0 rl -100 -100 rl 100 0 rl -100 100 rl} def \n");
fprintf(Jgc0.fdv,"/str {0 50 rm 0 -100 rl} def \n");
fprintf(Jgc0.fdv,"/cir {currentpoint 50 90 450 arc} def \n");
fprintf(Jgc0.fdv,"/cif {cir fill} def /bof {box fill} def /ocf {oct fill} def \n");
fprintf(Jgc0.fdv,"/trf {tri fill} def /dif {dia fill} def \n");
fprintf(Jgc0.fdv,"/items [{box}{oct}{tri}{plu}{cro}{dia}{hat}{tre}{zed}{why} \n");
fprintf(Jgc0.fdv,"{spi}{sta}{hou}{str}{cir}{cif}{bof}{ocf}{trf}{dif}] def \n");
fprintf(Jgc0.fdv,"/cs {gsave dup scale currentpoint newpath moveto items  \n");
fprintf(Jgc0.fdv,"exch get exec stroke grestore} def \n");

*/

/* Put the graph in a good position in the page: */
if (landscape) 
  fprintf(Jgc0.fdv,"90 rotate 30 -560 translate \n");
else
  fprintf(Jgc0.fdv,"40 40 translate \n");

/* End of JLP_GDev_PST::open: */
*dev_yorigin_is_on_top = Jgc0.dev_yorigin_is_on_top;
*jgc_dev_width = Jgc0.dev_width;
*jgc_dev_height = Jgc0.dev_height;

return(0);
}

/****************************************************************
* Draw a line to the output device (here postscript)
*
* INPUT:
* x1, y1: coordinates of starting point (mgo coordinates)
* x2, y2: coordinates of ending point (mgo coordinates)
* lwidth: line width
*
* OUTPUT:
* Mgc0.lwidth updated
****************************************************************/
int JLP_GDev_PST::line_device(int x1, int y1, int x2, int y2, int lwidth)
{
double dev_x, dev_y;

// When lwidth has to be changed, draw whole set with old settings
// and set new width 
    if(lwidth != Mgc0.lwidth) {
       fprintf(Jgc0.fdv,"stroke\n %d setlinewidth",lwidth);
       Mgc0.lwidth = lwidth;
       }

// Conversion from mgo to device coordinates:
    ConvMgoToDev(x1, y1, &dev_x, &dev_y);

//  Move pen up:
    fprintf(Jgc0.fdv,"%.2f %.2f m\n", dev_x, dev_y);

//  Move pen down and stroke:
    ConvMgoToDev(x2, y2, &dev_x, &dev_y);
    fprintf(Jgc0.fdv,"%.2f %.2f lsm\n", dev_x, dev_y);

// Update current position (mgo coordinates):
   Jgc0.xp = x2;
   Jgc0.xp = y2;

return(0);
}
/****************************************************************
* To draw polygons on the output device (here postcript file)
*
* JLP97: modif to get squares even if the
* output format is a rectangle...
* aspect = dev_height/dev_width 
*  size0_y = size0_x / aspect;
* mgo_x, mgo_y  : position in mgo coordinates 
* nsides  : number of sides
****************************************************************/
int JLP_GDev_PST::polygon_device(int mgo_x, int mgo_y, double expand, 
                               double angle, int nsides, int filled)
{
double dtheta, theta;
double g_dx, g_dy, aspect;
register int i;
double radius;
double xpsize, ypsize;        /* scale for points == g_dx*pdef*expand */
static int num = -1;          /* number of vertices used last time */
static double dev_x0, dev_y0;   /* constant part of xlist, ylist */
	    
 static double old_angle, old_xpsize, old_ypsize;
/* JLP2007: need double variables, since integers are not accurate enough
* for small symbols! */
 static double dev_xlist[MAX_POLYGON + 1],
             dev_ylist[MAX_POLYGON + 1];  /* vertices describing point */

 if(nsides < 2) {
    line_device(mgo_x, mgo_y, mgo_x, mgo_y + 1);
    return(0);
  }

   g_dx = (double)Jgc0.dev_width / (double)SCREEN_SIZE;
   g_dy = (double)Jgc0.dev_height / (double)SCREEN_SIZE;
// aspect ratio of screen, y/x
   aspect = (double)Jgc0.dev_height / (double)Jgc0.dev_width;     

   dtheta = 2. * PI / (double)nsides;
/* For instance if nsides=4, dtheta = pi/2 */

/* Compute length of the polygon sides: */
   radius = Mgc0.pdef * expand;
   xpsize = 2. * radius * sin(dtheta/2.) * g_dx;
/* For instance if nsides=4, xpsize = 2*radius*sin(pi/4) */
/* JLP97: get squares and circles, even with "landscape" and "postscript"
*   division with aspect */
   ypsize = 2. * radius * sin(dtheta/2.) * g_dy / aspect;

/* Computes new polygon only if different from previous call */
   if( num != nsides || angle != old_angle || xpsize != old_xpsize
       || ypsize != old_ypsize)
   {
     if(nsides > MAX_POLYGON) num = MAX_POLYGON;
     else num = nsides;

/* Starting position of the polygon: 
* (add 3 PI / 2 to have horizontal bases when angle=0.) */
     theta = 3. * PI / 2. - dtheta / 2. + angle * PI / 180;
     dev_xlist[0] = radius * cos(theta) * g_dx;
     dev_ylist[0] = radius * sin(theta) * g_dy /  aspect;
/* Relative motions to draw the polygon: (start with horizontal base) */
     theta = 0.;
     for(i = 1; i < num ;i++) {
	dev_xlist[i] = xpsize * cos(theta);
        dev_ylist[i] = ypsize * sin(theta);
        theta += dtheta;
     }
   }

/* Now draws: */
   ConvMgoToDev(mgo_x, mgo_y, &dev_x0, &dev_y0);
   fprintf(Jgc0.fdv,"newpath \n");
   fprintf(Jgc0.fdv,"%.2f %.2f m\n", dev_x0, dev_y0);
   fprintf(Jgc0.fdv,"%.2f %.2f rm\n", dev_xlist[0], dev_ylist[0]);
   for(i = 1; i < num; i++) {
     fprintf(Jgc0.fdv,"%.2f %.2f rl\n", dev_xlist[i], dev_ylist[i]);
     }
   fprintf(Jgc0.fdv,"closepath \n");
if(filled)
   fprintf(Jgc0.fdv,"fill \n");
else
   fprintf(Jgc0.fdv,"stroke \n");

return(0);
}
/****************************************************************
* To draw a circle to the output device (here to the postscript file
*
* Get circles even if the output format is a rectangle...
* mgo_x, mgo_y  : position in mgo coordinates 
* idiam : diameter in mgo coordinates 
****************************************************************/
int JLP_GDev_PST::circle_device(int mgo_x, int mgo_y, int idiam, int filled)
{
double radius, g_dx, g_dy;
double dev_x0, dev_y0;

 g_dx = (double)Jgc0.dev_width / (double)SCREEN_SIZE;
 g_dy = (double)Jgc0.dev_height / (double)SCREEN_SIZE;
	    
/* Compute radius in postscript units: */
   radius = ((g_dx + g_dy) / 2.)* (double)idiam / 2.;

/* Now draws: */
   ConvMgoToDev(mgo_x, mgo_y, &dev_x0, &dev_y0);
   fprintf(Jgc0.fdv,"newpath \n");
   fprintf(Jgc0.fdv,"%.2f %.2f %.2f 0. 360. arc\n", dev_x0, dev_y0, radius);
   fprintf(Jgc0.fdv,"closepath \n");
if(filled)
   fprintf(Jgc0.fdv,"fill \n");
else
   fprintf(Jgc0.fdv,"stroke \n");

return(0);
}
/****************************************************************
* Plot an image on the postscript file 
*
* INPUT:
* gamma1, gamma_d: (compression/enlargement) : not used yet 
****************************************************************/
int JLP_GDev_PST::plot_image(int *image, int nx, int ny, int idim, int xstart, 
                        int ystart, int gamma1, int gamma_d, 
                        int black_and_white) 
{
int status = 0;

/* B&W: */
    if(black_and_white)
       status = plot_image256(image,nx,ny,idim,Jgc0.gclr.r,Jgc0.gclr.ncolors,
                          xstart,ystart);
    else {
/* Color: */
      status = plot_imageRGB(image,nx,ny,idim,Jgc0.gclr.r,Jgc0.gclr.g,
                        Jgc0.gclr.b,Jgc0.gclr.ncolors,xstart,ystart);
/*
* Copy to gif image also:  (for interfacing with tcl/tk)
      status = gif_imageRGB(image,nx,ny,idim,Jgc0.gclr.r,Jgc0.gclr.g,
                            Jgc0.gclr.b,Jgc0.gclr.ncolors,"tmp.gif");
*/
    }
return(status);
}

/****************************************************************
* plot_image16
* 16 gray levels
* LUT contained in r[ncolors]
****************************************************************/
int JLP_GDev_PST::plot_image16(int *image, int nx, int ny, int idim, 
                         int *r, int ncolors, int xstart, int ystart)
{
double width_im, height_im, x, y;
int digits_per_sample = 16/BITS_PER_PIXEL;
int status, size, line_length, in_frame;

/* Counting the number of calls: */
  ncalls_image++;

/* Save old settings so that the following changes are limited
  only to that procedure: */
  fprintf(Jgc0.fdv,"gsave \n");

/*  Move pen up to the required starting position */
   ConvMgoToDev(xstart, ystart, &x, &y);
   fprintf(Jgc0.fdv,"%.2f %.2f translate \n",x,y);

/* Macro for line format: */
  line_length = (nx * digits_per_sample) / 8;        /* length in bytes */
  fprintf(Jgc0.fdv,"/myline%1d %d string def\n",ncalls_image,line_length);

/* Defines the format (in postscript units: inch/72) of the image: */
  ConvUserToDev((double)nx, (double)ny, &width_im, &height_im, &in_frame);
  fprintf(Jgc0.fdv,"%.2f %.2f scale \n", width_im, height_im);

/* Macro for image display: */
  fprintf(Jgc0.fdv,"/dispimage%1d {%d %d %d [%d 0 0 %d 0 0] \n",
          (int)ncalls_image, (int)nx, (int)ny, (int)digits_per_sample,
          (int)nx, (int)ny);
  fprintf(Jgc0.fdv,
    "{currentfile myline%1d readhexstring pop} image} def\n",
    ncalls_image);

/* Actual command: */
  fprintf(Jgc0.fdv,"dispimage%1d \n",ncalls_image);

/*Encoding in Hexadecimal code: */
  if(nx != idim) 
   {
   printf(" Fatal error: image size incompatible with curent settings\n");
   printf(" (Fatal error in size plot_image16, idim=%d != nx=%d)\n", idim, nx);
   return(-1);
   }
  size = nx * ny;
  status = this->pst_code16(image,size,r,ncolors);

/* Return to old settings: */
  fprintf(Jgc0.fdv,"grestore \n");

return(0);
}

/****************************************************************
* plot_image256
* 256 gray levels
* LUT contained in r[ncolors]
****************************************************************/
int JLP_GDev_PST::plot_image256(int *image, int nx, int ny, int idim, 
                                int *r, int ncolors, int xstart, int ystart)
{
double width_im, height_im, dev_x, dev_y;
int in_frame;

  int digits_per_sample = 8;
  int status, size, line_length;

/* Counting the number of calls: */
  ncalls_image++;

/* Save old settings so that the following changes are limited
  only to that procedure: */
  fprintf(Jgc0.fdv,"gsave \n");

/*  Move pen up to the bottom-left starting position */
   ConvMgoToDev(xstart, ystart, &dev_x, &dev_y);
   fprintf(Jgc0.fdv,"%.2f %.2f translate \n", dev_x, dev_y);

/* Macro for line format: */
  line_length = (nx * digits_per_sample) / 8;        /* length in bytes */
  fprintf(Jgc0.fdv,"/myline%1d %d string def\n",ncalls_image,line_length);

/* Defines the format (in postscript units: inch/72) of the image: */
  ConvUserToDev((double)nx, (double)ny, &width_im, &height_im, &in_frame);
  fprintf(Jgc0.fdv,"%.2f %.2f scale \n", width_im, height_im);

/* Macro for image display: */
  fprintf(Jgc0.fdv,"/dispimage%1d {%d %d %d [%d 0 0 %d 0 0] \n",
          (int)ncalls_image,(int)nx,(int)ny,(int)digits_per_sample,
          (int)nx,(int)ny);
  fprintf(Jgc0.fdv,
    "{currentfile myline%1d readhexstring pop} image} def\n",
    ncalls_image);

/* Actual command: */
  fprintf(Jgc0.fdv,"dispimage%1d \n",ncalls_image);

/*Encoding in Hexadecimal code: */
  if(nx != idim) 
   {
   printf(" Fatal error: image size incompatible with curent settings\n");
   printf(" (Fatal error in size plot_image256, idim=%d != nx=%d)\n",
             idim, nx);
   return(-1);
   }
  size = nx * ny;
  status = this->pst_code256(image,size,r,ncolors);

/* Return to old settings: */
  fprintf(Jgc0.fdv,"grestore \n");

return(0);
}
/*************************************************************
* Encoding in Hexadecimal code: 
* Version with 2 hexadigits per sample, i.e., 256 gray levels
**************************************************************/
int JLP_GDev_PST::pst_code256(int *image, int size, int *r, int ncolors)
{
register int i, k;
int icode, ival;

  k = 0;
  for(i=0; i<size; i++)
  {
    icode = MAXI(MINI(image[i],ncolors-1),0);
    ival = (int)((double)r[icode] * 255. / (double)MaxColorLevelForLUT);
    ival = MAXI(MINI(ival,255),0);
    fprintf(Jgc0.fdv,"%.2X",ival);
/* Write in lines of 80 characters: */
    k++; if(k == 39){fprintf(Jgc0.fdv,"\n"); k = 0;}
  }

 fprintf(Jgc0.fdv,"\n");
 return(0);
}
/****************************************************************
* Encoding in Hexadecimal code: 
* Version with 1 hexadigit per sample
* "0123456789ABCDEF"
*****************************************************************/
int JLP_GDev_PST::pst_code16(int *image, int size, int *r, int ncolors)
{
int icode, ival;
register int i, k;

  k = 0;
  for(i=0; i<size; i++)
  {
    icode = MAXI(MINI(image[i],ncolors-1),0);
    ival = (int)((double)r[icode] * 15. / (double)MaxColorLevelForLUT);
    ival = MAXI(MINI(ival,15),0);
    fprintf(Jgc0.fdv,"%X",ival);
/* Write in lines of 80 characters: */
    k++; if(k == 79){fprintf(Jgc0.fdv,"\n"); k = 0;}
  }

 fprintf(Jgc0.fdv,"\n");
 return(0);
}
/****************************************************************
* Set Jgc0.cwidth and Jgc0.cheight corresponding to expand0
****************************************************************/
int JLP_GDev_PST::Jgc0_set_font_size_from_device(double expand0)
{
int status = -1;
double g_dx, g_dy;

g_dx = (double)Jgc0.dev_width / (double)SCREEN_SIZE;
g_dy = (double)Jgc0.dev_height / (double)SCREEN_SIZE;

/* Size of characters: (for "mongo.h" ) */
// Default is expand0 = 1.2, FONT_SIZE_FOR_12PT= 12
// Checked in June 2017
Jgc0.cwidth = 1.6 * expand0 * FONT_SIZE_FOR_12PT  / g_dx; 
Jgc0.cheight = 0.2 * expand0 * FONT_SIZE_FOR_12PT / g_dy; 
if(Jgc0.TeX_flag == 1) Jgc0.cheight *= 1.5;

return(status);
}
/****************************************************************
* Draw a label to the output device (her to the postscript file)
*
* INPUT:
*  xstart, ystart : mgo coordinates
*  expand1 : to increase the size of fonts
****************************************************************/
double JLP_GDev_PST::label_device(const char *s, int xstart, int ystart, 
                                double angle1, double expand1, int drawit)
{
double dev_x, dev_y;
double expanded_font, length;

// Set Jgc0 font size from device:
Jgc0_set_font_size_from_device(expand1);

/* Font length in physical coordinates (in pts, i.e. inch/72) */
expanded_font = 2. * FONT_SIZE_FOR_12PT * expand1; 

/* Compute length (in mgo coordinates) and return if not display: */
length = Jgc0.cwidth * (double)(strlen(s));

/* Return if drawit == 0 */
 if(drawit == 0  || length == 0) return(length);

/* Draw the string if drawit == 1 */

/*  Move pen up to the required starting position */
   ConvMgoToDev(xstart, ystart, &dev_x, &dev_y);
   fprintf(Jgc0.fdv,"%.2f %.2f m\n", dev_x, dev_y);

/* Font and scale setup: */
   fprintf(Jgc0.fdv,"/Times-Roman findfont %d scalefont setfont\n",
	   (int)expanded_font);

/* If rotation: */
   if(angle1 != 0.)
      {
/* Save old settings so that the following changes are limited
  only to that procedure: */
      fprintf(Jgc0.fdv,"gsave \n");
      fprintf(Jgc0.fdv,"%.1f rotate\n", angle1);

/* Now draw the string: */
      fprintf(Jgc0.fdv,"( %s ) show\n",s);

/* Return to old settings: */
      fprintf(Jgc0.fdv,"grestore \n");
      }
   else
      {
/* If no rotation, simply draw the string: */
      fprintf(Jgc0.fdv,"( %s ) show\n",s);
      }

return(length);
}
/**********************************************************************
*
* FUNCTION: plot_imageRGB
*
* INPUT:   
* image[idim,*] : double array
* nx, ny : size of image
* r[ncolors], g[ncolors], b[ncolors] : look up table (with values from 0 
*                                      to MaxColorLevelForLUT)
**********************************************************************/
int JLP_GDev_PST::plot_imageRGB(int *image, int nx, int ny, int idim, 
                          int *r, int *g, int *b, int ncolors,
                          int xstart, int ystart)
{
double width_im, height_im, dev_x, dev_y;
int  red, green, blue, in_frame;
register int  i, j, k, icell;

  fprintf(Jgc0.fdv,"gsave\n");

/*  Move pen up to the bottom-left starting position */
   ConvMgoToDev(xstart, ystart, &dev_x, &dev_y);
   fprintf(Jgc0.fdv,"%.2f %.2f translate \n", dev_x, dev_y);

/* Defines the format (in postscript units) of the image: */
  ConvUserToDev((double)nx, (double)ny, &width_im, &height_im, &in_frame);
  fprintf(Jgc0.fdv,"%d %d scale \n", (int)width_im, (int)height_im);

/* From E. Anterrieu */
/* 8 bit encoding */
  fprintf(Jgc0.fdv,"%d %d 8 [%d 0 0 %d 0 0]\n",nx,ny,nx,ny);
  fprintf(Jgc0.fdv,"{currentfile %d string readhexstring pop}\n",3*nx);
  fprintf(Jgc0.fdv,"false 3 colorimage\n");
  for (j=0; j<ny; j++)
    {
    k = 1;
    for (i=0; i<nx; i++)
      {
      icell = image[i + j * idim];
      icell = MINI(icell, ncolors - 1);
      icell = MAXI(icell, 0);
/* Conversion to [0,255] color scale: */
      red = (int)((double)r[icell] * 255. / (double)MaxColorLevelForLUT);
      green = (int)((double)g[icell] * 255. / (double)MaxColorLevelForLUT);
      blue = (int)((double)b[icell] * 255. / (double)MaxColorLevelForLUT);
      fprintf(Jgc0.fdv,"%.2x%.2x%.2x", red, green, blue);
/* Each row of the source image begins on a byte boundary:
* if the nber of data bits per row is not a multiple of 8, the
* end of the row must be padded with extra bits to fill out the last byte! */
      if (k == 8) {fprintf(Jgc0.fdv,"\n");  k = 1;}
      else k++;
      }
    }
  fprintf(Jgc0.fdv,"\n grestore\n");

return(0);
}
/*************************************************************
* ConvUserToDevForImages
* to convert user coordinates to device coordinates
*
* INPUT:
*  user_x1, user_y1: user coordinates
*
* OUTPUT:
*  dev_x0, dev_y0: device coordinates
*  in_frame: 1 if (ix, iy) is visible in the window
*            0 otherwise
*
*************************************************************/
void JLP_GDev_PST::ConvUserToDevForImages(double user_x1,  double user_y1,
                                          double *dev_x0, double *dev_y0,
                                          int *in_frame)
{
int nx1, ny1;

// Initialization of the output parameters:
  *dev_x0 = 0; *dev_y0 = 0; *in_frame = 0;

  nx1 = Jgc0.nx;
  ny1 = Jgc0.ny;

// user_x1, user_y1 are the user coordinates
  if((user_x1 >= 0 && user_x1 <= nx1 - 1)
     && (user_y1 >= 0 && user_y1 <= ny1 - 1)) {
       *in_frame = 1;
     } else {
      user_x1 = MINI(user_x1, nx1 - 1);
      user_x1 = MAXI(user_x1, 0);
      user_y1 = MINI(user_y1, ny1 - 1);
      user_y1 = MAXI(user_y1, 0);
      *in_frame = 0;
     }

// Origin of device coordinate is on top left:
// whereas I want to display the images with origin on bottom left:
// Conversion:
   *dev_x0 = NINT(user_x1 + 0.5);
   *dev_y0 = NINT((double)ny1 - 0.5 - user_y1);

return;
}
/*************************************************************
* ConvDevToUserForImages
* to convert device coordinates to user coordinates
*
* Origin of device coordinate is on top left:
* whereas I want to display the images with origin on bottom left:
* Hence y = ny1 -1 - y
* JLP2014: I checked that I really get 64.0,64.0 at the center
* of the central pixel in a 128x128 frame.
*
* INPUT:
*  dev_x0, dev_y0: device coordinates
*
* OUTPUT:
*  user_x1, user_y1: user coordinates
*  in_frame: 1 if user_x1 and user_y1 are in the range [0, nx1[ or [0, ny1[
*            0 otherwise
*
*************************************************************/
void JLP_GDev_PST::ConvDevToUserForImages(double dev_x0, double dev_y0,
                                          double *user_x1, double *user_y1,
                                          int *in_frame)
{
int nx1, ny1;

*in_frame = 0;
*user_x1 = 0.;
*user_y1 = 0.;

  nx1 = Jgc0.nx;
  ny1 = Jgc0.ny;

  *user_x1 = -0.5 + dev_x0;
  *user_y1 = ny1 - 0.5 - dev_y0;

// user_x1, user_y1 are the user coordinates
  if((*user_x1 >= 0 && *user_x1 < nx1)
     && (*user_y1 >= 0 && *user_y1 < ny1)) {
       *in_frame = 1;
     } else {
      *in_frame = 0;
     }

/* DEBUG:
  printf("ConvDevToUserForImages: dev_x=%f dev_y=%f user_x1=%f user_y1=%f\n",
               dev_x0, dev_y0, *user_x1, *user_y1);
*/

return;
}
