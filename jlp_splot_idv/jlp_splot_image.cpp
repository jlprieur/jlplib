/******************************************************************
* "jlp_splot_image.cpp" 
*
*  JLP_DEVICE_IMAGE
*  JLP_PLOT_IMAGE(image,nx,ny,idim,xstart,ystart,gamma_d,black_and_white,idv1)
*  jlp_hardcopy_image(float *image_f1, int *image_i, int *i_nx22,...)
*  JLP_GET_2CIRCLES(float *x_cent, float *y_cent, float *diam1,
*                   float *diam2, int *ncirc, int *idv1)
*  jlp_get_winlimits(float *x1, float *y1, float *x2, float *y2, 
*                    int type_of_win, int *pressed_button, 
*                    int *in_frame, int idv)
*  JLP_CIRCLE1(float *x, float *y, float *diam, int *idv1)
*  jlp_setup_menu(char *items, int *nitems, int *menu_nsub, int *menu_slen,
*                   int *vertical, int *idv1)
*  jlp_select_menu(int *menu_select, int *menu_subselect, int *idv1)
*
* JLP
* Version 15/05/2017
*************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
// #include <string.h>  // Problems with index with gxwidgets : #undef index
#include <math.h>

#include "jlp_gdev.h"          // JLP Graphic Device class
#include "jlp_gdev_pst.h"      // JLP PST Image Graphic class
#include "jlp_gdev_idv.h"
#include "jlp_splot_idv.h"

#undef index      // Problems with index with gxwidgets (conflict with string.h)

#if JLP_USE_X11                 // New flag to disable X11 if necessary 
#include "jlp_gdev_x11.h"       // JLP X11 graphic class (in jlp_x11plot/)
#endif
#if JLP_USE_WXWID               // New flag to disable wxWidgets if necessary 
// #include "jlp_gdev_wxwid.h"  // JLP wxWidgets graphic class (in jlp_wxplot/)
#endif

/*
#define DEBUG
*/

/***************************************************************
* To open a graphic device for image display 
*
* INPUT:
* plotdev
* title
* image_f: float array containing the data
* nx, ny: size of image_f
*
* OUTPUT:
* gamma1: compression ratio between nx1 (float image) and nx2 (LUT image) 
* gamma_d:  ratio between Frame_nx and nx1 
* idv1: device number (to identify multiple graphic devices opened
*         simultaneously)
*
* Compression and dilatation factors 
* gamma1 is the ratio (size of input real image / size of integer LUT image)
* gamma_d is the size of one image pixel in screen pixel units
* gamma_d_mini is the minimum size (i.e. the value that was chosen
* when creating the window)
*
***************************************************************/
int JLP_DEVICE_IMAGE(char *plotdev, char *out_filename, char *title, 
                     float *image_f, int *nx, int *ny, 
                     int *gamma1, int *gamma_d, int *idv1)
{
JLP_GDev *Jgd0;
double *image_d;
int i, status = -1, gam1 = 1, gamd1 = 1;
char err_messg[128];

if(*nx <= 0 || *ny <= 0) {
  return(-1);
  }

image_d = new double[(*nx) * (*ny)];
for(i = 0; i < (*nx) * (*ny); i++) {
 image_d[i] = image_f[i];
 }

switch (plotdev[0]) 
{
// Postscript: Postscript, postscript, SQUARE, square, LANDSCAPE, landscape
  case 'P':
  case 'p':
  case 'S':
  case 's':
  case 'L':
  case 'l':
    Jgd0 = new JLP_GDev_PST(plotdev, out_filename, title, image_d, 
                              *nx, *ny, &gam1, &gamd1, &status, err_messg);
    *idv1 = Jgd0->Jgc0_dev_idv();
    break;
#if JLP_USE_X11
// X11: XDISPLAY, xdisplay, XTERM, xterm: 
  case 'X':
  case 'x':
    Jgd0 = new JLP_GDev_X11(plotdev, out_filename, title, image_d,
                             *nx, *ny, gam1, gamd1, status, err_messg);
    *idv1 = Jgd0->Jgc0_dev_idv();
    break;
#endif
#if JLP_USE_WXWID
// wxWidgets: wterm, wdisplay
  case 'W':
  case 'w':
    fprintf(stderr,"JLP_DEVICE_IMAGE/Error: wxWidgets plotdev %s not ready yet\n", 
            plotdev);
/*
    Jgd0 = new JLP_GDev_wxWID(plotdev, out_filename, title, image_d,
                              *nx, *ny, gam1, gamd1, status, err_messg);
    *idv1 = Jgd0->Jgc0_dev_idv();
*/
    break;
#endif
  default:
    fprintf(stderr,"JLP_DEVICE_IMAGE/Error: unknown device type: %s\n", 
            plotdev);
    break;
}

/* In case of failure free idv1 number */
  if(status) {
    fprintf(stderr,"JLP_DEVICE_IMAGE/Error opening %s \n", plotdev);
    exit(-1);
    }
  else {
/* Look for an identifier (idv1) and initialize it with current opened device */
    status = GDev_alloc_idv(Jgd0, idv1, err_messg);
#ifdef DEBUG
    printf("JLP_DEVICE_IMAGE/graphic device #idv1 = %d sucessfully opened\n", 
           *idv1);
#endif

// Initialize common bloc PARAMETERS for fortran interface:
// Should be avoided when many devices are opened simultaneously...
/*
    JGC_TO_COMMON(idv1);
*/
    }

/* Transfer to output parameters: */
*gamma1 = gam1;
*gamma_d = gamd1;

delete[] image_d;

return(status);
}
/* *************************************************************
* Plot an image on the graphic device 
*
* INPUT:
* xstart, ystart: lower-left corner of the frame in mgo coordinates
***************************************************************/
int JLP_PLOT_IMAGE(int *image2, int *nx2, int *ny2, int *idim2,
                   int *xstart, int *ystart, int *gamma1,
                   int *gamma_d, int *black_and_white, int *idv1)
{
int status = -1;

 if(GDev_from_idv(*idv1)) {
   status = GDev_from_idv(*idv1)->plot_image(image2, *nx2, *ny2, *idim2, *xstart,
                                       *ystart, *gamma1, *gamma_d, 
                                       *black_and_white);
 } else {
  fprintf(stderr, "JLP_PLOT_IMAGE/Error: only available for igdev devices \n");
  exit(-1);
 }

#ifdef TTTT
int xs, ys, nx, ny, idim, idv;
idv = *idv1;

/* Transfer (for Fortran interface) */
xs = *xstart; ys = *ystart;
nx = *nx1; ny = *ny1; idim = *idim1;

  switch (Jgc[*idv1].devtype)
  {
  case 1:
    {
    status = x11_image(image,nx,ny,idim,xs,ys,*gamma_d,idv);
    break;
    }
  case 2:
    {
/* B&W: */ 
    if(*black_and_white)
       status = pst_image256(image,nx,ny,idim,Jgc[idv].gclr.r,
                             Jgc[idv].gclr.ncolors,xs,ys,idv);
    else
/* Color: */
      status = pst_imageRGB(image,nx,ny,idim,Jgc[idv].gclr.r,Jgc[idv].gclr.g,
                            Jgc[idv].gclr.b,Jgc[idv].gclr.ncolors,xs,ys,idv);
      status = gif_imageRGB(image,nx,ny,idim,Jgc[idv].gclr.r,Jgc[idv].gclr.g,
                            Jgc[idv].gclr.b,Jgc[idv].gclr.ncolors,"tmp.gif");
    break;
    }
  default:
    {
    status = -1;
    break;
    }
  }
#endif
return(status);
}
/***************************************************************
*
* INPUT:
*
* image_f1: input float image to be plotted
* nx1, ny1: size of input image
* idim: first dimension of the input image
* low_thresh, high_thresh: low and high threshold of the image
* width, height: output size of the image on the plot (cm)
* title: title of the plot
* image_name, image_comments: to be written on the bottom of the plot
* f1_xstart, f1_ystart, f1_xend, f1_yend: window boundaries of zoomed image
* itt_is_linear: linear or log
* Options:
*  lut_scale: option to display the lut scale on the edge of the frame 
*             (1 if lut scale is wanted, 0 otherwise)
*              (for color prints only, at the bottom of the plot),
*  color_output:  1 if color output is wanted, 0 for black and white
* graphic_file: file with graphic commands
*
* OUTPUT:
*  postscript file
****************************************************************/
int jlp_hardcopy_image(float *image_f1, int *image_i, int *i_nx22,
                       int *i_ny22, int *nx2, int *ncolors, char *filename, 
                       char *comments, int lut_scale, int black_and_white, 
                       int high_resolution, int *nx1, int *ny1, 
                       int f1_xmin, int f1_ymin, int *f1_nx11, 
                       int *f1_ny11, int width_frame, int height_frame, 
                       int itt_is_linear, float lower_itt, float upper_itt, 
                       char *lut_type, int inversed_lut, int lut_offset, 
                       int lut_slope, char *graphic_file, char *pst_plotdev,
                       char *out_filename, char *title)
{
int idv_copy, private_lut, gamma1, gamma_d;
float xmin, xmax, ymin, ymax, f1_xmax, f1_ymax;
float pst_width, pst_height;
char extra_comments[80];
int status;

if(*nx1 <= 0 || *ny1 <= 0) {
  return(-1);
  }

/* Output size of postscript hardcopies (cm) (written by pstcopy1) */
      if(height_frame < width_frame)
        {
        pst_width = 16.;
        pst_height = pst_width * height_frame / width_frame; 
        }
      else
        {
        pst_height = 20.;
        pst_width = pst_height * width_frame / height_frame; 
        if(pst_width > 16)
          {
          pst_width = 16;
          pst_height = pst_width * height_frame / width_frame; 
          }
        }

/* Open output postscript file and write header: */
/* WARNING: f1_nx11, f1_ny11 can be modified in JLP_DEVICE_IMAGE: */

/***************************************************************
*  image_f1[nx1,ny1] is the initial image
*  image_i2[nx2,ny2] is the reduced (by gamma1) 
*                      or the initial image converted to LUT numbers
*  (the image is reduced of a factor gamma1 if nx1 or ny1 > max_size) 
*
* Frame_nx, Frame_ny: size of the frame in mgo coordinates 
* Frame_x0, Frame_y0: origin (lower-left corner) of the frame 
*                     in mgo coordinates 
* gamma1 is the ratio nx1/nx2
* gamma_d is the ratio Frame_nx/nx2, 
*         correspondance between output graphic memory array and 
*         integer image_i2 array (set to 1 if original array has been reduced)
*****************************************************************/

 strcpy(extra_comments," ");
 status = JLP_DEVICE_IMAGE(pst_plotdev, out_filename, title, image_f1,
                           f1_nx11,f1_ny11,&gamma1,&gamma_d,&idv_copy);
 if(status) {
   fprintf(stderr,"jlp_hardcopy_image/Error opening postscript file in JLP_DEVICE_IMAGE\n");
   return(-1);
   }
  if(GDev_from_idv(idv_copy) == NULL) {
   fprintf(stderr,"jlp_hardcopy_image/Error allocating idv number\n");
   return(-1);
   }

/* Loading of the LUT (linear ramp for postscript): */
   status = JLP_ALLOC_LUT(ncolors,&private_lut,&idv_copy);
   jlp_splot_change_lut(lut_type, inversed_lut, &lut_offset, &lut_slope, 
                        *ncolors, idv_copy);

/* LUT conversion with ITT (taking zoom into account): */
   CONVERT_TO_LUT(&image_f1[f1_xmin+f1_ymin*(*nx1)],f1_nx11,f1_ny11,nx1,
                  image_i,i_nx22,i_ny22,nx2,ncolors,&itt_is_linear,
                  &lower_itt,&upper_itt,&idv_copy);

/* 
* pstcopy1 changes the values of the offsets and axes lengths: 
* JGc0.xoff and JGc0.yoff, Jgc0.axlen, etc
*
* WARNING: pstcopy1 also writes a header!
*
* 9th argument is title,
*/
  f1_xmax = f1_xmin+(*f1_nx11)-1;
  f1_ymax = f1_ymin+(*f1_ny11)-1;
/*
  int pstcopy1(int *image1, int nx1_0, int ny1_0, int idim,
               float low_thresh, float high_thresh, int ncolors,
               float width, float height, char *title, char *image_name,
               char *image_comments, char *extra_comments, int lut_scale,
               int black_and_white, int high_resolution, int f1_xstart,
               int f1_ystart,int f1_xend, int f1_yend);
*/
  status = GDev_from_idv(idv_copy)->pstcopy1(image_i,*i_nx22,*i_ny22,*nx2,lower_itt,
                                       upper_itt,*ncolors,pst_width,pst_height,
                                       title, filename, 
                                       comments, extra_comments,
                                       lut_scale, black_and_white,
                                       high_resolution,
                                       f1_xmin, f1_ymin, f1_xmax, f1_ymax);
   if(status) fprintf(stderr,"jlp_hardcopy_image/Error in pstcopy1: status = %d \n",
                      status);

/* Set limits to make them in agreement with zoom and the new values
* of offx, offy, axlen, aylen defined in pstcopy1: */
 xmin = f1_xmin;
 xmax = f1_xmin + *f1_nx11;
 ymin = f1_ymin;
 ymax = f1_ymin + *f1_ny11;
 JLP_SET_NEW_LIMITS(&xmin, &xmax, &ymin, &ymax, &idv_copy);

/* Draw graphic complements: */
 if(*graphic_file) jlp_read_graphic_file(graphic_file,idv_copy);

/* Close postscript file and device */
   if(GDev_from_idv(idv_copy) != NULL) GDev_from_idv(idv_copy)->gdev_close();

return(status);
}
/**************************************************************
*
* JLP_GET_2CIRCLES
*
* To get one or two concentric circles for circular photometry
* or patch applications from a displayed image (digitized!)
*
* INPUT:
* ncirc: number of circles which are needed (1 or 2)
*
* OUTPUT:
*
* x_cent, y_cent: (user) coordinates of the center
* diam1, diam2: diameters of the small and big circles
*
****************************************************************/
int JLP_GET_2CIRCLES(float *x_cent, float *y_cent, float *diam1,
                     float *diam2, int *ncirc, int *idv1)
{
double xcent, ycent, ddiam1, ddiam2;
int nc;
int status = -1;

/* Transfer (for Fortran interface) */
nc = *ncirc;
*x_cent = 0.; *y_cent = 0.; *diam1 = 0.; *diam2 = 0.;
*ncirc = 0;

 if(GDev_from_idv(*idv1)) {
   status = GDev_from_idv(*idv1)->get_circles(xcent, ycent, ddiam1, ddiam2, nc);
   if(status == 0) {
     *x_cent = xcent; *y_cent = ycent; *diam1 = ddiam1; *diam2 = ddiam2;
     *ncirc = nc;
     }
   }


return(status);
}
/**************************************************************
 *
 * INPUT:
 * type_of_win:  1=line
 *               >1 = rectangle
 * OUTPUT:
 * x1, y1: coordinates of the beginning of the segment (user coord.)
 * x2, y2: coordinates of the end of the segment (user coord.)
 * in_frame
 ***************************************************************/
int jlp_get_winlimits(float *x1, float *y1, float *x2, float *y2, 
                      int type_of_win, int *pressed_button, 
                      int *in_frame, int idv)
{
double u_x1, u_y1, u_x2, u_y2;
int pbutton, in_frm=0;
int status = -1;

 *pressed_button = 0;
 *in_frame = 0;
 *x1 = 0.; *x2 = 0.; *y1 = 0.; *y2 = 0.;
 if(GDev_from_idv(idv)) {
   status = GDev_from_idv(idv)->get_winlimits(u_x1,u_y1,u_x2,u_y2,type_of_win,
                                     pbutton,in_frm);
   if(status == 0) {
     *x1 = u_x1; *x2 = u_x2; *y1 = u_y1; *y2 = u_y2;
     *pressed_button = pbutton;
     *in_frame = in_frm;
   }
 }

#ifdef DEBUG
printf("jlp_get_winlimits/ x1=%.1f y1=%.1f x2=%.1f y2=%.1f in_frame=%d\n",
         *x1, *y1, *x2, *y2, *in_frame);
#endif

return(status);
}
/***********************************************************************
* jlp_setup_menu
*
* INPUT:
*   items,nitems
*   items is an array with lines of 20 characters
*
**********************************************************************/
int jlp_setup_menu(char *items, int *nitems, int *menu_nsub, int *menu_slen,
                   int *vertical, int *idv1)
{
int status = -1;

 if(GDev_from_idv(*idv1)) {
   status = GDev_from_idv(*idv1)->setup_menu(items, *nitems, *menu_nsub, 
                                             *menu_slen, *vertical);
   }

return(status);
}
/***********************************************************************
* jlp_select_menu
*
* OUTPUT:
*   selection: index of the selected item
*
**********************************************************************/
int jlp_select_menu(int *menu_select, int *menu_subselect, int *idv1)
{
int status = -1;
int select = 0, subselect = 0;

 if(GDev_from_idv(*idv1)) {
   status = GDev_from_idv(*idv1)->select_menu(select, subselect);
   }

*menu_subselect = subselect;
*menu_select = select;

return(status);
}
