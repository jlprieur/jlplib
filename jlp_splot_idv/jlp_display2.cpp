/*************************************************************************
* "jlp_display2.c"
*
* To display images either to postscript or X11 display
*
* int SPLOT_IMAGE(float *image_f1, int *nx1, int *ny1, 
*                 int *ncolors, char *filename, char *comments,
*                 char *lut_type, int *itt_is_linear,
*                 float *lower_itt, float *upper_itt, 
*                 char *xlabel, char *ylabel, char *title,
*                 char *plotdev, int *nobox, int *lut_scale)
* int SPLOT_DEVICE2
* int SPLOT_IMAGE2(float *image_f1, int *nx1, int *ny1,
*                  int *nx2, int *ny2, 
*                  int *ncolors, char *filename, char *comments,
*                  int *itt_is_linear, float *lower_itt, float *upper_itt,
*                  char *xlabel, char *ylabel, char *zlabel, char *title,
*                  int *full_caption, float *xmin_user0, float *xmax_user0,
*                  float *ymin_user0, float *ymax_user0, int *nobox,
*                  int *lut_scale, int *idv1, int *color_output);
*
* JLP
* Version 14-06-2006
****************************************************************/
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "jlp_splot_idv.h"
#include "jlp_auto_scale1.h"

/*
#define DEBUG
*/

#ifndef PI 
#define PI 3.1415926536
#endif

static int jlp_plot2(int *image_i, int nx2, int ny2, int idim,
                     float low_thresh, float high_thresh, int ncolors,
                     char *xlabel, char *ylabel, char *zlabel, char *title, 
                     int full_caption, char *image_name, char *image_comments, 
                     int lut_scale, int nobox, int color_output, 
                     float xmin_user0, float xmax_user0, float ymin_user0, 
                     float ymax_user0, float lower_itt, float upper_itt,
                     char *axis_color, int idv);
/***********************************************************
* jlp_plot2 
* display an image to idv graphic device
*
* INPUT:
*
* image_i: input image to be plotted
* nx2, ny2: size of input image
* idim: first dimension of the input image 
* low_thresh, high_thresh: low and high threshold of the image
* width, height: output size of the image on the plot (cm)
* title: title of the plot 
* full_caption = 1 if comments and filename are wanted
* image_name, image_comments: to be written on the bottom of the plot
* nobox = 1 if box around the image is not wanted
* Options:
*   lut_scale: 1 if lut scale is wanted, 0 otherwise
*   color_output:     1 if color output is wanted, 0 for black and white
*
* OUTPUT:
* Postscript file (or display to X11 screen according to plotdev) 
*
************************************************************/
static int jlp_plot2(int *image_i, int nx2, int ny2, int idim,
                     float low_thresh, float high_thresh, int ncolors,
                     char *xlabel, char *ylabel, char *zlabel, char *title, 
                     int full_caption, char *image_name, char *image_comments, 
                     int lut_scale, int nobox, int color_output, 
                     float xmin_user0, float xmax_user0, float ymin_user0, 
                     float ymax_user0, float lower_itt, float upper_itt,
                     char *axis_color, int idv)
{
int ticks_in, box_numbers, black_and_white; 
/* JLPPPP: TO BE DONE LATER: GAMMA_D */
int ix0, iy0, ixlen, iylen, gamma1 = 1, gamma_d = 1, hardcopy_device;
int axis_also = 1, horiz = 0;
float scale_x, scale_y;             // Conversion between pixels and mgo coord.
float expand = 1.2;

hardcopy_device = (Jgdev_dev_type(idv) == 2) ? 1 : 0;
black_and_white = 1 - color_output;
ix0 = Jgdev_offx(idv);
iy0 = Jgdev_offy(idv);
ixlen = Jgdev_axlen(idv);
iylen = Jgdev_aylen(idv);
scale_x = Jgdev_fsx(idv);
scale_y = Jgdev_fsy(idv);
JLP_PLOT_IMAGE(image_i, &nx2, &ny2, &nx2, &ix0, &iy0, 
               &gamma1, &gamma_d,&black_and_white,&idv);

/* axes: */
   ticks_in = 0;
   box_numbers = 1;
/*
* int jlp_splot_box_type0(char *xlabel, char *ylabel, char *title, int ticks_in,
*              int box_numbers, int full_caption, int xgrid_is_wanted, 
*              int ygrid_is_wanted, float xmin_user0, float xmax_user0, 
*              float ymin_user0, float ymax_user0, int ix0, int iy0, 
*              int ixlen, int iylen, char *axis_color, int idv);
*/
   if(!nobox){
        jlp_splot_box_type0(xlabel, ylabel, title, ticks_in, box_numbers,
                            full_caption, 0, 0, xmin_user0, xmax_user0,
                            ymin_user0, ymax_user0, ix0, iy0, ixlen, iylen, 
                            expand, axis_color, idv);
        if(hardcopy_device && full_caption) 
                 jlp_comments_for_curves(image_name, image_comments, 
                 expand, idv);
        }

/* LUT scale on the bottom: */
if(lut_scale) {
   if(horiz) 
    jlp_splot_key(nx2,lower_itt,upper_itt,zlabel,horiz,axis_also,gamma_d,
                  scale_x, scale_y, idv);
   else 
    jlp_splot_key(ny2,lower_itt,upper_itt,zlabel,horiz,axis_also,gamma_d,
                  scale_x, scale_y, idv);
}

/* Flushes graphic to screen; */
  JLP_GFLUSH(&idv);

return(0);
}
/***************************************************************
* SPLOT_IMAGE
*
*  image_f1[nx1,ny1] is the initial image
*  image_i[nx2,ny2] is the reduced or initial image converted to LUT numbers
*  (the image is reduced if nx1 or ny1 > max_size) 
*
* INPUT:
* lower_itt,upper_itt: lower and upper thresholds for display
* lut_type: saw, log_rainbow, rainbow
*          or GRAY, SAW, RAINBOW, LOG_RAINBOW for reversed LUT
* itt_is_linear: 1 if linear, 0 if log
* plotdev: name of output device (e.g. square/test.ps)
* nobox: if set to 1, no box around image
* lut_scale: 1 if lut scale is wanted, 0 otherwise
* (to remove comments and title, simply use upper case for plotdev,
*  e.g., SQUARE, POSTSCRIPT...)
*
* OUTPUT:
* idv1
****************************************************************/
int SPLOT_IMAGE(float *image_f1, int *nx1, int *ny1, int *ncolors, 
               char *filename, char *comments, char *lut_type, 
               int *itt_is_linear, float *lower_itt, float *upper_itt, 
               char *xlabel, char *ylabel, char *zlabel, char *title,
               char *plotdev, int *idv1, float *xmin_user0, float *xmax_user0,
               float *ymin_user0, float *ymax_user0, int *nobox, 
               int *lut_scale)
{
int istat, nx2, ny2, color_output, max_size, full_caption;
int lut_offset, lut_slope, erase = 0;

/* Open graphic device and load LUT 
*/
/* Maximum size of arrays that are not reduced for display */
max_size = 500;
/* LUT parameters: */
lut_offset = (*ncolors) / 2;
lut_slope = (*ncolors) / 2; 
istat = SPLOT_DEVICE2(filename, image_f1, nx1, ny1, &nx2, &ny2, 
                      &max_size, ncolors, &color_output, lut_type, &lut_offset,
                      &lut_slope, title, plotdev, idv1);
if(istat) {
   printf("SPLOT_IMAGE/error in SPLOT_DEVICE2: status = %d\n",istat);
   return(-1);
   }

/* full caption if not upper case for square, portrait or landscape */
/* When full caption=1, title and comments are displayed */
 full_caption = ((*plotdev == 'S') || (*plotdev == 'P') 
                   || (*plotdev == 'L')) ? 0 : 1;

 SPLOT_IMAGE2(image_f1, nx1, ny1, &nx2, &ny2, ncolors, 
              filename, comments, itt_is_linear, lower_itt, upper_itt, 
              xlabel, ylabel, zlabel, title, &full_caption, 
              xmin_user0, xmax_user0, ymin_user0, ymax_user0, nobox, 
              lut_scale, idv1, &color_output, &erase);

JLP_SPCLOSE(idv1);

return(0);
}
/***************************************************************
* SPLOT_IMAGE2
*
*  image_f1[nx1,ny1] is the initial image
*  image_i[nx2,ny2] is the reduced or initial image converted to LUT numbers
*  (the image is reduced if nx1 or ny1 > max_size) 
*
* INPUT:
* lower_itt,upper_itt: lower and upper thresholds for display
* lut_type: saw, log_rainbow, rainbow
*          or GRAY, SAW, RAINBOW, LOG_RAINBOW for reversed LUT
* itt_is_linear: 1 if ramp, 0 if log
* plotdev: name of output device (e.g. square/test.ps)
* nobox: if set to 1, no box around image
* lut_scale: 1 if lut scale is wanted, 0 otherwise
* (to remove comments and title, simply use upper case for plotdev,
*  e.g., SQUARE, POSTSCRIPT...)
*
****************************************************************/
int SPLOT_IMAGE2(float *image_f1, int *nx1, int *ny1, int *nx2, int *ny2, 
                 int *ncolors, char *filename, char *comments, 
                 int *itt_is_linear, float *lower_itt, float *upper_itt, 
                 char *xlabel, char *ylabel, char *zlabel, char *title,
                 int *full_caption, float *xmin_user0, float *xmax_user0,
                 float *ymin_user0, float *ymax_user0, int *nobox, 
                 int *lut_scale, int *idv1, int *color_output,
                 int *erase)
{
int  *image_i, istat, isize2; 
char axis_color[40];

strcpy(axis_color, "Black");

/* Automatic scale: */
if(*lower_itt == *upper_itt)
   auto_scale_float(image_f1,*nx1,*ny1,*nx1,lower_itt,upper_itt);

#ifdef DEBUG
printf("SPLOT_IMAGE2/nx1=%d ny1=%d ncolors=%d, low=%.2e up=%.2e nobox=%d\n",
        *nx1, *ny1, *ncolors, *lower_itt, *upper_itt, *nobox);
printf("SPLOT_IMAGE2/xlabel=%s\n ylabel=%s\n title=%s\n",
        xlabel, ylabel, title);
#endif

/* Allocate memory for ITT-transformed image: */
 isize2 = (*nx2) * (*ny2) * sizeof(int);
 image_i = (int *) malloc(isize2);

/* Erase graphic device: */
  if(*erase) jlp_erase(*idv1);

/* LUT conversion : */
   CONVERT_TO_LUT(image_f1,nx1,ny1,nx1,image_i,nx2,ny2,nx2,ncolors,
                  itt_is_linear, lower_itt,upper_itt,idv1);

/* Now plotting the image to graphic device: */
   istat = jlp_plot2(image_i,*nx2,*ny2,*nx2,*lower_itt,*upper_itt,
                     *ncolors, xlabel,ylabel,zlabel,
                      title,*full_caption,filename,comments,
                      *lut_scale, *nobox, *color_output, *xmin_user0,
                      *xmax_user0, *ymin_user0, *ymax_user0,
                      *lower_itt, *upper_itt, axis_color, *idv1);

free(image_i);

if(istat) {
   printf("SPLOT_IMAGE2/error in jlp_plot2: status = %d\n",istat);
   return(-3);
   }

return(0);
}
/************************************************************************
* INPUT:
* filename: name of the input file containing the data
* image_f1: float array with input data
* nx1, ny1: size of input image
*
* OUTPUT:
* nx2, ny2: size of "LUT" integer image
* color_output
* idv1
************************************************************************/
int SPLOT_DEVICE2(char *filename, float *image_f1, int *nx1, int *ny1, 
                  int *nx2, int *ny2, int *max_size,
                  int *ncolors, int *color_output, char *lut_type, 
                  int *lut_offset, int *lut_slope, char *title, 
                  char *plotdev, int *idv1)
{
int gamma1, gamma_d, reversed, istat, private_lut;
char lut_type0[20], out_filename[256];

strcpy(out_filename, "tmp.ps");

if(*ncolors > 256) {
  printf("SPLOT_DEVICE2/Fatal error ncolors=%d > (max value = 256)!\n", 
           *ncolors);
  exit(-1);
  } 

/* Open graphic device and initialize graphic context: */
istat = JLP_DEVICE_IMAGE(plotdev, out_filename, title, image_f1, 
                         nx1, ny1, &gamma1, &gamma_d, idv1);
 *nx2 = gamma1 * (*nx1);
 *ny2 = gamma1 * (*ny1);

if(istat) {
  printf("SPLOT_DEVICE2/Fatal error opening device %s \n",plotdev);
  exit(-1);
  }

/* First loading of the LUT : */
/* (or resetting the LUT table to zero for postscript device): */
   istat = JLP_ALLOC_LUT(ncolors, &private_lut, idv1);
   if(!private_lut) {
     printf("SPLOT_DEVICE2/No private lut: read-only color cells\n");
     }

/* Reversed option with upper case: */
/*
* lut_type: saw, log_rainbow, rainbow, curve
*          or GRAY, SAW, RAINBOW, LOG_RAINBOW, CURVE for reversed LUT
*/
/* Note that we use an intermediary variable "lut_type0" 
* (since lut_type can be only a string and not a full "char" variable)
*/
   switch (lut_type[0]) 
     {
     case 'C': 
       reversed = 1; 
       strcpy(lut_type0,"curve");
       break;
     case 'G': 
       reversed = 1; 
       strcpy(lut_type0,"gray");
       break;
     case 'S': 
       reversed = 1; 
       strcpy(lut_type0,"saw");
       break;
     case 'R': 
       reversed = 1; 
       strcpy(lut_type0,"rainbow");
       break;
     case 'L': 
       reversed = 1; 
       strcpy(lut_type0,"log_rainbow");
       break;
     default:
       reversed = 0;
/* WARNING: pb with Fortran interface, so: */
       strncpy(lut_type0,lut_type,3);
       lut_type0[4]='\0';
       break;
     }

/* Loading the LUT: */
   jlp_splot_change_lut(lut_type0, reversed, lut_offset, lut_slope, *ncolors, 
                        *idv1);

/* B&W or color according to "color_output" flag: */
  if(!strncmp(lut_type0,"saw",3) || !strncmp(lut_type0,"gray",4))
     *color_output = 0;
  else
     *color_output = 1;

return(0);
}
