/**************************************************************
*  SPLOT_FOR 
* 
*  Fortran interface to C routines.
*
* Contains:
*   JLP_SPDEVICE_IMAGE
*   JLP_SPDEVICE_CURVE(plotdev,xmin,xmax,ymin,ymax,plan,title,idv1);
*   JLP_SPLABEL(xlabel,max_length,ix,iy,angle,expand,idrawit,length,idv1)
*   JLP_SPBOX(float box_xmin0, float box_xmax0, float box_ymin0, 
*             float box_ymax0,
*             char *xlabel, char *ylabel, char *title, int *ticks_in, 
*             int *box_numbers, char *filename, char *comments, int *idv1)
*
* JLP
* Version 12-07-2015
*************************************************************/
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "jlp_splot_idv.h"

/***************************************************************
* JLP_SPDEVICE_IMAGE 
* Fortran interface with JLP_DEVICE_IMAGE 
*
* OUTPUT: 
*  idv1: number of the plotting device
***************************************************************/
int JLP_SPDEVICE_IMAGE(char *plotdev, char *title, char *filename,
                     float *image_f, int *nx, int *ny, int *gamma1,
                     int *gamma_d, int *idv1)
{
int status;
char plotdev1[80], title1[40], out_filename1[256];

strcpy(out_filename1, "tmp.ps");

/* Filter on labels (for Fortran): */
jlp_string_copy(title, title1, 40);
jlp_string_copy(plotdev, plotdev1, 80);

status = JLP_DEVICE_IMAGE(plotdev1,out_filename1,title1,image_f,
                          nx,ny, gamma1,gamma_d,idv1);

if(status) printf("JLP_SPDEVICE_IMAGE/error %d\n",status);

return(0);
}
/***************************************************************
* JLP_SPDEVICE_CURVE 
* Fortran interface with JLP_DEVICE_CURVE 
*
* OUTPUT: 
*  idv1: number of the plotting device
***************************************************************/
void JLP_SPDEVICE_CURVE(char *plotdev, float *xmin_user1, float *xmax_user1, 
                        float *ymin_user1, float *ymax_user1, int *plan, 
                        char *title, int *idv1)
{
int status;
char plotdev1[80], title1[80], out_filename1[256];

strcpy(out_filename1, "tmp.ps");

/* Filter on labels (for Fortran): */
jlp_string_copy(title, title1, 80);
jlp_string_copy(plotdev, plotdev1, 80);

status = JLP_DEVICE_CURVE(plotdev1, out_filename1, xmin_user1, xmax_user1,
                          ymin_user1, ymax_user1, plan, title1, idv1);

if(status) printf("JLP_SPDEVICE_CURVE/error %d\n",status);

return;
}
/*****************************************************************************
*
* FUNCTION: JLP_SPLABEL
*
* PURPOSE: Draw a label (Fortran interface with jlp_label)
*
* INPUT:
* xlabel : label to be drawn
* ix, iy : (between 0 and 32000) window coordinates
* angle: (0 if horizontal, 90 if vertical)
* idrawit: 0 if only length is computed (for centering)
*          1 if routine is asked to draw
*
* OUTPUT:
* length: length in window coordinates of drawn string
*
* VERSION: 20-05-92
*
* AUTHOR: JLP
*
******************************************************************************/
void JLP_SPLABEL(char *xlabel, int *max_length, int *ix, int *iy,
                 float *angle, float *expand, int *idrawit, float *length,
                 int *idv1)
{
 char xlabel1[61];

/* Filter on labels (for Fortran): */
jlp_string_copy(xlabel, xlabel1, 60);

  *length = jlp_label(xlabel1,*ix,*iy,*angle,*expand,*idrawit,*idv1);
}
/*****************************************************************************
*
* FUNCTION: JLP_SPBOX
*
* PURPOSE: Draw a box for coordinate information
*          (Fortran interface with jlp_box)
*
******************************************************************************/
void JLP_SPBOX(float box_xmin0, float box_xmax0, float box_ymin0, 
               float box_ymax0,
               char *xlabel, char *ylabel, char *title, int *ticks_in, 
               int *box_numbers, char *filename, char *comments, int *idv1)
{
int status, full_caption, hardcopy_device;
float expand = 1.2;
char filename1[60], comments1[80];
char xlabel1[60], ylabel1[60], title1[120];
char axis_color[40];

/* Filter on labels (for Fortran): */
jlp_string_copy(xlabel, xlabel1, 60);
jlp_string_copy(ylabel, ylabel1, 60);
jlp_string_copy(title, title1, 120);
jlp_string_copy(filename, filename1, 60);
jlp_string_copy(comments, comments1, 60);
full_caption = 1;
strcpy(axis_color,"Black");
hardcopy_device = (Jgdev_dev_type(*idv1) == 2) ? 1 : 0;

/* (axis_type: 0=linear, 1=log10)
* jlp_splot_box(float box_xmin0, float box_xmax0, float box_ymin0, 
*               float box_ymax0, char *xlabel, char *ylabel, char *title, 
*               int ticks_in, int box_numbers, int full_caption, 
*               int jlp_axes_are_wanted, int xgrid_is_wanted, 
*               int ygrid_is_wanted, int x_is_log10, int y_is_log10, 
*               float expand, int idv)
*/

  status = jlp_splot_box(box_xmin0, box_xmax0, box_ymin0, box_ymax0,
                         xlabel1,ylabel1,title1,*ticks_in,*box_numbers,
                         full_caption,0,0,0,0,0,expand,axis_color,*idv1);
  if(hardcopy_device && full_caption)
           jlp_comments_for_curves(filename1, comments1, expand, *idv1);


  if(status) printf("JLP_SPBOX/error %d\n",status);
}
