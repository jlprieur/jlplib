/***************************************************************
* jlp_splot_lut1.cpp
* FUNCTION: Look-up-tables routines 
*
* lut_type: 'l'=log rainbow1 'r'=rainbow2 's'=saw 'g'=gray 'c'=curves
*           'p'=pisco
*
* Contains:
* CONVERT_TO_LUT(image1,nx1,ny1,idim1,image2,
*                 nx2,ny2,idim2,ncolors)
* jlp_splot_change_lut(lut_type, reversed, &ioff, &islope, ncolors, idv)
* jlp_splot_key(nn1,lower_itt,upper_itt,zlabel,horiz,axis_also,gamma1,gamma_d,idv)
* int JLP_ALLOC_LUT(int *ncolors, int *private_lut, int idv)
* int JLP_LOAD_LUT(int *r, int *g, int *b, int *ncolors, int *idv1)
* int JLP_REVERSE_LUT(int *idv1)
*
* VERSION: 10/02/2017 
*
* AUTHOR: JLP 
***************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "jlp_gdev_idv.h"
#include "jlp_splot_idv.h"

/*
#define DEBUG
*/

/***************************************************************
*
* FUNCTION: CONVERT_TO_LUT 
*
* PURPOSE: Image sampling and conversion from float to int.
*          Affects to each value the LUT address
*          WARNING: nx2 should be always smaller than nx1!
*
* INPUT:  image1[nx1*ny1] = float image
*         nx1, ny1 = size in x and y 
*         lower_itt = lower threshold
*         upper_itt = upper threshold 
*         ncolors = number of values for the LUT
*
* OUTPUT: image2[nx2*ny2] = int image
*
* VERSION: 09-08-2013
*
* AUTHOR: JLP 
*
***************************************************************/
int CONVERT_TO_LUT(float *image_f1, int *nx1, int *ny1, int *idim1,
                   int *image2, int *nx2, int *ny2, int *idim2,
                   int *ncolors, int *itt_is_linear, float *lower_itt, 
                   float *upper_itt, int *idv1)
/***************************************************************/
{
int i, j, status = -1;
int *image22;
double *image_d1;

if((nx1 <= 0) || (ny1 <= 0) || (nx1 > idim1)) {
  return(-1);
  }

image_d1 = new double[(*nx1) * (*ny1)];
for(j = 0; j < (*ny1); j++) {
 for(i = 0; i < (*nx1); i++) {
  image_d1[i + j * (*nx1)] = image_f1[i + j * (*idim1)];
  }
 }

image22 = new int[(*nx2) * (*ny2)];
for(j = 0; j < (*ny2); j++) {
 for(i = 0; i < (*nx2); i++) {
  image22[i + j * (*nx1)] = image2[i + j * (*idim2)];
  }
 }

 if(GDev_from_idv(*idv1)) {
   status = GDev_from_idv(*idv1)->convert_to_lut(image_d1, *nx1, *ny1, *nx1, 
                                                 image22, *nx2, *ny2, *nx2, 
                                                 *ncolors, *itt_is_linear, 
                                                 *lower_itt, *upper_itt);
   }

delete[] image_d1;
delete[] image22;

return(status);
}
/* *************************************************************
* Allocate look up table
*
* OUTPUT:
* private_lut: flag set to one if LUT cannot be changed
***************************************************************/
int JLP_ALLOC_LUT(int *ncolors, int *private_lut, int *idv1)
{
int status = -1;

 if(GDev_from_idv(*idv1)) {
   status = GDev_from_idv(*idv1)->jlp_alloc_lut(ncolors, private_lut);
   }

return(status);
}
/**************************************************************
* Load look up table
* Input:
* r, g, b: color values between 0 and 65535
***************************************************************/
int JLP_LOAD_LUT(int *r, int *g, int *b, int *ncolors, int *idv1)
{
int status = -1;

 if(GDev_from_idv(*idv1)) {
   status = GDev_from_idv(*idv1)->jlp_load_lut(r, g, b, *ncolors);
   }

return(status);


return(status);
}
/**************************************************************
* Reverse look up table
*
* INPUT:
* r, g, b: color values between 0 and 65535
***************************************************************/
int JLP_REVERSE_LUT(int *idv1)
{
int status = -1;

 if(GDev_from_idv(*idv1)) {
   status = GDev_from_idv(*idv1)->jlp_reverse_lut();
   }

return(status);
}
/****************************************************************
* Change the LUT for idev device 
* 
* INPUT:
* ncolors: number of colors
* reversed: 1 if reversed, 0 otherwise
* idv: graphic device number
*
* INPUT/OUTPUT:
* ioff: between 0 and ncolors
* islope: between 0 and ncolors
*    (ioff=ncolors/2, islope=ncolors/2 for standard scale)
*************************************************************/
int jlp_splot_change_lut(char *lut_type, int reversed, int *ioff, 
                               int *islope, int ncolors, int idv)
{
int status = -1;

 if(GDev_from_idv(idv)) {
   status = GDev_from_idv(idv)->jlp_change_lut(lut_type, reversed, ioff,      
                                               islope, ncolors);
   }

return(status);
}
/******************************************************************
* To display a key (LUT correspondance)
* with legend (if "axis_also").
* 
* INPUT:
* nn1 : width of "lut-converted" image (nx if horizontal, ny if vertical)
* horiz: 1 if horizontal, 0 for vertical
* axis_also: 1 if labeled axis
* scale_x, scale_y: Conversion between pixels and mgo coord (i.e., fsx, fsy).
*******************************************************************/
int jlp_splot_key(int nn1, float lower_itt, float upper_itt, char *zlabel, 
                  int horiz, int axis_also, int gamma1, int gamma_d, 
                  float scale_x, float scale_y, int idv)
{
int status = -1;

 if(GDev_from_idv(idv)) {
   status = GDev_from_idv(idv)->jlp_key(nn1, lower_itt, upper_itt, zlabel,
                                        horiz, axis_also, gamma1, gamma_d,
                                        scale_x, scale_y);
   }

return(status);
}
