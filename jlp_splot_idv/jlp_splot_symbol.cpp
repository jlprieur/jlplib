/******************************************************************
* "jlp_splot_symbol.cpp" 
*
* JLP
* Version 15/05/2017
*************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>  
#include <math.h>

#include "jlp_gdev.h"    // JLP Graphic Device class
#include "jlp_splot_idv.h"

/*
#define DEBUG
*/

// Contains:
int JLP_SYMBOL(int *x, int *y, int *isize, int *isymb, int *idv1);
int JLP_SYMBOL1(float *x, float *y, int *isize, int *isymb, int *idv1);
int JLP_SYMBOL2(float *x, float *y, float *size, int *isymb, int *idv1);
int JLP_SYMBOL_ERRORY1(float *x, float *y, float *erry, int *size, int *idv1);
int JLP_SYMBOL_ERRORX1(float *x, float *y, float *errx, int *size, int *idv1);
int JLP_CIRCLE1(float *x, float *y, float *diam, int *idv1);

/*******************************************************************
* INPUT:
* x, y: location in mgo coordinates 
* isize: size of symbol
* isymb: symbol type (square, circle, triangle, etc)
*******************************************************************/
int JLP_SYMBOL(int *x, int *y, int *isize, int *isymb, int *idv1)
{
int status = -1;

 if(GDev_from_idv(*idv1)) {
   status = GDev_from_idv(*idv1)->symbol(*x, *y, *isize, *isymb);
   }

return(status);
}
/*******************************************************************
* INPUT:
* x, y: location in user coordinates (assuming linear scale) 
* isize: size of symbol in (20 times) mgo coordinates
* isymb: symbol type (square, circle, triangle, etc)
*******************************************************************/
int JLP_SYMBOL1(float *x, float *y, int *isize, int *isymb, int *idv1)
{
int status = -1; 

 if(GDev_from_idv(*idv1)) {
   status = GDev_from_idv(*idv1)->symbol1(*x, *y, *isize, *isymb);
   }

return(status);
}
/*******************************************************************
* INPUT:
* x, y: location in user coordinates (assuming linear scale) 
* size: size of symbol in user coordinates
* isymb: symbol type (square, circle, triangle, etc)
*******************************************************************/
int JLP_SYMBOL2(float *x, float *y, float *size, int *isymb, int *idv1)
{
int status = -1, isize; 

 if(GDev_from_idv(*idv1)) {
/* Internally multiplied by 20 in jlp_symbol (mgo coordinates)
* so I divide by 20:
*/
   isize = (int)((float)*size * GDev_from_idv(*idv1)->Jgc0_fsx() / 20.);
   status = GDev_from_idv(*idv1)->symbol1(*x, *y, isize, *isymb);
   }

return(status);
}
/*******************************************************************
* INPUT:
* x, y: location in user coordinates (assuming linear scale) 
* errx: value of the x error 
* size: size of small segment at the end of the error bars (mgo coord) 
*******************************************************************/
int JLP_SYMBOL_ERRORX1(float *x, float *y, float *errx, int *size, int *idv1)
{
int status = -1;

 if(GDev_from_idv(*idv1)) {
   status = GDev_from_idv(*idv1)->symbol_errorx1(*x, *y, *errx, *size);
   }

return(status);
}
/*******************************************************************
* INPUT:
* x, y: location in user coordinates (assuming linear scale) 
* erry: value of the y error 
* size: size of small segment at the end of the error bars (mgo coord) 
*******************************************************************/
int JLP_SYMBOL_ERRORY1(float *x, float *y, float *erry, int *size, int *idv1)
{
int status = -1;

 if(GDev_from_idv(*idv1)) {
   status = GDev_from_idv(*idv1)->symbol_errory1(*x, *y, *erry, *size);
   }

return(status);
}
/*******************************************************************
* INPUT:
* x, y: location in user coordinates (assuming linear scale)
* diam: diameter (user coordinates)
*******************************************************************/
int JLP_CIRCLE1(float *x, float *y, float *diam, int *idv1)
{
int status = -1, ifill = 0;

 if(GDev_from_idv(*idv1)) {
   status = GDev_from_idv(*idv1)->circle1(*x, *y, *diam, ifill);
   }

return(status);
}
