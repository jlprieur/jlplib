/***************************************************************
* jlp_lut1.cpp
* JLP_GDev class
* FUNCTION: Look-up-tables routines 
*
* lut_type: 'l'=log rainbow1 'r'=rainbow2 's'=saw 'g'=gray 'c'=curves
*           'p'=pisco
*
* Contains:
* convert_to_lut(image1,nx1,ny1,idim1,image2,nx2,ny2,idim2,ncolors)
* jlp_change_lut(lut_type, reversed, &ioff, &islope, ncolors)
* jlp_change_lut_full(lut_type, reversed, &ioff, &islope, ncolors, 
*                      max_lut_level)
* jlp_key(nn1,lower_itt,upper_itt,zlabel,horiz,axis_also,gamma1,gamma_d)
* int jlp_alloc_lut(int *ncolors, int *private_lut)
* int jlp_load_lut(int *r, int *g, int *b, int *ncolors)
* int jlp_reverse_lut()
*
* VERSION: 10/02/2017 
*
* AUTHOR: JLP 
***************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "jlp_macros.h"  // For LUT, MINI, MAXI, etc
#include "jlp_gdev.h"

/*
#define DEBUG
*/
static int rgb_lut_saw(const int ioff, const int islope, 
                       int *r, int *g, int *b,
                       const int ncolors, const int max_lut_level);
static int rgb_lut_gray(const int ioff, const int islope, 
                        int *r, int *g, int *b,
                        const int ncolors, const int max_lut_level);
static int rgb_lut_rainbow(const int ioff, const int islope, 
                           int *r, int *g, int *b,
                           const int ncolors, const int max_lut_level);
static int rgb_lut_log_rainbow(const int ioff, const int islope, 
                               int *r, int *g, int *b,
                               const int ncolors, const int max_lut_level);
static int rgb_lut_for_curves(const int ioff, const int islope, 
                              int *r, int *g, int *b,
                              const int ncolors, const int max_lut_level);
static int rgb_lut_pisco(int *r, int *g, int *b, const int ncolors, 
                         const int max_lut_level);

/***************************************************************
*
* FUNCTION: convert_to_lut 
*
* PURPOSE: Image sampling and conversion from double to int.
*          Affects to each value the LUT address
*          WARNING: nx2 should be always smaller than nx1!
*
* INPUT:  image1[nx1*ny1] = double image
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
int JLP_GDev::convert_to_lut(double *image1, int nx1, int ny1, int idim1,
                             int *image2, int nx2, int ny2, int idim2,
                             int ncolors, int itt_is_linear, 
                             double lower_itt, double upper_itt)
/***************************************************************/
{
int   i, j;
int   i1, j1, ivalue, imax, gamma1, gamma_d;	
double scale, range, xcolors, work;

xcolors = (double)ncolors;
range = upper_itt - lower_itt;
if(range == 0.) range = 1.;
imax = ncolors-1;

/* Reduction factor is 1/gamma1 (if image larger than 600, cf. Xdisp1.c) */
gamma1 = (int)((double)idim1 / (double)idim2); 
gamma_d = (int)(1. / (double)gamma1); 

#ifdef DEBUG
printf("CONVERT_TO_LUT/Debug: ncolors=%d range=%.2f imax=%d, gamma1=%d gamma_d=%d\n",
       ncolors, range, imax, gamma1, gamma_d);
#endif

if(gamma1 <= 0 ) {
 printf(" *** convert_to_lut/Fatal error: gamma1 = %d!\n",gamma1);
 printf(" *** idim1 = %d smaller than idim2 = %d !\n", idim1, idim2);
 exit(-1);
 }

/* Display a warning message in case of compression: */
if(gamma1 != 1) 
  printf("convert_to_lut/Compression with gamma1 = (nx1=%d / nx2=%d) %d\n",
         idim1, idim2, gamma1);

/* ramp (linear) */
  if(itt_is_linear) {

  scale = xcolors/range;
  for (j=0; j < ny2; j++) {
    j1 = j * gamma1;
/* Case of 1-D images: (artificially Xdisp1 set ny2 to 128...) */
    j1 = MINI(j1, ny1-1);
    for (i=0; i < nx2; i++) {
     i1 = i * gamma1;
     i1 = MINI(i1, nx1-1);
     ivalue = (int)((image1[i1 + j1 * idim1]- lower_itt)*scale);
     ivalue = MAXI(ivalue,0);
     ivalue = MINI(ivalue,imax);
     image2[i + j * idim2] = Jgc0_lut(ivalue);
    }
  }

/* log  */
  } else {

  for (j=0; j < ny2; j++) {
    j1 = j * gamma1;
/* Case of 1-D images: (artificially Xdisp1 set ny2 to 128...) */
    j1 = MINI(j1, ny1-1);
    for (i=0; i < nx2; i++) {
     i1 = i * gamma1;
     i1 = MINI(i1, nx1-1);
/* Converted into a value between 1 and 10: */
     work = 1. + 9.*(image1[i1 + j1 * idim1]- lower_itt)/range;
     work = MAXI(work,1.);
/* And then between 0. and ncolors-1: */
     ivalue = (int)(xcolors*log10(work));
     ivalue = MAXI(0, ivalue);
     ivalue = MINI(ivalue, imax);
     image2[i + j * idim2] = Jgc0_lut(ivalue);
    }
  }
 }

return(0);
}
/******************************************************************************
* Allocate LUT (calls alloc_lut_device()
*
* OUTPUT:
*  ncolors: number of colors successfully allocated
*  private_lut: set to one if private LUT is allowed (i.e., LUT can be changed)
*******************************************************************************/
int JLP_GDev::jlp_alloc_lut(int *ncolors, int *private_lut) 
{
  int status;
    if(*ncolors > GCOLOR_MAX) {
      fprintf(stderr,"jlp_alloc_lut/fatal error ncolors=%d (max=%d) !\n", 
              *ncolors, GCOLOR_MAX);
      exit(-1);
      }
    for(int i = 0; i < GCOLOR_MAX; i++)  Jgc0.gclr.lut[i] = 0;
    status =  this->alloc_lut_device(Jgc0.gclr.lut, private_lut, ncolors);
#ifdef DEBUG
       if(!private_lut) 
         printf("alloc_lut/No private lut: read-only color cells\n");
       else 
         printf("alloc_lut/Private lut: read-write color cells\n");
#endif
// ncolors can be changed in alloc_lut_device:
  Jgc0.gclr.ncolors = *ncolors;
return(status);
}
/******************************************************************************
* jlp_load_lut
* Loading (r,g,b) colors to read/write color cells previously allocated,
* and whose addresses are stored in Jgc0.clr.lut array:
* If LUT is not private i.e., read-only LUT, look for cells with nearest values
* In this case, the values of Jgc0.clr.lut will be modified
*
* INPUT:
* r, g, b: color values between 0 and 65535
*******************************************************************************/
int JLP_GDev::jlp_load_lut(const int *r, const int *g, const int *b, 
                           int ncolors)
{
int status = 0;
 if(ncolors > GCOLOR_MAX) {
    printf("load_lut/fatal error ncolors=%d (max = %d) !\n", 
           ncolors, GCOLOR_MAX);
    exit(-1);
    }
 Jgc0.gclr.ncolors = ncolors;
 for(int i = 0; i < ncolors; i++) {
   Jgc0.gclr.r[i] = r[i];
   Jgc0.gclr.g[i] = g[i];
   Jgc0.gclr.b[i] = b[i];
  }

// Load (r,g,b) values to cells whose addresses are stored 
// in Jgc0.gclr.lut array.
// Warning: the values of Jgc0.gclr.lut can be modified 
// by load_lut_device (if read-only LUT):
  status = this->load_lut_device(r,g,b,Jgc0.gclr.lut,ncolors);

return status;
}
/******************************************************************************
* jlp_reverse_lut
* Reverse LUT:
*******************************************************************************/
int JLP_GDev::jlp_reverse_lut() 
{
 int status = -1;
 if(Jgc0.gclr.ncolors <= 0 || Jgc0.gclr.ncolors > GCOLOR_MAX) {
    printf("reverse_lut/fatal error ncolors=%d (max= %d) !\n", 
            Jgc0.gclr.ncolors, GCOLOR_MAX);
    exit(-1); 
    }
// r, g, b: color values between 0 and 65535
 for(int i = 0; i < Jgc0.gclr.ncolors; i++) {
    Jgc0.gclr.r[i] = MaxColorLevelForLUT - Jgc0.gclr.r[i];
    Jgc0.gclr.g[i] = MaxColorLevelForLUT - Jgc0.gclr.g[i];
    Jgc0.gclr.b[i] = MaxColorLevelForLUT - Jgc0.gclr.b[i];
    }
// Load (r,g,b) values to cells whose addresses are stored 
// in Jgc0.gclr.lut array.
// Warning: the values of Jgc0.gclr.lut can be modified 
// by load_lut_device (if read-only LUT):
  status = this->load_lut_device(Jgc0.gclr.r,Jgc0.gclr.g,Jgc0.gclr.b,
                                 Jgc0.gclr.lut,Jgc0.gclr.ncolors);

return(status);
}
/****************************************************************
* Change the LUT 
* 
* INPUT:
* ncolors: number of colors
* reversed: 1 if reversed, 0 otherwise
*
* INPUT/OUTPUT:
* ioff: between 0 and ncolors
* islope: between 0 and ncolors
*    (ioff=ncolors/2, islope=ncolors/2 for standard scale)
*************************************************************/
int JLP_GDev::jlp_change_lut(char *lut_type, int reversed, int *ioff, 
                             int *islope, int ncolors)
{
 int status, max_lut_level;
 int *r, *g, *b;

 r = new int[ncolors];
 g = new int[ncolors];
 b = new int[ncolors];

 max_lut_level = GetMaxLevelForLUT();

 status = jlp_change_lut_full(lut_type, reversed, ioff, islope, r, g, b,
                              ncolors, max_lut_level);

/* Now loading this LUT, i.e. loading (r,g,b)(i) colors in read/write color 
* cells previously allocated. 
*/
 if(status == 0) {
   status = jlp_load_lut(r, g, b, ncolors);
   } else {
   fprintf(stderr,"jlp_change_lut/Error status=%d\n", status);
   }
/* JLP99, problem with read-only color cells (return -2 in that case) */

delete[] r;
delete[] g;
delete[] b;
return(status);
}
/****************************************************************
* Change the LUT 
* 
* INPUT:
* lut_type: 'l'=log rainbow1 'r'=rainbow2 's'=saw 'g'=gray 'c'=curves
*           'p'=pisco
* ncolors: number of colors in LUT array
* reversed: 1 if reversed, 0 otherwise
*
* OUTPUT:
* r,g,b: LUT arrays
*
* INPUT/OUTPUT:
* ioff: between 0 and ncolors
* islope: between 0 and ncolors
*    (ioff=ncolors/2, islope=ncolors/2 for standard scale)
*************************************************************/
int jlp_change_lut_full(char *lut_type, const int reversed, int *ioff,
                        int *islope, int *r, int *g, int *b, 
                        const int ncolors, const int max_lut_level)
{
int status = 0;
int i;

/* Erase LUT arrays : */
   for(i = 0; i < ncolors; i++) {
      r[i] = 0;
      g[i] = 0;
      b[i] = 0;
      }

 *islope = MAXI(0,*islope);
 *islope = MINI(ncolors,*islope);
 *ioff = MAXI(0,*ioff);
 *ioff = MINI(ncolors,*ioff);

switch (lut_type[0])
 {

/* "saw" in pseudo-gray scale: 
   r,g,b: up 
   with a log scale
*/
    case 's':
        {
        rgb_lut_saw(*ioff, *islope, r, g, b, ncolors, max_lut_level); 
	break;
	}

/* smooth "gray" (pseudo-gray)
   r=g=b: up 
   with a log scale (to create a "linear" visual scale) 
*/
    case 'g':
        {
        rgb_lut_gray(*ioff, *islope, r, g, b, ncolors, max_lut_level); 
	break;
	}

/**************************************************
   rainbow1
   log_rainbow:  "l" with log scale
   with log scale
   r: flat-up     _/ 
   g: up-down  /\
   b: down-flat    \_
   ioff=0, islope=ncolors for standard scale
*/
    case 'l':
         {
         rgb_lut_log_rainbow(*ioff,*islope,r,g,b,ncolors, max_lut_level);
	 break;
	 }

/**************************************************
   rainbow2: "r" rainbow... 
   with log scale and contrasted colors 
   Like log_rainbow but with beginning with deeper blue (nearly black)
   r: flat-up     _/ 
   g: up-down  /\
   b: down-flat    \_
*/
    case 'r':
        {
        rgb_lut_rainbow(*ioff,*islope,r,g,b,ncolors, max_lut_level);
	break;
        }
/**************************************************
   curves: for displaying curves with colors 
   same as rainbow, but set the background to white
   to reduce the use of ink
*/
    case 'c':
        {
        rgb_lut_for_curves(*ioff,*islope,r,g,b,ncolors, max_lut_level);
	break;
        }
/***************************************************
 pisco: color LUT initially designed for ICCD images of PISCO
*/
    default:
        fprintf(stderr,"jlp_change_lut_full/Error: option lut_type=%s< is unknown\n",
                       lut_type);
        status = 1;
        break;
    case 'p':
        {
        rgb_lut_pisco(r, g, b, ncolors, max_lut_level);
	break;
        }
/* EOF switch: */
     }

/**************************************************
*  Reversed lut:
*/
    if(reversed) {
     for(i = 0; i < ncolors; i++) {
         r[i] = max_lut_level - r[i];
         g[i] = max_lut_level - g[i];
         b[i] = max_lut_level - b[i];
        }
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
int JLP_GDev::jlp_key(int nn1, double lower_itt, double upper_itt, 
                      char *zlabel, int horiz, int axis_also, int gamma1, 
                      int gamma_d, double scale_x, double scale_y)
{
double expand=1.4, len1;
int nn2, ncolors, smallest_ix, smallest_iy;
int i, j;
int nx1, ny1, xstart1, ystart1, xend1, yend1;
int *array, black_and_white, ixlen, iylen;


/* height of the key is ny: */
 nn2 = MAXI(4, nn1/40);

/* Should be a multiple of 4: (required by JLP_X11::plot_image) */
 nn1 = (nn1/4)*4;
 nn2 = (nn2/4)*4;

if(horiz) {
 xstart1 = Jgc0_offx(); 
 ystart1 = (int)((double)Jgc0_offy()*0.4);
 nx1 = nn1;
 ny1 = nn2;
 } else {
 xstart1 = (int)((double)(Jgc0_offx()*2.2)) + Jgc0_axlen();
 ystart1 = Jgc0_offy(); 
 nx1 = nn2;
 ny1 = nn1;
 }

 array = new int[nx1 * ny1];

 ncolors = Jgc0_ncolors();
/* Generates the image: */
 if(horiz) {
  for(j = 0; j < ny1; j++) 
    for(i = 0; i < nx1; i++) 
      array[i + j * nx1] = Jgc0_lut(i * ncolors / nx1);
 } else {
  for(j = 0; j < ny1; j++)
    for(i = 0; i < nx1; i++)
      array[i + j * nx1] = Jgc0_lut(j * ncolors / ny1);
 } 

/* Displays it now: */
  black_and_white = 0;
  plot_image(array, nx1, ny1, nx1, xstart1, ystart1, gamma1, gamma_d, 
             black_and_white);

/* Free allocated memory: */
  delete[] array;

if(axis_also)
{
/* Compute upper limits: */
  ixlen = (int)(nx1 * scale_x); 
  iylen = (int)(ny1 * scale_y); 
  xend1 = ixlen + xstart1;
  yend1 = iylen + ystart1;

/* 4 sides: */
  gdev_line(xstart1,ystart1,xstart1,yend1);
  gdev_line(xstart1,yend1,xend1,yend1);
  gdev_line(xend1,yend1,xend1,ystart1);
  gdev_line(xend1,ystart1,xstart1,ystart1);

/* Draw legend */
  Set_ltype_and_lwidth(0,0);

  if(horiz) {
/*
*  jlp_axis(xorigin,yorigin,axis_length,
*           min_user,max_user,angle,label_flag,ticks_up,TeX_flag,expand)
*/
    jlp_axis(xstart1, ystart1, ixlen, lower_itt, upper_itt, 0., 1, 
             0, 0, expand, &smallest_ix, &smallest_iy);
/* 0. degrees, expand, 1 is drawit */
    len1 = gdev_label(zlabel, xstart1, ystart1, 90., expand, 0);
    xstart1 += (ixlen - (int)len1)/2;
    ystart1 -= (int)(Jgc0_offy()*0.5);
    gdev_label(zlabel,xstart1,ystart1,0.,expand,1);
  } else {
    jlp_axis(xstart1, ystart1, iylen, lower_itt, upper_itt, 90., 1, 
             1, 0, expand, &smallest_ix, &smallest_iy);
/* 90. degrees, expand, 1 is drawit */
    len1 = gdev_label(zlabel, xstart1, ystart1, 90., expand, 0);
    xstart1 -= (int)(Jgc0_offx()*0.5); 
    ystart1 += (iylen - (int)len1)/2;
    gdev_label(zlabel,xstart1,ystart1,90.,expand,1);
  }

}
return(0);
}
/************************************************************************
* "saw" in pseudo-gray scale: 
*   r,g,b: up 
*   with a log scale
*   with a log scale (to create a "linear" visual scale) 
*
* INPUT:
*  ioff: between 0 and ncolors
*  islope: between 0 and ncolors
*  ncolors: number of colors (i.e., size of r, g, b arrays)
*
* OUTPUT:
*  r[ncolors], g[ncolors], b[ncolors] 
************************************************************************/
static int rgb_lut_saw(const int ioff, const int islope, 
                       int *r, int *g, int *b, 
                       const int ncolors, const int max_lut_level)
{
int i;
int iwork, ival, slope, ioff1;
double val;

/* Subtract something to ioff, otherwise too dark: */
	 ioff1 = ioff - islope/8;
/* *islope varies between 0 and ncolors, so we rescale it between 0 and .9: */
	 slope = (int)(0.9 * islope);
	 for(i=0; i < ncolors; i++)
	   {ival = i - ioff1;
	    iwork = (int)((double)ival/slope);
	    if(ival < 0) iwork = iwork - 1;
	    ival = ival - (int)(iwork * slope);
	    val = MAXI(0,ival);
	    val = 1. +  val * 9. / slope;
	    ival = (int)(max_lut_level*log10(val));
/* For some light colors:
	    r[i] = MINI(ival,max_lut_level);
	    g[i] = 30000 + r[i] / 3; 
	    b[i] = 35000 + r[i] / 4; 
But I prefer B&W, for publications*/
	    r[i] = MINI(ival,max_lut_level);
	    g[i] = r[i]; 
	    b[i] = r[i]; 
	   }
return(0);
}
/************************************************************************
* smooth "gray" (pseudo-gray)
*   r=g=b: up 
*   with a log scale (to create a "linear" visual scale) 
*
* INPUT:
*  ioff: between 0 and ncolors
*  islope: between 0 and ncolors
*  ncolors: number of colors (i.e., size of r, g, b arrays)
*
* OUTPUT:
*  r[ncolors], g[ncolors], b[ncolors] 
************************************************************************/
static int rgb_lut_gray(const int ioff, const int islope, 
                        int *r, int *g, int *b, 
                        const int ncolors, const int max_lut_level)
{
int ival, ioff1;
double val, slope;
int i;

/* Subtract something to *ioff, otherwise too dark: */
	 ioff1 = ioff - islope/2;
/* *islope varies between 0 and ncolors, so we rescale it between 0 and 1.5: */
	 slope = 1.5 * islope;
	 for(i=0; i < ncolors; i++)
	   {val = MAXI(0,(i - ioff1));
	    val = 1. +  val * 9. / slope;
	    ival = (int)(max_lut_level*log10(val));
	    r[i] = MINI(ival,max_lut_level);
	    g[i] = r[i];
	    b[i] = r[i];
/* For some light colors:
	    g[i] = 10000 + r[i] / 3; 
	    b[i] = 20000 + r[i] / 2; 
*/
	   }
return(0);
}
/****************************************************************
* lut_log_rainbow (rainbow1)
* To create a RGB LUT 
* 
*  log_rainbow LUT:
*  with log scale and a small contrast
*  r: flat-up     _/  . 
*  g: up-down  /\ .  
*  b: down-flat    \_ 
*  ioff=0, islope=ncolors for standard scale
*
* INPUT:
*  ioff: between 0 and ncolors
*  islope: between 0 and ncolors
*  ncolors: number of colors (i.e., size of r, g, b arrays)
*
* OUTPUT:
*  r[ncolors], g[ncolors], b[ncolors] 
*************************************************************/
static int rgb_lut_log_rainbow(const int ioff, const int islope, 
                               int *r, int *g, int *b, 
                               const int ncolors, const int max_lut_level)
{
 int n2, ival, ioff1, islope1;
 double val, slope;
 int i;

 islope1 = MINI(ncolors,islope);
 ioff1 = MAXI(0,ioff);

/* Erases lut: */
 for(i=0; i<ncolors; i++)
     {r[i] = 0; g[i] = 0; b[i] = 0;}

  n2 = ncolors/2;
/* Subtract something to ioff, otherwise too dark: */
  ioff1 = (ioff1 - (ncolors * 3) / 5) / 2;
/* islope varies between 0 and ncolors, so we rescale it between 0 and 1.5: */
 slope = 1.5 * islope1;
 for(i=0; i<n2; i++)
   {val = MAXI(0,(i-ioff1));
    val = 1. +  val * 9. / slope;
    val = max_lut_level*log10(val);
    ival = MINI((int)val,max_lut_level);
    ival = MAXI(0, ival);
/* first half: */
    r[i] = 0;
    g[i] = ival;
/* Not too bad with: b[n2-i] = (int)((double)ival*0.8);
 */
    b[n2-i-1] = (int)((double)ival * 0.8);
/* Second half:  */
    r[i+n2] = ival;
    g[ncolors-i-1] = ival;
   }

/* Solving continuity problems: */
 for(i=0; i<n2; i++) {
    r[i] = r[n2];
    b[i+n2] = b[n2-1];
   }

return(0);
}
/****************************************************************
* jlp_lut_pisco
* To create a RGB LUT 
*
* "Pisco LUT"
* Create a palette for color scale bitmaps:
* B: down-zero
* V: up-down
* R: zero-up
* 
* INPUT:
*  ioff: between 0 and ncolors
*  islope: between 0 and ncolors
*  ncolors: number of colors (i.e., size of r, g, b arrays)
*
* OUTPUT:
*  r[ncolors], g[ncolors], b[ncolors] 
*************************************************************/
static int rgb_lut_pisco(int *r, int *g, int *b, 
                         const int ncolors, const int max_lut_level)
{
int i1, i2, i3, imax;
double rslope, gslope, bslope, ascale;
int i;

// Erases lut: 
 for(i = 0; i < ncolors; i++)
     {r[i] = 0; g[i] = 0; b[i] = 0;}

if(ncolors != 256){ 
   fprintf(stderr, "rgb_lut_pisco/Error: ncolors=%d != 256!\n", ncolors);
   exit(-1);
   }

imax = 256;
ascale = (double)max_lut_level / 256.;

// Red:
i1 = 126;
rslope = 130. * ascale / (double)i1;
      for (i = 0; i < i1; i++)
        r[i] = (int)(rslope * i);

rslope = 125. * ascale / (double)(imax - i1);
      for (i = i1; i < 256; i++)
        r[i] = (int)(ascale * 130. + rslope * (i - i1));

// Green:
i1 = 66;
i2 = 116;
i3 = 166;
gslope = 140 * ascale / (double)i1;
      for (i = 0; i < i1; i++)
        g[i] = (int)(gslope * i);

gslope = 80 * ascale / (double)(i2 - i1);
      for (i = i1; i < i2; i++)
        g[i] = (int)(ascale * 140. + gslope * (i - i1));

gslope = 80 * ascale / (double)(i3 - i2);
      for (i = i2; i < i3; i++)
        g[i] = (int)(ascale * 220. - gslope * (i - i2));

gslope = 140 * ascale / (double)(imax - i3);
      for (i = i3; i < imax; i++)
        g[i] = (int)(ascale * 140. - gslope * (i - i3));

// Blue:
// Starting with dark blue, increasing then decreasing:
i1 = 46;
i2 = 126;
i3 = 216;
bslope = 100. * ascale / (double)i1;
      for (i = 0; i < i1; i++)
        b[i]  = (int)(bslope * i + ascale * 100.);

bslope = 90 * ascale / (double)(i2 - i1);
      for (i = i1; i < i2; i++)
        b[i]  = (int)(ascale * 200. - bslope * (i - i1));

bslope = 110 * ascale / (double)(i3 - i2);
      for (i = i2; i < i3; i++)
        b[i]  = (int)(ascale * 110. - bslope * (i - i2));

      for (i = i3; i < imax; i++)
        b[i]  = 0;

return(0);
}
/****************************************************************
* rainbow2: lut_rainbow 
* To create a RGB LUT (rainbow2) 
*  with linear scale and a high contrast
*
*  Like log_rainbow but with beginning with deeper blue (nearly black)
*  and a higher contrast
*  r: flat-up     _/ . 
*  g: up-down  /\ . 
*  b: down-flat    \_
*  ioff=0, islope=ncolors for standard scale
*
* INPUT:
*  ioff: between 0 and ncolors
*  islope: between 0 and ncolors
*  ncolors: number of colors (i.e., size of r, g, b arrays)
*
* OUTPUT:
*  r[ncolors], g[ncolors], b[ncolors] 
*************************************************************/
static int rgb_lut_rainbow(const int ioff, const int islope, 
                           int *r, int *g, int *b, 
                           const int ncolors, const int max_lut_level)
{
 int n2, col_range, ival, ioff1, islope1, ix;
 double val, slope;
 int i;

 islope1 = MINI(ncolors,islope);
 ioff1 = MAXI(0,ioff);

/* Erases lut: */
 for(i=0; i<ncolors; i++)
     {r[i] = 0; g[i] = 0; b[i] = 0;}

  n2 = ncolors/2;
/* Subtract something to ioff, otherwise too dark: */
  ioff1 = (ioff1 - (ncolors * 3) / 5) / 2;
/* islope varies between 0 and ncolors, so we rescale it between 0 and 1.5: */
  slope = 1.5 * islope1;
  for(i=0; i<n2; i++)
    {val = MAXI(0,(i-ioff1));
     val = 1. +  val * 9. / slope;
     val = max_lut_level*log10(val);
     ival = MINI((int)val,max_lut_level);
     ival = MAXI(0, ival);
/* Second half:  */
     r[i+n2] = ival;
     }
/* First half: */
/* For continuity problems: */
  for(i=0; i<n2; i++) r[i] = r[n2]; 

/* JLP 93: new blue table (bump shorter /\__): */
  n2 = ncolors/2;
  col_range = ncolors/3;
/* First part (till col_range) */
  for(i = 0; i < col_range; i++)
   {val = MAXI(0,((i * n2) / col_range -ioff1));
    val = 1. +  (val * 9. / slope);
    val = max_lut_level*log10(val);
    ival = MINI((int)val,max_lut_level);
    ival = MAXI(0, ival);
    b[i] = ival;
   }
/* Second part */
 for(i = col_range; i < ncolors; i++)
   {val = MAXI(0,(((2*col_range - i) * n2) / col_range -ioff1));
    val = 1. +  (val * 9. / slope);
    val = max_lut_level*log10(val);
    ival = MINI((int)val,max_lut_level);
    ival = MAXI(0, ival);
    b[i] = ival;
   }

/* JLP 93: new green table (bump shorter /\): */
  n2 = ncolors/2;
  col_range = ncolors/3;
/* First part (till col_range) */
 for(i = 0; i < col_range; i++)
   {val = MAXI(0,((i * n2) / col_range -ioff1));
    val = 1. +  (val * 9. / slope);
    val = max_lut_level*log10(val);
    ival = MINI((int)val,max_lut_level);
    ival = MAXI(0, ival);
    ix = MAXI(0, (ncolors-1 - ncolors/8 -i));
    g[ix] = ival;
   }
/* Second part */
 for(i = col_range; i < ncolors; i++)
   {val = MAXI(0,(((2*col_range - i) * n2) / col_range -ioff1));
    val = 1. +  (val * 9. / slope);
    val = max_lut_level*log10(val);
    ival = MINI((int)val,max_lut_level);
    ival = MAXI(0, ival);
    ix = MAXI(0, (ncolors-1 - ncolors/8 -i));
    g[ix] = ival;
   }

return(0);
}
/****************************************************************
* For curves 
* To create a RGB LUT 
* 
* Like rainbow but background set to white,
* to reduce use of ink
*
* INPUT:
*  ioff: between 0 and ncolors
*  islope: between 0 and ncolors
*  ncolors: number of colors (i.e., size of r, g, b arrays)
*
* OUTPUT:
*  r[ncolors], g[ncolors], b[ncolors] 
*************************************************************/
static int rgb_lut_for_curves(const int ioff, const int islope, 
                              int *r, int *g, int *b, 
                              const int ncolors, const int max_lut_level)
{
 int col_range;
 int i;

rgb_lut_log_rainbow(ioff, islope, r, g, b, ncolors, max_lut_level);

// Remember: max_lut_level = 255
 col_range = 1;
 for(i = 0; i < col_range; i++) {
    r[i] = max_lut_level;
    g[i] = max_lut_level;
    b[i] = max_lut_level;
  }

return(0);
}
