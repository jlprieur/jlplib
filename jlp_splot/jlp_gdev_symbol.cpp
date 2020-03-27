/***************************************************************
* JLP_GDev class:
* definition of the symbol routines  
*
* Contains:
*   symbol1_errorx(x,y,errx,size)
*   symbol1_errory(x,y,erry,size)
*   symbol(x,y,isize,isymb)
*   symbol1(x,y,isize,isymb)
*   circle1(x,y,diam,ifill)
*
* VERSION: 05/04/2007 
*
* AUTHOR: JLP 
***************************************************************/
#include <stdio.h>
#include <math.h>
#include "jlp_gdev.h"
#include "jlp_macros.h"   // NINT ...

/*****************************************************************
* Routine to draw error bars in Y  (lenth=2*erry)
*
* INPUT:
*   x,y coordinates (user coord.)
*   size: size of the small line at the ends of the bar (mgo coord)
*****************************************************************/
int JLP_GDev::symbol_errory1(double x, double y, double erry, int size)
{
double y_low, y_high;
int ix, ix_right, ix_left, iy_low, iy_high;

  y_low = MINI(MAXI(y - erry, Jgc0.axis_limits[2]), Jgc0.axis_limits[3]);
  y_high = MINI(MAXI(y + erry, Jgc0.axis_limits[2]), Jgc0.axis_limits[3]);

/* Conversion to mgo coordinates: */
 conv_user_to_mgo(x, y_low, &ix, &iy_low);
 conv_user_to_mgo(x, y_high, &ix, &iy_high);

/* Drawing the vertical line with two small horizontal lines at the end
*/
  ix_right = MAXI(ix + (20 * size)/2, Jgc0.offx);
  ix_left = MINI(ix - (20 * size)/2, Jgc0.offx + Jgc0.axlen);
  gdev_line(ix_left, iy_low, ix_right, iy_low);
  gdev_line(ix, iy_low, ix, iy_high);
  gdev_line(ix_left, iy_high, ix_right, iy_high);

return(0);
}
/*****************************************************************
* Routine to draw error bars in X  (length=2*errx)
*
* INPUT:
*   x,y coordinates (user coord.)
*   size: size of the small line at the ends of the bar (mgo coord)
*****************************************************************/
int JLP_GDev::symbol_errorx1(double x, double y, double errx, int size)
{
double x_left, x_right;
int iy, ix_right, ix_left, iy_low, iy_high;

  x_left = x - errx;
  x_right = x + errx;

/* Conversion to mgo coordinates: */
 conv_user_to_mgo(x_left, y, &ix_left, &iy);
 conv_user_to_mgo(x_right, y, &ix_right, &iy);

/* Drawing the horizontal line with two small vertical lines at the end
*/
  iy_high = iy + (20 * size)/2;
  iy_low = iy - (20 * size)/2;
  gdev_line(ix_left, iy, ix_right, iy);
  gdev_line(ix_left, iy_low, ix_left, iy_high);
  gdev_line(ix_right, iy_low, ix_right, iy_high);

return(0);
}
/************************************************************
* Routine to draw symbols on x,y 
*
* INPUT:
*   x,y coordinates (SCREEN coord.)
*   isize between 1 and 9  if called by NEWPLOT2 (multiplied internally by 20)
*   isymb: type of symbol:
*     0: histogram
*     1: a tiny point
*     2: white triangle (base down)
*     3: black triangle (base down)
*     4: cross +
*     5: cross X
*     6: open square
*     7: filled square
*     8: open circle with a diameter (20 * size)
*     9: filled circle with a diameter (20 * isize)
*     12: white triangle (base up)
*     13: black triangle (base up)
*
* JLP2007: A problem was found with xterm device  when isize<=10... 
***************************************************************/
int JLP_GDev::symbol(int ix, int iy, int isize, int isymb)
{
register int i, j;
double aspect1, expand1;
double angle; 
/* JLP2007 go to double to improve accuracy of small symbols: */
double cos1[21], sin1[21], size0_x, size0_y, size1_x, size1_y; 
int xleft, ylow, xright, yhigh;
int status;


/* JLP96: I add the possibility of saving this call into a file
* for further hardcopy... */
if(Jgc0.fp_backup != NULL)
      fprintf(Jgc0.fp_backup,"symbol %d %d %d %d\n",
                        ix,iy,isize,isymb); 

 size0_x = 20. * isize;
/* JLP97: modif to get squares and circles even if the 
* output format is a rectangle...
* aspect1 = height/width */
 aspect1 = (double)Jgc0.dev_height / (double)Jgc0.dev_width;     
 size0_y = size0_x / aspect1;

 switch (isymb)
 {
/* 0=histogram: */
   case 0:
   {
    gdev_line(ix, Jgc0.offy, ix, iy);
    break;
   }
/* 1=tiny point: */
   case 1:
   {
    gdev_line(ix, iy, ix, iy+1);
    break;
   }
/* 2=white triangle (base down) */
   case 2:
   {
    expand1 = size0_x / 200.;
/* polygon_device(x, y, expand, angle, nsides, filled) */
    status = polygon_device(ix, iy, expand1, 0., 3, 0);
/* When no polygon capability: */
    if(status) {
      ylow = NINT(iy - size0_y/2.);
      yhigh = NINT(iy + size0_y/2.);
      xleft = NINT(ix - size0_x/2.);
      xright = NINT(ix + size0_x/2.);
      gdev_line(xleft, ylow, xright, ylow);
      gdev_line(xleft, ylow, ix, yhigh);
      gdev_line(ix, yhigh, xright, ylow);
    }
    break;
   }
/* 3=black triangle (base down) */
   case 3:
   {
    expand1 = size0_x / 200.;
/* polygon_device(x, y, expand, angle, nsides, filled) */
    status = polygon_device(ix, iy, expand1, 0., 3, 1);
/* When no polygon capability: */
    if(status)
    {
      for(i=1; i<40; i++)
      {
      size1_x = (size0_x * (double)i)/40.;
      size1_y = size1_x / aspect1; 
      ylow = NINT(iy - size1_y/2.);
      yhigh = NINT(iy + size1_y/2.);
      xleft = NINT(ix - size1_x/2.);
      xright = NINT(ix + size1_x/2.);
      gdev_line(xleft, ylow, xright, ylow);
      gdev_line(xleft, ylow, ix, yhigh);
      gdev_line(ix, yhigh, xright, ylow);
      }
    }
    break;
   }
/* 4=cross like + */
   case 4:
   {
    ylow = NINT(iy - size0_y/2.);
    yhigh = NINT(iy + size0_y/2.);
    xleft = NINT(ix - size0_x/2.);
    xright = NINT(ix + size0_x/2.);
    gdev_line(xleft, iy, xright, iy);
    gdev_line(ix, ylow, ix, yhigh);
    break;
   }
/* 5=cross like X */
   default:
   case 5:
   {
    ylow = NINT(iy - size0_y/2.);
    yhigh = NINT(iy + size0_y/2.);
    xleft = NINT(ix - size0_x/2.);
    xright = NINT(ix + size0_x/2.);
    gdev_line(xleft, ylow, xright, yhigh);
    gdev_line(xleft, yhigh, xright, ylow);
    break;
   }
/* 6=white square */
   case 6:
   {
    expand1 = size0_x / 200.;
    status = polygon_device(ix, iy, expand1, 0., 4, 0);

/* When no polygon capability: */
    if(status) {
      ylow = NINT(iy - size0_y/2.);
      yhigh = NINT(iy + size0_y/2.);
      xleft = NINT(ix - size0_x/2.);
      xright = NINT(ix + size0_x/2.);
      gdev_line(xleft, ylow, xright, ylow);
      gdev_line(xright, ylow, xright, yhigh);
      gdev_line(xright, yhigh, xleft, yhigh);
      gdev_line(xleft, yhigh, xleft, ylow);
      }
    break;
   }
/* 7=black square */
   case 7:
   {
    expand1 = size0_x / 200.;
    status = polygon_device(ix, iy, expand1, 0., 4, 1);

/* When no polygon capability: */
    if(status) {
    for(i=1; i<40; i++)
    {
    size1_x = size0_x * (double)i/40.;
    size1_y = size1_x / aspect1;
    ylow = NINT(iy - size1_y/2.);
    yhigh = NINT(iy + size1_y/2.);
    xleft = NINT(ix - size1_x/2.);
    xright = NINT(ix + size1_x/2.);
    gdev_line(xleft, ylow, xright, ylow);
    gdev_line(xright, ylow, xright, yhigh);
    gdev_line(xright, yhigh, xleft, yhigh);
    gdev_line(xleft, yhigh, xleft, ylow);
    }
    }
    break;
   }
/* 8=open circle */
   case 8:
   {
/* circle(ix, iy, idiam, ifill) */
    status = circle_device(ix, iy, (int)size0_x, 0);

/* When circle function is not available: */
    if(status) {
      for(i=0; i<=20; i++) {
        angle = (double)i * PI / 10.;
        cos1[i] = cos(angle); sin1[i] = sin(angle);
      }
      for(i=1; i<=20; i++) {
        ylow = iy + NINT(size0_y * sin1[i-1] / 2.);
        yhigh = iy + NINT(size0_y * sin1[i] / 2.);
        xleft = ix + NINT(size0_x * cos1[i-1] / 2.);
        xright = ix + NINT(size0_x * cos1[i] / 2.);
        gdev_line(xleft, ylow, xright, yhigh);
      }
    }
    break;
   }
/* 9=black circle */
   case 9:
   {
    status = circle_device(ix, iy, (int)size0_x, 1);

/* When circle function is not available: */
    if(status) {
    for(i=0; i<=20; i++) 
     {angle = (double)i * PI / 10.;
      cos1[i] = cos(angle); sin1[i] = sin(angle);
     }
    for(i=1; i<40; i++)
    {
     for(j=1; j<=20; j++) 
     {
     size1_x = (size0_x * (double)i)/40;
     size1_y = size1_x / aspect1;
     ylow = NINT(iy + size1_y * sin1[j-1] / 2.);
     yhigh = NINT(iy + size1_y * sin1[j] / 2.);
     xleft = NINT(ix + size1_x * cos1[j-1] / 2.);
     xright = NINT(ix + size1_x * cos1[j] / 2.);
     gdev_line(xleft, ylow, xright, yhigh);
     }
    }
    }
    break;
   }
/* 12=white triangle (base up) */
   case 12:
   {
    expand1 = size0_x / 200.;
    status = polygon_device(ix, iy, expand1, 60., 3, 0);
/* When no polygon capability: */
    if(status) {
      yhigh = NINT(iy + size0_y/2.);
      ylow = NINT(iy - size0_y/2.);
      xleft = NINT(ix - size0_x/2.);
      xright = NINT(ix + size0_x/2.);
      gdev_line(xleft, yhigh, xright, yhigh);
      gdev_line(xleft, yhigh, ix, ylow);
      gdev_line(ix, ylow, xright, yhigh);
      }
    break;
   }
/* 13=black triangle (base up) */
   case 13:
   {
    expand1 = size0_x / 200.;
    status = polygon_device(ix, iy, expand1, 60., 3, 1);
/* When no polygon capability: */
    if(status) {
      for(i=1; i<40; i++)
      {
      size1_x = (size0_x * (double)i)/40.;
      size1_y = size1_x / aspect1;
      ylow = NINT(iy - size1_y/2.);
      yhigh = NINT(iy + size1_y/2.);
      xleft = NINT(ix - size1_x/2.);
      xright = NINT(ix + size1_x/2.);
      gdev_line(xleft, yhigh, xright, yhigh);
      gdev_line(xleft, yhigh, ix, ylow);
      gdev_line(ix, ylow, xright, yhigh);
      }
    }
    break;
   }
 } /* end of switch */

return(0);
}
/*****************************************************************
* JLP_SYMBOL1
* Same as JLP_SYMBOL but with x and y in user coordinates
******************************************************************/
int JLP_GDev::symbol1(double x, double y, int isize, int isymb)
{
int status, ix, iy;

conv_user_to_mgo(x, y, &ix, &iy);
status = this->symbol(ix,iy,isize,isymb);

return(status);
}
/*****************************************************************
* circle1
*
* INPUT:
* x,y,diam: in  user coordinates
*
******************************************************************/
int JLP_GDev::circle1(double x, double y, double diam, int ifill)
{
int isymbol, isize, status;
double tdx;

/* Internally multiplied by 20 in "symbol1"
*/
 tdx = Jgc0.axlen / (Jgc0.axis_limits[1] - Jgc0.axis_limits[0]);
 isize = (int) ((tdx * diam) / 20.);

/* With ISYMBOL = 8, draw an open circle with a diameter of 20 * isize
*/
 if(ifill) isymbol = 9;
  else isymbol = 8;

 status = this->symbol1(x, y, isize, isymbol);

return(status);
}
