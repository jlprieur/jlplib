/**************************************************************************
* "jlp_gdev_curves_draw.cpp"
*
* Definition of the members of the JLP_GDev class
* to draw curves
*
* JLP
* Version 10/02/2017
**************************************************************************/
#include <stdio.h>
#include <math.h>
#include <string.h>  // strcpy()
#include <ctype.h>   // isprint()

#include "jlp_gdev.h"
#include "jlp_macros.h"    // MINI, MAXI, etc

/* Contained here
int draw_curve(double *xplot, double *yplot, double *errx, double *erry, 
               int npts0, char *nchar00, char *pcolor0, int error_bars0)
int draw_curved_line(double *x, double *y, int npts, int ltype,
                     char *pcolor) 
int draw_mgo_chopper(int ixa, int iya, int ixb, int iyb, 
                     int ltype, int ldef)
*/
/* *********************************************************************
* draw_mgo_chopper.c 
* to chop a curve (dotted, dashed, etc).
***********************************************************************/

#define DOT_ON    100
#define DOT_OFF   200
#define SHORT_ON  320
#define SHORT_OFF 320
#define LONG_ON   800
#define LONG_OFF  320
#define LDEF 16                         /* SCREEN width of an lwidth 1 line */

#define EPS 1e-2

/*****************************************************************************
* FUNCTION: draw_curve 
*
* PURPOSE: To plot a set of locations stored in the arrays (xplot,yplot)
*          with lines, symbols or histogram-like format (i.e., L1, 42, H, ...)
*
* INPUT:  
*  xplot[] = double array with X user coordinates
*  yplot[] = double array with Y user coordinates
*  npts0 = number of points to draw
*  nchar   = type of character (L0= solid line, L1= dashed,
*            H0=histogram with solid line, 
*            H1= histogram with dashed line)
*  pcolor0 = color to be used (Purple, Aquamarine, ...)
*  error_bars = Flag (1 if error bars,  0 otherwise)
*
****************************************************************************/
int JLP_GDev::draw_curve(double *xplot, double *yplot, double *errx, 
                         double *erry, int npts0, char *nchar0, 
                         char *pcolor0, int error_bars0)
{
double box_xmin0, box_xmax0, box_ymin0, box_ymax0;
char ctest[2], default_color[40];
double min_xval, max_xval, min_yval, max_yval;
int i, isymb, isize;

box_xmin0 = Jgc0_box_xmin();
box_xmax0 = Jgc0_box_xmax();
box_ymin0 = Jgc0_box_ymin();
box_ymax0 = Jgc0_box_ymax();

if (box_xmin0 == box_xmax0 || box_ymin0 == box_ymax0) {
  fprintf(stderr,"JLP_GDev::draw_curve/Fatal error range in x or y is null! \n");
  fprintf(stderr,"box_xmin=%f box_xmax=%f box_ymin=%f box_ymax=%f \n", 
          box_xmin0, box_xmax0, box_ymin0, box_ymax0);
  return(-1);
}

/* Return if no points to be plotted: */
if(npts0 <= 0) {
  fprintf(stderr, "draw_curve/Error: npts0=%d\n", npts0);
  return(-1);
  }

/* Begin to plot: */
if(nchar0[0] == 'L' || nchar0[0] == 'l') {
  draw_curve_line(xplot, yplot, npts0, nchar0, pcolor0);
 } else if(nchar0[0] == 'H' || nchar0[0] == 'h') {
  draw_curve_histo(xplot, yplot, npts0, nchar0, pcolor0);
 } else {

/* Decoding the symbol and its size: */
   ctest[0] = nchar0[0]; ctest[1] = '\0';
   isymb = atoi(ctest);
   ctest[0] = nchar0[1]; ctest[1] = '\0';
   isize = atoi(ctest);
   ctest[0] = nchar0[2]; ctest[1] = '\0';
   isize = atoi(ctest) + isize * 10;
/* If null size, draw a tiny point: */
   if(isize == 0) isymb = 1;
   SetPColor(pcolor0);

/* To handle the cases when box_xmin > box_xmax: (JLP2007) */
   if(box_xmin0 <= box_xmax0) {
     min_xval = box_xmin0; max_xval = box_xmax0;
     } else {
     min_xval = box_xmax0; max_xval = box_xmin0;
     }
   if(box_ymin0 <= box_ymax0) {
     min_yval = box_ymin0; max_yval = box_ymax0;
     } else {
     min_yval = box_ymax0; max_yval = box_ymin0;
     }

/* Drawing the points
* JLP2002: do not draw the symbols on the axes
*/
   for(i=0; i < npts0; ++i) {
    if(xplot[i] >= min_xval && xplot[i] <= max_xval
       && yplot[i] >= min_yval && yplot[i] <= max_yval) {
          symbol1(xplot[i], yplot[i], isize, isymb);
          }
    }

/* Drawing the error bars */
   if(error_bars0 == 1) 
   {
    for(i=0; i<npts0; ++i) 
       if(xplot[i] >= min_xval && xplot[i] <= max_xval
          && yplot[i] >= min_yval && yplot[i] <= max_yval) {
           symbol_errorx1(xplot[i], yplot[i], errx[i], isize);
           symbol_errory1(xplot[i], yplot[i], erry[i], isize);
           }
   }
}

// Back to default options:
  strcpy(default_color, "Default");
  SetPColor(default_color);

return(0);
}
/*****************************************************************************
* FUNCTION: draw_curve_line 
*
* PURPOSE:
*
* INPUT:  
*  xplot0[] = double array with X user coordinates
*  yplot0[] = double array with Y user coordinates
*  npts0    = number of points to draw
*  nchar0   = type of character (L0= solid line, L1= dashed)
*  pcolor0  = color to be used for drawing the lines ("Default", "Red", etc)
*
****************************************************************************/
int JLP_GDev::draw_curve_line(double *xplot0, double *yplot0, int npts0, 
                              char *nchar0, char *pcolor0)
{
double box_xmin0, box_xmax0, box_ymin0, box_ymax0;
int status = -1, ltype = 0;

box_xmin0 = Jgc0_box_xmin();
box_xmax0 = Jgc0_box_xmax();
box_ymin0 = Jgc0_box_ymin();
box_ymax0 = Jgc0_box_ymax();

if (box_xmin0 == box_xmax0 
    || box_ymin0 == box_ymax0) {
  fprintf(stderr,"draw_curve_line/Error: range in x or y is null! \n");
  return(-1);
 }

/* Return if no points to be plotted: */
if(npts0 <= 0) {
  fprintf(stderr,"draw_curve_line/Error: npts0=%d\n", npts0);
  return(-1);
  }

/* Begins to plot: */
if(nchar0[0] == 'L' || nchar0[0] == 'l')
{
/* ltype = 0 for solid,  = 1 for dashed curves: */
   ltype = atoi(nchar0+1);

/* Connecting the points */
   draw_dashed_line(xplot0, yplot0, npts0, ltype, pcolor0);
   status = 0;
}
else {
   fprintf(stderr,"PLOT_CURVE_LINE/Error: bad line type, nchar0[0] = %c\n", 
           nchar0[0]);
   status = -1;
}

return(status);
}
/*****************************************************************************
* FUNCTION: draw_curve_histo 
* To draw a histogram
*
* INPUT:  
*  xplot[] = double array with X user coordinates
*  yplot[] = double array with Y user coordinates
*  npts0 = number of points to draw
*  nchar0   = type of character (H0=Solid black line, H1= Dashed black line, 
*  pcolor  = color to be used: Default (no filling), Aquamarine, Purple)
*
****************************************************************************/
int JLP_GDev::draw_curve_histo(double *xplot, double *yplot, int npts0, 
                               char *nchar0, char *pcolor0)
{
double box_xmin0, box_xmax0, box_ymin0, box_ymax0;
double x0, x1, y0, y1, min_xval, max_xval, min_yval, max_yval;
char nchar1[4], pcolor[32];
int status = -1, lwidth = 0, ltype = 0, old_ltype = 0, coloured_histo = 0; 
int i;

box_xmin0 = Jgc0_box_xmin();
box_xmax0 = Jgc0_box_xmax();
box_ymin0 = Jgc0_box_ymin();
box_ymax0 = Jgc0_box_ymax();

/* Transfer (for fortran interface) */
for(i = 0; i < MINI(4, (int)strlen(nchar0)) 
    && nchar0[i] && isprint(nchar0[i]); i++) nchar1[i] = nchar0[i];
nchar0[i] = '\0';

for(i = 0; i < MINI(32, (int)strlen(pcolor0)) 
    && pcolor0[i] && isprint(pcolor0[i]); i++) pcolor[i] = pcolor0[i];
pcolor[i] = '\0';
coloured_histo = (strncmp(pcolor0,"Default",7) == 0) ? 0 : 1;

if (box_xmin0 == box_xmax0 
    || box_ymin0 == box_ymax0) {
  fprintf(stderr,"draw_curve_histo/Error: range in x or y is null! \n");
  return(-1);
 }

/* Return if no points to be plotted: */
if(npts0 <= 0) {
  fprintf(stderr,"draw_curve_histo/Error: npts0=%d\n", npts0);
  return(-1);
  }

min_xval = box_xmin0;
max_xval = box_xmax0;
min_yval = box_ymin0;
max_yval = box_ymax0;
/* To handle cases when box_min > box_max: (JLP2007) */
if(min_xval > max_xval) {
   min_xval = max_xval; max_xval = box_xmin0;
   }
if(min_yval > max_yval) {
   min_yval = max_yval; max_yval = box_ymin0;
   }

/* Begins to plot: */
if(nchar0[0] == 'H' || nchar0[0] == 'h')
{

/* ltype = 0 for solid,  = 1 for dashed curves: 
*/
   ltype = atoi(nchar0+1);
   if(ltype >= 1 && ltype <= 4) {
     lwidth = Mgc0.lwidth;
     old_ltype = Mgc0.lltype;
     printf("draw_curve_histo/Setting lwidth=%d ltype=%d\n", lwidth, ltype);
     SetLineWidthAndType(lwidth, ltype);
   }

/* Connecting the points */

/* Loop on all the points: */
   for(i = 0; i < npts0 - 1; i++) {
      x0 = MINI(MAXI(xplot[i], min_xval), max_xval);
      x1 = MINI(MAXI(xplot[i+1], min_xval), max_xval);
      y1 = MINI(MAXI(yplot[i], min_yval), max_yval);
      if(coloured_histo) gdev_FilledRect1(x0, 0., x1, y1, pcolor); 
/* Draw the line after the rectangle in order to see it (to be superimposed)*/
      gdev_line1(x0, y0, x0, y1); 
      gdev_line1(x0, y1, x1, y1); 
      y0 = y1;
     }
/* Last point: */
    i = npts0 - 1;
    x0 = MINI(MAXI(xplot[i], min_xval), max_xval);
    x1 = MINI(MAXI(xplot[i] + (xplot[i] - xplot[i-1]), min_xval), max_xval);
    y1 = MINI(MAXI(yplot[i], min_yval), max_yval);
    if(coloured_histo) gdev_FilledRect1(x0, 0., x1, y1, pcolor); 
/* Draw the line after the rectangle in order to see it */
    gdev_line1(x0, y0, x0, y1); 
    gdev_line1(x0, y1, x1, y1); 
    gdev_line1(x1, y1, x1, 0.); 

/* Restore initial settings: */
   if(ltype >= 1 && ltype <= 4)
     SetLineWidthAndType(lwidth, old_ltype);
}
else {
   fprintf(stderr,"draw_curve_histo/Error: bad line type, nchar0[0] = %c\n", 
           nchar0[0]);
   status = -1;
}

return(status);
}
/***************************************************************************
* Draw dashed lines with mgo
* (When there is no possiblity of drawing with  
* dashed lines)
* NB: it would be better to use implemented facilities for postscript or X11
***************************************************************************/
int JLP_GDev::draw_dashed_line(double *xplot, double *yplot, int npts0, 
                               int ltype0, char *pcolor0)
{
int i;
double xx, yy, min_xval, min_yval, max_xval, max_yval;
int ixa, ixb, iya, iyb;
char old_color[40];

// Set new color: 
 GetCurrentPColor(old_color);
 SetPColor(pcolor0);

/* To handle cases when box_xmin is larger than box_xmax: (JLP2007)
*/
  if(Jgc0.axis_limits[0] <= Jgc0.axis_limits[1]) {
    min_xval = Jgc0.axis_limits[0]; max_xval = Jgc0.axis_limits[1]; 
    } else {
    min_xval = Jgc0.axis_limits[1]; max_xval = Jgc0.axis_limits[0]; 
    }

  if(Jgc0.axis_limits[2] <= Jgc0.axis_limits[3]) {
    min_yval = Jgc0.axis_limits[2]; max_yval = Jgc0.axis_limits[3]; 
    } else {
    min_yval = Jgc0.axis_limits[3]; max_yval = Jgc0.axis_limits[2]; 
    }

/* Starting point: */
  xx = MINI(MAXI(xplot[0], min_xval), max_xval);
  yy = MINI(MAXI(yplot[0], min_yval), max_yval);
  conv_user_to_mgo(xx, yy, &ixa, &iya);

/* Loop on all points: */
  for (i = 1; i < npts0; i++) {
    xx = MINI(MAXI(xplot[i], min_xval), max_xval);
    yy = MINI(MAXI(yplot[i], min_yval), max_yval);
    conv_user_to_mgo(xx, yy, &ixb, &iyb);
// Debug:    
//    gdev_line(ixa, iya, ixb, iyb);
    draw_mgo_chopper(ixa, iya, ixb, iyb, ltype0, Mgc0.ldef);
    ixa = ixb;
    iya = iyb;
  }

// Go back to previous color:
  SetPColor(old_color);

return(0);
}
/*********************************************************************
* From Mongo mgo_chopper
* Draw a dashed curve between (ixa,iya) to (ixb,iyb)
*
* INPUT:
* ixa, iya, ixb, iyb: start and end location in mgo coordinates
* int   ldef:  scale spacing for lwidth       
* int ltype: line type (0=solid, 1=dashed, etc)
*********************************************************************/
int JLP_GDev::draw_mgo_chopper(int ixa, int iya, int ixb, int iyb, 
                           int ltype, int ldef)
{
 double dist, dtogo, frac;
 double step;			/* step when drawing thick lines */
 double g_dx, g_dy;
 static double totdist = 0;	/* total distance drawn, so ltype cts at pts */
 int d, i, j, istart = 0, index, lw1, lw2, ix1, ix2, iy1, iy2;
 int onofflen, start = 0, xc, yc, xd, yd;

/* With some compilers:
   static int onoff[6][4] = { DOT_ON, DOT_OFF, DOT_ON, DOT_OFF,
                              SHORT_ON, SHORT_OFF, SHORT_ON, SHORT_OFF,
                              LONG_ON, LONG_OFF, LONG_ON, LONG_OFF,
                              SHORT_ON, SHORT_OFF, DOT_ON, DOT_OFF,
                              LONG_ON, LONG_OFF, DOT_ON, DOT_OFF,
                              LONG_ON, LONG_OFF, SHORT_ON, SHORT_OFF };
With gcc:
*/
   static int onoff[6][4] = { {DOT_ON, DOT_OFF, DOT_ON, DOT_OFF},
                              {SHORT_ON, SHORT_OFF, SHORT_ON, SHORT_OFF},
                              {LONG_ON, LONG_OFF, LONG_ON, LONG_OFF},
                              {SHORT_ON, SHORT_OFF, DOT_ON, DOT_OFF},
                              {LONG_ON, LONG_OFF, DOT_ON, DOT_OFF},
                              {LONG_ON, LONG_OFF, SHORT_ON, SHORT_OFF} };
   dist = 0.0;
   index = abs(ltype) - 1;
   g_dx = (double)Jgc0.dev_width / (double)SCREEN_SIZE;
   g_dy = (double)Jgc0.dev_height / (double)SCREEN_SIZE;


   if(ltype != 0) {
      onofflen = (int) (Mgc0.eexpand * (onoff[index][0] + onoff[index][1] +
 			                onoff[index][2] + onoff[index][3]));
      start = (int)totdist % onofflen;
      for(istart = 0;istart < 4;istart++) {
	 if(start < (int) (Mgc0.eexpand*onoff[index][istart])) break;
         start -= (int) (Mgc0.eexpand*onoff[index][istart]);
      }
   }

   if(abs(iyb-iya) > abs(ixb-ixa) ) {	/* thick lines drawn side by side */
      step = MINI((double)(ldef/2), 1.0 / g_dx);
   } else {				/* thick lines one above the other */
      step = MINI((double)(ldef/2), 1.0 / g_dy);
   }
 
   if(Mgc0.lwidth > 0) {
      lw1 = -(int)(LDEF*(Mgc0.lwidth - 1)/(2*step));
      lw2 = (int)(LDEF*Mgc0.lwidth/(2*step));
   } else {
      lw1 = lw2 = 1;
   }

   for(j = lw1;j <= lw2;j++) {
      if(iya == iyb && ixa == ixb) { 
         ix1 = MAXI( MINI( ixa+(int)(lw1*ldef/2), SCREEN_SIZE), 0);
         ix2 = MAXI( MINI( ixb+(int)(lw2*ldef/2), SCREEN_SIZE), 0);
         iy1 = MAXI( MINI( (int)(iya+j*step + 0.5), SCREEN_SIZE), 0);
         iy2 = MAXI( MINI( (int)(iyb+j*step + 0.5), SCREEN_SIZE), 0);
      } else if (abs(iyb-iya) > abs(ixb-ixa) ) {
         ix1 = MAXI( MINI((int)(ixa+j*step + 0.5), SCREEN_SIZE), 0);
         iy1 = iya;
         ix2 = MAXI( MINI((int)(ixb+j*step + 0.5), SCREEN_SIZE), 0);
         iy2 = iyb;
      } else {
         ix1 = ixa;
         iy1 = MAXI( MINI((int)(iya+j*step + 0.5), SCREEN_SIZE), 0);
         ix2 = ixb;
         iy2 = MAXI( MINI((int)(iyb+j*step + 0.5), SCREEN_SIZE), 0);
      }

      if (ltype == 0) {
	    gdev_line(ix1,iy1,ix2,iy2);
      } else {
         dist = hypot( (double)(ix2-ix1), (double)(iy2-iy1) );
         if(dist == 0) {
 	    gdev_line(ix1,iy1,ix2,iy2);
         } else {
            i = istart;
 	    d = (int)(Mgc0.eexpand*onoff[index][i++ % 4]) - start;
            dtogo = dist;
 	    if(istart%2 == 0) {			/* start with black segment */
               xc = ix1;
               yc = iy1;
 	    } else {				/* start with blank segment */
	       if(d > (int)dtogo || dist < EPS) {
		  continue;
	       }
 	       frac = d/dtogo;
               dtogo -= d;
               xc = (int)(ix1 + frac*(ix2 - ix1));
               yc = (int)(iy1 + frac*(iy2 - iy1));
               d = (int)(Mgc0.eexpand*onoff[index][i++%4]); 
 	    }
 
            for(;;) {
	       if(d > dtogo) {
 	       	  d = (int)dtogo;
 	       }
	       if(dtogo < EPS) {
		  if(dtogo > 0)
		    gdev_line(xc,yc,ix2,iy2);
		  break;
	       }
 	       frac = d/dtogo;
               xd = (int)(xc + frac*(ix2 - xc) + 0.5);
               yd = (int)(yc + frac*(iy2 - yc) + 0.5);
 
 	       gdev_line(xc,yc,xd,yd);
 
	       dtogo -= d;
               d = (int)(Mgc0.eexpand*onoff[index][i++ % 4]);
	       if(dtogo <= EPS) {
	       	  break;
	       }
 	       frac = d/dtogo;
               xc = (int)(xd + frac*(ix2 - xd) + 0.5);
               yc = (int)(yd + frac*(iy2 - yd) + 0.5);
 
	       dtogo -= d;
               d = (int)(Mgc0.eexpand*onoff[index][i++ % 4]); 
            }
         }         
      } 
   }
   totdist += dist;

return(0);
}
