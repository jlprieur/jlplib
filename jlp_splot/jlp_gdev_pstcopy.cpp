/*******************************************************************
* jlp_gdev_pstcopy.cpp
* Definition of some of the routines of the JLP_GDev_PST class
*
* Derived from Eric Anterrieu's XImage (Version 04-11-92)
*
* JLP
* Version 23-05-2006
******************************************************************/
#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <math.h>
#include "jlp_gdev_pst.h"       // JLP_GDev_PST class
#include "pst_set.h"
#include "jlp_time0.h"   // JLP_CTIME

/* Maximum size allowed for output (when high_resolution=0) : */
/* used in laser_compress */
#define LASER_MAX_SIZE 300 

static int define_image_axes(FILE *fp, int *nx, int *ny, double width, 
                             double height);
#ifdef OLD_VERSION
static int laser_border(FILE *fp, int *nx, int *ny, double width,
                        double height);
static int laser_compress2(int *image1, int nx1, int ny1, int idim,
                           int *comp_image, int comp_nx, int comp_ny,
                           int ifact);
#endif
static int laser_compress(int *image1, int nx1, int ny1, int idim,
                          int **comp_image, int *comp_nx, int *comp_ny,
                          int high_resolution);
static int laser_compress_mean(int *image1, int nx1, int ny1, int idim,
                               int *comp_image, int comp_nx, int comp_ny,
                               int ifact);
static int ps_caption(FILE *fp, int ixstart, int iystart, char *filename, 
                      char *comments, char *extra_comments, double mini, 
                      double maxi, int f1_xstart, int f1_ystart, int f1_xend, 
                      int f1_yend);

/***********************************************************
* JLP_GDev_PST::pstcopy1 
*
* INPUT:
*
* image_i: input "LUT-converted" image to be plotted
* nx_i, ny_i: size of image_i
* idim_i: first dimension of the input image_i 
* low_thresh, high_thresh: low and high threshold of the initial double image
* width, height: output size of the image on the plot (cm)
* title: title of the plot 
* image_name, image_comments: to be written on the bottom of the plot
* f1_xstart, f1_ystart, f1_xend, f1_yend: window boundaries of double image1
* Options:
*   lut_scale: 1 if lut scale is wanted, 0 otherwise
*   black_and_white:  0 if color output is wanted, 1 for black and white
*
* OUTPUT:
*  postscript file 
*
************************************************************/
int JLP_GDev_PST::pstcopy1(int *image_i, int nx_i, int ny_i, int idim_i,
                      double low_thresh, double high_thresh, int ncolors,
                      double width, double height, char *title, char *image_name,
                      char *image_comments, char *extra_comments,
                      int lut_scale, int black_and_white, int high_resolution,
                      int f1_xstart, int f1_ystart,int f1_xend, int f1_yend)
{
FILE  *fp1;
double  width1, height1, xstart, ystart, xx, yy; 
double FontSize, LineWidth, LUT_Height;
int *comp_image;
int comp_nx, comp_ny, nx1_i, ny1_i, offx, offy, axlen, aylen;
char buf1[40], buf2[40];

/* JLP97: for some unknown reason, bad output in Manchester
* when odd size, so: */
nx1_i = 2 * (nx_i / 2);
ny1_i = 2 * (ny_i / 2);

/* Conversion from cm to postscript units (72 dots / inch) */
     width1 = width * PSA4_RESOLUTION / 2.54; 
     height1 = height * PSA4_RESOLUTION / 2.54;

/* Old header from Eric Anterrieu: */
/*
      PS_Header(Jgc0.fdv,"jlp_pstcopy",image_name,
                "Times-Roman,Times-Roman-ISOLatin1",1,
                (PSA4_WIDTH_PT-width1)/2.0,(PSA4_HEIGHT_PT-height1)/2.0,
                (PSA4_WIDTH_PT+width1)/2.0,(PSA4_HEIGHT_PT+height1)/2.0);
      PS_Prolog(Jgc0.fdv);
*/
      if(lut_scale) PS_Prolog(Jgc0.fdv);

/* definition of axes around image: */
      define_image_axes(Jgc0.fdv,&nx1_i,&ny1_i,width1,height1);

      strcpy(buf1,"Times-Roman");
      strcpy(buf2,"Times-Roman-ISOLatin1");
      PS_FontISOLatin1(Jgc0.fdv,buf1,buf2);
/* Page 1 : */
      fprintf(Jgc0.fdv,"%%%%Page: %d\n",1);

/* Compute offset, update parameters and apply translation to the image: */
      xstart = MAXI(0, (PSA4_WIDTH_PT - width1 - 40)/4.0);
      ystart = (PSA4_HEIGHT_PT-height1)/2.0;
      ConvDevToMgo(xstart, ystart, &offx, &offy);
      ConvDevToMgo(xstart + width1, ystart + height1, &axlen, &aylen);
      axlen -= offx;
      aylen -= offy;
#ifdef DEBUG
printf("pstcopy1/New box parameters: offx=%d offy=%d axlen=%d aylen=%d\n", 
         offx, offy, axlen, aylen);
#endif
      SetNewBoxSetup(offx, offy, axlen, aylen);

      fprintf(Jgc0.fdv,"newpath\n %g %g translate\n stroke\n",
              xstart, ystart);

/* Image compression to reduce printing time: */
/* WARNING: ALREADY COMPRESSED if called by Xdisp1: here we process 
   the integer image... */
     laser_compress(image_i,nx1_i,ny1_i,idim_i,&comp_image,&comp_nx,&comp_ny,
                    high_resolution);

/* Beginning of B&W case */ 
   if(black_and_white)
     {
/* Old version: 
       PS_Image1_4bits(Jgc0.fdv,width1,height1,comp_nx,comp_ny,comp_nx,
                       comp_image,Jgc0.gclr.r,Jgc0.gclr.lut);
*/

/* Plots image in B&W: */
       PS_Image1_8bits(Jgc0.fdv,width1,height1,comp_nx,comp_ny,comp_nx,
                       comp_image,Jgc0.gclr.r,Jgc0.gclr.lut);
      }
     else
/* Beginning of color case */ 
      {
        PS_ImageRGB1(Jgc0.fdv,width1,height1,comp_nx,comp_ny,comp_nx,
                     comp_image,Jgc0.gclr.r,Jgc0.gclr.g, Jgc0.gclr.b); 
      }

/* Plots LUT with coordinates on bottom part of the plot:*/
      if(lut_scale && !black_and_white)
       {
       FontSize = 14.0;
       LineWidth = 0.4;
       LUT_Height = 20; 
#ifdef LUT_ON_THE_BOTTOM
/* Plots LUT with scale on the bottom of the plot:*/
       xx = 0.; yy = - height1/12.0-20.0;
       fprintf(Jgc0.fdv,"newpath\n %g %g translate\n stroke\n",
                xx, yy);
       strcpy(buf1, "Times-Roman-ISOLatin1");
       PS_LutRGB1(Jgc0.fdv,width1,LUT_Height,low_thresh,high_thresh,
                  Jgc0.gclr.r,Jgc0.gclr.g,Jgc0.gclr.b,
                  Jgc0.gclr.lut,ncolors,NULL,
                  buf1,FontSize,LineWidth,0.0,0.0,0.0);
       fprintf(Jgc0.fdv,"newpath\n %g %g translate\n stroke\n",
                -xx, -yy);

#else
/* Plots LUT with scale on the right of the plot:*/
      xx = width1 + 20. ; yy = 0.;
      fprintf(Jgc0.fdv,"newpath\n %g %g translate\n stroke\n",
              xx, yy);
      strcpy(buf1, "Times-Roman-ISOLatin1");
      PS_LutRGB1(Jgc0.fdv,LUT_Height,height1,low_thresh,high_thresh,
                 Jgc0.gclr.r,Jgc0.gclr.g,Jgc0.gclr.b,
                 Jgc0.gclr.lut,ncolors,NULL,
                 buf1,FontSize,LineWidth,0.0,0.0,0.0);
      fprintf(Jgc0.fdv,"newpath\n %g %g translate\n stroke\n",
               -xx, -yy);
#endif
      }
/* End of LUT and color case */ 

/* Free memory */
  free(comp_image);

/* Goes to bottom of the sheet */
    fprintf(Jgc0.fdv,"newpath\n %g %g translate\n stroke\n",
            -xstart, -ystart);

/* JLP96: I add possibility of drawing labels and symbols: */
    if(Jgc0.fp_backup != NULL)
        {
        fclose(Jgc0.fp_backup);
        Jgc0.fp_backup = NULL;
/* Warning: should not use Jgc0.fb_backup as a pointer, since JLP_SYMBOL
* would write to this file if it was open ... */
        if((fp1 = fopen(Jgc0.backup_fname,"r")) == NULL)
          fprintf(stderr, " Xdisp1/Error opening logfile \"%s\" \n",
                  Jgc0.backup_fname);
          draw_extra_symbols(fp1);
        fclose(fp1);
        }

/* Caption at the bottom xx=xstart yy=50*/ 
    ps_caption(Jgc0.fdv, (int)xstart, 50, image_name, image_comments, 
               extra_comments, low_thresh, high_thresh, f1_xstart, f1_ystart, 
               f1_xend, f1_yend);

return(0);
}
/****************************************************************
* Draw  extra symbols
* Called by jlp_pstcopy1
*
* INPUT:
* fp1: backup graphic file to read from (fp_backup)
****************************************************************/
int JLP_GDev_PST::draw_extra_symbols(FILE *fp1)
{
char buffer[80], s1[80], *pc;
int ix, iy, size, isymb;
double angle1, expand1, x1, y1, x2, y2;
int ix0, iy0, size0, isymb0, backup_to_file = 0;

/* Main loop: */
while(fgets(buffer,80,fp1) != NULL)
 {
/* Allowed calls: jlp_label1, jlp_label, jlp_symbol, jlp_line1 */
 if(!strncmp(buffer,"jlp_label1",10))
   {
     printf("buffer: %s \n",buffer);
     sscanf(buffer,"jlp_label1 %lf %lf %lf %lf @%s@\n",
                    &x1,&y1,&angle1,&expand1,s1);
/* As the previous statement discarded everything after a blank I use
the following: */
     pc = buffer;
     while(*pc && *pc != '@') pc++;
     if(*pc) pc++;
     strcpy(s1,pc); 
     pc = s1;
     while(*pc && *pc != '@') pc++;
     *pc = '\0';

/* JLP96/ Debug: */
     printf("jlp_label1 %f %f %f %.1f @%s@\n",
                              x1,y1,angle1,expand1,s1);
     fprintf(Jgc0.fdv,"%%%% jlp_label1 %f %f %f %.1f @%s@\n",
                              x1,y1,angle1,expand1,s1);
/* x1, y1 in user coordinates: */
      conv_user_to_mgo(x1, y1, &ix, &iy);
      gdev_label(s1,ix,iy,angle1,expand1,1,backup_to_file);
     fprintf(Jgc0.fdv,"%%%% jlp_label1_end\n");
   }
 else if(!strncmp(buffer,"jlp_label ",10))
   {
     sscanf(buffer,"jlp_label %d %d %lf %lf @%s@\n",
                    &ix,&iy,&angle1,&expand1,s1);
/* As the previous statement discarded everything after a blank I use
the following: */
     pc = buffer;
     while(*pc && *pc != '@') pc++;
     if(*pc) pc++;
     strcpy(s1,pc); 
     pc = s1;
     while(*pc && *pc != '@') pc++;
     *pc = '\0';

/* JLP96/ Debug: */
     printf("jlp_label %d %d %f %.1f @%s@\n",
                              (int)ix,(int)iy,angle1,expand1,s1);
     fprintf(Jgc0.fdv,"%%%% jlp_label %d %d %f %.1f @%s@\n",
                              (int)ix,(int)iy,angle1,expand1,s1);

/* Rescaling from interactive display (xterm, f.i.) to postscript parameters: */
       ix = ix - Jgc0.offx; 
       iy = iy - Jgc0.offy;
       gdev_label(s1,ix,iy,angle1,expand1,1,backup_to_file);
     fprintf(Jgc0.fdv,"%%%% jlp_label_end\n");
   }
/*************** jlp_symbol **********************************/
 else if(!strncmp(buffer,"jlp_symbol",10))
   {
      sscanf(buffer,"jlp_symbol %d %d %d %d\n",&ix,&iy,&size,&isymb);
      printf("jlp_symbol %d %d %d %d\n",(int)ix,(int)iy,(int)size,(int)isymb);
/* JLP96/ Debug: */
      fprintf(Jgc0.fdv,"%%%% jlp_symbol %d %d %d %d\n",
              (int)ix,(int)iy,(int)size,(int)isymb);
/* Rescaling: */
       ix = ix - Jgc0.offx; iy = iy - Jgc0.offy;
/* jlp_symbol calls only "jlp_line": */
        ix0 = ix; iy0 = iy; size0 = size; isymb0 = isymb;
        this->symbol(ix0,iy0,size0,isymb0);
      fprintf(Jgc0.fdv,"%%%% jlp_symbol_end\n");
   }
/*************** JLP_LINE1 **********************************/
 else if(!strncmp(buffer,"jlp_line1",9))
   {
      sscanf(buffer,"jlp_line1 %lf %lf %lf %lf\n",&x1,&y1,&x2,&y2);
      printf(" jlp_line1 %f %f %f %f\n",x1,y1,x2,y2);
      fprintf(Jgc0.fdv,"%%%% jlp_line1 %f %f %f %f\n",x1,y1,x2,y2);
      gdev_line1(x1,y1,x2,y2);
      fprintf(Jgc0.fdv,"%%%% jlp_line1_end\n");
   }

/* End of while loop */
 }

return(0);
}
/****************************************************************
* Draw axes
* by defining macro /drawaxes
* i.e. /drawaxes { ....} def
*
* Assume 0,1 scale (i.e. should be called after "scale" and before "grestore")
*
****************************************************************/
#define LINE_WIDTH  0.02
#define TICK_SIZE   3

static int define_image_axes(FILE *fp, int *nx, int *ny, double width, double height)
{
double dx, dy;
     dx = TICK_SIZE * (*nx) / width;
     dy = TICK_SIZE * (*ny) / height;

#ifdef NICE_AXES
     fprintf(fp,"/drawaxes { {} settransfer  /inci \n");
     fprintf(fp,"{/i i 1 add def i 100 eq \n"); 
     fprintf(fp," {/i 0 def currentpoint stroke moveto} if} def\n");
     fprintf(fp,"/setupticks {/i 0 def /ticks 10 \n");
     fprintf(fp,"{3 dy mul dy dy dy dy 2 dy mul dy dy dy dy}\n");
     fprintf(fp,"repeat 100 array astore def n 50 ge\n");
     fprintf(fp,"{ticks 0 5 dy mul put} if ticks 50 4 dy mul put} def\n");
     fprintf(fp,"/horizaxis {newpath setupticks n y moveto 0 y lineto\n");
     fprintf(fp,"n 1 add {/r ticks i get def inci 0 r rlineto\n");
     fprintf(fp,"1 r neg rmoveto} repeat stroke} def\n");
     fprintf(fp,"/vertaxis  {newpath setupticks x n moveto x 0 lineto\n");
     fprintf(fp,"n 1 add {/r ticks i get def inci r 0 rlineto\n");
     fprintf(fp,"r neg 1 rmoveto} repeat stroke} def\n");
     fprintf(fp," %f %f scale %f setlinewidth /n %d \n",
                 1.0/(*nx),1.0/(*ny),LINE_WIDTH,(int)*nx);
     fprintf(fp," def /y 0 def /dy %f def  horizaxis /y %d \n",
                -dx,(int)*ny);
     fprintf(fp," def /dy dy neg def horizaxis /n %d def /x 0 def /dy \n",
                (int)*ny);
     fprintf(fp," %f  def vertaxis /x %d def /dy dy neg def vertaxis} def\n",
                 -dx,(int)*nx);
#else
/* JLP2000: simpler axes, for publications:
/drawaxes {
newpath
0 0 m
0 1 rl
1 0 rl
0 -1 rl
closepath
% Small value otherwise black!
0.00001 setlinewidth
stroke
} def
*/
     fprintf(fp,"/drawaxes{ newpath 0 0 m 0 1 rl 1 0 rl 0 -1 rl\n");
     fprintf(fp,"closepath 0.00001 setlinewidth stroke } def\n");
#endif

return(0);
}
/******************************************************************
* Simple line around the image
******************************************************************/
#ifdef OLD_VERSION
static int laser_border(FILE *fp, int *nx, int *ny, double width, 
                        double height)
{
double work;
     work = *nx; if(*ny > work) work = *ny;
     fprintf(fp," /drawaxes { {} settransfer %f setlinewidth newpath\n",
             LINE_WIDTH/work);
     fprintf(fp,"0 0 moveto 0 1 lineto 1 1 lineto 1 0 lineto 0 0 lineto\n");
     fprintf(fp,"stroke } def\n");

return(0);
}
#endif
/******************************************************************
* Subroutine to compress the image to reduce output printing time 
* Actual compression is done in laser_compress2 or laser_compress_mean
*
******************************************************************/
static int laser_compress(int *image1, int nx1, int ny1, int idim,
                          int **comp_image, int *comp_nx, int *comp_ny,
                          int high_resolution)
{
int input_width, isize, ifact;

/* look for larger size of input image: */
input_width = (nx1 > ny1) ? nx1 : ny1;

/* Compute reduction factor: */
if(high_resolution) 
  ifact = 1;
else
  ifact = 1 + input_width/LASER_MAX_SIZE;

/* Output size: */
*comp_nx = nx1 / ifact;
*comp_ny = ny1 / ifact;
isize = (*comp_nx) * (*comp_ny) * sizeof(int);

#ifdef DEBUG
printf("laser_compress/input_width = %d comp_nx = %d comp_ny = %d ifact = %d\n",
         input_width,*comp_nx,*comp_ny,ifact);
#endif

/* Get memory space for compressed image: */
  *comp_image = (int *) malloc(isize);

/* Call compress routine: */
laser_compress_mean(image1,nx1,ny1,idim,*comp_image,*comp_nx,*comp_ny,ifact);

return(0);
}
/******************************************************************
* Subroutine to compress an image 
* Take a subsample of the input image 
*
******************************************************************/
#ifdef OLD_VERSION
static int laser_compress2(int *image1, int nx1, int ny1, int idim,
                           int *comp_image, int comp_nx, int comp_ny,
                           int ifact)
{
register int i, j, k;

/* If no compression, simply copy input image to output image: */
if(ifact == 1)
  {
    k = 0;
    for(j = 0; j < ny1; j++)
     {
     for(i = 0; i < nx1; i++)
      {
      comp_image[k] = image1[i + j * idim];
      k++;
      }
     }
  }
/* Else perform compression (subsample): */
 else
  {
    printf("laser_compress2/Compress input image (subsample) to nx= %d ny=%d\n",
                comp_nx, comp_ny);
    k = 0;
    for(j = 0; j < comp_ny; j++)
     {
     for(i = 0; i < comp_nx; i++)
      {
       comp_image[k] = image1[i * ifact  + (j * ifact) * idim]; 
       k++;
      }
     }
  }

return(0);
}
#endif
/******************************************************************
* Subroutine to compress an image 
* by computing the mean
*
* (Not tested ...)
******************************************************************/
static int laser_compress_mean(int *image1, int nx1, int ny1, int idim,
                               int *comp_image, int comp_nx, int comp_ny,
                               int ifact)
{
double sum;
int ifact2, iistart, iiend, jjstart, jjend;
register int i, j, ii, jj, k;

/* If no compression, simply copy input image to output image: */
if(ifact == 1)
  {
    printf("laser_compress_mean/No compression needed (max_size=%d > nx= %d and ny=%d\n",
           LASER_MAX_SIZE, comp_nx, comp_ny);
    k = 0;
    for(j = 0; j < ny1; j++)
     {
     for(i = 0; i < nx1; i++)
      {
      comp_image[k] = image1[i + j * idim];
      k++;
      }
     }
  }
/* Else perform compression (computing mean): */
 else
  {
    printf("laser_compress_mean/Compress input image (mean) to nx= %d ny=%d\n",
                comp_nx, comp_ny);
/* Area in pixels is ifact*ifact: */
    ifact2 = ifact * ifact;
    k = 0;
    for(j = 0; j < comp_ny; j++)
     {
     jjstart = j * ifact;
     jjend = jjstart + ifact;
     for(i = 0; i < comp_nx; i++)
      {
       iistart = i * ifact;
       iiend = iistart + ifact;
       sum = 0.;
       for(jj = jjstart; jj < jjend; jj++)
          for(ii = iistart; ii < iiend; ii++)
             sum +=image1[ii + jj * idim]; 
       comp_image[k] = (int)(sum / (double)ifact2); 
       k++;
      }
     }
  }

return(0);
}
/**********************************************************************
*
* Print caption at the bottom of the postscript page
*
* INPUT:  
*  filename = filename to be displayed 
*  fp = pointer to postscript file
*  ixstart, iystart: bottom_left coordinates use of the caption 
*
**********************************************************************/
static int ps_caption(FILE *fp, int ixstart, int iystart, char *filename, 
                      char *comments, char *extra_comments, double mini, 
                      double maxi, int f1_xstart, int f1_ystart, int f1_xend, 
                      int f1_yend) 
{
char current_date[60];
int err, iy;

iy = iystart;

fprintf(fp,"%%end(plot) !! for TeX include only\n");
fprintf(fp,"newpath\n");
fprintf(fp,"/Times-Roman findfont 10 scalefont setfont\n");
 JLP_CTIME(current_date, &err);
  if (err == 0) 
       {
       fprintf(fp,"%d %d m (%s) show\n",ixstart,iy,current_date);
       iy += 12;
       }

/* JLP93: mini and maxi */ 
fprintf(fp,"%d %d m (Thresholds: %g %g) show\n",ixstart,iy,mini,maxi);
iy += 12;
fprintf(fp,"%d %d m (Boundaries: Xstart=%d Ystart=%d Xend=%d Yend=%d) show\n",
               ixstart,iy,f1_xstart,f1_ystart,f1_xend,f1_yend);
iy += 12;

/* JLP93: add comments, filename (WARNING, write from bottom to top...): */

if(*extra_comments && *extra_comments != ' ')
  {
  fprintf(fp,"newpath\n");
  fprintf(fp,"/Times-Roman findfont 10 scalefont setfont\n");
  fprintf(fp,"%d %d m (Caption: %s) show\n",ixstart,iy,extra_comments);
  fprintf(fp,"stroke\n");
  iy += 12;
  }

if(*comments && *comments != ' ')
  {
  fprintf(fp,"newpath\n");
  fprintf(fp,"/Times-Roman findfont 10 scalefont setfont\n");
  fprintf(fp,"%d %d m (Comments: %s) show\n",ixstart,iy,comments);
  fprintf(fp,"stroke\n");
  iy += 14;
  }

if(*filename && *filename != ' ')
  {
  fprintf(fp,"newpath\n");
  fprintf(fp,"/Times-Roman findfont 16 scalefont setfont\n");
  fprintf(fp,"%d %d m (File: %s) show\n",ixstart,iy,filename);
  fprintf(fp,"stroke\n");
  }

/* Logo:
fprintf(fp,"newpath\n");
fprintf(fp,"/Times-Roman findfont 25 scalefont setfont\n");
fprintf(fp,"550 12 m logo\n");
fprintf(fp,"stroke\n");
*/

return(0);
}
