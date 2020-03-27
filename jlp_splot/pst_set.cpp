/**********************************************************************
* "pst_set.c" (Written by Eric Anterrieu, version 1991)
*
* void PS_Header(fp,creator,title,fonts,nbr_page,x_ll,y_ll,x_ur, y_ur) 
* void PS_Prolog(fp) 
* void PS_Trailer(fp,creator,title,comments,extra_comments,mini,maxi) 
* void PS_Page(fp,page) 
* void PS_Newpath(fp) 
* void PS_Stroke(fp) 
* void PS_Translate(fp,x,y) 
* void PS_FontISOLatin1(fp,fontname,isofontname) 
* void PS_GetString(string)
* void PS_Lut(fp,width,height,zmin,zmax,nbr_colors,lut,zlegend,
*             fontname,fontsize,linewidth,red,green,blue)
* void PS_LutRGB(fp,width,height,zmin,zmax,color1,color2,lut,zlegend,
*                fontname,fontsize,linewidth,red,green,blue)
* void PS_Image(fp,width,height,nx,ny,image)
* void PS_Image1_4bits(fp,width,height,nx,ny,idim,image,r,pst_lut)
* void PS_Image1_8bits(fp,width,height,nx,ny,idim,image,r,pst_lut)
* void PS_ImageRGB(fp,width,height,nx,ny,image,lut)
* void PS_ImageRGB1(fp,width,height,nx,ny,image,r,g,b)
* void PS_Isoc(fp,width,height,ny,nx,sobel)
* void PS_Bar3d(fp,width,height,ny,nx,z,x,y,theta,phi,
*               linepattern,linewidth,red,green,blue)
* void PS_Pixel_LLlabelling(fp,x_ll,y_ll,x_ur,y_ur,nbr_col,cols,xlegend,
*                           nbr_lin,lins,ylegend,fontname,fontsize,
*                           linewidth,red,green,blue)
* void PS_Pixel_URlabelling(fp,x_ll,y_ll,x_ur,y_ur,nbr_col,cols,xlegend,
*                           nbr_lin,lins,ylegend,fontname,fontsize,
*                           linewidth,red,green,blue)
* void PS_Axis_LLlabelling(fp,x_ll,y_ll,x_ur,y_ur,xmin,xmax,xlegend,
*                          ymin,ymax,ylegend,fontname,fontsize,
*                          linewidth,red,green,blue)
* void PS_Axis_URlabelling(fp,x_ll,y_ll,x_ur,y_ur,xmin,xmax,xlegend,
*                           ymin,ymax,ylegend,fontname,fontsize,
*                           linewidth,red,green,blue)
* void PS_Angle_LLlabelling(fp,x_ll,y_ll,x_ur,y_ur,col,rx,a1,a2,a3,
*                           xlegend,lin,ry,d1,d2,d3,ylegend,fontname,
*                           fontsize,linewidth,red,green,blue)
* void PS_Angle_URlabelling(fp,x_ll,y_ll,x_ur,y_ur,col,rx,a1,a2,a3,
*                           xlegend,lin,ry,d1,d2,d3,ylegend,fontname,
*                           fontsize,linewidth,red,green,blue)
* void PS_Rectangle(fp,x_ll,y_ll,x_ur,y_ur,linewidth,red,green,blue) 
* void PS_Grid(fp,x_ll,y_ll,x_ur,y_ur,xs,ys,linewidth,red,green,blue)
* void PS_UVcoverage(fp,nbr_config,nbr_baseline,nbr_sample,u,v,
*                    ax,bx,ay,by,linewidth,red,green,blue)
* void PS_Curve(fp,v1,v3,v2,v4,w1,w3,w2,w4,nbr_pts,x,y,
*               linepattern,linewidth,red,green,blue)
* void PS_Symbol(fp,v1,v3,v2,v4,w1,w3,w2,w4,nbr_pts,x,y,
*                symbpattern,linewidth,red,green,blue)
* void PS_Axis_LLlabelling_manu(fp,v1,v3,v2,v4,w1,w3,w2,w4,
*                               x1,x2,x3,xlegend,y1,y2,y3,ylegend,
*                               fontname,fontsize,linewidth,red,green,blue)
* void PS_Axis_URlabelling_manu(fp,v1,v3,v2,v4,w1,w3,w2,w4,
*                               x1,x2,x3,xlegend,y1,y2,y3,ylegend,
*                               fontname,fontsize,linewidth,red,green,blue)
* void PS_LutRGB1(fp,width,height,zmin,zmax,r,g,b,pst_lut,ncolors,zlegend,
*                 fontname,fontsize,linewidth,red,green,blue)
*
* From Eric Anterrieu (Version 01-12-91)
*
* JLP Version 24-05-2006
**********************************************************************/
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <math.h>
#include "pst_set.h"
#include "jlp_time0.h"   // CTIME

/* In this file (also declared in "pst_set.h")
void PS_Header(FILE *fp, char *creator, char *title, char *fonts,
               int nbr_page, double x_ll, double y_ll, double x_ur, double y_ur);
void PS_Prolog(FILE *fp);
void PS_Page(FILE *fp, int page);
void PS_Newpath(FILE *fp);
void PS_Stroke(FILE *fp);
void PS_Translate(FILE *fp, double x, double y);
void PS_FontISOLatin1(FILE *fp, char *fontname, char *isofontname);
void PS_GetString(char *string);
void PS_Lut(FILE *fp, double width, double height, double zmin, 
            double zmax, int nbr_colors, char *lut, char *zlegend,
            char *fontname, double fontsize, double linewidth, 
            double red, double green, double blue);
void PS_LutRGB(FILE *fp, double width, double height, double zmin, double zmax,
               int color1, int color2, int lut[3][256], char *zlegend,
               char *fontname, double fontsize, double linewidth,
               double red, double green, double blue);
void  PS_LutRGB1(FILE *fp, double width, double height, double zmin, double zmax,
                 int *r, int *g, int *b, int *pst_lut, int ncolors,
                 char *zlegend, char *fontname, double fontsize, double linewidth,
                 double red, double green, double blue);
void PS_Image(FILE *fp, double width, double height, int nx, int ny, int *image);
void PS_Image1_8bits(FILE *fp, double width, double height, int nx, int ny,
                     int idim, int *image, int *r, int *pst_lut);
void PS_Image1_4bits(FILE *fp, double width, double height, int nx, int ny,
                     int idim, int *image, int *r, int *pst_lut);
void PS_ImageRGB(FILE *fp, double width, double height, int nx, int ny, 
                 int *image, int lut[3][256]);
void PS_ImageRGB1(FILE *fp, double width, double height, int nx, int ny,
                  int idim, int *image, int *r, int *g, int *b);
void PS_Isoc(FILE *fp, double width, double height, int ny, int nx, int **sobel);
void PS_Bar3d(FILE *fp, double width, double height, int ny, int nx,
              double **z, double *x, double *y, double theta, double phi,
              int linepattern, double linewidth, 
              double red, double green, double blue);
void PS_Pixel_LLlabelling(FILE *fp, double x_ll, double y_ll, double x_ur, 
                          double y_ur, int nbr_col, int cols, char *xlegend,
                          int nbr_lin, int lins, char *ylegend, char *fontname,
                          double fontsize, double linewidth, double red,
                          double green, double blue);
void PS_Pixel_URlabelling(FILE *fp, double x_ll, double y_ll, double x_ur, 
                          double y_ur, int nbr_col, int cols, char *xlegend,
                          int nbr_lin, int lins, char *ylegend, char *fontname,
                          double fontsize, double linewidth, double red,
                          double green, double blue);
void PS_Axis_LLlabelling(FILE *fp, double x_ll, double y_ll, double x_ur, 
                         double y_ur, double xmin, double xmax, char *xlegend,
                         double ymin, double ymax, char *ylegend, char *fontname,
                         double fontsize, double linewidth, double red,
                         double green, double blue);
void PS_Axis_URlabelling(FILE *fp, double x_ll, double y_ll, double x_ur, 
                         double y_ur, double xmin, double xmax, char *xlegend,
                         double ymin, double ymax, char *ylegend, char *fontname,
                         double fontsize, double linewidth, double red,
                         double green, double blue);
void PS_Angle_LLlabelling(FILE *fp, double x_ll, double y_ll, double x_ur, 
                          double y_ur, int col, double rx, int a1, int a2, 
                          double a3, char *xlegend, int lin, double ry,
                          int d1, int d2, double d3, char *ylegend, 
                          char *fontname, double fontsize, double linewidth,
                          double red, double green, double blue);
void PS_Angle_URlabelling(FILE *fp, double x_ll, double y_ll, double x_ur, 
                          double y_ur, int col, double rx, int a1, int a2, 
                          double a3, char *xlegend, int lin, double ry,
                          int d1, int d2, double d3, char *ylegend, 
                          char *fontname, double fontsize, double linewidth,
                          double red, double green, double blue);
void PS_Rectangle(FILE *fp, double x_ll, double y_ll, double x_ur, double y_ur,
                  double linewidth, double red, double green, double blue);
void PS_Grid(FILE *fp, double x_ll, double y_ll, double x_ur, double y_ur,
             double xs, double ys, double linewidth, double red, double green, 
             double blue);
void  PS_UVcoverage(FILE *fp, int nbr_config, int *nbr_baseline, 
                    int *nbr_sample, double **u, double **v,
                    double ax, double bx, double ay, double by, double linewidth,
                    double red, double green, double blue);
void  PS_Curve(FILE *fp, double v1, double v3, double v2, double v4,
               double w1, double w3, double w2, double w4, int nbr_pts,
               double *x, double *y, int linepattern, double linewidth,
               double red, double green, double blue);
void  PS_Symbol(FILE *fp, double v1, double v3, double v2, double v4, 
                double w1, double w3, double w2, double w4, int nbr_pts,
                double *x, double *y, int symbpattern,double linewidth,
                double red, double green, double blue);
void PS_Axis_LLlabelling_manu(FILE *fp, double v1, double v3, double v2, double v4,
                              double w1, double w3, double w2, double w4,
                              double x1, double x2, int x3, char *xlegend,
                              double y1, double y2, int y3, char *ylegend,
                              char *fontname, double fontsize, double linewidth,
                              double red, double green, double blue);
void PS_Axis_URlabelling_manu(FILE *fp, double v1, double v3, double v2, double v4,
                              double w1, double w3, double w2, double w4,
                              double x1, double x2, int x3, char *xlegend,
                              double y1, double y2, int y3, char *ylegend,
                              char *fontname, double fontsize, double linewidth,
                              double red, double green, double blue);
*/
/**********************************************************************
*
* FUNCTION: PS_Header
*
* PURPOSE: Prints header in postscript file.
*
* INPUT:  creator = creator of postscript file
*         title = title
*         fonts = fonts used
*         nbr_page = number of pages
*         x_ll,y_ll,x_ur, y_ur = boundingbox
*         fp = pointer to postscript file
*
* AUTHOR: Eric ANTERRIEU (Version 01-12-91)
*
* JLP
* Version 14-04-2004
**********************************************************************/
/**********************************************************************/
void PS_Header(FILE *fp, char *creator, char *title, char *fonts,
               int nbr_page, double x_ll, double y_ll, double x_ur, double y_ur) 
{
char Date[60];
int err;

 fprintf(fp,"%%!PS-Adobe\n");
 fprintf(fp,"%%%%Creator: %s [from Eric Anterrieu...]\n",creator);
 fprintf(fp,"%%%%Title: %s\n",title);
JLP_CTIME(Date,&err);
if(err == 0) fprintf(fp,"%%%%CreationDate: %s\n",Date);

 fprintf(fp,"%%%%Pages: %d\n",nbr_page);
 fprintf(fp,"%%%%DocumentFonts: %s\n",fonts);
 fprintf(fp,"%%%%BoundingBox: %g %g %g %g\n",x_ll,y_ll,x_ur,y_ur);
 fprintf(fp,"%%%%EndComments\n");
}
/**********************************************************************/
/*                                                                    */
/* FUNCTION: PS_Prolog                                                */
/*                                                                    */
/* PURPOSE: Prints prolog in postscript file.                         */
/*                                                                    */
/* INPUT:  fp = pointer to postscript file                            */
/*                                                                    */
/**********************************************************************/
void PS_Prolog(FILE *fp) 
{

 fprintf(fp,"%%begin(plot) !! for TeX include only\n");
/* JLP96: I add some more definitions to be compatible with "jlp_pst": */
fprintf(fp,"/lt {lineto currentpoint stroke moveto} def\n");
/* End of JLP96 */
 fprintf(fp,"/m {moveto} def\n");
 fprintf(fp,"/l {lineto} def\n");
 fprintf(fp,"/rm {rmoveto} def\n");
 fprintf(fp,"/rl {rlineto} def\n");
 fprintf(fp,"/sh {show} def\n");
 fprintf(fp,"/rs {dup stringwidth pop 0 exch sub 0 rm show} def\n");
 fprintf(fp,"/cs {dup stringwidth pop 2 div 0 exch sub 0 rm show} def\n");
 fprintf(fp,"/symb1 {newpath 2 0 360 arc stroke} def\n");
 fprintf(fp,"/symb5 {newpath 2 0 360 arc fill stroke} def\n");
 fprintf(fp,"/symb2 {newpath m -2 -1.5 rm 2 4 rl 2 -4 rl -4 0 rl stroke} def\n");
 fprintf(fp,"/symb6 {newpath m -2 -1.5 rm 2 4 rl 2 -4 rl -4 0 rl fill stroke} def\n");
 fprintf(fp,"/symb3 {newpath m -2 -2 rm 0 4 rl 4 0 rl 0 -4 rl -4 0 rl stroke} def\n");
 fprintf(fp,"/symb7 {newpath m -2 -2 rm 0 4 rl 4 0 rl 0 -4 rl -4 0 rl fill stroke} def\n");
 fprintf(fp,"/symb4 {newpath m -2 0 rm 4 0 rl -2 2 rm 0 -4 rl stroke} def\n");
 fprintf(fp,"/symb8 {newpath m -2 -2 rm 4 4 rl -4 0 rm 4 -4 rl stroke} def\n");
 fprintf(fp,"/line1 {setlinewidth [] 0 setdash} def\n");
 fprintf(fp,"/line2 {setlinewidth [1 3] 0 setdash} def\n");
 fprintf(fp,"/line3 {setlinewidth [2 2] 0 setdash} def\n");
 fprintf(fp,"/line4 {setlinewidth [5 5] 0 setdash} def\n");
 fprintf(fp,"/line5 {setlinewidth [10 5] 0 setdash} def\n");
 fprintf(fp,"/line6 {setlinewidth [5 5 10 5] 0 setdash} def\n");
 fprintf(fp,"/line7 {setlinewidth [5 4 5 4 10 4] 0 setdash} def\n");
 fprintf(fp,"/line8 {setlinewidth [2 2 2 3 8 3] 0 setdash} def\n");
 fprintf(fp,"/logo {.95 -.05 0{setgray (OMP) rs -1 .5 rm}");
 fprintf(fp,"for 1 -.5 rm 1 setgray (OMP) rs} def\n");
 fprintf(fp,"%%%%EndProlog\n");
}
/**********************************************************************/
/*                                                                    */
/* FUNCTION: PS_Page                                                  */
/*                                                                    */
/* PURPOSE: Prints current page number in postscript file.            */
/*                                                                    */
/* INPUT:  page = current page number                                 */
/*         fp = pointer to postscript file                            */
/*                                                                    */
/**********************************************************************/
void PS_Page(FILE *fp, int page) 
{
 fprintf(fp,"%%%%Page: %d\n",page);
}
/**********************************************************************/
/*                                                                    */
/* FUNCTION: PS_Newpath                                               */
/*                                                                    */
/* PURPOSE: Initializes the current path to be empty.                 */
/*                                                                    */
/* INPUT:  fp = pointer to postscript file                            */
/*                                                                    */
/**********************************************************************/
void PS_Newpath(FILE *fp) 
{
if (fp != NULL)   fprintf(fp,"newpath\n");
}
/**********************************************************************/
/*                                                                    */
/* FUNCTION: PS_Stroke                                                */
/*                                                                    */
/* PURPOSE: Paints the current path.                                  */
/*                                                                    */
/* INPUT:  fp = pointer to postscript file                            */
/*                                                                    */
/**********************************************************************/
void PS_Stroke(FILE *fp) 
{
if (fp != NULL)   fprintf(fp,"stroke\n");
}
/**********************************************************************/
/*                                                                    */
/* FUNCTION: PS_Translate                                             */
/*                                                                    */
/* PURPOSE: Moves the origin of the user coordinate system.           */
/*                                                                    */
/* INPUT:  x, y = translation vector                                  */
/*         fp = pointer to postscript file                            */
/*                                                                    */
/**********************************************************************/
void PS_Translate(FILE *fp, double x, double y) 
{
if (fp != NULL)
  {
   fprintf(fp,"newpath\n");
   fprintf(fp,"%g %g translate\n",x,y);
   fprintf(fp,"stroke\n");
  }
}
/**********************************************************************/
/*                                                                    */
/* FUNCTION: PS_FontISOLatin1                                         */
/*                                                                    */
/* PURPOSE: Extension of Postscript font to ISOLatin1 font.           */
/*                                                                    */
/* INPUT:  fp = pointer to postscript file                            */
/*         fontname = standard font name                              */
/*         isofontname = ISOLatin1 font name                          */
/*                                                                    */
/**********************************************************************/
void PS_FontISOLatin1(FILE *fp, char *fontname, char *isofontname) 
{
 fprintf(fp,"%%%% Extension of %s to %s\n",fontname,isofontname);
 fprintf(fp,"/%s findfont\n",fontname);
 fprintf(fp,"dup length dict begin\n");
 fprintf(fp,"  {1 index /FID ne{def} {pop pop} ifelse} forall\n");
 fprintf(fp,"  /Encoding ISOLatin1Encoding def\n");
 fprintf(fp,"  currentdict\n");
 fprintf(fp,"end\n");
 fprintf(fp,"/%s exch definefont pop\n",isofontname);
}
/**********************************************************************/
/*                                                                    */
/* FUNCTION: PS_GetString                                             */
/*                                                                    */
/* PURPOSE: Formats a string.                                         */
/*                                                                    */
/* INPUT:  string = string to format                                  */
/*                                                                    */
/* OUTPUT: string = formatted string                                  */
/*                                                                    */
/**********************************************************************/
void PS_GetString(char *string)
{
register   int     i, j;
char    newstring[200];

i=0;
for (j=0; j< (int)strlen(string); j++)
  {
  switch (string[j])
    {
    case '(':
      newstring[i++] = '\\'; 
      newstring[i++] = '0'; newstring[i++] = '5'; newstring[i++] = '0';
      break;
    case ')':
      newstring[i++] = '\\'; 
      newstring[i++] = '0'; newstring[i++] = '5'; newstring[i++] = '1';
      break;
    case '<':
      newstring[i++] = '\\'; 
      newstring[i++] = '0'; newstring[i++] = '7'; newstring[i++] = '4';
      break;
    case '>':
      newstring[i++] = '\\'; 
      newstring[i++] = '0'; newstring[i++] = '7'; newstring[i++] = '6';
      break;
    case '{':
      newstring[i++] = '\\'; 
      newstring[i++] = '1'; newstring[i++] = '7'; newstring[i++] = '3';
      break;
    case '}':
      newstring[i++] = '\\'; 
      newstring[i++] = '1'; newstring[i++] = '7'; newstring[i++] = '5';
      break;
    case '[':
      newstring[i++] = '\\'; 
      newstring[i++] = '1'; newstring[i++] = '3'; newstring[i++] = '3';
      break;
    case ']':
      newstring[i++] = '\\'; 
      newstring[i++] = '1'; newstring[i++] = '3'; newstring[i++] = '5';
      break;
    case '|':
      newstring[i++] = '\\'; 
      newstring[i++] = '1'; newstring[i++] = '7'; newstring[i++] = '4';
      break;
    case 'Æ':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '4'; newstring[i++] = '3';
      break;
    case ' ':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '4'; newstring[i++] = '1';
      break;
    case '…':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '4'; newstring[i++] = '0';
      break;
    case 'ƒ':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '4'; newstring[i++] = '2';
      break;
    case '„':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '4'; newstring[i++] = '4';
      break;
    case '‚':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '5'; newstring[i++] = '1';
      break;
    case 'Š':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '5'; newstring[i++] = '0';
      break;
    case 'ˆ':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '5'; newstring[i++] = '2';
      break;
    case '‰':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '5'; newstring[i++] = '3';
      break;
    case '¡':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '5'; newstring[i++] = '5';
      break;
    case '':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '5'; newstring[i++] = '4';
      break;
    case 'Œ':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '5'; newstring[i++] = '6';
      break;
    case '‹':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '5'; newstring[i++] = '7';
      break;
    case '¢':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '6'; newstring[i++] = '3';
      break;
    case '•':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '6'; newstring[i++] = '2';
      break;
    case '“':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '6'; newstring[i++] = '4';
      break;
    case '”':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '6'; newstring[i++] = '6';
      break;
    case '£':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '7'; newstring[i++] = '2';
      break;
    case '—':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '7'; newstring[i++] = '1';
      break;
    case '–':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '7'; newstring[i++] = '3';
      break;
    case '':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '7'; newstring[i++] = '4';
      break;
    case '‡':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '4'; newstring[i++] = '7';
      break;
    case '¤':
      newstring[i++] = '\\'; 
      newstring[i++] = '3'; newstring[i++] = '6'; newstring[i++] = '1';
      break;
    case '@':
      newstring[i++] = '\\'; 
      newstring[i++] = '1'; newstring[i++] = '0'; newstring[i++] = '0';
      break;
    case '#':
      newstring[i++] = '\\'; 
      newstring[i++] = '0'; newstring[i++] = '4'; newstring[i++] = '3';
      break;
    case '$':
      newstring[i++] = '\\'; 
      newstring[i++] = '0'; newstring[i++] = '4'; newstring[i++] = '4';
      break;
    case '%':
      newstring[i++] = '\\'; 
      newstring[i++] = '0'; newstring[i++] = '4'; newstring[i++] = '5';
      break;
    case '/':
      newstring[i++] = '\\'; 
      newstring[i++] = '0'; newstring[i++] = '5'; newstring[i++] = '7';
      break;
    case '\\':
      newstring[i++] = '\\'; 
      newstring[i++] = '1'; newstring[i++] = '3'; newstring[i++] = '4';
      break;
    case 'õ':
      newstring[i++] = '\\'; 
      newstring[i++] = '2'; newstring[i++] = '4'; newstring[i++] = '7';
      break;
    case 'œ':
      newstring[i++] = '\\'; 
      newstring[i++] = '2'; newstring[i++] = '4'; newstring[i++] = '3';
      break;
    case 'ý':
      newstring[i++] = '\\'; 
      newstring[i++] = '2'; newstring[i++] = '6'; newstring[i++] = '2';
      break;
    case 'ü':
      newstring[i++] = '\\'; 
      newstring[i++] = '2'; newstring[i++] = '6'; newstring[i++] = '3';
      break;
    default:
      newstring[i++] = string[j];
      break;
    }
  }
newstring[i] = '\000';
strcpy(string,newstring);
}
/**********************************************************************/
/*                                                                    */
/* FUNCTION: PS_Lut                                                   */
/*                                                                    */
/* PURPOSE: Draws LUT caption.                                      */
/*                                                                    */
/* INPUT:  width,height = rectangle dimensions                        */
/*         zmin,zmax = min and max values                             */
/*         nbr_colors = number of colors                              */
/*         lut = equation of lut: lin, log or sqrt                    */
/*         fp = pointer to postscript file                            */
/*                                                                    */
/**********************************************************************/
void PS_Lut(FILE *fp, double width, double height, double zmin, 
            double zmax, int nbr_colors, char *lut, char *zlegend,
            char *fontname, double fontsize, double linewidth, 
            double red, double green, double blue)
/**********************************************************************/
{
register   int     i, j;
double   z, az, bz;
double   expz, manx, pasz, decz;

if (fp != NULL)
  {
   fprintf(fp,"newpath\n");
   fprintf(fp,"%g %g scale\n",width,height);
  if (nbr_colors > 0)
    {
     fprintf(fp,"%d 1 8 [%d 0 0 1 0 0]{<\n",nbr_colors,nbr_colors);
    for (i=0; i<nbr_colors; i++)
      {
      j = i*255;
      j /= (nbr_colors-1);
      if ((strcmp(lut,"log")) == 0)
        {
        z = j;
        z = 106.3016*log10(z+1.0);  /* 106.3016 = 256/log(255+1) */
        j = ((int) z);
        }
      if ((strcmp(lut,"sqrt")) == 0)
        {
        z = j;
        z = 15.9687*sqrt(z);       /* 15.9687 = 255/sqrt(255) */
        j = ((int) z);
        }
      if (j < 16)
         fprintf(fp,"0%x",j);
      else
         fprintf(fp,"%x",j);
      }
    }
  else
    {
     fprintf(fp,"%d 1 8 [%d 0 0 1 0 0]{<\n",-nbr_colors,-nbr_colors);
    for (i=0; i<(-nbr_colors); i++)
      {
      j = i*255;
      j /= (-nbr_colors-1);
      j = 255-j;
      if ((strcmp(lut,"log")) == 0)
        {
        z = j;
        z = 106.3016*log10(z+1.0);  /* 106.3016 = 256/log(255+1) */
        j = ((int) z);
        }
      if ((strcmp(lut,"sqrt")) == 0)
        {
        z = j;
        z = 15.9687*sqrt(z);        /* 15.9687 = 255/sqrt(255) */
        j = ((int) z);
        }
      if (j < 16)
         fprintf(fp,"0%x",j);
      else
         fprintf(fp,"%x",j);
      }
    }
   fprintf(fp,">} image\nstroke\n");
   fprintf(fp,"newpath\n");
   fprintf(fp,"1 %g div 1 scale\n",width);
   fprintf(fp,"1 %g div 1 exch scale\n",height);
   fprintf(fp,"stroke\n");

  az = width/(zmax-zmin);  bz = -width*zmin/(zmax-zmin);
  expz = floor(log10(zmax-zmin));
  manx = (zmax-zmin)/(pow(10.,expz));
  pasz = ceil(manx)*pow(10.,expz-1);
  decz = pasz*ceil(zmin/pasz) - zmin;
   fprintf(fp,"newpath\n");
   fprintf(fp,"/%s findfont %g scalefont setfont\n",fontname,fontsize);
   fprintf(fp,"%g line1\n",linewidth);
   fprintf(fp,"%g %g %g setrgbcolor\n",red,green,blue);
  for (z=(zmin+decz); z<=zmax; z+=pasz)
     fprintf(fp,"%.2f 0 m %.2f -6 l\n",az*z+bz,az*z+bz);
  for (z=(zmin+decz); z<=zmax; z+=2*pasz)
     fprintf(fp,"%.2f -20 m (%g) cs\n",az*z+bz,z);
  if (zlegend != NULL)
     fprintf(fp,"%g -26.5 m (%s) cs\n",width/2.0,zlegend);
   fprintf(fp,"stroke\n");
  }
}

/**********************************************************************
*
* FUNCTION: PS_LutRGB
*
* PURPOSE: Draws Lut RGB caption.
*
* INPUT:  width,height = rectangle dimensions
*         zmin,zmax = min and max values
*         color1, color1: address of boundary color cells
*         nbr_colors = number of colors
*         lut = equation of lut: lin, log or sqrt
*         fp = pointer to postscript file
*
**********************************************************************/
void  PS_LutRGB(FILE *fp, double width, double height, double zmin, double zmax,
                int color1, int color2, int lut[3][256], char *zlegend,
                char *fontname, double fontsize, double linewidth,
                double red, double green, double blue)
{
register   int     i, k;
int     nbr_colors;
double   z, az, bz;
double   expz, manx, pasz, decz;

if (fp != NULL)
  {
  nbr_colors = color2-color1+1;
  if (width > height)
     fprintf(fp,"%%%% RGB Look Up Table (mode H)\n");
  else
     fprintf(fp,"%%%% RGB Look Up Table (mode V)\n");
   fprintf(fp,"newpath\n");
   fprintf(fp,"%g %g scale\n",width,height);
/* 8 bit encoding */
  if (width > height)
     fprintf(fp,"%d 1 8 [%d 0 0 1 0 0]\n{<",nbr_colors,nbr_colors);
  else
     fprintf(fp,"1 %d 8 [1 0 0 %d 0 0]\n{<",nbr_colors,nbr_colors);
  k = 0;
  for (i=0; i<nbr_colors; i++)
    {
    if (lut[0][color1+i] < 16)  fprintf(fp,"0%x",lut[0][color1+i]);
    else  fprintf(fp,"%x",lut[0][color1+i]);
    if (k == 31) { fprintf(fp,"\n");  k = 0;}
    else k++;
    }
  if ((i != (nbr_colors-1)) && (k != 0))  fprintf(fp,"\n");
   fprintf(fp,">}\n{<");
  k = 0;
  for (i=0; i<nbr_colors; i++)
    {
    if (lut[1][color1+i] < 16)  fprintf(fp,"0%x",lut[1][color1+i]);
    else  fprintf(fp,"%x",lut[1][color1+i]);
    if (k == 31) { fprintf(fp,"\n");  k = 0;}
    else k++;
    }
  if ((i != (nbr_colors-1)) && (k != 0))  fprintf(fp,"\n");
   fprintf(fp,">}\n{<");
  k = 0;
  for (i=0; i<nbr_colors; i++)
    {
    if (lut[2][color1+i] < 16)  fprintf(fp,"0%x",lut[2][color1+i]);
    else  fprintf(fp,"%x",lut[2][color1+i]);
    if (k == 31) { fprintf(fp,"\n");  k = 0;}
    else k++;
    }
  if ((i != (nbr_colors-1)) && (k != 0))  fprintf(fp,"\n");
   fprintf(fp,">}\ntrue 3 colorimage\nstroke\n");
   fprintf(fp,"newpath\n");
   fprintf(fp,"1 %g div 1 scale\n",width);
   fprintf(fp,"1 %g div 1 exch scale\n",height);
   fprintf(fp,"stroke\n");

  expz = floor(log10(zmax-zmin));
  manx = (zmax-zmin)/(pow(10.,expz));
  pasz = ceil(manx)*pow(10.,expz-1);
  decz = pasz*ceil(zmin/pasz) - zmin;
   fprintf(fp,"newpath\n");
   fprintf(fp,"%g line1\n",linewidth);
   fprintf(fp,"%g %g %g setrgbcolor\n",red,green,blue);
   fprintf(fp,"0 0 m 0 %g l %g %g l %g 0 l closepath\n",
                 height,width,height,width);
   fprintf(fp,"stroke\n");
   fprintf(fp,"newpath\n");
   fprintf(fp,"/%s findfont %g scalefont setfont\n",fontname,fontsize);
   fprintf(fp,"%g line1\n",linewidth);
   fprintf(fp,"%g %g %g setrgbcolor\n",red,green,blue);

  if (width > height)
    {
    az = width/(zmax-zmin);  bz = -width*zmin/(zmax-zmin);
    for (z=(zmin+decz); z<=zmax; z+=pasz)
       fprintf(fp,"%.2f 0 m %.2f -6 l\n",az*z+bz,az*z+bz);
    for (z=(zmin+decz); z<=zmax; z+=2*pasz)
       fprintf(fp,"%.2f -15 m (%g) cs\n",az*z+bz,z);
    if (zlegend != NULL)
       fprintf(fp,"%g -26.5 m (%s) cs\n",width/2.0,zlegend);
     fprintf(fp,"stroke\n");
    }
  else
    {
    az = height/(zmax-zmin);  bz = -height*zmin/(zmax-zmin);
    for (z=(zmin+decz); z<=zmax; z+=pasz)
       fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",width,az*z+bz,width+6.0,az*z+bz);
    for (z=(zmin+decz); z<=zmax; z+=2*pasz)
       fprintf(fp,"%.2f %.2f m (%g) sh\n",width+8.0,az*z+bz-3.0,z);
    if (zlegend != NULL)
       fprintf(fp,"%.2f %.2f m (%s) sh\n",width+20.0,height/2.0,zlegend);
     fprintf(fp,"stroke\n");
    }
   fprintf(fp,"%%%% End of RGB Look Up Table\n");
  }
}
/**********************************************************************
*
* FUNCTION: PS_Image
*
* PURPOSE: Renders a gray shaded image.
*
* INPUT:  width,height = rectangle dimensions
*         nx,ny = image dimensions
*         image = pointer to integer image
*         fp = pointer to postscript file
*
* Warning: this version doesn't use LUT. Only ITT is taken into account
**********************************************************************/
void PS_Image(FILE *fp, double width, double height, int nx, int ny, int *image)
{
register   int  i, j, k;
int  gray;

if (fp != NULL)
  {
   fprintf(fp,"gsave\n");
   fprintf(fp,"%g %g scale\n",width,height);
/* 8 bit encoding */
   fprintf(fp,"%d %d 8 [%d 0 0 %d 0 0]\n",nx,ny,nx,ny);
   fprintf(fp,"{currentfile %d string readhexstring pop}\n",nx);
   fprintf(fp,"image\n");
  for (j=0; j<ny; j++)
    {
    k = 1;
    for (i=0; i < nx; i++)
      {
      gray = image[i + j * nx];
      if (gray < 16)  fprintf(fp,"0%x",gray);
      else  fprintf(fp,"%x",gray);
      if (k == 24) { fprintf(fp,"\n");  k = 1;}
      else k++;
      }
    if (k != 1)  fprintf(fp,"\n");
    }
/* JLP93: I add drawaxes... */
   fprintf(fp,"drawaxes\n");
   fprintf(fp,"grestore\n");
  }
}
/**********************************************************************
*
* FUNCTION: PS_Image1_8bits
*
* PURPOSE: Renders a gray shaded image.
*
* INPUT:  width,height = rectangle dimensions
*         nx,ny = image dimensions
*         image = pointer to integer image
*         fp = pointer to postscript file
*
* 8 bit encoding
*
* Same as PS_Image_8bits but uses LUT (red part only, since B&W). 
**********************************************************************/
void     PS_Image1_8bits(FILE *fp, double width, double height, int nx, int ny,
                         int idim, int *image, int *r, int *pst_lut)
{
register   int  i, j, k, icell;
int  gray;

if (fp != NULL)
  {
   fprintf(fp,"gsave\n");
   fprintf(fp,"%g %g scale\n",width,height);
/* 8 bit encoding */
   fprintf(fp,"%d %d 8 [%d 0 0 %d 0 0]\n",nx,ny,nx,ny);
   fprintf(fp,"{currentfile %d string readhexstring pop}\n",nx);
   fprintf(fp,"image\n");
  for (j=0; j<ny; j++)
    {
    k = 1;
    for (i=0; i < nx; i++)
      {
      icell = image[i + j * idim];
/* Conversion from 256**2 to 256 color scale: */
      gray = (int)((double)r[icell]/256.);
      if (gray < 16) fprintf(fp,"0%x",gray);
      else fprintf(fp,"%x",gray);
/* End of line every 24 codes (48 characters) */
      if (k == 24) { fprintf(fp,"\n");  k = 1;}
      else k++;
/* DEBUG
      if(i == nx/2) printf(" j= %d icell=%d, gray=%d \n",j,icell,gray);
*/
      }
/* End of line character at the end of each "image line" too: */
    if (k != 1) fprintf(fp,"\n");
    }
/* JLP93: I add drawaxes... */
  fprintf(fp,"drawaxes\n");
  fprintf(fp,"grestore\n");
  }
}
/**********************************************************************
*
* FUNCTION: PS_Image1_4bits
* Same as PS_Image1_8bits but 4 bit encoding only
*
* PURPOSE: Renders a gray shaded image.
*
* INPUT:  width,height = rectangle dimensions
*         nx,ny = image dimensions
*         image = pointer to integer image
*         fp = pointer to postscript file
*
*
* Same as PS_Image but uses LUT (red part only, since B&W). 
**********************************************************************/
void     PS_Image1_4bits(FILE *fp, double width, double height, int nx, int ny,
                         int idim, int *image, int *r, int *pst_lut)
{
register   int  i, j, k, icell;
int  gray;

if (fp != NULL)
  {
  fprintf(fp,"gsave\n");
  fprintf(fp,"%g %g scale\n",width,height);
/* 4 bit encoding */
  fprintf(fp,"%d %d 4 [%d 0 0 %d 0 0]\n",nx,ny,nx,ny);
  fprintf(fp,"{currentfile %d string readhexstring pop}\n",nx);
  fprintf(fp,"image\n");
  for (j=0; j<ny; j++)
    {
    k = 1;
    for (i=0; i < nx; i++)
      {
      icell = image[i + j * idim];
/* Conversion from 256**2 to 16 color scale: */
      gray = (int)((double)r[icell]/4096.);
      fprintf(fp,"%x",gray);
/* End of line every 48 codes (48 characters) */
      if (k == 48) { fprintf(fp,"\n");  k = 1;}
      else k++;
      }
/* End of line character at the end of each "image line" too: */
    if (k != 1) fprintf(fp,"\n");
    }
/* JLP93: add drawaxes... */
  fprintf(fp,"drawaxes\n");
  fprintf(fp,"grestore\n");
  }
}
/**********************************************************************
*
* FUNCTION: PS_ImageRGB
*
* PURPOSE: Renders a color image.
*
* INPUT:  width,height = rectangle dimensions
*         nx, ny = image dimensions
*         image = pointer to image 
*         lut = look up table
*         fp = pointer to postscript file
*
**********************************************************************/
void     PS_ImageRGB(FILE *fp, double width, double height, int nx, int ny, 
                     int *image, int lut[3][256])
{
register   int  i, j, k;
int  red, green ,blue;

if (fp != NULL)
  {
   fprintf(fp,"gsave\n");
   fprintf(fp,"%g %g scale\n",width,height);
/* 8 bit encoding */
   fprintf(fp,"%d %d 8 [%d 0 0 %d 0 0]\n",nx,ny,nx,ny);
   fprintf(fp,"{currentfile %d string readhexstring pop}\n",3*nx);
   fprintf(fp,"false 3 colorimage\n");
  for (j=0; j<ny; j++)
    {
    k = 1;
    for (i=0; i<nx; i++)
      {
      red = lut[0][image[i + j * nx]];
      if (red < 16)  fprintf(fp,"0%x",red);
      else  fprintf(fp,"%x",red);
      green = lut[1][image[i + j * nx]];
      if (green < 16)  fprintf(fp,"0%x",green);
      else  fprintf(fp,"%x",green);
      blue = lut[2][image[i + j * nx]];
      if (blue < 16)  fprintf(fp,"0%x",blue);
      else  fprintf(fp,"%x",blue);
      if (k == 8) { fprintf(fp,"\n");  k = 1;}
      else k++;
      }
    if (k != 1)  fprintf(fp,"\n");
    }
/* JLP93: I add drawaxes... */
   fprintf(fp,"drawaxes\n");
   fprintf(fp,"grestore\n");
  }
}
/**********************************************************************
*
* FUNCTION: PS_ImageRGB1
*
* PURPOSE: Renders a color image.
*
* INPUT:  width,height = rectangle dimensions
*         nx, ny = image dimensions
*         image = pointer to image 
*         lut = look up table
*         fp = pointer to postscript file
*
* Same as PS_ImageRBG but with "r,g,b" instead of "lut"
**********************************************************************/
void PS_ImageRGB1(FILE *fp, double width, double height, int nx, int ny,
                  int idim, int *image, int *r, int *g, int *b)
{
register   int  i, j, k, icell;
int  red, green, blue;

if (fp != NULL)
  {
   fprintf(fp,"gsave\n");
   fprintf(fp,"%g %g scale\n",width,height);
/* 8 bit encoding */
   fprintf(fp,"%d %d 8 [%d 0 0 %d 0 0]\n",nx,ny,nx,ny);
   fprintf(fp,"{currentfile %d string readhexstring pop}\n",3*nx);
   fprintf(fp,"false 3 colorimage\n");
  for (j=0; j<ny; j++)
    {
    k = 1;
    for (i = 0; i < nx; i++)
      {
      icell = image[i + j * idim];
/* Conversion from 256**2 to 256 color scale: */
      red = (int)((double)r[icell]/256.);
      if (red < 16)  fprintf(fp,"0%x",red);
      else  fprintf(fp,"%x",red);
      green = (int)((double)g[icell]/256.);
      if (green < 16)  fprintf(fp,"0%x",green);
      else  fprintf(fp,"%x",green);
      blue = (int)((double)b[icell]/256.);
      if (blue < 16)  fprintf(fp,"0%x",blue);
      else  fprintf(fp,"%x",blue);
      if (k == 8) { fprintf(fp,"\n");  k = 1;}
      else k++;
/* DEBUG
      if(i == nx/2) printf(" icell=%d, red=%d green=%d blue=%d \n",
                     icell,red,green,blue);
*/
      }
    if (k != 1)  fprintf(fp,"\n");
    }
/* JLP93: I add drawaxes... */
   fprintf(fp,"drawaxes\n");
   fprintf(fp,"grestore\n");
  }
}
/**********************************************************************
*
* FUNCTION: PS_Isoc
*
* PURPOSE: Draws
*
* INPUT:  width,height = rectangle dimensions
*         nx, ny = image dimensions
*         sobel = pointer to contour image
*         fp = pointer to postscript file
*
**********************************************************************/
void PS_Isoc(FILE *fp, double width, double height, int ny, int nx, int **sobel)
{
register   int  i, j;
int  b1, b2, b3, b4, h1, h2;
/*
int  x1, x2, y1, y2;
*/

if (fp != NULL)
  {
   fprintf(fp,"newpath\n");
   fprintf(fp,"%g %g scale\n",width,height);
   fprintf(fp,"%d %d 1 [%d 0 0 %d 0 0]{<\n",nx,ny,nx,ny);
  for (i=0; i<ny; i++)
    {
    for (j=0; j<nx; j+=8) {
      b1 = sobel[i][j+0]; b2 = sobel[i][j+1]; 
      b3 = sobel[i][j+2]; b4 = sobel[i][j+3];
      h1=8*b1+4*b2+2*b3+b4;
      b1 = sobel[i][j+4]; b2 = sobel[i][j+5]; 
      b3 = sobel[i][j+6]; b4 = sobel[i][j+7];
      h2=8*b1+4*b2+2*b3+b4;
       fprintf(fp,"%x%x",15-h1,15-h2);
      }
    if (i != (ny-1))  fprintf(fp,"\n");
    }
   fprintf(fp,">} image\nstroke\n");
   fprintf(fp,"newpath\n");
   fprintf(fp,"1 %g div 1 scale\n",width);
   fprintf(fp,"1 %g div 1 exch scale\n",height);
   fprintf(fp,"stroke\n");

/********
   fprintf(fp,"0.4 setlinewidth\n");
   fprintf(fp,"0 0 0 setrgbcolor\n");
   fprintf(fp,"0.5 0.5 scale\n");
  for (i=2; i<(ny-2); i+=2)
  for (j=2; j<(nx-2); j+=2)
    {
    if (sobel[i][j] == 1)
      {
      x1 = i;
      y1 = j;
      if (sobel[i-2][j] == 1)  
        {
        x2 = i-2;
        y2 = j;
         fprintf(fp,"n %d %d m  %d %d l s\n",x1,y1,x2,y2);
        }
      if (sobel[i+2][j] == 1)  
        {
        x2 = i+2;
        y2 = j;
         fprintf(fp,"n %d %d m  %d %d l s\n",x1,y1,x2,y2);
        }
      if (sobel[i][j-2] == 1)  
        {
        x2 = i;
        y2 = j-2;
         fprintf(fp,"n %d %d m  %d %d l s\n",x1,y1,x2,y2);
        }
      if (sobel[i][j+2] == 1)  
        {
        x2 = i;
        y2 = j+2;
         fprintf(fp,"n %d %d m  %d %d l s\n",x1,y1,x2,y2);
        }
      if (sobel[i-2][j-2] == 1)  
        {
        x2 = i-2;
        y2 = j-2;
         fprintf(fp,"n %d %d m  %d %d l s\n",x1,y1,x2,y2);
        }
      if (sobel[i+2][j-2] == 1)  
        {
        x2 = i+2;
        y2 = j-2;
         fprintf(fp,"n %d %d m  %d %d l s\n",x1,y1,x2,y2);
        }
      if (sobel[i-2][j+2] == 1)  
        {
        x2 = i-2;
        y2 = j+2;
         fprintf(fp,"n %d %d m  %d %d l s\n",x1,y1,x2,y2);
        }
      if (sobel[i+2][j+2] == 1)  
        {
        x2 = i+2;
        y2 = j+2;
         fprintf(fp,"n %d %d m  %d %d l s\n",x1,y1,x2,y2);
        }
      }
    }
   fprintf(fp,"2.0 2.0 scale\n");
********/
  }
}
/**********************************************************************
*
* FUNCTION: PS_Bar3d
*
* PURPOSE: Draws 3D histogram.
*
* INPUT:  width,height = rectangle dimensions
*         nx, ny = image dimensions
*         x,y,z = histogram
*         theta,phi = view point polar coordinates
*         fp = pointer to postscript file
*
**********************************************************************/
#define  DEGTORAD        0.017453293
#define  XWINDOWING(wx)  ((wx-w1)*(v2-v1)/(w2-w1) + v1)
#define  YWINDOWING(wy)  ((wy-w3)*(v4-v3)/(w4-w3) + v3)
#define  MAXIMUM(x,y)    (((x) > (y)) ? (x) : (y))
#define  MINIMUM(x,y)    (((x) < (y)) ? (x) : (y))

void PS_Bar3d(FILE *fp, double width, double height, int ny, int nx,
              double **z, double *x, double *y, double theta, double phi,
              int linepattern, double linewidth, 
              double red, double green, double blue)
{
register  int            i, j;
double          v1, v2, v3, v4, w1, w2, w3, w4, wx, wy, bas, haut;
double          xi1, xi2, xi3, xi4, yi1, yi2, yi3, yi4;
double          xs1, xs2, xs3, xs4, ys1, ys2, ys3, ys4;
double          zmin, zmax, sav;

if ((0.0 < theta) && (theta <= 90.0))
  {
  for (i=0; i<(ny/2); i++)
  for (j=0; j<(nx/2); j++)
    {
    sav = z[i][j];  
    z[i][j] = z[ny-1-i][nx-1-j];
    z[ny-1-i][nx-1-j] = sav;
    }
  theta += 180.0;
  }
if ((90.0 < theta) && (theta <= 180.0))
  {
  for (i=0; i<(ny/2); i++)
  for (j=0; j<nx; j++)
    {
    sav = z[i][j];
    z[i][j] = z[ny-1-i][j];
    z[ny-1-i][j] = sav;
    }
  theta += 90.0;
  }
if ((270.0 < theta) && (theta < 360.0))
  {
  for (i=0; i<(ny/2); i++)
  for (j=0; j<(nx/2); j++)
    {
    sav = z[i][j];
    z[i][j] = z[i][nx-1-j];
    z[i][nx-1-j] = sav;
    }
  theta -= 90.0;
  }

zmin = zmax = z[0][0];
for (i=0; i<ny; i++)
for (j=0; j<nx; j++)
  {
  zmin = MINIMUM(z[i][j],zmin);
  zmax = MAXIMUM(z[i][j],zmax);
  }
if (zmin < 0.0) bas = 0.0;
else bas = MAXIMUM(0.0,zmin - 0.1*(zmax-zmin));

w1 = w2 = -x[i]*sin(DEGTORAD*theta) 
         + y[j]*cos(DEGTORAD*theta);
w3 = w4  = -x[i]*sin(DEGTORAD*phi)*cos(DEGTORAD*theta) 
          - y[j]*sin(DEGTORAD*phi)*sin(DEGTORAD*theta)
          + z[i][j]*cos(DEGTORAD*phi);
for (i=0; i<ny; i++)
for (j=0; j<nx; j++)
  {
  haut = z[i][j];
  /* top: (i,j) */
  wx = -x[i]*sin(DEGTORAD*theta) 
      + y[j]*cos(DEGTORAD*theta);
  wy = -x[i]*sin(DEGTORAD*phi)*cos(DEGTORAD*theta) 
      - y[j]*sin(DEGTORAD*phi)*sin(DEGTORAD*theta)
      + haut*cos(DEGTORAD*phi);
  w1 = MINIMUM(wx,w1); 
  w2 = MAXIMUM(wx,w2); 
  w3 = MINIMUM(wy,w3); 
  w4 = MAXIMUM(wy,w4); 
  /* top: (i+1,j) */
  wx = -x[i+1]*sin(DEGTORAD*theta)
      + y[j]*cos(DEGTORAD*theta);
  wy = -x[i+1]*sin(DEGTORAD*phi)*cos(DEGTORAD*theta)
      - y[j]*sin(DEGTORAD*phi)*sin(DEGTORAD*theta)
      + haut*cos(DEGTORAD*phi);
  w1 = MINIMUM(wx,w1);
  w2 = MAXIMUM(wx,w2);
  w3 = MINIMUM(wy,w3);
  w4 = MAXIMUM(wy,w4);
  /* top: (i+1,j+1) */
  wx = -x[i+1]*sin(DEGTORAD*theta)
      + y[j+1]*cos(DEGTORAD*theta);
  wy = -x[i+1]*sin(DEGTORAD*phi)*cos(DEGTORAD*theta)
      - y[j+1]*sin(DEGTORAD*phi)*sin(DEGTORAD*theta)
      + haut*cos(DEGTORAD*phi);
  w1 = MINIMUM(wx,w1);
  w2 = MAXIMUM(wx,w2);
  w3 = MINIMUM(wy,w3);
  w4 = MAXIMUM(wy,w4);
  /* top: (i,j+1) */
  wx = -x[i]*sin(DEGTORAD*theta)
      + y[j+1]*cos(DEGTORAD*theta);
  wy = -x[i]*sin(DEGTORAD*phi)*cos(DEGTORAD*theta)
      - y[j+1]*sin(DEGTORAD*phi)*sin(DEGTORAD*theta)
      + haut*cos(DEGTORAD*phi);
  w1 = MINIMUM(wx,w1);
  w2 = MAXIMUM(wx,w2);
  w3 = MINIMUM(wy,w3);
  w4 = MAXIMUM(wy,w4);
  /* bottom: (i,j) */
  wx = -x[i]*sin(DEGTORAD*theta)
      + y[j]*cos(DEGTORAD*theta);
  wy = -x[i]*sin(DEGTORAD*phi)*cos(DEGTORAD*theta)
      - y[j]*sin(DEGTORAD*phi)*sin(DEGTORAD*theta)
      + bas*cos(DEGTORAD*phi);
  w1 = MINIMUM(wx,w1);
  w2 = MAXIMUM(wx,w2);
  w3 = MINIMUM(wy,w3);
  w4 = MAXIMUM(wy,w4);
  /* bottom: (i+1,j) */
  wx = -x[i+1]*sin(DEGTORAD*theta)
      + y[j]*cos(DEGTORAD*theta);
  wy = -x[i+1]*sin(DEGTORAD*phi)*cos(DEGTORAD*theta)
      - y[j]*sin(DEGTORAD*phi)*sin(DEGTORAD*theta)
      + bas*cos(DEGTORAD*phi);
  w1 = MINIMUM(wx,w1);
  w2 = MAXIMUM(wx,w2);
  w3 = MINIMUM(wy,w3);
  w4 = MAXIMUM(wy,w4);
  /* bottom: (i+1,j+1) */
  wx = -x[i+1]*sin(DEGTORAD*theta)
      + y[j+1]*cos(DEGTORAD*theta);
  wy = -x[i+1]*sin(DEGTORAD*phi)*cos(DEGTORAD*theta)
      - y[j+1]*sin(DEGTORAD*phi)*sin(DEGTORAD*theta)
      + bas*cos(DEGTORAD*phi);
  w1 = MINIMUM(wx,w1);
  w2 = MAXIMUM(wx,w2);
  w3 = MINIMUM(wy,w3);
  w4 = MAXIMUM(wy,w4);
  /* bottom: (i,j+1) */
  wx = -x[i]*sin(DEGTORAD*theta)
      + y[j+1]*cos(DEGTORAD*theta);
  wy = -x[i]*sin(DEGTORAD*phi)*cos(DEGTORAD*theta)
      - y[j+1]*sin(DEGTORAD*phi)*sin(DEGTORAD*theta)
      + bas*cos(DEGTORAD*phi);
  w1 = MINIMUM(wx,w1);
  w2 = MAXIMUM(wx,w2);
  w3 = MINIMUM(wy,w3);
  w4 = MAXIMUM(wy,w4);
  }

v1 = 0.0;
v2 = width;
v3 = 0.0;
v4 = height;

for (i=0; i<ny; i++)
for (j=0; j<nx; j++)
  {
  haut = z[i][j];
  /* top: (i,j) */
  wx = -x[i]*sin(DEGTORAD*theta) 
      + y[j]*cos(DEGTORAD*theta);
  wy = -x[i]*sin(DEGTORAD*phi)*cos(DEGTORAD*theta) 
      - y[j]*sin(DEGTORAD*phi)*sin(DEGTORAD*theta)
      + haut*cos(DEGTORAD*phi);
  xs1 = XWINDOWING(wx);
  ys1 = YWINDOWING(wy);
  /* top: (i+1,j) */
  wx = -x[i+1]*sin(DEGTORAD*theta)
      + y[j]*cos(DEGTORAD*theta);
  wy = -x[i+1]*sin(DEGTORAD*phi)*cos(DEGTORAD*theta)
      - y[j]*sin(DEGTORAD*phi)*sin(DEGTORAD*theta)
      + haut*cos(DEGTORAD*phi);
  xs2 = XWINDOWING(wx);
  ys2 = YWINDOWING(wy);
  /* top: (i+1,j+1) */
  wx = -x[i+1]*sin(DEGTORAD*theta)
      + y[j+1]*cos(DEGTORAD*theta);
  wy = -x[i+1]*sin(DEGTORAD*phi)*cos(DEGTORAD*theta)
      - y[j+1]*sin(DEGTORAD*phi)*sin(DEGTORAD*theta)
      + haut*cos(DEGTORAD*phi);
  xs3 = XWINDOWING(wx);
  ys3 = YWINDOWING(wy);
  /* top: (i,j+1) */
  wx = -x[i]*sin(DEGTORAD*theta)
      + y[j+1]*cos(DEGTORAD*theta);
  wy = -x[i]*sin(DEGTORAD*phi)*cos(DEGTORAD*theta)
      - y[j+1]*sin(DEGTORAD*phi)*sin(DEGTORAD*theta)
      + haut*cos(DEGTORAD*phi);
  xs4 = XWINDOWING(wx);
  ys4 = YWINDOWING(wy);
  /* bottom: (i,j) */
  wx = -x[i]*sin(DEGTORAD*theta)
      + y[j]*cos(DEGTORAD*theta);
  wy = -x[i]*sin(DEGTORAD*phi)*cos(DEGTORAD*theta)
      - y[j]*sin(DEGTORAD*phi)*sin(DEGTORAD*theta)
      + bas*cos(DEGTORAD*phi);
  xi1 = XWINDOWING(wx);
  yi1 = YWINDOWING(wy);
  /* bottom: (i+1,j) */
  wx = -x[i+1]*sin(DEGTORAD*theta)
      + y[j]*cos(DEGTORAD*theta);
  wy = -x[i+1]*sin(DEGTORAD*phi)*cos(DEGTORAD*theta)
      - y[j]*sin(DEGTORAD*phi)*sin(DEGTORAD*theta)
      + bas*cos(DEGTORAD*phi);
  xi2 = XWINDOWING(wx);
  yi2 = YWINDOWING(wy);
  /* bottom: (i+1,j+1) */
  wx = -x[i+1]*sin(DEGTORAD*theta)
      + y[j+1]*cos(DEGTORAD*theta);
  wy = -x[i+1]*sin(DEGTORAD*phi)*cos(DEGTORAD*theta)
      - y[j+1]*sin(DEGTORAD*phi)*sin(DEGTORAD*theta)
      + bas*cos(DEGTORAD*phi);
  xi3 = XWINDOWING(wx);
  yi3 = YWINDOWING(wy);
  /* bottom: (i,j+1) */
  wx = -x[i]*sin(DEGTORAD*theta)
      + y[j+1]*cos(DEGTORAD*theta);
  wy = -x[i]*sin(DEGTORAD*phi)*cos(DEGTORAD*theta)
      - y[j+1]*sin(DEGTORAD*phi)*sin(DEGTORAD*theta)
      + bas*cos(DEGTORAD*phi);
  xi4 = XWINDOWING(wx);
  yi4 = YWINDOWING(wy);

   fprintf(fp,"newpath\n");
   fprintf(fp,"1 1 1 setrgbcolor %g line%d\n",linewidth,linepattern);
   fprintf(fp,"%.2f %.2f m ",xs1,ys1);
   fprintf(fp,"%.2f %.2f l ",xs2,ys2);
   fprintf(fp,"%.2f %.2f l ",xs3,ys3);
   fprintf(fp,"%.2f %.2f l ",xs4,ys4);
   fprintf(fp,"closepath fill\nstroke\n");
   fprintf(fp,"newpath\n");
   fprintf(fp,"%g %g %g setrgbcolor %g line%d\n",red,green,blue,linewidth,linepattern);
   fprintf(fp,"%.2f %.2f m ",xs1,ys1);
   fprintf(fp,"%.2f %.2f l ",xs2,ys2);
   fprintf(fp,"%.2f %.2f l ",xs3,ys3);
   fprintf(fp,"%.2f %.2f l ",xs4,ys4);
   fprintf(fp,"closepath\nstroke\n");

   fprintf(fp,"newpath\n");
   fprintf(fp,"1 1 1 setrgbcolor %g line%d\n",linewidth,linepattern);
   fprintf(fp,"%.2f %.2f m ",xs3,ys3);
   fprintf(fp,"%.2f %.2f l ",xi3,yi3);
   fprintf(fp,"%.2f %.2f l ",xi2,yi2);
   fprintf(fp,"%.2f %.2f l ",xs2,ys2);
   fprintf(fp,"closepath fill\nstroke\n");
   fprintf(fp,"newpath\n");
   fprintf(fp,"%g %g %g setrgbcolor %g line%d\n",red,green,blue,linewidth,linepattern);
   fprintf(fp,"%.2f %.2f m ",xs3,ys3);
   fprintf(fp,"%.2f %.2f l ",xi3,yi3);
   fprintf(fp,"%.2f %.2f l ",xi2,yi2);
   fprintf(fp,"%.2f %.2f l ",xs2,ys2);
   fprintf(fp,"closepath\nstroke\n");

   fprintf(fp,"newpath\n");
   fprintf(fp,"1 1 1 setrgbcolor %g line%d\n",linewidth,linepattern);
   fprintf(fp,"%.2f %.2f m ",xs3,ys3);
   fprintf(fp,"%.2f %.2f l ",xi3,yi3);
   fprintf(fp,"%.2f %.2f l ",xi4,yi4);
   fprintf(fp,"%.2f %.2f l ",xs4,ys4);
   fprintf(fp,"closepath fill\nstroke\n");
   fprintf(fp,"newpath\n");
   fprintf(fp,"%g %g %g setrgbcolor %g line%d\n",red,green,blue,linewidth,linepattern);
   fprintf(fp,"%.2f %.2f m ",xs3,ys3);
   fprintf(fp,"%.2f %.2f l ",xi3,yi3);
   fprintf(fp,"%.2f %.2f l ",xi4,yi4);
   fprintf(fp,"%.2f %.2f l ",xs4,ys4);
   fprintf(fp,"closepath\nstroke\n");
  }
}
/**********************************************************************/
/*                                                                    */
/* FUNCTION: PS_Pixel_LLlabelling                                     */
/*                                                                    */
/* PURPOSE: Draws labelled box around an image (pixels labelling).    */
/*                                                                    */
/* INPUT:  x_ll,y_ll,x_ur,y_ur = rectangle coordinates                */
/*         nbr_col,cols = number of columns and columns step on x axis*/
/*         xlegend = legend on x axis                                 */
/*         nbr_lin,lins = number of lines and lines step on y axis    */
/*         ylegend = legend on y axis                                 */
/*         fontname,fontsize = font used                              */
/*         fp = pointer to postscript file                            */
/*                                                                    */
/**********************************************************************/
void PS_Pixel_LLlabelling(FILE *fp, double x_ll, double y_ll, double x_ur, 
                          double y_ur, int nbr_col, int cols, char *xlegend,
                          int nbr_lin, int lins, char *ylegend, char *fontname,
                          double fontsize, double linewidth, double red,
                          double green, double blue)
{
int     lin = 0, col = 0;
double   x, y, xs, ys;

if (fp != NULL)
  {
   fprintf(fp,"newpath\n");
   fprintf(fp,"/%s findfont %g scalefont setfont\n",fontname,fontsize);
   fprintf(fp,"%g line1\n",linewidth);
   fprintf(fp,"%g %g %g setrgbcolor\n",red,green,blue);

  xs = (x_ur-x_ll)*((double) cols)/((double) nbr_col);  xs /= 2.0;
   fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x_ll,y_ll,x_ll,y_ll-6.0);
  x = x_ll + xs;
  while (x < x_ur)
    {
     fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x,y_ll,x,y_ll-4.0);
    x += 2.0*xs;
    }
  x = x_ll + 2.0*xs;
  while (x < x_ur)
    {
     fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x,y_ll,x,y_ll-6.0);
    x += 2.0*xs;
    }
   fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x_ur,y_ll,x_ur,y_ll-6.0);
   fprintf(fp,"%.2f %.2f m (%d) cs\n",x_ll,y_ll-15.0,col);
  x = x_ll + 2.0*xs;
  while (x < x_ur)
    {
    col += cols;
     fprintf(fp,"%.2f %.2f m (%d) cs\n",x,y_ll-15.0,col);
    x += 2.0*xs;
    }
   fprintf(fp,"%.2f %.2f m (%d) cs\n",x_ur,y_ll-15.0,nbr_col);
  if (xlegend != NULL)
     fprintf(fp,"%g %g m (%s) cs\n",(x_ur+x_ll)/2.0,y_ll-26.5,xlegend);

  ys = (y_ur-y_ll)*((double) lins)/((double) nbr_lin);  ys /= 2.0;
   fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x_ll,y_ll,x_ll-6.0,y_ll);
  y = y_ll + ys;
  while (y < y_ur)
    {
     fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x_ll,y,x_ll-4.0,y);
    y += 2.0*ys;
    }
  y = y_ll + 2.0*ys;
  while (y < y_ur)
    {
     fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x_ll,y,x_ll-6.0,y);
    y += 2.0*ys;
    }
   fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x_ll,y_ur,x_ll-6.0,y_ur);
   fprintf(fp,"90 rotate\n");
   fprintf(fp,"%.2f %.2f m (%d) cs\n",y_ll,x_ll+8.0,lin);
  y = y_ll + 2.0*ys;
  while (y < y_ur)
    {
    lin += lins;
     fprintf(fp,"%.2f %.2f m (%d) cs\n",y,x_ll+8.0,lin);
    y += 2.0*ys;
    }
   fprintf(fp,"%.2f %.2f m (%d) cs\n",y_ur,x_ll+8.0,nbr_lin);
  if (ylegend != NULL)
     fprintf(fp,"%g %g m (%s) cs\n",(y_ll+y_ur)/2.0,x_ll+21.0,ylegend);
   fprintf(fp,"-90 rotate\n");

   fprintf(fp,"stroke\n");
  }
}

/**********************************************************************
*
* FUNCTION: PS_Pixel_URlabelling
*
* PURPOSE: Draws labelled box around an image (pixels labelling).
*
* INPUT:  x_ll,y_ll,x_ur,y_ur = rectangle coordinates
*         nbr_col,cols = number of columns and columns step on x axis
*         xlegend = legend on x axis
*         nbr_lin,lins = number of lines and lines step on y axis
*         ylegend = legend on y axis
*         fontname,fontsize = font used
*         fp = pointer to postscript file
*
**********************************************************************/
void PS_Pixel_URlabelling(FILE *fp, double x_ll, double y_ll, double x_ur, 
                          double y_ur, int nbr_col, int cols, char *xlegend,
                          int nbr_lin, int lins, char *ylegend, char *fontname,
                          double fontsize, double linewidth, double red,
                          double green, double blue)
{
int     lin = 0, col = 0;
double   x, y, xs, ys;

if (fp != NULL)
  {
   fprintf(fp,"newpath\n");
   fprintf(fp,"/%s findfont %g scalefont setfont\n",fontname,fontsize);
   fprintf(fp,"%g line1\n",linewidth);
   fprintf(fp,"%g %g %g setrgbcolor\n",red,green,blue);

  xs = (x_ur-x_ll)*((double) cols)/((double) nbr_col);  xs /= 2.0;
   fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x_ll,y_ur,x_ll,y_ur+6.0);
  x = x_ll + xs;
  while (x < x_ur)
    {
     fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x,y_ur,x,y_ur+4.0);
    x += 2.0*xs;
    }
  x = x_ll + 2.0*xs;
  while (x < x_ur)
    {
     fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x,y_ur,x,y_ur+6.0);
    x += 2.0*xs;
    }
   fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x_ur,y_ur,x_ur,y_ur+6.0);
   fprintf(fp,"%.2f %.2f m (%d) cs\n",x_ll,y_ur+8.0,col);
  x = x_ll + 2.0*xs;
  while (x < x_ur)
    {
    col += cols;
     fprintf(fp,"%.2f %.2f m (%d) cs\n",x,y_ur+8.0,col);
    x += 2.0*xs;
    }
   fprintf(fp,"%.2f %.2f m (%d) cs\n",x_ur,y_ur+8.0,nbr_col);
  if (xlegend != NULL)
     fprintf(fp,"%g %g m (%s) cs\n",(x_ur+x_ll)/2.0,y_ur+21.0,xlegend);

  ys = (y_ur-y_ll)*((double) lins)/((double) nbr_lin);  ys /= 2.0;
   fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x_ur,y_ll,x_ur+6.0,y_ll);
  y = y_ll + ys;
  while (y < y_ur)
    {
     fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x_ur,y,x_ur+4.0,y);
    y += 2.0*ys;
    }
  y = y_ll + 2.0*ys;
  while (y < y_ur)
    {
     fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x_ur,y,x_ur+6.0,y);
    y += 2.0*ys;
    }
   fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x_ur,y_ur,x_ur+6.0,y_ur);
   fprintf(fp,"90 rotate\n");
   fprintf(fp,"%.2f %.2f m (%d) cs\n",y_ll,-x_ur-15.0,lin);
  y = y_ll + 2.0*ys;
  while (y < y_ur)
    {
    lin += lins;
     fprintf(fp,"%.2f %.2f m (%d) cs\n",y,-x_ur-15.0,lin);
    y += 2.0*ys;
    }
   fprintf(fp,"%.2f %.2f m (%d) cs\n",y_ur,-x_ur-15.0,nbr_lin);
  if (ylegend != NULL)
     fprintf(fp,"%g %g m (%s) cs\n",(y_ll+y_ur)/2.0,-x_ur-26.5,ylegend);
   fprintf(fp,"-90 rotate\n");

   fprintf(fp,"stroke\n");
  }
}
/************************************************************************
*
* FUNCTION: PS_Axis_LLlabelling
*
* PURPOSE: Draws labelled box.
*
* INPUT:  x_ll,y_ll,x_ur,y_ur = rectangle coordinates
*         xmin,xmax = min and max values on x axis
*         xlegend = legend on x axis
*         ymin,ymax = min and max values on y axis
*         ylegend = legend on y axis
*         fontname,fontsize = font used
*         fp = pointer to postscript file
*
************************************************************************/
void PS_Axis_LLlabelling(FILE *fp, double x_ll, double y_ll, double x_ur, 
                          double y_ur, double xmin, double xmax, char *xlegend,
                          double ymin, double ymax, char *ylegend, char *fontname,
                          double fontsize, double linewidth, double red,
                          double green, double blue)
{
double   x, y, ax, bx, ay, by;
double   expx, manx, pasx, decx, expy, many, pasy, decy;

if (fp != NULL)
  {
   fprintf(fp,"newpath\n");
   fprintf(fp,"/%s findfont %g scalefont setfont\n",fontname,fontsize);
   fprintf(fp,"%g line1\n",linewidth);
   fprintf(fp,"%g %g %g setrgbcolor\n",red,green,blue);

  if ((xmax != 0.0) || (xmin != 0.0))
    {
    ax = (x_ur-x_ll)/(xmax-xmin);  bx = (x_ll*xmax-x_ur*xmin)/(xmax-xmin);
    expx = floor(log10(xmax-xmin));
    manx = (xmax-xmin)/(pow(10.,expx));
    pasx = ceil(manx)*pow(10.,expx-1);
    decx = pasx*ceil(xmin/pasx) - xmin;
    for (x=(xmin+decx); x<=xmax; x+=pasx)
       fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",ax*x+bx,y_ll,ax*x+bx,y_ll-6.0);
    for (x=(xmin+decx); x<=xmax; x+=2*pasx)
       fprintf(fp,"%.2f %.2f m (%g) cs\n",ax*x+bx,y_ll-15.0,x);
    if (xlegend != NULL)
       fprintf(fp,"%g %g m (%s) cs\n",(x_ur+x_ll)/2.0,y_ll-26.5,xlegend);
    }

  if ((ymax != 0.0) || (ymin != 0.0))
    {
     fprintf(fp,"90 rotate\n");
    ay = (y_ur-y_ll)/(ymax-ymin);  by = (y_ll*ymax-y_ur*ymin)/(ymax-ymin);
    expy = floor(log10(ymax-ymin));
    many = (ymax-ymin)/(pow(10.,expy));
    pasy = ceil(many)*pow(10.,expy-1);
    decy = pasy*ceil(ymin/pasy) - ymin;
    for (y=(ymin+decy); y<=ymax; y+=pasy)
       fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",ay*y+by,x_ll,ay*y+by,x_ll+6.0);
    for (y=(ymin+decy); y<=ymax; y+=2*pasy)
       fprintf(fp,"%.2f %.2f m (%g) cs\n",ay*y+by,x_ll+8.0,y);
    if (ylegend != NULL)
       fprintf(fp,"%g %g m (%s) cs\n",(y_ll+y_ur)/2.0,x_ll+21.0,ylegend);
     fprintf(fp,"-90 rotate\n");
    }

   fprintf(fp,"stroke\n");
  }
}
/**********************************************************************
*
* FUNCTION: PS_Axis_URlabelling
*
* PURPOSE: Draws labelled box.
*
* INPUT:  x_ll,y_ll,x_ur,y_ur = rectangle coordinates
*         xmax,xmin = min and max values on x axis
*         xlegend = legend on x axis
*         ymax,ymin = min and max values on y axis
*         ylegend = legend on y axis
*         fontname,fontsize = font used
*         fp = pointer to postscript file
*
**********************************************************************/
void PS_Axis_URlabelling(FILE *fp, double x_ll, double y_ll, double x_ur, 
                         double y_ur, double xmin, double xmax, char *xlegend,
                         double ymin, double ymax, char *ylegend, char *fontname,
                         double fontsize, double linewidth, double red,
                         double green, double blue)
{
double   x, y, ax, bx, ay, by;
double   expx, manx, pasx, decx, expy, many, pasy, decy;

if (fp != NULL)
  {
   fprintf(fp,"newpath\n");
   fprintf(fp,"/%s findfont %g scalefont setfont\n",fontname,fontsize);
   fprintf(fp,"%g line1\n",linewidth);
   fprintf(fp,"%g %g %g setrgbcolor\n",red,green,blue);

  if ((xmax != 0.0) || (xmin != 0.0))
    {
    ax = (x_ur-x_ll)/(xmax-xmin);  bx = (x_ll*xmax-x_ur*xmin)/(xmax-xmin);
    expx = floor(log10(xmax-xmin));
    manx = (xmax-xmin)/(pow(10.,expx));
    pasx = ceil(manx)*pow(10.,expx-1);
    decx = pasx*ceil(xmin/pasx) - xmin;
    for (x=(xmin+decx); x<=xmax; x+=pasx)
       fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",ax*x+bx,y_ur,ax*x+bx,y_ur+6.0);
    for (x=(xmin+decx); x<=xmax; x+=2*pasx)
       fprintf(fp,"%.2f %.2f m (%g) cs\n",ax*x+bx,y_ur+8.0,x);
    if (xlegend != NULL)
       fprintf(fp,"%g %g m (%s) cs\n",(x_ur+x_ll)/2.0,y_ur+21.0,xlegend);
    }

  if ((ymax != 0.0) || (ymin != 0.0))
    {
     fprintf(fp,"90 rotate\n");
    expy = floor(log10(ymax-ymin));
    many = (ymax-ymin)/(pow(10.,expy));
    pasy = ceil(many)*pow(10.,expy-1);
    decy = pasy*ceil(ymin/pasy) - ymin;
    ay = (y_ur-y_ll)/(ymax-ymin);  by = (y_ll*ymax-y_ur*ymin)/(ymax-ymin);
    for (y=(ymin+decy); y<=ymax; y+=pasy)
       fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",ay*y+by,-x_ur,ay*y+by,-x_ur-6.0);
    for (y=(ymin+decy); y<=ymax; y+=2*pasy)
       fprintf(fp,"%.2f %.2f m (%g) cs\n",ay*y+by,-x_ur-15.0,y);
    if (ylegend != NULL)
       fprintf(fp,"%g %g m (%s) cs\n",(y_ll+y_ur)/2.0,-x_ur-26.5,ylegend);
     fprintf(fp,"-90 rotate\n");
    }

   fprintf(fp,"stroke\n");
  }
}
/**********************************************************************/
/*                                                                    */
/* FUNCTION: PS_Angle_LLlabelling                                     */
/*                                                                    */
/* PURPOSE: Draws labelled box.                                       */
/*                                                                    */
/* INPUT:  x_ll,y_ll,x_ur,y_ur = rectangle coordinates                */
/*         xmax,xmin = min and max values on x axis                   */
/*         xlegend = legend on x axis                                 */
/*         ymax,ymin = min and max values on y axis                   */
/*         ylegend = legend on y axis                                 */
/*         fontname,fontsize = font used                              */
/*         fp = pointer to postscript file                            */
/*                                                                    */
/**********************************************************************/
void PS_Angle_LLlabelling(FILE *fp, double x_ll, double y_ll, double x_ur, 
                          double y_ur, int col, double rx, int a1, int a2, 
                          double a3, char *xlegend, int lin, double ry,
                          int d1, int d2, double d3, char *ylegend, 
                          char *fontname, double fontsize, double linewidth,
                          double red, double green, double blue)
{
int     aa1, aa2, dd1, dd2;
double   dx, dy, xx, yy, aa3, dd3;
double   xc, yc, width, height, pxlx, pxly;

if (fp != NULL)
  {
  width = x_ur-x_ll;
  height = y_ur-y_ll;
  pxlx = width/((double) col); /* inch */
  pxly = height/((double) lin); /* inch */
  xc = width/2.0 + 0.5*pxlx;
  yc = height/2.0 + 0.5*pxly;
  dx = (a3 - floor(a3))*pxlx/rx;
  dy = -(d3 - 10.0*floor(d3/10.0))*pxly/ry;

   fprintf(fp,"newpath\n");
   fprintf(fp,"/%s findfont %g scalefont setfont\n",fontname,fontsize);
   fprintf(fp,"%g line1\n",linewidth);
   fprintf(fp,"%g %g %g setrgbcolor\n",red,green,blue);

  xx = xc+dx;
  aa1 = a1; aa2 = a2; aa3 = floor(a3);
  if (aa3 == 60.0)  {aa3 = 0.0;  aa2++;}
  if (aa2 == 60)  {aa2 = 0;  aa1++;}
  while(1)
    {
     fprintf(fp,"%g %g m %g %g l\n",xx,y_ll,xx,y_ll-6.0);
     fprintf(fp,"%g %g m (%d %d %g) cs\n",xx,y_ll-15.0,aa1,aa2,aa3);
    aa3 += 1.0;
    if (aa3 == 60.0)  {aa3 = 0.0;  aa2++;}
    if (aa2 == 60)  {aa2 = 0;  aa1++;}
    xx -= 0.5*pxlx/rx;
    if (xx < x_ll)  break;
     fprintf(fp,"%g %g m %g %g l\n",xx,y_ll,xx,y_ll-4.0);
    xx -= 0.5*pxlx/rx;
    if (xx < x_ll)  break;
    }
  xx = xc+dx;
  aa1 = a1; aa2 = a2; aa3 = floor(a3);
  if (aa3 == 60.0)  {aa3 = 0.0;  aa2++;}
  if (aa2 == 60)  {aa2 = 0;  aa1++;}
  while(1)
    {
    if (aa3 != floor(a3))
      {
       fprintf(fp,"%g %g m %g %g l\n",xx,y_ll,xx,y_ll-6.0);
       fprintf(fp,"%g %g m (%d %d %g) cs\n",xx,y_ll-15.0,aa1,aa2,aa3);
      }
    aa3 -= 1.0;
    if (aa3 == -1.0)  {aa3 = 59.0;  aa2--;}
    if (aa2 == -1)  {aa2 = 59;  aa1--;}
    xx += 0.5*pxlx/rx;
    if (xx > x_ur)  break;
     fprintf(fp,"%g %g m %g %g l\n",xx,y_ll,xx,y_ll-4.0);
    xx += 0.5*pxlx/rx;
    if (xx > x_ur)  break;
    }
  if (xlegend != NULL)
     fprintf(fp,"%g %g m (%s) cs\n",(x_ur+x_ll)/2.0,y_ll-26.5,xlegend);

   fprintf(fp,"90 rotate\n");
  yy = yc+dy;
  dd1 = d1; dd2 = d2; dd3 = 10.0*floor(d3/10.0);
  if (dd3 == 60.0)  {dd3 = 0.0;  dd2++;}
  if (dd2 == 60)  {dd2 = 0;  dd1++;}
  while(1)
    {
     fprintf(fp,"%g %g m %g %g l\n",yy,x_ll,yy,x_ll+6);
     fprintf(fp,"%g %g m (%d %d %g) cs\n",yy,x_ll+8,dd1,dd2,dd3);
    dd3 += 10.0;
    if (dd3 == 60.0)  {dd3 = 0.0;  dd2++;}
    if (dd2 == 60)  {dd2 = 0;  dd1++;}
    yy += 5.0*pxly/ry;
    if (yy > y_ur)  break;
     fprintf(fp,"%g %g m %g %g l\n",yy,x_ll,yy,x_ll+4);
    yy += 5.0*pxly/ry;
    if (yy > y_ur)  break;
    }
  yy = yc+dy;
  dd1 = d1; dd2 = d2; dd3 = 10.0*floor(d3/10.0);
  if (dd3 == 60.0)  {dd3 = 0.0;  dd2++;}
  if (dd2 == 60)  {dd2 = 0;  dd1++;}
  while(1)
    {
    if (dd3 != 10.0*floor(d3/10.0))
      {
       fprintf(fp,"%g %g m %g %g l\n",yy,x_ll,yy,x_ll+6);
       fprintf(fp,"%g %g m (%d %d %g) cs\n",yy,x_ll+8,dd1,dd2,dd3);
      }
    dd3 -= 10.0;
    if (dd3 == -10.0)  {dd3 = 50.0;  dd2--;}
    if (dd2 == -1)  {dd2 = 59;  dd1--;}
    yy -= 5.0*pxly/ry;
    if (yy < y_ll)  break;
     fprintf(fp,"%g %g m %g %g l\n",yy,x_ll,yy,x_ll+4);
    yy -= 5.0*pxly/ry;
    if (yy < y_ll)  break;
    }
  if (ylegend != NULL)
     fprintf(fp,"%g %g m (%s) cs\n",(y_ll+y_ur)/2.0,x_ll+21.0,ylegend);
   fprintf(fp,"-90 rotate\n");
   fprintf(fp,"stroke\n");
  }
}
/**********************************************************************/
/*                                                                    */
/* FUNCTION: PS_Angle_URlabelling                                     */
/*                                                                    */
/* PURPOSE: Draws labelled box.                                       */
/*                                                                    */
/* INPUT:  x_ll,y_ll,x_ur,y_ur = rectangle coordinates                */
/*         xmax,xmin = min and max values on x axis                   */
/*         xlegend = legend on x axis                                 */
/*         ymax,ymin = min and max values on y axis                   */
/*         ylegend = legend on y axis                                 */
/*         fontname,fontsize = font used                              */
/*         fp = pointer to postscript file                            */
/*                                                                    */
/**********************************************************************/
void PS_Angle_URlabelling(FILE *fp, double x_ll, double y_ll, double x_ur,
                          double y_ur, int col, double rx, int a1, int a2,
                          double a3, char *xlegend, int lin, double ry,
                          int d1, int d2, double d3, char *ylegend,
                          char *fontname, double fontsize, double linewidth,
                          double red, double green, double blue)
{
int     aa1, aa2, dd1, dd2;
double   dx, dy, xx, yy, aa3, dd3;
double   xc, yc, width, height, pxlx, pxly, fx, fy;

if (fp != NULL)
  {
  width = x_ur-x_ll;
  height = y_ur-y_ll;
  pxlx = width/((double) col); /* inch */
  pxly = height/((double) lin); /* inch */
  xc = width/2.0 + 0.5*pxlx;
  yc = height/2.0 + 0.5*pxly;
  fx = rx*((double) col);
  fy = ry*((double) lin);
/**
 fprintf(stderr,"fx = %f\n",fx);
 fprintf(stderr,"fy = %f\n",fy);
 fprintf(stderr,"dx = %f\n",a3 - floor(a3));
 fprintf(stderr,"dy = %f\n",d3 - 10.0*floor(d3/10.0));
**/
  dx = (a3 - floor(a3))*pxlx/rx;
  dy = -(d3 - 10.0*floor(d3/10.0))*pxly/ry;

   fprintf(fp,"newpath\n");
   fprintf(fp,"/%s findfont %g scalefont setfont\n",fontname,fontsize);
   fprintf(fp,"%g line1\n",linewidth);
   fprintf(fp,"%g %g %g setrgbcolor\n",red,green,blue);

  xx = xc+dx;
  aa1 = a1; aa2 = a2; aa3 = floor(a3);
  if (aa3 == 60.0)  {aa3 = 0.0;  aa2++;}
  if (aa2 == 60)  {aa2 = 0;  aa1++;}
  while(1)
    {
     fprintf(fp,"%g %g m %g %g l\n",xx,y_ur,xx,y_ur+6.0);
     fprintf(fp,"%g %g m (%d %d %g) cs\n",xx,y_ur+8.0,aa1,aa2,aa3);
    aa3 += 1.0;
    if (aa3 == 60.0)  {aa3 = 0.0;  aa2++;}
    if (aa2 == 60)  {aa2 = 0;  aa1++;}
    xx -= 0.5*pxlx/rx;
    if (xx < x_ll)  break;
     fprintf(fp,"%g %g m %g %g l\n",xx,y_ur,xx,y_ur+4.0);
    xx -= 0.5*pxlx/rx;
    if (xx < x_ll)  break;
    }
  xx = xc+dx;
  aa1 = a1; aa2 = a2; aa3 = floor(a3);
  if (aa3 == 60.0)  {aa3 = 0.0;  aa2++;}
  if (aa2 == 60)  {aa2 = 0;  aa1++;}
  while(1)
    {
    if (aa3 != floor(a3))
      {
       fprintf(fp,"%g %g m %g %g l\n",xx,y_ur,xx,y_ur+6.0);
       fprintf(fp,"%g %g m (%d %d %g) cs\n",xx,y_ur+8.0,aa1,aa2,aa3);
      }
    aa3 -= 1.0;
    if (aa3 == -1.0)  {aa3 = 59.0;  aa2--;}
    if (aa2 == -1)  {aa2 = 59;  aa1--;}
    xx += 0.5*pxlx/rx;
    if (xx > x_ur)  break;
     fprintf(fp,"%g %g m %g %g l\n",xx,y_ur,xx,y_ur+4.0);
    xx += 0.5*pxlx/rx;
    if (xx > x_ur)  break;
    }
  if (xlegend != NULL)
     fprintf(fp,"%g %g m (%s) cs\n",(x_ur+x_ll)/2.0,y_ur+21.0,xlegend);

   fprintf(fp,"90 rotate\n");
  yy = yc+dy;
  dd1 = d1; dd2 = d2; dd3 = 10.0*floor(d3/10.0);
  if (dd3 == 60.0)  {dd3 = 0.0;  dd2++;}
  if (dd2 == 60)  {dd2 = 0;  dd1++;}
  while(1)
    {
     fprintf(fp,"%g %g m %g %g l\n",yy,-x_ur,yy,-x_ur-6);
     fprintf(fp,"%g %g m (%d %d %g) cs\n",yy,-x_ur-16,dd1,dd2,dd3);
    dd3 += 10.0;
    if (dd3 == 60.0)  {dd3 = 0.0;  dd2++;}
    if (dd2 == 60)  {dd2 = 0;  dd1++;}
    yy += 5.0*pxly/ry;
    if (yy > y_ur)  break;
     fprintf(fp,"%g %g m %g %g l\n",yy,-x_ur,yy,-x_ur-4);
    yy += 5.0*pxly/ry;
    if (yy > y_ur)  break;
    }
  yy = yc+dy;
  dd1 = d1; dd2 = d2; dd3 = 10.0*floor(d3/10.0);
  if (dd3 == 60.0)  {dd3 = 0.0;  dd2++;}
  if (dd2 == 60)  {dd2 = 0;  dd1++;}
  while(1)
    {
    if (dd3 != 10.0*floor(d3/10.0))
      {
       fprintf(fp,"%g %g m %g %g l\n",yy,-x_ur,yy,-x_ur-6);
       fprintf(fp,"%g %g m (%d %d %g) cs\n",yy,-x_ur-16,dd1,dd2,dd3);
      }
    dd3 -= 10.0;
    if (dd3 == -10.0)  {dd3 = 50.0;  dd2--;}
    if (dd2 == -1)  {dd2 = 59;  dd1--;}
    yy -= 5.0*pxly/ry;
    if (yy < y_ll)  break;
     fprintf(fp,"%g %g m %g %g l\n",yy,-x_ur,yy,-x_ur-4);
    yy -= 5.0*pxly/ry;
    if (yy < y_ll)  break;
    }
  if (ylegend != NULL)
     fprintf(fp,"%g %g m (%s) cs\n",(y_ll+y_ur)/2.0,-x_ur-26.5,ylegend);
   fprintf(fp,"-90 rotate\n");
   fprintf(fp,"stroke\n");
  }
}

/**********************************************************************/
/*                                                                    */
/* FUNCTION: PS_Rectangle                                             */
/*                                                                    */
/* PURPOSE: Draws a rectangle.                                        */
/*                                                                    */
/* INPUT:  x_ll,y_ll,x_ur,y_ur = rectangle coordinates                */
/*         fp = pointer to postscript file                            */
/*                                                                    */
/**********************************************************************/
void PS_Rectangle(FILE *fp, double x_ll, double y_ll, double x_ur, double y_ur,
                  double linewidth, double red, double green, double blue) 
{

if (fp != NULL)
  {
   fprintf(fp,"newpath\n");
   fprintf(fp,"%g line1\n",linewidth);
   fprintf(fp,"%g %g %g setrgbcolor\n",red,green,blue);
   fprintf(fp,"%g %g m %g %g l %g %g l %g %g l closepath\n",
                     x_ll,y_ll,x_ll,y_ur,x_ur,y_ur,x_ur,y_ll);
   fprintf(fp,"stroke\n");
  }
}
/**********************************************************************/
/*                                                                    */
/* FUNCTION: PS_Grid                                                  */
/*                                                                    */
/* PURPOSE: Draws a rectangle with a grid.                            */
/*                                                                    */
/* INPUT:  x_ll,y_ll,x_ur,y_ur = rectangle coordinates                */
/*         xs,ys = xstep and ystep of the grid                        */
/*         fp = pointer to postscript file                            */
/*                                                                    */
/**********************************************************************/
void PS_Grid(FILE *fp, double x_ll, double y_ll, double x_ur, double y_ur,
             double xs, double ys, double linewidth, double red, double green, 
             double blue)
{
double   x, y;

if (fp != NULL)
  {
   fprintf(fp,"newpath\n");
   fprintf(fp,"%g line1\n",linewidth);
   fprintf(fp,"%g %g %g setrgbcolor\n",red,green,blue);
  
   fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x_ll,y_ll,x_ll,y_ur);
  x = x_ll + xs;
  while (x < x_ur)
    {
     fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x,y_ll,x,y_ur);
    x += xs;
    }
   fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x_ur,y_ll,x_ur,y_ur);

   fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x_ll,y_ll,x_ur,y_ll);
  y = y_ll + ys;
  while (y < y_ur)
    {
     fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x_ll,y,x_ur,y);
    y += ys;
    }
   fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",x_ll,y_ur,x_ur,y_ur);

   fprintf(fp,"stroke\n");
  }
}
/**********************************************************************/
/*                                                                    */
/* FUNCTION: PS_UVcoverage                                            */
/*                                                                    */
/* PURPOSE: Draws a UV-coverage.                                      */
/*                                                                    */
/* INPUT:  x_ll,y_ll,x_ur,y_ur = rectangle coordinates                */
/*         fp = pointer to postscript file                            */
/*                                                                    */
/**********************************************************************/
void  PS_UVcoverage(FILE *fp, int nbr_config, int *nbr_baseline, 
                    int *nbr_sample, double **u, double **v,
                    double ax, double bx, double ay, double by, double linewidth,
                    double red, double green, double blue)
{
register   int  b, bb, c, s;

if (fp != NULL)
  {
   fprintf(fp,"%g line1\n",linewidth);
   fprintf(fp,"%g %g %g setrgbcolor\n",red,green,blue);
  for (c=0, bb=0; c<nbr_config; c++)
  for (b=0; b<nbr_baseline[c]; b++, bb++)
    {
     fprintf(fp,"%%%% config %d base %d\nnewpath\n",c+1,b+1);
     fprintf(fp,"%.2f %.2f m\n",ax*u[bb][0]+bx,ay*v[bb][0]+by);
    for (s=1; s<nbr_sample[c]; s++)
      {
       fprintf(fp,"%.2f %.2f l\n",ax*u[bb][s]+bx,ay*v[bb][s]+by);
      if ((50*(s/50)) == s)
        {
         fprintf(fp,"stroke\n");
         fprintf(fp,"newpath\n");
         fprintf(fp,"%.2f %.2f m\n",ax*u[bb][s]+bx,ay*v[bb][s]+by);
        }
      }
     fprintf(fp,"stroke\n");
    }
  for (c=0, bb=0; c<nbr_config; c++)
  for (b=0; b<nbr_baseline[c]; b++, bb++)
    {
     fprintf(fp,"%%%% config %d base %d\nnewpath\n",c+1,b+1);
     fprintf(fp,"%.2f %.2f m\n",-ax*u[bb][0]+bx,-ay*v[bb][0]+by);
    for (s=1; s<nbr_sample[c]; s++)
      {
       fprintf(fp,"%.2f %.2f l\n",-ax*u[bb][s]+bx,-ay*v[bb][s]+by);
      if ((50*(s/50)) == s)
        {
         fprintf(fp,"stroke\n");
         fprintf(fp,"newpath\n");
         fprintf(fp,"%.2f %.2f m\n",-ax*u[bb][s]+bx,-ay*v[bb][s]+by);
        }
      }
     fprintf(fp,"stroke\n");
    }
  }
/**
if (fp != NULL)
  {
   fprintf(fp,"%g line1\n",linewidth);
   fprintf(fp,"%g %g %g setrgbcolor\n",red,green,blue);
  for (c=0, bb=0; c<nbr_config; c++)
  for (b=0; b<nbr_baseline[c]; b++, bb++)
    {
     fprintf(fp,"%%%% config %d base %d\n",c+1,b+1);
    for (s=0; s<nbr_sample[c]; s+=5)
      {
       fprintf(fp,"newpath\n");
       fprintf(fp,"%.2f %.2f m\n",ax*u[bb][s]+bx,ay*v[bb][s]+by);
       fprintf(fp,"%.2f %.2f l\n",ax*u[bb][s+1]+bx,ay*v[bb][s+1]+by);
       fprintf(fp,"%.2f %.2f l\n",ax*u[bb][s+2]+bx,ay*v[bb][s+2]+by);
       fprintf(fp,"%.2f %.2f l\n",ax*u[bb][s+3]+bx,ay*v[bb][s+3]+by);
       fprintf(fp,"%.2f %.2f l\n",ax*u[bb][s+4]+bx,ay*v[bb][s+4]+by);
       fprintf(fp,"stroke\n");
      }
    }
  for (c=0, bb=0; c<nbr_config; c++)
  for (b=0; b<nbr_baseline[c]; b++, bb++)
    {
     fprintf(fp,"%%%% config %d base %d\n",c+1,b+1);
    for (s=0; s<nbr_sample[c]; s+=5)
      {
       fprintf(fp,"newpath\n");
       fprintf(fp,"%.2f %.2f m\n",-ax*u[bb][s]+bx,-ay*v[bb][s]+by);
       fprintf(fp,"%.2f %.2f l\n",-ax*u[bb][s+1]+bx,-ay*v[bb][s+1]+by);
       fprintf(fp,"%.2f %.2f l\n",-ax*u[bb][s+2]+bx,-ay*v[bb][s+2]+by);
       fprintf(fp,"%.2f %.2f l\n",-ax*u[bb][s+3]+bx,-ay*v[bb][s+3]+by);
       fprintf(fp,"%.2f %.2f l\n",-ax*u[bb][s+4]+bx,-ay*v[bb][s+4]+by);
       fprintf(fp,"stroke\n");
      }
    }
  }
**/
}
/**********************************************************************/
/*                                                                    */
/* FUNCTION: PS_Curve                                                 */
/*                                                                    */
/* PURPOSE: Draws a curve.                                            */
/*                                                                    */
/* INPUT:  v1,v2,v3,v4 = viewport coordinates                         */
/*         w1,w2,w3,w4 = window coordinates                           */
/*         nbr_pts = number of points                                 */
/*         x[0..nbr_pts-1],y[0..nbr_pts-1] = (x,y) coordinates        */
/*         linepattern = line pattern                                 */
/*         linewidth = line width for symbol pattern                  */
/*         red,green,blue = line color                                */
/*         fp = pointer to postscript file                            */
/*                                                                    */
/**********************************************************************/
void  PS_Curve(FILE *fp, double v1, double v3, double v2, double v4,
               double w1, double w3, double w2, double w4, int nbr_pts,
               double *x, double *y, int linepattern, double linewidth,
               double red, double green, double blue)
/**********************************************************************/
{
register   int        i;
int        c1, c2, c3, c4, code1, code2, code;
double      ax, bx, ay, by, xmin, xmax, ymin, ymax;

ax = (v2-v1)/(w2-w1);
bx = (v1*w2-v2*w1)/(w2-w1);
ay = (v4-v3)/(w4-w3);
by = (v3*w4-v4*w3)/(w4-w3);

xmin = w1 + (w2 - w1)/22.01;
xmax = w2 - (w2 - w1)/22.01;
ymin = w3 + (w4 - w3)/22.01;
ymax = w4 - (w4 - w3)/22.01;

if (fp != NULL)
  {
   fprintf(fp,"%g line%d\n",linewidth,linepattern);
   fprintf(fp,"%g %g %g setrgbcolor\n",red,green,blue);
   fprintf(fp,"newpath\n");
  c1 = 0;  if (x[0] < xmin) c1 =    1;
  c2 = 0;  if (x[0] > xmax) c2 =   10;
  c3 = 0;  if (y[0] < ymin) c3 =  100;
  c4 = 0;  if (y[0] > ymax) c4 = 1000;
  code1 = c1+c2+c3+c4;
  for (i=1; i<nbr_pts; i++)
    {
    c1 = 0;  if (x[i] < xmin) c1 =    1;
    c2 = 0;  if (x[i] > xmax) c2 =   10;
    c3 = 0;  if (y[i] < ymin) c3 =  100;
    c4 = 0;  if (y[i] > ymax) c4 = 1000;
    code2 = c1+c2+c3+c4;
    if ((code1 != 0) && (code2 != 0))  code = -1;
    if ((code1 == 0) || (code2 == 0))  code = 1;
    if ((code1 == 0) && (code2 == 0))  code = 0;
    switch(code)
      {
      case -1:
        break;
      case 0:
         fprintf(fp,"%.2f %.2f m ",ax*x[i-1]+bx,ay*y[i-1]+by);
         fprintf(fp,"%.2f %.2f l\n",ax*x[i]+bx,ay*y[i]+by);
        break;
      case 1:
        break;
      default:
        break;
      }
    code1 = code2;
    }
   fprintf(fp,"stroke\n");
  }
}
/**********************************************************************/
/*                                                                    */
/* FUNCTION: PS_Symbol                                                */
/*                                                                    */
/* PURPOSE: Draws symbols.                                            */
/*                                                                    */
/* INPUT:  v1,v2,v3,v4 = viewport coordinates                         */
/*         w1,w2,w3,w4 = window coordinates                           */
/*         nbr_pts = number of points                                 */
/*         x[0..nbr_pts-1],y[0..nbr_pts-1] = (x,y) coordinates        */
/*         symbpattern = symbol pattern                               */
/*         linewidth = line width for symbol pattern                  */
/*         red,green,blue = symbol color                              */
/*         fp = pointer to postscript file                            */
/*                                                                    */
/**********************************************************************/
void  PS_Symbol(FILE *fp, double v1, double v3, double v2, double v4, 
                double w1, double w3, double w2, double w4, int nbr_pts,
                double *x, double *y, int symbpattern,double linewidth,
                double red, double green, double blue)
{
register   int        i;
double      ax, bx, ay, by, xmin, xmax, ymin, ymax;

ax = (v2-v1)/(w2-w1);
bx = (v1*w2-v2*w1)/(w2-w1);
ay = (v4-v3)/(w4-w3);
by = (v3*w4-v4*w3)/(w4-w3);

xmin = w1 + (w2 - w1)/22.01;
xmax = w2 - (w2 - w1)/22.01;
ymin = w3 + (w4 - w3)/22.01;
ymax = w4 - (w4 - w3)/22.01;

if (fp != NULL)
  {
   fprintf(fp,"newpath\n");
   fprintf(fp,"%g line1\n",linewidth);
   fprintf(fp,"%g %g %g setrgbcolor\n",red,green,blue);
  for (i=0; i<nbr_pts; i++)
    {
    if ((xmin <= x[i]) && (x[i] <= xmax) && (ymin <= y[i]) && (y[i] <= ymax))
      {
       fprintf(fp,"%.2f %.2f ",ax*x[i]+bx,ay*y[i]+by);
       fprintf(fp,"symb%d\n",symbpattern);
      }
    }
   fprintf(fp,"stroke\n");
  }
}
/************************************************************************/
/*                                                                      */
/* FUNCTION: PS_Axis_LLlabelling_manu                                   */
/*                                                                      */
/* PURPOSE: Draws labelled box.                                         */
/*                                                                      */
/* INPUT:  v1,v3,v2,v4 = viewport coordinates                           */
/*         w1,w3,w2,w4 = window coordinates                             */
/*         x1,x2 = first tick and length of division on x axis          */
/*         x3 = number of ticks per division on x axis                  */
/*         xlegend = legend on x axis                                   */
/*         y1,y2 = first tick and length of division on y axis          */
/*         y3 = number of ticks per division on y axis                  */
/*         ylegend = legend on y axis                                   */
/*         fontname,fontsize = font used                                */
/*         fp = pointer to postscript file                              */
/*                                                                      */
/************************************************************************/
void PS_Axis_LLlabelling_manu(FILE *fp, double v1, double v3, double v2, double v4,
                              double w1, double w3, double w2, double w4,
                              double x1, double x2, int x3, char *xlegend,
                              double y1, double y2, int y3, char *ylegend,
                              char *fontname, double fontsize, double linewidth,
                              double red, double green, double blue)
{
int     i, j;
double   x, y, xx, yy, ax, bx, ay, by;

if (fp != NULL)
  {
   fprintf(fp,"newpath\n");
   fprintf(fp,"/%s findfont %g scalefont setfont\n",fontname,fontsize);
   fprintf(fp,"%g line1\n",linewidth);
   fprintf(fp,"%g %g %g setrgbcolor\n",red,green,blue);

  if (x2 != 0.0)
    {
    ax = (v2-v1)/(w2-w1);  bx = (v1*w2-v2*w1)/(w2-w1);
    for (x=x1; x<=w2; x+=x2)
      { 
       fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",ax*x+bx,v3,ax*x+bx,v3-6.0);
      i = 1;
      if (x3 != 0) xx = x + x2/((double) x3+1);
      while(i <= x3)
        {
        if (xx > w2) break;
         fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",ax*xx+bx,v3,ax*xx+bx,v3-4.0);
        xx += x2/((double) x3+1);
        i++;
        }
      }
    for (x=x1; x<=w2; x+=x2)
       fprintf(fp,"%.2f %.2f m (%g) cs\n",ax*x+bx,v3-15.0,x);
    if (xlegend != NULL)
       fprintf(fp,"%g %g m (%s) cs\n",(v2+v1)/2.0,v3-26.5,xlegend);
    }

  if (y2 != 0.0)
    {
     fprintf(fp,"90 rotate\n");
    ay = (v4-v3)/(w4-w3);  by = (v3*w4-v4*w3)/(w4-w3);
    for (y=y1; y<=w4; y+=y2)
      {
       fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",ay*y+by,v1,ay*y+by,v1+6.0);
      j = 1;
      if (y3 != 0) yy = y + y2/((double) y3+1);
      while(j <= y3)
        {
        if (yy > w4) break;
         fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",ay*yy+by,v1,ay*yy+by,v1+4.0);
        yy += y2/((double) y3+1);
        j++;
        }
      }
    for (y=y1; y<=w4; y+=y2)
       fprintf(fp,"%.2f %.2f m (%g) cs\n",ay*y+by,v1+8.0,y);
    if (ylegend != NULL)
       fprintf(fp,"%g %g m (%s) cs\n",(v3+v4)/2.0,v1+21.0,ylegend);
     fprintf(fp,"-90 rotate\n");
    }

   fprintf(fp,"stroke\n");
  }
}
/************************************************************************/
/*                                                                      */
/* FUNCTION: PS_Axis_URlabelling_manu                                   */
/*                                                                      */
/* PURPOSE: Draws labelled box.                                         */
/*                                                                      */
/* INPUT:  v1,v3,v2,v4 = viewport coordinates                           */
/*         w1,w3,w2,w4 = window coordinates                             */
/*         x1,x2 = first tick and length of division on x axis          */
/*         x3 = number of ticks per division on x axis                  */
/*         xlegend = legend on x axis                                   */
/*         y1,y2 = first tick and length of division on y axis          */
/*         y3 = number of ticks per division on y axis                  */
/*         ylegend = legend on y axis                                   */
/*         fontname,fontsize = font used                                */
/*         fp = pointer to postscript file                              */
/*                                                                      */
/************************************************************************/
void PS_Axis_URlabelling_manu(FILE *fp, double v1, double v3, double v2, double v4,
                              double w1, double w3, double w2, double w4,
                              double x1, double x2, int x3, char *xlegend,
                              double y1, double y2, int y3, char *ylegend,
                              char *fontname, double fontsize, double linewidth,
                              double red, double green, double blue)
{
int     i, j;
double   x, y, xx, yy, ax, bx, ay, by;

if (fp != NULL)
  {
   fprintf(fp,"newpath\n");
   fprintf(fp,"/%s findfont %g scalefont setfont\n",fontname,fontsize);
   fprintf(fp,"%g line1\n",linewidth);
   fprintf(fp,"%g %g %g setrgbcolor\n",red,green,blue);

  if (x2 != 0.0)
    {
    ax = (v2-v1)/(w2-w1);  bx = (v1*w2-v2*w1)/(w2-w1);
    for (x=x1; x<=w2; x+=x2)
      { 
       fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",ax*x+bx,v4,ax*x+bx,v4+6.0);
      i = 1;
      if (x3 != 0) xx = x + x2/((double) x3+1);
      while(i <= x3)
        {
        if (xx > w2) break;
         fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",ax*xx+bx,v4,ax*xx+bx,v4+4.0);
        xx += x2/((double) x3+1);
        i++;
        }
      }
    for (x=x1; x<=w2; x+=x2)
       fprintf(fp,"%.2f %.2f m (%g) cs\n",ax*x+bx,v4+8.0,x);
    if (xlegend != NULL)
       fprintf(fp,"%g %g m (%s) cs\n",(v2+v1)/2.0,v4+21.0,xlegend);
    }

  if (y2 != 0.0)
    {
     fprintf(fp,"90 rotate\n");
    ay = (v4-v3)/(w4-w3);  by = (v3*w4-v4*w3)/(w4-w3);
    for (y=y1; y<=w4; y+=y2)
      {
       fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",ay*y+by,-v2,ay*y+by,-v2-6.0);
      j = 1;
      if (y3 != 0) yy = y + y2/((double) y3+1);
      while(j <= y3)
        {
        if (yy > w4) break;
         fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",ay*yy+by,-v2,ay*yy+by,-v2-4.0);
        yy += y2/((double) y3+1);
        j++;
        }
      }
    for (y=y1; y<=w4; y+=y2)
       fprintf(fp,"%.2f %.2f m (%g) cs\n",ay*y+by,-v2-15.0,y);
    if (ylegend != NULL)
       fprintf(fp,"%g %g m (%s) cs\n",(v3+v4)/2.0,-v2-26.5,ylegend);
     fprintf(fp,"-90 rotate\n");
    }

   fprintf(fp,"stroke\n");
  }
}
/**********************************************************************
*
* FUNCTION: PS_LutRGB1
*
* PURPOSE: Draws Lut RGB caption.
*
* INPUT:  width,height = rectangle dimensions
*         zmin,zmax = min and max values
*         color1, color1: address of boundary color cells
*         nbr_colors = number of colors
*         lut = equation of lut: lin, log or sqrt
*         fp = pointer to postscript file
*
* JLP: Same as PS_LutRGB 
*      but with "r,g,b,pst_lut" instead of "color1,color2,lut" ...
**********************************************************************/
void  PS_LutRGB1(FILE *fp, double width, double height, double zmin, double zmax,
                 int *r, int *g, int *b, int *pst_lut, int ncolors,
                 char *zlegend, char *fontname, double fontsize, double linewidth,
                 double red, double green, double blue)
{
register   int     i, k, icell;
int     rr, gg, bb;
double   z, az, bz, expz, manx, pasz, decz;

if (fp != NULL)
  {
  if (width > height)
     fprintf(fp,"%%%% RGB Look Up Table (mode H)\n");
  else
     fprintf(fp,"%%%% RGB Look Up Table (mode V)\n");
   fprintf(fp,"newpath\n");
   fprintf(fp,"%g %g scale\n",width,height);
  if (width > height)
     fprintf(fp,"%d 1 8 [%d 0 0 1 0 0]\n{<",ncolors,ncolors);
  else
     fprintf(fp,"1 %d 8 [1 0 0 %d 0 0]\n{<",ncolors,ncolors);

/* First starting with red values*/
  k = 0;
  for (i=0; i<ncolors; i++)
    {
    icell = pst_lut[i];
    rr = r[icell]/256;
    if (rr < 16)  fprintf(fp,"0%x",rr);
    else  fprintf(fp,"%x",rr);
    if (k == 31) { fprintf(fp,"\n");  k = 0;}
    else k++;
    }
  if ((i != (ncolors-1)) && (k != 0))  fprintf(fp,"\n");
   fprintf(fp,">}\n{<");

/* Then green values */
  k = 0;
  for (i=0; i<ncolors; i++)
    {
    icell = pst_lut[i];
    gg = g[icell]/256;
    if (gg < 16)  fprintf(fp,"0%x",gg);
    else  fprintf(fp,"%x",gg);
    if (k == 31) { fprintf(fp,"\n");  k = 0;}
    else k++;
    }
  if ((i != (ncolors-1)) && (k != 0))  fprintf(fp,"\n");
   fprintf(fp,">}\n{<");

/* End with blue values: */
  k = 0;
  for (i=0; i<ncolors; i++)
    {
    icell = pst_lut[i];
    bb = b[icell]/256;
    if (bb < 16)  fprintf(fp,"0%x",bb);
    else  fprintf(fp,"%x",bb);
    if (k == 31) { fprintf(fp,"\n");  k = 0;}
    else k++;
    }
  if ((i != (ncolors-1)) && (k != 0))  fprintf(fp,"\n");

/* End of LUT: */
   fprintf(fp,">}\ntrue 3 colorimage\nstroke\n");
   fprintf(fp,"newpath\n");
   fprintf(fp,"1 %g div 1 scale\n",width);
   fprintf(fp,"1 %g div 1 exch scale\n",height);
   fprintf(fp,"stroke\n");

  expz = floor(log10(zmax-zmin));
  manx = (zmax-zmin)/(pow(10.,expz));
  pasz = ceil(manx)*pow(10.,expz-1);
  decz = pasz*ceil(zmin/pasz) - zmin;
   fprintf(fp,"newpath\n");
   fprintf(fp,"%g line1\n",linewidth);
   fprintf(fp,"%g %g %g setrgbcolor\n",red,green,blue);
   fprintf(fp,"0 0 m 0 %g l %g %g l %g 0 l closepath\n",
                 height,width,height,width);
   fprintf(fp,"stroke\n");
   fprintf(fp,"newpath\n");
   fprintf(fp,"/%s findfont %g scalefont setfont\n",fontname,fontsize);
   fprintf(fp,"%g line1\n",linewidth);
   fprintf(fp,"%g %g %g setrgbcolor\n",red,green,blue);

  if (width > height)
    {
    az = width/(zmax-zmin);  bz = -width*zmin/(zmax-zmin);
    for (z=(zmin+decz); z<=zmax; z+=pasz)
       fprintf(fp,"%.2f 0 m %.2f -6 l\n",az*z+bz,az*z+bz);
    for (z=(zmin+decz); z<=zmax; z+=2*pasz)
       fprintf(fp,"%.2f -15 m (%g) cs\n",az*z+bz,z);
    if (zlegend != NULL)
       fprintf(fp,"%g -26.5 m (%s) cs\n",width/2.0,zlegend);
     fprintf(fp,"stroke\n");
    }
  else
    {
    az = height/(zmax-zmin);  bz = -height*zmin/(zmax-zmin);
    for (z=(zmin+decz); z<=zmax; z+=pasz)
       fprintf(fp,"%.2f %.2f m %.2f %.2f l\n",width,az*z+bz,width+6.0,az*z+bz);
    for (z=(zmin+decz); z<=zmax; z+=2*pasz)
       fprintf(fp,"%.2f %.2f m (%g) sh\n",width+8.0,az*z+bz-3.0,z);
    if (zlegend != NULL)
       fprintf(fp,"%.2f %.2f m (%s) sh\n",width+20.0,height/2.0,zlegend);
     fprintf(fp,"stroke\n");
    }
   fprintf(fp,"%%%% End of RGB Look Up Table (1) \n");
  }
}
