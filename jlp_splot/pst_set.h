/**********************************************************************
* "post_set.h" with the routines defined in "post_set.c"
*
*
* JLP Version 24-05-2006
**********************************************************************/
#ifndef _POST_SET_INCL /* Sentry */
#define _POST_SET_INCL

/* in pst_set.cpp: 
*/
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
void PS_Image(FILE *fp, double width, double height, int nx, int ny, int *image);
/* EOF pst_set.cpp */

void  PS_LutRGB1(FILE *fp, double width, double height, double zmin, double zmax,
                 int *r, int *g, int *b, int *pst_lut, int ncolors,
                 char *zlegend, char *fontname, double fontsize, double linewidth,
                 double red, double green, double blue);
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

#endif /* EOF_POST_SET_INCL Sentry */
