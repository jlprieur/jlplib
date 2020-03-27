/**************************************************************************
* Definition of JLP_GText class
* used by JLP_GDev class 
*
* JLP
* Version 18/01/2017
**************************************************************************/
#include "jlp_gdev_def.h"
#include "jlp_gdev.h"                 
#if JLP_USE_X11
#include "jlp_gdev_x11.h"   // For accessing to drawing routines (in jlp_x11plot/)
#endif
#include "jlp_gdev_pst.h"   // For accessing to drawing routines (in jlp_splot/)

#ifndef _jlp_gtext_include                // BOF sentry
#define _jlp_gtext_include

#define GT_NFONT 6                         /* number of fonts */
#define GT_NCHAR 96                        /* number of characters in a font */

/* JLP 1991: il and ir becomes integers, since characters
 can be signed or unsigned according to the machines...
 IBM=unsigned
 DEC and SUN= signed...
*/
#define my_uchar short int

class JLP_GText {
public:
  JLP_GText(int& status);

  ~JLP_GText() {};

// Write a label with TeX fonts:
  double label_TeX(JLP_GDev &Jgd, const char *s, int xstart, int ystart,
                  double angle, double expand, int draw_it);

private:
 int load_font(char *font_file);
 char *tex_string(JLP_GDev& Jgd0, char *str, int draw_it,
                            int font_id, double sfac, double shift);
 void tex_dot(JLP_GDev& Jgd0, int n, int style, double expd);
 void tex_line(JLP_GDev& Jgd0, int lt, int len);
 void tex_macro(JLP_GDev& Jgd0, char *name, int draw_it, int font_id, 
                double sfac, double shift);


 char font_names[GT_NFONT][4];

 my_uchar *gt_font;                         // ptr to stroke table 
 my_uchar gt_ns[GT_NFONT*GT_NCHAR],         // number of strokes 
            gt_il[GT_NFONT*GT_NCHAR],       // left adjust 
            gt_ir[GT_NFONT*GT_NCHAR];       // right adjust 
 double gt_slen;                             // pixel length of str 
 int gt_fp[GT_NFONT*GT_NCHAR],              // pointer to chars
           gt_group,                        // level of TeX grouping 
           gt_nfont,                        // number of fonts 
           gt_x0,gt_y0;                     // origin of character string
 int gt_xp, gt_yp;                          // location of current point
 double gt_expand, gt_angle, gt_ffcos, gt_ffsin;

};

#endif                // EOF sentry
