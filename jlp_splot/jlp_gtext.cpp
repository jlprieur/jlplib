/**************************************************************************
* Definition of the members of the JLP_GText class
* used in my plotting package
*
* JLP
* Version 13/10/2006
**************************************************************************/
#include <math.h>
#include <unistd.h>                     // For open(), close() 
#include <fcntl.h>                      // For read()
#include <ctype.h>                      // For isdigit(), isalpha(), etc.
#include <string.h>
#include "jlp_gtext.h"
#include "jlp_gdev.h"

/*
#define DEBUG
*/

#define MAGSTEP 1.2                     /* size change corresponding to 1 */
#define SUB_SUPER_EXPAND 0.6            /* factor to expand sub/superscripts */
#define SLANT 0.2                       /* italic slant ratio */
#define ITALIC 020                      /* set `italic' bit */
#define CDEF     22                     /* Fonts are enlarged by CDEF */

// OLD Problem in include file, so I compute it myself:
/*
static double hypot(double x, double y)
{
double val;
val = sqrt(x*x + y*y);
return(val);
}
*/

static int jlp_ndata = 0;    /* number of numbers describing characters in fonts */
/**************************************************************
* Constructor
*
***************************************************************/
JLP_GText::JLP_GText(int& status) : gt_group(0), gt_nfont(0){
char font_file[81];
char exec_dir[81];
int length;

/* names of fonts, TeX mode */
#define ROMAN 0
  strcpy(font_names[0], "rm ");
#define GREEK 1
  strcpy(font_names[1], "gr ");
#define SCRIPT 2
  strcpy(font_names[2], "sc ");
#define TINY 3
  strcpy(font_names[3], "ti ");
#define OLD_ENGLISH 4
  strcpy(font_names[4], "oe ");
#define PRIVATE 5
  strcpy(font_names[5], "pr ");
/*#define ITALIC ?
   { "i", "it "} is special, and doesn't appear here */

/* read fonts file (i.e. $EXEC/fonts.bin)
* (As this is a binary file, it needs to be in a machine-dependant
* directory)
*/
   length=4;
#if 0
   JLP_GETENV("EXEC",&length,exec_dir,&status);
#else
// JLP2013: for my dell computer:
   strcpy(exec_dir, "/d/execlnx");
   status = 0;
#endif
    if(status) {
      fprintf(stderr,"JLP_GText/Fatal error getting $EXEC symbol: istat=%d \n",(int)status);
      exit(-1);
    } else {
      sprintf(font_file,"%s/fonts.bin",exec_dir);
      status = load_font(font_file);
      if(status) {
         printf("JLP_GText/Error in load_font/status = %d\n",(int)status);
         printf(" EXEC %s\n",exec_dir);
         printf(" Fonts: %s\n",font_file);
      }
    }

return;
}
/*********************************************************************
* From old version of Super Mongo (1989)
* Copyright (C) 1987, 1989 Robert Lupton and Patricia Monger
* but largely modified by JLP.
*
* The fonts which are used are public domain.
*
* Read font arrays in file
*
* INPUT:
* char *font_file : name of font file
**********************************************************************/
int JLP_GText::load_font(char *font_file)
{
   int fbin,
       nchar,           /* Number of characters in fonts, GT_NFONT*GT_NCHAR */
       ndata;           /* number of numbers describing characters in fonts */

   if((fbin = open(font_file,0)) < 0) {
      fprintf(stderr,"load_font/can't find font binary file %s\n",
              font_file);
      return(-1);
   }

   if(read(fbin,&nchar,sizeof(int)) != sizeof(int)) {
      fprintf(stderr,"load_font/can't read nchar from font file\n");
      return(-1);
   }
   gt_nfont = nchar/GT_NCHAR;

   if(nchar%GT_NCHAR != 0) {
      printf("load_font/last %d chars don't fit into a font\n",nchar%GT_NCHAR);      nchar = gt_nfont*GT_NCHAR;
   }

   if(gt_nfont > GT_NFONT) {
      printf("load_font/too many fonts (%d, > ",gt_nfont);
      printf("%d) specified in read_fonts\n",GT_NFONT);
      gt_nfont = GT_NFONT;
   }
   if(read(fbin,&ndata,sizeof(int)) != sizeof(int)) {
      fprintf(stderr,"load_font/can't read ndata from font file\n");
      close(fbin);
      return(-1);
   }
/* get space from heap */
    jlp_ndata = ndata;
   if((gt_font = (my_uchar *)malloc(ndata*sizeof(my_uchar))) == NULL) {
      fprintf(stderr,"load_font/couldn't allocate space for font table\n");
      close(fbin);
      return(-1);
   }
   if(read(fbin,gt_ns,nchar*sizeof(my_uchar)) != nchar*(int)sizeof(my_uchar)) {
      fprintf(stderr,"load_font/can't read gt_ns from font file\n");
      free(gt_font);
      close(fbin);
      return(-1);
   }
   if(read(fbin,gt_il,nchar*sizeof(my_uchar)) != nchar*(int)sizeof(my_uchar)) {
      fprintf(stderr,"load_font/can't read gt_il from font file\n");
      free(gt_font);
      close(fbin);
      return(-1);
   }
   if(read(fbin,gt_ir,nchar*sizeof(my_uchar)) != nchar*(int)sizeof(my_uchar)) {
      fprintf(stderr,"load_font/can't read gt_ir from font file\n");
      close(fbin);
      free(gt_font);
      return(-1);
   }
   if(read(fbin,gt_fp,nchar*sizeof(int)) != nchar*(int)sizeof(int)) {
      fprintf(stderr,"load_font/can't read gt_fp from font file\n");
      free(gt_font);
      close(fbin);
      return(-1);
   }
   if(read(fbin,gt_font,ndata*sizeof(my_uchar)) != ndata*(int)sizeof(my_uchar)) {
      fprintf(stderr,"load_font/can't read gt_font from font file\n");
      free(gt_font);
      close(fbin);
      return(-1);
   }
   (void)close(fbin);
   return(0);
}
/**************************************************************************/

/***********************************************************************
* Write a string to the graphic device Jgd0 with TeX fonts 
* (if draw_it is true, otherwise simply find it's length)
* char *s :  string to write
* int xstart, ystart : Starting position (MGO coord.)
* double angle, expand
* int draw_it  : should I really draw it?
************************************************************************/
double JLP_GText::label_TeX(JLP_GDev &Jgd0, const char *s, int xstart, 
                           int ystart, double angle, double expand, int draw_it)
{
double gt_xasp, gt_yasp, sfac;
int device_height, device_width;
char mystring[120];

// aspect ratio of screen, y/x
 device_height = Jgd0.Jgc0_dev_height();
 device_width = Jgd0.Jgc0_dev_width();
 gt_xasp = (double)device_height / (double)device_width;     
 gt_yasp = 1. / gt_xasp; 

/* To prevent modifications of input char*s, I copy to "mystring": */
strcpy(mystring,s);

   gt_xp = xstart; 
   gt_yp = ystart;
   gt_expand = expand;
   gt_angle = angle;
   gt_ffcos = cos(gt_angle*PI/180.); 
   gt_ffsin = sin(gt_angle*PI/180.);

   gt_slen = 0.0;
   gt_x0 = gt_xp;
   gt_y0 = gt_yp;

/* tex_string(char *str, int draw_it, int font_id,
                        double sfac, double shift, int idv);
* JLP 2006: the size of all fonts will be enlarged in tex_string
*           by Jgd0.eexpand"
*/
    sfac = 1.0;
    tex_string(Jgd0,mystring,draw_it,0,sfac,0.0);
    if(gt_group) {
      if(draw_it) fprintf(stderr,"label_TeX/Unclosed braces in string\n");
      gt_group = 0;
    }

   return(gt_slen);
}

/***********************************************************************
 *
 * And now the code to deal with TeX-style strings
* INPUT:
* char *str;	 string to process 
* int draw_it:	 do I actually want to draw this?
* int font_id:	 font to use 
* double sfac:	 current expansion 
* double shift:	 level of sub/superscript 
************************************************************************/
char* JLP_GText::tex_string(JLP_GDev& Jgd0, char *str, int draw_it,
                            int font_id, double sfac, double shift)
{
double gt_xasp, gt_yasp;
int device_height, device_width;
int size = 40;				/* size of "TeX" macro names */
char name[size];			/* name of "TeX" definition */
/* JLP 91 char to short int: */
my_uchar *t;
double expd,rsh,ecos,esin, x, y,
       len_expd,			/* effect of expand on gt_slen */
       sfac0 = sfac,			/* base level of sfac */
       shift0 = shift;		/* base value of shift */
int c,				/* font position of current char */
    italic = (font_id & ITALIC),	/* is font italic? */
    relo;				/* relocate, don't draw line */
int len,lt,pdef,i,ila,ix,iy,xa;

// aspect ratio of screen, y/x
 device_height = Jgd0.Jgc0_dev_height();
 device_width = Jgd0.Jgc0_dev_width();
 gt_xasp = (double)device_height / (double)device_width;     
 gt_yasp = 1. / gt_xasp; 

/* pdef: scale_size for points
*/
pdef = Jgd0.Jgc0_pdef();

   for(;;) {
      switch (*str) {
       case '\0':
	 return(str);
       case '^':			/* raise next group */
	 sfac *= SUB_SUPER_EXPAND;
	 shift += sfac;
	 str++;
	 continue;
       case '_':			/* lower next group */
	 sfac *= SUB_SUPER_EXPAND;
	 shift -= sfac;
	 str++;
	 continue;
       case '{':			/* start group */
	 gt_group++;
/* process group */
	 str = tex_string(Jgd0, ++str,draw_it,font_id,sfac,shift); 
	 sfac = sfac0;			/* reset after group */
	 shift = shift0;
	 continue;
       case '}':			/* end group */
	 if(--gt_group < 0) {
	    if(draw_it) fprintf(stderr,"tex_string/Too many closing braces: \"%s\"\n",str);
	    gt_group = 0;
	 } else {
	    return(++str);
	 }
	 break;
/* Dectection of TeX Macro, or escape, or font: */
       case '\\':			/* "TeX" macro, escape, or font */
	 str++;

/* Check if it is really TeX: if not, exit from here */
	 if(*str == '^' || *str == '_' || *str == '\\' || *str == ' ' ||
	    				  *str == '{' || *str == '}') {
	    break;
	 } else if(isdigit(*str) || *str == '-') {
	    double fac;
	    
	    if(*str == '-') {
	       str++;
	       if(!isdigit(*str)) {
		  if(draw_it)
		    fprintf(stderr,"tex_string/You must specify a magnification, not just \\-\n");
		  continue;
	       }
	       fac = 1/MAGSTEP;
	    } else {
	       fac = MAGSTEP;
	    }
	    for(i = *str - '0';i > 0;i--) {
	       sfac0 *= fac; sfac *= fac;
	    }
	    str++;			/* skip digit */
	    continue;
	 }

/* Now sure that it is a TeX macro and proceed.
* Decode the name of the TeX macro: */
	 for(i = 0; i < size; i++) {	/* find name of "TeX" macro */
	    name[i] = *str++;
	    if(!isalpha(name[i])) break;
	 }
	 if(i == 0) {
	    i++;			/* single non-alpha character */
	 } else if(!isspace(name[i])) {
	    str--;			/* went one too far */
	 }
	 name[i] = '\0';

#ifdef DEBUG
        printf("tex_string/TeX macro found: %s\n", name);
#endif

	 for(i = 0; i < GT_NFONT; i++) {
	    if(!strncmp(name, font_names[i], 3)) {
	       if(i >= gt_nfont) {		/* warn about missing fonts */
		  if(draw_it)
		    fprintf(stderr,"tex_string/No \\%s font is currently defined\n",name);
	       } else {
		  font_id = italic ? (i | ITALIC) : i;
	       }
	       break;
	    }
	 }
	 if(!strncmp(name,"i ",2) || !strncmp(name, "it ", 3)) {
	    italic = 1;
	    font_id |= ITALIC;
	    continue;
	 }

	 if(i != GT_NFONT) {		/* we found a font */
#ifdef DEBUG
        printf("tex_string/font %d was found.\n", i);
#endif
	    continue;
	 }
	 if(!strcmp(name,"point") || !strcmp(name,"apoint")) {
	    double ang;
	    int n = -1,style = -1;

	    if(name[0] == 'a') {	/* specify angle ptype */
	       while(isspace(*str)) str++;
	       ang = atof(str);
	       while(isdigit(*str)) str++;
	       if(*str == '.') {
		  str++;
		  while(isdigit(*str)) str++;
	       }
	       while(isspace(*str)) str++;
	    } else {
	       ang = gt_angle;
	    }

	    if(isdigit(*str)) n = *str++ - '0';
	    while(isspace(*str)) str++;
	    if(isdigit(*str)) style = *str++ - '0';
	    if(draw_it && (n < 0 || style < -1)) {
	       fprintf(stderr,"tex_string/You must specify a ptype with \\point: \"%s\"\n",str);
	       continue;
	    }
   
	    expd = gt_expand * CDEF * sfac;	/* full expansion */
	    ecos = gt_ffcos * expd;		/* expanded cosine */
	    esin = gt_ffsin * expd;		/* expanded sine */
	    len_expd = hypot(ecos*gt_xasp,esin/gt_yasp);
	    rsh = 16*shift/sfac;	/* relative shift */

	    if(draw_it) {
	       x = pdef/(CDEF*sfac) + 5;
	       y = rsh;
	       gt_xp = (int)(gt_xasp*(ecos*x - esin*y) + gt_x0);
	       gt_yp = (int)((esin*x + ecos*y)/gt_yasp + gt_y0);
	       tex_dot(Jgd0,n,style,expd/CDEF);
	    }

	    xa = (int)(2 * pdef / (CDEF*sfac) + 10); /* width of dot */
	    gt_slen += len_expd*xa;
	    gt_x0 += (int)(ecos*xa*gt_xasp);
	    gt_y0 += (int)(esin*xa/gt_yasp);
	 } else if(!strcmp(name,"line")) {
	    len = -1; lt = -1;
	    while(isspace(*str)) str++;
	    if(isdigit(*str)) lt = *str++ - '0';
	    while(isspace(*str)) str++;
	    len = atoi(str);
	    while(isdigit(*str)) str++;

	    if(draw_it && (len < 0 || lt < 0)) {
	       fprintf(stderr,
	       "tex_string/You must specify a length and type with \\line: \"%s\"\n",str);
	       continue;
	    }
   
	    expd = gt_expand * CDEF * sfac;	/* full expansion */
	    ecos = gt_ffcos * expd;		/* expanded cosine */
	    esin = gt_ffsin * expd;		/* expanded sine */
	    len_expd = hypot(ecos*gt_xasp,esin/gt_yasp);
	    rsh = 16*shift/sfac;	/* relative shift */

	    if(draw_it) {
	       x = 0;
	       y = rsh;
	       gt_xp = (int)(gt_xasp*(ecos*x - esin*y) + gt_x0);
	       gt_yp = (int)((esin*x + ecos*y)/gt_yasp + gt_y0);
	       tex_line(Jgd0,lt,len);
	    }

	    gt_slen += len;
	    gt_x0 += (int)(gt_xasp*gt_ffcos*len);
	    gt_y0 += (int)(gt_ffsin*len/gt_yasp);
	 } else {
/* JLP2006: Tex symbols were too small */
            sfac *= 1.5;
	    tex_macro(Jgd0,name,draw_it,font_id,sfac,shift);
	 }
	 sfac = sfac0;			/* reset up/down */
	 shift = shift0;
	 continue;
       default:
	 break;
      }

      expd = gt_expand * CDEF * sfac;     	/* full expansion */
      ecos = gt_ffcos * expd;			/* expanded cosine */
      esin = gt_ffsin * expd;			/* expanded sine */
      len_expd = hypot(ecos*gt_xasp,esin/gt_yasp);
      rsh = 16*shift/sfac;		/* relative shift */

      c = *str++;
      if(iscntrl(c)) {			/* control chars are not in fonts */
	 c = 0;
      } else {
	 c -= ' ';
      }
      c += (font_id & ~ITALIC)*GT_NCHAR;	/* select font */
      
/* ************* Drawing the current character *****************************/
      if(draw_it) {
	 relo = 1;			/* start new character */
	 ila = (int)gt_il[c];			/* left adjust */

/**** DEBUG *****/
        if(gt_fp[c] - 1 >= jlp_ndata) {
        fprintf(stderr, "JLP 2006: Fatal error: gt_fp[c] = %d > ndata = %d\n",
                          gt_fp[c] - 1, jlp_ndata);
        }
	 t = &gt_font[gt_fp[c] - 1];
	 for(i=0;i < (int)gt_ns[c];i++) {
	    ix = *(t++);			/* x */
	    iy = *(t++);			/* y */
	    if(ix == 31) {
	       relo = 1;		/* is it a new start? */
	    } else {
	       x = ix - ila + ((font_id & ITALIC) ? SLANT*(iy-23) : 0);
	       y = iy + rsh;
	       ix = (int)(gt_xasp*(ecos*x - esin*y) + gt_x0);
	       iy = (int)((esin*x + ecos*y)/gt_yasp + gt_y0);
	       
	       if(relo) {
		  relo = 0;		/* relocate, reset flag */
		  /* JLP91 (replace sc_relocate...) */
		  gt_xp = ix; 
                  gt_yp = iy;
	       } else {
		  Jgd0.gdev_line(gt_xp, gt_yp, ix, iy);
	       }
	       gt_xp = ix;
	       gt_yp = iy;			/* store last pos */
	    }
	 }
      }
/* ************* end of character *****************************/
      xa = gt_ir[c] - gt_il[c];		/* width of char */
/*
printf(" gt_ir[c] >%s<, gt_il[c] >%s< and int %d %d \n",
                      gt_ir[c],gt_il[c],(int)gt_ir[c],(int)gt_il[c]);
printf(" xa %d \n",xa); 
*/
      gt_slen += len_expd*xa;
/* JLP97: debug 
if(!draw_it) printf(">%s< xa=%d, l_expd=%.2f gt_slen=%.2f \n",str,xa,len_expd,gt_slen);
*/
      gt_x0 += (int)(ecos*xa*gt_xasp);
      gt_y0 += (int)(esin*xa/gt_yasp);		/* set new char origin */

      sfac = sfac0;			/* reset up/down */
      shift = shift0;
   }
   /*NOTREACHED*/
}

/********************************************************************
* expand the definition of a "TeX" macro, if it is defined
* char *name;			name of "macro" 
* int draw_it,			do I actually want to draw this? 
*     font_id;			font to use 
* double sfac,			current expansion 
*       shift;			level of sub/superscript 
*/
void JLP_GText::tex_macro(JLP_GDev& Jgd0, char *name, int draw_it, 
                          int font_id, double sfac, double shift)
{
    char buf1[20];
	int italic = (font_id & ITALIC);	/* is font italic? */
#ifdef DEBUG
 printf("tex_macro/called with name=%s draw_it=%d\n", name, draw_it);
#endif

   switch (*name) {
    case ',':
    	strcpy(buf1, " ");
    	(void)tex_string(Jgd0,buf1,draw_it,SCRIPT | italic,sfac,shift); return;
    case '!':
    	strcpy(buf1, " ");
    	(void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
    case 'a':
      if(!strcmp(name,"aleph")) {
    	  strcpy(buf1, "H");
      	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"alpha")) {
    	  strcpy(buf1, "a");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"ast")) {
    	  strcpy(buf1, "`");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      } else if(!strcmp(name,"asteroid")) {
    	  strcpy(buf1, ",");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      }
      break;
    case 'b':
      if(!strcmp(name,"beta")) {
    	  strcpy(buf1, "b");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"bigcirc")) {
    	  strcpy(buf1, "O");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'c':
      if(!strcmp(name,"cents")) {
    	  strcpy(buf1, "C");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"chi")) {
    	  strcpy(buf1, "c");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"circ")) {
    	  strcpy(buf1, "`");
    	  (void)tex_string(Jgd0,buf1,draw_it,SCRIPT | italic,sfac,shift); return;
      } else if(!strcmp(name,"clover")) {
    	  strcpy(buf1, "@");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"clubsuit")) {
    	  strcpy(buf1, "~");
    	  (void)tex_string(Jgd0,buf1,draw_it,SCRIPT | italic,sfac,shift); return;
      } else if(!strcmp(name,"comet")) {
    	  strcpy(buf1, "+");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      }
      break;
    case 'd':
      if(!strcmp(name,"dag")) {
    	  strcpy(buf1, "[");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"ddag")) {
    	  strcpy(buf1, "]");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"del")) {
    	  strcpy(buf1, "J");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"downarrow")) {
    	  strcpy(buf1, "N");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"diamondsuit")) {
    	  strcpy(buf1, "\\}");
    	  (void)tex_string(Jgd0,buf1,draw_it,SCRIPT | italic,sfac,shift); return;
      } else if(!strcmp(name,"delta")) {
    	  strcpy(buf1, "d");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"div")) {
    	  strcpy(buf1, "/");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'e':
      if(!strcmp(name,"epsilon")) {
    	  strcpy(buf1, "e");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"equinox")) {
    	  strcpy(buf1, "\\^");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH| italic,sfac,shift); return;
      } else if(!strcmp(name,"equiv")) {
    	  strcpy(buf1, "=");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"eta")) {
    	  strcpy(buf1, "h");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'f':
      if(!strcmp(name,"firtree")) {
    	  strcpy(buf1, "v");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'g':
      if(!strcmp(name,"gamma")) {
    	  strcpy(buf1, "g");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"ge") || !strcmp(name,"geq")) {
    	  strcpy(buf1, ">");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'h':
      if(!strcmp(name,"hbar")) {
    	  strcpy(buf1, "Z");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"heartsuit")) {
    	  strcpy(buf1, "|");
    	  (void)tex_string(Jgd0,buf1,draw_it,SCRIPT | italic,sfac,shift); return;
      }
      break;
    case 'i':
      if(!strcmp(name,"infty")) {
    	  strcpy(buf1, "B");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"int")) {
    	  strcpy(buf1, ":");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"iota")) {
    	  strcpy(buf1, "i");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'j':
      break;
    case 'k':
      if(!strcmp(name,"kappa")) {
    	  strcpy(buf1, "k");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'l':
      if(!strcmp(name,"langle")) {
    	  strcpy(buf1, "\\{");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"larrow")) {
    	  strcpy(buf1, "~");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"le") || !strcmp(name,"leq")) {
    	  strcpy(buf1, "<");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"lambda")) {
    	  strcpy(buf1, "l");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'm':
      if(!strcmp(name,"mp")) {
    	  strcpy(buf1, "-");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"mu")) {
    	  strcpy(buf1, "m");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'n':
      if(!strcmp(name,"nabla")) {
    	  strcpy(buf1, "J");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"ne")) {
    	  strcpy(buf1, "?");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"nu")) {
    	  strcpy(buf1, "n");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'o':
      if(!strcmp(name,"odot")) {
    	  strcpy(buf1, "(");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"oint")) {
    	  strcpy(buf1, ";");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"omega")) {
    	  strcpy(buf1, "w");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"omicron")) {
    	  strcpy(buf1, "o");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"oplus")) {
    	  strcpy(buf1, ")");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"otimes")) {
    	  strcpy(buf1, "K");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'p':
      if(!strcmp(name,"palmtree")) {
    	  strcpy(buf1, "R");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"parallel")) {
    	  strcpy(buf1, "I");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"partial")) {
    	  strcpy(buf1, "j");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"perp")) {
    	  strcpy(buf1, "T");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"phi")) {
    	  strcpy(buf1, "f");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"pi")) {
    	  strcpy(buf1, "p");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"pm")) {
    	  strcpy(buf1, "+");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"propto")) {
    	  strcpy(buf1, ",");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"psi")) {
    	  strcpy(buf1, "y");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'q':
      break;
    case 'r':
      if(!strcmp(name,"rangle")) {
    	  strcpy(buf1, "\\}");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"rightarrow")) {
    	  strcpy(buf1, "\\ ");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"rho")) {
    	  strcpy(buf1, "r");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 's':
      if(!strcmp(name,"shield")) {
    	  strcpy(buf1, ".");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"sigma")) {
    	  strcpy(buf1, "s");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"snow")) {
    	  strcpy(buf1, "\\ ");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH |italic,sfac,shift); return;
      } else if(!strcmp(name,"spadesuit")) {
    	  strcpy(buf1, "\\}");
    	  (void)tex_string(Jgd0,buf1,draw_it,SCRIPT | italic,sfac,shift); return;
      } else if(!strcmp(name,"sqrt")) {
    	  strcpy(buf1, "`");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"sum")) {
    	  strcpy(buf1, "$");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 't':
      if(!strcmp(name,"tau")) {
    	  strcpy(buf1, "t");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"theta")) {
    	  strcpy(buf1, "q");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"times")) {
    	  strcpy(buf1, "*");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'u':
      if(!strcmp(name,"uparrow")) {
    	  strcpy(buf1, "M");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"upsilon")) {
    	  strcpy(buf1, "u");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'v':
      if(!strcmp(name,"varepsilon")) {
    	  strcpy(buf1, "!");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"varphi")) {
    	  strcpy(buf1, "#");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"vartheta")) {
    	  strcpy(buf1, "\"");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'w':
      break;
    case 'x':
      if(!strcmp(name,"xi")) {
    	  strcpy(buf1, "x");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'y':
      break;
    case 'z':
      if(!strcmp(name,"zeta")) {
    	  strcpy(buf1, "z");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'A':
      if(!strcmp(name,"AA")) {
    	  strcpy(buf1, "A");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"Alpha")) {
    	  strcpy(buf1, "A");
    	  (void)tex_string(Jgd0,buf1,draw_it,ROMAN | italic,sfac,shift); return;
      } else if(!strcmp(name,"Aquarius")) {
    	  strcpy(buf1, "[");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      } else if(!strcmp(name,"Aries")) {
    	  strcpy(buf1, "-");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      }
      break;
    case 'B':
      if(!strcmp(name,"Beta")) {
    	  strcpy(buf1, "B");
    	  (void)tex_string(Jgd0,buf1,draw_it,ROMAN | italic,sfac,shift); return;
      }
      break;
    case 'C':
      if(!strcmp(name,"Cancer")) {
    	  strcpy(buf1, ":");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      } else if(!strcmp(name,"Capricorn")) {
    	  strcpy(buf1, "@");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      } else if(!strcmp(name,"Chi")) {
    	  strcpy(buf1, "X");
    	  (void)tex_string(Jgd0,buf1,draw_it,ROMAN | italic,sfac,shift); return;
      }
      break;
    case 'D':
      if(!strcmp(name,"Delta")) {
    	  strcpy(buf1, "D");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'E':
      if(!strcmp(name,"Earth")) {
    	  strcpy(buf1, "#");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      } else if(!strcmp(name,"Epsilon")) {
    	  strcpy(buf1, "E");
    	  (void)tex_string(Jgd0,buf1,draw_it,ROMAN | italic,sfac,shift); return;
      } else if(!strcmp(name,"Eta")) {
    	  strcpy(buf1, "H");
    	  (void)tex_string(Jgd0,buf1,draw_it,ROMAN | italic,sfac,shift); return;
      }
      break;
    case 'F':
      break;
    case 'G':
      if(!strcmp(name,"Gamma")) {
    	  strcpy(buf1, "G");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"Gemini")) {
    	  strcpy(buf1, "/");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      }
      break;
    case 'H':
      break;
    case 'I':
      if(!strcmp(name,"Iota")) {
    	  strcpy(buf1, "I");
    	  (void)tex_string(Jgd0,buf1,draw_it,ROMAN | italic,sfac,shift); return;
      }
      break;
    case 'J':
      if(!strcmp(name,"Jupiter")) {
    	  strcpy(buf1, "%");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      }
      break;
    case 'K':
      if(!strcmp(name,"Kappa")) {
    	  strcpy(buf1, "K");
    	  (void)tex_string(Jgd0,buf1,draw_it,ROMAN | italic,sfac,shift); return;
      }
      break;
    case 'L':
      if(!strcmp(name,"Lambda")) {
    	  strcpy(buf1, "L");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"Leo")) {
    	  strcpy(buf1, ";");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      } else if(!strcmp(name,"Libra")) {
    	  strcpy(buf1, "=");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      }
      break;
    case 'M':
      if(!strcmp(name,"Mars")) {
    	  strcpy(buf1, "$");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      } else if(!strcmp(name,"Mercury")) {
    	  strcpy(buf1, "!");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      } else if(!strcmp(name,"Moon")) {
    	  strcpy(buf1, "*");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      } else if(!strcmp(name,"Mu")) {
    	  strcpy(buf1, "M");
    	  (void)tex_string(Jgd0,buf1,draw_it,ROMAN | italic,sfac,shift); return;
      }
      break;
    case 'N':
      if(!strcmp(name,"Neptune")) {
    	  strcpy(buf1, "(");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      } else if(!strcmp(name,"Nu")) {
    	  strcpy(buf1, "N");
    	  (void)tex_string(Jgd0,buf1,draw_it,ROMAN | italic,sfac,shift); return;
      }
      break;
    case 'O':
      if(!strcmp(name,"Omega")) {
    	  strcpy(buf1, "W");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"Omicron")) {
    	  strcpy(buf1, "O");
    	  (void)tex_string(Jgd0,buf1,draw_it,ROMAN | italic,sfac,shift); return;
      }
      break;
    case 'P':
      if(!strcmp(name,"Phi")) {
    	  strcpy(buf1, "F");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"Pisces")) {
    	  strcpy(buf1, "]");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      } else if(!strcmp(name,"Pluto")) {
    	  strcpy(buf1, ")");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      } else if(!strcmp(name,"Pi")) {
    	  strcpy(buf1, "P");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"Psi")) {
    	  strcpy(buf1, "Y");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'Q':
      break;
    case 'R':
      if(!strcmp(name,"Rho")) {
    	  strcpy(buf1, "R");
    	  (void)tex_string(Jgd0,buf1,draw_it,ROMAN | italic,sfac,shift); return;
      }
      break;
    case 'S':
      if(!strcmp(name,"S")) {
    	  strcpy(buf1, "&");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"Sagittarius")) {
    	  strcpy(buf1, "?");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      } else if(!strcmp(name,"Saturn")) {
    	  strcpy(buf1, "&");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      } else if(!strcmp(name,"Scorpio")) {
    	  strcpy(buf1, ">");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      } else if(!strcmp(name,"Sigma")) {
    	  strcpy(buf1, "S");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"Sqrt")) {
    	  strcpy(buf1, "\\^");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'T':
      if(!strcmp(name,"Tau")) {
    	  strcpy(buf1, "T");
    	  (void)tex_string(Jgd0,buf1,draw_it,ROMAN | italic,sfac,shift); return;
      } else if(!strcmp(name,"Taurus")) {
    	  strcpy(buf1, ".");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      } else if(!strcmp(name,"Theta")) {
    	  strcpy(buf1, "Q");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'U':
      if(!strcmp(name,"Upsilon")) {
    	  strcpy(buf1, "U");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      } else if(!strcmp(name,"Uranus")) {
    	  strcpy(buf1, "'");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      }
      break;
    case 'V':
      if(!strcmp(name,"Venus")) {
    	  strcpy(buf1, "\"");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      } else if(!strcmp(name,"Virgo")) {
    	  strcpy(buf1, "<");
    	  (void)tex_string(Jgd0,buf1,draw_it,OLD_ENGLISH | italic,sfac,shift); return;
      }
      break;
    case 'W':
      break;
    case 'X':
      if(!strcmp(name,"Xi")) {
    	  strcpy(buf1, "X");
    	  (void)tex_string(Jgd0,buf1,draw_it,GREEK | italic,sfac,shift); return;
      }
      break;
    case 'Y':
      break;
    case 'Z':
      if(!strcmp(name,"Zeta")) {
    	  strcpy(buf1, "Z");
    	  (void)tex_string(Jgd0,buf1,draw_it,ROMAN | italic,sfac,shift); return;
      }
      break;
    default:
      break;
   }
   if(draw_it) fprintf(stderr,"tex_macro/Unknown \"TeX\" macro: %s\n",name);
}

/* JLP: not used any longer...
 * Does string contain special "TeX" mode characters?
static int tex_special(char *s)
{
   for(;;) {
      switch (*s++) {
       case '\0':
	 return(0);
       case '\\': case '_': case '^': case '{': case '}':
	 return(1);
      }
   }
}
*/

/***********************************************************************
 * Draw a dot in the middle of a TeX string
 ***********************************************************************/
void JLP_GText::tex_dot(JLP_GDev& Jgd0, int n, int style, double expd)
{
int hwidth, ix, iy;	

ix = gt_xp;
iy = gt_yp;
hwidth = (int)(10. * expd);

Jgd0.gdev_line(ix,iy-hwidth,ix,iy+hwidth);
Jgd0.gdev_line(ix-hwidth,iy,ix+hwidth,iy);

hwidth = (hwidth*7)/10;
Jgd0.gdev_line(ix-hwidth,iy-hwidth,ix+hwidth,iy+hwidth);
Jgd0.gdev_line(ix-hwidth,iy+hwidth,ix+hwidth,iy-hwidth);

return;
}
/*********************************************************************
* tex_line
* Draw a line in the middle of a TeX string
* int lt: line type 
* int len: length, SCREEN units
**********************************************************************/
void JLP_GText::tex_line(JLP_GDev& Jgd0, int lt, int len)
{
int old_ltype, old_lwidth, device_height, device_width;
double gt_xasp, gt_yasp;

// aspect ratio of screen, y/x
 device_height = Jgd0.Jgc0_dev_height();
 device_width = Jgd0.Jgc0_dev_width();
 gt_xasp = (double)device_height / (double)device_width;     
 gt_yasp = 1. / gt_xasp; 

#ifdef DEBUG
printf("JLP_GText: tex_line called with lt=%d len=%d\n", lt, len);
#endif

old_ltype = Jgd0.Jgc0_ltype();
old_lwidth = Jgd0.Jgc0_lwidth();
Jgd0.Set_ltype_and_lwidth(lt,0);

   Jgd0.gdev_line(gt_xp, gt_yp, (int)(gt_xp + gt_xasp*len*gt_ffcos),
            (int)(gt_yp + len*gt_ffsin / gt_yasp));

Jgd0.Set_ltype_and_lwidth(old_ltype, old_lwidth);
return;
}
