/***************************************************************
* Read the fonts files, and convert them to a binary format
* From Mongo
*
* JLP's version
* Version 14/10/2007
****************************************************************/
#include <stdio.h>
#include <stdlib.h>                 /* atoi() */
#include <ctype.h>
#include <math.h>
#include <malloc.h>
#include <fcntl.h>                  /* creat() ... */
#include <sys/unistd.h>             /* write(), read(), open(), close() ...*/

#define LSIZE 81		    /* length of a line */
#define LN2I 1.442695022	    /* 1/ln(e) */
#define NCHAR 96		    /* chars per font */
#define NFONT 6			    /* number of fonts */
#define TINY 1e-5
#ifdef VMS
#  define GOOD_EXIT 1
#  define BAD_EXIT 2
#else
#  define GOOD_EXIT 0
#  define BAD_EXIT 1
#endif

typedef struct {
   int nstroke,				/* number of strokes in a character */
       num,				/* number of character */
       offset,				/* offset in data */
       rank;				/* desired rank in data */
} GLYPH;


/* JLP 91 modification from char to MY_UCHAR 
 in 1991: MY_UCHAR=short integers instead of characters because
 of conflicts with IBM... 
*/
#define MY_UCHAR short int 

/* Prototypes of functions included here: */
static int get_nums(char *line, int *num, int *ns);
static int sort(GLYPH *array, int dimen);
static int read_index(GLYPH *index, char *indexfile);

int main(int ac, char *av[])
{
   char c,
   	line[LSIZE];
 char il,ir;                          /* left and right offsets */
 MY_UCHAR *font,		      /* actual description of characters */
        ladj[NCHAR*NFONT],		/* left adjustment of character */
        nstroke[NCHAR*NFONT],		/* offsets of characters in font */
        radj[NCHAR*NFONT];		/* right adjustment of character */

   FILE *infil;				/* in and out streams */
   GLYPH index[NCHAR*NFONT];		/* index to which characters we want */
   int i,j,k,
       nglyph,				/* number of glyphs requested */
       ns,				/* number of strokes in a glyph */
       num,				/* number of a glyph */
       off,				/* offset of a character */
       offset[NCHAR*NFONT],		/* offsets of characters in font */
       out,				/* fd for binary file */
       ndata;				/* number of ints describing chars */

   if(ac < 4) {
      fprintf(stderr,"Syntax: read_fonts data_file index_file binary_file\n");
      return(GOOD_EXIT);
   }

   if((infil = fopen(av[1],"r")) == NULL) {
      fprintf(stderr,"Can't open %s\n",av[1]);
      return(BAD_EXIT);
   }
   if((nglyph = read_index(index,av[2])) < 0) {
      return(BAD_EXIT);
   }
   if((out = creat(av[3],0644)) < 0) {
      fprintf(stderr,"Can't open %s\n",av[3]);
      return(BAD_EXIT);
   }
/*
 * Now sort index into increasing glyph number
 */
   sort(index,nglyph);
/*
 * index is in increasing glyph order, so read file to find offsets
 */
   for(i = 0;i < nglyph;) {
      if(fgets(line,LSIZE,infil) == NULL) {
	 fprintf(stderr,"Reached end of file after reading %d glyphs\n",i);
	 return(BAD_EXIT);
      }
      if(get_nums(line,&num,&ns) < 2) {
	 fprintf(stderr,"Error reading %dth glyph\n",i+1);
	 fprintf(stderr,"\"%s\"\n",line);
	 return(BAD_EXIT);
      }
/*
 * Reading a continuation line for ns == 32 is due to a bug in
 * the code that wrote the font tables: they include a blank line
 */
      if(ns >= 32) fgets(line,LSIZE,infil); /* skip continuation lines */
      for(j = ns - 68;j > 0;j -= 36) {
	 fgets(line,LSIZE,infil);
      }

      if(num > index[i].num) {
	 fprintf(stderr,"Glyph %d is missing\n",index[i].num);
	 return(BAD_EXIT);
      } else if(num == index[i].num) {
	 nstroke[index[i].rank] = index[i].nstroke = ns - 1;
	 i++;
	 while(index[i].num == num) {
	    nstroke[index[i].rank] = index[i].nstroke = ns - 1;
	    i++;
	 }
      }
   }
   fseek(infil,0,0);			/* rewind file */
/*
 * We now have the lengths of all the characters, so we can find the offsets.
 */
   for(off = i = 0;i < nglyph;i++) {
      index[i].offset = off;
      offset[index[i].rank] = off + 1;	/* offset is 1-indexed */
      if(i > 0 && index[i].num == index[i-1].num) {
	 offset[index[i].rank] = offset[index[i-1].rank];
      } else {
	 off += 2*index[i].nstroke;	/* increment offset for next char */
      }
   }

   ndata = off;
/* JLP 91 */
   if((font = (MY_UCHAR *)malloc(ndata*sizeof(MY_UCHAR))) == NULL) {
      fprintf(stderr,"Can't malloc space for array font\n");
      return(BAD_EXIT);
   }
/*
 * So read the offsets etc.
 */
   for(i = 0;i < nglyph;) {
      if(fgets(line,LSIZE,infil) == NULL) {
	 fprintf(stderr,"Reached end of file after reading %d glyphs\n",i);
	 return(BAD_EXIT);
      }
      if(get_nums(line,&num,&ns) < 2) {
	 fprintf(stderr,"Error reading %dth glyph\n",i+1);
	 return(BAD_EXIT);
      }
      if(num < index[i].num) {
/*
 * Reading a continuation line for ns == 32 is due to a bug in
 * the code that wrote the font tables: they include a blank line
 */
	 if(ns >= 32) fgets(line,LSIZE,infil); /* continuation lines */
	 for(j = ns - 68;j > 0;j -= 36) {
	    fgets(line,LSIZE,infil);
	 }
      } if(num > index[i].num) {
	 fprintf(stderr,"Glyph %d is missing\n",index[i].num);
	 return(BAD_EXIT);
      } else if(num == index[i].num) {
	 sscanf(&line[8],"%c",&il);
	 ladj[index[i].rank] = (MY_UCHAR) (il - 'R');
	 sscanf(&line[9],"%c",&ir);
	 radj[index[i].rank] = (MY_UCHAR) (ir - 'R');
	 off = index[i].offset;
	 ns--;				/* first `stroke' is [lr]adj */
	 for(j = 0,k = 10;j < ns;j++,k += 2) {
	    if(k == 72) {		/* next line please */
	       if(fgets(line,LSIZE,infil) == NULL) {
		  fprintf(stderr,"Reached end of file in %dth glyph\n",i);
		  return(BAD_EXIT);
	       }
	       k = 0;
	    }
	    sscanf(&line[k],"%c",&c);	/* x */
/* JLP91
	    font[off + 2*j] = (c == ' ') ? (char)31 : c - 'R';
*/
	    font[off + 2*j] = (c == ' ') ? (MY_UCHAR)31 : (MY_UCHAR) (c - 'R');
	    sscanf(&line[k+1],"%c",&c);	/* y */
	    font[off + 2*j + 1] = (MY_UCHAR) (-(c - 'R'));
	 }
	 if(ns == 31) fgets(line,LSIZE,infil); /* 32 bug again */
	 i++;
	 while(index[i].num == num) {
	    ladj[index[i].rank] = il - 'R';
	    radj[index[i].rank] = ir - 'R';
	    i++;
	 }
      }
   }

   write(out,&nglyph,sizeof(int));
   write(out,&ndata,sizeof(int));
/* JLP 91 */
   write(out,nstroke,nglyph*sizeof(MY_UCHAR));
   write(out,ladj,nglyph*sizeof (MY_UCHAR));
   write(out,radj,nglyph*sizeof(MY_UCHAR));
   write(out,offset,nglyph*sizeof(int));
/* JLP 91 */
   write(out,font,ndata*sizeof(MY_UCHAR));

return (GOOD_EXIT);
}

/*************************************************************/
/*
 * Read the index file, which should consist of a list of
 * desired glyphs (by Hershey number), or ranges of glyphs
 */
static int read_index(GLYPH *index, char *indexfile)
{
   char c,
   	line[LSIZE];
   FILE *fil;
   int i,j,k,
       num;				/* number of glyph */

   if((fil = fopen(indexfile,"r")) == NULL) {
      fprintf(stderr,"Can't open %s\n",indexfile);
      return(-1);
   }

   for(i = 0;i < NFONT;i++) {
      for(j = 0;j < NCHAR;) {
	 for(;;) {			/* deal with comments */
#ifdef VMS       			/* VMS has a bug with mixing i/o: */
	    fscanf(fil,"%[ \t\n\f]",line); /* skip whitespace */
      	    if(fscanf(fil,"%[#]",line) <= 0) { /* Does field start with a #? */
	       break;
	    }
	    fscanf(fil,"%*[^\n]\n");	/* eat rest of line */
#else
	    while(c = getc(fil), isspace(c)) ;
	    if(c == '#') {		/* a comment */
     	       fgets(line,LSIZE,fil);	/* read to end of line */
	    } else { 
	       ungetc(c,fil);
	       break;
	    }
#endif
	 }

	 if(fscanf(fil,"%d",&num) < 1) {
	    fclose(fil);
	    return(NCHAR*i + j);
	 }
	 if(num > 0) {
	    index[i*NCHAR + j].num = num;
	    index[i*NCHAR + j].rank = i*NCHAR + j;
	    j++;
	 } else {
	    for(k = index[i*NCHAR + j - 1].num + 1;k <= -num;k++) {
	       if(j >= NCHAR) {
		  fprintf(stderr,"Range to %d doesn't fit in %dth font\n",
			  					      -num,i);
		  break;
	       }
	       index[i*NCHAR + j].num = k;
	       index[i*NCHAR + j].rank = i*NCHAR + j;
	       j++;
	    }
	 }
      }
   }

   fclose(fil);
   return(NCHAR*NFONT);
}
      

/*******************************************************
*
*******************************************************/
static int get_nums(char *line, int *num, int *ns)
{
   char buff[6];
   int i;
   
   for(i = 0;i < 5;i++) buff[i] = line[i];
   buff[i] = '\0';
   *num = atoi(buff);
   for(i = 0;i < 3;i++) buff[i] = line[i+5];
   buff[i] = '\0';
   *ns = atoi(buff);
   if(*num == 0 && *ns == 0) {
      return(0);
   }
   return(2);
}

/*
 * We could use qsort, except that it is missing under VMS
 *
 * The sort is a Shell sort, taken from Press et.al. Numerical Recipes.
 */
static int sort(GLYPH *array, int dimen)
{
   GLYPH temp;
   int i,j,m,n,				/* counters */
       lognb2;				/* (int)(log_2(dimen)) */

   lognb2 = (int)log((double)dimen)*LN2I + TINY;	/* ~ log_2(dimen) */
   m = dimen;
   for(n = 0;n < lognb2;n++) {
      m /= 2;
      for(j = m;j < dimen;j++) {
         i = j - m;
	 temp = array[j];
	 while(i >= 0 && temp.num < array[i].num) {
	    array[i + m] = array[i];
	    i -= m;
	 }
	 array[i + m] = temp;
      }
   }
return(0);
}
