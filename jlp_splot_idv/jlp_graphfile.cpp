/***************************************************************
* To read a graphic file and display labels on the selected device
*
* VERSION: 15-01-2007
*
* AUTHOR: JLP
***************************************************************/
/* When defined send postscript file automatically to printer
#define HARDCOPY_TO_PRINTER
*/

#include <stdio.h>
#include <math.h>
#include <malloc.h>
#include <string.h>

#include "jlp_splot_idv.h"

/*
#define DEBUG
*/
#ifndef PI
#define PI 3.14159265358979323846
#endif

/*********************************************************************
* Graphic file 
*
* Example:

#Comments
/Comments
\Comments
%Comments
jlp_label1 150.604218 151.736603 0.000000 1.0 1.0 @Teds rtdhbds@
jlp_line1 94.000000 87.000000 141.631409 114.499977
jlp_symbol1 174. 215. 90 4 
jlp_symbol2 174. 215. 90. 4 
 23. 45.
 43. 67.
> 21. 34.
> 34. 56.
> 35. 45.
#Other comments

*********************************************************************/
int jlp_read_graphic_file(char *graphic_file, int idv)
{
int ix, iy, ix1, ix2, iy1, iy2, isize, isymb;
double x1, y1, x2, y2, angle1, expand1, size;
float f_x1, f_y1, f_x2, f_y2, f_size;
char buffer[81], s1[81], *pc;
FILE *graphic_to_read;

  if((graphic_to_read = fopen(graphic_file,"r")) == NULL)
    {
      fprintf(stderr,"read_graphic_file/Error opening input graphic file %s\n",
               graphic_file);
      return(-1);
    }

/* Main loop: */
 while(fgets(buffer,80,graphic_to_read) != NULL)
   {
/* Allowed calls: jlp_label, jlp_label1, jlp_symbol, jlp_line, jlp_line1 */
/*************************** jlp_label *************************/
/* Mongo coordinates */
     if(!strncmp(buffer,"jlp_label ",10))
       {
/* As the next line did not work with blanks within the string s1:
       sscanf(buffer,"jlp_label %d %d %lf %lf @%s@\n",
                                &ix,&iy,&angle1,&expand1,s1);
I use the following: */
       sscanf(buffer,"jlp_label %d %d %lf %lf \n",
                                &ix,&iy,&angle1,&expand1);
       pc = buffer;
       while(*pc && *pc != '@') pc++;
       if(*pc) pc++;
       strcpy(s1,pc);
       pc = s1;
       while(*pc && *pc != '@') pc++;
       *pc = '\0';

#ifdef DEBUG
       printf("Xd/jlp_label %d %d %f %.1f @%s@\n",
                              (int)ix,(int)iy,angle1,expand1,s1);
#endif
       jlp_label(s1,ix,iy,angle1,expand1,1,idv);
       }
/*************************** jlp_label1 *************************/
/* User coordinates */
     else if(!strncmp(buffer,"jlp_label1",10))
       {
/* As the next line did not work with blanks within the string s1:
       sscanf(buffer,"jlp_label1 %lf %lf %lf %lf @%s@\n",
                                &x1,&y1,&angle1,&expand1,s1);
I use the following: */
       sscanf(buffer,"jlp_label1 %lf %lf %lf %lf \n",
                                &x1,&y1,&angle1,&expand1);
       pc = buffer;
       while(*pc && *pc != '@') pc++;
       if(*pc) pc++;
       strcpy(s1,pc);
       pc = s1;
       while(*pc && *pc != '@') pc++;
       *pc = '\0';

#ifdef DEBUG
       printf("Xd/jlp_label1 %f %f %f %.1f @%s@\n",
                                 x1,y1,angle1,expand1,s1);
#endif
       jlp_label1(s1,x1,y1,angle1,expand1,1,idv);
       }
/*************************** jlp_symbol *************************/
/* Mongo coordinates */
     else if(!strncmp(buffer,"jlp_symbol ",11))
       {
       sscanf(buffer,"jlp_symbol %d %d %d %d\n",&ix,&iy,&isize,&isymb);
#ifdef DEBUG
       printf("Xd/jlp_symbol %d %d %d %d\n",
               (int)ix,(int)iy,(int)isize,(int)isymb);
#endif
/* JLP_SYMBOL calls "jlp_line": */
       JLP_SYMBOL(&ix,&iy,&isize,&isymb,&idv);
       } 
/*************************** jlp_symbol1 *************************/
/* User coordinates */
     else if(!strncmp(buffer,"jlp_symbol1",11))
       {
       sscanf(buffer,"jlp_symbol1 %lf %lf %d %d\n",&x1,&y1,&isize,&isymb);
#ifdef DEBUG
       printf("Xd/jlp_symbol1 %lf %lf %d %d\n",x1,y1,isize,isymb);
#endif
/* JLP_SYMBOL1 calls "jlp_line": */
       f_x1 = x1; f_y1 = y1;
       JLP_SYMBOL1(&f_x1,&f_y1,&isize,&isymb,&idv);
       } 
/*************************** jlp_symbol2 *************************/
/* User coordinates */
     else if(!strncmp(buffer,"jlp_symbol2",11))
       {
       sscanf(buffer,"jlp_symbol2 %lf %lf %lf %d\n",&x1,&y1,&size,&isymb);
#ifdef DEBUG
       printf("Xd/jlp_symbol2 %f %f %f %d\n",x1,y1,size,isymb);
#endif
/* JLP_SYMBOL2 calls "jlp_line": */
       f_x1 = x1; f_y1 = y1;
       f_size = size;
       JLP_SYMBOL2(&f_x1,&f_y1,&f_size,&isymb,&idv);
       } 
/*************************** jlp_line *************************/
/* mgo coordinates */
     else if(!strncmp(buffer,"jlp_line ",9))
       {
       sscanf(buffer,"jlp_line %d %d %d %d\n",&ix1,&iy1,&ix2,&iy2);
#ifdef DEBUG
       printf("Xd/jlp_line %d %d %d %d\n",ix1,iy1,ix2,iy2);
#endif
       jlp_line(ix1,iy1,ix2,iy2,idv);
       } 
/*************************** jlp_line1 *************************/
/* user coordinates */
     else if(!strncmp(buffer,"jlp_line1",9))
       {
       sscanf(buffer,"jlp_line1 %lf %lf %lf %lf\n",&x1,&y1,&x2,&y2);
#ifdef DEBUG
       printf("Xd/jlp_line1 %f %f %f %f\n",x1,y1,x2,y2);
#endif
       f_x1 = x1; f_y1 = y1;
       f_x2 = x2; f_y2 = y2;
       JLP_LINE1(&f_x1, &f_y1, &f_x2, &f_y2, &idv);
       } 
/*************************** comments *************************/
     else if(buffer[0] == '#' || buffer[0] == '\\' || buffer[0] == '/'
             || buffer[0] == '%')
       {
       continue;
       } 
/*************************** X Y coordinates *************************/
/* Draw crosses at the corresponding user coordinates */
     else 
       {
       if(buffer[0] == '>')
           sscanf(&buffer[1],"%lf %lf",&x1,&y1);
       else
           sscanf(buffer,"%lf %lf",&x1,&y1);
#ifdef DEBUG
       printf(buffer,"coordinates: X=%f Y=%f\n",x1,y1);
#endif
       isize = 5; isymb = 8;
       f_x1 = x1; f_y1 = y1;
       JLP_SYMBOL1(&f_x1,&f_y1,&isize,&isymb,&idv);
       } 
/* End of while: */
   }
fclose(graphic_to_read);

return(0);
}
