/******************************************************************
* "jlp_splot1.cpp" 
*
* JLP
* Version 15/08/2013
*************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>  
#include <math.h>

#include "jlp_splot_idv.h" 
#include "jlp_gdev.h"    // JLP Graphic Device class
#include "jlp_gdev_pst.h"          // JLP PST Curve Graphic class
#include "jlp_gdev_idv.h"

#if JLP_USE_X11          /* New flag to disable X11 if necessary */
#include "jlp_gdev_x11.h"       // JLP X11 graphic class (in jlp_x11plot/)
#endif
// No longer used (October 2014 ...)
#if JLP_USE_WXWID          /* New flag to disable wxWidgets if necessary */
//#include "jlp_cgdev_wxwid.h"     // JLP wxWidgets graphic class (in jlp_wxplot/)
#endif

/*
#define DEBUG
*/

/** Contains:
int jlp_line(int x1, int y1, int x2, int y2, int idv);
int jlp_polygon(int x, int y, float expand, float angle, int nsides,
                int filled, int idv);
int jlp_erase(int idv);
int jlp_cursor(int *ix, int *iy, char *cursor_type, int *pressed_button,
               int idv);
float jlp_label_backup(char *s, int ixstart, int iystart, float angle1,
                       float expand1, int drawit, int backup_to_file,
                       int idv);
float jlp_label(char *s, int ixstart, int iystart, float angle1,
                float expand1, int drawit, int idv);
float jlp_label1(char *s, float xstart, float ystart, float angle1,
                 float expand1, int drawit, int idv);
float jlp_label1_backup(char *s, float xstart, float ystart, float angle1,
                        float expand1, int drawit, int backup_to_file,
                        int idv);
int jlp_open_backup_file(int idv, char * fname);
int jlp_close_backup_file(int idv);
**/

/* *************************************************************
 * To draw a line from x1,y1 to x2,y2
 * "Mongo" coordinates
 ***************************************************************/
int jlp_line(int x1, int y1, int x2, int y2, int idv)
{
int status = -1;

 if(GDev_from_idv(idv)) {
   GDev_from_idv(idv)->gdev_line(x1, y1, x2, y2);
   status = 0;
   }

return(status);
}

/***************************************************************
* Draw a polygon
*
* nsides: Number of sides
***************************************************************/
int jlp_polygon(int x, int y, float expand, float angle, int nsides, 
                int filled, int idv)
{
int status = -1;

 if(GDev_from_idv(idv)) {
   GDev_from_idv(idv)->polygon_device(x, y, expand, angle, nsides, filled);
   status = 0;
   }

return(status);
}
/* *************************************************************
 * Erase everything
 ***************************************************************/
int jlp_erase(int idv)
{
int status = -1;

 if(GDev_from_idv(idv)) {
   GDev_from_idv(idv)->gdev_erase();
   status = 0;
   }

return(status);
}
/* *************************************************************************
 *
 * INPUT:
 *
 * OUTPUT:
 * pressed_button: 0 if no button recognized, 
 *                 1, 2 or 3 according to the button pressed on the mouse 
 ***************************************************************************/
int jlp_cursor(int *ix, int *iy, char *cursor_type, int *pressed_button, 
               int idv)
{
int jx, jy, pbutton;
int status = -1;

*ix = *iy = *pressed_button = 0;

 if(GDev_from_idv(idv)) {
   status = GDev_from_idv(idv)->cursor(jx, jy, cursor_type, pbutton);
   }
 *ix = jx; *iy = jy; *pressed_button = pbutton;

return(status);
}
/****************************************************************
* float jlp_label_backup
*
* Write a string to the plotter (if drawit is true, otherwise simply
* find it's length)
*
* char *s: string to write
* ixstart, iystart:  starting position (mgo coord.)
* drawit: should I really draw it?
* angle1: angle in degrees
*
* OUTPUT:
* length in mgo coordinates
****************************************************************/
float jlp_label_backup(char *s, int ixstart, int iystart, float angle1,
                       float expand1, int drawit, int backup_to_file, 
                       int idv)
{
float length = 0.;

 if(GDev_from_idv(idv)) {
   length = GDev_from_idv(idv)->gdev_label(s,ixstart,iystart,angle1,
                                           expand1, drawit, backup_to_file);
   }

return(length);
}
/****************************************************************
* float jlp_label
* (For Fortran interface use JLP_SPLABEL)
* Write a string to the plotter (if drawit is true, otherwise simply
* find it's length)
*
* Simpler version of jlp_label_backup
*
* char *s: string to write
* ixstart, iystart:  starting position (mgo coord.)
* drawit: should I really draw it?
* angle1: angle in degrees
*
* OUTPUT:
* length in mgo coordinates
****************************************************************/
float jlp_label(char *s, int ixstart, int iystart, float angle1,
                float expand1, int drawit, int idv)
{
float length = 0.;
int backup_to_file = 0; 

 if(GDev_from_idv(idv)) {
   length = GDev_from_idv(idv)->gdev_label(s,ixstart,iystart,angle1,
                                           expand1, drawit, backup_to_file);
   }

return(length);
}
/*****************************************************************
* jlp_label1
* Simple version of jlp_label1_backup
*
* OUTPUT:
* length in mgo coordinates
******************************************************************/
float jlp_label1(char *s, float xstart, float ystart, float angle1,
                 float expand1, int drawit, int idv)
{
float length;
int backup_to_file = 0;

length = jlp_label1_backup(s,xstart,ystart,angle1,expand1,drawit,backup_to_file,
                           idv);

return(length);
}
/*****************************************************************
* jlp_label1_backup
* Draw a label at location (start,ystart) in  user coordinates
*                                            (assuming linear scale)
*
* OUTPUT:
* length in mgo coordinates
******************************************************************/
float jlp_label1_backup(char *s, float xstart, float ystart, float angle1,
                        float expand1, int drawit, int backup_to_file,
                        int idv)
{
float length = 0.;

 if(GDev_from_idv(idv)) {
   length = GDev_from_idv(idv)->gdev_label1(s,xstart,ystart,angle1,expand1,
                                            drawit, backup_to_file);
   }

return(length);
}
/*******************************************************************
*
*******************************************************************/
int jlp_open_backup_file(int idv, char *fname)
{
int status = -1;

 if(GDev_from_idv(idv)) {
   status = GDev_from_idv(idv)->open_backup_file(fname);
   }

return(status);
}
/*******************************************************************
*
*******************************************************************/
int jlp_close_backup_file(int idv)
{
int status = -1;

 if(GDev_from_idv(idv)) {
   status = GDev_from_idv(idv)->close_backup_file();
   }

return(status);
}
