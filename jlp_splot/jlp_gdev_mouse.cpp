/**************************************************************************
* "jlp_gdev_mouse.cpp"
*
* Definition of the members of the JLP_GDev class
* used when handling mouse events 
*
* JLP
* Version 10/02/2017
**************************************************************************/
#include "jlp_gdev.h"

/* Defined here:
  int Mouse_AddLeftDownPoint(const double dev_x0, const double dev_y0);
  int Mouse_AddLeftUpPoint(const double dev_x0, const double dev_y0);
  int Mouse_GetLastLeftDownPoint(double *dev_x0, double *dev_y0, int *npts0);
  int Mouse_GetLastLeftUpPoint(double *dev_x0, double *dev_y0, int *npts0);
  int Mouse_RemoveLastLeftDownPoint();
  int Mouse_RemoveLastLeftUpPoint();
*/

/***********************************************************************
* Add a new point to the list of left-down points
*
* INPUT:
* dev_x0, dev_y0 : device coordinates of the input point
***********************************************************************/
int JLP_GDev::Mouse_AddLeftDownPoint(const double dev_x0, const double dev_y0)
{

 if(npts_LeftDown_1 < MAX_CURSOR_NPOINTS - 1) {
// Check if the user has not clicked twice by error:
   if(npts_LeftDown_1 > 0) {
     if(dev_x0 != LeftDown_dev_x_1[npts_LeftDown_1 - 1] &&
        dev_y0 != LeftDown_dev_y_1[npts_LeftDown_1 - 1]){
        LeftDown_dev_x_1[npts_LeftDown_1] = dev_x0;
        LeftDown_dev_y_1[npts_LeftDown_1] = dev_y0;
        npts_LeftDown_1++;
        } // Case of new point different from previous
// Case when no previous point has been entered
     } else {
        LeftDown_dev_x_1[npts_LeftDown_1] = dev_x0;
        LeftDown_dev_y_1[npts_LeftDown_1] = dev_y0;
        npts_LeftDown_1++;
      }
    } else {
      fprintf(stderr, "OnLeftDown/Error: too many points entered (max=%d)\n",
              npts_LeftDown_1);
      return(-1);
    }

#ifdef DEBUG
printf("add_left_down/ dev %f %f (n=%d)\n", dev_x0, dev_y0, npts_LeftDown_1 - 1);
#endif

return(0);
}
/***********************************************************************
* Add a new point to the list of left-up points
*
* INPUT:
* dev_x0, dev_y0 : device coordinates of the input point
***********************************************************************/
int JLP_GDev::Mouse_AddLeftUpPoint(const double dev_x0, const double dev_y0)
{

 if(npts_LeftUp_1 < MAX_CURSOR_NPOINTS - 1) {
// Check if the user has not clicked twice by error:
   if(npts_LeftUp_1 > 0) {
     if(dev_x0 != LeftUp_dev_x_1[npts_LeftUp_1 - 1] &&
        dev_y0 != LeftUp_dev_y_1[npts_LeftUp_1 - 1]){
        LeftUp_dev_x_1[npts_LeftUp_1] = dev_x0;
        LeftUp_dev_y_1[npts_LeftUp_1] = dev_y0;
        npts_LeftUp_1++;
        } // Case of new point different from previous
// Case when no previous point has been entered
     } else {
       LeftUp_dev_x_1[npts_LeftUp_1] = dev_x0;
       LeftUp_dev_y_1[npts_LeftUp_1] = dev_y0;
       npts_LeftUp_1++;
     }
   } else {
    fprintf(stderr, "OnLeftUp/Error: too many points entered (max=%d)\n",
            npts_LeftUp_1);
    return(-1);
   }

#ifdef DEBUG
printf("add_left_up/ dev %f %f (n=%d)\n", dev_x0, dev_y0, npts_LeftUp_1 - 1);
#endif

return(0);
}
/***********************************************************************
* Get the coordinates of the last entered left-down point
*
* OUTPUT:
* dev_x0, dev_y0 : device coordinates of the input point
* npts0 : current total number of points 
***********************************************************************/
int JLP_GDev::Mouse_GetLastLeftDownPoint(double *dev_x0, double *dev_y0, int *npts0)
{
int ii, status = -1;

*dev_x0 = -1;
*dev_y0 = -1;
*npts0 = 0;

if(npts_LeftDown_1 > 0) {
  *npts0 = npts_LeftDown_1;
  ii = npts_LeftDown_1 - 1;
  *dev_x0 = LeftDown_dev_x_1[ii];
  *dev_y0 = LeftDown_dev_y_1[ii]; 
  status = 0;
  }

return(status);
}
/***********************************************************************
* Get the coordinates of the last entered left-up point
*
* OUTPUT:
* dev_x0, dev_y0 : device coordinates of the input point
* npts0 : current total number of points
***********************************************************************/
int JLP_GDev::Mouse_GetLastLeftUpPoint(double *dev_x0, double *dev_y0, int *npts0)
{
int ii, status = -1;

*dev_x0 = -1;
*dev_y0 = -1;
*npts0 = 0;

if(npts_LeftUp_1 > 0) {
  *npts0 = npts_LeftUp_1;
  ii = npts_LeftUp_1 - 1;
  *dev_x0 = LeftUp_dev_x_1[ii];
  *dev_y0 = LeftUp_dev_y_1[ii];
  status = 0;
  }

return(status);
}
/***********************************************************************
* Remove the last entered point from the list
*
***********************************************************************/
int JLP_GDev::Mouse_RemoveLastLeftDownPoint()
{
int status = -1;

if(npts_LeftDown_1 > 0) {
  npts_LeftDown_1--;
  status = 0;
  } else {
  npts_LeftDown_1 = 0;
  }

return(status);
}
/***********************************************************************
* Remove the last entered point from the list
*
***********************************************************************/
int JLP_GDev::Mouse_RemoveLastLeftUpPoint()
{
int status = -1;

if(npts_LeftUp_1 > 0) {
  npts_LeftUp_1--;
  status = 0;
  } else {
  npts_LeftUp_1 = 0;
  }

return(status);
}
/***********************************************************************
* Erase all points from the list
***********************************************************************/
void JLP_GDev::Mouse_EraseLeftDownPoints()
{
 npts_LeftDown_1 = 0;
return;
}
/***********************************************************************
* Erase all points from the list
***********************************************************************/
void JLP_GDev::Mouse_EraseLeftUpPoints()
{
 npts_LeftUp_1 = 0;
return;
}
