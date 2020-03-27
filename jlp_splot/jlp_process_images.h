/******************************************************************************
* Name:     jlp_process2.h 
*
* Purpose:  simple image processing used by JLP_iGDev_wxWID and JLP_wx_iGDProc1 
*           routines contained in jlp_process2.cpp 
*
* Author:   JLP 
* Version:  27/12/2015 
******************************************************************************/
#ifndef _jlp_process2_h
#define _jlp_process2_h

int GetRectangleBoxLimits2(double *x1_1, double *y1_1, double *x1_2, 
                           double *y1_2, int nx1, int ny1);
int ComputeThresholdsFromBox2(double *array1, int nx1, int ny1, double x1,
                            double y1, double x2, double y2, 
                            double *lower_itt, double *upper_itt,
                            char *results);
int StatisticsFromBox2(double *array1, int nx1, int ny1, double x1, 
                       double y1, double x2, double y2, char *results);
int PhotometryInCircle2(double *array1, int nx1, int ny1, double xc, 
                        double yc, double radius, char *results);
int AstrometryInCircle2(double *array1, int nx1, int ny1, double xc, 
                        double yc, double radius, char *results);
int CircleFromBox2(int nx1, int ny1, double x1_1,  double y1_1, double x1_2,  
                   double y1_2, double *xc, double *yc, double *radius);
int SliceFromLine2(double *array1, int nx1, int ny1, double x1,  double y1,
                   double x2,  double y2, double *xplot, double *yplot,
                   int *nplot, char *title, char *xlabel, char *ylabel);
int SumCircle2(double *array1, int nx, int ny, double xc, double yc,
               double radius, double *sum, double *npts);

#endif
