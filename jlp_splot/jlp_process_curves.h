/******************************************************************************
* Name:     jlp_process_curve.h 
*
* Purpose:  simple image processing used by JLP_iGDev_wxWID and JLP_wx_iGDProc1 
*           routines contained in jlp_process2.cpp 
*
* Author:   JLP 
* Version:  07/02/2016
******************************************************************************/
#ifndef _jlp_process_curve_h
#define _jlp_process_curve_h

int GetRectangleBoxLimits1(double *x1_1, double *y1_1, double *x1_2,
                           double *y1_2);
int StatisticsFromBox1(double *xplot0, double *yplot0, int npts0, double x1,
                       double x2, char *results);
int PhotometryInBox1(double *xplot0, double *yplot0, int npts0, double x1,
                     double x2, char *results);
int AstrometryInBox1(double *xplot0, double *yplot0, int npts0, double x1,
                     double x2, char *results);
int SumBox1(double *xplot0, double *yplot0, int npts0, double x1,
            double x2, double *sum2, double *npts2);

#endif
