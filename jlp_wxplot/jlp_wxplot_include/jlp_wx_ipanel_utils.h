/*********************************************************************
* jlp_wx_ipanel_utils.h
* Miscellaneous programs used by JLP_wx_ipanel... classes
* JLP
* Version 03/08/2013
**********************************************************************/
#ifndef _jlp_wx_ipanel_utils_h
#define _jlp_wx_ipanel_utils_h

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

//#ifndef WX_PRECOMP
    #include "wx/wx.h"
//#endif

int gdp_sum_circle(double *array1, int nx, int ny, double xc, double yc,
                   double radius, double *sum, double *npts);
int gdp_offset_correction(double *array1, double *offset1, int nx1, int ny1,
                          int positive);
int gdp_ffield_correction(double *array1, double *offset1, int nx1, int ny1,
                          int sigma_level);
int gdp_statistics(double *array1, int nx1, int ny1, double* mean,
                   double* sigma);

#endif
