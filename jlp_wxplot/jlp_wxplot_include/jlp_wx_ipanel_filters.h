/******************************************************************************
* Speck_filter.h
* Purpose:     filtering images
* Author:      JLP
* Version:     25/01/2015
******************************************************************************/
#ifndef speck_filter_h    // sentry
#define speck_filter_h

  int UnsharpMaskingFilter(double *in_image1, double *out_image2,
                           int nx2, int ny2, int iwidth);
  int HighContrastFilter1(double *modsq_centered, double *autoc,
                          double *UnresolvedModsqFrame,
                          const double SigmaUnresolvedModsq,
                          const int nx2, const int ny2);
  int HighContrastFilter2(double *modsq_centered, double *autoc,
                          int nx2, int ny2);
  int MedianProfileFilter(double *modsq_centered, double *autoc,
                          double *UnresolvedModsqFrame,
                          const double SigmaUnresolvedModsq,
                          bool version_2008, const int nx2, const int ny2);
  int ComputeProfile(double *ima0, int nx0, int ny0, double *pro0,
                     int np0, int ioption);
  int SubtractProfile(double *ima0, int nx0, int ny0, double *pro0,
                      int np0);
  int subtract_back_model(double *autoc_data, double *back_model,
                          double *autoc_flattened, int nx, int ny,
                          int idim);


#endif               // EOF sentry
