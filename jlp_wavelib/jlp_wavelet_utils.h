/***********************************************************************
* jlp_wavelet_utils.h
* Prototypes of functions included in jlp_wavelet_utils.h:
*
* JLP
* Version 27/11/2020
***********************************************************************/
#ifndef jlp_wavelet_h_
#define jlp_wavelet_h_

#include <stdlib.h> // exit()

#include <vector>
#include <string>

#include "wavelet2s.h"
#include "jlp_fitsio.h"         // for JLP_RDFITS_2D

void findthresh(vector<double> &vector1, int N, double& t);
void* maxval_2d(vector<vector<double> > &arr, double &max);
void* maxval_1d(vector<double> &arr, double &max);
int jlp_image_to_vector(double *dble_image1, int nx1, int ny1,
                        vector<vector<double> > &vec1);
int jlp_LoadFITSImage(char *filename1, double **dble_image1, 
                      int *nx1, int *ny1, char *comments1, char *errmess1);
int jlp_dwt_2d_transform(vector<vector<double> > &vec1, int ny1, int nx1,
                         double **dble_image2, int *nx2, int *ny2,
                         double **dble_image3, int *nx3, int *ny3);
int jlp_dwt_2d_subset_largecoef(vector<double> &output0, 
                                vector<double> &dwt_coef1,
                                vector<double> flag0, vector<int> length0, 
                                vector<int> length2,
                                string nm0, int rows_n, int cols_n,
                                double **dble_image3, int *nx3, int *ny3,
                                double coef_frac1);
int jlp_dwt_2d_medium_bandpass(vector<double> &output0,
                                vector<double> &dwt_coef1,
                                vector<double> flag0, vector<int> length0,
                                vector<int> length2,
                                string nm0, int rows_n, int cols_n,
                                double **dble_image3, int *nx3, int *ny3);
int jlp_dwt_2d_sym_transform(vector<vector<double> > &vec1, int ny1, int nx1,
                             double **dble_image2, int *nx2, int *ny2,
                             double **dble_image3, int *nx3, int *ny3);
int jlp_idwt_reconstructed( vector<double> output0, vector<double> flag0, 
                            vector<int> length0, string nm0, int nx1, int ny1,
                            double **dble_image3, int *nx3, int *ny3);
int jlp_swt_2d_transform(vector<vector<double> > &vec1, int ny1, int nx1,
                         double **dble_image2, int *nx2, int *ny2,
                         double **dble_image3, int *nx3, int *ny3);

#endif
