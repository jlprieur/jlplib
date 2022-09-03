//============================================================================
// Name        : jlp_imagedemo1.cpp
// From imagedemo1.cpp by Raffat Hussain
// Author      : JLP 
// Version     :  05/12/2020
// Copyright   : 
// Description : DWT of arbitrary size image using symmetric or periodic extension
//============================================================================

#include <stdlib.h> // exit()

#include <vector>
#include <string>

#include "wavelet2s.h"
#include "jlp_fitsio.h"         // for JLP_RDFITS_2D

#include "jlp_wavelet_utils.h"

/* defined in jlp_wavelet_utils.h
int jlp_LoadFITSImage(char *filename1, double **dble_image1, 
                      int *nx1, int *ny1, char *comments1, char *errmess1);
int jlp_image_to_vector(double *dble_image1, int nx1, int ny1,
                        vector<vector<double> > &vec1);
int jlp_dwt_2d_sym_transform(vector<vector<double> > &vec1, int ny1, int nx1,
                             double **dble_image2, int *nx2, int *ny2,
                             double **dble_image3, int *nx3, int *ny3);
*/

/***********************************************************************
*
*************************************************************************/
int main(int argc, char* argv[]) {
char filename1[120], comments1[80], errmess1[200];
double *dble_image1, *dble_image2, *dble_image3;
int nx1, ny1, nx2, ny2, nx3, ny3;

/*
strcpy(filename1, "lena512.fits");
*/
strcpy(filename1, "snow.fits");
jlp_LoadFITSImage(filename1, &dble_image1, &nx1, &ny1, comments1, errmess1);

vector<vector<double> > vec1(ny1, vector<double>(nx1));
jlp_image_to_vector(dble_image1, nx1, ny1, vec1);

jlp_dwt_2d_sym_transform(vec1, ny1, nx1, &dble_image2, &nx2, &ny2,
                         &dble_image3, &nx3, &ny3);

/*
strcpy(filename1, "lena512_dwt_sym.fits");
strcpy(comments1, "DWT 2d sym transform of lena512.fits");
*/
strcpy(filename1, "snow_dwt_sym.fits");
strcpy(comments1, "DWT 2d sym transform of snow.fits");
printf("Output dwt 2d sym image: nx2=%d ny2=%d\n", nx2, ny2);
JLP_WRFITS_2D_dble(dble_image2, nx2, ny2, nx2, filename1, comments1, errmess1);

/*
strcpy(filename1, "lena512_recon.fits");
strcpy(comments1, "Reconsruction from DWT 2d sym transform of lena512.fits");
*/
strcpy(filename1, "snow_recon.fits");
strcpy(comments1, "Reconsruction from DWT 2d sym transform of snow.fits");
printf("Output reconstructed image: nx3=%d ny3=%d\n", nx3, ny3);
JLP_WRFITS_2D_dble(dble_image3, nx3, ny3, nx3, filename1, comments1, errmess1);

return(0);
}
