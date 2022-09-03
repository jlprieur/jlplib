//============================================================================
// Name        : jlp_swt2Ddemo.cpp
// From swt2Ddemo.cpp by Rafat Hussain
// Author      : JLP
// Version     :  05/12/2020
// Copyright   : 
// Description : 2D SWT Demo 
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
int jlp_swt_2d_transform(vector<vector<double> > &vec1, int nx1, int ny1,
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

if(argc == 2) {
strcpy(filename1, argv[1]);
} else {
strcpy(filename1, "cameraman.fits");
}
jlp_LoadFITSImage(filename1, &dble_image1, &nx1, &ny1, comments1, errmess1);

vector<vector<double> > vec1(ny1, vector<double>(nx1));
jlp_image_to_vector(dble_image1, nx1, ny1, vec1);

jlp_swt_2d_transform(vec1, ny1, nx1, &dble_image2, &nx2, &ny2,
                     &dble_image3, &nx3, &ny3);

// Low Pass Image
strcpy(filename1, "cameraman_low_pass.fits");
strcpy(comments1, "Low Pass Image from static wavelet transform of cameraman.fits");
printf("Output low pass image: nx2=%d ny2=%d\n", nx2, ny2);
JLP_WRFITS_2D_dble(dble_image2, nx2, ny2, nx2, filename1, comments1, errmess1);

strcpy(filename1, "cameraman_band_pass.fits");
strcpy(comments1, "BandPass Images from static wavelet transform of cameraman.fits");
printf("Output bandpass set of images: nx3=%d ny3=%d\n", nx3, ny3);
JLP_WRFITS_2D_dble(dble_image3, nx3, ny3, nx3, filename1, comments1, errmess1);

return(0);
}
