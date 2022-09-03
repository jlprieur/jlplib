/*************************************************************************
* jlp_wavelet_filter1.cpp
*
* JLP
* Version 15/01/2021
*************************************************************************/

#include <stdlib.h> // exit()
#include <vector>
#include <string>

//#include "wavelet2s.h"
#include "jlp_fitsio.h"         // for JLP_RDFITS_2D

#include "jlp_wavelet_utils.h"

/***********************************************************************
*
* This code decomposes a 512X512 grayscale image to 6 levels 
* and uses a) only 10% coefficients and b) only 2% coefficients to reconstruct the image.
*************************************************************************/
int main(int argc, char* argv[]) {
char filename1[120], comments1[80], errmess1[200];
double *dble_image1, *dble_image2, *dble_image3;
int nx1, ny1, nx2, ny2, nx3, ny3;

// strcpy(filename1, "lena512.fits");
 strcpy(filename1, "snow.fits");
jlp_LoadFITSImage(filename1, &dble_image1, &nx1, &ny1, comments1, errmess1);

vector<vector<double> > vec1(ny1, vector<double>(nx1));
jlp_image_to_vector(dble_image1, nx1, ny1, vec1);

jlp_dwt_2d_transform(vec1, ny1, nx1, &dble_image2, &nx2, &ny2,
                     &dble_image3, &nx3, &ny3);

/*
strcpy(filename1, "lena512_dwt_scaled.fits");
strcpy(comments1, "Scaled DWT 2d transform of lena512.fits");
*/
strcpy(filename1, "snow_dwt_scaled.fits");
strcpy(comments1, "Scaled DWT 2d transform of snow.fits");
printf("Output scaled dwt image: nx2=%d ny2=%d\n", nx2, ny2);
JLP_WRFITS_2D_dble(dble_image2, nx2, ny2, nx2, filename1, comments1, errmess1);

/*
strcpy(filename1, "lena512_dwt_subset.fits");
strcpy(comments1, "Subset of coeff, 2d IDWT transform of lena512.fits");
*/
strcpy(filename1, "snow_dwt_subset.fits");
strcpy(comments1, "Subset of coeff, 2d IDWT transform of snow.fits");
printf("Output subset idwt image: nx3=%d ny3=%d\n", nx3, ny3);
JLP_WRFITS_2D_dble(dble_image3, nx3, ny3, nx3, filename1, comments1, errmess1);

return(0);
}
