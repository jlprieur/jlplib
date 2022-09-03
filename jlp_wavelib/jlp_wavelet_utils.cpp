/***********************************************************************
* jlp_wavelet_utils.cpp
*
* The Following Wavelets are in the Database:
* haar, db1, db2, db3, db4, db5, db6, db7, db8, db9, db10, 
* db11, db12, db13, db14, db15.
* bior1.1, bio1.3, bior1.5, bior2.2, bior2.4,bior2.6,bior2.8, 
* bior3.1, bior3.3, bior3.5, bior3.7, bior3.9, bior4.4,
* bior5.5, bior6.8.
* coif1, coif2, coif3, coif4, coif5.
*
* JLP
* Version 27/11/2020
***********************************************************************/

#include <stdlib.h> // exit()

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm> // sort()


#include "wavelet2s.h"
#include "jlp_fitsio.h"         // for JLP_RDFITS_2D

#include "jlp_wavelet_utils.h"

/************************************************************************
* Prototypes included here and defined in jlp_wavelet_utils.h:

void* maxval_2d(vector<vector<double> > &arr, double &max);
void* maxval_1d(vector<double> &arr, double &max);
int jlp_image_to_vector(double *dble_image1, int ny1, int nx1,
                        vector<vector<double> > &vec1);
int jlp_LoadFITSImage(char *filename1, double **dble_image1, 
                      int *nx1, int *ny1, char *comments1, char *errmess1);
int jlp_dwt_2d_transform(vector<vector<double> > &vec1, int ny1, int nx1,
                         double **dble_image2, int *nx2, int *ny2,
                         double **dble_image3, int *nx3, int *ny3);
int jlp_dwt_2d_subset(vector<double> &output0, vector<double> &dwt_coef1);
int jlp_dwt_2d_sym_transform(vector<vector<double> > &vec1, int ny1, int nx1,
                             double **dble_image2, int *nx2, int *ny2,
                             double **dble_image3, int *nx3, int *ny3)
int jlp_idwt_reconstructed( vector<double> output0, vector<double> flag0, 
                            vector<int> length0, string nm0, int ny1, int nx1,
                            double **dble_image3, int *nx3, int *ny3);
int jlp_swt_2d_transform(vector<vector<double> > &vec1, int ny1, int nx1,
                         double **dble_image2, int *nx2, int *ny2,
                         double **dble_image3, int *nx3, int *ny3);

**************************************************************************/

/*********************************************************************
*
***********************************************************************/
void findthresh(vector<double> &vector1, int N, double& t){
    sort(vector1.begin(), vector1.end(), greater<double>());
    t = vector1.at(N-1);
}
/******************************************************************
* Determining the maximum value of 2D vector
******************************************************************/
void* maxval_2d(vector<vector<double> > &arr, double &max){
    max = 0;
// width: n_cols=arr[0].size() = nx (i index here)
// height: n_rows=arr.size() = ny (j index here)
    for (unsigned int i =0; i < arr.size(); i++) {
        for (unsigned int j =0; j < arr[0].size(); j++) {
            if (max <= arr[i][j]){
                max = arr[i][j];
            }
        }
    }
    return 0;
}

/******************************************************************
* Determining the maximum value of 1D vector
******************************************************************/
void* maxval_1d(vector<double> &arr, double &max){
    max = 0;
    for (unsigned int i =0; i < arr.size(); i++) {
        if (max <= arr[i]){
            max = arr[i];
        }

    }
    return 0;
}

/******************************************************************
* Convert an image to a 2D vector 
*******************************************************************/
int jlp_image_to_vector(double *dble_image1, int nx1, int ny1,
                        vector<vector<double> > &vec1) 
{
int i, j, k;

// nrows=ny1 ncols=nx1
// vector<vector<double> > vec1(nrows, vector<double>(ncols));

 k = 0;
// width: n_cols=arr[0].size() = nx (i index here)
// height: n_rows=arr.size() = ny (j index here)
// Loop on j row index first:
 for(j = 0; j < ny1; j++){
     for(i = 0; i < nx1; i++) {
         vec1[j][i] = dble_image1[k];
         k++;
     }
   }

return(0);
}

/******************************************************************
* Load an image in a 2DFITS file or a single plane in 3D FITS cube
*
*******************************************************************/
int jlp_LoadFITSImage(char *filename1, double **dble_image1, 
                      int *nx1, int *ny1, char *comments1, char *errmess1)
{
int nz0;
int status, iframe = 1;

// dble_image1: array with the data contained in FITS file
// nx1, ny1, nz0: size of data cube
// iframe: index of image plane to be loaded from 1 to nz (in case of 3D data)

status = JLP_RDFITS_2D_dble(dble_image1, nx1, ny1, &nz0, iframe, filename1,
                             comments1, errmess1);
if (status) {
  fprintf(stderr, "Error loading image from %s (status=%d)\n %s\n",
          filename1, status, errmess1);
  return(-2);
  }

return(0);
}
/************************************************************************
* Discrete Wavelet Transform
*
************************************************************************/
int jlp_dwt_2d_transform(vector<vector<double> > &vec1, int ny1, int nx1,
                             double **dble_image2, int *nx2, int *ny2,
                             double **dble_image3, int *nx3, int *ny3)
{
// nm0: name of the wavelet family 
 string nm0 = "db2";
 vector<double> l1,h1,l2,h2;
 vector<int> length0, length2;
 int size_n, rows_n, cols_n;
 double max0, frac_coef0;
 vector<double> output0, flag0;
 vector<double> dwt_coef1, dwt_coef2;

/***
Wavelet Filters

15. Filters: int filtcoef(string nm, vector<double> &lpd, vector<double> &hpd, vector<double> &lpr, vector<double> &hpr)

nm: Wavelet name.
lpd: Low Pass Decomposition Filter Coefficients.
hpd: High Pass Decomposition Filter Coefficients.
lpr: Low Pass Reconstruction Filter Coefficients.
hpr: High Pass Reconstruction Filter Coefficients.

All filters are vector<double> objects and can be obtained by specifying the wavelet name. Currently, following Wavelets are available:

Daubechies : db1,db2,.., ,db15
Biorthogonal: bior1.1 ,bior1.3 ,bior1.5 ,bior2.2 ,bior2.4 ,bior2.6 ,bior2.8 ,bior3.1 ,bior3.3 ,bior3.5 ,bior3.7 ,bior3.9 ,bior4.4 ,bior5.5 ,bior6.8
Coiflets: coif1,coif2,coif3,coif4,coif5
Symmlets: sym2,........, sym10
*****/

/*
filtcoef returns the four lowpass and highpass, decomposition and reconstruction filters associated with the orthogonal or biorthogonal wavelet nm0.
*/
 filtcoef(nm0, l1, h1, l2, h2);

// Finding 2D DWT Transform of the image using symetric extension algorithm
// Extension is set to 0 (eg., int e = 0)

// J0: decomposition level
 int J0 = 6;

// dwt, discrete wavelet transform:
  dwt_2d(vec1, J0, nm0, output0, flag0, length0);

/* This algorithm computes DWT of image of any given size. 
* Together with convolution and subsampling operations it is clear 
* that subsampled images are of different length than dyadic length images. 
* In order to compute the "effective" size of DWT we do additional calculations.
*/
  dwt_output_dim_sym(length0, length2, J0);

/* length2 gives the integer vector that contains the size of subimages 
* that will combine to form the displayed output image. 
* The last two entries of length2 gives the size of DWT ( rows_n by cols_n)
*/
  size_n = length2.size();
  rows_n = length2[size_n - 2];
  cols_n = length2[size_n -1];
  printf("From length2: size_n=%d rows_n=%d cols_n=%d\n", size_n, rows_n, cols_n);

  vector<vector< double> > dwt_disp1(rows_n, vector<double>(cols_n));

/* dispDWT returns the 2D object dwt_disp1 which will be displayed as dwt_output0 
*/
  dispDWT(output0, dwt_disp1, length0, length2, J0);

/*** Not nice for display
 *nx2 = cols_n;
 *ny2 = rows_n;
 *dble_image2 = new double[cols_n * rows_n];
 for (int j = 0; j < (*ny2); j++){
    for(int i = 0; i < (*nx2); i++){
     (*dble_image2)[ i + j * (*nx2)] = dwt_disp1[j][i];
    }
 }
****/

// Storing the DWT coefficients in two different vectors that will be used to approximate
   vector<vector<double> >  dwt_output0 = dwt_disp1;

// Image with two different sets of chosen coefficients.
   dwt_coef1 = output0;
   dwt_coef2 = output0;

// Computing max value max0 of dwt_output0:
// max value max0 is needed to take care of overflow which happens because
// of convolution operations performed on unsigned 8 bit images
   maxval_2d(dwt_output0, max0);

// dwt_hold0 is created to hold the dwt output as further operations need to be
// carried out on dwt_output in order to display scaled images.
   vector<vector<double> > dwt_hold0(rows_n, vector<double>( cols_n));
   dwt_hold0 = dwt_output0;
 
// Displaying Scaled Image
// Setting coefficients of created image to the scaled DWT output values
   *nx2 = dwt_output0[0].size();
   *ny2 = dwt_output0.size();
printf("ny2=%d\n", *ny2);
printf("or=%d\n", dwt_output0.size());
   *dble_image2 = new double[cols_n * rows_n];
    for (int j = 0; j < (*ny2); j++ ){
        for (int i = 0; i < (*nx2); i++ ) {
            if ( dwt_output0[j][i] <= 0.0){
                dwt_output0[j][i] = 0.0;
            }
            if ( j <= (length2[0]) && i <= (length2[1]) ) {
                (*dble_image2)[i + j * (*nx2)] =
                        (dwt_output0[j][i] / max0) * 255.0;
            } else {
                (*dble_image2)[i + j * (*nx2)] = dwt_output0[j][i] ;
            }
        }
    }

// Case 1 : Only 10% of the largest coefficients are considered
 frac_coef0 = 0.10;
 jlp_dwt_2d_subset_largecoef(output0, dwt_coef1, flag0, length0, length2,
                             nm0, rows_n, cols_n, dble_image3, nx3, ny3, 
                             frac_coef0);

// Case 2 : Only 2% of the largest coefficients are considered
/****
 frac_coef0 = 0.02;
 jlp_dwt_2d_subset_largecoef(output0, dwt_coef1, flag0, length0, length2,
                             nm0, rows_n, cols_n, dble_image3, nx3, ny3, 
                             frac_coef0);
****/

return(0);
}
/************************************************************************
* Only a fraction of the largest coefficients are considered
*
* Case 1 : Only 10% of the largest coefficients are considered
* Case 2 : Only 2% of the largest coefficients are considered
*   frac_coef1: fraction of the coefficients to be used
*
*************************************************************************/
int jlp_dwt_2d_subset_largecoef(vector<double> &output0, 
                                vector<double> &dwt_coef1,
                                vector<double> flag0, vector<int> length0, 
                                vector<int> length2,
                                string nm0, int rows_n, int cols_n, 
                                double **dble_image3, int *nx3, int *ny3,
                                double frac_coef1)
{
   double temp;
   int n_coef1;

// Output is the 1D DWT vector
   n_coef1= int (output0.size() * frac_coef1);

   printf("jlp_dwt_2d_subset: n_coef1=%d\n", n_coef1);

// Finding Threshold Value corresponding to n_coef1

   vector<double> temp1;
// iostream
       printf("temp1: size: %d\n", temp1.size());
       printf("capacity: %d\n", (int)temp1.capacity());
       printf("max_size: %d\n", (int) temp1.max_size());
       for (unsigned int i =0; i < dwt_coef1.size(); i++) {
             double tempval = abs(dwt_coef1[i]);
// Add a new element at the end of the vector, after it current last element:
             temp1.push_back(tempval);

           }
       printf("temp1: new size: %d\n", temp1.size());
       printf("capacity: %d\n", (int)temp1.capacity());
       printf("max_size: %d\n", (int) temp1.max_size());

   double thresh1= 0.0;

// Determine the threshold of the coefficients
       findthresh(temp1, n_coef1, thresh1);
       printf("Output from findthresh thresh1 =: %f\n", thresh1);

// fstream
       ofstream temp_("temp.txt");
           for (unsigned int i =0; i < temp1.size(); i++){
                  temp_ << temp1[i] << " " ;
                }

// Reset coeffficients value depending on threshold value
           for (unsigned int i =0; i < dwt_coef1.size(); i++) {
                temp = abs(dwt_coef1[i]);

                if (temp < thresh1){
// if at != 0., the pixel is valid, so but putting it to zero, invalidate it:
                    dwt_coef1.at(i)= 0.0;
                }
            }

// Finding IDWT (inverse dwt) from this subset of coefficients dwt_coef1
  vector<vector<double> > idwt_output1(rows_n, vector<double>(cols_n));

     idwt_2d( dwt_coef1, flag0, nm0, idwt_output1, length0);

  double max1;
     maxval_2d(idwt_output1, max1);

  *nx3 = idwt_output1[0].size();
  *ny3 = idwt_output1.size();
  *dble_image3 = new double[(*nx3) * (*ny3)];
   for (int j = 0; j < *ny3; j++ ) {
      for (int i = 0; i < *nx3; i++ ){
          if ( idwt_output1[j][i] <= 0.0){
               idwt_output1[j][i] = 0.0;
           }
          if ( j <= (length2[0]) && i <= (length2[1]) ) {
              (*dble_image3)[i + j * (*nx3)] =
                      (idwt_output1[j][i] / max1) * 255.0;
          } else {
              (*dble_image3)[i + j * (*nx3)] = idwt_output1[j][i] ;
          }
      }
   }

return(0);
}
/************************************************************************
* Only the medium bandpass coefficients are considered
*
*************************************************************************/
int jlp_dwt_2d_medium_bandpass(vector<double> &output0, 
                                vector<double> &dwt_coef1,
                                vector<double> flag0, vector<int> length0, 
                                vector<int> length2,
                                string nm0, int rows_n, int cols_n, 
                                double **dble_image3, int *nx3, int *ny3)
{
   double temp;

   printf("jlp_dwt_2d_medium_bandpass\n");

   vector<double> temp1;
// iostream
       printf("temp1: size: %d\n", temp1.size());
       printf("capacity: %d\n", (int)temp1.capacity());
       printf("max_size: %d\n", (int) temp1.max_size());
       for (unsigned int i =0; i < dwt_coef1.size(); i++) {
             double tempval = abs(dwt_coef1[i]);
// Add a new element at the end of the vector, after it current last element:
             temp1.push_back(tempval);

           }
       printf("temp1: new size: %d\n", temp1.size());
       printf("capacity: %d\n", (int)temp1.capacity());
       printf("max_size: %d\n", (int) temp1.max_size());

// fstream
       ofstream temp_("temp.txt");
           for (unsigned int i =0; i < temp1.size(); i++){
                  temp_ << temp1[i] << " " ;
                }

/* length2 gives the integer vector that contains the size of subimages
* that will combine to form the displayed output image.
* The last two entries of length2 gives the size of DWT ( rows_n by cols_n)
*/
  isize_n = length2.size();
  irows_n = length2[size_n - 2];
  icols_n = length2[size_n -1];
  printf("From length2: size_n=%d rows_n=%d cols_n=%d\n", 
          isize_n, irows_n, icols_n);
  printf("From max_size of dwt_coeff: %d\n", dwt_coeff1.size() );

// Reset coeffficients value depending on threshold value
  int kk = 0;
  for (int jj = 0; jj < irows_n; jj++) {
     for (int ii = 0; ii < icols_n; ii++) {

      if((ii < isize_n) && (jj < isize_n) && (kk < dwt_coef1.size())) {
// if at != 0., the pixel is valid, so but putting it to zero, invalidate it:
                    dwt_coef1.at(kk)= 0.0;
       }
       kk++;
     }
   }

// Finding IDWT (inverse dwt) from this subset of coefficients dwt_coef1
  vector<vector<double> > idwt_output1(rows_n, vector<double>(cols_n));

     idwt_2d( dwt_coef1, flag0, nm0, idwt_output1, length0);

   double max1;
     maxval_2d(idwt_output1, max1);

  *nx3 = idwt_output1[0].size();
  *ny3 = idwt_output1.size();
  *dble_image3 = new double[(*nx3) * (*ny3)];
   for (int j = 0; j < *ny3; j++ ) {
      for (int i = 0; i < *nx3; i++ ){
          if ( idwt_output1[j][i] <= 0.0){
               idwt_output1[j][i] = 0.0;
           }
          if ( j <= (length2[0]) && i <= (length2[1]) ) {
              (*dble_image3)[i + j * (*nx3)] =
                      (idwt_output1[j][i] / max1) * 255.0;
          } else {
              (*dble_image3)[i + j * (*nx3)] = idwt_output1[j][i] ;
          }
      }
   }

return(0);
}

/************************************************************************
* Discrete Wavelet Transform
*
************************************************************************/
int jlp_dwt_2d_sym_transform(vector<vector<double> > &vec1, int ny1, int nx1,
                             double **dble_image2, int *nx2, int *ny2,
                             double **dble_image3, int *nx3, int *ny3)
{
// nm0: name of the wavelet family 
 string nm0 = "db3";
 vector<double> l1,h1,l2,h2;
 vector<int> length2;
 int size_n, rows_n, cols_n;

// Finding 2D DWT Transform of the image using symetric extension algorithm
// Extension is set to 3 (eg., int e = 3)

 vector<int> length0;
 vector<double> output0, flag0;
// J0: decomposition level
 int J0 = 3;

 filtcoef(nm0, l1, h1, l2, h2);

// dwt, discrete wavelet transform:
  dwt_2d_sym(vec1, J0, nm0, output0, flag0, length0);

/* This algorithm computes DWT of image of any given size. 
* Together with convolution and subsampling operations it is clear 
* that subsampled images are of different length than dyadic length images. 
* In order to compute the "effective" size of DWT we do additional calculations.
*/
  dwt_output_dim_sym(length0, length2, J0);

/* length2 is gives the integer vector that contains the size of subimages 
* that will combine to form the displayed output image. 
* The last two entries of length2 gives the size of DWT ( rows_n by cols_n)
*/
  size_n = length2.size();
  rows_n=length2[size_n - 2];
  cols_n = length2[size_n -1];

  vector<vector< double> > dwt_disp1(rows_n, vector<double>(cols_n));

/* The dispDWT function returns the 2D object dwt_disp1 which will be displayed 
* using OPENCV's image handling functions
*/
  dispDWT(output0, dwt_disp1, length0 ,length2, J0);

 *nx2 = dwt_disp1[0].size();
 *ny2 = dwt_disp1.size();
 *dble_image2 = new double[(*nx2) * (*ny2)];
 for (int j = 0; j < (*ny2); j++){
    for(int i = 0; i < (*nx2); i++){
     (*dble_image2)[ i + j * (*nx2)] = dwt_disp1[j][i];
    }
 }

jlp_idwt_reconstructed(output0, flag0, length0, nm0, nx1, ny1,
                       dble_image3, nx3, ny3);

return(0);
}

/*************************************************************************
* Inverse discrete wavelength transform
*
**************************************************************************/
int jlp_idwt_reconstructed( vector<double> output0, vector<double> flag0, 
                            vector<int> length0, string nm0, int nx1, int ny1,
                            double **dble_image3, int *nx3, int *ny3)
{

// Finding IDWT
// nrows=ny1, ncols=nx1
vector<vector<double> > idwt_output3(ny1, vector<double>(nx1));

 idwt_2d_sym(output0, flag0, nm0, idwt_output3, length0);
 *nx3 = idwt_output3[0].size();
 *ny3 = idwt_output3.size();
  printf(" nx3=%d ny3=%d\n", *nx3, *ny3);
 *dble_image3 = new double[(*nx3) * (*ny3)];
 for (int j = 0; j < (*ny3); j++){
    for(int i = 0; i < (*nx3); i++){
     (*dble_image3)[ i + j * (*nx3)] = idwt_output3[j][i];
    }
 }

return(0);
}
/***********************************************************************
* Static Wavelet Transform: swt
*************************************************************************/
int jlp_swt_2d_transform(vector<vector<double> > &vec1, int ny1, int nx1,
                         double **dble_image2, int *nx2, int *ny2,
                         double **dble_image3, int *nx3, int *ny3)
{
// nm0: name of the wavelet family
 string nm0 = "db2";
 int rows_n, cols_n;
 double max_blur, temp0;
 vector<double> output0;

// Finding 2D DWT Transform of the image using symetric extension algorithm
// Extension is set to 0 (eg., int e = 0)

// J0: decomposition level
 int J0 = 3;

  printf("intput size: (ny1=%d)*(nx1=%d)=%d \n", ny1, nx1, ny1*nx1); 
// swt, static wavelet transform:
  swt_2d(vec1, J0, nm0, output0);
  printf("Output from swt_2d: output0.size=%d\n", output0.size()); 
  printf(" Loop OK \n"); 

/* This algorithm computes SWT of image of any given size.
* Together with convolution and subsampling operations it is clear
* that subsampled images are of different length than dyadic length images.
* In order to compute the "effective" size of SWT we do additional calculations.
*/
// Output values: rows_n, cols_n
  dwt_output_dim(vec1, rows_n, cols_n);

// Computing Low Pass Image
// Extract and Display Low Pass Image at the Jth stage
 vector<vector<double> > blur(rows_n, vector<double>(cols_n));

   for (int j=0; j < rows_n; j++){
           for (int i=0; i < cols_n; i++){
            blur[j][i] = output0[i + j*cols_n];
           }
   }

// Compute max of blur array:
  maxval_2d(blur, max_blur);

// Displaying Low Pass Image
// Setting coefficients of created image to the scaled DWT output values
   *nx2 = cols_n;
   *ny2 = rows_n;
   printf("Low Pass Image: nx2=%d ny2=%d\n", *nx2, *ny2);

   *dble_image2 = new double[(*nx2) * (*ny2)];
    for (int j = 0; j < (*ny2); j++ ){
        for (int i = 0; i < (*nx2); i++ ) {
          if ( blur[j][i] <= 0.0){
                blur[j][i] = 0.0;
          }
          (*dble_image2)[i + j * (*nx2)] = (blur[j][i] / max_blur) * 255.0;
        }
    }

// Displaying BandPass Images

    vector<vector<double> >  detail(3 * rows_n, vector<double>(J0 * cols_n));

// Computing BandPass Images
    for (int k=0; k < J0; k++) {
    for (int j=0; j < 3*rows_n; j++) {
        for(int i=0+ k*cols_n; i < (k+1)*cols_n; i++) {
          temp0 = output0[(3*k+1)*rows_n*cols_n + j * cols_n + i - k*cols_n];
          detail[j][i]= temp0;
        }
    }
    }

   *nx3 = J0 * cols_n;
   *ny3 = J0 * rows_n;
   printf("Band Pass Images: nx3=%d ny3=%d\n", *nx3, *ny3);

   *dble_image3 = new double[(*nx3) * (*ny3)];
   for (int j = 0; j < *ny3; j++ ) {
      for (int i = 0; i < *nx3; i++ ){
             if ( detail[j][i] <= 0.0){
                     detail[j][i] = 0.0;
             }
          (*dble_image3)[i + j * (*nx3)] = detail[j][i];
          }
       }

return(0);
}
