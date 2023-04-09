/******************************************************************************
* Name:        jlp_wx_ipanel_process.cpp
*              JLP_wxImagePanel class
* Purpose:     Processing speckle images
*
* Author:      JLP
* Version:     06/03/2020
******************************************************************************/

#include "jlp_wx_ipanel.h"
#include "jlp_wx_patch_dlg.h"
#include "jlp_wx_ipanel_filters.h"   // speckle_subtract_model(),
                                   // UnsharpMaskingFilter(),...
#include "jlp_wx_ipanel_utils.h"     // gdp_offset_correction(),...
#include "jlp_wx_loadimage1.h"     // JLP_wxLoadFITSImage()

// JLP2017: put it here to avoid problems with TBYTE and mingw (mswindows)
#include "jlp_fitsio.h"       // JLP_RDFITS_2D_dble
#include "jlp_numeric.h"      // RECENT_FFT, rotate_set, jlp_sort_array ...

/* Contains:
  int FilterUnsharp(int box_width);
  int FilterHighContrast(int filter_option);
  int FilterVeryHighContrast();
  int BinaryMeasFromCircProfile(const int n_for_patches, 
                                double *xc, double *yc, double *radius,
                                double *rho10, double *theta10,
                                double *error_rho10, double *error_theta10);
  int SpeckleModelSubtraction();
  int OffsetCorrection();
  int FlatFieldCorrection();
  int LoadUnresolvedAutoc();
  int ComputeUnresolvedModsq();
  int AutoMeasureAutocBinary();
  int AutoMeasureAutocBinaryWithParam(double angle0,
                                   double *rho10, double *theta10,
                                   double *error_rho10, double *error_theta10);
  int AutoMeasureShackHartmann(double *pupil_mask0,
                               int nx0, int ny0, int x_half_width,
                               int y_half_width, double sigma_threshold,
                               double *peak_x, double *peak_y,
                               int *npeaks, int npeaks_maxi);
*/

static int LookForPeaks(double *in_image0, double *pupil_mask0,
                        int nx0, int ny0, int x_half_width,
                        int y_half_width, double sigma_threshold,
                        double *peak_x, double *peak_y,
                        int *npeaks, int npeaks_maxi);
static bool Is_a_peak(double *in_image0, int nx0, int ny0, int x_half_width,
                      int y_half_width, double sigma_threshold, int ix, int iy);

/**************************************************************************
* To be used after a change of the LUT/ITT settings
**************************************************************************/
void JLP_wxImagePanel::wxIP_UpdateImageDisplay(){
bool reset_ITT_to_MinMax;
// Update image values and refresh display:
 reset_ITT_to_MinMax = false;
 Image1_wxgdev->UpdateCImage1Values(dble_image1, nx1, ny1, reset_ITT_to_MinMax);
return;
}
/**************************************************************************
* 0=NONE
* 1=soft unsharp (UNSH1) 2=medium unsharp (UNSH2) 3=hard unsharp (UNSH3)
* 4=hard unsharp (UNSH4)
* 5=high contrast1 (VHC1) 6=high contrast2 (VHC2), 7=high contrast3 (VHC3)
* 8=very high contrast4 (VHC4) 
**************************************************************************/
void JLP_wxImagePanel::ApplyFilter(int filter0)
{
int hc_filter_option;
bool reset_ITT_to_MinMax = true;

switch(filter0) {
// Filter 0=FilterNone (i.e. go back to original image)
  default:
  case 0:
    RestoreOriginalImage();
// Update image values and refresh display:
    reset_ITT_to_MinMax = true;
    Image1_wxgdev->UpdateCImage1Values(dble_image1, nx1, ny1, reset_ITT_to_MinMax);
    break;
// Filter 1=Unsharp1: soft unsharp masking with a large square box
// Setting cell half_width to 10
  case 1:
    FilterUnsharp(10);
    break;
// Filter 2=Unsharp2: medium unsharp masking with a medium sized square box
// Setting cell half_width to 6
  case 2:
    FilterUnsharp(6);
    break;
// Filter 3=Unsharp3: unsharp masking with a small square box
// Setting cell half_width to 4
  case 3:
    FilterUnsharp(4);
    break;
// Filter 4=Unsharp4: unsharp masking with a very small square box
// Setting cell half_width to 3
  case 4:
    FilterUnsharp(3);
    break;
/***
* FilterHighContrast filter_option:
* 1 = HC1: Wiener deconvolution version 2008 with unresolved modsq
* 2 = HC2: median profile filter: version 2008 with unresolved modsq
* For popup menu (without unresolved modsq):
* 3 = VHC1: high contrast version 2008 without unresolved modsq
* 4 = VHC2: high contrast version 2015 without unresolved modsq
* 5 = VHC3: median profile version 2008 without unresolved modsq
* 6 = VHC4: median profile version 2015 without unresolved modsq
****/
// Filter 5, 6, 7, 8 = FilterHighContrast 1, 2, 3, 4, 5, 6
// strioscopic filter (with high contrast)
  case 5:
  case 6:
  case 7:
  case 8:
    hc_filter_option = filter0 - 2;
    FilterHighContrast(hc_filter_option);
    break;
}

return;
}
/*********************************************************************
* Filter: unsharp masking
**********************************************************************/
int JLP_wxImagePanel::FilterUnsharp(int box_width)
{
double *tmp;
wxString str1;
register int i;
bool reset_ITT_to_MinMax;

// Restore original image (to prevent piling up the filters):
 RestoreOriginalImage();

// Unsharp masking (impossible to have the same input/output array pointers):
 tmp = new double[nx1 * ny1];
 UnsharpMaskingFilter(dble_image1, tmp, nx1, ny1, box_width);

// Copy output to canvas image pointer:
 for(i = 0; i < nx1 * ny1; i++) dble_image1[i] = tmp[i];

// Update image values and refresh display:
 reset_ITT_to_MinMax = true;
 Image1_wxgdev->UpdateCImage1Values(dble_image1, nx1, ny1, reset_ITT_to_MinMax);

// Write to logbook window (and record on the logbook file):
 str1.Printf(wxT("%% Unsharp masking with box=%d \n"), box_width);
 if(jlp_logbook != NULL) jlp_logbook->WriteToLogbook(str1, true);

 delete[] tmp;
return(0);
}
/*********************************************************************
* Filter: high contrast
*
* INPUT:
* filter_option:
* 1 = HC1: Wiener deconvolution version 2008 with unresolved modsq
* 2 = HC2: median profile filter: version 2008 with unresolved modsq
* For popup menu (without unresolved modsq):
* 3 = VHC1: high contrast version 2008 without unresolved modsq
* 4 = VHC2: high contrast version 2015 without unresolved modsq
* 5 = VHC3: median profile version 2008 without unresolved modsq
* 6 = VHC4: median profile version 2015 without unresolved modsq
*
**********************************************************************/
int JLP_wxImagePanel::FilterHighContrast(int filter_option)
{
double *modsq_centered;
double *tmp_r, *tmp_i;
register int i;
wxString str1;
bool reset_ITT_to_MinMax;
bool version_2008;
int status;

// Restore original image (to prevent piling up the filters):
 RestoreOriginalImage();

// Need unresolved modsq frame for options 1 and 2
// prompt for it if not loaded yet
 if(filter_option == 1 || filter_option == 2) {
  if(UnresolvedModsq == NULL) {
  status = ComputeUnresolvedModsq();
  if(status) return(-1);
  }
 }

 tmp_r = new double[nx1 * ny1];
 tmp_i = new double[nx1 * ny1];

// Copy (and recenter) data to temporary array for further FFT:
 RECENT_FFT_DOUBLE(dble_image1, tmp_r, &nx1, &ny1, &nx1);
 for(i = 0; i < nx1 * ny1; i++) tmp_i[i] = 0.;

// modsq is the FFT -1 of autocorrelation:
// JLP99: warning: NY,NX!!!!
 fftw_2D_double(tmp_r, tmp_i, ny1, nx1, -1);

// Shift zero frequency from the lower left corner to the center:
 RECENT_FFT_DOUBLE(tmp_r, tmp_r, &nx1, &ny1, &nx1);

 modsq_centered = tmp_r;
/***
* FilterHighContrast filter_option:
* 1 = HC1: Wiener deconvolution version 2008 with unresolved modsq
* 2 = HC2: median profile filter: version 2008 with unresolved modsq
* For popup menu (without unresolved modsq):
* 3 = VHC1: high contrast version 2008 without unresolved modsq
* 4 = VHC2: high contrast version 2015 without unresolved modsq
* 5 = VHC3: median profile version 2008 without unresolved modsq
* 6 = VHC4: median profile version 2015 without unresolved modsq
****/
 switch(filter_option) {
  case 1:
    HighContrastFilter1(modsq_centered, dble_image1, UnresolvedModsq,
                        SigmaUnresolvedModsq, nx1, ny1);
    str1 = wxT("%% High contrast filter 2015-modsq (HC1): Wiener deconvolution\n");
    break;
  case 2:
    version_2008 = true;
    MedianProfileFilter(modsq_centered, dble_image1, UnresolvedModsq,
                        SigmaUnresolvedModsq, version_2008, nx1, ny1);
    str1 = wxT("%% High contrast median filter 2008-modsq (HC2)\n");
    break;
  case 3:
    HighContrastFilter1(modsq_centered, dble_image1,
                        NULL, 0., nx1, ny1);
    str1 = wxT("%% Very high contrast filter 2008 (VHC1)\n");
    break;
  case 4:
    HighContrastFilter2(modsq_centered, dble_image1, nx1, ny1);
    str1 = wxT("%% Very high contrast filter 2015 (VHC2)\n");
    break;
  case 5:
    version_2008 = true;
    MedianProfileFilter(modsq_centered, dble_image1, NULL, 0.,
                        version_2008, nx1, ny1);
    str1 = wxT("%% Very high contrast median filter 2008 (VHC3)\n");
    break;
  case 6:
    version_2008 = false;
    MedianProfileFilter(modsq_centered, dble_image1, NULL, 0.,
                        version_2008, nx1, ny1);
    str1 = wxT("%% Very high contrast median filter 2015 (VHC4)\n");
    break;
 }

// Update image values
  reset_ITT_to_MinMax = true;
  Image1_wxgdev->UpdateCImage1Values(dble_image1, nx1, ny1,
                                  reset_ITT_to_MinMax);

// Write to logbook window (and record on the logbook file):
  if(jlp_logbook != NULL) jlp_logbook->WriteToLogbook(str1, true);

delete[] tmp_r;
delete[] tmp_i;
return(0);
}
/*********************************************************************
* Speckle processing: subtraction of a model
* Useful to measure very close binaries
**********************************************************************/
int JLP_wxImagePanel::SpeckleModelSubtraction()
{
int status;
bool reset_ITT_to_MinMax;
wxString str1;

// Load unresolved autocorrelation if needed:
 if(UnresolvedAutoc == NULL) {
  status = LoadUnresolvedAutoc();
  if(status) return(-1);
 }

// Subtract model:
// in "jlp_wx_ipanel_filters.cpp"
// int subtract_back_model(double *autoc_data, double *back_model,
//                        double *autoc_flattened, int nx, int ny, int idim);
//
 if(!subtract_back_model(dble_image1, UnresolvedAutoc, dble_image1,
                            nx1, ny1, nx1)) {
// Update image values and refresh display:
   reset_ITT_to_MinMax = true;
   Image1_wxgdev->UpdateCImage1Values(dble_image1, nx1, ny1, reset_ITT_to_MinMax);
   str1 = wxT("%% Model subtracted: ") + UnresolvedAutocFilename + wxT("\n");
// Write to logbook window (and record on the logbook file):
   if(jlp_logbook != NULL) jlp_logbook->WriteToLogbook(str1, true);
   status = 0;
 } else {
   status = -1;
 }

return(status);
}
/*********************************************************************
* Load the autocorrelation of an unresolved star
* (needed for subtracting a model and very high contrast filtering)
**********************************************************************/
int JLP_wxImagePanel::LoadUnresolvedAutoc()
{
int inx, iny, inz;
wxString str1, filename, exec_directory, default_path, default_name;
wxString err_message;
char err_mess[128];

// strcpy(back_model_fname, "/d/execlnx/unres_d.fits");

 if(UnresolvedAutoc == NULL) {
   delete[] UnresolvedAutoc;
   UnresolvedAutoc = NULL;
 }

// Prompt the user for the FITS filename:
  default_name = _T("unres_d.fits");
  exec_directory = wxString(_T("EXEC"));
  if(!(wxGetEnv(exec_directory, &default_path))){
    default_path = _T("");
  }

  filename = wxFileSelector(_T("Select autoc. of unresolved star"),
                            default_path, default_name, _T("fits|fit"),
                            _T("FITS files (*.fits;*.fit)|*.fits;*.fit"));

  if (filename.IsEmpty() ) return(-1);

// Load a new image in FITS format
 if(JLP_wxLoadFITSImage(filename, &UnresolvedAutoc, &inx, &iny, &inz, 0) != 0) {
   err_message = _T("LoadUnresolvedAutoc/Error opening input image: ")
                 + filename;
   wxLogError(err_message);
   return(-1);
 }
 if(nx1 != inx || ny1 != iny) {
   sprintf(err_mess, "LoadUnresolvedAutoc/Error inconsistent size of \n\
nx1=%d ny1=%d (whereas inx=%d iny=%d)", nx1, ny1, inx, iny);
   delete[] UnresolvedAutoc;
   UnresolvedAutoc = NULL;
   wxLogError(wxString(err_mess, wxConvUTF8));
   return(-1);
 }

// Now update filename:
  UnresolvedAutocFilename = filename;

return(0);
}

/*********************************************************************
* Select and use offset file to make flat field correction
**********************************************************************/
int JLP_wxImagePanel::OffsetCorrection(int positive)
{
int status, nx, ny, nz, iplane = 1;
char offset_fname[100], err_messg[120], comments[80];
double *offset_array;
wxString str1, str2, full_filename, exec_directory, default_path, default_name;
wxString filename, path, extension;
bool reset_ITT_to_MinMax;

// Prompt the user for the FITS filename:
  default_name = _T("");
  default_path = _T("");

  full_filename = wxFileSelector(_T("Select offset file"), default_path,
                            default_name, _T("fits|fit"),
                            _T("FITS files (*.fits;*.fit)|*.fits;*.fit"));

  if ( !full_filename ) return(-1);

strncpy(offset_fname, full_filename.mb_str(), 100);

// Read offset image:
 status = JLP_RDFITS_2D_dble(&offset_array, &nx, &ny, &nz,
                       iplane, offset_fname, comments, err_messg);
 if(nx != nx1 || ny != ny1 || nz > 1) {
  str1.Printf("OffsetCorrection/Error, wrong offset size: nx=%d ny=%d nz=%d\n",
  nx, ny, nz);
  wxLogError(wxString(err_messg, wxConvUTF8));
  return(-1);
 }

// Offset correction
 if(!gdp_offset_correction(dble_image1, offset_array, nx1, ny1, positive)){
// Update image values and refresh display:
   reset_ITT_to_MinMax = true;
   Image1_wxgdev->UpdateCImage1Values(dble_image1, nx1, ny1, reset_ITT_to_MinMax);
// Removes the directory name (since the full path is generally too long...)
   wxFileName::SplitPath(full_filename, &path, &filename, &extension);
   str2.Printf("%% Offset correction (positive=%d) with: ", positive);
   str1 = str2 + filename + wxT(".") + extension + wxT("\n");
// Write to logbook window (and record on the logbook file):
   if(jlp_logbook != NULL) jlp_logbook->WriteToLogbook(str1, true);
   status = 0;
 } else {
   wxLogError(wxString(err_messg, wxConvUTF8));
   status = -1;
 }

return(status);
}
/*********************************************************************
* Select and use flat field file to make flat field correction
**********************************************************************/
int JLP_wxImagePanel::FlatFieldCorrection(int sigma_level)
{
int status, nx, ny, nz, iplane = 1;
char ffield_fname[100], err_messg[120], comments[80];
double *ffield_array;
wxString str1, str2, full_filename, exec_directory, default_path, default_name;
wxString filename, path, extension;
bool reset_ITT_to_MinMax;

// Prompt the user for the FITS filename:
  default_name = _T("");
  default_path = _T("");

  full_filename = wxFileSelector(_T("Select flat field file"), default_path,
                            default_name, _T("fits|fit"),
                            _T("FITS files (*.fits;*.fit)|*.fits;*.fit"));

  if ( !full_filename ) return(-1);

strncpy(ffield_fname, full_filename.mb_str(), 100);

// Read offset image:
 status = JLP_RDFITS_2D_dble(&ffield_array, &nx, &ny, &nz,
                       iplane, ffield_fname, comments, err_messg);
 if(nx != nx1 || ny != ny1 || nz > 1) {
  str1.Printf("FlatFieldCorrection/Error, wrong flat field size: nx=%d ny=%d nz=%d\n",
  nx, ny, nz);
  wxLogError(wxString(err_messg, wxConvUTF8));
  return(-1);
 }

// Offset correction
 if(!gdp_ffield_correction(dble_image1, ffield_array, nx1, ny1, sigma_level)){
// Update image values and refresh display:
   reset_ITT_to_MinMax = true;
   Image1_wxgdev->UpdateCImage1Values(dble_image1, nx1, ny1, reset_ITT_to_MinMax);
// Removes the directory name (since the full path is generally too long...)
   wxFileName::SplitPath(full_filename, &path, &filename, &extension);
   str2.Printf("%% FlatField correction (sigma=%d) with: ", sigma_level);
   str1 = str2 + filename + wxT(".") + extension + wxT("\n");
// Write to logbook window (and record on the logbook file):
   if(jlp_logbook != NULL) jlp_logbook->WriteToLogbook(str1, true);
   status = 0;
 } else {
   wxLogError(wxString(err_messg, wxConvUTF8));
   status = -1;
 }

return(status);
}
/*************************************************************************
* Compute the unresolved modsq
* ( i.e., the FFT of an unresolved autocorrelation)
* and store it into UnresolvedModsq[]
* (needed for very high contrast filter)
**************************************************************************/
int JLP_wxImagePanel::ComputeUnresolvedModsq()
{
double *tmp_r, *tmp_i;
int status;
double ww, ww1, ww2;
int nn, imax, jmax;
register int i, j;

// Load unresolved autocorrelation if needed:
 if(UnresolvedAutoc == NULL) {
  status = LoadUnresolvedAutoc();
  if(status != 0) return(-1);
 }

// Erase previous array
// (necessary if the size of the displayed file has changed):
 if(UnresolvedModsq != NULL) {
   delete[] UnresolvedModsq;
 }
 UnresolvedModsq = new double[nx1 * ny1];

//***************************************
// Compute the square modulus (modsq)

// Allocate temporary arrays for FFT:
 tmp_r = new double[nx1 * ny1];
 tmp_i = new double[nx1 * ny1];

// Copy (and recenter) data to temporary array for further FFT:
RECENT_FFT_DOUBLE(UnresolvedAutoc, tmp_r, &nx1, &ny1, &nx1);
 for(i = 0; i < nx1 * ny1; i++) tmp_i[i] = UnresolvedAutoc[i];
 for(i = 0; i < nx1 * ny1; i++) tmp_i[i] = 0.;

// modsq is the FFT -1 of autocorrelation:
// JLP99: warning: NY,NX!!!!
 fftw_2D_double(tmp_r, tmp_i, ny1, nx1, -1);

// Shift zero frequency from the lower left corner to the center:
RECENT_FFT_DOUBLE(tmp_r, UnresolvedModsq, &nx1, &ny1, &nx1);

delete[] tmp_r;
delete[] tmp_i;

//***************************************
// Compute sigma in the lower left corner:
nn = 0;

jmax = MAXI(8, ny1/16);
imax = MAXI(8, nx1/16);
ww1 = 0.;
ww2 = 0.;
nn = 0;
for(j = 0; j < jmax; j++) {
 for(i = 0; i < imax; i++) {
   ww = UnresolvedModsq[i + j * nx1];
   ww1 += ww;
   ww2 += ww * ww;
   nn++;
   }
 }
if(nn > 3) {
  SigmaUnresolvedModsq = ww2/(double)nn - SQUARE(ww1/(double)nn);
  SigmaUnresolvedModsq = MAXI(SigmaUnresolvedModsq, 1.e-9);
  SigmaUnresolvedModsq = sqrt(SigmaUnresolvedModsq);
  }

return(0);
}
/*************************************************************************
* Automatic measurement of phase wavefront (Shack-Hartmann sensor)
*
* INPUT:
* x_half_width, y_half_width : half size of the box around the pixel to
*                              be examined
* peak_x, peak_y : x,y coordinates of the peaks
* npeaks : number of peaks
* npeaks_max : size of peak_x and peak_y
**************************************************************************/
int JLP_wxImagePanel::AutoMeasureShackHartmann(double *pupil_mask0,
                                      int nx0, int ny0, int x_half_width,
                                      int y_half_width, double sigma_threshold,
                                      double *peak_x, double *peak_y,
                                      int *npeaks, int npeaks_maxi)
{
int status;

if((nx0 != nx1) || (ny0 != ny1)){
 fprintf(stderr, "AutoMeasureShackHartmann/Error size of pupil mask is not compatible with the displayed image !\n");
 return(-1);
 }

status = LookForPeaks(dble_image1, pupil_mask0,
                      nx1, ny1, x_half_width, y_half_width,
                      sigma_threshold, peak_x, peak_y, npeaks, npeaks_maxi);

// Call wxIP_UpdateImageDisplay (necessary to see the new image)
if(status == 0) wxIP_UpdateImageDisplay();

return(status);
}
/*************************************************************************
* Automatic determination of the sky level
* by measuring the mean value on the four edges of the image
**************************************************************************/
static int LookForPeaks(double *in_image0, double *pupil_mask0,
                        int nx0, int ny0, int x_half_width,
                        int y_half_width, double sigma_threshold,
                        double *peak_x, double *peak_y,
                        int *npeaks, int npeaks_maxi)
{
int i, j;
double *tmp_image;

*npeaks = 0;
tmp_image = new double[nx0 * ny0];
for(i = 0; i < nx0 * ny0; i++) tmp_image[i] = 0.;

// Scan the intersection of the image with the pupil mask:
 for(j = 0; j < ny0; j++) {
   for(i = 0; i < nx0; i++) {
// Check if pixel is inside the pupil:
     if(pupil_mask0[i + j * nx0] > 0.) {
       if(Is_a_peak(in_image0, nx0, ny0, x_half_width, y_half_width,
                    sigma_threshold, i, j)){
         if(*npeaks < npeaks_maxi) {
           peak_x[*npeaks] = i;
           peak_y[*npeaks] = j;
           (*npeaks)++;
// Put 1 on each peak for display
           tmp_image[i + j * nx0] = 1.;
           } else {
            fprintf(stderr, "LookForPeaks/Warning: %d peaks found (> maxi=%d)\n",
                    *npeaks, npeaks_maxi);
            return(0);
           }
         }
// DEBUG to see the pupil mask:
//           tmp_image[i + j * nx0] = 1.;
       } // if pupil_mask[i + j * nx0] > 0.
     }
   }

// Update image for display
for(i = 0; i < nx0 * ny0; i++) in_image0[i] = tmp_image[i];
delete[] tmp_image;

return(0);
}
/************************************************************************
* Check if (ix, iy) is the location of a peak
*************************************************************************/
static bool Is_a_peak(double *in_image1, int nx1, int ny1, int x_half_width,
                     int y_half_width, double sigma_threshold, int ix, int iy)
{
double sum, sumsq, mean[4], sigma[4], w_mean, w_sigma, value;
int npts, istart[4], iend[4], jstart[4], jend[4];
int i, j, k, m, k_mean_mini, k_sigma_mini, ix_min, ix_max, iy_min, iy_max;
bool is_a_peak;

ix_min = MAXI(0, ix - x_half_width);
ix_max = MINI(nx1 - 1, ix + x_half_width);
iy_min = MAXI(0, iy - y_half_width);
iy_max = MINI(ny1 - 1, iy + y_half_width);

istart[0] = ix_min;
iend[0] = ix_min;
jstart[0] = iy_min ;
jend[0] = iy_max;

istart[1] = ix_max;
iend[1] = ix_max;
jstart[1] = iy_min;
jend[1] = iy_max;

istart[2] = ix_min;
iend[2] = ix_max;
jstart[2] = iy_min;
jend[2] = iy_min;

istart[3] = ix_min;
iend[3] = ix_max;
jstart[3] = iy_max;
jend[3] = iy_max;

 for(k = 0; k < 3; k++) {
   mean[k] = 0.;
   sigma[k] = 0.;
   sum = 0.;
   sumsq = 0.;
   npts = 0;
   for(j = jstart[k]; j <= jend[k]; j++) {
     for(i = istart[k]; i <= iend[k]; i++) {
       value = in_image1[i + j * nx1];
       sum += value;
       sumsq += value * value;
       npts++;
       }
     }
// Mean:
     if(npts != 0) {
       mean[k] = sum / (double)npts;
       sigma[k] = sqrt((sumsq / (double)npts) - (mean[k] * mean[k]));
       } else {
       fprintf(stderr,"Is_a_peak/Error: ix_min=%d ix_max=%d iy_min=%d iy_max=%d npts=0 !\n",
               ix_min, ix_max, iy_min, iy_max);
       return(false);
       }
   }

// Pseudo-median (second lowest value) of the 4 edges:
 w_mean = mean[0];
 w_sigma = sigma[0];
 k_mean_mini = 0;
 k_sigma_mini = 0;
// Two iterations:
 for(m = 0; m < 2; m++) {
   for(k = 0; k < 4; k++) {
     if((k != k_mean_mini) && (w_mean < mean[k])) k_mean_mini = k;
     if((k != k_sigma_mini) && (w_sigma < sigma[k])) k_sigma_mini = k;
     }
   }
 w_mean = mean[k_mean_mini];
 w_sigma = sigma[k_sigma_mini];

// First criterium: (value - mean) > sigma_threshold * sigma;
 value = in_image1[ix + iy * nx1];
 is_a_peak = true;
 if((value - w_mean) < (sigma_threshold * w_sigma)) is_a_peak = false;

// Second criterium: value > value_of_neighbours
// Rather large box to avoid double points
x_half_width = 5;
y_half_width = 5;
ix_min = MAXI(0, ix - x_half_width);
ix_max = MINI(nx1 - 1, ix + x_half_width);
iy_min = MAXI(0, iy - y_half_width);
iy_max = MINI(ny1 - 1, iy + y_half_width);
k = 0;
   for(j = iy_min; j <= iy_max; j++) {
     for(i = ix_min; i <= ix_max; i++) {
      if(value < in_image1[i + j * nx1]) is_a_peak = false;
     }
   }

return(is_a_peak);
}
