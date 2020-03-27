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
  int FilterCircularGradient(const int iangle);
  int FilterCircularProfile(const int n_for_patches)
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
static int subtract_min_circ_profile(double *image0, int nx0, int ny0, 
                                     int ixc0, int iyc0);

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
* 4=high contrast1 (VHC1) 5=high contrast2 (VHC2), 6=high contrast3 (VHC3)
* 7=very high contrast4 (VHC4) 8=medium gradient(GRAD1), 9=hard gradient(GRAD2)
* 10=CPROF 11=CIRC1(rot 10 deg), 12=CIRC2(rot 20 deg)
* 13=CIRC3(rot 30 deg)
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
// Filter 4, 5, 6, 7 = FilterHighContrast 1, 2, 3, 4, 5, 6
// strioscopic filter (with high contrast)
  case 4:
  case 5:
  case 6:
  case 7:
    hc_filter_option = filter0 - 1;
    FilterHighContrast(hc_filter_option);
    break;
// 8 = GRAD1: medium gradient
  case 8:
    FilterGradient(1);
    break;
// 9 = GRAD2: hard gradient
  case 9:
    FilterGradient(2);
    break;
// 10=CPROF 
  case 10:
    FilterCircularProfile(80);
    break;
// 11=CIRC1 (rot 10 deg) 
  case 11:
    FilterCircularGradient(1);
    break;
// 12=CIRC2 (rot 20 deg) 
  case 12:
    FilterCircularGradient(2);
    break;
// 13=CIRC3 (rot 30 deg) 
  case 13:
    FilterCircularGradient(3);
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
* Filter: gradient 
*
* OPTION:
*   igrad=1 for medium gradient(GRAD1)
*   igrad=2 for hard gradient(GRAD2)
*
**********************************************************************/
int JLP_wxImagePanel::FilterGradient(const int igrad)
{
double *tmp_ima, *smooth_ima, gradx, grady, ww, wsmooth, thresh0;
int i, j, iw, npts;
wxString str1;
bool reset_ITT_to_MinMax;

// Restore original image (to prevent piling up the filters):
 RestoreOriginalImage();

 npts = nx1 * ny1;
 tmp_ima = new double[npts];
 for(i = 0; i < npts; i++) tmp_ima[i] = dble_image1[i]; 
 for(i = 0; i < npts; i++) dble_image1[i] = 0.; 

 smooth_ima = new double[npts];
    for(i = 0; i < npts; i++) smooth_ima[i] = 0.; 

if(igrad == 1) {
 iw = 2;
 str1 = wxT("%% Gradient filter 2020 (GRAD1)\n");
 } else {
 iw = 1;
 str1 = wxT("%% Gradient filter 2020 (GRAD2)\n");
 }


// hard gradient:
for(i = iw; i < nx1-iw; i++) {
  for(j = iw; j < ny1-iw; j++) {
// [+1 +1 +1]
// [+1 +1 +1]
// [+1 +1 +1]
    wsmooth = tmp_ima[i-1 + (j+1)*nx1] + tmp_ima[i-1 + j*nx1]
            + tmp_ima[i-1 + (j-1)*nx1] 
            + tmp_ima[i + (j+1)*nx1] 
            + tmp_ima[i + j*nx1] + tmp_ima[i + (j-1)*nx1]
            + tmp_ima[i+1 + (j+1)*nx1] 
            + tmp_ima[i+1 + j*nx1] + tmp_ima[i+1 + (j-1)*nx1];
    smooth_ima[i + j * nx1] = wsmooth / 9.; 
// [-1 0 +1]
// [-1 0 +1]
// [-1 0 +1]
    gradx = - tmp_ima[i-1 + (j+1)*nx1] - tmp_ima[i-1 + j*nx1]
            - tmp_ima[i-1 + (j-1)*nx1]  + tmp_ima[i+1 + (j+1)*nx1] 
            + tmp_ima[i+1 + j*nx1] + tmp_ima[i+1 + (j-1)*nx1];
// [-1 -1 -1]
// [ 0  0  0]
// [+1 +1 +1]
    grady = - tmp_ima[i-1 + (j+1)*nx1] - tmp_ima[i + (j+1)*nx1]
            - tmp_ima[i+1 + (j+1)*nx1] + tmp_ima[i-1 + (j-1)*nx1] 
            + tmp_ima[i + (j-1)*nx1] + tmp_ima[i+1 + (j-1)*nx1];
    ww = gradx*gradx + grady*grady; 
    if(ww > 0.) dble_image1[i + j * nx1] = sqrt(ww); 
  }
}
    for(i = 0; i < npts; i++) 
      dble_image1[i] = smooth_ima[i] - dble_image1[i]; 
// Remove the very center:
  dble_image1[nx1/2 + (ny1/2) * nx1] = 0.;

// Sort the intensity values of the image 
 jlp_sort_array_intensities(dble_image1, nx1, ny1, tmp_ima, npts); 

// Threshold=total-80 values 
iw = npts - 80;
if(iw < 0) iw = npts - 5;
thresh0 = tmp_ima[iw];
for(i=0; i < npts; i++) if(dble_image1[i] < thresh0) dble_image1[i] = 0.;

// Update image values
  reset_ITT_to_MinMax = true;
  Image1_wxgdev->UpdateCImage1Values(dble_image1, nx1, ny1,
                                  reset_ITT_to_MinMax);

// Write to logbook window (and record on the logbook file):
  if(jlp_logbook != NULL) jlp_logbook->WriteToLogbook(str1, true);

delete[] tmp_ima;
delete[] smooth_ima;
return(0);
}
/*********************************************************************
* Filter: circular gradient 
*
* OPTION:
* 11=CIRC1(rot 10 deg) 
* 12=CIRC2(rot 20 deg) 
* 13=CIRC3(rot 30 deg) 
*
**********************************************************************/
int JLP_wxImagePanel::FilterCircularGradient(const int iangle)
{
double *tmp_ima;
double angle0, thresh0;
int i, j, npts, iw, status, ixc, iyc, irho2;
wxString str1;
bool reset_ITT_to_MinMax;

// Restore original image (to prevent piling up the filters):
 RestoreOriginalImage();

npts = nx1 * ny1;
tmp_ima = new double[npts];
for(i=0; i < npts; i++) tmp_ima[i] = 0.;

 switch(iangle) {
    default:
    case 1:
      angle0 = 10. * PI / 180.;
      str1 = wxT("%% Circular gradient filter 10 deg (CIRC1)\n");
      break;
    case 2:
      angle0 = 20. * PI / 180.;
      str1 = wxT("%% Circular gradient filter 20 deg (CIRC2)\n");
      break;
    case 3:
      angle0 = 30. * PI / 180.;
      str1 = wxT("%% Circular gradient filter 30 deg (CIRC3)\n");
      break;
 }

// Use private variables:
// dble_image1, nx1, ny1
// Rotation of the image (in jlplib/jlp_numeric/rotate_set.cpp) :
//int rotimage(double *ima, double *imarot, int nx, int ny, double angle)
 status = rotimage(dble_image1, tmp_ima, nx1, ny1, angle0);

// Subtracting rotated image:
for(i=0; i < npts; i++) dble_image1[i] -= tmp_ima[i];

// Sort the intensity values of the image 
 jlp_sort_array_intensities(dble_image1, nx1, ny1, tmp_ima, npts); 

// Threshold=total-80 values 
iw = npts - 80;
if(iw < 0) iw = npts - 10;
thresh0 = tmp_ima[iw];
ixc = nx1/2;
iyc = ny1/2;

for(i=0; i < nx1; i++) 
  for(j=0; j < ny1; j++) 
    {
    irho2 = SQUARE(i - ixc) + SQUARE(j - iyc);
// Discard all points too close to the center too:
    if((dble_image1[i + j * nx1] < thresh0) || (irho2 < 16)) 
        dble_image1[i + j * nx1] = 0.;
    }

// Update image values
reset_ITT_to_MinMax = true;
Image1_wxgdev->UpdateCImage1Values(dble_image1, nx1, ny1,
                                    reset_ITT_to_MinMax);

// Write to logbook window (and record on the logbook file):
if(jlp_logbook != NULL) jlp_logbook->WriteToLogbook(str1, true);

delete[] tmp_ima;
return(0);
}
/*********************************************************************
* Filter: circular profile 
*
* OPTION:
* 10=CPROF 
*
* INPUT
*  n_for_patches: number of points above the threshold
*
**********************************************************************/
int JLP_wxImagePanel::FilterCircularProfile(const int n_for_patches)
{
double *tmp_ima, thresh0;
int i, j, npts, iw, status, ixc, iyc, irho2;
wxString str1;
bool reset_ITT_to_MinMax;

// Restore original image (to prevent piling up the filters):
 RestoreOriginalImage();

str1 = wxT("%% Circular profile filter (CPROF)\n");

npts = nx1 * ny1;
tmp_ima = new double[npts];
for(i=0; i < npts; i++) tmp_ima[i] = dble_image1[i];

ixc = nx1/2;
iyc = ny1/2;
status = subtract_min_circ_profile(tmp_ima, nx1, ny1, ixc, iyc);
for(i=0; i < npts; i++) dble_image1[i] = tmp_ima[i];

// Sort the intensity values of the image 
 jlp_sort_array_intensities(dble_image1, nx1, ny1, tmp_ima, npts); 

// n_for_patches: number of points above the threshold
// Threshold=total-n_for_patches values 
iw = npts - n_for_patches;
if(iw < 0) iw = npts - 10;
thresh0 = tmp_ima[iw];
for(i=0; i < nx1; i++) 
  for(j=0; j < ny1; j++) 
    {
    irho2 = SQUARE(i - ixc) + SQUARE(j - iyc);
// Discard all points too close to the center too:
    if((dble_image1[i + j * nx1] < thresh0) || (irho2 < 16)) 
        dble_image1[i + j * nx1] = 0.;
    }

// Update image values
reset_ITT_to_MinMax = true;
Image1_wxgdev->UpdateCImage1Values(dble_image1, nx1, ny1,
                                    reset_ITT_to_MinMax);

// Write to logbook window (and record on the logbook file):
if(jlp_logbook != NULL) jlp_logbook->WriteToLogbook(str1, true);

delete[] tmp_ima;
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
* Automatic measurement of binaries (for autocorrelations)
**************************************************************************/
int JLP_wxImagePanel::BinaryMeasFromCircProfile(const int n_for_patches,
                                 double *xc, double *yc, 
                                 double *radius, double *rho10, double *theta10,
                                 double *error_rho10, double *error_theta10)
{
int status, ixc, iyc;

ixc = nx1/2;
iyc = ny1/2;
*xc = (double)ixc;
*yc = (double)iyc;
*radius = 3;
*rho10 = 0.;
*theta10 = 0.;
*error_rho10 = 0.;
*error_theta10 = 0.;

// Apply circular profile filter
FilterCircularProfile(n_for_patches);

// Measure symmetric spots if any 
status = AutoMeasureAutocBinaryWithParam(rho10, theta10, error_rho10,
                                         error_theta10);

printf("Measurements: rho0=%.2f+/-%.2f pix. theta0=%.2f+/-%.2f deg.\n",
       *rho10, *error_rho10, *theta10 * (180. / PI),
       *error_theta10 * (180. / PI));

if((*error_rho10 > 0.01) && (*error_theta10 * (180. / PI) > 0.01)
 && (*error_rho10 < 5.) && (*error_theta10 * (180. / PI) < 10.)) {
      status = 0;
      } else {
      status = -1;
      }

if(status == 0) {
  *xc = (double)ixc + (*rho10) * cos(*theta10); 
  *yc = (double)ixc + (*rho10) * sin(*theta10); 
// Radius increasing with distance to center:
// 3 at rho=10
// 10 at rho=80
  *radius = 2. + (*rho10) * 0.1; 
  }

printf("Goodness of the fit: status=%d\n", status);

return(status);
}
/*************************************************************************
* Automatic measurement of binaries (for autocorrelations)
**************************************************************************/
int JLP_wxImagePanel::AutoMeasureAutocBinary()
{
double rho10, theta10, error_rho10, error_theta10;
double xc, yc, radius;
int status, n_for_patches;

n_for_patches = 80.;
status = BinaryMeasFromCircProfile(n_for_patches,
                                 &xc, &yc, &radius, &rho10, &theta10,
                                 &error_rho10, &error_theta10);

return(status);
}
/*************************************************************************
* Automatic measurement of binaries (for autocorrelations)
*
* OUTPUT:
*   rho10, err_rho10
*   theta10, error_theta10 (radians)
**************************************************************************/
int JLP_wxImagePanel::AutoMeasureAutocBinaryWithParam( double *rho10, 
                                        double *theta10, double *error_rho10, 
                                        double *error_theta10) 
{
double min0;
int status, i, j, ixc, iyc;
double rho1, tan1, weight1, sum_weight1;
double r1, rr1, t1, tt1;

*rho10 = 0.;
*theta10 = 0.;
*error_rho10 = 0.;
*error_theta10 = 0.;

ixc = nx1 / 2;
iyc = ny1 / 2;

// Looking for the sky level as the minimum (far from the center):
min0 = 1.E+8;
for(i=0; i < nx1; i++) 
  {
  for(j=0; j < ny1; j++) 
    {
// Measure outside the circle centered on the center of the image, of radius=3
    if((ABS(i - ixc) > 3) && (ABS(j - iyc) > 3) 
       && (dble_image1[i + j * nx1] > 0.)
       && (dble_image1[i + j * nx1] < min0)) {
       min0 = dble_image1[i + j * nx1];
       }
    }
  }

sum_weight1 = 0.;
r1 = 0.;
t1 = 0.;
rr1 = 0.;
tt1 = 0.;
for(i=0; i < nx1; i++) 
  {
  for(j=0; j < ny1; j++) 
    {
    if((dble_image1[i + j * nx1] > min0)
       && (i-ixc != 0.)) {
      rho1 = (double)(SQUARE(i - ixc) + SQUARE(j - iyc));
      tan1 = (double)(j - iyc)/(double)(i - ixc);
      if(rho1 > 0) {
        rho1 = sqrt(rho1);
        weight1 = dble_image1[i + j * nx1] - min0;
        r1 += rho1 * weight1;
        t1 += atan(tan1) * weight1;
        rr1 += SQUARE(rho1) * weight1;
        tt1 += SQUARE(atan(tan1)) * weight1;
        sum_weight1 += weight1;
        }
     }
  }
}
// Measurement and error: 
if(sum_weight1 > 0.) {
 *rho10 = r1 / sum_weight1;
 *theta10 = t1 / sum_weight1;
 *error_rho10 = (rr1 / sum_weight1) - SQUARE(*rho10); 
 *error_theta10 = (tt1 / sum_weight1) - SQUARE(*theta10); 
 if(*error_rho10 > 0) *error_rho10 = sqrt(*error_rho10);
 if(*error_theta10 > 0) *error_theta10 = sqrt(*error_theta10);
}
/* DEBUG
*/
printf("Measure: rho0=%.2f+/-%.2f pix. theta0=%.2f+/-%.2f deg. (min0=%.3e sumweight=%.3e))\n", 
   *rho10, *error_rho10, *theta10 * (180. / PI),
   *error_theta10 * (180. / PI), min0, sum_weight1);

return(status);
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
/**************************************************************************
* Compute the minimum circular profile of an image 
*
* INPUT:
* image0: input 2D array
* nx0, ny0: dimension of the array
*
* OUTPUT:
* value0: value corresponding to the median
**************************************************************************/
static int subtract_min_circ_profile(double *image0, int nx0, int ny0, 
                                     int ixc0, int iyc0)
{
double *profile, rho;
int i, j, npts, nprof, iprof;

npts = nx0 * ny0;

// To avoid round off error, I multiply by 4 (and add 0.5):
nprof = 4 * (int)sqrt((double)(SQUARE(nx0)+SQUARE(ny0)));
profile = new double[nprof];

for(i = 0; i < nprof; i++) {
   profile[i] = 1.e+9;
   }

// Compute the minimum profile
  for(i = 0; i < nx0; i++) 
    {
    for(j = 0; j < ny0; j++) 
      {
      rho = SQUARE(i - ixc0) + SQUARE(j - iyc0); 
      if(rho > 0.) rho = sqrt(rho);
// To avoid round off error, I multiply by 4 and add 0.5:
       iprof = (int)(4. * rho + 0.5);
       if(image0[i + j * nx0] < profile[iprof])
            profile[iprof] = image0[i + j * nx0];
      }
    }

// Subtract the minimum profile
  for(i = 0; i < nx0; i++) 
    {
    for(j = 0; j < ny0; j++) 
      {
      rho = SQUARE(i - ixc0) + SQUARE(j - iyc0); 
      if(rho > 0.) rho = sqrt(rho);
// To avoid round off error, I multiply by 4 and add 0.5:
       iprof = (int)(4. * rho + 0.5);
       image0[i + j * nx0] -= profile[iprof];
      }
    }

delete[] profile;
return(0);
}
