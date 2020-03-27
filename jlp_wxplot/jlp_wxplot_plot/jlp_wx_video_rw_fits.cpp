/******************************************************************************
* Name:        jlp_wx_video_rw_fits.cpp
* Purpose:     read/write 2D/3D FITS files
* Author:      JLP
* Version:     18/04/2016
******************************************************************************/
#include "jlp_wx_video_panel.h"

#include "jlp_wx_ipanel.h"      // JLP_wxImagePanel class
#include "jlp_fitsio.h"         // for JLP_RDFITS_2D 

/******************************************************************
* Load new settings for FITS cube 
* 
* INPUT:
* ihdu0: HDU number in FITS file
*******************************************************************/
int JLP_wxVideoPanel::LoadNewFITSCube(wxString full_filename0, const int ihdu0, 
                                      const int nx0, const int ny0, 
                                      const int nz0)
{

 nz1 = 0;
 m_iframe = 0;
 full_filename1 = wxT("");

if((nz0 <= 1) || (full_filename0.IsEmpty())) return(-1);

// Save to private parameters:
 full_filename1 = full_filename0;
 nx1 = nx0;
 ny1 = ny0;
 nz1 = nz0;
 m_ihdu = ihdu0;

// If 3D fits, write the frame number to the second field of the status bar:
/*
  m_spea_frame->UpdateStatusBarAfterLoading(full_filename1, nx0, ny0, nz0);
  str1.Printf(wxT("Frame #%d/%d"), m_iframe, nz1);
  m_spea_frame->SetStatusText(str1, 1);
*/
// Check Idle radio item:
  EnableStopVideo(false);

return(0);
}
/******************************************************************
* Read an image from a 2DFITS file or from a single plane in 3D FITS cube
* 
* INPUT:
* iframe: index of image plane to be loaded from 1 to nz (in case of 3D data)
*******************************************************************/
int JLP_wxVideoPanel::ReadFITSImage_1(const int iframe, double **dble_image0)
{
int nx0, ny0, nz0;
wxString full_filename0;
char filename1[120], comments1[80], errmess1[200];
int status;
wxString buffer;

if((nz1 <= 1) || (full_filename1.IsEmpty())) return(-1);

// nx0, ny0, nz0: size of data cube
// dble_image1: array with the data contained in FITS file
// iframe: index of image plane to be loaded from 1 to nz (in case of 3D data)

// Load a new image or image plane in FITS format 
strncpy(filename1, full_filename1.mb_str(), 120);
status = JLP_RD_3DXFITS_2D_dble(dble_image0, &nx0, &ny0, &nz0, iframe, m_ihdu,
                                filename1, comments1, errmess1);
if (status) {
  wxLogError(_T("Couldn't load image from '") + full_filename1 + _T("'."));
  wxLogError(wxString(errmess1));
  return(-2);
  }

/***************************************************************/
 if(nx0 != nx1 || ny0 != ny1) {
   wxLogError(wxT("Error: incompatible size of new image plane"));
   return(-1);
   }

// Save current value of iframe and nz0 to private variables:
m_iframe = iframe;

return(0);
}
