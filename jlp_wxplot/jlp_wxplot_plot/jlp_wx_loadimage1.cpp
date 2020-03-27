/******************************************************************************
* jlp_wx_loadimage1.cpp
* To load FITS, GIF, JPEG, ... images
*
* Author:  JLP 
* Version: 10/12/2012 
******************************************************************************/
#include "jlp_wx_loadimage1.h"  // Prototypes of routines defined here
#include "jlp_fitsio.h"         // JLP_RDFITS_2D_dble

/******************************************************************
* Open a new frame with images in GIF, TIFF, JPEG, FITS ... format 
*
* INPUT: 
* filename: if not NULL, load this image, otherwise prompt the user
*           with a dialog window.
*
* OUTPUT:
* filename: image file name 
*
*******************************************************************/
int JLP_wxDisplayToNewFrame(wxString &filename)
{
// JLP_wxImageFrame1 *new_frame1 = (JLP_wxImageFrame1 *)NULL;
wxImage image;
wxString title, path, extension, buffer;
double *dble_image1;
int nx1, ny1, nz1;

// Prompt the user for the filename, if not set by the calling routine:
if(filename.IsNull()) {
  filename = wxFileSelector(_T("Select gif, jpeg, bmp, ... image"));
  if ( !filename ) return(-1);
  }

// Removes the directory name (since the full path is generally too long...) 
  wxFileName::SplitPath(filename, &path, &title, &extension);

// Filter the extension:

// Handle the case of a FITS file:
   if((extension.CmpNoCase(_T("fits")) == 0) || 
      (extension.CmpNoCase(_T("fit")) == 0)) { 

      JLP_wxLoadFITSImage(filename, &dble_image1, &nx1, &ny1, &nz1, 0);

// Create an independant frame with this image:
// (since parent = NULL)
// TOBECHECKED JLP2013
#if 0
      new_frame1 = new JLP_wxImageFrame1(NULL, dble_image1, nx1, ny1, nz1, 
            filename, title);
      new_frame1->Show();
#endif

// Handle the case of a file in another format:
   } else { 
// Load jpeg, tiff, pcx, etc:
     if ( !image.LoadFile(filename) ) {
       buffer = _T("Couldn't load image from >") + filename + _T("<");
       wxLogError(buffer);
       return(-2);
       }
// Create an independant frame with this image:
// (since parent = NULL)
#if 0
     new_frame1 = new JLP_wxImageFrame1(NULL, image, filename, title);
     new_frame1->Show();
#endif
    }

#if 0
if(new_frame1 != NULL) delete new_frame1;
#endif
return(0);
}
/******************************************************************
* Load images from FITS files
*
* INPUT: 
* filename: if not NULL, load this image, otherwise prompt the user
*           with a dialog window.
* iframe: image plane to be loaded (from 1 to nz1)
*
* OUTPUT:
* filename: image file name 
* dble_image1: array with the data contained in FITS file
* nx1, ny1, nz1: size of data cube
*
*******************************************************************/
int JLP_wxLoadFITSImage(wxString &filename, double **dble_image1,
                        int *nx1, int *ny1, int *nz1, int iframe)
{
char filename1[120], comments1[80], errmess1[200];
int status;
wxString buffer;

// Prompt the user for the FITS filename, if not set by the calling routine:
if(filename.IsNull()) {
  filename = wxFileSelector(_T("Select image FITS file"), _T(""), _T(""),
                            _T("fits|fit"), 
                            _T("FITS files (*.fits;*.fit)|*.fits;*.fit"));
 
  if ( !filename ) return(-1);
  }

strncpy(filename1, filename.mb_str(), 120);
status = JLP_RDFITS_2D_dble(dble_image1, nx1, ny1, nz1, iframe, filename1, 
                             comments1, errmess1);
if (status) {
  buffer.Printf(_T("Couldn't load image from %s \n %s"), filename1, errmess1);
  wxLogError(buffer);
  return(-2);
  }

return(0);
}
/******************************************************************
* Load images from FITS complex files (tables and XTENSION images)
*
* INPUT: 
* filename: if not NULL, load this image, otherwise prompt the user
*           with a dialog window.
* iframe: image plane to be loaded (from 1 to nz1)
*
* OUTPUT:
* filename: image file name 
* dble_image1: array with the data contained in FITS file
* nx1, ny1, nz1: size of data cube
*
*******************************************************************/
int JLP_wxLoadXFITSImage(wxString &filename, double **dble_image1,
                         int *nx1, int *ny1, int *nz1, int iframe,
                         int ihdu)
{
char filename1[120], comments1[80], errmess1[200];
int status;
wxString buffer;

// Prompt the user for the FITS filename, if not set by the calling routine:
if(filename.IsNull()) {
  filename = wxFileSelector(_T("Select 3d-table-image FITS file"), 
                            _T(""), _T(""), _T("fits|fit"), 
                            _T("FITS files (*.fits;*.fit)|*.fits;*.fit"));
 
  if ( !filename ) return(-1);
  }

strncpy(filename1, filename.mb_str(), 120);
status = JLP_RD_3DXFITS_2D_dble(dble_image1, nx1, ny1, nz1, iframe, ihdu,
                                filename1, comments1, errmess1);
/* DEBUG PC ONLY
status = JLP_RDXFITS_2D_dble(dble_image1, nx1, ny1, nz1, iframe, ihdu,
                             filename1, comments1, errmess1);
*/
if (status) {
  buffer.Printf(_T("Couldn't load image from %s \n %s"), filename1, errmess1);
  wxLogError(buffer);
  return(-2);
  }

return(0);
}
