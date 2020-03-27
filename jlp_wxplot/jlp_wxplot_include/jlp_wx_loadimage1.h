/******************************************************************************
* jlp_wx_loadimage1.cpp
* To load FITS, GIF, JPEG, ... images
*
* Author:      JLP 
* Version:     01/12/2008 
******************************************************************************/
#ifndef jlp_wx_loadimage1_h    // sentry 
#define jlp_wx_loadimage1_h

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#undef index             // To solve conflicts with index in "string.h" ...
    #include "wx/wx.h"
#endif

#include "wx/image.h"
#include "wx/file.h"
#include "wx/filename.h"
#include "wx/mstream.h"
#include "wx/wfstream.h"
#include "wx/quantize.h"

// Display images in GIF, TIFF, JPEG, ... format 
int JLP_wxDisplayToNewFrame(wxString &filename);

// Load an image from FITS file
int JLP_wxLoadFITSImage(wxString &filename, double **dble_image1,
                        int *nx1, int *ny1, int *nz1, int iframe);
int JLP_wxLoadXFITSImage(wxString &filename, double **dble_image1,
                        int *nx1, int *ny1, int *nz1, int iframe,
                        int ihdu, FILE *header_outfile);

#endif               // EOF sentry
