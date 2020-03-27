/********************************************************************
* jlp_wxspeckle1_dlg_utils.h
* To compute mean and std deviation from Xdisp1.log
* rho, theta (for binaries)
* Append results to a LateX file
*
* JLP
* Version 06/02/2019
********************************************************************/
#ifndef _jlp_wxspeckle1_dlg_set2_h   // BOF sentry
#define _jlp_wxspeckle1_dlg_set2_h

int speckle_process_data(const wxString data_fname,
                         const wxString LatexFilename,
                         const wxString original_FITS_fname,
                         const wxString processed_FITS_fname,
                         wxString &error_message);
int decode_info_from_FITS_file(const char *original_fits_fname, char *date,
                               char *filter, char *object_name, double *epoch,
                               double *year, int *xbin, int *ybin,
                               char *comments);

#endif // EOF sentry
