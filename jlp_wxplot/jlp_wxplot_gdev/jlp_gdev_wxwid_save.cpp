/*****************************************************************************
* JLP_GDev_wxWID class: saving routines to FITS, JPEG, Postscript, etc.
*
* Part with PNG/JPEG was taken from wxwidgets tutorials: samples/image/image.cpp
* (Author:  Robert Roebling ; Copyright:   (c) 1998-2005 Robert Roebling)
* The orther part was written by JLP only...  *
* JLP
* Version 16/02/2017
******************************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif

#include "wx/image.h"
#include "wx/file.h"
#include "wx/filename.h"
#include "wx/mstream.h"
#include "wx/wfstream.h"
#include "wx/quantize.h"

#include "jlp_splot_idv.h"       // jlp_hardcopy_image()
#include "jlp_gdev_wxwid.h" // JLP_GDev_wxWID Class
#include "jlp_fitsio.h"      // JLP_WRFITS_2D_dble()
#include "jlp_gdev_pst.h"  // JLP_GDev_PST class
#include "jlp_wx_pstcopy_dlg.h"  // JLP_PstCopy_Dlg()

/************************************************************************
* Save the current bitmap to image-formatted file
*
************************************************************************/
void JLP_GDev_wxWID::SaveGraphicToFile()
{
char filename[128], comments[80], err_mess[80];
wxString buffer;
double *array0;
int status, nx0, ny0;
bool saved = false;

// Init all image handlers in case it was not already done:
// (Needed to read PNG,JPEG, etc)
::wxInitAllImageHandlers();

// Convert backup DC to image:
   wxImage image1 = backup_dc_bitmap2->ConvertToImage();

   wxString savefilename = wxFileSelector( wxT("Save Image"),
                                           wxEmptyString,
                                           wxEmptyString,
                                           (const wxChar *)NULL,
                                           wxT("JPEG files (*.jpg)|*.jpg|")
                                           wxT("PNG files (*.png)|*.png|")
                                           wxT("FITS files (*.fits)|*.fits|")
                                           wxT("BMP files (*.bmp)|*.bmp|")
                                           wxT("GIF files (*.gif)|*.gif|")
                                           wxT("TIFF files (*.tif)|*.tif|")
                                           wxT("PCX files (*.pcx)|*.pcx|")
                                           wxT("ICO files (*.ico)|*.ico|")
                                           wxT("CUR files (*.cur)|*.cur"),
                                           wxFD_SAVE,
                                           this);

   if ( savefilename.empty() ) return;

   wxString extension;
   wxFileName::SplitPath(savefilename, NULL, NULL, &extension);

// Handle the case of a FITS file:
   if((extension.CmpNoCase(_T("fits")) == 0) ||
      (extension.CmpNoCase(_T("fit")) == 0)) {
/* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
*/
    if((Jgc0.gdev_graphic_type == 3) && (c_image1 != NULL)) {
      sprintf(comments,"Created by JLP_GDev_wxWID version 02/01/2015");
      sprintf(filename, "%s", (const char*)(savefilename.mb_str()));
      status = c_image1->GetDoubleArray(&array0, &nx0, &ny0);
      if(status == 0) {
        status = JLP_WRFITS_2D_dble(array0, nx0, ny0, nx0, filename,
                                    comments, err_mess);
        if(status == 0) saved = true;
        delete[] array0;
        }
      } else {
     buffer.Printf(wxT("Error creating FITS file: the option is not available with curves "));
     wxMessageBox(buffer, wxT("JLP_Gdev_wxWID::SaveGraphicToFile"),
               wxOK | wxICON_ERROR);
      }
   }
   else if ( extension == _T("bmp") )
   {
       static const int bppvalues[] =
       {
           wxBMP_1BPP,
           wxBMP_1BPP_BW,
           wxBMP_4BPP,
           wxBMP_8BPP,
           wxBMP_8BPP_GREY,
           wxBMP_8BPP_RED,
           wxBMP_8BPP_PALETTE,
           wxBMP_24BPP
       };

       const wxString bppchoices[] =
       {
           _T("1 bpp color"),
           _T("1 bpp B&W"),
           _T("4 bpp color"),
           _T("8 bpp color"),
           _T("8 bpp greyscale"),
           _T("8 bpp red"),
           _T("8 bpp own palette"),
           _T("24 bpp")
       };

       int bppselection = wxGetSingleChoiceIndex(_T("Set BMP BPP"),
                                                 _T("Image sample: save file"),
                                                 WXSIZEOF(bppchoices),
                                                 bppchoices,
                                                 this);
       if ( bppselection != -1 )
       {
           int format = bppvalues[bppselection];
           image1.SetOption(wxIMAGE_OPTION_BMP_FORMAT, format);

           if ( format == wxBMP_8BPP_PALETTE )
           {
               unsigned char *cmap = new unsigned char [256];
               for ( int i = 0; i < 256; i++ )
                   cmap[i] = (unsigned char)i;
               image1.SetPalette(wxPalette(256, cmap, cmap, cmap));

               delete[] cmap;
           }
        }
// Save image:
       saved = image1.SaveFile(savefilename, wxBITMAP_TYPE_BMP);
   }
   else if ( extension == _T("png") )
   {
       static const int pngvalues[] =
       {
           wxPNG_TYPE_COLOUR,
           wxPNG_TYPE_COLOUR,
           wxPNG_TYPE_GREY,
           wxPNG_TYPE_GREY,
           wxPNG_TYPE_GREY_RED,
           wxPNG_TYPE_GREY_RED,
       };

       const wxString pngchoices[] =
       {
           _T("Colour 8bpp"),
           _T("Colour 16bpp"),
           _T("Grey 8bpp"),
           _T("Grey 16bpp"),
           _T("Grey red 8bpp"),
           _T("Grey red 16bpp"),
       };

       int sel = wxGetSingleChoiceIndex(_T("Set PNG format"),
                                        _T("Image sample: save file"),
                                        WXSIZEOF(pngchoices),
                                        pngchoices,
                                        this);
       if ( sel != -1 )
       {
           image1.SetOption(wxIMAGE_OPTION_PNG_FORMAT, pngvalues[sel]);
           image1.SetOption(wxIMAGE_OPTION_PNG_BITDEPTH, sel % 2 ? 16 : 8);
       }
// Save image:
       saved = image1.SaveFile(savefilename, wxBITMAP_TYPE_PNG);
   }
   else if ( extension == _T("cur") )
   {
       image1.Rescale(32,32);
       image1.SetOption(wxIMAGE_OPTION_CUR_HOTSPOT_X, 0);
       image1.SetOption(wxIMAGE_OPTION_CUR_HOTSPOT_Y, 0);
// This shows how you can save an image with explicitly specified image format:
       saved = image1.SaveFile(savefilename, wxBITMAP_TYPE_CUR);
   }
   else if ( extension == _T("jpg") ) {
// Set reasonable quality and compression:
       image1.SetOption(wxIMAGE_OPTION_QUALITY, 80);
// Save image:
       saved = image1.SaveFile(savefilename, wxBITMAP_TYPE_JPEG);
   }


   if ( !saved )
   {
       // This one guesses image format from filename extension
       // (it may fail if the extension is not recognized):
       if(!image1.SaveFile(savefilename))
  wxMessageBox(wxT("Format is not available in this version"), 
	       wxT("JLP_Gdev_wxWID::SaveGraphicToFile"),
               wxOK | wxICON_ERROR);
   }
return;
}
/************************************************************************
* To save display of curves to postscript file
************************************************************************/
int JLP_GDev_wxWID::PstCopyOfDisplayForCurves(char *pst_filename)
{
int status = -1, ic;
JLP_GDev *Jwx;
char plotdev[64], err_messg[128];
wxString buffer;

// plotdev: "square" or "fullpage" (horizontal landscape)
//strcpy(plotdev, "square");
strcpy(plotdev, "fullpage");

// New JLP_cGDev_wxWID object (for display on postscript file)
Jwx = new JLP_GDev_PST(plotdev, pst_filename, Jgc0.box_title,
                       Jgc0.xmin_user, Jgc0.xmax_user,
                       Jgc0.ymin_user, Jgc0.ymax_user, Jgc0.box_plan,
                       &status, err_messg);
if(status) {
  buffer.Printf(wxT("Creating new JLP_cGDev_PST/Error: %s"), err_messg);
  wxMessageBox(buffer, wxT("JLP_Gdev_wxWID::PstCopyOfDisplayForCurves"),
               wxOK | wxICON_ERROR);
 } else {
// Load data from private parameters (xplot_1, yplot_1, errorx_1, errory_1, etc)
// (in "jlp_splot/jlp_gdev_curves_process.cpp")
  for(ic = 0; ic < ncurves_1; ic++) {
    Jwx->Curves_LoadPlotDataToPrivateParameters(&xplot_1[ic * nmaxi_1], 
                             &yplot_1[ic * nmaxi_1],
                             &errorx_1[ic * nmaxi_1], &errory_1[ic * nmaxi_1],
                             npts_1[ic], &nchar_1[ic * 4], &pcolor_1[ic * 32],
                             &plot_fname_1[ic * 128], 0);
    }
// Load settings from Jgc0 structure
  Jwx->Curves_LoadPlotSettings(Jgc0.box_xlabel, Jgc0.box_ylabel, Jgc0.box_title,
                               Jgc0.pen_colour, Jgc0.pen_default_colour,
                               Jgc0.backgd_colour,
                               Jgc0.box_xgrid, Jgc0.box_ygrid, Jgc0.xaxis_type,
                               Jgc0.box_plan, Jgc0.axis_limits[0],
                               Jgc0.axis_limits[1], Jgc0.axis_limits[2],
                               Jgc0.axis_limits[3]);
  Jwx->PlotAllCurves_splot();
//  delete Jwx;
 }

return(status);
}
/************************************************************************
* To save display of images to postscript file
************************************************************************/
int JLP_GDev_wxWID::PstCopyOfDisplayForImages(char *pst_filename)
{
JLP_PstCopy_Dlg *PstCopyDlg;
int answer, tex_fonts, black_and_white, high_resolution, lut_scale;
double *dble_image0;
float *image_f1;
int *image_i, f1_nx1, f1_ny1;
double low_thresh, high_thresh, plot_width, plot_height;
int ncolors, f1_xstart, f1_ystart, f1_xend, f1_yend;
char title[80], image_comments[80], graphic_file[64];
char plotdev[60], input_filename[80];
int i, lut_reversed, lut_offset, lut_slope, itt_is_linear;
char lut_type[32];
wxString itt_type0;
int x1_box0, y1_box0, x2_box0, y2_box0;

if(c_image1 == NULL) return(-1);

strcpy(input_filename, "");

// Options for hardcopy:
 PstCopyDlg = new JLP_PstCopy_Dlg(NULL, wxT("Options for postscript copy"));

// WARNING: There is a bug here coming from "wxwidgets"
// when using ShowModal, the system doesn't return
// The computer may even crash if there are too many of those processed hanging
// around and using CPU time !
 answer = PstCopyDlg->ShowModal();

 if(answer == 0) {
// Retrieve the patch parameters have another try with those parameters
   PstCopyDlg->RetrieveData(&tex_fonts, &black_and_white, &high_resolution,
                            &lut_scale);
   delete PstCopyDlg;
// Return without harcopy:
   } else {
   delete PstCopyDlg;
   return(-1);
   }

// Copy image data to temporary array:
c_image1->GetDoubleArray(&dble_image0, &f1_nx1, &f1_ny1);

image_f1 = new float[f1_nx1 * f1_ny1];
for(i = 0; i < f1_nx1 * f1_ny1; i++) image_f1[i] = dble_image0[i];

// Work space for pstcopy1 only:
image_i = new int[f1_nx1 * f1_ny1];
for(i = 0; i < f1_nx1 * f1_ny1; i++) image_i[i] = 0;

 ncolors = 256;
 strcpy(image_comments, "");

/* Zoomed image:
* TOBEDONE later: zoom ...
* f1_xstart, f1_ystart, ,f1_xend, f1_yend
*/
f1_xstart = 0; f1_ystart = 0;
f1_xend = f1_nx1; f1_yend = f1_ny1;
// width, height: output size of the image on the plot (cm)
 plot_width = 15.;
 plot_height = 15.;
 itt_is_linear = c_image1->ITT_Is_Linear();
 c_image1->GetITT_Thresh(&itt_type0, &low_thresh, &high_thresh, &x1_box0,
                         &y1_box0, &x2_box0, &y2_box0);
 c_image1->Get_LUT_type(lut_type);
 c_image1->Get_LUT_param(lut_offset, lut_slope, lut_reversed);
 strcpy(graphic_file, "");
 strcpy(plotdev, "square");
 strcpy(title, "");

/*int jlp_hardcopy_image(float *image_f1, int *image_i, int *nx2,
                       int *ny2, int *nx2, int *ncolors,
                       char *input_filename,
                       char *comments, int lut_scale, int black_and_white,
                       int high_resolution, int *nx1, int *ny1,
                       int f1_xmin, int f1_ymin, int *f1_nx11,
                       int *f1_ny11, int width_frame, int height_frame,
                       int itt_is_linear, double lower_itt, double upper_itt,
                       char *lut_type, int inversed_lut, int lut_offset,
                       int lut_slope, char *graphic_file, char *pst_plotdev,
                       char *out_filename, char *title)
*/
jlp_hardcopy_image(image_f1, image_i, &f1_nx1, &f1_ny1, &f1_nx1, &ncolors,
                   input_filename, image_comments,
                   lut_scale, black_and_white, high_resolution,
                   &f1_nx1, &f1_ny1, f1_xstart, f1_ystart, &f1_nx1,
                   &f1_ny1, plot_width, plot_height,
                   itt_is_linear, low_thresh, high_thresh,
                   lut_type, lut_reversed, lut_offset, lut_slope,
                   graphic_file, plotdev, pst_filename, title);


// Free memory:
delete[] image_f1;
delete[] image_i;
delete[] dble_image0;

return(0);
}
/************************************************************************
* Output curve as a postscript file (GSEG)
************************************************************************/
void JLP_GDev_wxWID::SaveGraphicToPostscriptFile()
{
wxString save_filename;
char pst_filename[128];

   wxFileDialog dialog(NULL, _T("save to postscript file"), wxEmptyString,
                       wxEmptyString, _T("files (*.ps)|*.ps"),
                       wxFD_SAVE|wxFD_OVERWRITE_PROMPT);
   if (dialog.ShowModal() != wxID_OK) return;

   save_filename = dialog.GetPath();

   if ( save_filename.empty() ) return;
   strcpy(pst_filename, (const char *)save_filename.mb_str());

/* GDevGraphicType:
* 1 = jlp_splot_curves
* 2 = jlp_splot_images
* 3 = wx_scrolled/jlp_splot_images
* 4 = gsegraf_2d_curves
* 5 = gsegraf_2d_images
* 6 = gsegraf_3d_curves
* 7 = gsegraf_3d_images
* 8 = gsegraf_polar_curve
* 7 = gsegraf_polar_curve
*/
  if(Jgc0.gdev_graphic_type == 1)
    PstCopyOfDisplayForCurves(pst_filename);
  else if((Jgc0.gdev_graphic_type == 2)
         || (Jgc0.gdev_graphic_type == 3))
    PstCopyOfDisplayForImages(pst_filename);
  else if(Jgc0.gdev_graphic_type == 4)
    PstCopyOfDisplayFromBackupForGseg(pst_filename);
  else
    PstCopyOfDisplayFromBitmapForGseg(pst_filename);

return;
}
/************************************************************************
* Output curve as a postscript file (GSEG)
************************************************************************/
int JLP_GDev_wxWID::PstCopyOfDisplayFromBitmapForGseg(char *pst_filename)
{
wxPrintData *my_printer = new wxPrintData();
wxPostScriptDC *pst_dc0;
int width2, height2, pst_x0, pst_y0, pst_width0, pst_height0;

// my_printer->SetPaperId(wxPAPER_A4_rotated);
my_printer->SetPaperId(wxPAPER_A4);
my_printer->SetFilename(pst_filename);
my_printer->SetPrintMode(wxPRINT_MODE_FILE);
pst_dc0 = new wxPostScriptDC(*my_printer);

if(pst_dc0->IsOk() == true) {
// tell it where to find the afm files
// pst_dc0.GetPrintData().SetFontMetricPath(wxT("afm/"));
// set the resolution in points per inch (the default is 720)
// pst_dc0->SetResolution(1440);

  pst_dc0->StartDoc(wxT("pri*nting..."));
  pst_dc0->StartPage();
  width2 = backup_dc_bitmap2->GetWidth();
  height2 = backup_dc_bitmap2->GetHeight();

// Draw on the device context:
// pst_dc0->Blit(0, 0, width2, height2, backup_dc, 0, 0, wxCOPY);
  pst_x0 = 200;
  pst_y0 = 200;
  pst_width0 = 5000;
  pst_height0 = (double)(pst_width0  * height2) / (double)width2;
  pst_dc0->StretchBlit(pst_x0, pst_y0, pst_width0, pst_height0, backup_dc,
                       0, 0, width2, height2, wxCOPY, false, 0, 0);
  pst_dc0->EndPage();
  pst_dc0->EndDoc();
 }

return(0);
}
/************************************************************************
* Redraw the curves to the backup postscript dc
* and save it to a postscript file (GSEG)
************************************************************************/
int JLP_GDev_wxWID::PstCopyOfDisplayFromBackupForGseg(char *pst_filename)
{

// Open file and backup postscriptdc (in "jlp_gdev_wxwid_gseg.cpp")
  OpenBackupPostscriptDC(pst_filename);

// Draw to postscriptdc (in "jlp_gdev_wxwid_gseg.cpp")
  RedrawToBackupDC(7002);

// Close file and backup postscriptdc (in "jlp_gdev_wxwid_gseg.cpp")
  CloseBackupPostscriptDC();

return(0);
}
