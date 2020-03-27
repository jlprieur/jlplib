/******************************************************************************
* Name:        jlp_gdev_wxwid_canvas.cpp (JLP_GDev_wxWID class)
*
* Purpose:     Handling menu and events
*              that are defined in "jlp_gdev_wxwid_menu.cpp"
*
* Author:      JLP
* Version:     12/02/2017
******************************************************************************/
#include <stdlib.h>   // exit()
#include "jlp_gdev_wxwid.h"
#include "jlp_wx_gdproc.h"         // JLP_wx_iGDProc class
#include "jlp_wxgdev_popup.h"       // For JLP_wxGDev_Popup
#include "jlp_wxgdev_labels.h"      // For JLP_wxGDevLabels
#include "jlp_wxgdev_shapes.h"      // For JLP_wxGDevShapes
#include "jlp_wx_ipanel.h"          // JLP_wxImagePanel class
#include "jlp_wx_scrolled1.h"    // JLP_wxScrolledImage

/*
int  LoadNewCImage1(JLP_wxImage1 *image1);
void ApplyCurrentSettingsToCImage1();
int  RestoreOriginalCImage1();
int  ReadCImage1Data(double *array, int nx, int ny);
int  UpdateCImage1Values(double *array, int nx, int ny,
                         const bool reset_ITT_to_MinMax);
void ConvDevToUserForImages(long ix, long iy, double *x1, double *y1,
                            int *in_frame);
void ConvUserToDevForImages(double x1,  double y1, int *ix, int *iy,
                            int *in_frame);
void InitBackupDC(int width, int height);
void OnPaint( wxPaintEvent &WXUNUSED(event) );
void OnScrollPaint( wxPaintEvent &WXUNUSED(event) );
void RedrawToBackupDC(const int calling_routine_nber);
void RedrawImageToBackupDC();
*/
/*************************************************************
* Load a new image
*************************************************************/
int JLP_GDev_wxWID::LoadNewCImage1(JLP_wxImage1 *image1)
{

c_image1 = image1;

// Apply current settings (LUT, ITT, etc) to c_image1
ApplyCurrentSettingsToCImage1();

// Initialize backup DC with the right size:
MySetSize();

// Starting with automatic thresholds from box:
wxgdev_settings1.InternalProcessingMode = 0;

// Apply settings and update popup menu:
if(m_popup_menu1 != NULL)
    m_popup_menu1->UpdatePopupMenu_InternalProcessingMode();

RedrawToBackupDC(1001);

return(0);
}
/************************************************************************
* Apply current settings (LUT, ITT, etc) for image display
*
************************************************************************/
void JLP_GDev_wxWID::ApplyCurrentSettingsToCImage1()
{
int status, max_lut_level;

if(c_image1 == NULL) return;

// Apply previous settings for ITT:
 GDevSetITT_linear(wxgdev_settings1.itt_is_linear);

// Apply settings for LUT:
  GDevSetLUT(wxgdev_settings1.lut_type);
  max_lut_level = GetMaxLevelForLUT();
  if(wxgdev_settings1.lut_reversed) c_image1->ReverseLUT(max_lut_level);

// Apply settings for ITT thresholds type:
   status = GDevSet_ITT_Thresh(wxgdev_settings1.itt_type,
                               wxgdev_settings1.low_itt_thresh,
                               wxgdev_settings1.up_itt_thresh,
                               wxgdev_settings1.itt_x1_box,
                               wxgdev_settings1.itt_y1_box,
                               wxgdev_settings1.itt_x2_box,
                               wxgdev_settings1.itt_y2_box);
// Set image threshold to MinMax otherwise (thus ignoring wxgdev_settings1):
   if(status != 0) {
     wxgdev_settings1.itt_type = wxT("MinMax");
    }

// Then retrieve good values for the next time:
//     wxgdev_settings1.low_itt_thresh = c_image1->Get_Low_Threshold();
//     wxgdev_settings1.up_itt_thresh = c_image1->Get_Up_Threshold();

return;
}
/************************************************************************
* Restore original image
************************************************************************/
int JLP_GDev_wxWID::RestoreOriginalCImage1()
{

if(c_image1 == NULL) return(-1);

// Restore original data:
c_image1->RestoreOriginalImage();

// Update image with current LUT and ITT selection:
 c_image1->Update_with_LUT_and_ITT(MaxColorLevelForLUT);

// Update backup bitmap:
RedrawToBackupDC(1002);

return(0);
}
/************************************************************************
* Get access to c_image1 data
* i.e. read c_image1 data
************************************************************************/
int JLP_GDev_wxWID::ReadCImage1Data(double *array, int nx, int ny)
{
int i, nx1, ny1, status;
double *data;

if(c_image1 == NULL) return(-1);

status = c_image1->GetDoubleArray(&data, &nx1, &ny1);

if(status == 0) {
  if((nx != nx1) || (ny != ny1)) {
    fprintf(stderr,
    "ReadCImage1Data/Fatal error: inconsistent size/nx=%d ny=%d nx1=%d ny1=%d\n",
    nx, ny, nx1, ny1);
  exit(-2);
  }

  for(i = 0; i < nx *ny; i++) array[i] = data[i];
  delete[] data;
  }

return(0);
}
/*************************************************************
* Update the values of current image
* i.e. write new data onto c_image1
*
*************************************************************/
int  JLP_GDev_wxWID::UpdateCImage1Values(double *array, int nx, int ny,
                                        const bool reset_ITT_to_MinMax)
{
int nx1, ny1;
wxString buffer;

if(c_image1 == NULL) {
  buffer.Printf(wxT("UpdateCImage1Values/Fatal error: c_image1 = NULL\n"));
  wxMessageBox(buffer, wxT("JLP_GDev_wxWID/UpdateCImage1Values"),
               wxOK | wxICON_ERROR);
  return(-1);
}

nx1 = c_image1->Get_nx1();
ny1 = c_image1->Get_ny1();

if(nx != nx1 || ny != ny1) {
  buffer.Printf(wxT("UpdateCImage1Values/Fatal error: inconsistent size/nx=%d ny=%d nx1=%d ny1=%d\n"),
         nx, ny, nx1, ny1);
  wxMessageBox(buffer, wxT("JLP_GDev_wxWID/UpdateCImage1Values"),
               wxOK | wxICON_ERROR);
  return(-1);
  }

c_image1->PutDoubleArray(array, nx, ny);

// When called by the parent, for filter computation, reset_ITT_to_MinMax=true
// When called by jlp_wx_ipanel, for image update, reset_ITT_to_MinMax=false
// Also called by jlp_wx_ipanel, by Erase() with reset_ITT_to_MinMax=false
if(reset_ITT_to_MinMax) {
  wxgdev_settings1.itt_type = wxT("MinMax");
  }

// Apply current settings (LUT, ITT, etc) to c_image1
ApplyCurrentSettingsToCImage1();

RedrawToBackupDC(1003);

return(0);
}
/*************************************************************
* ConvDevToUserForImages
* to convert device coordinates to user coordinates
*
* Origin of device coordinate is on top left:
* whereas I want to display the images with origin on bottom left:
* Hence y = ny1 -1 - y
* JLP2014: I checked that I really get 64.0,64.0 at the center
* of the central pixel in a 128x128 frame.
*
* INPUT:
*  dev_x0, dev_y0: device coordinates
*
* OUTPUT:
*  user_x1, user_y1: user coordinates
*  in_frame: 1 if user_x1 and user_y1 are in the range [0, nx1[ or [0, ny1[
*            0 otherwise
*
*************************************************************/
void JLP_GDev_wxWID::ConvDevToUserForImages(double dev_x0, double dev_y0,
                                            double *user_x1, double *user_y1,
                                            int *in_frame)
{
int ix, iy, nx1, ny1;
int gamma1, gamma_d, iwx, iwy;

*in_frame = 0;
*user_x1 = 0.;
*user_y1 = 0.;
ix = NINT(dev_x0 + 0.5);
iy = NINT(dev_y0 + 0.5);

if(c_image1 == NULL || m_scrolled_window == NULL) return;

// JLP2009: I checked that I really get 64.0,64.0 at the center
// of the central pixel in a 128x128 frame.
   c_image1->Get_gamma(&gamma1, &gamma_d);
   nx1 = c_image1->Get_nx1();
   ny1 = c_image1->Get_ny1();

// since: iwx = NINT((user_x1 + 0.5) * gamma_d);
// since: iwy = NINT((ny1 - 0.5 - user_y1) * gamma_d);
   m_scrolled_window->CalcUnscrolledPosition(ix, iy, &iwx, &iwy );
   if(gamma_d > 1) {
     *user_x1 = -0.5 + ((double)iwx / (double)gamma_d);
     *user_y1 = ny1 - 0.5 - (double)iwy / (double)gamma_d ;
   } else if(gamma1 > 1) {
     *user_x1 = -0.5 + ((double)iwx * (double)gamma1);
     *user_y1 = ny1 - 0.5 - (double)iwy * (double)gamma1 ;
   } else {
     *user_x1 = -0.5 + (double)iwx;
     *user_y1 = ny1 - 0.5 - (double)iwy;
   }
// user_x1, user_y1 are the user coordinates
  if((*user_x1 >= 0 && *user_x1 < nx1)
     && (*user_y1 >= 0 && *user_y1 < ny1)) {
       *in_frame = 1;
     } else {
      *in_frame = 0;
     }
/* DEBUG:
   printf("ConvDevToUserForImages: x=%d y=%d User coord: x1=%f y1=%f\n",
               ix, iy, *user_x1, *user_y1);
*/
return;
}
/*************************************************************
* ConvUserToDevForImages
* to convert user coordinates to device coordinates
*
* INPUT:
*  x1, y1: user coordinates
*
* OUTPUT:
*  dev_x0, dev_y0: device coordinates
*  in_frame: 1 if (ix, iy) is visible in the window
*            0 otherwise
*
*************************************************************/
void JLP_GDev_wxWID::ConvUserToDevForImages(double x1,  double y1,
                                            double *dev_x0, double *dev_y0,
                                            int *in_frame)
{
int ny1, ixsize, iysize, ix, iy;
int gamma1, gamma_d, iwx, iwy;


// Initialization of the output parameters:
  *dev_x0 = 0; *dev_y0 = 0; *in_frame = 0;

if(c_image1 == NULL || m_scrolled_window == NULL) return;

  ny1 = c_image1->Get_ny1();
// Origin of device coordinate is on top left:
// whereas I want to display the images with origin on bottom left:
// Conversion:
   c_image1->Get_gamma(&gamma1, &gamma_d);
   if(gamma_d > 1) {
     iwx = NINT((x1 + 0.5) * (double)gamma_d);
     iwy = NINT(((double)ny1 - 0.5 - y1) * (double)gamma_d);
   } else if(gamma1 > 1) {
     iwx = NINT((x1 + 0.5) / (double)gamma1);
     iwy = NINT(((double)ny1 - 0.5 - y1) / (double)gamma1);
   } else {
     iwx = NINT(x1 + 0.5);
     iwy = NINT((double)ny1 - 0.5 - y1);
   }
   m_scrolled_window->CalcScrolledPosition(iwx, iwy, &ix, &iy);

// Check if point is visible:
  GetClientSize(&ixsize, &iysize);
  if(ix < 0 || ix >= ixsize || iy < 0 || iy >= iysize)
   *in_frame = 0;
  else
   *in_frame = 1;
/* DEBUG:
printf("ConvUserToDevForImages/x1=%.2f y1=%.2f ix=%d iy=%d ixsize=%d iysize=%d\n",
        x1, y1, ix, iy, ixsize, iysize);
*/
  *dev_x0 = (double)ix;
  *dev_y0 = (double)iy;

return;
}
/*************************************************************
* Create a Device Context to be able to write on it outside
* of Paint events (and also inside of Paint events...)
* (Called for ReSize events and each time the size of the canvas is modified)
*
* INPUT:
*  width20 : bitmap_width
*  height20 : bitmap_height
*************************************************************/
void JLP_GDev_wxWID::InitBackupDC(int width20, int height20)
{
int x0, y0;
wxString buffer;

if(width20 <= 0 || height20 <= 0) {
  fprintf(stderr, "InitBackupDC/Fatal error: width=%d height=%d\n",
          width20, height20);
  buffer.Printf(wxT("InitBackupDC/Fatal error: width=%d height=%d"),
                width20, height20);
  wxMessageBox(buffer, wxT("JLP_cDev_wxWID"), wxOK | wxICON_ERROR);
  exit(-1);
  }

// backup Device Context:
// Create a new bitmap by copying the current image bitmap2
// WARNING: this bitmap will be then attached to backup_dc...
  if(backup_dc_bitmap2 != NULL) delete backup_dc_bitmap2;

// Create bitmap with no data
/* DEBUG
  printf("InitBackupDC/ width=%d, height=%d\n", width20, height20);
*/
  backup_dc_bitmap2 = new wxBitmap(width20, height20);
  m_bitmap_width = width20;
  m_bitmap_height = height20;

  if(backup_dc == NULL) {
   fprintf(stderr, "InitBackupDC/Fatal error: backup_dc is NULL \n");
   exit(-1);
   }

// Assign this bitmap to the memory Device Context:
  backup_dc->SelectObject(*backup_dc_bitmap2);

// Basic setup of the backup device:
  backup_dc->Clear();
//  backup_dc->SetPen( *wxBLACK_PEN);
//  backup_dc->SetBackground( *wxGREY_BRUSH);
//  backup_dc->SetBackground( *wxWHITE_BRUSH);

// Update GSEG graphic window size:
  if(jlp_gseg_wxwid1 != NULL) {
   x0 = 0;
   y0 = 0;
   jlp_gseg_wxwid1->GSEG_SetWindowLimits(x0, m_bitmap_width,
                                          y0, m_bitmap_height);
   if(jlp_gsegraf1 != NULL) jlp_gsegraf1->GSEG_InitializePlotSize();
  }

// Update fontsi with private variables:
  SetFontToBackupDC(font_name1, font_size1);

#if 0
// draw a green circle
  backup_dc->SetBrush(*wxGREEN_BRUSH); // green filling
  backup_dc->SetPen( wxPen( wxColor(255,0,0), 5 ) ); // 5-pixels-thick red outline
  backup_dc->DrawCircle( wxPoint(200,100), 25 /* radius */ );
#endif

// Initialise the plot (necessary to be able to refresh the display !)
 RedrawToBackupDC(1301);

return;
}
/*************************************************************************
* Set Client Size to width, and height
* Create new backup_dc_bitmap and update the size of the scrolled window
*
**************************************************************************/
int JLP_GDev_wxWID::SetNewBitmapSize(int width, int height)
{
wxString buffer;

if(initialized != 1234) return(-1);

 if((width <= 0) || (height <= 0)) {
 fprintf(stderr, "JLP_GDev_wxWID::SetNewBitmapSize/Fatal error: width=%d height=%d\n",
         width, height);
 buffer.Printf(wxT("SetNewBitmapSize/Fatal error: width=%d height=%d"),
         width, height);
 wxMessageBox(buffer, wxT("JLP_cDev_wxWID"), wxOK | wxICON_ERROR);
 return(-1);
 }

// Initialize backup device context (used as a buffer for a faster display):
// Routine defined in application program:
  InitBackupDC(width, height);

// Gsegraf/update size parameters:
  if(jlp_gsegraf1 != NULL) jlp_gsegraf1->GSEG_InitializePlotSize();

// jlp_splot/Update user/device conversion parameters (in Jgc0) :
  JLP_GDev::SetNewDeviceSize(width, height);

// JLP2017: do not call RedrawToBackupDC here, to avoid too many calls

return(0);
}
/*************************************************************
* Update the drawing of the image to the backup_dc
*************************************************************/
void JLP_GDev_wxWID::RedrawScrolledImageToBackupDC()
{
wxBitmap *image1_bitmap;
wxString err_msg;
int scroll_x, scroll_y;

if((m_scrolled_window == NULL) || (initialized != 1234)) return;

if((backup_dc->IsOk() == false) ||  (c_image1 == NULL)) return;

image1_bitmap = c_image1->GetBitmap2();

// Draw image1 bitmap onto backup_dc at 0,0 location
  backup_dc->DrawBitmap(*image1_bitmap, 0, 0);

// Get origin in case of scrolling window:
  m_scrolled_window->CalcUnscrolledPosition(0, 0, &scroll_x, &scroll_y);

// Draw labels if needed:

/****  DEBUG:
//  wxgdev_settings1.pen_colour = wxColour(200, 100, 50);
  printf("pen_color=%d %d %d\n",
        wxgdev_settings1.pen_colour.Red(),
        wxgdev_settings1.pen_colour.Green(),
        wxgdev_settings1.pen_colour.Blue());
****/
  if(m_wxlabels1 != NULL)
     m_wxlabels1->DrawAllImageLabelsToBackupDC(this, backup_dc,
                                               wxgdev_settings1.pen_colour,
                                               scroll_x, scroll_y);

// Draw shapes if needed:
  if(m_wxshapes1 != NULL)
     m_wxshapes1->DrawShapesToBackupDC(this, backup_dc,
                                       wxgdev_settings1.pen_colour,
                                       scroll_x, scroll_y);

return;
}
/*************************************************************
* Erase all the drawings inside the window
*
*************************************************************/
int JLP_GDev_wxWID::gdev_erase()
{

// Check that curve mode is active:
 if(initialized != 1234) return(-1);

 backup_dc->Clear();
// backup_dc->SetPen( *wxBLACK_PEN);
// backup_dc->SetBackground( *wxGREY_BRUSH);

return(0);
}
/************************************************************************
* To enlarge the window at the maximum size:
* JLP 2015: Should not be done at this level but on the jlp_wx_GraphicPanel class
* SEE ALSO : void JLP_wxGraphicPanel::OnResize( wxSizeEvent &event )
* Not done with SetNewSize()...
************************************************************************/
void JLP_GDev_wxWID::OnResize( wxSizeEvent &event )
{
int client_width0, client_height0;

if(initialized == 1234) {

// Good size without borders
GetClientSize(&client_width0, &client_height0);
/*
printf("JLP_GDev_wxWID::OnResize: ClientSize=%d %d\n",
        client_width0, client_height0);
*/

ResizePlot1(client_width0, client_height0);

}

// Skip this event (to avoid infinite processing of the same event):
 event.Skip();

return;
}
/************************************************************************
* To enlarge the window at the maximum size:
* Also called from the jlp_wx_GraphicPanel class
* (but both GetSize() or GetClientSize() give a bad size (too large)
*  so I correct this size and update plot parameters)
************************************************************************/
void JLP_GDev_wxWID::ResizePlot1(const int width0, const int height0)
{
int width2, height2;

// Step 1: resize the drawing panel with the large size and update the drawings
 SetNewBitmapSize(width0, height0);

// Get it back after modifications if needed:
 Get_ClientSize(&width2, &height2);
/*
printf("JLP_GDev_wxWID::Resize1/From GetClientSize/width,height = %d,%d\n",
        width2, height2);
*/

// Step 2: resize the drawing panel with the smaller size and update the drawings
 SetNewBitmapSize(width2, height2);

// JLP2017: do not call DrawToBackupDC here, to avoid too many calls
// RedrawToBackupDC(1301);

return;
}
/*************************************************************
* Draw help information
*************************************************************/
void JLP_GDev_wxWID::DrawHelpInfo(wxDC *dc1, wxString help_text)
{
#ifdef NEW_FONTS
int PointSize;
wxFont def_Font = wxSystemSettings::GetFont(wxSYS_DEFAULT_GUI_FONT);
wxFont *my_Font;
#endif

if(help_text.IsEmpty() || !dc1->IsOk())return;

// Select nicer fonts than default fonts:
#ifdef NEW_FONTS
PointSize = def_Font.GetPointSize();
// wxSWISS_FONT allows rotation with DrawRotatedText
my_Font = new wxFont(PointSize, wxFONTFAMILY_SWISS,
                     wxFONTSTYLE_NORMAL, wxFONTWEIGHT_BOLD,
                     false, wxEmptyString, wxFONTENCODING_DEFAULT);
dc1->SetFont(*my_Font);
#endif

// Draw help information about processing mode on top of the frame
dc1->DrawText(help_text, 152, 0);

// Go back to default fonts:
#ifdef NEW_FONTS
dc1->SetFont(def_Font);
delete my_Font;
#endif

return;
}
/*************************************************************
* Draw pixel information (coordinates and pixel value)
*************************************************************/
void JLP_GDev_wxWID::DisplayCoordinatesForCurves(wxString coordinates_text)
{
wxString str0;

if(initialized != 1234) return;

if(m_StaticCoord != NULL)
    m_StaticCoord->SetLabel(coordinates_text);

if((m_StaticHelp != NULL) && (active_gdproc != NULL)) {
   str0 = active_gdproc->Get_HelpText();
   m_StaticHelp->SetLabel(str0);
  }

return;
}
/*************************************************************
* Draw pixel information (coordinates and pixel value)
*************************************************************/
void JLP_GDev_wxWID::DisplayCoordinatesForImages(wxDC *dc1,
                                                 wxString coordinates_text)
{
int iheight;

if((initialized != 1234) && (active_gdproc == NULL)) return;

// Draw pixel information on top of the frame

//****************************************************************
if((m_StaticHelp == NULL) || (m_StaticCoord == NULL)) {
// First erase:
  dc1->SetPen(*wxBLACK_PEN);
  dc1->SetBrush(*wxWHITE_BRUSH);
// dc1->DrawRectangle(0, 0, 150, 16);
#ifdef _WIN32
  iheight = 32; // Windows: height of white rectangle
#else
  iheight = 16; // Linux: height of white rectangle
#endif

  dc1->DrawRectangle(0, 0, nx2, iheight);

// Then display string:
  dc1->DrawText(coordinates_text, 2, 0);

// Draw help information about processing mode on top of the frame
  dc1->DrawText(active_gdproc->Get_HelpText(), 152, 0);
//****************************************************************
} else {
 if(m_StaticCoord != NULL) {
  m_StaticCoord->SetLabel(coordinates_text);
  }
 if(m_StaticHelp != NULL) {
  m_StaticHelp->SetLabel(active_gdproc->Get_HelpText());
  }
}

return;
}
/*************************************************************
* To update the display when Paint events are detected
*************************************************************/
void JLP_GDev_wxWID::OnPaint( wxPaintEvent &event )
{
// Construct a Device Context on which graphics and text
// can be drawn.
// wxPaintDC is a constructor and pass a pointer to the window
// on which you wish to paint:
wxDC *dc0;
int  width0, height0;

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
 if(Jgc0.gdev_graphic_type != 3) {
    dc0 = new wxPaintDC(this);
    dc0->GetSize(&width0, &height0);
    wxGDev_RenderForCurves(dc0, width0, height0);
    delete dc0;
  }

// Pass on Paint event to scrolled window
// when required (i.e. when Jgc0.gdev_graphic_type==3):
// (Solution needed for MSWINDOWS ...)
event.Skip();
return;
}
/*************************************************************
* To update the display with any kind of graphic context
*************************************************************/
void JLP_GDev_wxWID::wxGDev_RenderForCurves( wxDC *dc1, const int width1,
                                             const int height1)
{
int width2, height2;
int xoffset, yoffset;

if(initialized != 1234) return;

// JLP2016: problem to fill in the frame:
// copy from backup_dc(0,0) to screen (0,0)
if(backup_dc_bitmap2 != NULL && backup_dc != NULL) {
// xdest, ydest, width (to be copied from source),
// height (to be copied from source), source, xsrc, ysrc
  width2 = backup_dc_bitmap2->GetWidth();
  height2 = backup_dc_bitmap2->GetHeight();
  xoffset = MAXI(0, width2 - width1);
  yoffset = MAXI(0, height2 - height1);
  dc1->Blit(0, 0, width2, height2, backup_dc, xoffset, yoffset, wxCOPY);
  } else {
  fprintf(stderr, "wxGDev_RenderForCurves/Not ready yet: backup_dc still null!\n");
  }

return;
}
/*************************************************************
* To update the display with any kind of graphic context
* (Still needed by jlp_gdev_wxwid_process for selecting rectangles...)
*************************************************************/
void JLP_GDev_wxWID::wxGDev_RenderForScrolledImages( wxDC *scroll_dc0 )
{

if((backup_dc_bitmap2 == NULL) || (backup_dc == NULL)
   || (scroll_dc0 == NULL)) {
  wxMessageBox(wxT("Error/Not ready yet: backup_dc still null!"),
               wxT("wxGDev_RenderForImages"), wxICON_ERROR);
  return;
  }

// Clear screen first
  scroll_dc0->Clear();

/*** DEBUG:
  scroll_dc0->SetPen( wxPen( wxColor(255,0,0), 5 ) ); // 5-pixels-thick red outline
  scroll_dc0->DrawLine(0, 0, 2000, 2000);
***/

// copy from backup_dc(0,0) to screen (0,0)
  scroll_dc0->Blit(0, 0, backup_dc_bitmap2->GetWidth(),
                   backup_dc_bitmap2->GetHeight(),
                   backup_dc, 0, 0, wxCOPY);

// Draw help information about processing mode on top of the frame
  if(active_gdproc != NULL) {
    scroll_dc0->DrawText(active_gdproc->Get_HelpText(), 152, 0);
   }

return;
}
/****************************************************************************
* Redo the drawings from scratch outside of Paint events
* Called when resizing the panel
*
* INPUT:
*  calling_routine_nber : nber used to trace the calling routine (for debug)
****************************************************************************/
void JLP_GDev_wxWID::RedrawToBackupDC(const int calling_routine_nber)
{
int scroll_x, scroll_y;
wxString err_msg;

if(initialized != 1234 || backup_dc == NULL) return;

if(!backup_dc->IsOk()) return;

// wxDC:Clear
 backup_dc->Clear();

/* Pointers defined in brush.h
wxBLUE_BRUSH, wxGREEN_BRUSH, wxWHITE_BRUSH, wxBLACK_BRUSH, wxGREY_BRUSH,
wxMEDIUM_GREY_BRUSH, wxLIGHT_GREY_BRUSH, wxTRANSPARENT_BRUSH, wxCYAN_BRUSH,
wxRED_BRUSH,
  backup_dc->SetBackground( *wxWHITE_BRUSH);
*/

/* pen.h
wxRED_PEN, wxCYAN_PEN, wxGREEN_PEN, wxBLACK_PEN, wxWHITE_PEN,
wxTRANSPARENT_PEN, wxBLACK_DASHED_PEN, wxGREY_PEN, wxMEDIUM_GREY_PEN,
wxLIGHT_GREY_PEN,
*/

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
  if(Jgc0.gdev_graphic_type == 3) {
     RedrawScrolledImageToBackupDC();

  } else if(Jgc0.gdev_graphic_type >= 4) {
// Plot curves/images with GSEG routines :
     PlotAllCurves_gseg();
  } else {
// For jlp_splot curves (or images ?) 

#ifdef DEBUG
printf("RedrawToBakucpDC/Calling routine : %d\n", calling_routine_nber);
#endif

// Plot all the curves with current values of wxgdev_settings1
// (NB: this is a gdev routine in "jlp_splot/jlp_gdev_curves.cpp")
     PlotAllCurves_splot();

// No scroll window for curves, hence scrolling set to zero:
     scroll_x = 0;
     scroll_y = 0;

/****** DEBUG
  printf("RedrawToBackupDC/DEBUG 1: color=%d %d %d\n",
        wxgdev_settings1.pen_colour.Red(),
        wxgdev_settings1.pen_colour.Green(),
        wxgdev_settings1.pen_colour.Blue());
*********/
// Draw labels if needed:
     if(m_wxlabels1 != NULL) {
        m_wxlabels1->DrawAllImageLabelsToBackupDC(this, backup_dc,
                                                  wxgdev_settings1.pen_colour,
                                                  scroll_x, scroll_y);
     }

// Draw shapes if needed:
     if(m_wxshapes1 != NULL) {
        m_wxshapes1->DrawShapesToBackupDC(this, backup_dc,
                                          wxgdev_settings1.pen_colour,
                                          scroll_x, scroll_y);
     }

// Redo drawing from metafile:
#if USE_METAFILE
     if(MetaFileIsOpened) ReadAndExecuteMetaFile();
#endif

 } // case when Jgc0.gdev_graphic_type < 3

// Refresh screen (to avoid partial update of the screen)
 wxGdev_Refresh();

// Used for DEBUG only
#if 0
// wxDC:DrawRectangle
  backup_dc->DrawRectangle( 20, 70, 100, 125 );
// Fill rectangle in red:
  backup_dc->SetBrush( *wxRED_BRUSH);
  backup_dc->DrawRectangle( 90, 80, 120, 225 );
// Draw in cyan:
  backup_dc->SetPen( *wxCYAN_PEN);
  backup_dc->DrawLine( 100, 100, 220, 325 );
  backup_dc->DrawText( _T("JLP  wxwidgets Wplot1"), 5, 5 );
#endif

return;
}
/*************************************************************
* Plot the curves to GSEG device
*************************************************************/
void JLP_GDev_wxWID::PlotAllCurves_gseg()
{
   if(jlp_gsegraf1 == NULL) {
     fprintf(stderr, "JLP_GDev_wxWID::PlotAllCurves_gseg/ Error: jlp_gsegraf1 is NULL");
     return;
     }

// Takes time in case of 3D plots:
// Busy cursor: NOT WORKING
//  GetParent()->SetCursor(*wxHOURGLASS_CURSOR);
//  SetCursor(*wxHOURGLASS_CURSOR);
wxBeginBusyCursor();

// Draw graph
   jlp_gsegraf1->GSEG_DrawGraph();

// Update extra labels if any
   jlp_gsegraf1->GSEG_DrawExtraLabels();

// End of busy cursor: NOT WORKING
//  GetParent()->SetCursor(wxNullCursor);
//  SetCursor(wxNullCursor);
wxEndBusyCursor();
return;
}
