/**************************************************************************
* jlp_gdev_wxwid_utils.cpp
*
* JLP_GDev_wxWID class (for Curves/Images)
* Derived from virtual class JLP_GDev defined in "jlp_gdev.h"
* Basic graphic functions:
*
* JLP
* Version 22/06/2015
**************************************************************************/
/* To define "std" as the standard prefix (e.g. before printf, scanf, ...) */
using namespace std;

#include "jlp_gdev_wxwid.h"
#include "jlp_wx_gpanel.h"
#include "jlp_wx_ipanel.h"

#include "jlp_gdev_idv.h"              // GDev_free()
#include "jlp_macros.h"           // NINT

/************************************************************************
* Cursor function:
************************************************************************/
int JLP_GDev_wxWID::CursorGetCoord(int& x, int& y, char* cursor_type,
                                   int& pressed_button)
{
 fprintf(stderr,"Sorry cursor function not available here\n");
 return(-1);
}
/************************************************************************
* To display the position of the cursor:
************************************************************************/
int JLP_GDev_wxWID::CreateCursorPosition(int ix, int iy,
                                         int width, int height)
{
/* JLPDEC2008
  return CreateStaticButton(Xgc0.CursorPosition," ",ix, iy,width,height);
*/
return 0;
}
/************************************************************************
* Get idev number for plotting (for external routines) to this panel:
************************************************************************/
int JLP_GDev_wxWID::Get_idev(int& idev)
{
int status = -1;

idev = -1;

  if(initialized == 1234) {
    idev = Jgc0.dev_idv;
    status = 0;
    }

return(status);
}
/*********************************************************************
* Get the client size
* (needed for obtaining the right coordinates with the cursor)
**********************************************************************/
void JLP_GDev_wxWID::Get_ClientSize(int *width0, int *height0)
{
// Good size without borders
  GetClientSize(&client_width1, &client_height1);
// Copy to output parameters:
  *width0 = client_width1;
  *height0 = client_height1;
return;
}
/************************************************************************
* Draw a rectangle (user coordinates)
* Used by PiscoSpeck2 to display the ROI (color=white, penwidth=2)
*************************************************************************/
int JLP_GDev_wxWID::DrawRectangle_UC(double user_xstart0, double user_ystart0,
                                     double user_xend0, double user_yend0,
                                     const int rr, const int gg, const int bb,
                                     const int pen_width, const int filled)
{
wxColour wxC;
wxBrush old_brush;
wxPen old_pen;
double dev_xstart1, dev_ystart1, dev_xend1, dev_yend1;
int in_frame;

  if(backup_dc == NULL) return(-1);

  old_pen = backup_dc->GetPen();
  old_brush = backup_dc->GetBrush();

  wxC = wxColour(rr, gg, bb, wxALPHA_OPAQUE);
  backup_dc->SetPen(wxPen(wxC, pen_width));
//  backup_dc->SetPen(wxPen(*wxWHITE, pen_width));
  if(!filled) backup_dc->SetBrush(*wxTRANSPARENT_BRUSH);
  else backup_dc->SetBrush(wxC);

  ConvUserToDev(user_xstart0, user_ystart0, &dev_xstart1, &dev_ystart1,
                &in_frame);

// JLP 2015: in_frame does'nt work
//  if(in_frame != 1) return(-2);
  ConvUserToDev(user_xend0, user_yend0, &dev_xend1, &dev_yend1, &in_frame);
// JLP 2015: in_frame does'nt work
//    if(in_frame != 1) return(-3);

  backup_dc->DrawLine(dev_xstart1, dev_ystart1, dev_xstart1, dev_yend1);
  backup_dc->DrawLine(dev_xstart1, dev_yend1, dev_xend1, dev_yend1);
  backup_dc->DrawLine(dev_xend1, dev_yend1, dev_xend1, dev_ystart1);
  backup_dc->DrawLine(dev_xend1, dev_ystart1, dev_xstart1, dev_ystart1);

backup_dc->SetBrush(old_brush);
backup_dc->SetPen(old_pen);

return(0);
}
/************************************************************************
* Pen colour
* (version wxColour)
************************************************************************/
void  JLP_GDev_wxWID::SetPenColour(const wxColour wxC)
{
  if(backup_dc == NULL) return;

  backup_dc->SetPen(wxPen(wxC));
  wxgdev_settings1.pen_colour = wxC;

return;
}
/************************************************************************
* Pen colour
* (version rr, gg, bb)
************************************************************************/
void  JLP_GDev_wxWID::SetPenColour(const int rr, const int gg, const int bb)
{
wxColour wxC;

  if(backup_dc == NULL) return;

  wxC = wxColour(rr, gg, bb, wxALPHA_OPAQUE);
  backup_dc->SetPen(wxPen(wxC));
  wxgdev_settings1.pen_colour = wxC;

return;
}
/************************************************************************
* Pen colour
* (version UINT32 rgba color)
************************************************************************/
void  JLP_GDev_wxWID::SetPenColour(const UINT32 color_rgba0)
{
wxColour wxC;

  if(backup_dc == NULL) return;

  jlp_convert_uint32_to_wxcolour(color_rgba0, &wxC);
  backup_dc->SetPen(wxPen(wxC));
  wxgdev_settings1.pen_colour = wxC;

return;
}
/************************************************************************
* Conversion from UINT32 rgba color to wxColour
************************************************************************/
int jlp_convert_uint32_to_wxcolour(const UINT32 color_rgba0, wxColour *wxC)
{
int rr, gg, bb, aa;
unsigned urr, ugg, ubb, uaa;

#ifdef WORDS_BIGENDIAN
  aa = ((color_rgba0 & 0xFF000000) >> 24) ;
  bb = ((color_rgba0 & 0x00FF0000) >> 16) ;
  gg = ((color_rgba0 & 0x0000FF00) >> 8) ;
  rr =  (color_rgba0 & 0x000000FF);
#else
  rr = ((color_rgba0 & 0xFF000000) >> 24) ;
  gg = ((color_rgba0 & 0x00FF0000) >> 16) ;
  bb = ((color_rgba0 & 0x0000FF00) >> 8) ;
  aa =  (color_rgba0 & 0x000000FF);
#endif

  urr = (unsigned)rr;
  ugg = (unsigned)gg;
  ubb = (unsigned)bb;
  uaa = (unsigned)aa;

  *wxC = wxColour(urr, ugg, ubb, uaa);

return(0);
}

/************************************************************************
* Get the image data
************************************************************************/
int JLP_GDev_wxWID::GetImageArray(double **array0, int *nx0, int *ny0)
{
int status = -1;
  if(c_image1 != NULL) {
    status = c_image1->GetDoubleArray(array0, nx0, ny0);
    }
return(status);
}
/*************************************************************
* Return the filter selection of the popup menu
*************************************************************/
int JLP_GDev_wxWID::GetFilterSelection() {
 return(wxgdev_settings1.filter);
}
/*************************************************************
* Settings from outside
* (see also OnChangeITT_... for interactive input)
*************************************************************/
void JLP_GDev_wxWID::GDevSetITT_type(wxString itt_type)
{
  wxgdev_settings1.itt_type = itt_type;
  if(c_image1 != NULL) c_image1->SetITT_type(itt_type);
}
void JLP_GDev_wxWID::GDevSetITT_linear(int is_linear)
{
  wxgdev_settings1.itt_is_linear = is_linear;
  if(c_image1 != NULL) c_image1->SetITT_linear(is_linear);
}
void JLP_GDev_wxWID::GDevSetLUT(char *lut_type)
{
  int max_lut_level;
  strcpy(wxgdev_settings1.lut_type, lut_type);
  max_lut_level = GetMaxLevelForLUT();
  if(c_image1 != NULL) c_image1->SetLUT(lut_type, max_lut_level);
}
void JLP_GDev_wxWID::GDevSetFilter(int filter_type)
{
  wxgdev_settings1.filter = filter_type;
}
/*************************************************************
* Settings from outside
* (see also OnChangeITT_... for interactive input)
*************************************************************/
int JLP_GDev_wxWID::GDWX_WriteToLogbook(wxString str1, bool save_to_file0)
{
int status = -1;

if(initialized == 1234) {
  if(m_parent_ipanel != NULL)
    status = m_parent_ipanel->wxIP_WriteToLogbook(str1, save_to_file0);
  else if(m_parent_gpanel != NULL)
    status = m_parent_gpanel->wxGP_WriteToLogbook(str1, save_to_file0);
  }

return(status);
}

