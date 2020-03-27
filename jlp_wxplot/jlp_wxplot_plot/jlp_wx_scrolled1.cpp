/****************************************************************************
* jlp_wx_scrolled1.cpp
* class JLP_wxScrolled1
*
* JLP
* Version 10/09/2017 
*****************************************************************************/
#include "jlp_wx_scrolled1.h"
 
BEGIN_EVENT_TABLE(JLP_wxScrolled1, wxWindow)

// Catch mouse events
  EVT_MOTION(JLP_wxScrolled1::OnMotion)
  EVT_LEFT_UP(JLP_wxScrolled1::OnLeftUp)
  EVT_LEFT_DOWN(JLP_wxScrolled1::OnLeftDown)
  EVT_RIGHT_UP(JLP_wxScrolled1::OnRightUp)

  EVT_PAINT(JLP_wxScrolled1::OnPaint)

// JLP2017: SHOULD NOT Catch scrolling events for windows !!!
//  EVT_SCROLLWIN(JLP_wxScrolled1::OnScrollWindow)

END_EVENT_TABLE()

/****************************************************************************
* Constructor: 
*****************************************************************************/
JLP_wxScrolled1::JLP_wxScrolled1(JLP_GDev_wxWID *parent_jlpgdev0, wxWindowID id)
                           : wxScrolledWindow((wxWindow *)parent_jlpgdev0, id)
{
  m_jlp_gdev_wxwid = parent_jlpgdev0;
 
// Init scrolled area size, scrolling speed, etc. 
  m_jlp_gdev_wxwid->MySetSize();
 
}
/****************************************************************************
* Overloading OnPaint routine 
* (called by PAINT events ) 
*
*****************************************************************************/
void JLP_wxScrolled1::OnPaint(wxPaintEvent &event)
{
// Construct a Device Context on which graphics and text
// can be drawn.
// wxPaintDC is a constructor and pass a pointer to the window
// on which you wish to paint:
wxDC *dc0 = new wxPaintDC(this);

/*
void wxScrolledWindow::DoPrepareDC(wxDC& dc)
Call this function to prepare the device context for drawing a scrolled image.
It sets the device origin according to the current scroll position.
(Should be called when handling a paint event only)
*/
  DoPrepareDC(*dc0);

// Actual painting:
  OnDraw(*dc0);
 
event.Skip();
return;
}
/****************************************************************************
* Overloading OnDraw routine 
* (called by PAINT events for wxScrolledWindow class) 
*
*****************************************************************************/
void JLP_wxScrolled1::OnDraw(wxDC& dc)
{
wxBitmap* bitmap2;
int bitmap2_width, bitmap2_height;
wxMemoryDC *backup_dc;
wxString str0;
int status, ix0, iy0;

/* render the image - in a real app, if your scrolled area is somewhat big, 
 you will want to draw only visible parts, not everything like below 
*/
  bitmap2 = m_jlp_gdev_wxwid->backup_dc_bitmap2_ptr();
  bitmap2_width = bitmap2->GetWidth();
  bitmap2_height = bitmap2->GetHeight();

// if static bitmap:
//  dc.DrawBitmap(*bitmap2, 0, 0, false);
 
  backup_dc = m_jlp_gdev_wxwid->backup_dc;
 
// copy from backup_dc(0,0) to screen (0,0)
  dc.Blit(0, 0, bitmap2_width, bitmap2_height, backup_dc, 0, 0, wxCOPY);

// Draw help information about processing mode on top of the frame
  status = m_jlp_gdev_wxwid->GetActiveExternalProcessingHelpText(&str0, 
                                                                 &ix0, &iy0);
  if(status == 0) dc.DrawText(str0, ix0, iy0);

return;
}
/****************************************************************************
*
*****************************************************************************/
void JLP_wxScrolled1::OnMotion(wxMouseEvent& event)
{
  m_jlp_gdev_wxwid->OnMotion(event);
  event.Skip();
}
/****************************************************************************
*
*****************************************************************************/
void JLP_wxScrolled1::OnLeftUp(wxMouseEvent& event)
{
  m_jlp_gdev_wxwid->OnLeftUp(event);
  event.Skip();
}
/****************************************************************************
*
*****************************************************************************/
void JLP_wxScrolled1::OnLeftDown(wxMouseEvent& event)
{
  m_jlp_gdev_wxwid->OnLeftDown(event);
  event.Skip();
}
/****************************************************************************
*
*****************************************************************************/
void JLP_wxScrolled1::OnRightUp(wxMouseEvent& event)
{
  m_jlp_gdev_wxwid->OnRightUp(event);
  event.Skip();
}
