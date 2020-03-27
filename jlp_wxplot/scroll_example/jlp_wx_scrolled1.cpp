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

// Catch scrolling events
  EVT_SCROLLWIN(JLP_wxScrolled1::OnScrollWindow)

END_EVENT_TABLE()

/****************************************************************************
* Constructor: 
*****************************************************************************/
JLP_wxScrolled1::JLP_wxScrolled1(wxWindow* parent, wxWindowID id,
                                 wxBitmap* bitmap0)
                                 : wxScrolledWindow(parent, id)
{
 
  bitmap_width1 = bitmap0->GetWidth();
  bitmap_height1 = bitmap0->GetHeight();
 
/* init scrolled area size, scrolling speed, etc. */
  SetScrollbars(1,1, bitmap_width1, bitmap_height1, 0, 0);
 
  bitmap1 = new wxBitmap(*bitmap0);
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
 
return;
}
/****************************************************************************
* Overloading OnDraw routine 
* (called by PAINT events for wxScrolledWindow class) 
*
*****************************************************************************/
void JLP_wxScrolled1::OnDraw(wxDC& dc)
{
/* render the image - in a real app, if your scrolled area is somewhat big, 
 you will want to draw only visible parts, not everything like below 
*/
  dc.DrawBitmap(*bitmap1, 0, 0, false);
 
// also check wxScrolledWindow::PrepareDC
return;
}
/****************************************************************************
*
*****************************************************************************/
void JLP_wxScrolled1::OnMotion(wxMouseEvent& event)
{
}
/****************************************************************************
*
*****************************************************************************/
void JLP_wxScrolled1::OnLeftUp(wxMouseEvent& event)
{
}
/****************************************************************************
*
*****************************************************************************/
void JLP_wxScrolled1::OnLeftDown(wxMouseEvent& event)
{
}
/****************************************************************************
*
*****************************************************************************/
void JLP_wxScrolled1::OnRightUp(wxMouseEvent& event)
{
}
/****************************************************************************
*
*****************************************************************************/
void JLP_wxScrolled1::OnScrollWindow(wxScrollWinEvent &event)
{
}
