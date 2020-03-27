/****************************************************************************
* jlp_wx_scroll1.h
*
* JLP
* Version 10/09/2017 
*****************************************************************************/
#ifndef _jlp_wx_scrolled1_h
#define _jlp_wx_scrolled1_h

#include "wx/wx.h"
 
/****************************************************************************
*
*****************************************************************************/
class JLP_wxScrolled1 : public wxScrolledWindow
{

public:
  JLP_wxScrolled1(wxWindow* parent, wxWindowID id, wxBitmap* bitmap0);
  ~JLP_wxScrolled1() {
    delete bitmap1;
  }
  void OnPaint(wxPaintEvent &event);
  void OnDraw(wxDC& dc);

  void OnMotion(wxMouseEvent& event);
  void OnLeftDown(wxMouseEvent& event);
  void OnLeftUp(wxMouseEvent& event);
  void OnRightUp(wxMouseEvent& event);
  void OnScrollWindow(wxScrollWinEvent &event);

private:
  wxBitmap* bitmap1;
  int bitmap_width1, bitmap_height1;

DECLARE_EVENT_TABLE()
};

#endif
