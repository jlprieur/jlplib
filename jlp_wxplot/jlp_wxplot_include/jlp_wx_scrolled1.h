/****************************************************************************
* jlp_wx_scroll1.h
*
* JLP
* Version 10/09/2017 
*****************************************************************************/
#ifndef _jlp_wx_scrolled1_h
#define _jlp_wx_scrolled1_h

#include "wx/wx.h"
#include "jlp_gdev_wxwid.h"
 
/****************************************************************************
*
*****************************************************************************/
class JLP_wxScrolled1 : public wxScrolledWindow
{

public:
  JLP_wxScrolled1(JLP_GDev_wxWID *parent_jlpgdev0, wxWindowID id);
  ~JLP_wxScrolled1() {return;};
  void OnPaint(wxPaintEvent &event);
  void OnDraw(wxDC& dc);

  void OnMotion(wxMouseEvent& event);
  void OnLeftDown(wxMouseEvent& event);
  void OnLeftUp(wxMouseEvent& event);
  void OnRightUp(wxMouseEvent& event);
  void OnScrollWindow(wxScrollWinEvent &event);

private:
  JLP_GDev_wxWID *m_jlp_gdev_wxwid;

DECLARE_EVENT_TABLE()
};

#endif
