/****************************************************************************
* jlp_scrolled
*
* JLP
* Version 10/09/2017 
*****************************************************************************/
#include "wx/wx.h"
#include <iostream>
 
/****************************************************************************
*
*****************************************************************************/
class JLP_wxScrolledWindowImageComponent : public wxScrolledWindow
{

public:
  ScrolledImageComponent(wxWindow* parent, wxWindowID id, wxString image_path);
  ~ScrolledImageComponent() {
    delete bitmap;
  }
  void OnDraw(wxDC& dc);

private:
  wxBitmap* bitmap;
  int w,h;
};

/****************************************************************************
*
*****************************************************************************/
ScrolledImageComponent::ScrolledImageComponent(wxWindow* parent, wxWindowID id,
                                               wxString image_path)
                                               : wxScrolledWindow(parent, id)
{
  wxImage image(image_path);
  if(!image.IsOk())
    {
      wxMessageBox(wxT("There was an error loading the image"));
      return;
    }
 
  w = image.GetWidth();
  h = image.GetHeight();
 
/* init scrolled area size, scrolling speed, etc. */
  SetScrollbars(1,1, w, h, 0, 0);
 
  bitmap = new wxBitmap( image );
}

/****************************************************************************
*
*****************************************************************************/
void ScrolledImageComponent::OnDraw(wxDC& dc)
{
/* render the image - in a real app, if your scrolled area is somewhat big, 
 you will want to draw only visible parts, not everything like below 
*/
  dc.DrawBitmap(*bitmap, 0, 0, false);
 
// also check wxScrolledWindow::PrepareDC
return;
}
 
/****************************************************************************
*
*****************************************************************************/
class MyApp: public wxApp
{
  wxFrame *frame;

public:
 
  bool OnInit()
  {
    wxInitAllImageHandlers();
    wxBoxSizer* sizer = new wxBoxSizer(wxHORIZONTAL);
    frame = new wxFrame((wxFrame *)NULL, -1,  wxT("Scrolling an Image"), 
                         wxPoint(50,50), wxSize(650,650));
 
    ScrolledImageComponent* my_image = new ScrolledImageComponent(frame, 
                                                wxID_ANY, wxT("my_image.jpg") );
    sizer->Add(my_image, 1, wxALL | wxEXPAND, 120);
    frame->SetSizer(sizer);
    frame->Show();
    return true;
  } 
};
 
IMPLEMENT_APP(MyApp)
