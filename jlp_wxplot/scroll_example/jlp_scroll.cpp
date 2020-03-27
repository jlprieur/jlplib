/****************************************************************************
* jlp_scroll.cpp
*
* JLP
* Version 10/09/2017 
*****************************************************************************/
#include <iostream>
#include "jlp_wx_scrolled1.h"
 
/****************************************************************************
*
*****************************************************************************/
class MyApp: public wxApp
{
  wxFrame *frame;

public:
 
  bool OnInit()
  {
  wxImage *image0;
  wxString image_path0;
  wxBitmap *bitmap0;

// To read jpg format:
    wxInitAllImageHandlers();

// Read image and create bitmap:
  image_path0 =  wxT("my_image.jpg");
  image0= new wxImage(image_path0);
  if(!image0->IsOk()) {
      wxMessageBox(wxT("There was an error loading the image"));
      return false;
    }
  
  bitmap0 = new wxBitmap( *image0 );

// Main sizer for inserting the scrolled window
    wxBoxSizer* sizer = new wxBoxSizer(wxHORIZONTAL);
    frame = new wxFrame((wxFrame *)NULL, -1,  wxT("Scrolling an Image"), 
                         wxPoint(10,10), wxSize(800,800));
 
    JLP_wxScrolled1* my_image = new JLP_wxScrolled1(frame, wxID_ANY, bitmap0 );
    sizer->Add(my_image, 1, wxALL | wxEXPAND, 10);
    frame->SetSizer(sizer);

    frame->Show();
    return true;
  } 
};
 
IMPLEMENT_APP(MyApp)
