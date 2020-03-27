/******************************************************************************
* jlp_cgdev_wxwid_onclick.cpp 
* JLP_wxGseg_Canvas class
* Purpose:     Popup menu for displaying a curve with wxwidgets
*
void JLP_wxGseg_Canvas::OnContextMenu(wxContextMenuEvent& event)
void JLP_wxGseg_Canvas::ShowPopupMenu(const wxPoint& pos)
void JLP_wxGseg_Canvas::OnSetCursor(wxCommandEvent& event)
void JLP_wxGseg_Canvas::OnSetBoxType(wxCommandEvent& event)
void JLP_wxGseg_Canvas::OnSetPenColour(wxCommandEvent& event)
void JLP_wxGseg_Canvas::OnSetBackgdColour(wxCommandEvent& event)
void JLP_wxGseg_Canvas::OnChangeBoxLimits(wxCommandEvent& event)
void JLP_wxGseg_Canvas::OnCopy(wxCommandEvent& WXUNUSED(event))
void JLP_wxGseg_Canvas::OnSave(wxCommandEvent &WXUNUSED(event) )
*
* Author:      JLP 
* Version:     06/02/2016 
******************************************************************************/
#include "jlp_cgdev_wxwid_id.h"
#include "jlp_wx_gscanvas.h"
#include "jlp_wx_cursor.h"   // jlp_SetCrossHairCursor1, etc

/**************************************************************************
* JLP_wxGseg_Canvas class (derived from wxPanel)
**************************************************************************/

BEGIN_EVENT_TABLE(JLP_wxGseg_Canvas, wxPanel)
// catch paint events
  EVT_PAINT(JLP_wxGseg_Canvas::OnPaint)

// Check in wx/event.h for the full list:
/*
  EVT_LEAVE_WINDOW(JLP_wxGseg_Canvas::OnLeaveWindow)
*/
// catch motion events
  EVT_MOTION(JLP_wxGseg_Canvas::OnMotion)
  EVT_LEFT_DOWN(JLP_wxGseg_Canvas::OnLeftDown)
  EVT_LEFT_UP(JLP_wxGseg_Canvas::OnLeftUp)

// catch size events 
// JLP2016: not needed here since handled in jlp_wx_gpanel.cpp
//  EVT_SIZE (JLP_wxGseg_Canvas::OnResize)
 
// Popup menu:
#if USE_CONTEXT_MENU
    EVT_CONTEXT_MENU(JLP_wxGseg_Canvas::OnContextMenu)
#else
    EVT_RIGHT_UP(JLP_wxGseg_Canvas::OnRightUp)
#endif

   EVT_MENU(wxID_COPY, JLP_wxGseg_Canvas::OnCopy)
//   EVT_LEFT_DCLICK(JLP_wxGseg_Canvas::OnSaveDClick)   // Left double click
   EVT_MENU (ID_INFO_CURVE,  JLP_wxGseg_Canvas::OnInfoCurve)
   EVT_MENU (ID_SAVE,  JLP_wxGseg_Canvas::OnSave)
   EVT_MENU (ID_SAVE_TO_PST,  JLP_wxGseg_Canvas::OnSaveToPostscript)
// Setup: cursor
   EVT_MENU(ID_CURSOR_CROSS, JLP_wxGseg_Canvas::OnSetCursor)
   EVT_MENU(ID_CURSOR_BIG_CROSS, JLP_wxGseg_Canvas::OnSetCursor)
   EVT_MENU(ID_CURSOR_CROSSHAIR, JLP_wxGseg_Canvas::OnSetCursor)
   EVT_MENU(ID_CURSOR_CROSSHAIR1, JLP_wxGseg_Canvas::OnSetCursor)
   EVT_MENU(ID_CURSOR_ARROW, JLP_wxGseg_Canvas::OnSetCursor)
   EVT_MENU(ID_CURSOR_DOWN_ARROW, JLP_wxGseg_Canvas::OnSetCursor)
// Setup: pen colour
   EVT_MENU(ID_BLACK_PEN_COLOUR, JLP_wxGseg_Canvas::OnSetPenColour)
   EVT_MENU(ID_RED_PEN_COLOUR, JLP_wxGseg_Canvas::OnSetPenColour)
   EVT_MENU(ID_GREEN_PEN_COLOUR, JLP_wxGseg_Canvas::OnSetPenColour)
   EVT_MENU(ID_BLUE_PEN_COLOUR, JLP_wxGseg_Canvas::OnSetPenColour)
   EVT_MENU(ID_WHITE_PEN_COLOUR, JLP_wxGseg_Canvas::OnSetPenColour)
// Setup: background colour
   EVT_MENU(ID_BLACK_BACK_COLOUR, JLP_wxGseg_Canvas::OnSetBackgdColour)
   EVT_MENU(ID_GREY_BACK_COLOUR, JLP_wxGseg_Canvas::OnSetBackgdColour)
   EVT_MENU(ID_YELLOW_BACK_COLOUR, JLP_wxGseg_Canvas::OnSetBackgdColour)
   EVT_MENU(ID_WHITE_BACK_COLOUR, JLP_wxGseg_Canvas::OnSetBackgdColour)
   EVT_MENU(ID_BOX_TYPE0, JLP_wxGseg_Canvas::OnSetBoxType)
   EVT_MENU(ID_BOX_TYPE1, JLP_wxGseg_Canvas::OnSetBoxType)
   EVT_MENU(ID_BOX_TYPE2, JLP_wxGseg_Canvas::OnSetBoxType)
   EVT_MENU(ID_BOX_TICKS_IN, JLP_wxGseg_Canvas::OnSetBoxType)
   EVT_MENU(ID_BOX_XGRID, JLP_wxGseg_Canvas::OnSetBoxType)
   EVT_MENU(ID_BOX_YGRID, JLP_wxGseg_Canvas::OnSetBoxType)
// Box limits;
   EVT_MENU(ID_BOX_LIMITS_MANUAL, JLP_wxGseg_Canvas::OnChangeBoxLimits)
   EVT_MENU(ID_BOX_LIMITS_MANUAL1, JLP_wxGseg_Canvas::OnChangeBoxLimits)
   EVT_MENU(ID_BOX_LIMITS_AUTO, JLP_wxGseg_Canvas::OnChangeBoxLimits)
   EVT_MENU(ID_BOX_LIMITS_WITH_BOX, JLP_wxGseg_Canvas::OnChangeBoxLimits)
   EVT_MENU(ID_BOX_LIMITS_ZOOM_IN, JLP_wxGseg_Canvas::OnChangeBoxLimits)
   EVT_MENU(ID_BOX_LIMITS_ZOOM_OUT, JLP_wxGseg_Canvas::OnChangeBoxLimits)
// Processing:
   EVT_MENU (ID_STATISTICS,  JLP_wxGseg_Canvas::OnSetInteractiveProcessing)
   EVT_MENU (ID_PHOTOMETRY,  JLP_wxGseg_Canvas::OnSetInteractiveProcessing)
   EVT_MENU (ID_ASTROMETRY,  JLP_wxGseg_Canvas::OnSetInteractiveProcessing)
   EVT_MENU (ID_ADD_LABEL,  JLP_wxGseg_Canvas::OnSetInteractiveProcessing)
   EVT_MENU (ID_REM_LABEL,  JLP_wxGseg_Canvas::OnSetInteractiveProcessing)
   EVT_MENU (ID_IDLE_LABEL,  JLP_wxGseg_Canvas::OnSetInteractiveProcessing)
END_EVENT_TABLE()

/*************************************************************
* Determine the location where to display the popup menu
* and then display it
* (other way of display it when no mouse is available)
*************************************************************/
#if USE_CONTEXT_MENU
void JLP_wxGseg_Canvas::OnContextMenu(wxContextMenuEvent& event)
{
if(initialized != 1234) return;

    wxPoint point = event.GetPosition();
    // If from keyboard
    if (point.x == -1 && point.y == -1) {
        wxSize size = GetSize();
        point.x = size.x / 2;
        point.y = size.y / 2;
    } else {
        point = ScreenToClient(point);
    }
    ShowPopupMenu(point);
}
#endif
/*************************************************************
* Display the popup menu
*************************************************************/
void JLP_wxGseg_Canvas::ShowPopupMenu(const wxPoint& pos)
{
if(PopupMenu1 == NULL) return;

// Make menu items checked correctly, according to private parameters:
  UpdatePopupMenu();

// Display the popup menu:
// It is recommended to not explicitly specify coordinates
// when calling PopupMenu in response to mouse click,
// because some of the ports (namely, wxGTK) can do a better job
// of positioning the menu in that case
// OLD:  PopupMenu(PopupMenu1, pos.x, pos.y);
// NEW:
  PopupMenu(PopupMenu1);

return;
}
/************************************************************************
* Set the canvas cursor to Cross Hair, Cross Hair with a cross, etc. 
*
* (Radio-Buttons)
************************************************************************/
void JLP_wxGseg_Canvas::OnSetCursor(wxCommandEvent& event)
{

if(initialized != 1234) return;

  switch (event.GetId())
  {
// Set the canvas cursor to Cross Hair
   case ID_CURSOR_CROSSHAIR:
     cgdev_settings1.cursor_type = wxT("CrossHair");
     break;
// Set the canvas cursor to Cross Hair with a cross in the center
// (necessary when crosshair is not well refreshed)
   case ID_CURSOR_CROSSHAIR1:
     cgdev_settings1.cursor_type = wxT("CrossHair1");
     break;
// Set the canvas cursor to down arrow (with bitmap)
   case ID_CURSOR_DOWN_ARROW:
     cgdev_settings1.cursor_type = wxT("DownArrow");
     break;
// Set the canvas cursor to Arrow
   case ID_CURSOR_ARROW:
     cgdev_settings1.cursor_type = wxT("Arrow");
     break;
// Set the canvas cursor to BigCross
   case ID_CURSOR_BIG_CROSS:
     cgdev_settings1.cursor_type = wxT("BigCross");
     break;
// Set the canvas cursor to Cross
   default:
   case ID_CURSOR_CROSS:
     cgdev_settings1.cursor_type = wxT("Cross");
     break;
  }

UpdatePopupMenu_Cursor();

return;
}
/************************************************************************
* Set the canvas pen color to black, white, etc 
************************************************************************/
void JLP_wxGseg_Canvas::OnSetPenColour(wxCommandEvent& event)
{
if(initialized != 1234) return;

  switch (event.GetId())
  {
   default:
   case ID_BLACK_PEN_COLOUR:
     cgdev_settings1.pen_default_colour = *wxBLACK;
     break;
   case ID_RED_PEN_COLOUR:
     cgdev_settings1.pen_default_colour = *wxRED;
     break;
   case ID_GREEN_PEN_COLOUR:
     cgdev_settings1.pen_default_colour = *wxGREEN;
     break;
   case ID_BLUE_PEN_COLOUR:
     cgdev_settings1.pen_default_colour = *wxBLUE;
     break;
   case ID_WHITE_PEN_COLOUR:
     cgdev_settings1.pen_default_colour = *wxWHITE;
     break;
  }

 cgdev_settings1.pen_colour = cgdev_settings1.pen_default_colour;

// Update the popup menu:
  UpdatePopupMenu_PenColour();

return;
}
/************************************************************************
* Set the canvas background color to black, white, etc 
************************************************************************/
void JLP_wxGseg_Canvas::OnSetBackgdColour(wxCommandEvent& event)
{

if(initialized != 1234) return;

  switch (event.GetId())
  {
   case ID_BLACK_BACK_COLOUR:
     cgdev_settings1.backgd_colour = *wxBLACK;
     break;
   case ID_YELLOW_BACK_COLOUR:
     cgdev_settings1.backgd_colour = *wxYELLOW; 
     break;
   case ID_GREY_BACK_COLOUR:
     cgdev_settings1.backgd_colour = *wxLIGHT_GREY; 
     break;
   case ID_WHITE_BACK_COLOUR:
     cgdev_settings1.backgd_colour = *wxWHITE;
     break;
  }

// Update the popup menu:
UpdatePopupMenu_BackgroundColour();

return;
}
/************************************************************************
* Set the box type and other box options 
*
* (Radio-Buttons)
************************************************************************/
void JLP_wxGseg_Canvas::OnSetBoxType(wxCommandEvent& event)
{

if(initialized != 1234) return;

  switch (event.GetId())
  {
// MGO axes:
   default:
   case ID_BOX_TYPE0:
     cgdev_settings1.box_type = 0;
     break;
// JLP axes:
   case ID_BOX_TYPE1:
     cgdev_settings1.box_type = 1;
     break;
// GSEG axes:
   case ID_BOX_TYPE2:
     cgdev_settings1.box_type = 2;
     break;
// Toggle ticks_in:
   case ID_BOX_TICKS_IN:
     cgdev_settings1.ticks_in = 1 - cgdev_settings1.ticks_in;
     break;
// Toggle x grid:
   case ID_BOX_XGRID:
     cgdev_settings1.xgrid = 1 - cgdev_settings1.xgrid;
     break;
// Toggle y grid:
   case ID_BOX_YGRID:
     cgdev_settings1.ygrid = 1 - cgdev_settings1.ygrid;
     break;
  }

// Update the popup menu:
UpdatePopupMenu_BoxType();

return;
}
/************************************************************************
* Prompt the user for new Box Limits 
*
* ID_BOX_LIMITS_AUTO,
* ID_BOX_LIMITS_MANUAL,
* ID_BOX_LIMITS_MANUAL1,
* ID_BOX_LIMITS_WITH_BOX,
************************************************************************/
void JLP_wxGseg_Canvas::OnChangeBoxLimits(wxCommandEvent& event)
{
if(initialized != 1234) return;

// Possible problem avoided here!
// (i.e. conflict between "Statistics" and "InteractiveSelection")
// This event can be generated by PopupMenuEraseCheckBoxes():
// so I found a wayout to prevent loosing processing selection:
  if(software_event1){
   software_event1 = 0;
   return;
   }

// Cancel all previous processing settings
 cgdev_settings1.InternalProcessingMode = -1;
 UpdatePopupMenu_InternalProcessingMode();

switch (event.GetId())
  {
  default:
  case ID_BOX_LIMITS_WITH_BOX:
// ProcessingMode=18 when this mode is activated
    cgdev_settings1.InternalProcessingMode = 18;
// Apply settings and update popup menu:
    UpdatePopupMenu_InternalProcessingMode();
  case ID_BOX_LIMITS_AUTO:
    BoxLimitsAuto();
    break;
  case ID_BOX_LIMITS_MANUAL:
  case ID_BOX_LIMITS_MANUAL1:
    BoxLimitsPrompt();
    break;
  }

RedrawToBackupDC();

return;
}
/************************************************************************
* Prompt the user for new Box Limits 
* Box_xmin, Box_xmax, Box_ymin, Box_ymax, chosen by the user 
************************************************************************/
void JLP_wxGseg_Canvas::BoxLimitsPrompt()
{
wxString s_values, s_question;
double w1, w2, w3, w4;

if(initialized != 1234) return;
 
s_question.Printf(wxT("Current box boundaries are: xmin,xmax,ymin,ymax:%.4g %.4g %.4g %.4g\n Enter new values (CR to cancel):"),
                  Jgc0.xminuser, Jgc0.xmaxuser, Jgc0.yminuser, Jgc0.ymaxuser);

s_values.Printf(wxT("%.4g %.4g %.4g %.4g"), 
                  Jgc0.xminuser, Jgc0.xmaxuser, Jgc0.yminuser, Jgc0.ymaxuser);

wxString result = wxGetTextFromUser(s_question, _T("New thresholds:"),
                                   s_values, NULL);
if (!result.IsEmpty()){
 if(sscanf(result.char_str(), "%lf %lf %lf %lf", &w1, &w2, &w3, &w4) == 4) {
   SetBoxLimits(w1, w2, w3, w4);
   }

// Set axes to MGO axes:
 cgdev_settings1.box_type = 0;

 RedrawToBackupDC();
 }

return;
}
/************************************************************************
* InfoCurve: to display information about plotted curve(s) 
************************************************************************/
void JLP_wxGseg_Canvas::OnInfoCurve(wxCommandEvent& WXUNUSED(event))
{
wxString str1, str0;
int k;

// Full information: 
/*
 char nchar1[4*NCURVES_MAX], pcolor1[32*NCURVES_MAX];
  char plot_fname1[128*NCURVES_MAX];
  int *npts1, nmaxi1, nout_maxi1, ncurves1, ncurves_maxi1;
*/

str1.Printf(wxT("ncurves=%d \n"), ncurves1);
for(k = 0; k < ncurves1; k++) {
 str0.Printf(wxT("Curve #%d : %s \n\
   (npts=%d, pcolor=%s, line_type=%s)\n"),
             k + 1, &plot_fname1[k * 128], npts1[k], &pcolor1[32 * k], 
             &nchar1[4 * k]);
 str1.Append(str0);
 }

// Frame boundaries:
str0.Printf(wxT("Boundaries: xmin,xmax,ymin,ymax: %.4g %.4g %.4g %.4g\n"),
             Jgc0.xminuser, Jgc0.xmaxuser, Jgc0.yminuser, Jgc0.ymaxuser);

str1.Append(str0);
 (void)wxMessageBox(str1, _T("Info. about displayed curve(s)"),
                    wxICON_INFORMATION | wxOK );
/* For debug:
 wxPuts(wxT("INFO/OK: ") + info_message);
*/
return;
}
/************************************************************************
* Copy to clipboard
************************************************************************/
void JLP_wxGseg_Canvas::OnCopy(wxCommandEvent& WXUNUSED(event))
{
if(initialized != 1234) return;
#if wxUSE_CLIPBOARD
    wxBitmapDataObject *dobjBmp = new wxBitmapDataObject;
    dobjBmp->SetBitmap(*backup_dc_bitmap2);

    wxTheClipboard->Open();

    if ( !wxTheClipboard->SetData(dobjBmp) ) {
        wxLogError(_T("Failed to copy bitmap to clipboard"));
    }

    wxTheClipboard->Close();
#endif
return;
}
/************************************************************************
* Save the current bitmap to file
*
************************************************************************/
void JLP_wxGseg_Canvas::OnSave(wxCommandEvent &WXUNUSED(event) )
{
wxString savefilename;
bool saved = false;

if(initialized != 1234) return;

wxImage image1 = backup_dc_bitmap2->ConvertToImage();

// Init all image handlers in case it was not already done:
// (Needed to read PNG,JPEG, etc)
::wxInitAllImageHandlers();

// Gif is not available yet ?
   savefilename = wxFileSelector( wxT("Save Image to jpg, png, tiff, gif"), 
            wxT(""), wxT(""), wxT("jpg|png|tiff|gif"), 
            wxT("Files (*.jpg;*.png;*.tif;*.gif)|*.jpg;*.png;*.tif;*.gif"),
            wxFD_SAVE, this);

   if ( savefilename.empty() ) return;

   wxString extension;
   wxFileName::SplitPath(savefilename, NULL, NULL, &extension);

   if ( extension == _T("jpg") ) {
// Set reasonable quality and compression:
       image1.SetOption(wxIMAGE_OPTION_QUALITY, 80);
// Save image:
       saved = image1.SaveFile(savefilename, wxBITMAP_TYPE_JPEG);
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


return;
}
/************************************************************************
* Output curve as a postscript file
************************************************************************/
void JLP_wxGseg_Canvas::OnSaveToPostscript(wxCommandEvent &WXUNUSED(event))
{
wxString filename;
char pst_filename[128];

if(initialized != 1234) return;

   wxFileDialog dialog(NULL, _T("Save to postscript file"), wxEmptyString,
                       wxEmptyString, _T("Files (*.ps)|*.ps"),
                       wxFD_SAVE|wxFD_OVERWRITE_PROMPT);
   if (dialog.ShowModal() == wxID_OK) {
       filename = dialog.GetPath();
       strcpy(pst_filename,filename.c_str());
       PstCopyOfDisplay(pst_filename);
   }

return;
}
/************************************************************************
* Statistics: to display statistics about the displayed image
*   (within a box selected by the user)
* Astrometry: to compute the position of a pattern within a circle
*   (circle interactively selected by the user)
* Photometry: to compute the flux within a circle of the displayed image
*   (circle interactively selected by the user)
************************************************************************/
void JLP_wxGseg_Canvas::OnSetInteractiveProcessing(wxCommandEvent& event)
{
/* InteractiveProcessingMode :
* -1=None 0=BoxLimits 1=Statistics, 2=Astrometry 3=Photometry
* 4=Add a label 5=Remove a label 6=Add the scale bar 7=Add the North-East
* 8=Slice 9=Add a line 10=Add a rectangle 11=Add a circle 12=Add an ellipse
* 13=Add a ring 14=Remove a shape
*/
  switch(event.GetId()) {
   case ID_STATISTICS:
     cgdev_settings1.InternalProcessingMode = 1;
     break;
   case ID_ASTROMETRY:
     cgdev_settings1.InternalProcessingMode = 2;
     break;
   case ID_PHOTOMETRY:
     cgdev_settings1.InternalProcessingMode = 3;
     break;
   case ID_ADD_LABEL:
     cgdev_settings1.InternalProcessingMode = 4;
     break;
   case ID_REM_LABEL:
     cgdev_settings1.InternalProcessingMode = 5;
     break;
   case ID_IDLE_LABEL:
     cgdev_settings1.InternalProcessingMode = -1;
     break;
  }

// Apply settings and update popup menu:
UpdatePopupMenu_InternalProcessingMode();

return;
}
