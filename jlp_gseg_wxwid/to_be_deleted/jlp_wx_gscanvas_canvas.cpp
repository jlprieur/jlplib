/******************************************************************************
* Name:        jlp_cgdev_wxwid_canvas.cpp (JLP_wxGseg_Canvas class)
*
* Purpose:     Handling menu and events
*              that are defined in "jlp_cgdev_wxwid_menu.cpp"
*
* Author:      JLP
* Version:     12/02/2016
******************************************************************************/
#include <stdlib.h>   // exit()
#include "jlp_wx_gscanvas.h"
#include "jlp_wx_gsproc.h"         // JLP_wx_iGDProc class
#include "jlp_wx_curve_labels.h"    // For JLP_wxImageLabels

/*
int  LoadNewImage(double *array, int nx, int ny, wxString fname)
int  UpdateImageValues(double *array, int nx, int ny,
                       const bool reset_ITT_to_MinMax);
void InitBackupDC(int nx20, int ny20)
void OnPaint( wxPaintEvent &WXUNUSED(event) )
void RedrawToBackupDC()
*/
/*************************************************************
* Draw pixel information (coordinates and pixel value)
*************************************************************/
void JLP_wxGseg_Canvas::DisplayCoordinates(wxString coordinates_text)
{

if(m_StaticCoord != NULL)
    m_StaticCoord->SetLabel(coordinates_text);

if(m_StaticHelp != NULL)
   m_StaticHelp->SetLabel(active_gsproc->Get_HelpText());

return;
}
/*************************************************************
* To update the display when Paint events are detected
*************************************************************/
void JLP_wxGseg_Canvas::OnPaint( wxPaintEvent &WXUNUSED(event) )
{
// Construct a Device Context on which graphics and text
// can be drawn.
// wxPaintDC is a constructor and pass a pointer to the window
// on which you wish to paint:
wxDC *dc;
 dc = new wxPaintDC(this);

// Update the screen:
cgdev_render(dc);

delete dc;
return;
}
/*************************************************************
* To update the display with any kind of graphic context
*************************************************************/
void JLP_wxGseg_Canvas::cgdev_render( wxDC *dc1 )
{
int width1, height1;
int width2, height2;
int xoffset, yoffset;

if(initialized != 1234) return;

// wxDC:Clear
 dc1->Clear();

// JLP2016: problem to fill in the frame:
// copy from backup_dc(0,0) to screen (0,0)
if(backup_dc_bitmap2 != NULL && backup_dc != NULL) {
// xdest, ydest, width (to be copied from source), 
// height (to be copied from source), source, xsrc, ysrc
  dc1->GetSize(&width1, &height1);
  width2 = backup_dc_bitmap2->GetWidth();
  height2 = backup_dc_bitmap2->GetHeight();
  xoffset = MAXI(0, width2 - width1); 
  yoffset = MAXI(0, height2 - height1); 
  dc1->Blit(0, 0, width2, height2, backup_dc, xoffset, yoffset, wxCOPY);
  } else {
  fprintf(stderr, "Not ready yet: backup_dc still null!\n");
  }

return;
}
/*************************************************************
* Refresh screen outside Paint events
*
*************************************************************/
void JLP_wxGseg_Canvas::MyRefreshScreen()
{
/* OLD VERSION without paint request
wxDC *dc1;

// Warning: should not use wxPaintDC outside of Paint events,
// so use wxClientDC:
   dc1 = new wxClientDC(this);

// Update the screen with backup_dc (hence erase previous overlay drawings):
   cgdev_render(dc1);
   delete dc1;
******/

// Update display on the screen
// WARNING: Refresh invalidates the window and queues the paint request
if(m_panel != NULL) m_panel->Refresh();

// Here is something to try in case of problems :
// call Update() immediately after your call to Refresh().
// Update forces an immediate repaint of any invalidated rectangles.

return;
}
/*************************************************************
* Redo the drawings from scratch outside of Paint events
* Called when resizing the panel
* Now also calls MyrefreshScreen at the end
*
* This is also a trick to redraw on PostScript DC
*************************************************************/
void JLP_wxGseg_Canvas::RedrawToBackupDC()
{
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

// Plot all the curves with current values of cgdev_settings1 
 should_plot_allcurves = 1;
 RefreshAllCurves();

// Redo drawing from metafile:
#if USE_METAFILE
 if(MetaFileIsOpened) ReadAndExecuteMetaFile();
#endif

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

// Calling Refresh with a paint event request at the end 
 MyRefreshScreen();

return;
}
