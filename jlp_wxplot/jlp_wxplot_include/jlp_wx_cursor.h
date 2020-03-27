/******************************************************************************
* jlp_wx_cursor.h
* Routines used by JLP_wxImageCanvas and JLP_wxCurveCanvas classes 
* to display various cursors
* Cursor types: Arrow, Bitmap, Cross, CrossHair, CrossHair1
*
* Author:      JLP 
* Version:     12/07/2012
******************************************************************************/
#ifndef _jlp_wx_cursor_h  // Sentry
#define _jlp_wx_cursor_h  

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
#include "wx/wx.h"
#endif

#include "wx/mstream.h"
#include "wx/wfstream.h"
#include "wx/quantize.h"
#include "wx/bitmap.h"
#include "wx/cursor.h"

/************************************************************************
* Cursor routines (contained in jlp_wx_cursor.cpp) 
************************************************************************/
  void jlp_SetCrossHairCursor(wxCursor &my_cursor, wxString &c_cursor_type);
  void jlp_SetCrossHairCursor1(wxCursor &my_cursor, wxString &c_cursor_type);
  void jlp_SetDownArrowCursor(wxCursor& my_cursor, wxColour c_PenColour, 
                              wxString& c_cursor_type);
  void jlp_SetArrowCursor(wxCursor &my_cursor, wxString &c_cursor_type);
  void jlp_SetCrossCursor(wxCursor& my_cursor, wxColour c_PenColour, 
                          wxString& c_cursor_type, int iwidth);

#endif // EOF Sentry
