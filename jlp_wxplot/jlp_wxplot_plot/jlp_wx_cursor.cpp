/******************************************************************************
* jlp_wx_cursor.cpp
* Routines used by JLP_wxImageCanvas and JLP_wxCurveCanvas classes
* to display various cursors
* Cursor types: Arrow, Bitmap, Cross, CrossHair, CrossHair1
*
* Author:      JLP
* Version:     12/07/2012
******************************************************************************/
#include "jlp_wx_cursor.h"

/*
  void jlp_SetCrossHairCursor(wxCursor &my_cursor, wxString &c_cursor_type);
  void jlp_SetCrossHairCursor1(wxCursor &my_cursor, wxString &c_cursor_type);
  void jlp_SetDownArrowCursor(wxCursor& my_cursor, wxColour c_PenColour,
                              wxString& c_cursor_type)
  void jlp_SetArrowCursor(wxCursor &my_cursor, wxString &c_cursor_type);
  void jlp_SetCrossCursor(wxCursor& my_cursor, wxColour c_PenColour,
                          wxString& c_cursor_type, int iwidth);
*/


static void create_down_arrow_cursor(wxCursor &down_arrow_cursor,
                                     wxColour c_PenColour);
static void create_cross_cursor(wxCursor &cross_cursor,
                                wxColour c_PenColour, int iwidth);

#ifndef BYTEE
//#define BYTEE char
#define BYTEE int 
#endif

// Bitmap for cursor:  (1=black, 0=white for MSW)
// 128 values * 8 bits = 1024 = 32*32 bitmap
static BYTEE my_down_arrow_bits[] = { 255, 255, 255, 255, 31,
  255, 255, 255, 31, 255, 255, 255, 31, 255, 255, 255,
  31, 255, 255, 255, 31, 255, 255, 255, 31, 255, 255,
  255, 31, 255, 255, 255, 31, 255, 255, 255, 25, 243,
  255, 255, 19, 249, 255, 255, 7, 252, 255, 255, 15, 254,
  255, 255, 31, 255, 255, 255, 191, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255 };


// Mask:
// mask: 1 is opaque, 0 is transparent (for Linux):
// mask: 0 is opaque, 1 is transparent (for MSW):
// 128 values * 8 bits = 1024 = 32*32
static BYTEE my_down_arrow_mask[] = { 240, 1, 0, 0, 240, 1,
  0, 0, 240, 1, 0, 0, 240, 1, 0, 0, 240, 1, 0, 0, 240, 1,
  0, 0, 240, 1, 0, 0, 240, 1, 0, 0, 255, 31, 0, 0, 255,
  31, 0, 0, 254, 15, 0, 0, 252, 7, 0, 0, 248, 3, 0, 0,
  240, 1, 0, 0, 224, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0 };

/*******************************************************************
* CrossHair cursor without anything in the center
* Cursor types: Arrow, Bitmap, Cross, CrossHair, CrossHair1
*******************************************************************/
void jlp_SetCrossHairCursor(wxCursor& my_cursor, wxString& c_cursor_type)
{
    c_cursor_type = wxT("CrossHair");
    my_cursor =  wxCursor( wxCURSOR_BLANK );
//    SetCursor(my_cursor);
//    Refresh();
return;
}
/*******************************************************************
* CrossHair cursor, with a cross cursor in the center
* Cursor types: Arrow, Bitmap, Cross, CrossHair, CrossHair1
*******************************************************************/
void jlp_SetCrossHairCursor1(wxCursor& my_cursor, wxString& c_cursor_type)
{
    c_cursor_type = wxT("CrossHair1");
    my_cursor = wxCursor( wxCURSOR_CROSS );
return;
}
/*******************************************************************
* Down arrow cursor
* Cursor types: Arrow, Bitmap, Cross, CrossHair, CrossHair1
*******************************************************************/
void jlp_SetDownArrowCursor(wxCursor& my_cursor, wxColour c_PenColour,
                            wxString& c_cursor_type)
{
   wxCursor down_arrow_cursor;
// Down arrow with a bitmap:
   create_down_arrow_cursor(down_arrow_cursor, c_PenColour);
   c_cursor_type = wxT("DownArrow");
   my_cursor = down_arrow_cursor;
return;
}
/*
wxCURSOR_ARROW  A standard arrow cursor.
wxCURSOR_RIGHT_ARROW    A standard arrow cursor pointing to the right.
wxCURSOR_BLANK  Transparent cursor.
wxCURSOR_BULLSEYE       Bullseye cursor.
wxCURSOR_CHAR   Rectangular character cursor.
wxCURSOR_CROSS  A cross cursor.
wxCURSOR_HAND   A hand cursor.
wxCURSOR_IBEAM  An I-beam cursor (vertical line).
wxCURSOR_LEFT_BUTTON    Represents a mouse with the left button depressed.
wxCURSOR_MAGNIFIER      A magnifier icon.
wxCURSOR_MIDDLE_BUTTON  Represents a mouse with the middle button depressed.
wxCURSOR_NO_ENTRY       A no-entry sign cursor.
wxCURSOR_PAINT_BRUSH    A paintbrush cursor.
wxCURSOR_PENCIL         A pencil cursor.
wxCURSOR_POINT_LEFT     A cursor that points left.
wxCURSOR_POINT_RIGHT    A cursor that points right.
wxCURSOR_QUESTION_ARROW         An arrow and question mark.
wxCURSOR_RIGHT_BUTTON   Represents a mouse with the right button depressed.
wxCURSOR_SIZENESW       A sizing cursor pointing NE-SW.
wxCURSOR_SIZENS         A sizing cursor pointing N-S.
wxCURSOR_SIZENWSE       A sizing cursor pointing NW-SE.
wxCURSOR_SIZEWE         A sizing cursor pointing W-E.
wxCURSOR_SIZING         A general sizing cursor.
wxCURSOR_SPRAYCAN       A spraycan cursor.
wxCURSOR_WAIT   A wait cursor.
wxCURSOR_WATCH  A watch cursor.
wxCURSOR_ARROWWAIT      A cursor with both an arrow and an hourglass, (windows.)*/
/*******************************************************************
* Arrow cursor
* Cursor types: Arrow, Bitmap, Cross, CrossHair, CrossHair1
*******************************************************************/
void jlp_SetArrowCursor(wxCursor& my_cursor, wxString& c_cursor_type)
{
    c_cursor_type = wxT("Arrow");
    my_cursor = wxCursor( wxCURSOR_ARROW );
return;
}
/*******************************************************************
* Cross cursor
* INPUT:
* c_PenColour
* iwidth: width of cursor
*
* OUTPUT:
* c_cursor_type: "BigCross" or "Cross"
*******************************************************************/
void jlp_SetCrossCursor(wxCursor& my_cursor, wxColour c_PenColour,
                        wxString& c_cursor_type, int iwidth)
{
     wxCursor cross_cursor;
// Cross with a bitmap:
     create_cross_cursor(cross_cursor, c_PenColour, iwidth);
     if(iwidth > 32)
          c_cursor_type = wxT("BigCross");
      else
          c_cursor_type = wxT("Cross");
      my_cursor = cross_cursor;
return;
}
/************************************************************************
* Cursor with a bitmap
* Down arrow
************************************************************************/
static void create_down_arrow_cursor(wxCursor &down_arrow_cursor,
                                     wxColour c_PenColour){
int nxb = 32, nyb = 32;
#ifdef __WXMSW__
// 128 values * 8 bits = 1024 = 32*32
// wxBitmap down_arrow_bitmap(my_down_arrow_bits, 32, 32);
wxBitmap *down_arrow_bitmap, *down_arrow_mask_bitmap;
#endif
register int i;
char down_arrow_bits_char[128], down_arrow_mask_char[128];
// White is 1, Black is 0 for MSW:
if(c_PenColour == *wxWHITE) {
  for(i = 0; i < nxb*nyb/8; i++) down_arrow_bits_char[i] = my_down_arrow_bits[i];
  } else {
  for(i = 0; i < nxb*nyb/8; i++) down_arrow_bits_char[i] = 255 - my_down_arrow_bits[i];
  }
for(i = 0; i < nxb*nyb/8; i++) down_arrow_mask_char[i] = my_down_arrow_mask[i];
#ifdef __WXMSW__
down_arrow_bitmap = new wxBitmap(down_arrow_bits_char, nxb, nyb);

// Mask inversion, since MWS and GTK/linux have opposite behaviour
// for masks:
for(i = 0; i < nxb*nyb/8; i++) down_arrow_mask_char[i] = 255 - down_arrow_mask_char[i];
down_arrow_mask_bitmap = new wxBitmap(down_arrow_bits_char, nxb, nyb);
down_arrow_bitmap->SetMask(new wxMask(*down_arrow_mask_bitmap));
wxImage down_arrow_image = down_arrow_bitmap->ConvertToImage();
// hotSpotX and hotSpotY are currently only used under Windows when loading from an icon file,
// to specify the cursor hotspot relative to the top left of the image.
down_arrow_image.SetOption(wxIMAGE_OPTION_CUR_HOTSPOT_X, 6);
down_arrow_image.SetOption(wxIMAGE_OPTION_CUR_HOTSPOT_Y, 14);
down_arrow_cursor = wxCursor(down_arrow_image);
#else
// Colours are only available for GTK/Motif:
if(c_PenColour == *wxWHITE)
down_arrow_cursor = wxCursor(down_arrow_bits_char, nxb, nyb, 6, 14,
                             down_arrow_mask_char, wxBLACK, wxWHITE);
else
down_arrow_cursor = wxCursor(down_arrow_bits_char, nxb, nyb, 6, 14,
                             down_arrow_mask_char, wxWHITE, wxBLACK);
#endif
return;
}
/************************************************************************
* Cursor with a bitmap
* Cross
************************************************************************/
static void create_cross_cursor(wxCursor &cross_cursor,
                                wxColour c_PenColour, int iwidth){
// Maximum size: 32*32 bits ?
// I try with 64*64 = 512 * 8
// 32*32 = 128 * 8
// 64*64 = 512 * 8
char cross_bits[512], cross_mask[512];
int nxb, nyb, ic, jc;
register int i;
#ifdef __WXMSW__
// mask: 0 is opaque, 1 is transparent for MSWindows:
// Maximum size 32x32 bits for MSWindows:
  nxb = 32;
  nyb = 32;
#else
// mask: 1 is opaque, 0 is transparent for GTK/Linux:
if(iwidth > 32) {
  nxb = 64;
  nyb = 64;
  } else {
  nxb = 32;
  nyb = 32;
  }
#endif
// Setup the mask, assuming GTK/Linux behaviour
// mask: 1 is opaque, 0 is transparent for GTK/Linux:
for(i = 0; i < (nxb * nyb / 8); i++) {
// 255 = 11111111   = black
   cross_bits[i] = 255;
   cross_mask[i] = 0;
   }

ic = nxb/2;
jc = nyb/2;
 for(i = 0; i < nxb; i++)
 {
// Central line: 1111111111
 cross_mask[(i + nxb * jc)/8] = 255;
// Central column: 1 = 00000001
 cross_mask[(ic + nxb * i)/8] |= 1;
 }

#ifdef __WXMSW__
// 128 values * 8 bits = 1024 = 32*32
wxBitmap *cross_bitmap, *cross_mask_bitmap;
char cross_char[512];
// White is 1, Black is 0 for MSW:
if(c_PenColour == *wxWHITE) {
  for(i = 0; i < nxb*nyb/8; i++) cross_char[i] = 255 - cross_bits[i];
  } else {
  for(i = 0; i < nxb*nyb/8; i++) cross_char[i] = cross_bits[i];
  }
cross_bitmap = new wxBitmap(cross_char, nxb, nyb);

// Inversion, since MWS and GTK/linux have opposite behaviour
// for masks:
for(i = 0; i < nxb*nyb/8; i++) cross_mask[i] = 255 - cross_mask[i];
cross_mask_bitmap = new wxBitmap(cross_mask, nxb, nyb);
cross_bitmap->SetMask(new wxMask(*cross_mask_bitmap));
wxImage cross_image = cross_bitmap->ConvertToImage();
// hotSpotX and hotSpotY are currently only used under Windows when loading from an icon file,
// to specify the cursor hotspot relative to the top left of the image.
cross_image.SetOption(wxIMAGE_OPTION_CUR_HOTSPOT_X, nxb/2);
cross_image.SetOption(wxIMAGE_OPTION_CUR_HOTSPOT_Y, nyb/2);
cross_cursor = wxCursor(cross_image);
#else
if(c_PenColour == *wxWHITE)
// nx, ny, ix, jc,...
cross_cursor = wxCursor(cross_bits, nxb, nyb, nxb/2, nyb/2, cross_mask,
                             wxWHITE, wxBLACK);
else
cross_cursor = wxCursor(cross_bits, nxb, nyb, nxb/2, nyb/2, cross_mask,
                             wxBLACK, wxWHITE);
#endif
return;
}
