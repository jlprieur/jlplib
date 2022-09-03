/**************************************************************************
* jlp_wxgdev_popup.h
* JLP_wxGDev_Popup class (for Curves/Images)
*
* JLP
* Version 12/09/2017
**************************************************************************/
#ifndef __jlp_wxgdev_popup_h                     /* sentry */
#define __jlp_wxgdev_popup_h

#include "wx/wx.h"
#include "jlp_gdev_wxwid.h"       // JLP_GDev_wxWID class

#define POPUP_NITEMS_MAX 20       // Maximum number of items in popup menu

/****************************************************************************
*
*****************************************************************************/
class JLP_wxGDev_Popup : public wxWindow
{

public:

 JLP_wxGDev_Popup(JLP_GDev_wxWID *jlp_gdev_wxwid0, 
                  int gdev_graphic_type0); 

/* Destructor:
* (Should be declared as virtual)
*/
  ~JLP_wxGDev_Popup() { };
  void Main_Init();

// Defined in jlp_wxgdev_popup_create.cpp:
  void DeletePopupMenu();
  void CreatePopupMenu();
  void CreateImageMenuForSplot();
  void CreateImageMenuFilterForSplot();
  void CreateGsegrafMenu();
  void CreateShapeMenu();
  void CreateColourBackgroundMenu();
  void CreateSetupMenuForCurves();

// Defined in jlp_wxgdev_popup_update.cpp:
  void UpdatePopupMenu_Cursor();
  void UpdatePopupMenu_LUT(const int update_display);
  void UpdatePopupMenu_Zoom();
  void UpdatePopupMenu_Filter();
  void UpdatePopupMenu_Gsegraf(const int update_display);
  void UpdatePopupMenu_ITT(const int update_display);
  void UpdatePopupMenu_PenColour(const int update_display);
  void UpdatePopupMenu_BackgroundColour(const int update_display);
  void UpdatePopupMenu_BoxType(const int update_display);
  void UpdatePopupMenu_InternalProcessingMode();
  void UpdatePopupMenu(wxGDev_SETTINGS wxgdev_settings0, int update_display); 
  void PopupMenuEraseProcessingCheckBoxes();

// Defined in jlp_wxgdev_popup_onclick1.cpp:
  void OnSetCursor(wxCommandEvent& event);
  void OnSetPenColour(wxCommandEvent& event);
  void OnSetBackgdColour(wxCommandEvent& event);
  void OnSetBoxType(wxCommandEvent& event);
  void OnChangeBoxLimits(wxCommandEvent& event);
  void OnChangeLUT(wxCommandEvent& event);
  void OnChangeZoom(wxCommandEvent& event);
  void OnChangeITT_is_linear(wxCommandEvent& event);
  void OnChangeITT_thresholds(wxCommandEvent& event);
  void OnSetInteractiveProcessing(wxCommandEvent& event);
  void InfoImage(wxCommandEvent& event);
  void OnInfoCurve(wxCommandEvent& WXUNUSED(event));
  void OnCopy(wxCommandEvent& event);
  void OnPaste(wxCommandEvent& event);

// Defined in jlp_wxgdev_popup_onclick2.cpp:
  void OnDisplayDbleImage3D(wxCommandEvent &event);
  void OnDisplayDbleImageContours(wxCommandEvent &event);
  void OnSave(wxCommandEvent& event);
  void OnSaveToPostscript(wxCommandEvent& event);
  void OnViewChangeAxisLimits(wxCommandEvent& event);
  void OnViewAxisRotate(wxCommandEvent& event);
  void OnSelectFilter(wxCommandEvent& event);
  void OnAddShape(wxCommandEvent& event);
  void OnChangeShape(wxCommandEvent& event);
  void OnAddLabel(wxCommandEvent& event);
  void OnRemoveLabel(wxCommandEvent& event);
  void OnAddScaleBar(wxCommandEvent& event);
  void OnAddNorthEastLabel(wxCommandEvent& event);
  void OnRemoveScaleBar(wxCommandEvent& event);
  void OnRemoveNorthEastLabel(wxCommandEvent& event);
  void OnSetLabelContours(wxCommandEvent& event);

// Accessor:
  wxMenu* GetPopupMenu1(){return PopupMenu1;};

private:
  int initialized, gdev_graphic_type1, software_event1;
  JLP_GDev_wxWID *jlp_gdev_wxwid1;

// Popup menu (PopupMenu1):
  wxMenu *PopupMenu1, *menuInfo, *menuSetup, *menuCursor;
  wxMenu *menuLUT, *menuITT1, *menuITT2, *menuFilter;
  wxMenu *menuProcess, *menuPen, *menuZoom, *menuLabel, *menuShape;
  wxMenu *menuBackgd, *menuBoxType, *menuBoxLimits;
  wxMenu *menuGsegraf;

  DECLARE_EVENT_TABLE()
}; 

#endif
