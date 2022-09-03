/****************************************************************************
* Name: spea_frame_id.h  (SpeaFrame class)
* list of ID used by SpeaFrame class
* 
* JLP
* Version 11/03/2016
****************************************************************************/
#ifndef _spea_frame_id_h_
#define _spea_frame_id_h_

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif

//---------------------------------------------------------------------
//---------------------------------------------------------------------
enum{
//  ID_QUIT         = wxID_EXIT,
  ID_QUIT         = 1050, 

// Menu/File:
  ID_LOAD_SPECTRUM_FILE,
  ID_LOAD_SHAPES_FILE,
  ID_LOAD_RESULTS_FILE,
  ID_LOAD_SPECTRAL_LIB,
  ID_LOAD_IMAGE,
  ID_LOAD_MUSE_IMAGE,
  ID_LOAD_MUSE_NOISE,

  ID_SAVE_SOLUTION_ASCII,
  ID_SAVE_SOLUTION_FITS,

// Menu/Help context
  ID_CONTEXT_HELP,

// Menu/Notebook:
  ID_NOTEBK_NOTEBOOK,
  ID_NOTEBK_PARAM,
  ID_NOTEBK_GRAPHIC,
  ID_NOTEBK_DISPLAY,
  ID_NOTEBK_FIT,
  ID_NOTEBK_TEXT,

// Text panel:
  ID_SHOW_HEADER,
  ID_SHOW_BAD_FRAMES,
  ID_SHOW_SHAPES,
  ID_SHOW_RESULTS,
  ID_TEXT_VALID_CHANGES,
  ID_TEXT_CANCEL_CHANGES,
  ID_TEXT_SAVETOFILE,

// Param panel: 
  ID_PARAM_CMB_DUST,
  ID_PARAM_CMB_WEIGHT,
  ID_PARAM_WEIGHT_BUTTON,
  ID_PARAM_VALID,
  ID_PARAM_LOAD1,
  ID_PARAM_LOAD2,
  ID_PARAM_SAVE1,
  ID_PARAM_SAVE2,
  ID_PARAM_DEFAULT,

// Fit panel: 
  ID_FIT_START1,
  ID_FIT_START2,
  ID_FIT_VALIDATE,

// Plot
  ID_PLOT_RESET,
  ID_PLOT_SPECTRUM1,
  ID_PLOT_SPECTRUM2,
 
// Display 
  ID_DISPLAY_ICHECKBOX,
  ID_DISPLAY_VCHECKBOX,

// Display processing
  ID_DISPROC_SHOW_SPECTRUM,
  ID_DISPROC_IDLE,
  ID_DISPROC_RESET,
  ID_DISPROC_RECT,
  ID_DISPROC_CIRC,
  ID_DISPROC_ELLIPS,
  ID_DISPROC_RING,
  ID_DISPROC_CANCEL,
  ID_DISPROC_MOVE,
  ID_DISPROC_SELECT,
  ID_DISPROC_ALL,
  ID_DISPROC_REJECTION,
 
// Logbook
  ID_LOGBOOK_SHOW,
  ID_LOGBOOK_HIDE,
  ID_LOGBOOK_CLEAR,
  ID_LOGBOOK_CLEAN,
  ID_LOGBOOK_SAVE,

// Dialogs:
  ID_WEIGHT_OK,
  ID_WEIGHT_CANCEL,

// Help:
  ID_ABOUT          = wxID_ABOUT,
  ID_HELP           = wxID_HELP

};

#endif
