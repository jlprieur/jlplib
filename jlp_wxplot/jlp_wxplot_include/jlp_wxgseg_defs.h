/******************************************************************************
* jlp_wxgseg_defs.h
*
* Author:      JLP 
* Version:     08/01/2017
******************************************************************************/
#ifndef jlp_wxgseg_defs_h    // sentry 
#define jlp_wxgseg_defs_h

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif

#include "wx/image.h"
#include "wx/file.h"
#include "wx/filename.h"
#include "wx/mstream.h"
#include "wx/wfstream.h"
#include "wx/quantize.h"
#include "wx/dcmemory.h"         // wxMemoryDC

enum
{
 ID_GSEG_CMB_GRAPHIC_TYPE = 1500,
 ID_GSEG_CMB_PLOT_TYPE,
 ID_GSEG_CMB_AXIS_TYPE,
 ID_GSEG_CMB_CONTOURS,
 ID_GSEG_CMB_PEN_WIDTH,
 ID_GSEG_GRID,
 ID_GSEG_EQUAL_SCALE,
 ID_GSEG_X_REVERSED,
 ID_GSEG_Y_REVERSED,
 ID_GSEG_Z_REVERSED,
 ID_GSEG_XLABEL,
 ID_GSEG_YLABEL,
 ID_GSEG_ZLABEL,
 ID_GSEG_TITLE,
 ID_GSEG_PLOT_DEVICE,
 ID_GSEG_OK,
 ID_GSEG_CANCEL
};

// Should be larger than the number of gseg_plot_type possibilities 
#define NCHOICES 12

typedef struct {
wxComboBox *combo;
wxString choices[NCHOICES];
wxString label;
int nchoices;
} JLP_ComboBox;

#endif
