/********************************************************************
* jlp_gseg_wxwidg.cpp
* JLP_Gseg_Wxwid class 
*
* JLP
* Version 14/12/2016
********************************************************************/
#include "jlp_gseg_wxwid.h"

/**************************************************************************
* Constructor:
*
* Backup Device Context (accessible to public, for drawing)
*  wxMemoryDC *backup_dc;
**************************************************************************/
JLP_Gseg_Wxwid::JLP_Gseg_Wxwid(int device_width0, int device_height0,
                               wxMemoryDC *backup_dc0, wxPostScriptDC *backup_pst_dc0)
{
GSEG_InitializePlot(device_width0, device_height0, backup_dc0, backup_pst_dc0);

}
/**************************************************************************
*
**************************************************************************/
void JLP_Gseg_Wxwid::GSEG_InitializePlot(int device_width0, int device_height0,
                                         wxMemoryDC *backup_dc0, 
                                         wxPostScriptDC *backup_pst_dc0)
{

// Init all image handlers in case it was not already done:
// (Needed to read PNG,JPEG, etc)
  wxInitAllImageHandlers();

 InitPostscriptDC(backup_pst_dc0);

// Correspondance with private variables
// and setting both xorigin and yorigin values to zero:
#ifdef BAD_VERSION
 GSEG_SetWindowLimits(0, device_width0, 0, device_height0);
#else
  xx1 = 0;
  window_width1 = device_width0;
  yy1 = 0;
  window_height1 =  device_height0;
#endif

// Postscript settings:
pst_xx1 = 0;
pst_yy1 = 0;
// Unit for postscript is one point = 1 /72 inch
// A4 format * 8
pst_width1 = 8. * (double)(210 / 25.4) * 72.0;
pst_height1 = 8. * (double)(297 / 25.4) * 72.0;

x_wx_to_pst_scale1 = (double)pst_width1 / (double)window_width1;
// Trick to keep the same aspect ratio of graphics for postscript and wx window
y_wx_to_pst_scale1 = x_wx_to_pst_scale1
                     * ((double)window_height1 / (double)window_width1);

printf("ZZZ x_scale=%f yscale=%f\n", x_wx_to_pst_scale1, y_wx_to_pst_scale1);


}
/**************************************************************************
*
**************************************************************************/
void JLP_Gseg_Wxwid::InitPostscriptDC(wxPostScriptDC *backup_pst_dc0)
{
// Correspondance with private variables: 
gseg_backup_pst_dc = backup_pst_dc0;

return;
}


