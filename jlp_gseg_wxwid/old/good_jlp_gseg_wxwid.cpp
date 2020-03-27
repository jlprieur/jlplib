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

// Correspondance with private variables: 
gseg_backup_dc = backup_dc0;
gseg_backup_pst_dc = backup_pst_dc0;

window_width1 = device_width0;
window_height1 = device_height0;
// Both xorigin and yorigin are set to zero:
xx1 = 0;
yy1 = 0;

// Init all image handlers in case it was not already done:
// (Needed to read PNG,JPEG, etc)
  wxInitAllImageHandlers();

// Postscript coordinates:
pst_xx1 = 0;
pst_yy1 = 0;
// Unit for postscript is one point = 1 /72 inch 
// Rotated A4 format * 5 
pst_width1 = 5. * (double)(297 / 25.4) * 72.0;
pst_height1 = 5. * (double)(210 / 25.4) * 72.0;

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


