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
                               wxMemoryDC *backup_dc0, 
                               wxPostScriptDC *backup_pst_dc0)
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

// Correspondance with private variables
// and setting both xorigin and yorigin values to zero:
  GSEG_SetWindowLimits(0, device_width0, 0, device_height0);

// Init all image handlers in case it was not already done:
// (Needed to read PNG,JPEG, etc)
  wxInitAllImageHandlers();

return;
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


