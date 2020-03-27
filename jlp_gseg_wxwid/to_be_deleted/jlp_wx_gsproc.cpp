/*************************************************************************
* \file jlp_wx_gsproc.cpp (virtual abstract class) 
* \class JLP_wx_gsProc called by JLP_wxGseg_Canvas (image Graphic Device) 
* \brief Image processing with data points entered interactively 
* \author JLP
* \date 05/02/2016
*
* JLP
* Version 05/02/2016
**************************************************************************/
#include "jlp_wx_gsproc.h" // JLP_wx_gsProc class
#include "jlp_wx_gscanvas.h" // JLP_wx_GsegCanvas class

/*********************************************************************
* Set new configuration of JLP_iGDev_wxWID
*********************************************************************/
int JLP_wx_gsProc::AskNewBoxLimits_to_cgdev() {
// Limits box type: 1=line, 2=rectangle, 3=circle
  m_wxgseg_canvas1->SetNewRequiredPoints(n_PointsRequired1, LimitsBoxType1);
  return(0);
}
