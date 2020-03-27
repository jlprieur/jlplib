/*************************************************************************
* \file jlp_wx_gdproc.h (virtual abstract class) 
* \class JLP_wx_GDProc called by JLP_GDev_wxWID (image/curve Graphic Device) 
* \brief Image processing with data points entered interactively 
* \author JLP
* \date 15/02/2017
*
* JLP
* Version 15/02/2017
**************************************************************************/
#include "jlp_wx_gdproc.h" // JLP_wx_GDProc class

/**************************************************************************
*
* Output:
*  processing_mode0 : interactive processing mode
*  limits_box_type0 : e.g., 5=ring
*  n_points_required0 : set according to box type
*
**************************************************************************/
int JLP_wx_GDProc::GetActiveProcessingMode(int *processing_mode0,
                                           int *limits_box_type0,
                                           int *n_points_required0)
{
// Set output values:
*processing_mode0 = ProcessingMode1;
*limits_box_type0 = LimitsBoxType1;
*n_points_required0 = n_PointsRequired1;

return(0);
}
