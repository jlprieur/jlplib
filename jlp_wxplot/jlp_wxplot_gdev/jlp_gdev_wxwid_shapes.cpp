/******************************************************************************
* jlp_gdev_wxwid_shapes.cpp
* JLP_GDev_wxWID class
* Purpose:  displaying shapes on the image
*
* Author:   JLP 
* Version:  03/02/2016 
******************************************************************************/
#include "jlp_gdev_wxwid.h"
#include "jlp_wxgdev_shapes.h"      // JLP_wxGDevShape class
#include "jlp_wxgdev_popup.h"       // For JLP_wxGDev_Popup

/*
  void AddShape(double x1, double y1, double x2, double y2, int shape_type);
  void RemoveShape(double x0, double y0);
  void RotateShape(double x1, double y1, double x2, double y2, int shape_type);
  void MagnifyShape(double x1, double y1, double x2, double y2, int shape_type);
  void EraseAllShapes();
  void CancelLastShape();
*/

/**********************************************************************
* Add a shape to the image
**********************************************************************/
void JLP_GDev_wxWID::AddShape(double x1, double y1, double x2, double y2,
                               int shape_type)
{
if(m_wxshapes1 != NULL) {
  m_wxshapes1->AddShape(x1, y1, x2, y2, shape_type);
  RedrawToBackupDC(6003);
  }
}
/**********************************************************************
* Rotate a shape
**********************************************************************/
void JLP_GDev_wxWID::RotateShape(double x1, double y1, double x2, double y2)
{
if(m_wxshapes1 != NULL) {
  m_wxshapes1->RotateShape(x1, y1, x2, y2);
  RedrawToBackupDC(6004);
  }
}
/**********************************************************************
* Magnify a shape 
**********************************************************************/
void JLP_GDev_wxWID::MagnifyShape(double x1, double y1, double x2, double y2)
{
if(m_wxshapes1 != NULL) {
  m_wxshapes1->MagnifyShape(x1, y1, x2, y2);
  RedrawToBackupDC(6005);
  }
}
/**********************************************************************
* Move a shape
**********************************************************************/
void JLP_GDev_wxWID::MoveShape(double x1, double y1, double x2, double y2)
{
if(m_wxshapes1 != NULL) {
  m_wxshapes1->MoveShape(x1, y1, x2, y2);
  RedrawToBackupDC(6006);
  }
}
/**********************************************************************
* Remove a shape from the image
**********************************************************************/
void JLP_GDev_wxWID::RemoveShape(double x0, double y0)
{
if(m_wxshapes1 != NULL) {
  m_wxshapes1->RemoveShape(x0, y0);
  RedrawToBackupDC(6007);
  }
}
/**********************************************************************
* Remove all shapes from the image
**********************************************************************/
void JLP_GDev_wxWID::EraseAllShapes()
{
if(m_wxshapes1 != NULL) {
  m_wxshapes1->EraseAllShapes();
  RedrawToBackupDC(6008);
  }
}
/**********************************************************************
* Remove the last entered shape from the image
**********************************************************************/
void JLP_GDev_wxWID::CancelLastShape()
{
if(m_wxshapes1 != NULL) {
  m_wxshapes1->CancelLastShape();
  RedrawToBackupDC(6009);
  }
}
/**********************************************************************
* Get the closest shape
**********************************************************************/
int JLP_GDev_wxWID::GetClosestShape(double x0, double y0, int *imin, 
                                     JLP_SHAPE *shape0)
{
int status = -1;

if(m_wxshapes1 != NULL) {
   status = m_wxshapes1->GetClosestShape(x0, y0, imin, shape0);
  }

return(status);
}
/**********************************************************************
* Get the number of shapes
**********************************************************************/
int JLP_GDev_wxWID::GetNShapes(int *nshapes)
{
int status = -1;

*nshapes = 0;

if(m_wxshapes1 != NULL) {
  *nshapes = m_wxshapes1->NShapes();
  status = 0;
  }

return(status);
}
/**********************************************************************
* Get the shape of index ishape
**********************************************************************/
int JLP_GDev_wxWID::GetShape(const int ishape, JLP_SHAPE *shape0)
{
int status = -1;

if(m_wxshapes1 != NULL) {
  status = m_wxshapes1->GetShape(ishape, shape0);
  }

return(status);
}
