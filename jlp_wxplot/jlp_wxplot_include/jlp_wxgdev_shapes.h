/***********************************************************************
* jlp_wxgdev_shapes.h
* JLP_wxGDevShapes class defined for adding labels to an image.
* Used by jlp_gdev_wxwid routines
*
* JLP
* Version 10/02/2017
***********************************************************************/
#ifndef _jlp_wxgdev_shapes_h 
#define _jlp_wxgdev_shapes_h 

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
#undef index  // To solve problems with index (conflicts with "string.h") ...
    #include "wx/wx.h"
#endif
#include "wx/graphics.h"   // wxGraphicsContext

#define MAX_SHAPES 1024

typedef struct {
double x1, y1;
double x2, y2;
double angle;
int type;
} JLP_SHAPE;

// Simple declaration here to avoid "boomerang" effect:
class JLP_GDev_wxWID;

class JLP_wxGDevShapes
{
public:
 
 JLP_wxGDevShapes();

 int AddShape(double x1, double y1, double x2, double y2, int shape_type0);
 int RotateShape(double x1, double y1, double x2, double y2);
 int MoveShape(double x1, double y1, double x2, double y2);
 int MagnifyShape(double x1, double y1, double x2, double y2);
 int RemoveShape(double x1, double y1);
 int CenterOfShape(JLP_SHAPE shape0, double *xc, double *yc);
 int GetClosestShape(double x0, double y0, int *imin, JLP_SHAPE *shape0);
 int GetShape(const int ishape, JLP_SHAPE *shape0);
 void DrawShapesToBackupDC(JLP_GDev_wxWID *jlp_wx_gdev1,
                           wxMemoryDC *backup_dc, wxColour c_PenColour,
                           int scroll_x, int scroll_y);
 void CancelLastShape(){
   if(nshapes1 > 0) nshapes1--;
  };

 void EraseAllShapes(){
   nshapes1 = 0;
  };

 int NShapes(){return(nshapes1);};

private:

// Shapes 
  int nshapes1;
  JLP_SHAPE shape1[MAX_SHAPES];

};

#endif
